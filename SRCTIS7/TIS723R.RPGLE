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
002900170721     D MSG             S             78    DIM(38) CTDATA PERRCD(1)
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
021000170721     C                   SETOFF                                           48
021100170721     C                   SETOFF                                       495051
021200120119     C                   SETOFF                                       525354
021300120119     C                   SETOFF                                       555657
021400970526     C                   SETOFF                                         5859
021500970526     C                   SETOFF                                       606162
021600970526     C                   SETOFF                                       636465
021700970526     C                   SETOFF                                       666768
021800150114     C                   SETOFF                                       6970
021900970526     C                   SETOFF                                       727374
022000120109     C                   SETOFF                                       757677
022100970526     C                   SETOFF                                       787980
022200970526     C                   SETOFF                                       818283
022300970526     C                   SETOFF                                       848586
022400970526     C                   SETOFF                                       878889
022500970526     C*
022600970526     C                   ENDSR
022700970526     C*------------------------------------------------------------------------*
022800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
022900970526     C*------------------------------------------------------------------------*
023000970526     C     SNDMSG        BEGSR
023100970526     C*
023200970526     C* PULISCE IL SFILE MESSAGGI
023300970526     C                   MOVEL     '1'           *IN40
023400991026     C                   WRITE     s723DMC
023500970526     C                   MOVEL     '0'           *IN40
023600970526     C*
023700970526     C* VISUALIZZA IL MESSAGGIO
023800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
023900970526     C                   CALL      'XMSG'        XMSGPA
024000970526IF  1C     DSMSER        IFEQ      *ZEROS
024100970526     C                   MOVEL     DSMSKE        MSGKEY
024200970526E   1C                   ENDIF
024300970526     C                   Z-ADD     1             MSGULT
024400991026     C                   WRITE     s723DMS
024500991026     C                   WRITE     s723DMC
024600970526     C*
024700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
024800970526     C                   MOVEL     *BLANKS       DSMSMS
024900970526     C*
025000970526     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200970526     C* ENTGES - GESTIONE ENTER -> INVIO
025300970526     C*------------------------------------------------------------------------*
025400970526     C     ENTGES        BEGSR
025500970526     C*
025600970526     C* CONTROLLA I CAMPI IMMESSI
025700970526     C                   EXSR      DECODI
025800970526     C*
025900970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
026000970526IF  1C     DEPOP1        IFEQ      'O05'
026100970526     C                   MOVEL     '1'           FINL00
026200970526E   1C                   ENDIF
026300970526     C*
026400970526     C                   ENDSR
026500031229     C*------------------------------------------------------------------------*
026600031229     C* F01GES - GESTIONE F01 -> VISUALIZZA REGOLE PARAMETRI TRADUTTORE
026700031229     C*------------------------------------------------------------------------*
026800031229     C     F01GES        BEGSR
026900031229     C*
027000031229     C* Passo o il traduttore standard relativo al tipo file corrente o se inserito il
027100031229     C* traduttore specifico
027200031229     C                   IF        VIDPGT <> *blanks
027300031229     C                   EVAL      wPGT = VIDPGT
027400031229     C                   ELSE
027500031229     C                   EVAL      wPGT = VIDPGTD
027600031229     C                   ENDIF
027700031229     C*
027800031229     C* Visualizzo le regole relative
027900031229     C                   CALL      'TIS723C'
028000031229     C                   PARM                    wPGT             10
028100040107     C                   PARM                    wERR              1
028200040107     C*
028300040107     C* Verifico se trovato membro con descrizoine dettaglio parametri traduttore
028400040107     C                   IF        wERR = '1'
028500040107     C                   MOVEL     MSG(23)       DSMSMS
028600040107     C                   ENDIF
028700031229     C*
028800031229     C                   ENDSR
028900141111     C*------------------------------------------------------------------------*
029000141111     C* F02GES - GESTIONE F02 -> PARAMETRI AGGIUNTIVI
029100141111     C*------------------------------------------------------------------------*
029200141111     C     F02GES        BEGSR
029300150113     C*
029400150113     C                   clear                   trulkpjds
029500150113     C*
029600150113     C* Gestione estensione anagrafica clienti/tipi file => TRC PE
029700150113     C                   CLEAR                   DVIREPE
029800150113     C                   EVAL      KCEdti  = virDTI
029900150113     C                   EVAL      vireTRC = 'PE'
030000150113     C     KEYvire1_C    CHAIN     tivire1l
030100150113     C                   IF        %found(tivire1l)
030200150114     C                   SETON                                        71
030300150113     C                   EVAL      DVIREPE = vireFLO
030400150113     C                   EVAL      OKPJKPJBU = §virePEKPJ
030500150113     C                   ENDIF
030600141111     C*
030700141111     C                   call      'TRULKPJR'
030800141111     C                   parm                    kpjba
030900141111     C                   parm                    trulkpjds
031000141111     C*
031100141111     C* Gestione estensione anagrafica clienti/tipi file => TRC PE
031200150113     C                   IF        OKPJOPZ = 'F06'
031300141111     C                   CLEAR                   DVIREPE
031400141111     C                   EVAL      KCEdti  = virDTI
031500141111     C                   EVAL      vireTRC = 'PE'
031600141111     C     KEYvire1_C    CHAIN     tivire1l
031700141111     C                   IF        %found(tivire1l)
031800141111     C                   IF        OKPJKPJBU <> *blanks
031900141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
032000141111     C                   EVAL      vireFLO = DVIREPE
032100141111     C                   UPDATE    tivire00
032200150114     C                   SETON                                        71
032300141111     C                   ELSE
032400141111     C                   DELETE    tivire00
032500150114     C                   SETOFF                                       71
032600141111     C                   ENDIF
032700141111     C                   ELSE
032800141111     C                   IF        OKPJKPJBU <> *blanks
032900141111     C                   EVAL      vireKSC = virKSC
033000141111     C                   EVAL      vireTIP = virTIP
033100141111     C                   EVAL      vireDTI = virDTI
033200141111     C                   EVAL      vireTRC = 'PE'
033300141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
033400141111     C                   EVAL      vireFLO = DVIREPE
033500141111     C                   WRITE     tivire00
033600150114     C                   SETON                                        71
033700141111     C                   ENDIF
033800141111     C                   ENDIF
033900150113     C                   ENDIF
034000141111     C*
034100141111     C                   ENDSR
034200970526     C*------------------------------------------------------------------------*
034300970526     C* F03GES - GESTIONE F03 -> FINE
034400970526     C*------------------------------------------------------------------------*
034500970526     C     F03GES        BEGSR
034600970526     C*
034700970526     C* IMPOSTA "PREMUTO F03"
034800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
034900991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
035000970526     C*
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
035400970526     C*------------------------------------------------------------------------*
035500970526     C     F06GES        BEGSR
035600970526     C*
035700970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
035800970526IF  1C     DEPOP1        IFEQ      'F06'
035900970526     C                   EXSR      INSERI
036000970526E   1C                   ENDIF
036100970526IF  1C     DEPOP1        IFEQ      'O02'
036200970526     C                   EXSR      MODIFI
036300970526E   1C                   ENDIF
036400991014IF  1C     DEPOP1        IFEQ      'O03'
036500011129     C                   EXSR      COPIA
036600991014E   1C                   ENDIF
036700991104IF  1C*    DEPOP1        IFEQ      'O04'
036800991104     C*                  EXSR      ANNULL
036900991104E   1C*                  ENDIF
037000970526IF  1C     DEPOP1        IFEQ      'O07'
037100970526     C                   EXSR      RIPRIS
037200970526E   1C                   ENDIF
037300080603     C*
037400080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
037500080603     C                   CLEAR                   DVIRE01
037600080603     C                   EVAL      KCEdti  = virDTI
037700080603     C                   EVAL      vireTRC = '01'
037800080603     C     KEYvire1_C    CHAIN     tivire1l
037900080603     C                   IF        %found(tivire1l)
038000080603     C                   IF        vidEML <> *blanks
038100080603     C                   EVAL      §vire01INV = 'S'
038200080603     C                   EVAL      §vire01EML = vidEML
038300080603     C                   EVAL      vireFLO = DVIRE01
038400080603     C                   UPDATE    tivire00
038500080603     C                   ELSE
038600080603     C                   DELETE    tivire00
038700080603     C                   ENDIF
038800080603     C                   ELSE
038900080603     C                   IF        vidEML <> *blanks
039000080603     C                   EVAL      vireKSC = virKSC
039100080603     C                   EVAL      vireTIP = virTIP
039200080603     C                   EVAL      vireDTI = virDTI
039300080603     C                   EVAL      vireTRC = '01'
039400080603     C                   EVAL      §vire01INV = 'S'
039500080603     C                   EVAL      §vire01EML = vidEML
039600080603     C                   EVAL      vireFLO = DVIRE01
039700080603     C                   WRITE     tivire00
039800080603     C                   ENDIF
039900080603     C                   ENDIF
040000970526     C*
040100011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
040200011129     C                   IF        errl00 = '0'                                 *NO ERRORI
040300011129     C                   CALL      'TIS70UR'
040400011129     C                   PARM                    KPJBA
040500011129     C                   PARM                    VIDTIP
040600011129     C                   PARM                    VIDKSC
040700011129     C                   PARM      *blanks       flgope            1
040800011129     C                   ENDIF
040900011129     C*
041000970526     C                   ENDSR
041100970526     C*------------------------------------------------------------------------*
041200970526     C* F12GES - GESTIONE F12 -> RITORNO
041300970526     C*------------------------------------------------------------------------*
041400970526     C     F12GES        BEGSR
041500970526     C*
041600970526     C* IMPOSTA "PREMUTO F12"
041700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
041800991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
041900970526     C*
042000970526     C                   ENDSR
042100970526     C*------------------------------------------------------------------------*
042200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
042300970526     C*------------------------------------------------------------------------*
042400970526     C     F24GES        BEGSR
042500970526     C*
042600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
042700970526     C*
042800970526     C                   ENDSR
042900970526     C*------------------------------------------------------------------------*
043000970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
043100970526     C*------------------------------------------------------------------------*
043200970526     C     INSERI        BEGSR
043300970526     C*
043400970526     C* CONTROLLA I CAMPI IMMESSI
043500970526     C                   EXSR      DECODI
043600970526     C*
043700970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
043800991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
043900991026     C                   movel     VIDTip        virtip
044000991026     C                   move      VIDksc        virksc
044100991026     C                   z-add     VIDseq        virseq
044200991026     C                   Z-ADD     viddti        G08dat
044300991026     C                   Z-ADD     *ZEROS        G08inv
044400991026     C                   MOVEL     '0'           G08ERR
044500991026     C                   CALL      'XSRDA8'
044600991026     C                   PARM                    WLBDA8
044700991026     C                   Z-ADD     G08inv        virdti
044800991026     c                   exsr      impvir
044900991026     C                   WRITE     tivir000
045000991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
045100970602     C*
045200970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
045300020228     C****               EXSR      AZZVID
045400970526E   1C                   ENDIF
045500970526     C*
045600970526     C                   ENDSR
045700970526     C*------------------------------------------------------------------------*
045800970526     C* MODIFI - OPERAZIONI DI MODIFICA
045900970526     C*------------------------------------------------------------------------*
046000970526     C     MODIFI        BEGSR
046100970526     C*
046200970526     C* CONTROLLA I CAMPI IMMESSI
046300970526     C                   EXSR      DECODI
046400970526     C*
046500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
046600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
046700991026     C                   EXSR      LETvir
046800970526     C*
046900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
047000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047100991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
047200991026     C                   UPDATE    tivir000
047300101027     C                   EVAL      S20MSG = MSG(3)
047400101027     C                   EVAL      DSMSMS = MSG(3)
047500101027     C*
047600101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
047700101027     C                   IF        *IN80
047800111215     C                             and AS400_ok = *on
047900101027      /FREE
048000101027
048100101027         if *in30 = *off;
048200101027           pInMaskChk  = VIDPPIMC;
048300101027         else;
048400101027           pInMaskChk  = pOutCliMskChk;
048500101027         endif;
048600101027
048700101027         if *in31 = *off;
048800101027           pInMaskFile = VIDPPIMF;
048900101027         else;
049000101027           pInMaskFile = pOutCliMskFil;
049100101027         endif;
049200101027
049300101027         if *in32 = *off;
049400101027           pInMaskSem  = VIDPPIMS;
049500101027         else;
049600101027           pInMaskSem  = pOutCliMskSem;
049700101027         endif;
049800101027
049900101027         if *in33 = *off;
050000101027           pInMaskZip  = VIDPPIMZ;
050100101027         else;
050200101027           pInMaskZip  = pOutCliMskZip;
050300101027         endif;
050400101027
050500101027
050600101027         if UBBVSQL01_Upd01UnqCliTipFile(
050700101027                         pInConn
050800101027                       : VIDKSC
050900101027                       : VIDTIP
051000101027                       : pInMaskChk
051100101027                       : pInMaskFile
051200101027                       : pInMaskSem
051300101027                       : pInMaskZip) = 0;
051400101027         else;
051500101027              S20MSG = MSG(29);
051600101027              DSMSMS = MSG(29);
051700101027         endif;
051800101027
051900101027      /END-FREE
052000101027     C
052100101027     C*
052200101027     C* Quindi (già che c sono ricarico anche il job
052300101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
052400101027     C                                           VIDKSC
052500101027     C                                          :VIDTIP
052600101027     C                                          :pOutCliProc)
052700101027     C                   ENDIF
052800101027     C*
052900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
053000970526E   2C                   ENDIF
053100970526E   1C                   ENDIF
053200970526     C*
053300970526     C                   ENDSR
053400991014     C*------------------------------------------------------------------------*
053500991014     C* copia  - OPERAZIONI DI copia
053600991014     C*------------------------------------------------------------------------*
053700991014     C     copia         BEGSR
053800991014     C*
053900991014     C* CONTROLLA I CAMPI IMMESSI
054000991014     C                   EXSR      DECODI
054100991014     C*
054200991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
054300991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
054400991026     C                   movel     VIDTip        virtip
054500991110     C                   move      VIDksc        virksc
054600991110     C                   z-add     VIDseq        virseq
054700991110     C                   Z-ADD     viddti        G08dat
054800991110     C                   Z-ADD     *ZEROS        G08inv
054900991110     C                   MOVEL     '0'           G08ERR
055000991110     C                   CALL      'XSRDA8'
055100991110     C                   PARM                    WLBDA8
055200991110     C                   Z-ADD     G08inv        virdti
055300991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
055400991026     C                   write     tivir000
055500991025     C                   MOVEL     MSG(3)        S20MSG
055600991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
055700991014E   2C                   ENDIF
055800991014     C*
055900991014     C                   ENDSR
056000970526     C*------------------------------------------------------------------------*
056100970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
056200970526     C*------------------------------------------------------------------------*
056300991104     C*    ANNULL        BEGSR
056400991025     C*
056500991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
056600991104     C*                  EXSR      LETvir
056700991025     C*
056800991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
056900991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
057000991104     C*                  DELETE    tivir000                                     *CANCELLA
057100991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
057200991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
057300991104E   1C*                  ENDIF
057400970526     C*
057500991104     C*                  ENDSR
057600970526     C*------------------------------------------------------------------------*
057700970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
057800970526     C*------------------------------------------------------------------------*
057900970526     C     RIPRIS        BEGSR
058000970526     C                   ENDSR
058100970526     C*------------------------------------------------------------------------*
058200991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
058300970526     C*------------------------------------------------------------------------*
058400991026     C     LETvir        BEGSR
058500970526     C*
058600991025     C                   MOVEL     S20tip        KCEtip
058700991026     C                   MOVEL     S20ksc        KCEksc
058800000420     C     KEYvi2        CHAIN     tivir03L                           9998
058900970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
059000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
059100991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
059200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
059300970526E   1C                   ENDIF
059400970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
059500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
059600991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
059700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
059800970526E   1C                   ENDIF
059900970526     C*
060000970526     C                   ENDSR
060100970526     C*------------------------------------------------------------------------*
060200970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
060300970526     C*------------------------------------------------------------------------*
060400970526     C     DECODI        BEGSR
060500001120     C*
060600001120     C*----------------------
060700001120     C* Controllo del codice
060800001120     C*----------------------
060900001120     C     ERRL00        IFEQ      '0'
061000001120     C     VIDksc        IFne      *blank
061100001120     C     VIDksc        andne     *zeros
061200001120     C                   exsr      cntksc
061300001120     C                   ELSE
061400991027     C                   MOVEL     '1'           ERRL00
061500991027     C                   SETON                                        65
061600991027     C                   MOVEL     MSG(12)       DSMSMS
061700001120     C                   ENDIF
061800001120     C                   ENDIF
061900101026     C*
062000101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
062100101026     C     ERRL00        IFEQ      '0'
062200101026     C     VIDisv        lookup    isvftpeml                              80
062300101026 xxx C***                setoff                                       80
062400101026     C                   ENDIF
062500001120     C*----------------------
062600001120     C* Controllo della sequenza
062700001120     C*----------------------
062800001120     C     ERRL00        IFEQ      '0'
062900001120     C     VIDseq        IFne      0
063000001120     C                   ELSE
063100991027     C                   MOVEL     '1'           ERRL00
063200991027     C                   SETON                                        67
063300991027     C                   MOVEL     MSG(12)       DSMSMS
063400001120     C                   ENDIF
063500001120     C                   ENDIF
063600120109     C*----------------------
063700120109     C* Controllo tipo file
063800120109     C*----------------------
063900120109     C     ERRL00        IFEQ      '0'
064000120109     C     VIDtip        IFne      *blank
064100120109     C                   exsr      cnttip
064200120109     C                   ELSE
064300120109     C                   MOVEL     '1'           ERRL00
064400120109     C                   SETON                                        66
064500120109     C                   MOVEL     MSG(12)       DSMSMS
064600120109     C                   ENDIF
064700120109     C                   ENDIF
064800120109     C*----------------------
064900120109     C* Tipo servizio
065000120109     C*----------------------
065100120109     C     ERRL00        IFEQ      '0'
065200120109     C     VIDisv        IFne      *blank
065300120109     C                   exsr      cntisv
065400120109     C* Verifico la compatibilità del servizio rispetto al tipo-file
065500120109     C                   IF        VTFUD <> §ISVVERS
065600120109     C                   MOVEL     '1'           ERRL00
065700120109     C                   SETON                                        6670
065800120109     C                   MOVEL     MSG(24)       DSMSMS
065900120109     C                   ENDIF
066000120109     C                   ELSE
066100120109     C                   MOVEL     '1'           ERRL00
066200120109     C                   SETON                                        70
066300120109     C                   MOVEL     MSG(12)       DSMSMS
066400120109     C                   ENDIF
066500120109     C                   ENDIF
066600120109     C*--------------------
066700120109     C* Dati solo x upload no in download
066800120109     C*--------------------
066900120109     C* a seconde se siamo in Download o Upload sproteggo alcuni campi e ne proteggo altri
067000120109     C* e se non abbiamo errori pulisco i campi protetti (se abbiamo un errore potrebbe essere
067100120109     C*  che non siamo in D o U e i valori nei campi servono)
067200120109     C                   IF        §ISVVers   = 'D'
067300120109     c                   SETON                                        93
067400120109     C     ERRL00        IFEQ      '0'
067500120109     C                   MOVEL     *blanks       VIDpoi
067600170721     C                   MOVEL     *blanks       VIDke2
067700120109     C                   MOVEL     *blanks       VIDEmL
067800120109     C                   MOVEL     *blanks       VIDfi2
067900120109     C                   MOVEL     *blanks       VIDfi4
068000120109     C                   MOVEL     *blanks       VIDfi4D
068100120109     C                   ENDIF
068200120109     C                   ENDIF
068300120109     C                   IF        §ISVVers   = 'U'
068400120109     C                   SETOFF                                       93
068500120109     C     ERRL00        IFEQ      '0'
068600120109     C                   MOVEL     *blanks       VIDTMbD
068700120109     C                   MOVEL     *blanks       VIDTip2
068800120109     C                   MOVEL     *blanks       VIDTip3
068900120109     C                   MOVEL     *blanks       VIDTip4
069000120119     C                   MOVEL     *blanks       VIDKsc2
069100120119     C                   MOVEL     *blanks       VIDKsc3
069200120119     C                   MOVEL     *blanks       VIDKsc4
069300120109     C                   ENDIF
069400120109     C                   ENDIF
069500170721     C*----------------------
069600170721     C* Codice opzione
069700170721     C*----------------------
069800170721     C     ERRL00        IFEQ      '0'
069900170721     C                   exsr      cntke2
070000170721     C                   ENDIF
070100100614     C*--------------------
070200100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
070300100614     C*--------------------
070400100614     C     ERRL00        IFEQ      '0'
070500100614     C                   IF        DEPOP1 = 'F06' OR
070600100614     C                             DEPOP1 = 'O03'
070700100614     C                   IF        §ISVPROC = 'F' AND
070800100614     C                             §ISVVERS = 'D'
070900100614     C                   IF        VIDtmbd = *blanks AND
071000100614     C                             vtfud   = 'D'
071100100614     C                   MOVEL     'M'           VIDtmbd
071200100614     C                   ENDIF
071300100614     C                   ENDIF
071400100614     C                   ENDIF
071500100614     C                   ENDIF
071600030603     C*----------------------
071700030603     C* Tipo membro
071800030603     C*----------------------
071900030603     C     ERRL00        IFEQ      '0'
072000030603     C     VIDtmbd       IFNE      *blank
072100030603     C                   IF        §ISVPROC = 'F' AND
072200030603     C                             §ISVVERS = 'D'
072300030603     C                   ELSE
072400030603     C                   MOVEL     '1'           ERRL00
072500030603     C                   SETON                                        61
072600030603     C                   MOVEL     MSG(22)       DSMSMS
072700030603     C                   ENDIF
072800030603     C                   ENDIF
072900030603     C                   ENDIF
073000120109     C*----------------------
073100120109     C* Campi duplicazione automatica su tipi file
073200120109     C*----------------------
073300120109     C     ERRL00        IFEQ      '0'
073400120109     C*
073500120109     C                   EVAL      MaxCiclo = %elem(Tips)
073600120109     C                   EVAL      Tips(1)  = VIDTip2
073700120109     C                   EVAL      Tips(2)  = VIDTip3
073800120109     C                   EVAL      Tips(3)  = VIDTip4
073900120119     C* se il tipo file è valorizzato e il cliente relativo no (o 0 o ' ')
074000120119     C* lo forzo col cliente principale
074100120119     C                   IF        (VIDKsc2 = *zeros or
074200120119     C                              VIDKsc2 = *blank   ) and
074300120119     C                             VIDTip2 <> *blank
074400120119     C                   EVAL      VIDKsc2 =  VIDKsc
074500120119     C                   ENDIF
074600120119     C                   IF        (VIDKsc3 = *zeros or
074700120119     C                              VIDKsc3 = *blank   ) and
074800120119     C                             VIDTip3 <> *blank
074900120119     C                   EVAL      VIDKsc3 =  VIDKsc
075000120119     C                   ENDIF
075100120119     C                   IF        (VIDKsc4 = *zeros or
075200120119     C                              VIDKsc4 = *blank   ) and
075300120119     C                             VIDTip4 <> *blank
075400120119     C                   EVAL      VIDKsc4 =  VIDKsc
075500120119     C                   ENDIF
075600120119     C                   EVAL      Kscs(1)  = VIDKsc2
075700120119     C                   EVAL      Kscs(2)  = VIDKsc3
075800120119     C                   EVAL      Kscs(3)  = VIDKsc4
075900120109     C     1             DO        MaxCiclo      i
076000120109     C     Tips(i)       IFNE      *blank
076100120119     C     Kscs(i)       ORNE      *blank
076200120119     C     Kscs(i)       ANDNE     *zeros
076300120109     C                   IF        §ISVVERS = 'D'
076400120119     C                   EXSR      CntTip234
076500120111     C* se mi è stato dato un errore, esco dal ciclo
076600120111     C                   IF        ERRL00 <> '0'
076700120111     C                   LEAVE
076800120111     C                   ENDIF
076900120109     C                   ELSE
077000120109     C                   SELECT
077100120109     C                   WHEN      i = 1
077200120119     C                   SETON                                        7549
077300120109     C                   WHEN      i = 2
077400120119     C                   SETON                                        7650
077500120109     C                   WHEN      i = 3
077600120119     C                   SETON                                        7751
077700120109     C                   ENDSL
077800120109     C                   MOVEL     '1'           ERRL00
077900120109     C                   MOVEL     MSG(30)       DSMSMS
078000120109     C                   ENDIF
078100120109     C                   ENDIF
078200120109     C                   ENDDO
078300120109     C*
078400120109     C                   ENDIF
078500001120     C*----------------------
078600001120     C* Controllo della descrizione
078700001120     C*----------------------
078800001120     C     ERRL00        IFEQ      '0'
078900001120     C     VIDdes        IFne      *blanks
079000001120     C                   ELSE
079100991122     C*                  MOVEL     '1'           ERRL00
079200991122     C*                  SETON                                        62
079300991122     C*                  MOVEL     MSG(12)       DSMSMS
079400991122     C                   MOVEL     VDDTIP        VIDDES
079500001120     C                   ENDIF
079600001120     C                   ENDIF
079700991026     C*--------------------
079800991026     C* DATA inizio
079900991026     C*--------------------
080000991026     C     *LIKE         DEFINE    VIDDti        DtiVID
080100001120     C     ERRL00        IFEQ      '0'
080200991026     C                   Z-ADD     VIDDti        wda8
080300991026     C                   Z-ADD     *ZEROS        da8w
080400991026     C                   EXSR      CHKDA8
080500991026     C                   Z-ADD     WDA8          viddti
080600991026     C                   Z-ADD     DA8W          dtivid
080700001120     C     ERRL00        IFEQ      '1'
080800991026     C                   SETON                                        63
080900991026     C                   MOVEL     MSG(14)       dsmsms
081000001120     C                   ENDIF
081100001120     C                   ENDIF
081200991026     C* INSERIMENTO -> RECORD INESISTENTE
081300001120     C     ERRL00        IFEQ      '0'
081400001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
081500001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
081600991026     C                   move      vidksc        KCEksc
081700000420     C                   move      vidtip        KCEtip
081800000420     C                   z-add     dtivid        KCEdti
081900000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
082000001120     C     *IN99         IFEQ      *OFF
082100991026     C                   MOVEL     '1'           ERRL00
082200991026     C                   MOVEL     MSG(8)        DSMSMS
082300991026     C                   SETON                                        676566
082400991026     C                   SETON                                        63
082500001120     C                   ENDIF
082600001120     C                   ENDIF
082700001120     C                   ENDIF
082800001120     C*--------------------
082900991026     C* DATA SCADENZA
083000991026     C*--------------------
083100991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
083200001120     C     ERRL00        IFEQ      '0'
083300991026     C                   Z-ADD     VIDDtf        WDA8
083400991026     C                   Z-ADD     *ZEROS        DA8W
083500991026     C                   EXSR      CHKDA8
083600991026     C                   Z-ADD     WDA8          VIDDtf
083700991026     C                   Z-ADD     DA8W          DtfVID
083800001120     C     ERRL00        IFEQ      '1'
083900991026     C                   SETON                                        68
084000991026     C                   MOVEL     MSG(14)       DSMSMS
084100001120     C                   ENDIF
084200001120     C                   ENDIF
084300991026     C* SCADENZA < DECORRENZA
084400001120     C     ERRL00        IFEQ      '0'
084500001120     C     DtfVID        IFLT      DtiVID
084600991026     C                   SETON                                        68
084700991026     C                   MOVEL     MSG(15)       DSMSMS
084800991026     C                   MOVEL     '1'           ERRL00
084900001120     C                   ENDIF
085000001120     C                   ENDIF
085100991026     C* SCADENZA >= DATA CORRENTE
085200141021     C     ERRL00        IFEQ      '0'
085300141021     C***  DEPOP1        IFEQ      'O02'
085400141021     C***  DEPOP1        OREQ      'O07'
085500141021     C***  DEPOP1        OREQ      'F06'
085600141021     C     DEPOP1        IFEQ      'O07'
085700141021     C     DEPOP1        OREQ      'F06'
085800001120     C     DtfVID        IFLT      DATCOR
085900001120     C                   SETON                                        68
086000991026     C                   MOVEL     MSG(16)       DSMSMS
086100991026     C                   MOVEL     '1'           ERRL00
086200001120     C                   ENDIF
086300001120     C                   ENDIF
086400001120     C                   ENDIF
086500991026     C*---
086600991026     C* DECORRENZA + SCADENZA
086700991026     C*---
086800991026     C* ACCAVALLAMENTO DATI
086900001120     C     ERRL00        IFEQ      '0'
087000991026     C                   MOVEL     VIDtip        KCEtip
087100991026     C                   MOVEL     vidksc        KCEksc
087200991026     C     KEyvir        SETLL     tivir03L
087300991026     C     KEyvir        READE(N)  tivir03L                               99
087400001120     C     *IN99         DOWEQ     *OFF
087500001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
087600991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
087700991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
087800001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
087900991026     C     DtiVID        ANDLE     virdtf
088000991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
088100991026     C     DtfVID        ANDLE     virdtf
088200991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
088300991026     C     virdtf        ANDLE     DtfVID
088400991026     C                   SETON                                        666765
088500991026     C                   SETON                                        6368
088600991026     C                   MOVEL     MSG(17)       DSMSMS
088700991026     C                   MOVEL     '1'           ERRL00
088800991026     C                   LEAVE
088900001120     C                   ENDIF
089000001120     C                   ENDIF
089100991026     C     KEyvir        READE(N)  tivir03L                               99
089200001120     C                   ENDDO
089300001120     C                   ENDIF
089400001120     C*--------------------
089500001120     C* Controllo filiale
089600001120     C*--------------------
089700001120     C     ERRL00        IFEQ      '0'
089800001120     C     VIDpoi        IFne      *blank
089900001120     C     VIDpoi        andne     *zeros
090000001120     C                   exsr      cntpoi
090100001120     C                   ELSE
090200120109     C     §ISVVers      IFNE      'D'
090300991027     C                   MOVEL     '1'           ERRL00
090400000724     C                   MOVEL     MSG(12)       DSMSMS
090500991027     C                   SETON                                        69
090600991122     C                   ENDIF
090700001120     C                   ENDIF
090800001120     C                   ENDIF
090900001120     C*--------------------
091000011016     C* Programma traduttore
091100001120     C*--------------------
091200001120     C     ERRL00        IFEQ      '0'
091300001120     C     VIDpgt        IFNE      *blank
091400001120     C                   CALL(e)   'TIS721C'
091500050112     C                   PARM      *blanks       LIBRERIA         10
091600001120     C                   PARM      VIDpgt        OBJECT           10
091700001120     C                   PARM      '*PGM   '     TYPE              7
091800001120     C                   PARM      '0'           ESITO             1
091900001120     C                   IF        ESITO = '1'
092000001120     C                   MOVEL     '1'           ERRL00
092100001120     C                   SETON                                        54
092200001120     C                   MOVEL     MSG(19)       DSMSMS
092300001120     C                   ENDIF
092400001120     C                   ENDIF
092500001120     C                   ENDIF
092600001120     C*--------------------
092700001120     C* Programma post-traduzione
092800001120     C*--------------------
092900001120     C     ERRL00        IFEQ      '0'
093000001120     C     VIDpgi        IFNE      *blank
093100001120     C                   CALL(e)   'TIS721C'
093200050112     C                   PARM      *blanks       LIBRERIA         10
093300001120     C                   PARM      VIDpgi        OBJECT           10
093400001120     C                   PARM      '*PGM   '     TYPE              7
093500001120     C                   PARM      '0'           ESITO             1
093600001120     C                   IF        ESITO = '1'
093700001120     C                   MOVEL     '1'           ERRL00
093800001120     C                   SETON                                        55
093900001120     C                   MOVEL     MSG(19)       DSMSMS
094000001120     C                   ENDIF
094100001120     C                   ENDIF
094200001120     C                   ENDIF
094300001120     C*--------------------
094400001120     C* File da tradurre
094500001120     C*--------------------
094600001120     C     ERRL00        IFEQ      '0'
094700001120     C     VIDfid        IFNE      *blank
094800001120     C                   CALL(e)   'TIS721C'
094900050112     C                   PARM      *blanks       LIBRERIA         10
095000001120     C                   PARM      VIDfid        OBJECT           10
095100001120     C                   PARM      '*FILE  '     TYPE              7
095200001120     C                   PARM      '0'           ESITO             1
095300001120     C                   IF        ESITO = '1'
095400001120     C                   MOVEL     '1'           ERRL00
095500001120     C                   SETON                                        53
095600001120     C                   MOVEL     MSG(20)       DSMSMS
095700001120     C                   ENDIF
095800001120     C                   ENDIF
095900001120     C                   ENDIF
096000001120     C*--------------------
096100001120     C* File storico
096200001120     C*--------------------
096300001120     C     ERRL00        IFEQ      '0'
096400001120     C     VIDfis        IFNE      *blank
096500001120     C                   CALL(e)   'TIS721C'
096600050112     C                   PARM      *blanks       LIBRERIA         10
096700001120     C                   PARM      VIDfis        OBJECT           10
096800001120     C                   PARM      '*FILE  '     TYPE              7
096900001120     C                   PARM      '0'           ESITO             1
097000001120     C                   IF        ESITO = '1'
097100001120     C                   MOVEL     '1'           ERRL00
097200001120     C                   SETON                                        56
097300001120     C                   MOVEL     MSG(20)       DSMSMS
097400001120     C                   ENDIF
097500001120     C                   ENDIF
097600001120     C                   ENDIF
097700001120     C*--------------------
097800001120     C* File tradotto
097900001120     C*--------------------
098000001120     C     ERRL00        IFEQ      '0'
098100001120     C     VIDfit        IFNE      *blank
098200001120     C                   CALL(e)   'TIS721C'
098300050112     C                   PARM      VIDfi3        LIBRERIA         10
098400001120     C                   PARM      VIDfit        OBJECT           10
098500001120     C                   PARM      '*FILE  '     TYPE              7
098600001120     C                   PARM      '0'           ESITO             1
098700001120     C                   IF        ESITO = '1'
098800001120     C                   MOVEL     '1'           ERRL00
098900001120     C                   SETON                                        57
099000001120     C                   MOVEL     MSG(20)       DSMSMS
099100001120     C                   ENDIF
099200001120     C                   ENDIF
099300001120     C                   ENDIF
099400001120     C*--------------------
099500001120     C* Libreria file tradotto
099600001120     C*--------------------
099700001120     C     ERRL00        IFEQ      '0'
099800001120     C     VIDfi3        IFNE      *blank
099900001120     C                   CALL(e)   'TIS721C'
100000050112     C                   PARM      *blanks       LIBRERIA         10
100100001120     C                   PARM      VIDfi3        OBJECT           10
100200001120     C                   PARM      '*LIB   '     TYPE              7
100300001120     C                   PARM      '0'           ESITO             1
100400001120     C                   IF        ESITO = '1'
100500001120     C                   MOVEL     '1'           ERRL00
100600001120     C                   SETON                                        52
100700001120     C                   MOVEL     MSG(21)       DSMSMS
100800001120     C                   ENDIF
100900001120     C                   ENDIF
101000001120     C                   ENDIF
101100040816     C*--------------------
101200100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
101300040816     C*--------------------
101400040816     C     ERRL00        IFEQ      '0'
101500040816     C     VIDfit        IFne      *blank
101600040816     C     VIDpgt        ORne      *blank
101700040816     C                   EVAL      TBEcod = 'IFT'
101800040816     C     KEYtbe1_P     SETLL     tntbe01l
101900040816     C                   IF        %equal(tntbe01l)
102000040816     C* Inizializzo indici e schiere
102100040816     C                   Z-ADD     *zeros        j
102200040816     C                   CLEAR                   skTipF
102300040816     C                   CLEAR                   skPgt
102400040816     C                   CLEAR                   skFileT
102500100311     C                   CLEAR                   skMembroT
102600040816     C* Carico le schiere da tabella 'IFT'
102700040816     C     KEYtbe1_P     READE     tntbe01l
102800040816     C                   DOW       not %eof(tntbe01l)
102900040816     C                   IF        TBEatb = *blanks
103000040816     C                   EVAL      DIFT = TBEuni
103100040816     C                   ADD       1             j
103200100311     C                   EVAL      skTipF(j)    = TBEke1
103300100311     C                   EVAL      skPgt(j)     = §IFTPGT
103400100311     C                   EVAL      skFileT(j)   = §IFTFILE
103500100311     C                   EVAL      skMembroT(j) = §IFTTMBD
103600040816     C                   ENDIF
103700040816     C     KEYtbe1_P     READE     tntbe01l
103800040816     C                   ENDDO
103900040816     C* Imposto cosa testare
104000040816     C                   IF        VIDPGT <> *blanks
104100040816     C                   EVAL      wVIDPGT = VIDPGT
104200040816     C                   ELSE
104300040816     C                   EVAL      wVIDPGT = VIDPGTD
104400040816     C                   ENDIF
104500040816     C                   IF        VIDFIT <> *blanks
104600040816     C                   EVAL      wVIDFIT = VIDFIT
104700040816     C                   ELSE
104800040816     C                   EVAL      wVIDFIT = VIDFITD
104900040816     C                   ENDIF
105000040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
105100040823     C                   MOVEL     '0'           wFlgIFTok         1
105200100223     C                   SETOFF                                       16
105300040816     C     wVIDPGT       LOOKUP    skPgt                                  15
105400040816     C                   IF        %found
105500040816     C* Testo se vi è corrispondenza
105600040816     C                   EVAL      j = 1
105700040816     C                   DOW       j <= %elem(skTipF)
105800100223     C                   IF        skTipF(j)  = VIDTIP  AND
105900100223     C                             skPgt(j)   = wVIDPGT
106000100223     C                   SETON                                        16
106100100311     C*
106200100223     C                   IF        skFileT(j) = wVIDFIT
106300040816     C                   MOVEL     '1'           wFlgIFTok
106400100311     C*
106500100311     C                   IF        DEPOP1 = 'O02' OR
106600100311     C                             DEPOP1 = 'O03' OR
106700100311     C                             DEPOP1 = 'F06'
106800100312     C                   IF        VIDtmbd      =  *blanks AND
106900100312     C                             skMembroT(j) <> *blanks
107000100311     C                   EVAL      VIDtmbd = skMembroT(j)
107100100311     C                   SETON                                        61
107200100311     C                   ENDIF
107300100311     C                   ENDIF
107400100311     C*
107500040816     C                   LEAVE
107600040816     C                   ENDIF
107700100311     C*
107800100223     C                   ENDIF
107900040816     C                   ADD       1             j
108000040816     C                   ENDDO
108100040816     C* Se nn trovata esatta corrispondenza => errore
108200100223     C                   IF        *IN16 and wFlgIFTok = '0'
108300040816     C                   MOVEL     '1'           ERRL00
108400040816     C                   MOVEL     MSG(25)       DSMSMS
108500040816     C                   SETON                                        665457
108600040816     C                   ENDIF
108700040816     C                   ENDIF
108800040816     C                   ENDIF
108900040816     C                   ENDIF
109000040823     C                   ENDIF
109100170119     C*--------------------
109200170119     C* Controllo particolare x anagrafiche d download:
109300170119     C* - se indicati parametri di traduzione      => obbligatorio *pgm traduttore
109400170119     C* - se indicati parametri di post-traduzione => obbligatorio *pgm post-traduzione
109500170119     C* - se indicatp *pgm traduttore              => obbligatorio file tradotto
109600170119     C*--------------------
109700170119     C***  ERRL00        IFEQ      '0'
109800170119     C***  §ISVVers      IFEQ      'D'
109900170119     C***                IF        VIDppt <> *blanks AND VIDpgt = *blanks
110000170119     C***                MOVEL     '1'           ERRL00
110100170119     C***                MOVEL     MSG(35)       DSMSMS
110200170119     C***                SETON                                        54
110300170119     C***                ENDIF
110400170119     C*
110500170119     C***                IF        VIDpgt <> *blanks AND VIDfit = *blanks
110600170119     C***                MOVEL     '1'           ERRL00
110700170119     C***                MOVEL     MSG(36)       DSMSMS
110800170119     C***                SETON                                        57
110900170119     C***                ENDIF
111000170119     C*
111100170119     C***                IF        VIDppi <> *blanks AND VIDpgi = *blanks
111200170119     C***                MOVEL     '1'           ERRL00
111300170119     C***                MOVEL     MSG(37)       DSMSMS
111400170119     C***                SETON                                        55
111500170119     C***                ENDIF
111600170119     C*
111700170119     C***                ENDIF
111800170119     C***                ENDIF
111900051026     C*--------------------
112000051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
112100051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
112200051026     C*--------------------
112300051026     C     ERRL00        IFEQ      '0'
112400120109     C     §ISVVers      IFEQ      'D'
112500051026     C     VIDpgt        IFNE      virPGT
112600051026     C     VIDpgi        ORNE      virPGI
112700051026     C                   CALL      'TIS723R1'
112800051026     C                   PARM                    VIDKSC
112900051026     C                   PARM                    VIDTIP
113000120109     C                   PARM                    §ISVVers
113100051026     C                   PARM      *zeros        wRecZero          9 0
113200051026     C                   IF        wRecZero > *zeros
113300051026     C                   MOVEL     '1'           ERRL00
113400051026     C                   MOVEL     MSG(26)       DSMSMS
113500051026     C                   SETON                                        5455
113600051026     C                   ENDIF
113700051026     C                   ENDIF
113800051026     C                   ENDIF
113900051026     C                   ENDIF
114000060616     C*--------------------
114100060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
114200060616     C*--------------------
114300060616     C     ERRL00        IFEQ      '0'
114400060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
114500060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
114600060616     C                   ELSE
114700060616     C                   MOVEL     '1'           ERRL00
114800060616     C                   SETON                                        535556
114900060616     C                   MOVEL     MSG(27)       DSMSMS
115000060616     C                   ENDIF
115100060616     C                   ENDIF
115200081020     C*--------------------
115300081020     C* Mail x eventuale feed-back richiesto
115400081020     C*--------------------
115500081020     C     ERRL00        IFEQ      '0'
115600081020     C                   CLEAR                   DSEMAIL
115700081020     C                   EVAL      §EMLINDI = VIDEML
115800081020     C                   CALL      'XEMAIL'
115900081020     C                   PARM                    DSEMAIL
116000081020     C                   IF        §EMLERRO = '1'
116100081020     C                   MOVEL     '1'           ERRL00
116200081020     C                   SETON                                        64
116300081020     C                   MOVEL     §EMLMSGO      DSMSMS
116400081020     C                   ENDIF
116500081020     C                   ENDIF
116600001120     C*
116700001120     C                   ENDSR
116800000626     C*------------------------------------------------------------------------*
116900000626     C* Cntisv - CONTROLLA codice tipo legame
117000000626     C*------------------------------------------------------------------------*
117100001120     C     cntisv        begsr
117200001120     C                   clear                   TIBS02DS
117300001120     C*
117400101027     C     '?'           SCAN      Vidisv                                 28
117500001120     C*  Ricerca
117600001120     C                   eval      t02tla = 'L'
117700101027     C                   IF        *IN28 = *ON
117800001120     C                   movel     'R'           t02mod
117900001120     C                   else
118000001120     C                   movel     'C'           t02mod
118100001120     C                   movel(p)  vidisv        t02ke1
118200001120     C                   end
118300000626     C                   movel     'ISV'         t02cod
118400001120     C                   movel     knsif         t02sif
118500001120     C                   CALL      'TIBS02R'
118600001120     C                   parm                    KPJBA
118700001120     C                   parm                    TIBS02DS
118800001120     C
118900001120     C*
119000001120     C                   IF        T02err = *blanks
119100000626     C                   movel     T02ke1        Vidisv
119200000626     C                   movel     T02uni        vddisv
119300030603     C                   movel(P)  T02uni        DISV
119400001120     C                   else
119500001120     C                   movel     '1'           errl00
119600001120     C                   seton                                        70
119700001120     C                   MOVEL     MSG(18)       DSMSMS
119800001120     C                   ENDIF
119900001120     C
120000001120     C                   endsr
120100170721     C*------------------------------------------------------------------------*
120200170721     C* CntKE2 - CONTROLLA codice opzione
120300170721     C*------------------------------------------------------------------------*
120400170721     C     cntke2        begsr
120500170721     C                   clear                   TIBS02DS
120600170721     C*
120700170721     C     '?'           SCAN      Vidke2                                 28
120800170721     C*  Ricerca
120900170721     C                   eval      t02tla = 'L'
121000170721     C                   IF        *IN28 = *ON
121100170721     C                   movel     'R'           t02mod
121200170721     C                   else
121300170721     C                   movel     'C'           t02mod
121400170721     C                   movel(p)  vidisv        t02ke1
121500170721     C                   end
121600170721     C                   movel     'VPR'         t02cod
121700170721     c                   movel     'VPCKE2'      T02ke1
121800170721     C* il campo lo imposto se sono entrato in controllo, se sono in ricerca no
121900170721     c                   if        t02mod = 'C'
122000170721     c                   movel     vidke2        T02ke2
122100170721     c                   else
122200170721     c                   clear                   T02ke2
122300170721     c                   endif
122400170721     C                   movel     knsif         t02sif
122500170721     C                   CALL      'TIBS02R'
122600170721     C                   parm                    KPJBA
122700170721     C                   parm                    TIBS02DS
122800170721     C
122900170721     C*
123000170721     C                   IF        T02err = *blanks
123100170721     C* il campo lo imposto se sono entrato in ricerca, se sono in controllo no
123200170721     c                   if        t02mod = 'R'
123300170721     C                   movel     T02ke2        Vidke2
123400170721     c                   endif
123500170721     C                   movel     T02uni        vddke2
123600170721     C                   else
123700170721     C                   movel     '1'           errl00
123800170721     C                   seton                                        48
123900170721     C                   MOVEL     MSG(38)       DSMSMS
124000170721     C                   movel     *all'*'       vddke2
124100170721     C                   ENDIF
124200170721     C
124300170721     C                   endsr
124400991027     C*------------------------------------------------------------------------*
124500991027     C* cnttip - Controlla esistenza tipo file su tivt00f
124600991027     C*------------------------------------------------------------------------*
124700001120     C     cnttip        begsr
124800001120     C                   clear                   TIBS02DS
124900000626     C*
125000101027     C     '?'           SCAN      vidtip                                 28
125100001120     C*  Ricerca
125200001120     C                   eval      t02tla = 'L'
125300101027     C                   IF        *IN28 = *ON
125400001120     C                   movel     'R'           t02mod
125500001120     C                   else
125600001120     C                   movel     'C'           t02mod
125700001120     C                   movel(p)  vidtip        t02ke1
125800001120     C                   end
125900991108     C                   movel     'ITF'         t02cod
126000001120     C                   movel     knsif         t02sif
126100991108     C                   CALL      'TIBS02R'
126200001120     C                   parm                    KPJBA
126300001120     C                   parm                    TIBS02DS
126400001120     C
126500001120     C*
126600001120     C                   IF        T02err = *blanks
126700991108     C                   movel     T02ke1        vidtip
126800020218     C                   movel     T02uni        DITF
126900020218     C                   movel     §ITFDESC      vddtip
127000021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
127100021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
127200021014     C                   if        vidfit = *blanks
127300021014     C                   movel     §ITFLIBD      vidfi2
127400021014     C                   endif
127500001120     C                   else
127600001120     C                   movel     '1'           errl00
127700991108     C                   movel     *blank        vddtip
127800111215     C* pulisco anche il cod. tabella associata
127900111215     C                   movel     *blank        vidtab
128000001120     C                   seton                                        66
128100991108     C                   MOVEL     msg(11)       DSMSMS
128200991108     C                   ENDIF
128300001120     C                   if        errl00 = '0'
128400120109     C     vidtip        chain     tivtf01l                           97
128500001120     C                   if        not *in97
128600001120     C                   movel     vtfpgt        vidpgtd
128700001120     C                   movel     vtfpgi        vidpgid
128800001120     C                   movel     vtffid        vidfidd
128900001120     C                   movel     vtffis        vidfisd
129000001120     C                   movel     vtffit        vidfitd
129100111214     C                   movel     vtffi1        vidtab
129200101013     C                   movel     vtfppi        DVTFPPI
129300101026     C                   if        DEPOP1 = 'F06' OR
129400101026     C                             DEPOP1 = 'O03'
129500101026     C                   select
129600101026     C                   when      vtfUD = 'U'
129700101026     C                   movel     D§VTFPPIMF    vidppiMC
129800101026     C                   when      vtfUD = 'D'
129900101026     C                   movel     D§VTFPPIMF    vidppiMF
130000101026     C                   endsl
130100101026     C                   movel     D§VTFPPIMS    vidppiMS
130200101026     C                   movel     D§VTFPPIMZ    vidppiMZ
130300101026     C                   endif
130400100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
130500001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
130600100308     C                   endif
130700001120     C                   else
130800001120     C                   movel     *blank        vidpgtd
130900001120     C                   movel     *blank        vidpgid
131000001120     C                   movel     *blank        vidfidd
131100001120     C                   movel     *blank        vidfisd
131200001120     C                   movel     *blank        vidfitd
131300001120     C                   movel     *blank        vidfi4d
131400001120     C                   endif
131500001120     C                   endif
131600001120     C*
131700001120     C                   endsr
131800991027     C*------------------------------------------------------------------------*
131900120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
132000991027     C*------------------------------------------------------------------------*
132100120109     C     CntTip234     BEGSR
132200120119     C*
132300120119     C* se ho indicato un cliente devo indicare il tipo file
132400120119     C                   IF        Kscs(i) <>*blank and
132500120119     C                             Kscs(i) <>*zeros and
132600120119     C                             Tips(i) = *blank
132700120119     C                   SELECT
132800120119     C                   WHEN      i = 1
132900120119     C                   SETON                                        7549
133000120119     C                   WHEN      i = 2
133100120119     C                   SETON                                        7650
133200120119     C                   WHEN      i = 3
133300120119     C                   SETON                                        7751
133400120119     C                   ENDSL
133500120119     C                   MOVEL     '1'           ERRL00
133600120119     C                   MOVEL     MSG(34)       DSMSMS
133700120119     C                   LEAVESR
133800120119     C                   ENDIF
133900010913     C*
134000120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
134100120119     C                   IF        Tips(i) = VIDTip and
134200120119     C                             Kscs(i) = VIDKsc
134300120111     C                   SELECT
134400120111     C                   WHEN      i = 1
134500120119     C                   SETON                                        7549
134600120111     C                   WHEN      i = 2
134700120119     C                   SETON                                        7650
134800120111     C                   WHEN      i = 3
134900120119     C                   SETON                                        7751
135000120111     C                   ENDSL
135100120111     C                   MOVEL     '1'           ERRL00
135200120111     C                   MOVEL     MSG(33)       DSMSMS
135300120111     C                   LEAVESR
135400120111     C                   ENDIF
135500120111     C*
135600120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
135700120119     C* indicato
135800120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
135900120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
136000120119     C                   EVAL      x_VIRKsc = Kscs(i)
136100120111     C                   EVAL      x_VIRTip = Tips(i)
136200120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
136300120111     C     K03VIR06      SETLL     TIVIR06L
136400120111     C     K02VIR06      READE     TIVIR06L
136500120111     C                   if        %eof
136600120109     C                   SELECT
136700120109     C                   WHEN      i = 1
136800120109     C                   SETON                                        75
136900120109     C                   WHEN      i = 2
137000120109     C                   SETON                                        76
137100120109     C                   WHEN      i = 3
137200120109     C                   SETON                                        77
137300120109     C                   ENDSL
137400120109     C                   MOVEL     '1'           ERRL00
137500120109     C                   MOVEL     MSG(31)       DSMSMS
137600120119     C* se il tipo file / cliente per duplica esiste,
137700120119     C* questo non deve contenere nelle sue coppie per duplica
137800120119     C* il tipo file / cliente in gestione
137900120111     C                   ELSE
138000120119     C*
138100120120     C                   if        TIS723R2U_Ctrl_Chain_Dup(
138200120120     C                              VIDTip       :
138300120120     C                              VIDKsc       :
138400120120     C                              Tips(i)      :
138500120120     C                              Kscs(i)      ) = '1'
138600120120     C                   SELECT
138700120120     C                   WHEN      i = 1
138800120120     C                   SETON                                        7549
138900120120     C                   WHEN      i = 2
139000120120     C                   SETON                                        7650
139100120120     C                   WHEN      i = 3
139200120120     C                   SETON                                        7751
139300120120     C                   ENDSL
139400120120     C                   MOVEL     '1'           ERRL00
139500120120     C                   MOVEL     MSG(32)       DSMSMS
139600120120     C                   ENDIF
139700120120     C***
139800120120     C***                MOVEL     x_VIRPTH      DVIR01
139900120120     C***                IF        §VIR01Tip2 = VIDTip and
140000120120     C***                          §VIR01Ksc2 = VIDKsc     or
140100120120     C***                          §VIR01Tip3 = VIDTip and
140200120120     C***                          §VIR01Ksc3 = VIDKsc     or
140300120120     C***                          §VIR01Tip4 = VIDTip and
140400120120     C***                          §VIR01Ksc4 = VIDKsc
140500120120     C***                SELECT
140600120120     C***                WHEN      i = 1
140700120120     C***                SETON                                        7549
140800120120     C***                WHEN      i = 2
140900120120     C***                SETON                                        7650
141000120120     C***                WHEN      i = 3
141100120120     C***                SETON                                        7751
141200120120     C***                ENDSL
141300120120     C***                MOVEL     '1'           ERRL00
141400120120     C***                MOVEL     MSG(32)       DSMSMS
141500120120     C***                ENDIF
141600120120     C***
141700120111     C                   ENDIF
141800120109     C*
141900120109     C                   ENDSR
142000120109     C*------------------------------------------------------------------------*
142100120109     C* cntksc - Controlla esistenza codice
142200120109     C*------------------------------------------------------------------------*
142300120109     C     cntksc        begsr
142400120109     C*
142500010913     C     vidksc        chain     tianv01l
142600010913     C                   if        %found
142700010913     C                   movel     ANVDES        vddksc
142800010913     C                   else
142900001120     C                   move      vidksc        wcli              7 0
143000991210     C                   CLEAR                   BS69DS
143100991210     C                   CLEAR                   ACODS
143200991210     C                   MOVEL     KNSIF         I69SIF
143300991210     C                   Z-ADD     WCLI          I69KAC
143400991210     C                   CALL      'TIBS69R'
143500991210     C                   PARM                    BS69DS
143600991210     C                   PARM                    ACODS
143700991210     C                   PARM                    INDDS
143800991210     C                   PARM                    CLPDS
143900991210     C                   PARM                    CLSDS
144000991210     C     O69ERR        IFNE      '1'
144100991210     C                   MOVEL     ACORAG        vddksc
144200991210     C                   ELSE
144300991210     C                   MOVEL     *ALL'*'       vddksc
144400010913     C                   MOVE      '1'           errl00
144500010913     C                   MOVEL     msg(11)       dsmsms
144600010913     C                   ENDIF
144700000727     C                   ENDIF
144800010913     C*
144900010913     C                   ENDSR
145000991027     C*------------------------------------------------------------------------*
145100040901     C* cntpoi - Controlla codice filiale
145200991027     C*------------------------------------------------------------------------*
145300001120     C     cntpoi        begsr
145400001120     C*
145500010529     C                   if        vidpoi <> '046' and
145600020809     C                             vidpoi <> '999' and
145700020902     C                             vidpoi <> '500' and
145800020902     C                             vidpoi <> '789'
145900001120     C*
146000001120     C                   move      *blank        vddpoi
146100001120     C     '?'           scan      vidpoi                                 97
146200001120     C                   if        *in97
146300001120     C                   movel     *blank        cod1
146400001120     C                   movel     *blank        des1
146500001120     C                   movel     *blank        pra1
146600001120     C                   call      'TNSD24R'
146700001120     C                   parm                    cod1              3
146800001120     C                   parm                    tip1              1
146900001120     C                   parm                    des1             25
147000001120     C                   parm                    pra1              3
147100001120     C                   movel     cod1          vidpoi
147200001120     C                   endif
147300001120     C                   move      vidpoi        cod1n             3 0
147400001120     C     cod1n         chain     azorg01l                           98
147500001120     C                   if        not *in98 and orgfag <>'V'
147600001120     C                   movel     orgdes        vddpoi
147700001120     C                   else
147800001120     C                   seton                                        69
147900001120     C                   move      '1'           errl00
148000001120     C                   movel     msg(13)       dsmsms
148100001120     C                   endif
148200001120     C*
148300001120     C                   else
148400001120     C                   movel     'SEDE BOLOGNA'vddpoi
148500001120     C                   endif
148600001120     C*
148700001120     C                   endsr
148800991210     C*------------------------------------------------------------------------*
148900991210     C* FINSR - OPERAZIONI FINALI
149000991210     C*------------------------------------------------------------------------*
149100991210     C     FINSR         BEGSR
149200991210     C*
149300991210     C* CHIUDE I FILE DEI PGM CHIAMATI
149400991210     C                   CLEAR                   bs69ds
149500991210     C                   CLEAR                   acods
149600991210     C                   CLEAR                   indds
149700991210     C                   CLEAR                   clpds
149800991210     C                   CLEAR                   clsds
149900991210     C                   MOVEL     'C'           i69tla
150000991210     C                   MOVEL     knsif         i69sif
150100991210     C                   CALL      'TIBS69R'
150200991210     C                   PARM                    bs69ds
150300991210     C                   PARM                    acods
150400991210     C                   PARM                    indds
150500991210     C                   PARM                    clpds
150600991210     C                   PARM                    clsds
150700991210     C*
150800991210     C                   ENDSR
150900991026     C*------------------------------------------------------------------------*
151000991026     C* CHKDA8 - CONTROLLA LA DATA (8)
151100991026     C*------------------------------------------------------------------------*
151200991026     C     CHKDA8        BEGSR
151300991026     C*
151400991026     C     *LIKE         DEFINE    VIDDti        WDA8
151500991026     C     *LIKE         DEFINE    VIDDti        DA8W
151600991026     C*
151700991026     C                   Z-ADD     WDA8          G08DAT
151800991026     C                   Z-ADD     *ZEROS        G08INV
151900991026     C                   MOVEL     '0'           G08ERR
152000991026     C                   CALL      'XSRDA8'
152100991026     C                   PARM                    WLBDA8
152200991026     C                   Z-ADD     G08DAT        WDA8
152300991026     C                   Z-ADD     G08INV        DA8W
152400991026IF  1C     G08ERR        IFEQ      '1'
152500991026     C                   MOVEL     '1'           ERRL00
152600991026E   1C                   ENDIF
152700991026     C*
152800991026     C                   ENDSR
152900970526     C*------------------------------------------------------------------------*
153000970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
153100970526     C*------------------------------------------------------------------------*
153200970526     C     IREVID        BEGSR
153300080604     C*
153400080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
153500080604     C                   IF        §vire01INV = 'S'
153600080604     C                   EVAL      VIDEML = §vire01EML
153700080604     C                   ENDIF
153800970526     C* CAMPI
153900991026     C                   MOVEL     virtip        VIDtip
154000020218     C                   MOVE      virksc        vidksc
154100020218     C                   Z-ADD     virseq        vidseq
154200060308     C                   IF        DEPOP1 = 'O03'
154300060308     C                   MOVEL     *blanks       vidpoi
154400060308     C                   ELSE
154500060308     C                   MOVE      virpoi        vidpoi
154600060308     C                   ENDIF
154700991026     C                   MOVEL     virdes        VIDdes
154800991026     C                   MOVEL     virpgt        VIDpgt
154900991026     C                   MOVEL     virppt        VIDppt
155000991026     C                   MOVEL     virpgi        VIDpgi
155100991026     C                   MOVEL     virppi        VIDppi
155200991026     C                   MOVEL     virfid        VIDfid
155300991026     C                   MOVEL     virfis        VIDfis
155400991026     C                   MOVEL     virfit        VIDfit
155500000710     C                   MOVEL     virfi3        VIDfi3
155600000710     C                   MOVEL     virfi4        VIDfi4
155700010123     C                   MOVEL     virfi2        VIDfi2
155800030526     C                   MOVEL     virpth        DVIR01
155900030526     C                   MOVEL     §VIR01TM      VIDtmbd
156000120109     C                   MOVEL     §VIR01Tip2    VIDTip2
156100120109     C                   MOVEL     §VIR01Tip3    VIDTip3
156200120109     C                   MOVEL     §VIR01Tip4    VIDTip4
156300120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
156400120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
156500120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
156600000710  ***C*                  MOVEL     virctl        VIDctl
156700991026     C                   Z-ADD     virdti        G08INV
156800991026     C                   Z-ADD     *ZEROS        G08DAT
156900991026     C                   MOVEL     '3'           G08ERR
157000991026     C                   CALL      'XSRDA8'
157100991026     C                   PARM                    WLBDA8
157200991026     C                   Z-ADD     G08DAT        viddti
157300970527     C*
157400991026     C                   Z-ADD     virdtf        G08INV
157500991026     C                   Z-ADD     *ZEROS        G08DAT
157600991026     C                   MOVEL     '3'           G08ERR
157700991026     C                   CALL      'XSRDA8'
157800991026     C                   PARM                    WLBDA8
157900991026     C                   Z-ADD     G08DAT        viddtf
158000020218     C                   MOVEL     virfi1        vidisv
158100170721     C                   MOVEL     §vir01vpco    vidke2
158200051122     C*
158300051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
158400051122     C                   IF        DEPOP1 = 'F06' OR
158500051122     C                             DEPOP1 = 'O03' OR
158600051122     C                             DEPOP1 = 'O07'
158700051122     C                   EVAL      VIDDTI = DATCORINV
158800060726     C                   EVAL      VIDDTF = 31122039
158900051122     C                   ENDIF
159000020218     C*
159100970526     C                   ENDSR
159200970526     C*------------------------------------------------------------------------*
159300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
159400970526     C*------------------------------------------------------------------------*
159500970526     C     AZZVID        BEGSR
159600970526     C* CAMPI
159700991026     C                   MOVEL     *blank        VIDksc
159800000710  ***C*                  MOVEL     *blank        VIDpt1
159900000710  ***C*                  MOVEL     *blank        VIDpt2
160000010123     C                   z-add     0             vidseq
160100010123     C                   move      *zeros        vidpoi
160200060726     C                   EVAL      VIDDTI = DATCORINV
160300060726     C                   EVAL      VIDDTF = 31122039
160400991025     C                   MOVEL     *blank        VIDtip
160500991025     C                   MOVEL     *blank        VIDdes
160600991025     C                   MOVEL     *blank        VIDpgt
160700991025     C                   MOVEL     *blank        VIDppt
160800991025     C                   MOVEL     *blank        VIDpgi
160900991025     C                   MOVEL     *blank        VIDppi
161000991025     C                   MOVEL     *blank        VIDfid
161100991025     C                   MOVEL     *blank        VIDfis
161200991025     C                   MOVEL     *blank        VIDfit
161300000710     C                   MOVEL     *blank        VIDfi3
161400000710     C                   MOVEL     *blank        VIDfi4
161500010123     C                   MOVEL     *blank        VIDfi2
161600000710  ***C*                  MOVEL     *blank        VIDctl
161700080603     C                   MOVEL     *blank        VIDeml
161800970526     C*
161900970526     C                   ENDSR
162000970526     C*------------------------------------------------------------------------*
162100991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
162200970526     C*------------------------------------------------------------------------*
162300991026     C     IMPvir        BEGSR
162400970526     C*
162500991027     C                   move      VIDpoi        virpoi
162600991026     C                   movel     VIDdes        virdes
162700991026     C                   movel     VIDpgt        virpgt
162800991026     C                   movel     VIDppt        virppt
162900991026     C                   movel     VIDpgi        virpgi
163000991026     C                   movel     VIDppi        virppi
163100991026     C                   movel     VIDfid        virfid
163200991026     C                   movel     VIDfis        virfis
163300991026     C                   movel     VIDfit        virfit
163400000710     C                   movel     VIDfi3        virfi3
163500000710     C                   movel     VIDfi4        virfi4
163600010123     C                   movel     VIDfi2        virfi2
163700030526     C                   MOVEL     VIDtmbd       §VIR01TM
163800120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
163900120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
164000120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
164100120119     C                   IF        VIDKsc2 = *zeros
164200120119     C                   EVAL      VIDKsc2 = *blank
164300120119     C                   ENDIF
164400120119     C                   IF        VIDKsc3 = *zeros
164500120119     C                   EVAL      VIDKsc3 = *blank
164600120119     C                   ENDIF
164700120119     C                   IF        VIDKsc4 = *zeros
164800120119     C                   EVAL      VIDKsc4 = *blank
164900120119     C                   ENDIF
165000120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
165100120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
165200120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
165300170721     C                   movel     VIDke2        §vir01vpco
165400030526     C                   MOVEL     DVIR01        virpth
165500000710  ***C*                  movel     VIDctl        virctl
165600991026     C                   Z-ADD     viddtf        G08dat
165700991026     C                   Z-ADD     *ZEROS        G08inv
165800991026     C                   MOVEL     '0'           G08ERR
165900991026     C                   CALL      'XSRDA8'
166000991026     C                   PARM                    WLBDA8
166100991026     C                   Z-ADD     G08inv        virdtf
166200000628     C                   movel     VIDisv        virfi1
166300970526     C*
166400970526     C                   ENDSR
166500970526     C*------------------------------------------------------------------------*
166600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
166700970526     C*------------------------------------------------------------------------*
166800970526     C     IMPFUN        BEGSR
166900101013     C*
167000101026     C                   SETOFF                                       44
167100101026     C                   SETOFF                                       30
167200101026     C                   SETOFF                                       31
167300101026     C                   SETOFF                                       32
167400101026     C                   SETOFF                                       33
167500970526     C*
167600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
167700970526IF  1C     DEPOP1        IFEQ      'O05'
167800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
167900970526     C                   MOVEL     'N'           FUDATT
168000970526     C                   MOVEL     FUDRIG        FUD(6)
168100970526E   1C                   ENDIF
168200141111     C*
168300141111     C* TASTO FUNZIONE F02 ATTIVO SOLO PER OPZIONI "DISPOSITIVE"
168400141111     C                   IF        DEPOP1 = 'O02' OR
168500141111     C                             DEPOP1 = 'O03' OR
168600141111     C                             DEPOP1 = 'F06'
168700141111     C                   MOVEL     FUD(2)        FUDRIG                         *PARAMETRI EXT.
168800141111     C                   MOVEL     'S'           FUDATT
168900141111     C                   MOVEL     FUDRIG        FUD(2)
169000141111     C                   ENDIF
169100970526     C*
169200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
169300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
169400970526     C                   EXSR      FUNGES
169500970526     C*
169600970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
169700970526     C*---
169800970526     C* 90 -> PROTEZIONE CHIAVE
169900970526     C* 91 -> PROTEZIONE TOTALE
170000970526     C*---
170100970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
170200970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
170300970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
170400970526     C                   SETOFF                                       91
170500970526E   2C                   ENDIF
170600101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
170700101013E   2C                   ENDIF
170800970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
170900991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
171000970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
171100970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
171200970526E   2C                   ENDIF
171300970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
171400970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
171500970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
171600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
171700970526     C                   SETON                                        91
171800970526E   2C                   ENDIF
171900970526X   1C                   ELSE                                                   *I N T E R R O G A
172000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
172100970526     C                   SETON                                        91
172200970526E   1C                   ENDIF
172300970526     C*
172400970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
172500970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
172600991025     C                   MOVEL     S20tip        KCEtip
172700991026     C                   MOVEL     S20ksc        KCEksc
172800000420     C     KEYvi2        CHAIN     tivir03L                           9998
172900080603IF  2C     *IN99         IFEQ      *ON
173000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
173100970526     C                   MOVEL     MSG(6)        DSMSMS
173200970526X   2C                   ELSE
173300080603     C*
173400080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
173500080603     C                   EVAL      KCEdti  = virDTI
173600080603     C                   EVAL      vireTRC = '01'
173700080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
173800080603     C                   IF        %found(tivire1l)
173900080603     C                   EVAL      DVIRE01 = vireFLO
174000080603     C                   ENDIF
174100150114     C*
174200150114     C* Aggancio anche il record d estensione parametri clienti/tipi-file => TRC PE
174300150114     C                   SETOFF                                       71
174400150114     C                   EVAL      KCEdti  = virDTI
174500150114     C                   EVAL      vireTRC = 'PE'
174600150114     C     KEYvire1_C    CHAIN     tivire1l
174700150114     C                   IF        %found(tivire1l)
174800150114     C                   SETON                                        71
174900150114     C                   ENDIF
175000080603     C*
175100080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
175200980128IF  3C     DEPOP1        IFEQ      'O05'
175300991014IF  3C     DEPOP1        oreq      'O02'
175400101026     C*
175500101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
175600101026     C     virFI1        lookup    isvftpeml                              80
175700101026     C                   IF        *IN80
175800111215     C                             and AS400_ok = *on
175900101026      /FREE
176000101026
176100130215       // (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
176200130215       //  visualizzazione se l'utente è di sede;
176300130215       //  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione
176400130215       //  quindi non potrebbero comunque)
176500130215       if DUtLPO = 'S';
176600101026         if UBBVSQL01_RtvCliTipFile(
176700101026                         pInConn
176800101026                       : virKSC
176900101026                       : virTIP
177000101026                       : pOutNumRec
177100101026                       : pOutCliTipTras
177200101026                       : pOutCliMskChk
177300101026                       : pOutCliMskFil
177400101026                       : pOutCliMskSem
177500101026                       : pOutCliPerc
177600101026                       : pOutCliEml1
177700101026                       : pOutCliEml2
177800101026                       : pOutCliEml3
177900101026                       : pOutCliEml4
178000101026                       : pOutCliEml5
178100101026                       : pOutCliSogg
178200101026                       : pOutCliTesto
178300101026                       : pOutCliFtpAddr
178400101026                       : pOutCliFtpDir
178500101026                       : pOutCliFtpUsr
178600101026                       : pOutCliFtpPwd
178700101026                       : pOutCliFtpFAS4
178800101026                       : pOutCliPercOld
178900101026                       : pOutCliTentat
179000101026                       : pOutCliRitardo
179100101026                       : pOutCliLibAS4
179200101026                       : pOutCliFilAS4
179300101026                       : pOutCliMbrAS4
179400101026                       : pOutCliProc
179500101026                       : pOutCliSched
179600101026                       : pOutCliAccorp
179700101026                       : pOutCliMskZip
179800101026                       : pOutCliDataStr
179900101026                       : pOutCliDataEnd
180000101026                       : pOutCliCodComp
180100101026                       : pOutCliFtpCmd1
180200101026                       : pOutCliEmlErr
180300101026                       : pOutCliHtUrlTo
180400101026                       : pOutCliHtUsrTo
180500101026                       : pOutCliHtPwdTo
180600101026                       : pOutCliHtConTy
180700101026                       : pOutCliHtSndEx
180800101026                       : pOutCliHtSndPr
180900101026                       : pOutCliHtUsPxy
181000101026                       : pOutCliUsrIns
181100101026                       : pOutCliUsrUpd
181200101026                       : pOutCliDatUpd
181300101026                       : pOutCliNote
181400101026                       : pOutCliFtpPrt
181500101026                       : pOutCliFtpTy) = 0;
181600101026              if pOutNumRec = 1;
181700101026                 VIDPPIMC = pOutCliMskChk;
181800101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
181900101026                    *in30 = *on;
182000101026                 endif;
182100101026                 VIDPPIMF = pOutCliMskFil;
182200101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
182300101026                    *in31 = *on;
182400101026                 endif;
182500101026                 VIDPPIMS = pOutCliMskSem;
182600101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
182700101026                    *in32 = *on;
182800101026                 endif;
182900101026                 VIDPPIMZ = pOutCliMskZip;
183000101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
183100101026                    *in33 = *on;
183200101026                 endif;
183300101026              endif;
183400101026         endif;
183500130214       endif;
183600101026
183700101026      /END-FREE
183800101026     C
183900101026     C                   ELSE
184000101026     C                   SETON                                        44
184100101026     C                   ENDIF
184200101026     C*
184300980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
184400980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
184500980128E   3C                   ENDIF
184600970526E   2C                   ENDIF
184700970526X   1C                   ELSE                                                   *INSERIMENTO
184800970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
184900970526E   1C                   ENDIF
185000970526     C*
185100970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
185200970526IF  1C     DEPOP1        IFEQ      'O02'
185300970526     C                   MOVEL     DEC(1)        VIDFUN
185400970526E   1C                   ENDIF
185500991014IF  1C     DEPOP1        IFEQ      'O03'
185600991014     C                   MOVEL     DEC(6)        VIDFUN
185700991014E   1C                   ENDIF
185800970526IF  1C     DEPOP1        IFEQ      'O04'
185900970526     C                   MOVEL     DEC(2)        VIDFUN
186000970526E   1C                   ENDIF
186100970526IF  1C     DEPOP1        IFEQ      'O05'
186200970526     C                   MOVEL     DEC(3)        VIDFUN
186300970526E   1C                   ENDIF
186400970526IF  1C     DEPOP1        IFEQ      'O07'
186500970526     C                   MOVEL     DEC(4)        VIDFUN
186600970526E   1C                   ENDIF
186700970526IF  1C     DEPOP1        IFEQ      'F06'
186800970526     C                   MOVEL     DEC(5)        VIDFUN
186900970526E   1C                   ENDIF
187000970526     C*
187100970526     C                   ENDSR
187200101026
187300101026
187400101026
187500101026     C*--------------------------------------------------------------------------------------------*
187600101026     C* CARTAB - carica le tabelle occorrenti
187700101026     C*--------------------------------------------------------------------------------------------*
187800101026     C     CARTAB        BEGSR
187900101026     C*
188000101026     C* Tipi servizi soggetti a controllo unificazioni
188100101026     C                   z-add     *zeros        ww                3 0
188200101026     C                   eval      tbecod = 'ISV'
188300101026     C     tbecod        setll     tntbe01l
188400101026     C     tbecod        reade     tntbe01l
188500101026     C                   dow       not %eof(tntbe01l)
188600101026     C                   movel     tbeuni        disv
188700101026     C                   if        §isvproc = 'F'
188800101026     C                   add       1             ww
188900101026     C                   eval      isvftpeml(ww) = tbeke1
189000101026     C                   endif
189100101026     C     tbecod        reade     tntbe01l
189200101026     C                   enddo
189300101026     C*
189400101026     C                   ENDSR
189500101026
189600101026     c*------------------------------------------------------------------------*
189700101026     c* opnCONN - Apertura connessione
189800101026     c*------------------------------------------------------------------------*
189900101026     c     OPNCONN       BEGSR
190000101026      /FREE
190100101026
190200101026        if not *in40;
190300101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
190400101026              pInConn = pOutConn;
190500101026           else;
190600101026              *in40 = *on;
190700101026              DSMSMS = MSG(28);
190800101026           endif;
190900101026        endif;
191000101026
191100101026      /END-FREE
191200101026      *
191300101026     c                   ENDSR
191400101026     c*------------------------------------------------------------------------*
191500101026
191600101026
191700101026
191800101026     c*------------------------------------------------------------------------*
191900101026     c* clsCONN - Chiusura connessione
192000101026     c*------------------------------------------------------------------------*
192100101026     c     CLSCONN       BEGSR
192200101026      /FREE
192300101026
192400101026           if UBBVSQL01_Disconnect(pInConn) = 0;
192500101026           else;
192600101026              *in40 = *on;
192700101026           endif;
192800101026
192900101026      /END-FREE
193000101026      *
193100101026     c                   ENDSR
193200101026     c*------------------------------------------------------------------------*
193300101026
193400101026
193500101026
193600970526     C*------------------------------------------------------------------------*
193700970526     C* *INZSR - OPERAZIONI INIZIALI
193800970526     C*------------------------------------------------------------------------*
193900970526     C     *INZSR        BEGSR
194000970526     C*---
194100970526     C* RICEVIMENTO PARAMETRI
194200970526     C*---
194300970526     C     *ENTRY        PLIST
194400970612     C                   PARM                    KPJBA
194500991028     C                   movel     kpjbu         tis720
194600000420     C*
194700000420     C* Inverto subito la data presa in input
194800000420     C                   Z-ADD     s20dti        wda8
194900000420     C                   Z-ADD     *ZEROS        da8w
195000000420     C                   EXSR      CHKDA8
195100000420     C                   Z-ADD     DA8W          KCEdti
195200130215     C*
195300130215     C     *DTAARA       DEFINE    §azute        azuteds
195400130215     C     *DTAARA       DEFINE    §datiute      ddatiute
195500130215     C                   IN(E)     *DTAARA
195600130215if  1C                   IF        %Error
195700130215     c                   EVAL      I34Tla = 'L'
195800130215     C                   CALL      'TIBS34R'
195900130215     C                   PARM                    Tibs34Ds
196000130215     C                   IN        *DTAARA
196100130215e   1C                   ENDIF
196200970526     C*
196300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
196400970526     C     XMSGPA        PLIST
196500970526     C                   PARM                    DSMSMS
196600970526     C                   PARM                    DSMSPG
196700970526     C                   PARM                    DSMSKE
196800970526     C                   PARM                    DSMSER
196900991108     C*---
197000970526     C*
197100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
197200970526     C                   CALL      'XKEY'
197300970526     C                   PARM                    DSKEY
197400101026     C*
197500101026     C* CARICA LE TABELLE OCCORRENTI
197600101027     C                   EXSR      CARTAB
197700111215     C*
197800111215     C* Reperisco il sistema AS/400 corrente
197900111215     C                   EVAL      AS400_ok = *off
198000111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
198100111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
198200111215     C                   EVAL      AS400_ok = *on
198300111215     C                   ENDIF
198400101026     C*
198500130214     C* Apro la connessione al DB "BARTVAS" (solo se no solo visualizzazione)
198600130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
198700130215     C*  visualizzazione se l'utente è di sede;
198800130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
198900130215     C*  quindi non potrebbero comunque)
199000130215     C                   IF        DUtLPO = 'S'
199100111215     C                   IF        AS400_ok = *on
199200111215     C                   exsr      opnCONN
199300111215     C                   ENDIF
199400130214     C                   ENDIF
199500970526     C*---
199600970526     C* VARIABILI NON RIFERITE AL DATA BASE
199700970526     C*---
199800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
199900991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
200000991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
200100991025     C     *LIKE         DEFINE    S20OP0        WOP0
200200991025     C     *LIKE         DEFINE    S20OP1        WOP1
200300970526     C*---
200400970526     C* VARIABILI DI CONTROLLO
200500970526     C*---
200600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
200700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
200800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
200900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
201000970526     C*
201100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
201200991026     C                   MOVEL     'TIS723R'     DSMSPG
201300991026     C                   MOVEL     'TIS723R'     MSGQUE
201400970526     C                   MOVEL     *BLANKS       MSGKEY
201500970526     C                   MOVEL     *BLANKS       DSMSMS
201600970526     C*---
201700970526     C* VARIABILI RIFERITE AL DATA BASE
201800970526     C*---
201900991026     C     *LIKE         DEFINE    virtip        KCEtip
202000991026     C     *LIKE         DEFINE    virksc        KCEksc
202100000420     C     *LIKE         DEFINE    virdti        KCEdti
202200970526     C*---
202300970526     C* CHIAVI DI LETTURA
202400970526     C*---
202500000420     C* LETTURA tivir03L - INCOMPLETA
202600991026     C     KEYvir        KLIST
202700080603     C                   KFLD                    KCEtip
202800080603     C                   KFLD                    KCEksc
202900080603     C* LETTURA tivir03L - COMPLETA
203000080603     C     KEYvi2        KLIST
203100080603     C                   KFLD                    KCEtip
203200080603     C                   KFLD                    KCEksc
203300080603     C                   KFLD                    KCEdti
203400080603     C* LETTURA tivire1L - COMPLETA
203500080603     C     KEYvire1_C    KLIST
203600080603     C                   KFLD                    KCEksc
203700080603     C                   KFLD                    KCEtip
203800080603     C                   KFLD                    KCEdti
203900080603     C                   KFLD                    vireTRC
204000080603     C* LETTURA tntbe01L - PARZIALE
204100040816     C     KEYtbe1_P     KLIST
204200040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
204300120111     C* LETTURA tivir06L - COMPLETA
204400120111     C     K03VIR06      KLIST
204500120111     C                   KFLD                    x_VIRKsc
204600120111     C                   KFLD                    x_VIRTip
204700120111     C                   KFLD                    x_VIRDtF
204800120111     C* LETTURA tivir06L - PARZIALE
204900120111     C     K02VIR06      KLIST
205000120111     C                   KFLD                    x_VIRKsc
205100120111     C                   KFLD                    x_VIRTip
205200970528     C*
205300970528     C* CALCOLA LA DATA CORRENTE
205400970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
205500970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
205600970528     C                   Z-ADD     WN8           G08DAT
205700051122     C                   Z-ADD     WN8           DATCORINV         8 0
205800970528     C                   Z-ADD     *ZEROS        G08INV
205900970528     C                   MOVEL     '0'           G08ERR
206000970528     C                   CALL      'XSRDA8'
206100970528     C                   PARM                    WLBDA8
206200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
206300970526     C*
206400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
206500991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
206600991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
206700051122     C*
206800051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
206900051122     C                   IF        DEPOP1 = 'F06' OR
207000051122     C                             DEPOP1 = 'O03' OR
207100051122     C                             DEPOP1 = 'O07'
207200051122     C                   EVAL      VIDDTI = DATCORINV
207300060726     C                   EVAL      VIDDTF = 31122039
207400051122     C                   ENDIF
207500000629     C*
207600000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
207700000629     C                   IF        DEPOP1 = 'F06' OR
207800000629     C                             DEPOP1 = 'O03'
207900000629     C                   SETOFF                                       92
208000000629     C                   ELSE
208100000629     C                   SETON                                        92
208200000629     C                   ENDIF
208300970526     C*
208400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
208500970526     C                   EXSR      IMPFUN
208600970526     C*
208700970526     C                   ENDSR
208800970526     C*------------------------------------------------------------------------*
208900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
209000970526     C*------------------------------------------------------------------------*
209100970526     C     FUNGES        BEGSR
209200970526     C*
209300970526     C* PULISCO CAMPI DI LAVORO
209400970526     C                   Z-ADD     0             W                 5 0
209500970526     C*
209600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
209700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
209800970526     C     1             DO        24            I
209900970526     C                   MOVEL     FUD(I)        FUDRIG
210000970526     C*
210100970526     C* PULIZIA FUNZIONI PRECEDENTI
210200970526     C                   MOVEL     *BLANKS       FUV(I)
210300970526     C*
210400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
210500970526     C* (SALTA F24)
210600970526     C     FUDATT        IFEQ      'S'
210700970526     C     FUDFUN        ANDNE     '24'
210800970526     C                   ADD       1             W
210900970526     C                   MOVEL     FUDDES        FUV(W)
211000970526     C                   END
211100970526     C                   END                                                    DO WHILE
211200970526     C*
211300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
211400970526     C* ALTRIMENTI DISATTIVO F24
211500970526     C     1             DO        24            I
211600970526     C                   MOVEL     FUD(I)        FUDRIG
211700970526     C*
211800970526     C     FUDFUN        IFEQ      '24'
211900970526     C     W             IFGT      6
212000970526     C                   MOVEL     'S'           FUDATT
212100970526     C                   ELSE
212200970526     C                   MOVEL     *BLANKS       FUDATT
212300970526     C                   END                                                    W GT 8
212400970526     C                   MOVEL     FUDRIG        FUD(I)
212500970526     C                   END
212600970526     C                   END                                                    DO WHILE
212700970526     C*
212800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
212900970526     C     W             DIV       6             I
213000970526     C                   MVR                     Z
213100970526     C*
213200970526     C     Z             IFNE      0
213300970526     C     I             ADD       1             Z
213400970526     C                   ELSE
213500970526     C                   Z-ADD     I             Z                 5 0
213600970526     C                   END
213700970526     C*
213800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
213900970526     C                   ADD       1             FUNGRU
214000970526     C     FUNGRU        IFGT      Z
214100970526     C     FUNGRU        ORLT      1
214200970526     C                   Z-ADD     1             FUNGRU
214300970526     C                   END
214400970526     C*
214500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
214600970526     C     FUNGRU        IFEQ      1
214700970526     C                   MOVEL     FUV(1)        FUN001
214800970526     C                   MOVEL     FUV(2)        FUN002
214900970526     C                   MOVEL     FUV(3)        FUN003
215000970526     C                   MOVEL     FUV(4)        FUN004
215100970526     C                   MOVEL     FUV(5)        FUN005
215200970526     C                   MOVEL     FUV(6)        FUN006
215300970526     C                   END
215400970526     C*
215500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
215600970526     C     FUNGRU        IFEQ      2
215700970526     C                   MOVEL     FUV(7)        FUN001
215800970526     C                   MOVEL     FUV(8)        FUN002
215900970526     C                   MOVEL     FUV(9)        FUN003
216000970526     C                   MOVEL     FUV(10)       FUN004
216100970526     C                   MOVEL     FUV(11)       FUN005
216200970526     C                   MOVEL     FUV(12)       FUN006
216300970526     C                   END
216400970526     C*
216500970526     C* TERZO GRUPPO DI 6 FUNZIONI
216600970526     C     FUNGRU        IFEQ      3
216700970526     C                   MOVEL     FUV(13)       FUN001
216800970526     C                   MOVEL     FUV(14)       FUN002
216900970526     C                   MOVEL     FUV(15)       FUN003
217000970526     C                   MOVEL     FUV(16)       FUN004
217100970526     C                   MOVEL     FUV(17)       FUN005
217200970526     C                   MOVEL     FUV(18)       FUN006
217300970526     C                   END
217400970526     C*
217500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
217600970526     C     FUNGRU        IFEQ      4
217700970526     C                   MOVEL     FUV(19)       FUN001
217800970526     C                   MOVEL     FUV(20)       FUN002
217900970526     C                   MOVEL     FUV(21)       FUN003
218000970526     C                   MOVEL     FUV(22)       FUN004
218100970526     C                   MOVEL     *BLANKS       FUN005
218200970526     C                   MOVEL     *BLANKS       FUN006
218300970526     C                   END
218400970526     C*
218500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
218600970526     C                   MOVEL     *BLANKS       FUN007
218700970526     C                   MOVEL     *BLANKS       FUN008
218800970526     C*
218900970526     C     1             DO        24            I
219000970526     C                   MOVEL     FUD(I)        FUDRIG
219100970526     C     FUDFUN        IFEQ      '24'
219200970526     C     FUDATT        ANDEQ     'S'
219300970526     C                   MOVEL     FUDDES        FUN008
219400970526     C                   END
219500970526     C                   END                                                    DO
219600970526     C*
219700970526     C                   ENDSR
219800970526     C*------------------------------------------------------------------------*
219900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
220000970526     C*------------------------------------------------------------------------*
220100970526     C     FUNCON        BEGSR
220200970526     C*
220300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
220400970526     C                   EXSR      FUNVAR
220500970526     C*
220600970526     C* PULISCO CAMPI DI LAVORO
220700970526     C                   Z-ADD     0             I                 5 0
220800970526     C                   Z-ADD     0             W                 5 0
220900970526     C*
221000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
221100970526     C     FUNKEY        IFNE      *BLANKS
221200970526     C     1             DO        24            I
221300970526     C                   MOVEL     FUD(I)        FUDRIG
221400970526     C*
221500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
221600970526     C     FUNKEY        IFEQ      FUDFUN
221700970526     C     FUDATT        ANDEQ     'S'
221800970526     C                   Z-ADD     I             W
221900970526     C                   END
222000970526     C                   END                                                    DO WHILE
222100970526     C*
222200970526     C* FUNZIONE NON ATTIVA (ERRORE)
222300970526     C     W             IFEQ      0
222400970526     C*
222500970526     C* EMISSIONE MESSAGGIO DI ERRORE
222600970526     C                   MOVEL     MSG(1)        DSMSMS
222700970526     C*
222800970526     C                   END                                                    W=0
222900970526     C                   END                                                    FUNKEY NE *BLANKS
223000970526     C*
223100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
223200970526     C     W             IFNE      0
223300970526     C     FUNKEY        OREQ      *BLANKS
223400970526     C     FUNKEY        IFEQ      *BLANKS
223500970526     C                   EXSR      ENTGES
223600970526     C                   END
223700031229     C     FUNKEY        IFEQ      '01'
223800031229     C                   EXSR      F01GES
223900031229     C                   END
224000141111     C     FUNKEY        IFEQ      '02'
224100141111     C                   EXSR      F02GES
224200141111     C                   END
224300970526     C     FUNKEY        IFEQ      '03'
224400970526     C                   EXSR      F03GES
224500970526     C                   END
224600970526     C     FUNKEY        IFEQ      '06'
224700970526     C                   EXSR      F06GES
224800970526     C                   END
224900970526     C     FUNKEY        IFEQ      '12'
225000970526     C                   EXSR      F12GES
225100970526     C                   END
225200970526     C     FUNKEY        IFEQ      '24'
225300970526     C                   EXSR      F24GES
225400970526     C                   END
225500970526     C                   END
225600970526     C*
225700970526     C                   ENDSR
225800970526     C*------------------------------------------------------------------------*
225900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
226000970526     C*------------------------------------------------------------------------*
226100970526     C     FUNVAR        BEGSR
226200970526     C*
226300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
226400970526     C*
226500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
226600970526     C     KEY           IFEQ      ENTER
226700970526     C                   MOVEL     *BLANKS       FUNKEY
226800970526     C                   END
226900970526     C     KEY           IFEQ      F1
227000970526     C                   MOVEL     '01'          FUNKEY
227100970526     C                   END
227200970526     C     KEY           IFEQ      F2
227300970526     C                   MOVEL     '02'          FUNKEY
227400970526     C                   END
227500970526     C     KEY           IFEQ      F3
227600970526     C                   MOVEL     '03'          FUNKEY
227700970526     C                   END
227800970526     C     KEY           IFEQ      F4
227900970526     C                   MOVEL     '04'          FUNKEY
228000970526     C                   END
228100970526     C     KEY           IFEQ      F5
228200970526     C                   MOVEL     '05'          FUNKEY
228300970526     C                   END
228400970526     C     KEY           IFEQ      F6
228500970526     C                   MOVEL     '06'          FUNKEY
228600970526     C                   END
228700970526     C     KEY           IFEQ      F7
228800970526     C                   MOVEL     '07'          FUNKEY
228900970526     C                   END
229000970526     C     KEY           IFEQ      F8
229100970526     C                   MOVEL     '08'          FUNKEY
229200970526     C                   END
229300970526     C     KEY           IFEQ      F9
229400970526     C                   MOVEL     '09'          FUNKEY
229500970526     C                   END
229600970526     C     KEY           IFEQ      F10
229700970526     C                   MOVEL     '10'          FUNKEY
229800970526     C                   END
229900970526     C     KEY           IFEQ      F11
230000970526     C                   MOVEL     '11'          FUNKEY
230100970526     C                   END
230200970526     C     KEY           IFEQ      F12
230300970526     C                   MOVEL     '12'          FUNKEY
230400970526     C                   END
230500970526     C     KEY           IFEQ      F13
230600970526     C                   MOVEL     '13'          FUNKEY
230700970526     C                   END
230800970526     C     KEY           IFEQ      F14
230900970526     C                   MOVEL     '14'          FUNKEY
231000970526     C                   END
231100970526     C     KEY           IFEQ      F15
231200970526     C                   MOVEL     '15'          FUNKEY
231300970526     C                   END
231400970526     C     KEY           IFEQ      F16
231500970526     C                   MOVEL     '16'          FUNKEY
231600970526     C                   END
231700970526     C     KEY           IFEQ      F17
231800970526     C                   MOVEL     '17'          FUNKEY
231900970526     C                   END
232000970526     C     KEY           IFEQ      F18
232100970526     C                   MOVEL     '18'          FUNKEY
232200970526     C                   END
232300970526     C     KEY           IFEQ      F19
232400970526     C                   MOVEL     '19'          FUNKEY
232500970526     C                   END
232600970526     C     KEY           IFEQ      F20
232700970526     C                   MOVEL     '20'          FUNKEY
232800970526     C                   END
232900970526     C     KEY           IFEQ      F21
233000970526     C                   MOVEL     '21'          FUNKEY
233100970526     C                   END
233200970526     C     KEY           IFEQ      F22
233300970526     C                   MOVEL     '22'          FUNKEY
233400970526     C                   END
233500970526     C     KEY           IFEQ      F23
233600970526     C                   MOVEL     '23'          FUNKEY
233700970526     C                   END
233800970526     C     KEY           IFEQ      F24
233900970526     C                   MOVEL     '24'          FUNKEY
234000970526     C                   END
234100970526     C     KEY           IFEQ      ROLLUP
234200970526     C                   MOVEL     'RU'          FUNKEY
234300970526     C                   END
234400970526     C     KEY           IFEQ      HELP
234500970526     C                   MOVEL     'HP'          FUNKEY
234600970526     C                   END
234700970526     C                   ENDSR
234800970526     C*------------------------------------------------------------------------*
234900970526     C* NOTE COSTRUZIONE SCHIERE
235000970526     C*------------------------------------------------------------------------*
235100970526     C*
235200970526     C* -MSG-MESSAGGI
235300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
235400970526     C* (NON TOCCARE MSG,1 E MSG,2)
235500970526     C*
235600970526     C* -CMD-COMANDI
235700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
235800970526     C*
235900970526     C* -OPD-OPZIONI
236000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
236100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
236200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
236300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
236400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
236500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
236600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
236700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
236800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
236900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
237000970526     C*
237100970526     C* -FUD-FUNZIONI
237200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
237300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
237400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
237500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
237600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
237700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
237800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
237900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
238000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
238100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
238200970526     O*------------------------------------------------------------------------*
238300970526** DEC - DECODIFICHE
238400970526    MODIFICA                                                      1
238500970526  ANNULLAMENTO                                                    2
238600970526VISUALIZZAZIONE                                                   3
238700970526   RIPRISTINO                                                     4
238800970526  INSERIMENTO                                                     5
238900991014     COPIA                                                        6
239000970526** MSG - MESSAGGI
239100991026TIS723R- Tasto funzionale non gestito                                          1
239200991026TIS723R- Inserimento effettuato                                                2
239300991026TIS723R- Modifica effettuata                                                   3
239400991026TIS723R- Annullamento effettuato                                               4
239500991026TIS723R- Ripristino effettuato                                                 5
239600991026TIS723R- Richiesta funzione per record inesistente                             6
239700991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
239800991026TIS723R- Richiesta funzione per record esistente                               8
239900991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
240000991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
240100991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
240200000724TIS723R- Campo obbligatorio                                                    12
240300991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
240400000724TIS723R- Data errata                                                           14
240500000724TIS723R- La data fine deve essere maggiore della data inizio                   15
240600000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
240700000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
240800000724TIS723R- Codice tipo servizio non presente in tabella                          18
240900001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
241000001120TIS723R- File inesistente, verificare e correggere.                            20
241100001120TIS723R- Libreria inesistente, verificare e correggere.                        21
241200030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
241300040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
241400040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
241500040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
241600051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
241700060616TIS723R- Valori incompatibili, correggere!                                     27
241800101026TIS723R- Errore BartVAS: fatal error                                           28
241900101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
242000120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
242100120119TIS723R- Tipo file non abilitato per cliente indicato                          31
242200120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
242300120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
242400120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
242500170119TIS723R- Se presenti parametri è richiersto anche *pgm traduttore              35
242600170119TIS723R- Se presente *pgm traduttore è richiesto anche file tradotto           36
242700170119TIS723R- Se presenti parametri è richiersto anche *pgm post-traduzione         37
242800170721TIS723R- Codice Opzione "VPC" non presente in tabella VPR                      38
242900970526** FUD - FUNZIONI
243000141111F01=Desc.Parametri -        (S/N)=S  <== S-FISSO           funz.01¬
243100141111F02=Parametri      -        (S/N)=N                        funz.02¬
243200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
243300970526                   -        (S/N)=N                        funz.04¬
243400970526                   -        (S/N)=N                        funz.05¬
243500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
243600970527                   -        (S/N)=N                        funz.07¬
243700970527                   -        (S/N)=N                        funz.08¬
243800970527                   -        (S/N)=N                        funz.09¬
243900970527                   -        (S/N)=N                        funz.10¬
244000970526                   -        (S/N)=N                        funz.11¬
244100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
244200970526                   -        (S/N)=N                        funz.13¬
244300970526                   -        (S/N)=N                        funz.14¬
244400970526                   -        (S/N)=N                        funz.15¬
244500970526                   -        (S/N)=N                        funz.16¬
244600970526                   -        (S/N)=N                        funz.17¬
244700970526                   -        (S/N)=N                        funz.18¬
244800970526                   -        (S/N)=N                        funz.19¬
244900970526                   -        (S/N)=N                        funz.20¬
245000970526                   -        (S/N)=N                        funz.21¬
245100970526                   -        (S/N)=N                        funz.22¬
245200970526                   -        (S/N)=N                        funz.23¬
245300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
