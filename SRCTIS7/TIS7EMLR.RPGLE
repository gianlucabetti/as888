000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120
000500010912     Fwfptl00f  IF   E             DISK
000600010912     Ftiabl00f  IF   E             DISK
000700010912     Fwfeml01l  UF A E           K DISK
000800001120
000900010912
001000010912
001100010912     D*------------
001200010912     D* SHIERE COMANDI
001300010912     D*------------
001400010913     D TXT             S            100    DIM(32) CTDATA PERRCD(1)
001500010913     D CMD             S             80    DIM(02) CTDATA PERRCD(1)
001600010912     D*------------
001700010912     D* ARCHITETTURA
001800010912     D*------------
001900010912     D KPJBA         E DS
002000010912     D*------------
002100010912     D* DS CONTROLLO E NORMALIZZAZIONE E-MAIL
002200010912     D*------------
002300010912     D DSEMAIL       E DS
002400010912     D*------------------
002500010912     D* DS REPERIMENTO DATI UTENTE
002600010912     D*-------------------
002700010912     DBS69DS         E DS                  extname(TIBS69DS)
002800010912     DACODS          E DS                  extname(CNACO00F)
002900010912     DINDDS          E DS                  extname(CNIND00F)
003000010912     DCLPDS          E DS                  extname(CNCLP00F)
003100010912     DCLSDS          E DS                  extname(FNCLS00F)
003200010912     D*------------------
003300010912     D* VARIABILI DI WRK
003400010912     D*-------------------
003500010912     D wAlfa40         s             40    INZ(*all' ')
003600010914     D wContaSND       s              4  0 INZ(9999)
003700010912
003800010912
003900000414     C*------------------------------------------------------------------------*
004000000414     C* MAIN LINE
004100000414     C*------------------------------------------------------------------------*
004200000414     C*
004300010914     C***                EXSR      EXEABL
004400010914     C***                EXSR      EXEPTL
004500010912     C*
004600010914     C                   EXSR      SNDEML
004700010912     C*
004800010912     C                   SETON                                        LR
004900010912     C*------------------------------------------------------------------------*
005000010912     C*  EXEABL - ROUTINE DI REPERIMENTO DATI DA FILE TIABL00F
005100010912     C*------------------------------------------------------------------------*
005200010912     C     EXEABL        BEGSR
005300010912     C*
005400010912     C                   READ      TIABL00F
005500010912     C*
005600010912     C                   DOW       not %eof(TIABL00F)
005700010912     C* Se presente e-mail => PROCEDO
005800010912     C                   IF        ablEML <> *blanks
005900010912     C* Inizializzo il buffer di output del file WFEML00F
006000010912     C                   CLEAR                   WFEML000
006100010912     C*
006200010912     C                   MOVE      ablUTE        wfeKSC
006300010912     C                   EVAL      wfeRSP = ablRSP
006400010912     C                   EVAL      wfeEML = ablEML
006500010912     C* Controllo e normalizzo l'e-mail
006600010912     C                   EXSR      CHKEML
006700010912     C* Reperimento dati anagrafici cliente tramite TIBS69
006800010912     C                   EXSR      DECCLI
006900010912     C* Verifico già esistenza e-mail => se non esiste scrivo il record nel wrk-file
007000010912     C     wfeEML        SETLL     WFEML01L                               55
007100010912     C  N55              WRITE     WFEML000
007200010912     C                   ENDIF
007300010912     C* Proseguo la lettura
007400010912     C                   READ      TIABL00F
007500010912     C*
007600010912     C                   ENDDO
007700010912     C*
007800010912     C                   ENDSR
007900010912     C*------------------------------------------------------------------------*
008000010912     C*  EXEPTL - ROUTINE DI REPERIMENTO DATI DA FILE WFPTL00F
008100010912     C*------------------------------------------------------------------------*
008200010912     C     EXEPTL        BEGSR
008300010912     C*
008400010912     C                   READ      WFPTL00F
008500010912     C*
008600010912     C                   DOW       not %eof(WFPTL00F)
008700010912     C* Se presente e-mail => PROCEDO
008800010912     C                   IF        wfpEML <> *blanks
008900010912     C* Inizializzo il buffer di output del file WFEML00F
009000010912     C                   CLEAR                   WFEML000
009100010912     C*
009200010912     C                   EVAL      wfeKSC = wfpKSC
009300010912     C                   EVAL      wfeIOC = wfpIOC
009400010912     C                   EVAL      wfeRAG = wfpRAG
009500010912     C                   EVAL      wfeIND = wfpIND
009600010912     C                   EVAL      wfeLOC = wfpLOC
009700010912     C                   EVAL      wfePRO = wfpPRO
009800010912     C                   EVAL      wfeCAP = wfpCAP
009900010912     C                   EVAL      wfeNAZ = wfpNAZ
010000010912     C                   EVAL      wfeRSP = wfpRSP
010100010912     C                   EVAL      wfeEML = wfpEML
010200010912     C* Controllo e normalizzo l'e-mail
010300010912     C                   EXSR      CHKEML
010400010912     C* Verifico già esistenza e-mail => se non esiste scrivo il record nel wrk-file
010500010912     C     wfeEML        SETLL     WFEML01L                               55
010600010912     C  N55              WRITE     WFEML000
010700010912     C                   ENDIF
010800010912     C* Proseguo la lettura
010900010912     C                   READ      WFPTL00F
011000010912     C*
011100010912     C                   ENDDO
011200010912     C*
011300010912     C                   ENDSR
011400010912     C*------------------------------------------------------------------------*
011500010912     C* DECCLI - DECODIFICA IL CODICE CLIENTE E REPERISCE I DATI ANAGRAFICI
011600010912     C*------------------------------------------------------------------------*
011700010912     C     DECCLI        BEGSR
011800010912     C*
011900010912     C                   CLEAR                   BS69DS
012000010912     C                   CLEAR                   ACODS
012100010912     C                   MOVEL     KNSIF         I69SIF
012200010912     C                   Z-ADD     wfeKSC        I69KAC
012300010912     C                   Z-ADD     wfeKSC        I69KIN
012400010912     C                   CALL      'TIBS69R'
012500010912     C                   PARM                    BS69DS
012600010912     C                   PARM                    ACODS
012700010912     C                   PARM                    INDDS
012800010912     C                   PARM                    CLPDS
012900010912     C                   PARM                    CLSDS
013000010912     C     O69ERR        IFNE      '1'
013100010912     C                   EVAL      wfeRAG = ACORAG
013200010912     C                   EVAL      wfeIND = INDVIA
013300010912     C                   EVAL      wfeLOC = INDCIT
013400010912     C                   MOVEL     INDCAP        wfeCAP
013500010912     C                   EVAL      wfePRO = INDPRV
013600010912     C                   EVAL      wfeNAZ = INDSTA
013700010912     C                   IF        INDIVA <> *blanks
013800010912     C                   EVAL      wfeIOC = INDIVA
013900010912     C                   ELSE
014000010912     C                   EVAL      wfeIOC = INDCDF
014100010912     C                   ENDIF
014200010912     C                   ELSE
014300010912     C                   EVAL      wfeCHK = '2'
014400010912     C                   ENDIF
014500010912     C*
014600010912     C                   ENDSR
014700010912     C*------------------------------------------------------------------------*
014800010912     C* CHKEML - CONTROLLA E NORMALIZZA L'E-MAIL
014900010912     C*------------------------------------------------------------------------*
015000010912     C     CHKEML        BEGSR
015100010912     C*
015200010912     C                   EVAL      §EMLINDI = wfeEML
015300010912     C                   CALL      'XEMAIL'
015400010912     C                   PARM                    DSEMAIL
015500010912     C                   IF        §EMLERRO <> '1'
015600010912     C                   EVAL      wfeEML = §EMLINDO
015700010912     C                   ELSE
015800010912     C                   EVAL      wfeCHK = '1'
015900010912     C                   ENDIF
016000010912     C*
016100010912     C                   ENDSR
016200010912     C*------------------------------------------------------------------------*
016300010912     C* SNDEML - COMPONE TESTO CORPO MESSAGGI OED INVIA E-MAIL
016400010912     C*------------------------------------------------------------------------*
016500010912     C     SNDEML        BEGSR
016600010914     C*
016700010914     C                   z-add     *zeros        curSND            4 0
016800010912     C*
016900010912     C*  Mi posiziono all'inizio del file WFEML01L
017000010912     C     *loval        SETLL     WFEML01L
017100010912     C*  E scorro tutto il file
017200010912     C                   READ      WFEML01L
017300010914     C                   DOW       not %eof(WFEML01L) and curSND < wContaSND
017400010912     C*
017500010912     C* Se record è OK
017600010912     C                   IF        wfeCHK = *blanks
017700010914     C*
017800010914     C                   add       1             curSND
017900010912     C*
018000010912     C* Inizializzo variabili
018100010912     C                   movel     *blanks       wrkEml          253
018200010912     C                   movel     *blanks       wrkMsg         5000
018300010912     C                   movel     *blanks       wrkOgg           44
018400010912     C* Valorizzo i campi della e-m@ail
018500010914     C                   eval      wrkEml = wfeEML
018600110524     C                   eval      wrkOgg = 'Nuovi servizi VAS BRT S.p.A.'
018700010913     C***                eval      wrkMsg = wAlfa40 +'Spett.: '+wfeRAG   +':/N'+
018800010914     C                   eval      wrkMsg = 'Alla c.a.  : '+wfeRSP       +':/N'+
018900010912     C                                                                    ':/N'+
019000010913     C                                    %trim(TXT(01))+' '+%trim(TXT(02))+' '+
019100010913     C                                    %trim(TXT(03))+' '+%trim(TXT(04))+' '+
019200010913     C                                    %trim(TXT(05))+' '+%trim(TXT(06))+' '+
019300010913     C                                    %trim(TXT(07))+' '+%trim(TXT(08))+' '+
019400010913     C                                    %trim(TXT(09))+' '+%trim(TXT(10))+' '+
019500010913     C                                    %trim(TXT(11))+' '+%trim(TXT(12))+' '+
019600010913     C                                    %trim(TXT(13))+' '+%trim(TXT(14))+' '+
019700010913     C                                    %trim(TXT(15))+' '+%trim(TXT(16))+' '+
019800010913     C                                    %trim(TXT(17))+' '+%trim(TXT(18))+' '+
019900010913     C                                    %trim(TXT(19))+' '+%trim(TXT(20))+' '+
020000010913     C                                    %trim(TXT(21))+' '+%trim(TXT(22))+' '+
020100010913     C                                    %trim(TXT(23))+' '+%trim(TXT(24))+' '+
020200010913     C                                    %trim(TXT(25))+' '+%trim(TXT(26))+' '+
020300010914     C                                    %trim(TXT(27))
020400010912     C*
020500010912     C                   CALL(e)   'TIS701C'
020600010912     C                   PARM                    wrkEml
020700010912     C                   PARM                    wrkOgg
020800010912     C                   PARM                    wrkMsg
020900010914     C*
021000010914     C* Dopo l'invio fleggo il record come "già inviato"
021100010914     C                   EVAL      wfechk = '0'
021200010914     C                   UPDATE    WFEML000
021300010913     C*
021400010913     C* DOPO OGNI E-MAIL INVIATA ATTENDO 2 SECONDI
021500010913     C                   Z-ADD     80            lengh            15 5
021600010913     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
021700010913     C                   PARM                    CMD(1)
021800010913     C                   PARM                    lengh
021900010912     C*
022000010912     C                   ENDIF
022100010912     C* Proseguo con la lettura
022200010912     C                   READ      WFEML01L
022300010912     C*
022400010912     C                   ENDDO
022500010912     C*
022600010912     C                   ENDSR
022700010912     C*------------------------------------------------------------------------*
022800010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
022900010912     C*------------------------------------------------------------------------*
023000010912     C     *INZSR        BEGSR
023100010912     C*---
023200010912     C* RICEVIMENTO PARAMETRI
023300010912     C*---
023400010912     C     *ENTRY        PLIST
023500010912     C                   PARM                    KPJBA
023600010912     C*
023700010912     C                   ENDSR
023800010913** TXT - TESTO PER CORPO E-MAIL
023900120305:/NRICHIESTA DI RITIRO MERCI E NUOVI SERVIZI BRT SU INTERNET
024000010913:/N
024100010912:/NGentile Cliente,
024200010914:/Nè a Sua disposizione la nuova versione del sito Internet.
024300010912:/N
024400010913:/NNuova veste grafica per una più agevole consultazione e, soprattutto, nuovi servizi
024500010913e informazioni.
024600010912:/N
024700010913:/NTra le novità Le segnaliamo il servizio "RICHIESTA DI RITIRO", accessibile dalla pagina
024800010913principale attraverso l'area "servizi per i clienti" . Tale servizio è stato pensato per
024900010913consentire ai clienti di ottenere il ritiro delle proprie spedizioni in modo pratico, preciso,
025000010913veloce, economico ed automatico.
025100010912:/N
025200120305:/NLa nuova versione del sito BRT contiene anche altre interessanti novità:
025300010913:/N
025400010913:/N-  SEGNALAZIONI DI GIACENZE, per inviare via Internet le disposizioni necessarie alla riconsegna.
025500010913:/N-  VISUALIZZAZIONE DELLA FIRMA dei destinatari delle spedizioni DPD Europa.
025600010913:/N-  CONTRASSEGNI, per la visualizzazione delle modalità d'incasso e pagamento.
025700010913:/N
025800120305:/NSul sito www.brt.it troverà le informazioni necessarie per utilizzare efficacemente tutti i
025900010913nuovi servizi pensati, come sempre, nell'intento di offrirLe SOLUZIONI INFORMATICHE VELOCI, AGILI
026000010913ed a BASSO COSTO.
026100010913:/N
026200010913:/NCogliamo l'occasione per inviarLe i nostri più cordiali saluti.
026300010913:/N
026400010912:/N
026500120305:/NBRT Corriere Espresso
026600010913** CMD - COMANDI CL
026700010913DLYJOB DLY(2)
026800010913
