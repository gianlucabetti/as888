000100000000     h*--------------------------------------------------------------------------------------------*
000200060515     h* Calcolo instradamento cappario DPD GeoPost
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f* Data base
000700000000     f*--------------------------------------------------------------------------------------------*
000800060515     fAZORG01L  IF   E           K DISK
000900060515     fTABEL00F  IF   E           K DISK
001000060515     fDPCCC01L  IF   E           K DISK
001100060606     fDPCCC02L  IF   E           K DISK    rename(DPCCC000:DPCCC002)
001200060515     fDPCSC01L  IF   E           K DISK
001300060515     fDPCDP01L  IF   E           K DISK
001400060519     fDPCLO01L  IF   E           K DISK
001500000000     d*--------------------------------------------------------------------------------------------*
001600000000     d* Data structure
001700000000     d*--------------------------------------------------------------------------------------------*
001800060510     D*------------------
001900060510     D* DS "XSRDA8" - CONTROLLA DATA (8)
002000060510     D*------------------
002100060510     D WLBDA8          DS
002200060510     D  G08DAT                 1      8  0
002300060510     D  G08INV                 9     16  0
002400060510     D  G08ERR                17     17
002500060510     D  G08TGI                18     22  0
002600060515     d*------------------
002700060515     d* DS DI WRK
002800060515     d*------------------
002900060515     d DS15          E DS
003000060515     d OG143         E DS
003100061109     d DPCRO00F      E DS
003200061109     d DS_DPCRO        DS
003300061109     d  d_croOSRT                          like(croOSRT)
003400061109     d  d_croDDEP                          like(croDDEP)
003500061109     d  d_croDSRT                          like(croDSRT)
003600061109     d  d_croPTYG                          like(croPTYG)
003700061109     d  d_croBID                           like(croBID)
003800061109     d  d_croBIDP                          like(croBIDP)
003900150518     d  d_croPTCB                          like(croPTCB)
004000150518     d  d_croPTCE                          like(croPTCE)
004100061109     d DS_DPCAL      E DS                  EXTNAME(DPCAL00F) PREFIX(d_)
004200061109     d DS_DPCDY      E DS                  EXTNAME(DPCDY00F) PREFIX(d_)
004300060510     d*------------------
004400060510     d* DS DI PROCEDURA
004500060510     d*------------------
004600060510     d TISIE2DS      E DS
004700060515     d TISIE3DS      E DS
004800060515     d*------------------
004900060515     d* VARIABILI D WRK
005000060515     d*------------------
005100000000     c*--------------------------------------------------------------------------------------------*
005200000000     c* Main lines
005300000000     c*--------------------------------------------------------------------------------------------*
005400150518     C*
005500150518     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005600150518     C
005700150518     C/EXEC SQL
005800150518     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005900150518     C/END-EXEC
006000060516     C*
006100060516     C* Verifico subito il tipo lancio richiesto
006200060516     C                   if        ISIE3TLA = 'C'                               * solo chiusura
006300060516     C                   seton                                        lr
006400060516     C                   endif
006500060516     C*
006600060516     C                   if        ISIE3TLA = 'E'                               * solo elaborazione
006700060516     C                   exsr      esegui
006800060516     C                   seton                                        rt
006900060516     C                   endif
007000060516     C*
007100060516     C                   if        ISIE3TLA = *blanks                           * elabora e chiude
007200060516     C                   exsr      esegui
007300060516     C                   seton                                        lr
007400060516     C                   endif
007500060515     C*
007600060515     C*------------------------------------------------------------------------*
007700060515     C* ESEGUI - Routine di calcolo instradamento cappario DPD GeoPost
007800060515     C*------------------------------------------------------------------------*
007900060515     C     ESEGUI        BEGSR
008000060520     C*
008100060520     C* Resetto indicatore d errore
008200060520     C                   SETOFF                                       70
008300060515     C*
008400060515     C* Innanzitutto inizializzo i campi d output della ds d procedura corrente
008500060515     C                   EXSR      AZZOUT
008600060515     C*
008700060515     C* Quindi controllo correttezza parametri in input
008800060515     C                   EXSR      CHKPAR
008900061114     C*
009000061114     C* Quindi reperisco la versione da considerare
009100061114     C  N70              EXSR      REPVER
009200060515     C*
009300060515     C* Reperisco il depot DPD relativo alla linea di partenza passata in input
009400060520     C  N70              EXSR      REPDEP
009500060515     C*
009600060515     C* Verifico esistenza depot partenza DPD sui file cappario DPD
009700060606     C                   MOVEL(P)  wR_Depot      wCurrentDepot     4
009800060627     C                   SETOFF                                       60
009900060606     C  N70              EXSR      CHKDEP
010000060515     C*
010100060515     C* Reperisco dati relativi alla nazione destinatario
010200060520     C  N70              EXSR      REPNAZ
010300060519     C*
010400060519     C* Verifica se nazione destino gestisce il cappario (vedi Irlanda)
010500060520     C  N70              EXSR      CHKPTCSYS
010600060515     C*
010700060515     C* Reperisco dati relativi al servizio DPD
010800060520     C  N70              EXSR      REPSRV
010900060519     C*
011000060519     C* Verifico instradamento x area/località DPD
011100060530     C  N70              EXSR      REPCTY
011200060516     C*
011300060516     C* Reperisco dati relativi all'instradamento DPD
011400060520     C  N70              EXSR      REPROUTE
011500060530     C*
011600060530     C* Vaglio instradamento calcolato rispetto a ALLOW (permessi) e DENY (divieti)
011700060530     C  N70              EXSR      CHKALWDNY
011800060515     C*
011900060515     C* Al termine valorizzo i dati d output d procedura corrente
012000060520     C  N70              EXSR      VALOUT
012100060515     C*
012200060515     C                   ENDSR
012300060515     C*------------------------------------------------------------------------*
012400060515     C* AZZOUT - Routine di inizializzazione campi d ouput procedura corrente
012500060515     C*------------------------------------------------------------------------*
012600060515     C     AZZOUT        BEGSR
012700060515     C*
012800060515     C                   EVAL      OSIE3VER  = *zeros
012900060515     C                   EVAL      OSIE3VERD = *blanks
013000060515     C                   EVAL      OSIE3DDE  = *zeros
013100060515     C                   EVAL      OSIE3NAZN = *zeros
013200060515     C                   EVAL      OSIE3NAZ2 = *blanks
013300060515     C                   EVAL      OSIE3NAZ3 = *blanks
013400060515     C                   EVAL      OSIE3SRVM = *blanks
013500060515     C                   EVAL      OSIE3SRVD = *blanks
013600060515     C                   EVAL      OSIE3OSRT = *blanks
013700060515     C                   EVAL      OSIE3RDEP = *blanks
013800060515     C                   EVAL      OSIE3DDEP = *blanks
013900060515     C                   EVAL      OSIE3DSRT = *blanks
014000060515     C                   EVAL      OSIE3PTYG = *blanks
014100060515     C                   EVAL      OSIE3BID  = *zeros
014200060516     C                   EVAL      OSIE3BIDP = *blanks
014300060515     C                   EVAL      OSIE3ERR  = *blanks
014400060515     C                   EVAL      OSIE3ERRL = *blanks
014500060520     C                   EVAL      OSIE3ERRD = *blanks
014600060515     C*
014700060515     C                   ENDSR
014800060515     C*------------------------------------------------------------------------*
014900060515     C* CHKPAR - Routine di controllo correttezza parametri d input
015000060515     C*------------------------------------------------------------------------*
015100060515     C     CHKPAR        BEGSR
015200060520     C*
015300060520     C* Inizializzo variabile esplicitamento errori
015400060520     C                   MOVEL     *blanks       wErrore          20
015500060531     C*
015600060531     C* Innanzitutto imposto il CAP "utilizzato" = al CAP destinatario passato in input
015700060608     C                   EVAL      USIE3CAD = %trim(ISIE3CAD)
015800060608     C*
015900060608     C* Normalizzo il CAP destinatario compattando gli spazi eventualmente presenti
016000060608     C                   Z-ADD     1             wPos              1 0
016100060629     C                   DOW       wPos < %len(%trim(USIE3CAD))
016200060608     C                   IF        %subst(USIE3CAD:wPos:1) = *blanks
016300060608     C                   EVAL      USIE3CAD = %subst(USIE3CAD:1:wPos-1) +
016400060608     C                                        %subst(USIE3CAD:wPos+1)
016500060621     C                   EVAL      wPos = wPos - 1
016600060608     C                   ENDIF
016700060608     C                   ADD       1             wPos
016800060608     C                   ENDDO
016900060515     C*
017000060515     C* Verifica presenza campi obbligatori
017100060515     C*
017200060515     C* - data spedizione...
017300060515     C                   IF        ISIE3DSP = *zeros
017400060520     C                   EVAL      wErrore = '(Data spedizione)'
017500060515     C                   EVAL      OSIE3ERR  = '1'
017600060515     C                   ENDIF
017700060515     C*
017800060515     C* - cap destinatario
017900060515     C                   IF        ISIE3CAD = *blanks
018000060520     C                   EVAL      wErrore = '(CAP destinatario)'
018100060515     C                   EVAL      OSIE3ERR  = '1'
018200060515     C                   ENDIF
018300060515     C*
018400060515     C* - servizio DPD
018500060515     C                   IF        ISIE3SRV = *zeros
018600060520     C                   EVAL      wErrore = '(Servizio DPD)'
018700060515     C                   EVAL      OSIE3ERR  = '1'
018800060515     C                   ENDIF
018900060515     C*
019000060515     C* - linea di partenza
019100060515     C                   IF        ISIE3LNP = *zeros
019200060520     C                   EVAL      wErrore = '(Linea partenza)'
019300060515     C                   EVAL      OSIE3ERR  = '1'
019400060515     C                   ENDIF
019500060515     C*
019600060515     C* Verifica campi facoltativi
019700060520     C*
019800060520     C* - data riferimento...
019900060520     C                   IF        ISIE3DRI = *zeros
020000060606     C***                EVAL      ISIE3DRI = DATCOR
020100060606     C                   EVAL      ISIE3DRI = ISIE3DSP
020200060520     C                   ENDIF
020300060515     C*
020400060515     C* - ora spedizione...
020500060515     C                   IF        ISIE3HSP = *zeros
020600060515     C                   EVAL      ISIE3HSP = 120000
020700060515     C                   ENDIF
020800060515     C*
020900060515     C* - eccezioine x Bartolini...
021000060515     C                   IF        ISIE3EXC = *blanks
021100060515     C                   EVAL      ISIE3EXC = 'N'
021200060515     C                   ENDIF
021300060515     C*
021400060515     C* Se nn ok => imposto subito l'errore generale d procedura
021500060515     C                   IF        OSIE3ERR <> *blanks
021600060516     C                   EVAL      OSIE3ERRL = '92'
021700060520     C                   EVAL      OSIE3ERRD = 'Errore: parametri obbligatori '+
021800060520     C                             'non valorizzati.' + %trimr(wErrore)
021900060515     C                   ENDIF
022000060515     C*
022100060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
022200060515     C                   IF        OSIE3ERR <> *blanks
022300060520     C                   SETON                                        70
022400060515     C                   ENDIF
022500060515     C*
022600060515     C                   ENDSR
022700060515     C*------------------------------------------------------------------------*
022800060515     C* REPVER - Routine di reperimento versione cappario DPD GeoPost
022900060515     C*------------------------------------------------------------------------*
023000060515     C     REPVER        BEGSR
023100060515     C*
023200060515     C                   CLEAR                   TISIE2DS
023300060515     C                   EVAL      SIE2TLA = 'E'
023400060515     C                   EVAL      SIE2DRI = ISIE3DRI
023500060515     C                   CALL      'TISIE2R'
023600060515     C                   PARM                    TISIE2DS
023700060515     C*
023800060515     C* Se nn ok => imposto subito l'errore generale d procedura
023900060515     C                   IF        SIE2VER = *zeros
024000060515     C                   EVAL      OSIE3ERR  = '1'
024100060516     C                   EVAL      OSIE3ERRL = '90'
024200060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento versione'+
024300060515     C                             '. (DPCVE01L)'
024400060515     C                   ENDIF
024500060515     C*
024600060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
024700060515     C                   IF        OSIE3ERR <> *blanks
024800060520     C                   SETON                                        70
024900060515     C                   ENDIF
025000060515     C*
025100060515     C                   ENDSR
025200060515     C*------------------------------------------------------------------------*
025300060515     C* REPDEP - Routine di reperimento depot DPD di partenza
025400060515     C*------------------------------------------------------------------------*
025500060515     C     REPDEP        BEGSR
025600060515     C*
025700060515     C                   MOVEL     *blanks       wR_Depot          4
025800061114     C*
025900061114     C     ISIE3LNP      CHAIN     AZORG01L
026000061114     C                   IF        %found(AZORG01L)
026100061114     C                   EVAL      OG143 = ORGDE3
026200061114     C                   EVAL      wR_Depot = §OGDP1
026300061114     C                   ENDIF
026400060515     C*
026500060515     C* Se nn ok => imposto subito l'errore generale d procedura
026600060515     C                   IF        wR_Depot = *blanks
026700060515     C                   EVAL      OSIE3ERR  = '1'
026800060516     C                   EVAL      OSIE3ERRL = '91'
026900060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento depot ' +
027000060515     C                             ' partenza. (AZORG01L)'
027100060515     C                   ENDIF
027200060515     C*
027300060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
027400060515     C                   IF        OSIE3ERR <> *blanks
027500060520     C                   SETON                                        70
027600060515     C                   ENDIF
027700060515     C*
027800060515     C                   ENDSR
027900060515     C*------------------------------------------------------------------------*
028000060515     C* CHKDEP - Routine di verifica esistenza depot DPD partenza su file cappario DPD
028100060515     C*------------------------------------------------------------------------*
028200061114     C     CHKDEP        BEGSR
028300061114     C*
028400061114     C                   CLEAR                   DPCDP000
028500061114     C                   MOVEL     *blanks       wIATALikeCode     3
028600061114     C*
028700061114     C                   EVAL      cdpVER = SIE2VER
028800061114     C                   EVAL      cdpDPC = wCurrentDepot
028900061114     C     KEYcdp01_C    CHAIN     DPCDP01L
029000061114     C                   IF        %found(DPCDP01L)
029100061114     C                   IF        cdpATB = *blanks
029200061114     C                   MOVEL(P)  cdpIATA       wIATALikeCode
029300061114     C                   ELSE
029400061114     C                   EVAL      OSIE3ERR  = '1'
029500061114     C                   EVAL      OSIE3ERRL = '94'
029600061114     C  N60              EVAL      OSIE3ERRD = 'Errore: depot partenza ' +
029700061114     C                             'annullato. (DPCDP01L)'
029800061114     C   60              EVAL      OSIE3ERRD = 'Errore: depot arrivo ' +
029900061114     C                             'annullato. (DPCDP01L)'
030000061114     C                   ENDIF
030100061114     C                   ELSE
030200061114     C                   EVAL      OSIE3ERR  = '1'
030300061114     C                   EVAL      OSIE3ERRL = '94'
030400061114     C  N60              EVAL      OSIE3ERRD = 'Errore: depot partenza ' +
030500061114     C                             'inesistente. (DPCDP01L)'
030600061114     C   60              EVAL      OSIE3ERRD = 'Errore: depot arrivo ' +
030700061114     C                             'inesistente. (DPCDP01L)'
030800061114     C                   ENDIF
030900061114     C*
031000061114     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
031100061114     C                   IF        OSIE3ERR <> *blanks
031200061114     C                   SETON                                        70
031300061114     C                   ENDIF
031400060515     C*
031500060515     C                   ENDSR
031600060515     C*------------------------------------------------------------------------*
031700060515     C* REPNAZ - Routine di reperimento dati nazione destinatario
031800060515     C*------------------------------------------------------------------------*
031900060515     C     REPNAZ        BEGSR
032000060515     C*
032100060515     C                   Z-ADD     *zeros        wNazIsoN          3 0
032200060515     C                   MOVEL     *blanks       wNazIso2          2
032300060515     C                   MOVEL     *blanks       wNazIso3          3
032400060519     C                   MOVEL     *blanks       wNazPTC           1 0
032500060515     C*
032600061114     C                   CLEAR                   DS15
032700060515     C                   EVAL      tblKUT = 1
032800060515     C                   EVAL      tblCOD = '15'
032900060515     C                   EVAL      tblKEY = ISIE3NZD
033000060515     C     KEYtabel_C    CHAIN     TABEL00F
033100060515     C                   IF        %found(TABEL00F)
033200060515     C                   IF        tblFLG = *blanks
033300060515     C                   EVAL      DS15 = tblUNI
033400170111     C*
033500170111     C* Se presente Nazione Instradamento Forzata considero quella
033600170111     C                   IF        §15DPDISO2 <> *blanks
033700170111     C                   EVAL      cccVER  = SIE2VER
033800170111     C                   EVAL      cccISO2 = §15DPDISO2
033900170111     C     KEYccc02_C    CHAIN     DPCCC02L
034000170111     C                   IF        %found(DPCCC02L)
034100170111     C                   IF        cccATB = *blanks
034200170111     C                   EVAL      wNazIsoN = cccISOK
034300170111     C                   EVAL      wNazIso2 = cccISO2
034400170111     C                   EVAL      wNazIso3 = cccISO3
034500170111     C                   EVAL      wNazPTC  = cccPTC
034600170111     C                   ENDIF
034700170111     C                   ENDIF
034800170111     C*
034900170111     C* ... diversamente utilizzo Nazione della spedizione
035000170111     C                   ELSE
035100170111     C*
035200170111     C                   EVAL      cccVER  = SIE2VER
035300170111     C                   EVAL      cccISOK = §15CIE
035400060515     C     KEYccc01_C    CHAIN     DPCCC01L
035500060606     C                   IF        %found(DPCCC01L)
035600060515     C                   IF        cccATB = *blanks
035700060515     C                   EVAL      wNazIsoN = cccISOK
035800060515     C                   EVAL      wNazIso2 = cccISO2
035900060515     C                   EVAL      wNazIso3 = cccISO3
036000060519     C                   EVAL      wNazPTC  = cccPTC
036100060515     C                   ENDIF
036200060515     C                   ENDIF
036300170111     C*
036400170111     C                   ENDIF
036500170111     C*
036600060515     C                   ENDIF
036700060515     C                   ENDIF
036800060515     C*
036900060515     C* Se nn ok => imposto subito l'errore generale d procedura
037000060515     C                   IF        wNazIsoN = *zeros  OR
037100060515     C                             wNazIso2 = *blanks OR
037200060515     C                             wNazIso3 = *blanks
037300060515     C                   EVAL      OSIE3ERR  = '1'
037400060516     C                   EVAL      OSIE3ERRL = '93'
037500060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento nazione.'+
037600060515     C                             ' (DPCCC01L)'
037700060515     C                   ENDIF
037800060515     C*
037900060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
038000060515     C                   IF        OSIE3ERR <> *blanks
038100060520     C                   SETON                                        70
038200060515     C                   ENDIF
038300060515     C*
038400060515     C                   ENDSR
038500060519     C*------------------------------------------------------------------------*
038600060519     C* CHKPTCSYS - Verifica se nazione destino gestisce il cappario (vedi Irlanda)
038700060519     C*------------------------------------------------------------------------*
038800060519     C     CHKPTCSYS     BEGSR
038900060519     C*
039000060519     C* Se nazione nn gestisce il cappario => forzo a *blanks il campo CAP destinatario
039100060519     C* ricevuto in input
039200060519     C                   IF        wNazPTC = 1
039300060531     C                   EVAL      USIE3CAD = *blanks
039400060519     C                   ENDIF
039500060519     C*
039600060519     C* Eseguo forzatura => in caso d nazione destinatario Irlanda forzo il CAP
039700060519     C* destinatario a '1' fisso
039800060519     C                   IF        ISIE3NZD = 'IRL'
039900060531     C                   EVAL      USIE3CAD = '1'
040000060519     C                   ENDIF
040100060519     C*
040200060519     C                   ENDSR
040300060515     C*------------------------------------------------------------------------*
040400060515     C* REPSRV - Routine di verifica e reperimento dati relativi al servizio DPD
040500060515     C*------------------------------------------------------------------------*
040600060515     C     REPSRV        BEGSR
040700060515     C*
040800060515     C                   MOVEL     *blanks       wSrvMark          1
040900060515     C                   MOVEL     *blanks       wSrvDes          16
041000060515     C*
041100060515     C                   EVAL      cscVER  = SIE2VER
041200060515     C                   EVAL      cscSRVC = ISIE3SRV
041300060515     C     KEYcsc01_C    CHAIN     DPCSC01L
041400060515     C                   IF        %found(DPCSC01L)
041500060515     C                   IF        cscATB = *blanks
041600060515     C                   EVAL      wSrvMark = cscSRVM
041700060515     C                   EVAL      wSrvDes  = CSCSRVD
041800060515     C                   ENDIF
041900060515     C                   ENDIF
042000060515     C*
042100060515     C* Se nn ok => imposto subito l'errore generale d procedura
042200060520     C                   IF        wSrvMark = *blanks AND
042300060515     C                             wSrvDes  = *blanks
042400060515     C                   EVAL      OSIE3ERR  = '1'
042500060516     C                   EVAL      OSIE3ERRL = '95'
042600060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento servizio'+
042700060515     C                             '. (DPCSC01L)'
042800060515     C                   ENDIF
042900060515     C*
043000060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
043100060515     C                   IF        OSIE3ERR <> *blanks
043200060520     C                   SETON                                        70
043300060515     C                   ENDIF
043400060515     C*
043500060515     C                   ENDSR
043600060519     C*------------------------------------------------------------------------*
043700060519     C* REPCTY - Routine di verifica esistenza ecezione x area/località DPD
043800060519     C*------------------------------------------------------------------------*
043900060530     C     REPCTY        BEGSR
044000060519     C*
044100060519     C* Se ricevuti in ingresso AREA e/o LOCALITA "provo" d gestire
044200060519     C                   IF        ISIE3AREA <> *blanks OR
044300060519     C                             ISIE3CTY  <> *blanks
044400060519     C*
044500060519     C                   EVAL      cloVER  = SIE2VER
044600060520     C                   EVAL      cloISO2 = wNazIso2
044700060519     C                   EVAL      cloAREA = ISIE3AREA
044800060519     C                   EVAL      cloCTY  = ISIE3CTY
044900060519     C     KEYclo01_C    CHAIN     DPCLO01L
045000060519     C                   IF        %found(DPCLO01L)
045100060519     C                   IF        cloATB = *blanks
045200060519     C*
045300060519     C* Se trovata eccezione x AREA/LOCALITA forzo il valore dell'ecezione sul CAP
045400060519     C* destinatario ricevuto in input
045500060531     C                   EVAL      USIE3CAD = cloPTC
045600060519     C                   ENDIF
045700060519     C                   ENDIF
045800060519     C*
045900060519     C                   ENDIF
046000060519     C*
046100060519     C                   ENDSR
046200060516     C*------------------------------------------------------------------------*
046300060516     C* REPROUTE - Routine di reperimento dati relativi all'instradamento DPD
046400060516     C*------------------------------------------------------------------------*
046500060516     C     REPROUTE      BEGSR
046600060516     C*
046700060530     C                   CLEAR                   ds_dpcro
046800060516     C                   MOVEL     *blanks       wO_Sort           4
046900060516     C                   MOVEL     *blanks       wD_Depot          4
047000060516     C                   MOVEL     *blanks       wD_Sort           4
047100060516     C                   MOVEL     *blanks       wPty_Group        1
047200060516     C                   Z-ADD     *zeros        wBarcodeID        3 0
047300060516     C                   MOVEL     *blanks       wBarcodeID_Prt    1
047400150518     C                   MOVEL     *blanks       wZIPCODE_Da       9
047500150518     C                   MOVEL     *blanks       wZIPCODE_A        9
047600060516     C*
047700060516     C/EXEC SQL
047800060622     C+ declare C1 cursor for
047900061109     C+ select croOSRT, croDDEP, croDSRT, croPTYG, croBID,
048000150518     C+ croBIDP, croPTCB, croPTCE from dpcro03l
048100060516     C+ where croVER = :SIE2VER and croISO2 = :wNazIso2 and
048200060516     C+ (croSRVC = :ISIE3SRV or croSRVC = 0)  and
048300060608     C+ (croROUP = :wR_Depot or croROUP = '0000') and
048400060531     C+ :USIE3CAD between croPTCB and croPTCE and
048500060530     C+ croATB = ' '
048600070523     C+ order by croSRVC desc, croROUP desc, croPTYG desc
048700060622     C+ for read only
048800060516     C/END-EXEC
048900060516     C
049000060516     C/EXEC SQL
049100060516     C+ open C1
049200060516     C/END-EXEC
049300060516     C
049400060516     C/EXEC SQL
049500060622     C+ Fetch next from C1 into :ds_dpcro
049600060516     C/END-EXEC
049700060516     C*
049800060516     C* Se reperito qualcosa => analizzo
049900060516     C* ....altrimenti trattasi d errore di instradamento
050000060516     C                   if        sqlcod <> *zeros
050100060516     C                   EVAL      OSIE3ERR  = '1'
050200060516     C                   EVAL      OSIE3ERRL = '96'
050300060520     C                   EVAL      OSIE3ERRD = 'Errore in calcolo ' +
050400060516     C                             'instradamento. (DPCRO01L)'
050500060516     C*
050600060516     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
050700060516     C                   IF        OSIE3ERR <> *blanks
050800060520     C                   SETON                                        70
050900060516     C                   ENDIF
051000060516     C*
051100060516     C                   else
051200060516     C*
051300060516     C* Valorizzo i dati dell'instradamento così reperito
051400061109     C                   eval      wO_Sort        = d_croOSRT
051500061109     C                   eval      wD_Depot       = d_croDDEP
051600061109     C                   eval      wD_Sort        = d_croDSRT
051700061109     C                   eval      wPty_Group     = d_croPTYG
051800061109     C                   eval      wBarcodeID     = d_croBID
051900061109     C                   eval      wBarcodeID_Prt = d_croBIDP
052000150518     C                   eval      wZIPCODE_Da    = d_croPTCB
052100150518     C                   eval      wZIPCODE_A     = d_croPTCE
052200060516     C*
052300060516     C                   endif
052400060516     C*
052500060516     C/EXEC SQL
052600060516     C+ close C1
052700060516     C/END-EXEC
052800060516     C*
052900060516     C                   ENDSR
053000060530     C*------------------------------------------------------------------------*
053100060530     C* CHKALWDNY - Routine di verifica instradamento calcolato rispetto a ALLOW e DENY
053200060530     C*------------------------------------------------------------------------*
053300060530     C     CHKALWDNY     BEGSR
053400060530     C*
053500060530     C                   CLEAR                   ds_dpcal
053600060530     C                   CLEAR                   ds_dpcdy
053700060530     C*
053800060530     C/EXEC SQL
053900060629     C+ declare C2 cursor for
054000061109     C+ select calID from dpcal01l
054100060629     C+ where calVER = :SIE2VER and
054200060629     C+ calRULF = :wR_Depot and
054300060629     C+ calSRVC = :ISIE3SRV and
054400060629     C+ ((calISO2 = :wNazIso2 and :USIE3CAD between calPTCB and calPTCE) or
054500060629     C+ calRULT = :wD_Depot) and
054600060629     C+ calATB = ' '
054700061110     C+ order by calVER, calRULF desc, calSRVC desc, calRULT desc, calISO2 desc
054800060622     C+ for read only
054900060530     C/END-EXEC
055000060530     C
055100060530     C/EXEC SQL
055200060530     C+ open C2
055300060530     C/END-EXEC
055400060530     C
055500060530     C/EXEC SQL
055600061109     C+ Fetch next from C2 into :d_calID
055700060530     C/END-EXEC
055800060530     C*
055900060530     C* Se reperito qualcosa => analizzo
056000060530     C* ....altrimenti trattasi d errore di permesso instradamento
056100060530     C                   if        sqlcod <> *zeros
056200060530     C                   EVAL      OSIE3ERR  = '1'
056300060530     C                   EVAL      OSIE3ERRL = '97'
056400060530     C                   EVAL      OSIE3ERRD = 'Instradamento non ' +
056500060530     C                             'consentito. (DPCAL01L)'
056600060530     C*
056700060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
056800060530     C                   IF        OSIE3ERR <> *blanks
056900060530     C                   SETON                                        70
057000060530     C                   ENDIF
057100060530     C*
057200060530     C                   else
057300060530     C*
057400060530     C/EXEC SQL
057500060629     C+ declare C3 cursor for
057600061109     C+ select cdyID from dpcdy01l
057700061109     C+ where cdyVER = :SIE2VER and cdyID = :d_calID and
057800060629     C+ cdyRULF = :wR_Depot and
057900060629     C+ cdySRVC = :ISIE3SRV and
058000060629     C+ ((cdyISO2 = :wNazIso2 and :USIE3CAD between cdyPTCB and cdyPTCE) or
058100060629     C+ cdyRULT = :wD_Depot) and
058200060629     C+ cdyATB = ' '
058300061109     C+ order by cdyVER, cdyID, cdyRULF desc, cdySRVC desc, cdyRULT desc,
058400061109     C+ cdyISO2 desc
058500060629     C+ for read only
058600060530     C/END-EXEC
058700060530     C
058800060530     C/EXEC SQL
058900060530     C+ open C3
059000060530     C/END-EXEC
059100060530     C
059200060530     C/EXEC SQL
059300061109     C+ Fetch next from C3 into :d_cdyID
059400060530     C/END-EXEC
059500060530     C*
059600060530     C* Se reperito qualcosa => trattasi d errore d divieto
059700060530     C* ....altrimenti trattasi tutto ok
059800060530     C                   if        sqlcod = *zeros
059900060530     C                   EVAL      OSIE3ERR  = '1'
060000060530     C                   EVAL      OSIE3ERRL = '98'
060100060530     C                   EVAL      OSIE3ERRD = 'Instradamento ' +
060200060530     C                             'negato. (DPCDY01L)'
060300060530     C*
060400060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
060500060530     C                   IF        OSIE3ERR <> *blanks
060600060530     C                   SETON                                        70
060700060530     C                   ENDIF
060800060530     C*
060900060530     C                   endif
061000060530     C*
061100060530     C/EXEC SQL
061200060530     C+ close C3
061300060530     C/END-EXEC
061400060530     C*
061500060530     C                   endif
061600060530     C*
061700060530     C/EXEC SQL
061800060530     C+ close C2
061900060530     C/END-EXEC
062000060530     C*
062100150518     C* Controllo per forzatura relativa alla sola nazione 'GB' =>
062200150518     C* ... consentiti unicamente CAP in bolla della medesima lunghezza dei CAP
062300150518     C* ... su CAPPARIO DPD
062400150518     C                   IF        wNazIso2 = 'GB'
062500150518     C                   IF        %len(%trim(USIE3CAD)) <>
062600150518     C                             %len(%trim(wZIPCODE_A))
062700150518     C*
062800150518     C                   EVAL      OSIE3ERR  = '1'
062900150518     C                   EVAL      OSIE3ERRL = '99'
063000150518     C                   EVAL      OSIE3ERRD = 'Errore - CAP errato. '
063100150518     C*
063200150518     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
063300150518     C                   IF        OSIE3ERR <> *blanks
063400150518     C                   SETON                                        70
063500150518     C                   ENDIF
063600150518     C*
063700150518     C                   ENDIF
063800150518     C                   ENDIF
063900150518     C*
064000060530     C                   ENDSR
064100060515     C*------------------------------------------------------------------------*
064200060515     C* VALOUT - Routine di valorizzazione dati ouput procedura corrente
064300060515     C*------------------------------------------------------------------------*
064400060515     C     VALOUT        BEGSR
064500060515     C*
064600060515     C                   EVAL      OSIE3VER  = SIE2VER
064700060515     C                   EVAL      OSIE3VERD = SIE2VERD
064800060515     C                   EVAL      OSIE3DDE  = SIE2DDE
064900060606     C*
065000060606     C* Effettuo forzatura => in stampa etichetta deve essere indicata la nazione del depot
065100060606     C* d arrivo e nn la nazione d destino della spedizione (strano ma vero !!!)
065200060606     C                   MOVEL(P)  wD_Depot      wCurrentDepot     4
065300060627     C                   SETON                                        60
065400060606     C                   EXSR      CHKDEP
065500060607     C                   EVAL      OSIE3IATAD = wIATALikeCode
065600060606     C                   IF        cdpISO2 <> *blanks
065700060606     C                   EVAL      cccVER  = SIE2VER
065800060606     C                   EVAL      cccISO2 = cdpISO2
065900060606     C     KEYccc02_C    CHAIN     DPCCC02L
066000060606     C                   IF        %found(DPCCC02L)
066100060606     C                   IF        cccATB = *blanks
066200060607     C                   EVAL      OSIE3NAZND = cccISOK
066300060607     C                   EVAL      OSIE3NAZ2D = cccISO2
066400060607     C                   EVAL      OSIE3NAZ3D = cccISO3
066500060606     C                   ENDIF
066600060606     C                   ENDIF
066700060606     C                   ENDIF
066800060606     C*
066900060515     C                   EVAL      OSIE3NAZN = wNazIsoN
067000060516     C                   EVAL      OSIE3NAZ2 = wNazIso2
067100060516     C                   EVAL      OSIE3NAZ3 = wNazIso3
067200060516     C                   EVAL      OSIE3SRVM = wSrvMark
067300060516     C                   EVAL      OSIE3SRVD = wSrvDes
067400060516     C                   EVAL      OSIE3OSRT = wO_Sort
067500060516     C                   EVAL      OSIE3RDEP = wR_Depot
067600060516     C                   EVAL      OSIE3DDEP = wD_Depot
067700060516     C                   EVAL      OSIE3DSRT = wD_Sort
067800060516     C                   EVAL      OSIE3PTYG = wPty_Group
067900060516     C                   EVAL      OSIE3BID  = wBarcodeID
068000060516     C                   EVAL      OSIE3BIDP = wBarcodeID_Prt
068100061109     C                   EVAL      OSIE3ALWID= d_calID
068200060515     C*
068300060515     C                   ENDSR
068400060515     C*--------------------------------------------------------------------------------------------*
068500060515     C* *inzsr - operazioni iniziali
068600060515     C*--------------------------------------------------------------------------------------------*
068700000000     C     *inzsr        BEGSR
068800060515     C*
068900060515     C* Ricevimento parametri
069000060515     C     *ENTRY        PLIST
069100060515     C                   PARM                    TISIE3DS
069200060515     C*
069300060515     C* Definizione chiavi di lettura
069400060515     C*
069500060515     C* TABEL00F - Completa
069600060515     C     KEYtabel_C    KLIST
069700060515     C                   KFLD                    tblKUT
069800060515     C                   KFLD                    tblCOD
069900060515     C                   KFLD                    tblKEY
070000060515     C*
070100060515     C* DPCCC01L - Completa
070200060515     C     KEYccc01_C    KLIST
070300060515     C                   KFLD                    cccVER
070400060515     C                   KFLD                    cccISOK
070500060606     C*
070600060606     C* DPCCC02L - Completa
070700060606     C     KEYccc02_C    KLIST
070800060606     C                   KFLD                    cccVER
070900060606     C                   KFLD                    cccISO2
071000060515     C*
071100060515     C* DPCSC01L - Completa
071200060515     C     KEYcsc01_C    KLIST
071300060515     C                   KFLD                    cscVER
071400060515     C                   KFLD                    cscSRVC
071500060515     C*
071600060515     C* DPCDP01L - Completa
071700061114     C     KEYcdp01_C    KLIST
071800060515     C                   KFLD                    cdpVER
071900060515     C                   KFLD                    cdpDPC
072000060519     C*
072100060519     C* DPCLO01L - Completa
072200060519     C     KEYclo01_C    KLIST
072300060519     C                   KFLD                    cloVER
072400060520     C                   KFLD                    cloISO2
072500060519     C                   KFLD                    cloAREA
072600060519     C                   KFLD                    cloCTY
072700060510     C*
072800060510     C* CALCOLA LA DATA CORRENTE
072900060510     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
073000060510     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
073100060510     C                   Z-ADD     WNUM8         G08DAT
073200060510     C                   Z-ADD     *ZEROS        G08INV
073300060510     C                   MOVEL     '0'           G08ERR
073400060510     C                   CALL      'XSRDA8'
073500060510     C                   PARM                    WLBDA8
073600060510     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
073700060515     C*
073800060515     C                   ENDSR
