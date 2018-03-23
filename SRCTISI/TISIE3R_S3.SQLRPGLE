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
003900061109     d DS_DPCAL      E DS                  EXTNAME(DPCAL00F) PREFIX(d_)
004000061109     d DS_DPCDY      E DS                  EXTNAME(DPCDY00F) PREFIX(d_)
004100060510     d*------------------
004200060510     d* DS DI PROCEDURA
004300060510     d*------------------
004400060510     d TISIE2DS      E DS
004500060515     d TISIE3DS      E DS
004600060515     d*------------------
004700060515     d* VARIABILI D WRK
004800060515     d*------------------
004900000000     c*--------------------------------------------------------------------------------------------*
005000000000     c* Main lines
005100000000     c*--------------------------------------------------------------------------------------------*
005200060516     C*
005300060516     C* Verifico subito il tipo lancio richiesto
005400060516     C                   if        ISIE3TLA = 'C'                               * solo chiusura
005500060516     C                   seton                                        lr
005600060516     C                   endif
005700060516     C*
005800060516     C                   if        ISIE3TLA = 'E'                               * solo elaborazione
005900060516     C                   exsr      esegui
006000060516     C                   seton                                        rt
006100060516     C                   endif
006200060516     C*
006300060516     C                   if        ISIE3TLA = *blanks                           * elabora e chiude
006400060516     C                   exsr      esegui
006500060516     C                   seton                                        lr
006600060516     C                   endif
006700060515     C*
006800060515     C*------------------------------------------------------------------------*
006900060515     C* ESEGUI - Routine di calcolo instradamento cappario DPD GeoPost
007000060515     C*------------------------------------------------------------------------*
007100060515     C     ESEGUI        BEGSR
007200060520     C*
007300060520     C* Resetto indicatore d errore
007400060520     C                   SETOFF                                       70
007500060515     C*
007600060515     C* Innanzitutto inizializzo i campi d output della ds d procedura corrente
007700060515     C                   EXSR      AZZOUT
007800060515     C*
007900060515     C* Quindi controllo correttezza parametri in input
008000060515     C                   EXSR      CHKPAR
008100061114     C*
008200061114     C* Quindi reperisco la versione da considerare
008300061114     C  N70              EXSR      REPVER
008400060515     C*
008500060515     C* Reperisco il depot DPD relativo alla linea di partenza passata in input
008600060520     C  N70              EXSR      REPDEP
008700060515     C*
008800060515     C* Verifico esistenza depot partenza DPD sui file cappario DPD
008900060606     C                   MOVEL(P)  wR_Depot      wCurrentDepot     4
009000060627     C                   SETOFF                                       60
009100060606     C  N70              EXSR      CHKDEP
009200060515     C*
009300060515     C* Reperisco dati relativi alla nazione destinatario
009400060520     C  N70              EXSR      REPNAZ
009500060519     C*
009600060519     C* Verifica se nazione destino gestisce il cappario (vedi Irlanda)
009700060520     C  N70              EXSR      CHKPTCSYS
009800060515     C*
009900060515     C* Reperisco dati relativi al servizio DPD
010000060520     C  N70              EXSR      REPSRV
010100060519     C*
010200060519     C* Verifico instradamento x area/località DPD
010300060530     C  N70              EXSR      REPCTY
010400060516     C*
010500060516     C* Reperisco dati relativi all'instradamento DPD
010600060520     C  N70              EXSR      REPROUTE
010700060530     C*
010800060530     C* Vaglio instradamento calcolato rispetto a ALLOW (permessi) e DENY (divieti)
010900060530     C  N70              EXSR      CHKALWDNY
011000060515     C*
011100060515     C* Al termine valorizzo i dati d output d procedura corrente
011200060520     C  N70              EXSR      VALOUT
011300060515     C*
011400060515     C                   ENDSR
011500060515     C*------------------------------------------------------------------------*
011600060515     C* AZZOUT - Routine di inizializzazione campi d ouput procedura corrente
011700060515     C*------------------------------------------------------------------------*
011800060515     C     AZZOUT        BEGSR
011900060515     C*
012000060515     C                   EVAL      OSIE3VER  = *zeros
012100060515     C                   EVAL      OSIE3VERD = *blanks
012200060515     C                   EVAL      OSIE3DDE  = *zeros
012300060515     C                   EVAL      OSIE3NAZN = *zeros
012400060515     C                   EVAL      OSIE3NAZ2 = *blanks
012500060515     C                   EVAL      OSIE3NAZ3 = *blanks
012600060515     C                   EVAL      OSIE3SRVM = *blanks
012700060515     C                   EVAL      OSIE3SRVD = *blanks
012800060515     C                   EVAL      OSIE3OSRT = *blanks
012900060515     C                   EVAL      OSIE3RDEP = *blanks
013000060515     C                   EVAL      OSIE3DDEP = *blanks
013100060515     C                   EVAL      OSIE3DSRT = *blanks
013200060515     C                   EVAL      OSIE3PTYG = *blanks
013300060515     C                   EVAL      OSIE3BID  = *zeros
013400060516     C                   EVAL      OSIE3BIDP = *blanks
013500060515     C                   EVAL      OSIE3ERR  = *blanks
013600060515     C                   EVAL      OSIE3ERRL = *blanks
013700060520     C                   EVAL      OSIE3ERRD = *blanks
013800060515     C*
013900060515     C                   ENDSR
014000060515     C*------------------------------------------------------------------------*
014100060515     C* CHKPAR - Routine di controllo correttezza parametri d input
014200060515     C*------------------------------------------------------------------------*
014300060515     C     CHKPAR        BEGSR
014400060520     C*
014500060520     C* Inizializzo variabile esplicitamento errori
014600060520     C                   MOVEL     *blanks       wErrore          20
014700060531     C*
014800060531     C* Innanzitutto imposto il CAP "utilizzato" = al CAP destinatario passato in input
014900060608     C                   EVAL      USIE3CAD = %trim(ISIE3CAD)
015000060608     C*
015100060608     C* Normalizzo il CAP destinatario compattando gli spazi eventualmente presenti
015200060608     C                   Z-ADD     1             wPos              1 0
015300060629     C                   DOW       wPos < %len(%trim(USIE3CAD))
015400060608     C                   IF        %subst(USIE3CAD:wPos:1) = *blanks
015500060608     C                   EVAL      USIE3CAD = %subst(USIE3CAD:1:wPos-1) +
015600060608     C                                        %subst(USIE3CAD:wPos+1)
015700060621     C                   EVAL      wPos = wPos - 1
015800060608     C                   ENDIF
015900060608     C                   ADD       1             wPos
016000060608     C                   ENDDO
016100060515     C*
016200060515     C* Verifica presenza campi obbligatori
016300060515     C*
016400060515     C* - data spedizione...
016500060515     C                   IF        ISIE3DSP = *zeros
016600060520     C                   EVAL      wErrore = '(Data spedizione)'
016700060515     C                   EVAL      OSIE3ERR  = '1'
016800060515     C                   ENDIF
016900060515     C*
017000060515     C* - cap destinatario
017100060515     C                   IF        ISIE3CAD = *blanks
017200060520     C                   EVAL      wErrore = '(CAP destinatario)'
017300060515     C                   EVAL      OSIE3ERR  = '1'
017400060515     C                   ENDIF
017500060515     C*
017600060515     C* - servizio DPD
017700060515     C                   IF        ISIE3SRV = *zeros
017800060520     C                   EVAL      wErrore = '(Servizio DPD)'
017900060515     C                   EVAL      OSIE3ERR  = '1'
018000060515     C                   ENDIF
018100060515     C*
018200060515     C* - linea di partenza
018300060515     C                   IF        ISIE3LNP = *zeros
018400060520     C                   EVAL      wErrore = '(Linea partenza)'
018500060515     C                   EVAL      OSIE3ERR  = '1'
018600060515     C                   ENDIF
018700060515     C*
018800060515     C* Verifica campi facoltativi
018900060520     C*
019000060520     C* - data riferimento...
019100060520     C                   IF        ISIE3DRI = *zeros
019200060606     C***                EVAL      ISIE3DRI = DATCOR
019300060606     C                   EVAL      ISIE3DRI = ISIE3DSP
019400060520     C                   ENDIF
019500060515     C*
019600060515     C* - ora spedizione...
019700060515     C                   IF        ISIE3HSP = *zeros
019800060515     C                   EVAL      ISIE3HSP = 120000
019900060515     C                   ENDIF
020000060515     C*
020100060515     C* - eccezioine x Bartolini...
020200060515     C                   IF        ISIE3EXC = *blanks
020300060515     C                   EVAL      ISIE3EXC = 'N'
020400060515     C                   ENDIF
020500060515     C*
020600060515     C* Se nn ok => imposto subito l'errore generale d procedura
020700060515     C                   IF        OSIE3ERR <> *blanks
020800060516     C                   EVAL      OSIE3ERRL = '92'
020900060520     C                   EVAL      OSIE3ERRD = 'Errore: parametri obbligatori '+
021000060520     C                             'non valorizzati.' + %trimr(wErrore)
021100060515     C                   ENDIF
021200060515     C*
021300060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
021400060515     C                   IF        OSIE3ERR <> *blanks
021500060520     C                   SETON                                        70
021600060515     C                   ENDIF
021700060515     C*
021800060515     C                   ENDSR
021900060515     C*------------------------------------------------------------------------*
022000060515     C* REPVER - Routine di reperimento versione cappario DPD GeoPost
022100060515     C*------------------------------------------------------------------------*
022200060515     C     REPVER        BEGSR
022300060515     C*
022400060515     C                   CLEAR                   TISIE2DS
022500060515     C                   EVAL      SIE2TLA = 'E'
022600060515     C                   EVAL      SIE2DRI = ISIE3DRI
022700060515     C                   CALL      'TISIE2R'
022800060515     C                   PARM                    TISIE2DS
022900060515     C*
023000060515     C* Se nn ok => imposto subito l'errore generale d procedura
023100060515     C                   IF        SIE2VER = *zeros
023200060515     C                   EVAL      OSIE3ERR  = '1'
023300060516     C                   EVAL      OSIE3ERRL = '90'
023400060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento versione'+
023500060515     C                             '. (DPCVE01L)'
023600060515     C                   ENDIF
023700060515     C*
023800060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
023900060515     C                   IF        OSIE3ERR <> *blanks
024000060520     C                   SETON                                        70
024100060515     C                   ENDIF
024200060515     C*
024300060515     C                   ENDSR
024400060515     C*------------------------------------------------------------------------*
024500060515     C* REPDEP - Routine di reperimento depot DPD di partenza
024600060515     C*------------------------------------------------------------------------*
024700060515     C     REPDEP        BEGSR
024800060515     C*
024900060515     C                   MOVEL     *blanks       wR_Depot          4
025000061114     C*
025100061114     C     ISIE3LNP      CHAIN     AZORG01L
025200061114     C                   IF        %found(AZORG01L)
025300061114     C                   EVAL      OG143 = ORGDE3
025400061114     C                   EVAL      wR_Depot = §OGDP1
025500061114     C                   ENDIF
025600060515     C*
025700060515     C* Se nn ok => imposto subito l'errore generale d procedura
025800060515     C                   IF        wR_Depot = *blanks
025900060515     C                   EVAL      OSIE3ERR  = '1'
026000060516     C                   EVAL      OSIE3ERRL = '91'
026100060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento depot ' +
026200060515     C                             ' partenza. (AZORG01L)'
026300060515     C                   ENDIF
026400060515     C*
026500060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
026600060515     C                   IF        OSIE3ERR <> *blanks
026700060520     C                   SETON                                        70
026800060515     C                   ENDIF
026900060515     C*
027000060515     C                   ENDSR
027100060515     C*------------------------------------------------------------------------*
027200060515     C* CHKDEP - Routine di verifica esistenza depot DPD partenza su file cappario DPD
027300060515     C*------------------------------------------------------------------------*
027400061114     C     CHKDEP        BEGSR
027500061114     C*
027600061114     C                   CLEAR                   DPCDP000
027700061114     C                   MOVEL     *blanks       wIATALikeCode     3
027800061114     C*
027900061114     C                   EVAL      cdpVER = SIE2VER
028000061114     C                   EVAL      cdpDPC = wCurrentDepot
028100061114     C     KEYcdp01_C    CHAIN     DPCDP01L
028200061114     C                   IF        %found(DPCDP01L)
028300061114     C                   IF        cdpATB = *blanks
028400061114     C                   MOVEL(P)  cdpIATA       wIATALikeCode
028500061114     C                   ELSE
028600061114     C                   EVAL      OSIE3ERR  = '1'
028700061114     C                   EVAL      OSIE3ERRL = '94'
028800061114     C  N60              EVAL      OSIE3ERRD = 'Errore: depot partenza ' +
028900061114     C                             'annullato. (DPCDP01L)'
029000061114     C   60              EVAL      OSIE3ERRD = 'Errore: depot arrivo ' +
029100061114     C                             'annullato. (DPCDP01L)'
029200061114     C                   ENDIF
029300061114     C                   ELSE
029400061114     C                   EVAL      OSIE3ERR  = '1'
029500061114     C                   EVAL      OSIE3ERRL = '94'
029600061114     C  N60              EVAL      OSIE3ERRD = 'Errore: depot partenza ' +
029700061114     C                             'inesistente. (DPCDP01L)'
029800061114     C   60              EVAL      OSIE3ERRD = 'Errore: depot arrivo ' +
029900061114     C                             'inesistente. (DPCDP01L)'
030000061114     C                   ENDIF
030100061114     C*
030200061114     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
030300061114     C                   IF        OSIE3ERR <> *blanks
030400061114     C                   SETON                                        70
030500061114     C                   ENDIF
030600060515     C*
030700060515     C                   ENDSR
030800060515     C*------------------------------------------------------------------------*
030900060515     C* REPNAZ - Routine di reperimento dati nazione destinatario
031000060515     C*------------------------------------------------------------------------*
031100060515     C     REPNAZ        BEGSR
031200060515     C*
031300060515     C                   Z-ADD     *zeros        wNazIsoN          3 0
031400060515     C                   MOVEL     *blanks       wNazIso2          2
031500060515     C                   MOVEL     *blanks       wNazIso3          3
031600060519     C                   MOVEL     *blanks       wNazPTC           1 0
031700060515     C*
031800061114     C                   CLEAR                   DS15
031900060515     C                   EVAL      tblKUT = 1
032000060515     C                   EVAL      tblCOD = '15'
032100060515     C                   EVAL      tblKEY = ISIE3NZD
032200060515     C     KEYtabel_C    CHAIN     TABEL00F
032300060515     C                   IF        %found(TABEL00F)
032400060515     C                   IF        tblFLG = *blanks
032500060515     C                   EVAL      DS15 = tblUNI
032600060515     C                   EVAL      cccVER  = SIE2VER
032700060515     C                   EVAL      cccISOK = §15CIE
032800060515     C     KEYccc01_C    CHAIN     DPCCC01L
032900060606     C                   IF        %found(DPCCC01L)
033000060515     C                   IF        cccATB = *blanks
033100060515     C                   EVAL      wNazIsoN = cccISOK
033200060515     C                   EVAL      wNazIso2 = cccISO2
033300060515     C                   EVAL      wNazIso3 = cccISO3
033400060519     C                   EVAL      wNazPTC  = cccPTC
033500060515     C                   ENDIF
033600060515     C                   ENDIF
033700060515     C                   ENDIF
033800060515     C                   ENDIF
033900060515     C*
034000060515     C* Se nn ok => imposto subito l'errore generale d procedura
034100060515     C                   IF        wNazIsoN = *zeros  OR
034200060515     C                             wNazIso2 = *blanks OR
034300060515     C                             wNazIso3 = *blanks
034400060515     C                   EVAL      OSIE3ERR  = '1'
034500060516     C                   EVAL      OSIE3ERRL = '93'
034600060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento nazione.'+
034700060515     C                             ' (DPCCC01L)'
034800060515     C                   ENDIF
034900060515     C*
035000060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
035100060515     C                   IF        OSIE3ERR <> *blanks
035200060520     C                   SETON                                        70
035300060515     C                   ENDIF
035400060515     C*
035500060515     C                   ENDSR
035600060519     C*------------------------------------------------------------------------*
035700060519     C* CHKPTCSYS - Verifica se nazione destino gestisce il cappario (vedi Irlanda)
035800060519     C*------------------------------------------------------------------------*
035900060519     C     CHKPTCSYS     BEGSR
036000060519     C*
036100060519     C* Se nazione nn gestisce il cappario => forzo a *blanks il campo CAP destinatario
036200060519     C* ricevuto in input
036300060519     C                   IF        wNazPTC = 1
036400060531     C                   EVAL      USIE3CAD = *blanks
036500060519     C                   ENDIF
036600060519     C*
036700060519     C* Eseguo forzatura => in caso d nazione destinatario Irlanda forzo il CAP
036800060519     C* destinatario a '1' fisso
036900060519     C                   IF        ISIE3NZD = 'IRL'
037000060531     C                   EVAL      USIE3CAD = '1'
037100060519     C                   ENDIF
037200060519     C*
037300060519     C                   ENDSR
037400060515     C*------------------------------------------------------------------------*
037500060515     C* REPSRV - Routine di verifica e reperimento dati relativi al servizio DPD
037600060515     C*------------------------------------------------------------------------*
037700060515     C     REPSRV        BEGSR
037800060515     C*
037900060515     C                   MOVEL     *blanks       wSrvMark          1
038000060515     C                   MOVEL     *blanks       wSrvDes          16
038100060515     C*
038200060515     C                   EVAL      cscVER  = SIE2VER
038300060515     C                   EVAL      cscSRVC = ISIE3SRV
038400060515     C     KEYcsc01_C    CHAIN     DPCSC01L
038500060515     C                   IF        %found(DPCSC01L)
038600060515     C                   IF        cscATB = *blanks
038700060515     C                   EVAL      wSrvMark = cscSRVM
038800060515     C                   EVAL      wSrvDes  = CSCSRVD
038900060515     C                   ENDIF
039000060515     C                   ENDIF
039100060515     C*
039200060515     C* Se nn ok => imposto subito l'errore generale d procedura
039300060520     C                   IF        wSrvMark = *blanks AND
039400060515     C                             wSrvDes  = *blanks
039500060515     C                   EVAL      OSIE3ERR  = '1'
039600060516     C                   EVAL      OSIE3ERRL = '95'
039700060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento servizio'+
039800060515     C                             '. (DPCSC01L)'
039900060515     C                   ENDIF
040000060515     C*
040100060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
040200060515     C                   IF        OSIE3ERR <> *blanks
040300060520     C                   SETON                                        70
040400060515     C                   ENDIF
040500060515     C*
040600060515     C                   ENDSR
040700060519     C*------------------------------------------------------------------------*
040800060519     C* REPCTY - Routine di verifica esistenza ecezione x area/località DPD
040900060519     C*------------------------------------------------------------------------*
041000060530     C     REPCTY        BEGSR
041100060519     C*
041200060519     C* Se ricevuti in ingresso AREA e/o LOCALITA "provo" d gestire
041300060519     C                   IF        ISIE3AREA <> *blanks OR
041400060519     C                             ISIE3CTY  <> *blanks
041500060519     C*
041600060519     C                   EVAL      cloVER  = SIE2VER
041700060520     C                   EVAL      cloISO2 = wNazIso2
041800060519     C                   EVAL      cloAREA = ISIE3AREA
041900060519     C                   EVAL      cloCTY  = ISIE3CTY
042000060519     C     KEYclo01_C    CHAIN     DPCLO01L
042100060519     C                   IF        %found(DPCLO01L)
042200060519     C                   IF        cloATB = *blanks
042300060519     C*
042400060519     C* Se trovata eccezione x AREA/LOCALITA forzo il valore dell'ecezione sul CAP
042500060519     C* destinatario ricevuto in input
042600060531     C                   EVAL      USIE3CAD = cloPTC
042700060519     C                   ENDIF
042800060519     C                   ENDIF
042900060519     C*
043000060519     C                   ENDIF
043100060519     C*
043200060519     C                   ENDSR
043300060516     C*------------------------------------------------------------------------*
043400060516     C* REPROUTE - Routine di reperimento dati relativi all'instradamento DPD
043500060516     C*------------------------------------------------------------------------*
043600060516     C     REPROUTE      BEGSR
043700060516     C*
043800060530     C                   CLEAR                   ds_dpcro
043900060516     C                   MOVEL     *blanks       wO_Sort           4
044000060516     C                   MOVEL     *blanks       wD_Depot          4
044100060516     C                   MOVEL     *blanks       wD_Sort           4
044200060516     C                   MOVEL     *blanks       wPty_Group        1
044300060516     C                   Z-ADD     *zeros        wBarcodeID        3 0
044400060516     C                   MOVEL     *blanks       wBarcodeID_Prt    1
044500060516     C*
044600060516     C/EXEC SQL
044700060622     C+ declare C1 cursor for
044800061109     C+ select croOSRT, croDDEP, croDSRT, croPTYG, croBID,
044900061109     C+ croBIDP from dpcro03l
045000060516     C+ where croVER = :SIE2VER and croISO2 = :wNazIso2 and
045100060516     C+ (croSRVC = :ISIE3SRV or croSRVC = 0)  and
045200060608     C+ (croROUP = :wR_Depot or croROUP = '0000') and
045300060531     C+ :USIE3CAD between croPTCB and croPTCE and
045400060530     C+ croATB = ' '
045500070523     C+ order by croSRVC desc, croROUP desc, croPTYG desc
045600060622     C+ for read only
045700060516     C/END-EXEC
045800060516     C
045900060516     C/EXEC SQL
046000060516     C+ open C1
046100060516     C/END-EXEC
046200060516     C
046300060516     C/EXEC SQL
046400060622     C+ Fetch next from C1 into :ds_dpcro
046500060516     C/END-EXEC
046600060516     C*
046700060516     C* Se reperito qualcosa => analizzo
046800060516     C* ....altrimenti trattasi d errore di instradamento
046900060516     C                   if        sqlcod <> *zeros
047000060516     C                   EVAL      OSIE3ERR  = '1'
047100060516     C                   EVAL      OSIE3ERRL = '96'
047200060520     C                   EVAL      OSIE3ERRD = 'Errore in calcolo ' +
047300060516     C                             'instradamento. (DPCRO01L)'
047400060516     C*
047500060516     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
047600060516     C                   IF        OSIE3ERR <> *blanks
047700060520     C                   SETON                                        70
047800060516     C                   ENDIF
047900060516     C*
048000060516     C                   else
048100060516     C*
048200060516     C* Valorizzo i dati dell'instradamento così reperito
048300061109     C                   eval      wO_Sort        = d_croOSRT
048400061109     C                   eval      wD_Depot       = d_croDDEP
048500061109     C                   eval      wD_Sort        = d_croDSRT
048600061109     C                   eval      wPty_Group     = d_croPTYG
048700061109     C                   eval      wBarcodeID     = d_croBID
048800061109     C                   eval      wBarcodeID_Prt = d_croBIDP
048900060516     C*
049000060516     C                   endif
049100060516     C*
049200060516     C/EXEC SQL
049300060516     C+ close C1
049400060516     C/END-EXEC
049500060516     C*
049600060516     C                   ENDSR
049700060530     C*------------------------------------------------------------------------*
049800060530     C* CHKALWDNY - Routine di verifica instradamento calcolato rispetto a ALLOW e DENY
049900060530     C*------------------------------------------------------------------------*
050000060530     C     CHKALWDNY     BEGSR
050100060530     C*
050200060530     C                   CLEAR                   ds_dpcal
050300060530     C                   CLEAR                   ds_dpcdy
050400060530     C*
050500060530     C/EXEC SQL
050600060629     C+ declare C2 cursor for
050700061109     C+ select calID from dpcal01l
050800060629     C+ where calVER = :SIE2VER and
050900060629     C+ calRULF = :wR_Depot and
051000060629     C+ calSRVC = :ISIE3SRV and
051100060629     C+ ((calISO2 = :wNazIso2 and :USIE3CAD between calPTCB and calPTCE) or
051200060629     C+ calRULT = :wD_Depot) and
051300060629     C+ calATB = ' '
051400061110     C+ order by calVER, calRULF desc, calSRVC desc, calRULT desc, calISO2 desc
051500060622     C+ for read only
051600060530     C/END-EXEC
051700060530     C
051800060530     C/EXEC SQL
051900060530     C+ open C2
052000060530     C/END-EXEC
052100060530     C
052200060530     C/EXEC SQL
052300061109     C+ Fetch next from C2 into :d_calID
052400060530     C/END-EXEC
052500060530     C*
052600060530     C* Se reperito qualcosa => analizzo
052700060530     C* ....altrimenti trattasi d errore di permesso instradamento
052800060530     C                   if        sqlcod <> *zeros
052900060530     C                   EVAL      OSIE3ERR  = '1'
053000060530     C                   EVAL      OSIE3ERRL = '97'
053100060530     C                   EVAL      OSIE3ERRD = 'Instradamento non ' +
053200060530     C                             'consentito. (DPCAL01L)'
053300060530     C*
053400060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
053500060530     C                   IF        OSIE3ERR <> *blanks
053600060530     C                   SETON                                        70
053700060530     C                   ENDIF
053800060530     C*
053900060530     C                   else
054000060530     C*
054100060530     C/EXEC SQL
054200060629     C+ declare C3 cursor for
054300061109     C+ select cdyID from dpcdy01l
054400061109     C+ where cdyVER = :SIE2VER and cdyID = :d_calID and
054500060629     C+ cdyRULF = :wR_Depot and
054600060629     C+ cdySRVC = :ISIE3SRV and
054700060629     C+ ((cdyISO2 = :wNazIso2 and :USIE3CAD between cdyPTCB and cdyPTCE) or
054800060629     C+ cdyRULT = :wD_Depot) and
054900060629     C+ cdyATB = ' '
055000061109     C+ order by cdyVER, cdyID, cdyRULF desc, cdySRVC desc, cdyRULT desc,
055100061109     C+ cdyISO2 desc
055200060629     C+ for read only
055300060530     C/END-EXEC
055400060530     C
055500060530     C/EXEC SQL
055600060530     C+ open C3
055700060530     C/END-EXEC
055800060530     C
055900060530     C/EXEC SQL
056000061109     C+ Fetch next from C3 into :d_cdyID
056100060530     C/END-EXEC
056200060530     C*
056300060530     C* Se reperito qualcosa => trattasi d errore d divieto
056400060530     C* ....altrimenti trattasi tutto ok
056500060530     C                   if        sqlcod = *zeros
056600060530     C                   EVAL      OSIE3ERR  = '1'
056700060530     C                   EVAL      OSIE3ERRL = '98'
056800060530     C                   EVAL      OSIE3ERRD = 'Instradamento ' +
056900060530     C                             'negato. (DPCDY01L)'
057000060530     C*
057100060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
057200060530     C                   IF        OSIE3ERR <> *blanks
057300060530     C                   SETON                                        70
057400060530     C                   ENDIF
057500060530     C*
057600060530     C                   endif
057700060530     C*
057800060530     C/EXEC SQL
057900060530     C+ close C3
058000060530     C/END-EXEC
058100060530     C*
058200060530     C                   endif
058300060530     C*
058400060530     C/EXEC SQL
058500060530     C+ close C2
058600060530     C/END-EXEC
058700060530     C*
058800060530     C                   ENDSR
058900060515     C*------------------------------------------------------------------------*
059000060515     C* VALOUT - Routine di valorizzazione dati ouput procedura corrente
059100060515     C*------------------------------------------------------------------------*
059200060515     C     VALOUT        BEGSR
059300060515     C*
059400060515     C                   EVAL      OSIE3VER  = SIE2VER
059500060515     C                   EVAL      OSIE3VERD = SIE2VERD
059600060515     C                   EVAL      OSIE3DDE  = SIE2DDE
059700060606     C*
059800060606     C* Effettuo forzatura => in stampa etichetta deve essere indicata la nazione del depot
059900060606     C* d arrivo e nn la nazione d destino della spedizione (strano ma vero !!!)
060000060606     C                   MOVEL(P)  wD_Depot      wCurrentDepot     4
060100060627     C                   SETON                                        60
060200060606     C                   EXSR      CHKDEP
060300060607     C                   EVAL      OSIE3IATAD = wIATALikeCode
060400060606     C                   IF        cdpISO2 <> *blanks
060500060606     C                   EVAL      cccVER  = SIE2VER
060600060606     C                   EVAL      cccISO2 = cdpISO2
060700060606     C     KEYccc02_C    CHAIN     DPCCC02L
060800060606     C                   IF        %found(DPCCC02L)
060900060606     C                   IF        cccATB = *blanks
061000060607     C                   EVAL      OSIE3NAZND = cccISOK
061100060607     C                   EVAL      OSIE3NAZ2D = cccISO2
061200060607     C                   EVAL      OSIE3NAZ3D = cccISO3
061300060606     C                   ENDIF
061400060606     C                   ENDIF
061500060606     C                   ENDIF
061600060606     C*
061700060515     C                   EVAL      OSIE3NAZN = wNazIsoN
061800060516     C                   EVAL      OSIE3NAZ2 = wNazIso2
061900060516     C                   EVAL      OSIE3NAZ3 = wNazIso3
062000060516     C                   EVAL      OSIE3SRVM = wSrvMark
062100060516     C                   EVAL      OSIE3SRVD = wSrvDes
062200060516     C                   EVAL      OSIE3OSRT = wO_Sort
062300060516     C                   EVAL      OSIE3RDEP = wR_Depot
062400060516     C                   EVAL      OSIE3DDEP = wD_Depot
062500060516     C                   EVAL      OSIE3DSRT = wD_Sort
062600060516     C                   EVAL      OSIE3PTYG = wPty_Group
062700060516     C                   EVAL      OSIE3BID  = wBarcodeID
062800060516     C                   EVAL      OSIE3BIDP = wBarcodeID_Prt
062900061109     C                   EVAL      OSIE3ALWID= d_calID
063000060515     C*
063100060515     C                   ENDSR
063200060515     C*--------------------------------------------------------------------------------------------*
063300060515     C* *inzsr - operazioni iniziali
063400060515     C*--------------------------------------------------------------------------------------------*
063500000000     C     *inzsr        BEGSR
063600060515     C*
063700060515     C* Ricevimento parametri
063800060515     C     *ENTRY        PLIST
063900060515     C                   PARM                    TISIE3DS
064000060515     C*
064100060515     C* Definizione chiavi di lettura
064200060515     C*
064300060515     C* TABEL00F - Completa
064400060515     C     KEYtabel_C    KLIST
064500060515     C                   KFLD                    tblKUT
064600060515     C                   KFLD                    tblCOD
064700060515     C                   KFLD                    tblKEY
064800060515     C*
064900060515     C* DPCCC01L - Completa
065000060515     C     KEYccc01_C    KLIST
065100060515     C                   KFLD                    cccVER
065200060515     C                   KFLD                    cccISOK
065300060606     C*
065400060606     C* DPCCC02L - Completa
065500060606     C     KEYccc02_C    KLIST
065600060606     C                   KFLD                    cccVER
065700060606     C                   KFLD                    cccISO2
065800060515     C*
065900060515     C* DPCSC01L - Completa
066000060515     C     KEYcsc01_C    KLIST
066100060515     C                   KFLD                    cscVER
066200060515     C                   KFLD                    cscSRVC
066300060515     C*
066400060515     C* DPCDP01L - Completa
066500061114     C     KEYcdp01_C    KLIST
066600060515     C                   KFLD                    cdpVER
066700060515     C                   KFLD                    cdpDPC
066800060519     C*
066900060519     C* DPCLO01L - Completa
067000060519     C     KEYclo01_C    KLIST
067100060519     C                   KFLD                    cloVER
067200060520     C                   KFLD                    cloISO2
067300060519     C                   KFLD                    cloAREA
067400060519     C                   KFLD                    cloCTY
067500060510     C*
067600060510     C* CALCOLA LA DATA CORRENTE
067700060510     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
067800060510     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
067900060510     C                   Z-ADD     WNUM8         G08DAT
068000060510     C                   Z-ADD     *ZEROS        G08INV
068100060510     C                   MOVEL     '0'           G08ERR
068200060510     C                   CALL      'XSRDA8'
068300060510     C                   PARM                    WLBDA8
068400060510     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
068500060622     C*
068600060622     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
068700060622     C
068800060622     C/EXEC SQL
068900060622     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
069000060622     C/END-EXEC
069100060515     C*
069200060515     C                   ENDSR
