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
002600061109     d*------------------
002700061109     d* SCHIERE D WRK
002800061109     d*------------------
002900061110     d*
003000061109     d* Abbinamenti organigramma <=> depot
003100061110     d  skORGFIL       s                   dim(350) like(orgFIL) inz(*zero)
003200061110     d  skORGCDP       s                   dim(350) like(§ogdp1) inz(*blanks)
003300061110     d  jORG           s              3i 0 inz(*zeros)
003400061110     d*
003500061110 xxx d* Anagrafico depot DPD GeoPost
003600061110     d  skCDPCDP       s                   dim(3000) like(cdpDPC)  inz(*blanks)
003700061110     d  skCDPIATA      s                   dim(3000) like(cdpIATA) inz(*blanks)
003800061110     d  jCDP           s              5i 0 inz(*zeros)
003900061109     d
004000061109     d
004100061109     d
004200061109     d
004300061109     d
004400061109     d
004500060515     d*------------------
004600060515     d* DS DI WRK
004700060515     d*------------------
004800060515     d DS15          E DS
004900060515     d OG143         E DS
005000061109     d DPCRO00F      E DS
005100061109     d DS_DPCRO        DS
005200061109     d  d_croOSRT                          like(croOSRT)
005300061109     d  d_croDDEP                          like(croDDEP)
005400061109     d  d_croDSRT                          like(croDSRT)
005500061109     d  d_croPTYG                          like(croPTYG)
005600061109     d  d_croBID                           like(croBID)
005700061109     d  d_croBIDP                          like(croBIDP)
005800061109     d DS_DPCAL      E DS                  EXTNAME(DPCAL00F) PREFIX(d_)
005900061109     d DS_DPCDY      E DS                  EXTNAME(DPCDY00F) PREFIX(d_)
006000060510     d*------------------
006100060510     d* DS DI PROCEDURA
006200060510     d*------------------
006300060510     d TISIE2DS      E DS
006400060515     d TISIE3DS      E DS
006500060515     d*------------------
006600060515     d* VARIABILI D WRK
006700060515     d*------------------
006800000000     c*--------------------------------------------------------------------------------------------*
006900000000     c* Main lines
007000000000     c*--------------------------------------------------------------------------------------------*
007100060516     C*
007200060516     C* Verifico subito il tipo lancio richiesto
007300060516     C                   if        ISIE3TLA = 'C'                               * solo chiusura
007400060516     C                   seton                                        lr
007500060516     C                   endif
007600060516     C*
007700060516     C                   if        ISIE3TLA = 'E'                               * solo elaborazione
007800060516     C                   exsr      esegui
007900060516     C                   seton                                        rt
008000060516     C                   endif
008100060516     C*
008200060516     C                   if        ISIE3TLA = *blanks                           * elabora e chiude
008300060516     C                   exsr      esegui
008400060516     C                   seton                                        lr
008500060516     C                   endif
008600060515     C*
008700060515     C*------------------------------------------------------------------------*
008800060515     C* ESEGUI - Routine di calcolo instradamento cappario DPD GeoPost
008900060515     C*------------------------------------------------------------------------*
009000060515     C     ESEGUI        BEGSR
009100060520     C*
009200060520     C* Resetto indicatore d errore
009300060520     C                   SETOFF                                       70
009400060515     C*
009500060515     C* Innanzitutto inizializzo i campi d output della ds d procedura corrente
009600060515     C                   EXSR      AZZOUT
009700060515     C*
009800060515     C* Quindi controllo correttezza parametri in input
009900060515     C                   EXSR      CHKPAR
010000060515     C*
010100060515     C* Reperisco il depot DPD relativo alla linea di partenza passata in input
010200060520     C  N70              EXSR      REPDEP
010300060515     C*
010400060515     C* Verifico esistenza depot partenza DPD sui file cappario DPD
010500060606     C                   MOVEL(P)  wR_Depot      wCurrentDepot     4
010600060627     C                   SETOFF                                       60
010700060606     C  N70              EXSR      CHKDEP
010800060515     C*
010900060515     C* Reperisco dati relativi alla nazione destinatario
011000060520     C  N70              EXSR      REPNAZ
011100060519     C*
011200060519     C* Verifica se nazione destino gestisce il cappario (vedi Irlanda)
011300060520     C  N70              EXSR      CHKPTCSYS
011400060515     C*
011500060515     C* Reperisco dati relativi al servizio DPD
011600060520     C  N70              EXSR      REPSRV
011700060519     C*
011800060519     C* Verifico instradamento x area/località DPD
011900060530     C  N70              EXSR      REPCTY
012000060516     C*
012100060516     C* Reperisco dati relativi all'instradamento DPD
012200060520     C  N70              EXSR      REPROUTE
012300060530     C*
012400060530     C* Vaglio instradamento calcolato rispetto a ALLOW (permessi) e DENY (divieti)
012500060530     C  N70              EXSR      CHKALWDNY
012600060515     C*
012700060515     C* Al termine valorizzo i dati d output d procedura corrente
012800060520     C  N70              EXSR      VALOUT
012900060515     C*
013000060515     C                   ENDSR
013100060515     C*------------------------------------------------------------------------*
013200060515     C* AZZOUT - Routine di inizializzazione campi d ouput procedura corrente
013300060515     C*------------------------------------------------------------------------*
013400060515     C     AZZOUT        BEGSR
013500060515     C*
013600060515     C                   EVAL      OSIE3VER  = *zeros
013700060515     C                   EVAL      OSIE3VERD = *blanks
013800060515     C                   EVAL      OSIE3DDE  = *zeros
013900060515     C                   EVAL      OSIE3NAZN = *zeros
014000060515     C                   EVAL      OSIE3NAZ2 = *blanks
014100060515     C                   EVAL      OSIE3NAZ3 = *blanks
014200060515     C                   EVAL      OSIE3SRVM = *blanks
014300060515     C                   EVAL      OSIE3SRVD = *blanks
014400060515     C                   EVAL      OSIE3OSRT = *blanks
014500060515     C                   EVAL      OSIE3RDEP = *blanks
014600060515     C                   EVAL      OSIE3DDEP = *blanks
014700060515     C                   EVAL      OSIE3DSRT = *blanks
014800060515     C                   EVAL      OSIE3PTYG = *blanks
014900060515     C                   EVAL      OSIE3BID  = *zeros
015000060516     C                   EVAL      OSIE3BIDP = *blanks
015100060515     C                   EVAL      OSIE3ERR  = *blanks
015200060515     C                   EVAL      OSIE3ERRL = *blanks
015300060520     C                   EVAL      OSIE3ERRD = *blanks
015400060515     C*
015500060515     C                   ENDSR
015600060515     C*------------------------------------------------------------------------*
015700060515     C* CHKPAR - Routine di controllo correttezza parametri d input
015800060515     C*------------------------------------------------------------------------*
015900060515     C     CHKPAR        BEGSR
016000060520     C*
016100060520     C* Inizializzo variabile esplicitamento errori
016200060520     C                   MOVEL     *blanks       wErrore          20
016300060531     C*
016400060531     C* Innanzitutto imposto il CAP "utilizzato" = al CAP destinatario passato in input
016500060608     C                   EVAL      USIE3CAD = %trim(ISIE3CAD)
016600060608     C*
016700060608     C* Normalizzo il CAP destinatario compattando gli spazi eventualmente presenti
016800060608     C                   Z-ADD     1             wPos              1 0
016900060629     C                   DOW       wPos < %len(%trim(USIE3CAD))
017000060608     C                   IF        %subst(USIE3CAD:wPos:1) = *blanks
017100060608     C                   EVAL      USIE3CAD = %subst(USIE3CAD:1:wPos-1) +
017200060608     C                                        %subst(USIE3CAD:wPos+1)
017300060621     C                   EVAL      wPos = wPos - 1
017400060608     C                   ENDIF
017500060608     C                   ADD       1             wPos
017600060608     C                   ENDDO
017700060515     C*
017800060515     C* Verifica presenza campi obbligatori
017900060515     C*
018000060515     C* - data spedizione...
018100060515     C                   IF        ISIE3DSP = *zeros
018200060520     C                   EVAL      wErrore = '(Data spedizione)'
018300060515     C                   EVAL      OSIE3ERR  = '1'
018400060515     C                   ENDIF
018500060515     C*
018600060515     C* - cap destinatario
018700060515     C                   IF        ISIE3CAD = *blanks
018800060520     C                   EVAL      wErrore = '(CAP destinatario)'
018900060515     C                   EVAL      OSIE3ERR  = '1'
019000060515     C                   ENDIF
019100060515     C*
019200060515     C* - servizio DPD
019300060515     C                   IF        ISIE3SRV = *zeros
019400060520     C                   EVAL      wErrore = '(Servizio DPD)'
019500060515     C                   EVAL      OSIE3ERR  = '1'
019600060515     C                   ENDIF
019700060515     C*
019800060515     C* - linea di partenza
019900060515     C                   IF        ISIE3LNP = *zeros
020000060520     C                   EVAL      wErrore = '(Linea partenza)'
020100060515     C                   EVAL      OSIE3ERR  = '1'
020200060515     C                   ENDIF
020300060515     C*
020400060515     C* Verifica campi facoltativi
020500060520     C*
020600060520     C* - data riferimento...
020700060520     C                   IF        ISIE3DRI = *zeros
020800060606     C***                EVAL      ISIE3DRI = DATCOR
020900060606     C                   EVAL      ISIE3DRI = ISIE3DSP
021000060520     C                   ENDIF
021100060515     C*
021200060515     C* - ora spedizione...
021300060515     C                   IF        ISIE3HSP = *zeros
021400060515     C                   EVAL      ISIE3HSP = 120000
021500060515     C                   ENDIF
021600060515     C*
021700060515     C* - eccezioine x Bartolini...
021800060515     C                   IF        ISIE3EXC = *blanks
021900060515     C                   EVAL      ISIE3EXC = 'N'
022000060515     C                   ENDIF
022100060515     C*
022200060515     C* Se nn ok => imposto subito l'errore generale d procedura
022300060515     C                   IF        OSIE3ERR <> *blanks
022400060516     C                   EVAL      OSIE3ERRL = '92'
022500060520     C                   EVAL      OSIE3ERRD = 'Errore: parametri obbligatori '+
022600060520     C                             'non valorizzati.' + %trimr(wErrore)
022700060515     C                   ENDIF
022800060515     C*
022900060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
023000060515     C                   IF        OSIE3ERR <> *blanks
023100060520     C                   SETON                                        70
023200060515     C                   ENDIF
023300060515     C*
023400060515     C                   ENDSR
023500060515     C*------------------------------------------------------------------------*
023600060515     C* REPVER - Routine di reperimento versione cappario DPD GeoPost
023700060515     C*------------------------------------------------------------------------*
023800060515     C     REPVER        BEGSR
023900060515     C*
024000060515     C                   CLEAR                   TISIE2DS
024100060515     C                   EVAL      SIE2TLA = 'E'
024200060515     C                   EVAL      SIE2DRI = ISIE3DRI
024300060515     C                   CALL      'TISIE2R'
024400060515     C                   PARM                    TISIE2DS
024500060515     C*
024600060515     C* Se nn ok => imposto subito l'errore generale d procedura
024700060515     C                   IF        SIE2VER = *zeros
024800060515     C                   EVAL      OSIE3ERR  = '1'
024900060516     C                   EVAL      OSIE3ERRL = '90'
025000060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento versione'+
025100060515     C                             '. (DPCVE01L)'
025200060515     C                   ENDIF
025300060515     C*
025400060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
025500060515     C                   IF        OSIE3ERR <> *blanks
025600060520     C                   SETON                                        70
025700060515     C                   ENDIF
025800060515     C*
025900060515     C                   ENDSR
026000060515     C*------------------------------------------------------------------------*
026100060515     C* REPDEP - Routine di reperimento depot DPD di partenza
026200060515     C*------------------------------------------------------------------------*
026300060515     C     REPDEP        BEGSR
026400060515     C*
026500060515     C                   MOVEL     *blanks       wR_Depot          4
026600060515     C*
026700061110     C                   Z-ADD     1             jORG
026800061110     C     ISIE3LNP      LOOKUP    skORGFIL(jORG)                         55
026900061110     C                   IF        %found
027000061110     C                   EVAL      wR_Depot = skORGCDP(jORG)
027100060515     C                   ENDIF
027200060515     C*
027300060515     C* Se nn ok => imposto subito l'errore generale d procedura
027400060515     C                   IF        wR_Depot = *blanks
027500060515     C                   EVAL      OSIE3ERR  = '1'
027600060516     C                   EVAL      OSIE3ERRL = '91'
027700060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento depot ' +
027800060515     C                             ' partenza. (AZORG01L)'
027900060515     C                   ENDIF
028000060515     C*
028100060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
028200060515     C                   IF        OSIE3ERR <> *blanks
028300060520     C                   SETON                                        70
028400060515     C                   ENDIF
028500060515     C*
028600060515     C                   ENDSR
028700060515     C*------------------------------------------------------------------------*
028800060515     C* CHKDEP - Routine di verifica esistenza depot DPD partenza su file cappario DPD
028900060515     C*------------------------------------------------------------------------*
029000061110 xxx C     CHKDEP        BEGSR
029100060515     C*
029200060606     C                   CLEAR                   DPCDP000
029300060607     C                   MOVEL     *blanks       wIATALikeCode     3
029400061110     C*
029500061110     C                   Z-ADD     1             jCDP
029600061110     C     wCurrentDepot LOOKUP    skCDPCDP(jCDP)                         55
029700061110     C                   IF        %found
029800061110     C                   EVAL      wIATALikeCode = skORGCDP(jORG)
029900060515     C                   ELSE
030000060515     C                   EVAL      OSIE3ERR  = '1'
030100060516     C                   EVAL      OSIE3ERRL = '94'
030200060627     C  N60              EVAL      OSIE3ERRD = 'Errore: depot partenza ' +
030300061110     C                             'errato. (DPCDP01L)'
030400060627     C   60              EVAL      OSIE3ERRD = 'Errore: depot arrivo ' +
030500061110     C                             'errato. (DPCDP01L)'
030600060515     C                   ENDIF
030700060515     C*
030800060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
030900060515     C                   IF        OSIE3ERR <> *blanks
031000060520     C                   SETON                                        70
031100060515     C                   ENDIF
031200060515     C*
031300060515     C                   ENDSR
031400060515     C*------------------------------------------------------------------------*
031500060515     C* REPNAZ - Routine di reperimento dati nazione destinatario
031600060515     C*------------------------------------------------------------------------*
031700060515     C     REPNAZ        BEGSR
031800060515     C*
031900060515     C                   Z-ADD     *zeros        wNazIsoN          3 0
032000060515     C                   MOVEL     *blanks       wNazIso2          2
032100060515     C                   MOVEL     *blanks       wNazIso3          3
032200060519     C                   MOVEL     *blanks       wNazPTC           1 0
032300060515     C*
032400060515     C                   CLEAR                   DS15
032500060515     C                   EVAL      tblKUT = 1
032600060515     C                   EVAL      tblCOD = '15'
032700060515     C                   EVAL      tblKEY = ISIE3NZD
032800060515     C     KEYtabel_C    CHAIN     TABEL00F
032900060515     C                   IF        %found(TABEL00F)
033000060515     C                   IF        tblFLG = *blanks
033100060515     C                   EVAL      DS15 = tblUNI
033200060515     C                   EVAL      cccVER  = SIE2VER
033300060515     C                   EVAL      cccISOK = §15CIE
033400060515     C     KEYccc01_C    CHAIN     DPCCC01L
033500060606     C                   IF        %found(DPCCC01L)
033600060515     C                   IF        cccATB = *blanks
033700060515     C                   EVAL      wNazIsoN = cccISOK
033800060515     C                   EVAL      wNazIso2 = cccISO2
033900060515     C                   EVAL      wNazIso3 = cccISO3
034000060519     C                   EVAL      wNazPTC  = cccPTC
034100060515     C                   ENDIF
034200060515     C                   ENDIF
034300060515     C                   ENDIF
034400060515     C                   ENDIF
034500060515     C*
034600060515     C* Se nn ok => imposto subito l'errore generale d procedura
034700060515     C                   IF        wNazIsoN = *zeros  OR
034800060515     C                             wNazIso2 = *blanks OR
034900060515     C                             wNazIso3 = *blanks
035000060515     C                   EVAL      OSIE3ERR  = '1'
035100060516     C                   EVAL      OSIE3ERRL = '93'
035200060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento nazione.'+
035300060515     C                             ' (DPCCC01L)'
035400060515     C                   ENDIF
035500060515     C*
035600060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
035700060515     C                   IF        OSIE3ERR <> *blanks
035800060520     C                   SETON                                        70
035900060515     C                   ENDIF
036000060515     C*
036100060515     C                   ENDSR
036200060519     C*------------------------------------------------------------------------*
036300060519     C* CHKPTCSYS - Verifica se nazione destino gestisce il cappario (vedi Irlanda)
036400060519     C*------------------------------------------------------------------------*
036500060519     C     CHKPTCSYS     BEGSR
036600060519     C*
036700060519     C* Se nazione nn gestisce il cappario => forzo a *blanks il campo CAP destinatario
036800060519     C* ricevuto in input
036900060519     C                   IF        wNazPTC = 1
037000060531     C                   EVAL      USIE3CAD = *blanks
037100060519     C                   ENDIF
037200060519     C*
037300060519     C* Eseguo forzatura => in caso d nazione destinatario Irlanda forzo il CAP
037400060519     C* destinatario a '1' fisso
037500060519     C                   IF        ISIE3NZD = 'IRL'
037600060531     C                   EVAL      USIE3CAD = '1'
037700060519     C                   ENDIF
037800060519     C*
037900060519     C                   ENDSR
038000060515     C*------------------------------------------------------------------------*
038100060515     C* REPSRV - Routine di verifica e reperimento dati relativi al servizio DPD
038200060515     C*------------------------------------------------------------------------*
038300060515     C     REPSRV        BEGSR
038400060515     C*
038500060515     C                   MOVEL     *blanks       wSrvMark          1
038600060515     C                   MOVEL     *blanks       wSrvDes          16
038700060515     C*
038800060515     C                   EVAL      cscVER  = SIE2VER
038900060515     C                   EVAL      cscSRVC = ISIE3SRV
039000060515     C     KEYcsc01_C    CHAIN     DPCSC01L
039100060515     C                   IF        %found(DPCSC01L)
039200060515     C                   IF        cscATB = *blanks
039300060515     C                   EVAL      wSrvMark = cscSRVM
039400060515     C                   EVAL      wSrvDes  = CSCSRVD
039500060515     C                   ENDIF
039600060515     C                   ENDIF
039700060515     C*
039800060515     C* Se nn ok => imposto subito l'errore generale d procedura
039900060520     C                   IF        wSrvMark = *blanks AND
040000060515     C                             wSrvDes  = *blanks
040100060515     C                   EVAL      OSIE3ERR  = '1'
040200060516     C                   EVAL      OSIE3ERRL = '95'
040300060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento servizio'+
040400060515     C                             '. (DPCSC01L)'
040500060515     C                   ENDIF
040600060515     C*
040700060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
040800060515     C                   IF        OSIE3ERR <> *blanks
040900060520     C                   SETON                                        70
041000060515     C                   ENDIF
041100060515     C*
041200060515     C                   ENDSR
041300060519     C*------------------------------------------------------------------------*
041400060519     C* REPCTY - Routine di verifica esistenza ecezione x area/località DPD
041500060519     C*------------------------------------------------------------------------*
041600060530     C     REPCTY        BEGSR
041700060519     C*
041800060519     C* Se ricevuti in ingresso AREA e/o LOCALITA "provo" d gestire
041900060519     C                   IF        ISIE3AREA <> *blanks OR
042000060519     C                             ISIE3CTY  <> *blanks
042100060519     C*
042200060519     C                   EVAL      cloVER  = SIE2VER
042300060520     C                   EVAL      cloISO2 = wNazIso2
042400060519     C                   EVAL      cloAREA = ISIE3AREA
042500060519     C                   EVAL      cloCTY  = ISIE3CTY
042600060519     C     KEYclo01_C    CHAIN     DPCLO01L
042700060519     C                   IF        %found(DPCLO01L)
042800060519     C                   IF        cloATB = *blanks
042900060519     C*
043000060519     C* Se trovata eccezione x AREA/LOCALITA forzo il valore dell'ecezione sul CAP
043100060519     C* destinatario ricevuto in input
043200060531     C                   EVAL      USIE3CAD = cloPTC
043300060519     C                   ENDIF
043400060519     C                   ENDIF
043500060519     C*
043600060519     C                   ENDIF
043700060519     C*
043800060519     C                   ENDSR
043900060516     C*------------------------------------------------------------------------*
044000060516     C* REPROUTE - Routine di reperimento dati relativi all'instradamento DPD
044100060516     C*------------------------------------------------------------------------*
044200060516     C     REPROUTE      BEGSR
044300060516     C*
044400060530     C                   CLEAR                   ds_dpcro
044500060516     C                   MOVEL     *blanks       wO_Sort           4
044600060516     C                   MOVEL     *blanks       wD_Depot          4
044700060516     C                   MOVEL     *blanks       wD_Sort           4
044800060516     C                   MOVEL     *blanks       wPty_Group        1
044900060516     C                   Z-ADD     *zeros        wBarcodeID        3 0
045000060516     C                   MOVEL     *blanks       wBarcodeID_Prt    1
045100060516     C*
045200060516     C/EXEC SQL
045300060622     C+ declare C1 cursor for
045400061109     C+ select croOSRT, croDDEP, croDSRT, croPTYG, croBID,
045500061109     C+ croBIDP from dpcro03l
045600060516     C+ where croVER = :SIE2VER and croISO2 = :wNazIso2 and
045700060516     C+ (croSRVC = :ISIE3SRV or croSRVC = 0)  and
045800060608     C+ (croROUP = :wR_Depot or croROUP = '0000') and
045900060531     C+ :USIE3CAD between croPTCB and croPTCE and
046000060530     C+ croATB = ' '
046100060516     C+ order by croSRVC desc, croROUP desc
046200060622     C+ for read only
046300060516     C/END-EXEC
046400060516     C
046500060516     C/EXEC SQL
046600060516     C+ open C1
046700060516     C/END-EXEC
046800060516     C
046900060516     C/EXEC SQL
047000060622     C+ Fetch next from C1 into :ds_dpcro
047100060516     C/END-EXEC
047200060516     C*
047300060516     C* Se reperito qualcosa => analizzo
047400060516     C* ....altrimenti trattasi d errore di instradamento
047500060516     C                   if        sqlcod <> *zeros
047600060516     C                   EVAL      OSIE3ERR  = '1'
047700060516     C                   EVAL      OSIE3ERRL = '96'
047800060520     C                   EVAL      OSIE3ERRD = 'Errore in calcolo ' +
047900060516     C                             'instradamento. (DPCRO01L)'
048000060516     C*
048100060516     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
048200060516     C                   IF        OSIE3ERR <> *blanks
048300060520     C                   SETON                                        70
048400060516     C                   ENDIF
048500060516     C*
048600060516     C                   else
048700060516     C*
048800060516     C* Valorizzo i dati dell'instradamento così reperito
048900061109     C                   eval      wO_Sort        = d_croOSRT
049000061109     C                   eval      wD_Depot       = d_croDDEP
049100061109     C                   eval      wD_Sort        = d_croDSRT
049200061109     C                   eval      wPty_Group     = d_croPTYG
049300061109     C                   eval      wBarcodeID     = d_croBID
049400061109     C                   eval      wBarcodeID_Prt = d_croBIDP
049500060516     C*
049600060516     C                   endif
049700060516     C*
049800060516     C/EXEC SQL
049900060516     C+ close C1
050000060516     C/END-EXEC
050100060516     C*
050200060516     C                   ENDSR
050300060530     C*------------------------------------------------------------------------*
050400060530     C* CHKALWDNY - Routine di verifica instradamento calcolato rispetto a ALLOW e DENY
050500060530     C*------------------------------------------------------------------------*
050600060530     C     CHKALWDNY     BEGSR
050700060530     C*
050800060530     C                   CLEAR                   ds_dpcal
050900060530     C                   CLEAR                   ds_dpcdy
051000060530     C*
051100060530     C/EXEC SQL
051200060629     C+ declare C2 cursor for
051300061109     C+ select calID from dpcal01l
051400060629     C+ where calVER = :SIE2VER and
051500060629     C+ calRULF = :wR_Depot and
051600060629     C+ calSRVC = :ISIE3SRV and
051700060629     C+ ((calISO2 = :wNazIso2 and :USIE3CAD between calPTCB and calPTCE) or
051800060629     C+ calRULT = :wD_Depot) and
051900060629     C+ calATB = ' '
052000061110     C+ order by calVER, calRULF desc, calSRVC desc, calRULT desc, calISO2 desc
052100060622     C+ for read only
052200060530     C/END-EXEC
052300060530     C
052400060530     C/EXEC SQL
052500060530     C+ open C2
052600060530     C/END-EXEC
052700060530     C
052800060530     C/EXEC SQL
052900061109     C+ Fetch next from C2 into :d_calID
053000060530     C/END-EXEC
053100060530     C*
053200060530     C* Se reperito qualcosa => analizzo
053300060530     C* ....altrimenti trattasi d errore di permesso instradamento
053400060530     C                   if        sqlcod <> *zeros
053500060530     C                   EVAL      OSIE3ERR  = '1'
053600060530     C                   EVAL      OSIE3ERRL = '97'
053700060530     C                   EVAL      OSIE3ERRD = 'Instradamento non ' +
053800060530     C                             'consentito. (DPCAL01L)'
053900060530     C*
054000060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
054100060530     C                   IF        OSIE3ERR <> *blanks
054200060530     C                   SETON                                        70
054300060530     C                   ENDIF
054400060530     C*
054500060530     C                   else
054600060530     C*
054700060530     C/EXEC SQL
054800060629     C+ declare C3 cursor for
054900061109     C+ select cdyID from dpcdy01l
055000061109     C+ where cdyVER = :SIE2VER and cdyID = :d_calID and
055100060629     C+ cdyRULF = :wR_Depot and
055200060629     C+ cdySRVC = :ISIE3SRV and
055300060629     C+ ((cdyISO2 = :wNazIso2 and :USIE3CAD between cdyPTCB and cdyPTCE) or
055400060629     C+ cdyRULT = :wD_Depot) and
055500060629     C+ cdyATB = ' '
055600061109     C+ order by cdyVER, cdyID, cdyRULF desc, cdySRVC desc, cdyRULT desc,
055700061109     C+ cdyISO2 desc
055800060629     C+ for read only
055900060530     C/END-EXEC
056000060530     C
056100060530     C/EXEC SQL
056200060530     C+ open C3
056300060530     C/END-EXEC
056400060530     C
056500060530     C/EXEC SQL
056600061109     C+ Fetch next from C3 into :d_cdyID
056700060530     C/END-EXEC
056800060530     C*
056900060530     C* Se reperito qualcosa => trattasi d errore d divieto
057000060530     C* ....altrimenti trattasi tutto ok
057100060530     C                   if        sqlcod = *zeros
057200060530     C                   EVAL      OSIE3ERR  = '1'
057300060530     C                   EVAL      OSIE3ERRL = '98'
057400060530     C                   EVAL      OSIE3ERRD = 'Instradamento ' +
057500060530     C                             'negato. (DPCDY01L)'
057600060530     C*
057700060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
057800060530     C                   IF        OSIE3ERR <> *blanks
057900060530     C                   SETON                                        70
058000060530     C                   ENDIF
058100060530     C*
058200060530     C                   endif
058300060530     C*
058400060530     C/EXEC SQL
058500060530     C+ close C3
058600060530     C/END-EXEC
058700060530     C*
058800060530     C                   endif
058900060530     C*
059000060530     C/EXEC SQL
059100060530     C+ close C2
059200060530     C/END-EXEC
059300060530     C*
059400060530     C                   ENDSR
059500060515     C*------------------------------------------------------------------------*
059600060515     C* VALOUT - Routine di valorizzazione dati ouput procedura corrente
059700060515     C*------------------------------------------------------------------------*
059800060515     C     VALOUT        BEGSR
059900060515     C*
060000060515     C                   EVAL      OSIE3VER  = SIE2VER
060100060515     C                   EVAL      OSIE3VERD = SIE2VERD
060200060515     C                   EVAL      OSIE3DDE  = SIE2DDE
060300060606     C*
060400060606     C* Effettuo forzatura => in stampa etichetta deve essere indicata la nazione del depot
060500060606     C* d arrivo e nn la nazione d destino della spedizione (strano ma vero !!!)
060600060606     C                   MOVEL(P)  wD_Depot      wCurrentDepot     4
060700060627     C                   SETON                                        60
060800060606     C                   EXSR      CHKDEP
060900060607     C                   EVAL      OSIE3IATAD = wIATALikeCode
061000060606     C                   IF        cdpISO2 <> *blanks
061100060606     C                   EVAL      cccVER  = SIE2VER
061200060606     C                   EVAL      cccISO2 = cdpISO2
061300060606     C     KEYccc02_C    CHAIN     DPCCC02L
061400060606     C                   IF        %found(DPCCC02L)
061500060606     C                   IF        cccATB = *blanks
061600060607     C                   EVAL      OSIE3NAZND = cccISOK
061700060607     C                   EVAL      OSIE3NAZ2D = cccISO2
061800060607     C                   EVAL      OSIE3NAZ3D = cccISO3
061900060606     C                   ENDIF
062000060606     C                   ENDIF
062100060606     C                   ENDIF
062200060606     C*
062300060515     C                   EVAL      OSIE3NAZN = wNazIsoN
062400060516     C                   EVAL      OSIE3NAZ2 = wNazIso2
062500060516     C                   EVAL      OSIE3NAZ3 = wNazIso3
062600060516     C                   EVAL      OSIE3SRVM = wSrvMark
062700060516     C                   EVAL      OSIE3SRVD = wSrvDes
062800060516     C                   EVAL      OSIE3OSRT = wO_Sort
062900060516     C                   EVAL      OSIE3RDEP = wR_Depot
063000060516     C                   EVAL      OSIE3DDEP = wD_Depot
063100060516     C                   EVAL      OSIE3DSRT = wD_Sort
063200060516     C                   EVAL      OSIE3PTYG = wPty_Group
063300060516     C                   EVAL      OSIE3BID  = wBarcodeID
063400060516     C                   EVAL      OSIE3BIDP = wBarcodeID_Prt
063500061109     C                   EVAL      OSIE3ALWID= d_calID
063600060515     C*
063700060515     C                   ENDSR
063800061109     C*------------------------------------------------------------------------*
063900061110     C* CARTABANA - Routine di caricamento dati atabellati e anagrafici
064000061109     C*------------------------------------------------------------------------*
064100061110     C     CARTABANA     BEGSR
064200061110     C*
064300061110     C* ************
064400061110     C* Organigramma
064500061110     C* ************
064600061110     C                   clear                   skORGFIL
064700061110     C                   clear                   skORGCDP
064800061110     C                   z-add     *zeros        jORG
064900061110     C*
065000061110     C     *loval        setll     AZORG01L
065100061110     C                   read      AZORG01L
065200061110     C                   dow       not %eof(AZORG01L)
065300061110     C                   eval      OG143 = ORGDE3
065400061110     C                   add       1             jORG
065500061110     C                   eval      skORGFIL(jORG) = orgFIL
065600061110     C                   eval      skORGCDP(jORG) = §ogdp1
065700061110     C                   read      AZORG01L
065800061110     C                   enddo
065900061110     C*
066000061110     C* ************
066100061110     C* Anagrafico Depot DPD GeoPost
066200061110     C* ************
066300061110     C                   clear                   skCDPCDP
066400061110     C                   clear                   skCDPIATA
066500061110     C                   z-add     *zeros        jCDP
066600061110     C*
066700061110     C     SIE2VER       setll     DPCDP01L
066800061110     C                   read      DPCDP01L
066900061110     C                   dow       not %eof(DPCDP01L)
067000061110     C                   if        cdpATB = *blanks
067100061110     C                   add       1             jCDP
067200061110     C                   eval      skCDPCDP(jCDP)  = cdpDPC
067300061110     C                   eval      skCDPIATA(jCDP) = cdpIATA
067400061110     C                   endif
067500061110     C                   read      DPCDP01L
067600061110     C                   enddo
067700061110     C*
067800061109     C                   ENDSR
067900060515     C*--------------------------------------------------------------------------------------------*
068000060515     C* *inzsr - operazioni iniziali
068100060515     C*--------------------------------------------------------------------------------------------*
068200000000     C     *inzsr        BEGSR
068300060515     C*
068400060515     C* Ricevimento parametri
068500060515     C     *ENTRY        PLIST
068600060515     C                   PARM                    TISIE3DS
068700061110     C*
068800061110     C* Reperisco immediatamente la versione da considerare
068900061110     C                   EXSR      REPVER
069000061110     C*
069100061110     C* Caricamento dati tabellari e anagrafici
069200061110     C                   EXSR      CARTABANA
069300060515     C*
069400060515     C* Definizione chiavi di lettura
069500060515     C*
069600060515     C* TABEL00F - Completa
069700060515     C     KEYtabel_C    KLIST
069800060515     C                   KFLD                    tblKUT
069900060515     C                   KFLD                    tblCOD
070000060515     C                   KFLD                    tblKEY
070100060515     C*
070200060515     C* DPCCC01L - Completa
070300060515     C     KEYccc01_C    KLIST
070400060515     C                   KFLD                    cccVER
070500060515     C                   KFLD                    cccISOK
070600060606     C*
070700060606     C* DPCCC02L - Completa
070800060606     C     KEYccc02_C    KLIST
070900060606     C                   KFLD                    cccVER
071000060606     C                   KFLD                    cccISO2
071100060515     C*
071200060515     C* DPCSC01L - Completa
071300060515     C     KEYcsc01_C    KLIST
071400060515     C                   KFLD                    cscVER
071500060515     C                   KFLD                    cscSRVC
071600060515     C*
071700060515     C* DPCDP01L - Completa
071800061110 xxx C     KEYcdp01_C    KLIST
071900060515     C                   KFLD                    cdpVER
072000060515     C                   KFLD                    cdpDPC
072100060519     C*
072200060519     C* DPCLO01L - Completa
072300060519     C     KEYclo01_C    KLIST
072400060519     C                   KFLD                    cloVER
072500060520     C                   KFLD                    cloISO2
072600060519     C                   KFLD                    cloAREA
072700060519     C                   KFLD                    cloCTY
072800060510     C*
072900060510     C* CALCOLA LA DATA CORRENTE
073000060510     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
073100060510     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
073200060510     C                   Z-ADD     WNUM8         G08DAT
073300060510     C                   Z-ADD     *ZEROS        G08INV
073400060510     C                   MOVEL     '0'           G08ERR
073500060510     C                   CALL      'XSRDA8'
073600060510     C                   PARM                    WLBDA8
073700060510     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
073800060622     C*
073900060622     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
074000060622     C
074100060622     C/EXEC SQL
074200060622     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
074300060622     C/END-EXEC
074400060515     C*
074500060515     C                   ENDSR
