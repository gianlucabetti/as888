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
033400060515     C                   EVAL      cccVER  = SIE2VER
033500060515     C                   EVAL      cccISOK = §15CIE
033600060515     C     KEYccc01_C    CHAIN     DPCCC01L
033700060606     C                   IF        %found(DPCCC01L)
033800060515     C                   IF        cccATB = *blanks
033900060515     C                   EVAL      wNazIsoN = cccISOK
034000060515     C                   EVAL      wNazIso2 = cccISO2
034100060515     C                   EVAL      wNazIso3 = cccISO3
034200060519     C                   EVAL      wNazPTC  = cccPTC
034300060515     C                   ENDIF
034400060515     C                   ENDIF
034500060515     C                   ENDIF
034600060515     C                   ENDIF
034700060515     C*
034800060515     C* Se nn ok => imposto subito l'errore generale d procedura
034900060515     C                   IF        wNazIsoN = *zeros  OR
035000060515     C                             wNazIso2 = *blanks OR
035100060515     C                             wNazIso3 = *blanks
035200060515     C                   EVAL      OSIE3ERR  = '1'
035300060516     C                   EVAL      OSIE3ERRL = '93'
035400060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento nazione.'+
035500060515     C                             ' (DPCCC01L)'
035600060515     C                   ENDIF
035700060515     C*
035800060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
035900060515     C                   IF        OSIE3ERR <> *blanks
036000060520     C                   SETON                                        70
036100060515     C                   ENDIF
036200060515     C*
036300060515     C                   ENDSR
036400060519     C*------------------------------------------------------------------------*
036500060519     C* CHKPTCSYS - Verifica se nazione destino gestisce il cappario (vedi Irlanda)
036600060519     C*------------------------------------------------------------------------*
036700060519     C     CHKPTCSYS     BEGSR
036800060519     C*
036900060519     C* Se nazione nn gestisce il cappario => forzo a *blanks il campo CAP destinatario
037000060519     C* ricevuto in input
037100060519     C                   IF        wNazPTC = 1
037200060531     C                   EVAL      USIE3CAD = *blanks
037300060519     C                   ENDIF
037400060519     C*
037500060519     C* Eseguo forzatura => in caso d nazione destinatario Irlanda forzo il CAP
037600060519     C* destinatario a '1' fisso
037700060519     C                   IF        ISIE3NZD = 'IRL'
037800060531     C                   EVAL      USIE3CAD = '1'
037900060519     C                   ENDIF
038000060519     C*
038100060519     C                   ENDSR
038200060515     C*------------------------------------------------------------------------*
038300060515     C* REPSRV - Routine di verifica e reperimento dati relativi al servizio DPD
038400060515     C*------------------------------------------------------------------------*
038500060515     C     REPSRV        BEGSR
038600060515     C*
038700060515     C                   MOVEL     *blanks       wSrvMark          1
038800060515     C                   MOVEL     *blanks       wSrvDes          16
038900060515     C*
039000060515     C                   EVAL      cscVER  = SIE2VER
039100060515     C                   EVAL      cscSRVC = ISIE3SRV
039200060515     C     KEYcsc01_C    CHAIN     DPCSC01L
039300060515     C                   IF        %found(DPCSC01L)
039400060515     C                   IF        cscATB = *blanks
039500060515     C                   EVAL      wSrvMark = cscSRVM
039600060515     C                   EVAL      wSrvDes  = CSCSRVD
039700060515     C                   ENDIF
039800060515     C                   ENDIF
039900060515     C*
040000060515     C* Se nn ok => imposto subito l'errore generale d procedura
040100060520     C                   IF        wSrvMark = *blanks AND
040200060515     C                             wSrvDes  = *blanks
040300060515     C                   EVAL      OSIE3ERR  = '1'
040400060516     C                   EVAL      OSIE3ERRL = '95'
040500060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento servizio'+
040600060515     C                             '. (DPCSC01L)'
040700060515     C                   ENDIF
040800060515     C*
040900060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
041000060515     C                   IF        OSIE3ERR <> *blanks
041100060520     C                   SETON                                        70
041200060515     C                   ENDIF
041300060515     C*
041400060515     C                   ENDSR
041500060519     C*------------------------------------------------------------------------*
041600060519     C* REPCTY - Routine di verifica esistenza ecezione x area/località DPD
041700060519     C*------------------------------------------------------------------------*
041800060530     C     REPCTY        BEGSR
041900060519     C*
042000060519     C* Se ricevuti in ingresso AREA e/o LOCALITA "provo" d gestire
042100060519     C                   IF        ISIE3AREA <> *blanks OR
042200060519     C                             ISIE3CTY  <> *blanks
042300060519     C*
042400060519     C                   EVAL      cloVER  = SIE2VER
042500060520     C                   EVAL      cloISO2 = wNazIso2
042600060519     C                   EVAL      cloAREA = ISIE3AREA
042700060519     C                   EVAL      cloCTY  = ISIE3CTY
042800060519     C     KEYclo01_C    CHAIN     DPCLO01L
042900060519     C                   IF        %found(DPCLO01L)
043000060519     C                   IF        cloATB = *blanks
043100060519     C*
043200060519     C* Se trovata eccezione x AREA/LOCALITA forzo il valore dell'ecezione sul CAP
043300060519     C* destinatario ricevuto in input
043400060531     C                   EVAL      USIE3CAD = cloPTC
043500060519     C                   ENDIF
043600060519     C                   ENDIF
043700060519     C*
043800060519     C                   ENDIF
043900060519     C*
044000060519     C                   ENDSR
044100060516     C*------------------------------------------------------------------------*
044200060516     C* REPROUTE - Routine di reperimento dati relativi all'instradamento DPD
044300060516     C*------------------------------------------------------------------------*
044400060516     C     REPROUTE      BEGSR
044500060516     C*
044600060530     C                   CLEAR                   ds_dpcro
044700060516     C                   MOVEL     *blanks       wO_Sort           4
044800060516     C                   MOVEL     *blanks       wD_Depot          4
044900060516     C                   MOVEL     *blanks       wD_Sort           4
045000060516     C                   MOVEL     *blanks       wPty_Group        1
045100060516     C                   Z-ADD     *zeros        wBarcodeID        3 0
045200060516     C                   MOVEL     *blanks       wBarcodeID_Prt    1
045300150518     C                   MOVEL     *blanks       wZIPCODE_Da       9
045400150518     C                   MOVEL     *blanks       wZIPCODE_A        9
045500060516     C*
045600060516     C/EXEC SQL
045700060622     C+ declare C1 cursor for
045800061109     C+ select croOSRT, croDDEP, croDSRT, croPTYG, croBID,
045900150518     C+ croBIDP, croPTCB, croPTCE from dpcro03l
046000060516     C+ where croVER = :SIE2VER and croISO2 = :wNazIso2 and
046100060516     C+ (croSRVC = :ISIE3SRV or croSRVC = 0)  and
046200060608     C+ (croROUP = :wR_Depot or croROUP = '0000') and
046300060531     C+ :USIE3CAD between croPTCB and croPTCE and
046400060530     C+ croATB = ' '
046500070523     C+ order by croSRVC desc, croROUP desc, croPTYG desc
046600060622     C+ for read only
046700060516     C/END-EXEC
046800060516     C
046900060516     C/EXEC SQL
047000060516     C+ open C1
047100060516     C/END-EXEC
047200060516     C
047300060516     C/EXEC SQL
047400060622     C+ Fetch next from C1 into :ds_dpcro
047500060516     C/END-EXEC
047600060516     C*
047700060516     C* Se reperito qualcosa => analizzo
047800060516     C* ....altrimenti trattasi d errore di instradamento
047900060516     C                   if        sqlcod <> *zeros
048000060516     C                   EVAL      OSIE3ERR  = '1'
048100060516     C                   EVAL      OSIE3ERRL = '96'
048200060520     C                   EVAL      OSIE3ERRD = 'Errore in calcolo ' +
048300060516     C                             'instradamento. (DPCRO01L)'
048400060516     C*
048500060516     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
048600060516     C                   IF        OSIE3ERR <> *blanks
048700060520     C                   SETON                                        70
048800060516     C                   ENDIF
048900060516     C*
049000060516     C                   else
049100060516     C*
049200060516     C* Valorizzo i dati dell'instradamento così reperito
049300061109     C                   eval      wO_Sort        = d_croOSRT
049400061109     C                   eval      wD_Depot       = d_croDDEP
049500061109     C                   eval      wD_Sort        = d_croDSRT
049600061109     C                   eval      wPty_Group     = d_croPTYG
049700061109     C                   eval      wBarcodeID     = d_croBID
049800061109     C                   eval      wBarcodeID_Prt = d_croBIDP
049900150518     C                   eval      wZIPCODE_Da    = d_croPTCB
050000150518     C                   eval      wZIPCODE_A     = d_croPTCE
050100060516     C*
050200060516     C                   endif
050300060516     C*
050400060516     C/EXEC SQL
050500060516     C+ close C1
050600060516     C/END-EXEC
050700060516     C*
050800060516     C                   ENDSR
050900060530     C*------------------------------------------------------------------------*
051000060530     C* CHKALWDNY - Routine di verifica instradamento calcolato rispetto a ALLOW e DENY
051100060530     C*------------------------------------------------------------------------*
051200060530     C     CHKALWDNY     BEGSR
051300060530     C*
051400060530     C                   CLEAR                   ds_dpcal
051500060530     C                   CLEAR                   ds_dpcdy
051600060530     C*
051700060530     C/EXEC SQL
051800060629     C+ declare C2 cursor for
051900061109     C+ select calID from dpcal01l
052000060629     C+ where calVER = :SIE2VER and
052100060629     C+ calRULF = :wR_Depot and
052200060629     C+ calSRVC = :ISIE3SRV and
052300060629     C+ ((calISO2 = :wNazIso2 and :USIE3CAD between calPTCB and calPTCE) or
052400060629     C+ calRULT = :wD_Depot) and
052500060629     C+ calATB = ' '
052600061110     C+ order by calVER, calRULF desc, calSRVC desc, calRULT desc, calISO2 desc
052700060622     C+ for read only
052800060530     C/END-EXEC
052900060530     C
053000060530     C/EXEC SQL
053100060530     C+ open C2
053200060530     C/END-EXEC
053300060530     C
053400060530     C/EXEC SQL
053500061109     C+ Fetch next from C2 into :d_calID
053600060530     C/END-EXEC
053700060530     C*
053800060530     C* Se reperito qualcosa => analizzo
053900060530     C* ....altrimenti trattasi d errore di permesso instradamento
054000060530     C                   if        sqlcod <> *zeros
054100060530     C                   EVAL      OSIE3ERR  = '1'
054200060530     C                   EVAL      OSIE3ERRL = '97'
054300060530     C                   EVAL      OSIE3ERRD = 'Instradamento non ' +
054400060530     C                             'consentito. (DPCAL01L)'
054500060530     C*
054600060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
054700060530     C                   IF        OSIE3ERR <> *blanks
054800060530     C                   SETON                                        70
054900060530     C                   ENDIF
055000060530     C*
055100060530     C                   else
055200060530     C*
055300060530     C/EXEC SQL
055400060629     C+ declare C3 cursor for
055500061109     C+ select cdyID from dpcdy01l
055600061109     C+ where cdyVER = :SIE2VER and cdyID = :d_calID and
055700060629     C+ cdyRULF = :wR_Depot and
055800060629     C+ cdySRVC = :ISIE3SRV and
055900060629     C+ ((cdyISO2 = :wNazIso2 and :USIE3CAD between cdyPTCB and cdyPTCE) or
056000060629     C+ cdyRULT = :wD_Depot) and
056100060629     C+ cdyATB = ' '
056200061109     C+ order by cdyVER, cdyID, cdyRULF desc, cdySRVC desc, cdyRULT desc,
056300061109     C+ cdyISO2 desc
056400060629     C+ for read only
056500060530     C/END-EXEC
056600060530     C
056700060530     C/EXEC SQL
056800060530     C+ open C3
056900060530     C/END-EXEC
057000060530     C
057100060530     C/EXEC SQL
057200061109     C+ Fetch next from C3 into :d_cdyID
057300060530     C/END-EXEC
057400060530     C*
057500060530     C* Se reperito qualcosa => trattasi d errore d divieto
057600060530     C* ....altrimenti trattasi tutto ok
057700060530     C                   if        sqlcod = *zeros
057800060530     C                   EVAL      OSIE3ERR  = '1'
057900060530     C                   EVAL      OSIE3ERRL = '98'
058000060530     C                   EVAL      OSIE3ERRD = 'Instradamento ' +
058100060530     C                             'negato. (DPCDY01L)'
058200060530     C*
058300060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
058400060530     C                   IF        OSIE3ERR <> *blanks
058500060530     C                   SETON                                        70
058600060530     C                   ENDIF
058700060530     C*
058800060530     C                   endif
058900060530     C*
059000060530     C/EXEC SQL
059100060530     C+ close C3
059200060530     C/END-EXEC
059300060530     C*
059400060530     C                   endif
059500060530     C*
059600060530     C/EXEC SQL
059700060530     C+ close C2
059800060530     C/END-EXEC
059900060530     C*
060000150518     C* Controllo per forzatura relativa alla sola nazione 'GB' =>
060100150518     C* ... consentiti unicamente CAP in bolla della medesima lunghezza dei CAP
060200150518     C* ... su CAPPARIO DPD
060300150518     C                   IF        wNazIso2 = 'GB'
060400150518     C                   IF        %len(%trim(USIE3CAD)) <>
060500150518     C                             %len(%trim(wZIPCODE_A))
060600150518     C*
060700150518     C                   EVAL      OSIE3ERR  = '1'
060800150518     C                   EVAL      OSIE3ERRL = '99'
060900150518     C                   EVAL      OSIE3ERRD = 'Errore - CAP errato. '
061000150518     C*
061100150518     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
061200150518     C                   IF        OSIE3ERR <> *blanks
061300150518     C                   SETON                                        70
061400150518     C                   ENDIF
061500150518     C*
061600150518     C                   ENDIF
061700150518     C                   ENDIF
061800150518     C*
061900060530     C                   ENDSR
062000060515     C*------------------------------------------------------------------------*
062100060515     C* VALOUT - Routine di valorizzazione dati ouput procedura corrente
062200060515     C*------------------------------------------------------------------------*
062300060515     C     VALOUT        BEGSR
062400060515     C*
062500060515     C                   EVAL      OSIE3VER  = SIE2VER
062600060515     C                   EVAL      OSIE3VERD = SIE2VERD
062700060515     C                   EVAL      OSIE3DDE  = SIE2DDE
062800060606     C*
062900060606     C* Effettuo forzatura => in stampa etichetta deve essere indicata la nazione del depot
063000060606     C* d arrivo e nn la nazione d destino della spedizione (strano ma vero !!!)
063100060606     C                   MOVEL(P)  wD_Depot      wCurrentDepot     4
063200060627     C                   SETON                                        60
063300060606     C                   EXSR      CHKDEP
063400060607     C                   EVAL      OSIE3IATAD = wIATALikeCode
063500060606     C                   IF        cdpISO2 <> *blanks
063600060606     C                   EVAL      cccVER  = SIE2VER
063700060606     C                   EVAL      cccISO2 = cdpISO2
063800060606     C     KEYccc02_C    CHAIN     DPCCC02L
063900060606     C                   IF        %found(DPCCC02L)
064000060606     C                   IF        cccATB = *blanks
064100060607     C                   EVAL      OSIE3NAZND = cccISOK
064200060607     C                   EVAL      OSIE3NAZ2D = cccISO2
064300060607     C                   EVAL      OSIE3NAZ3D = cccISO3
064400060606     C                   ENDIF
064500060606     C                   ENDIF
064600060606     C                   ENDIF
064700060606     C*
064800060515     C                   EVAL      OSIE3NAZN = wNazIsoN
064900060516     C                   EVAL      OSIE3NAZ2 = wNazIso2
065000060516     C                   EVAL      OSIE3NAZ3 = wNazIso3
065100060516     C                   EVAL      OSIE3SRVM = wSrvMark
065200060516     C                   EVAL      OSIE3SRVD = wSrvDes
065300060516     C                   EVAL      OSIE3OSRT = wO_Sort
065400060516     C                   EVAL      OSIE3RDEP = wR_Depot
065500060516     C                   EVAL      OSIE3DDEP = wD_Depot
065600060516     C                   EVAL      OSIE3DSRT = wD_Sort
065700060516     C                   EVAL      OSIE3PTYG = wPty_Group
065800060516     C                   EVAL      OSIE3BID  = wBarcodeID
065900060516     C                   EVAL      OSIE3BIDP = wBarcodeID_Prt
066000061109     C                   EVAL      OSIE3ALWID= d_calID
066100060515     C*
066200060515     C                   ENDSR
066300060515     C*--------------------------------------------------------------------------------------------*
066400060515     C* *inzsr - operazioni iniziali
066500060515     C*--------------------------------------------------------------------------------------------*
066600000000     C     *inzsr        BEGSR
066700060515     C*
066800060515     C* Ricevimento parametri
066900060515     C     *ENTRY        PLIST
067000060515     C                   PARM                    TISIE3DS
067100060515     C*
067200060515     C* Definizione chiavi di lettura
067300060515     C*
067400060515     C* TABEL00F - Completa
067500060515     C     KEYtabel_C    KLIST
067600060515     C                   KFLD                    tblKUT
067700060515     C                   KFLD                    tblCOD
067800060515     C                   KFLD                    tblKEY
067900060515     C*
068000060515     C* DPCCC01L - Completa
068100060515     C     KEYccc01_C    KLIST
068200060515     C                   KFLD                    cccVER
068300060515     C                   KFLD                    cccISOK
068400060606     C*
068500060606     C* DPCCC02L - Completa
068600060606     C     KEYccc02_C    KLIST
068700060606     C                   KFLD                    cccVER
068800060606     C                   KFLD                    cccISO2
068900060515     C*
069000060515     C* DPCSC01L - Completa
069100060515     C     KEYcsc01_C    KLIST
069200060515     C                   KFLD                    cscVER
069300060515     C                   KFLD                    cscSRVC
069400060515     C*
069500060515     C* DPCDP01L - Completa
069600061114     C     KEYcdp01_C    KLIST
069700060515     C                   KFLD                    cdpVER
069800060515     C                   KFLD                    cdpDPC
069900060519     C*
070000060519     C* DPCLO01L - Completa
070100060519     C     KEYclo01_C    KLIST
070200060519     C                   KFLD                    cloVER
070300060520     C                   KFLD                    cloISO2
070400060519     C                   KFLD                    cloAREA
070500060519     C                   KFLD                    cloCTY
070600060510     C*
070700060510     C* CALCOLA LA DATA CORRENTE
070800060510     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
070900060510     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
071000060510     C                   Z-ADD     WNUM8         G08DAT
071100060510     C                   Z-ADD     *ZEROS        G08INV
071200060510     C                   MOVEL     '0'           G08ERR
071300060510     C                   CALL      'XSRDA8'
071400060510     C                   PARM                    WLBDA8
071500060510     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
071600060515     C*
071700060515     C                   ENDSR
