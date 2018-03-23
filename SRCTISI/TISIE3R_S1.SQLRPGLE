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
003100060516     d DS_DPCRO      E DS                  EXTNAME(DPCRO00F)
003200060530     d DS_DPCAL      E DS                  EXTNAME(DPCAL00F)
003300060530     d DS_DPCDY      E DS                  EXTNAME(DPCDY00F)
003400060510     d*------------------
003500060510     d* DS DI PROCEDURA
003600060510     d*------------------
003700060510     d TISIE2DS      E DS
003800060515     d TISIE3DS      E DS
003900060515     d*------------------
004000060515     d* VARIABILI D WRK
004100060515     d*------------------
004200000000     c*--------------------------------------------------------------------------------------------*
004300000000     c* Main lines
004400000000     c*--------------------------------------------------------------------------------------------*
004500060516     C*
004600060516     C* Verifico subito il tipo lancio richiesto
004700060516     C                   if        ISIE3TLA = 'C'                               * solo chiusura
004800060516     C                   seton                                        lr
004900060516     C                   endif
005000060516     C*
005100060516     C                   if        ISIE3TLA = 'E'                               * solo elaborazione
005200060516     C                   exsr      esegui
005300060516     C                   seton                                        rt
005400060516     C                   endif
005500060516     C*
005600060516     C                   if        ISIE3TLA = *blanks                           * elabora e chiude
005700060516     C                   exsr      esegui
005800060516     C                   seton                                        lr
005900060516     C                   endif
006000060515     C*
006100060515     C*------------------------------------------------------------------------*
006200060515     C* ESEGUI - Routine di calcolo instradamento cappario DPD GeoPost
006300060515     C*------------------------------------------------------------------------*
006400060515     C     ESEGUI        BEGSR
006500060520     C*
006600060520     C* Resetto indicatore d errore
006700060520     C                   SETOFF                                       70
006800060515     C*
006900060515     C* Innanzitutto inizializzo i campi d output della ds d procedura corrente
007000060515     C                   EXSR      AZZOUT
007100060515     C*
007200060515     C* Quindi controllo correttezza parametri in input
007300060515     C                   EXSR      CHKPAR
007400060515     C*
007500060515     C* Quindi reperisco la versione da considerare
007600060520     C  N70              EXSR      REPVER
007700060515     C*
007800060515     C* Reperisco il depot DPD relativo alla linea di partenza passata in input
007900060520     C  N70              EXSR      REPDEP
008000060515     C*
008100060515     C* Verifico esistenza depot partenza DPD sui file cappario DPD
008200060606     C                   MOVEL(P)  wR_Depot      wCurrentDepot     4
008300060627     C                   SETOFF                                       60
008400060606     C  N70              EXSR      CHKDEP
008500060515     C*
008600060515     C* Reperisco dati relativi alla nazione destinatario
008700060520     C  N70              EXSR      REPNAZ
008800060519     C*
008900060519     C* Verifica se nazione destino gestisce il cappario (vedi Irlanda)
009000060520     C  N70              EXSR      CHKPTCSYS
009100060515     C*
009200060515     C* Reperisco dati relativi al servizio DPD
009300060520     C  N70              EXSR      REPSRV
009400060519     C*
009500060519     C* Verifico instradamento x area/località DPD
009600060530     C  N70              EXSR      REPCTY
009700060516     C*
009800060516     C* Reperisco dati relativi all'instradamento DPD
009900060520     C  N70              EXSR      REPROUTE
010000060530     C*
010100060530     C* Vaglio instradamento calcolato rispetto a ALLOW (permessi) e DENY (divieti)
010200060530     C  N70              EXSR      CHKALWDNY
010300060515     C*
010400060515     C* Al termine valorizzo i dati d output d procedura corrente
010500060520     C  N70              EXSR      VALOUT
010600060515     C*
010700060515     C                   ENDSR
010800060515     C*------------------------------------------------------------------------*
010900060515     C* AZZOUT - Routine di inizializzazione campi d ouput procedura corrente
011000060515     C*------------------------------------------------------------------------*
011100060515     C     AZZOUT        BEGSR
011200060515     C*
011300060515     C                   EVAL      OSIE3VER  = *zeros
011400060515     C                   EVAL      OSIE3VERD = *blanks
011500060515     C                   EVAL      OSIE3DDE  = *zeros
011600060515     C                   EVAL      OSIE3NAZN = *zeros
011700060515     C                   EVAL      OSIE3NAZ2 = *blanks
011800060515     C                   EVAL      OSIE3NAZ3 = *blanks
011900060515     C                   EVAL      OSIE3SRVM = *blanks
012000060515     C                   EVAL      OSIE3SRVD = *blanks
012100060515     C                   EVAL      OSIE3OSRT = *blanks
012200060515     C                   EVAL      OSIE3RDEP = *blanks
012300060515     C                   EVAL      OSIE3DDEP = *blanks
012400060515     C                   EVAL      OSIE3DSRT = *blanks
012500060515     C                   EVAL      OSIE3PTYG = *blanks
012600060515     C                   EVAL      OSIE3BID  = *zeros
012700060516     C                   EVAL      OSIE3BIDP = *blanks
012800060515     C                   EVAL      OSIE3ERR  = *blanks
012900060515     C                   EVAL      OSIE3ERRL = *blanks
013000060520     C                   EVAL      OSIE3ERRD = *blanks
013100060515     C*
013200060515     C                   ENDSR
013300060515     C*------------------------------------------------------------------------*
013400060515     C* CHKPAR - Routine di controllo correttezza parametri d input
013500060515     C*------------------------------------------------------------------------*
013600060515     C     CHKPAR        BEGSR
013700060520     C*
013800060520     C* Inizializzo variabile esplicitamento errori
013900060520     C                   MOVEL     *blanks       wErrore          20
014000060531     C*
014100060531     C* Innanzitutto imposto il CAP "utilizzato" = al CAP destinatario passato in input
014200060608     C                   EVAL      USIE3CAD = %trim(ISIE3CAD)
014300060608     C*
014400060608     C* Normalizzo il CAP destinatario compattando gli spazi eventualmente presenti
014500060608     C                   Z-ADD     1             wPos              1 0
014600060629     C                   DOW       wPos < %len(%trim(USIE3CAD))
014700060608     C                   IF        %subst(USIE3CAD:wPos:1) = *blanks
014800060608     C                   EVAL      USIE3CAD = %subst(USIE3CAD:1:wPos-1) +
014900060608     C                                        %subst(USIE3CAD:wPos+1)
015000060621     C                   EVAL      wPos = wPos - 1
015100060608     C                   ENDIF
015200060608     C                   ADD       1             wPos
015300060608     C                   ENDDO
015400060515     C*
015500060515     C* Verifica presenza campi obbligatori
015600060515     C*
015700060515     C* - data spedizione...
015800060515     C                   IF        ISIE3DSP = *zeros
015900060520     C                   EVAL      wErrore = '(Data spedizione)'
016000060515     C                   EVAL      OSIE3ERR  = '1'
016100060515     C                   ENDIF
016200060515     C*
016300060515     C* - cap destinatario
016400060515     C                   IF        ISIE3CAD = *blanks
016500060520     C                   EVAL      wErrore = '(CAP destinatario)'
016600060515     C                   EVAL      OSIE3ERR  = '1'
016700060515     C                   ENDIF
016800060515     C*
016900060515     C* - servizio DPD
017000060515     C                   IF        ISIE3SRV = *zeros
017100060520     C                   EVAL      wErrore = '(Servizio DPD)'
017200060515     C                   EVAL      OSIE3ERR  = '1'
017300060515     C                   ENDIF
017400060515     C*
017500060515     C* - linea di partenza
017600060515     C                   IF        ISIE3LNP = *zeros
017700060520     C                   EVAL      wErrore = '(Linea partenza)'
017800060515     C                   EVAL      OSIE3ERR  = '1'
017900060515     C                   ENDIF
018000060515     C*
018100060515     C* Verifica campi facoltativi
018200060520     C*
018300060520     C* - data riferimento...
018400060520     C                   IF        ISIE3DRI = *zeros
018500060606     C***                EVAL      ISIE3DRI = DATCOR
018600060606     C                   EVAL      ISIE3DRI = ISIE3DSP
018700060520     C                   ENDIF
018800060515     C*
018900060515     C* - ora spedizione...
019000060515     C                   IF        ISIE3HSP = *zeros
019100060515     C                   EVAL      ISIE3HSP = 120000
019200060515     C                   ENDIF
019300060515     C*
019400060515     C* - eccezioine x Bartolini...
019500060515     C                   IF        ISIE3EXC = *blanks
019600060515     C                   EVAL      ISIE3EXC = 'N'
019700060515     C                   ENDIF
019800060515     C*
019900060515     C* Se nn ok => imposto subito l'errore generale d procedura
020000060515     C                   IF        OSIE3ERR <> *blanks
020100060516     C                   EVAL      OSIE3ERRL = '92'
020200060520     C                   EVAL      OSIE3ERRD = 'Errore: parametri obbligatori '+
020300060520     C                             'non valorizzati.' + %trimr(wErrore)
020400060515     C                   ENDIF
020500060515     C*
020600060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
020700060515     C                   IF        OSIE3ERR <> *blanks
020800060520     C                   SETON                                        70
020900060515     C                   ENDIF
021000060515     C*
021100060515     C                   ENDSR
021200060515     C*------------------------------------------------------------------------*
021300060515     C* REPVER - Routine di reperimento versione cappario DPD GeoPost
021400060515     C*------------------------------------------------------------------------*
021500060515     C     REPVER        BEGSR
021600060515     C*
021700060515     C                   CLEAR                   TISIE2DS
021800060515     C                   EVAL      SIE2TLA = 'E'
021900060515     C                   EVAL      SIE2DRI = ISIE3DRI
022000060515     C                   CALL      'TISIE2R'
022100060515     C                   PARM                    TISIE2DS
022200060515     C*
022300060515     C* Se nn ok => imposto subito l'errore generale d procedura
022400060515     C                   IF        SIE2VER = *zeros
022500060515     C                   EVAL      OSIE3ERR  = '1'
022600060516     C                   EVAL      OSIE3ERRL = '90'
022700060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento versione'+
022800060515     C                             '. (DPCVE01L)'
022900060515     C                   ENDIF
023000060515     C*
023100060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
023200060515     C                   IF        OSIE3ERR <> *blanks
023300060520     C                   SETON                                        70
023400060515     C                   ENDIF
023500060515     C*
023600060515     C                   ENDSR
023700060515     C*------------------------------------------------------------------------*
023800060515     C* REPDEP - Routine di reperimento depot DPD di partenza
023900060515     C*------------------------------------------------------------------------*
024000060515     C     REPDEP        BEGSR
024100060515     C*
024200060515     C                   MOVEL     *blanks       wR_Depot          4
024300060515     C*
024400060515     C     ISIE3LNP      CHAIN     AZORG01L
024500060515     C                   IF        %found(AZORG01L)
024600060515     C                   EVAL      OG143 = ORGDE3
024700060515     C                   EVAL      wR_Depot = §OGDP1
024800060515     C                   ENDIF
024900060515     C*
025000060515     C* Se nn ok => imposto subito l'errore generale d procedura
025100060515     C                   IF        wR_Depot = *blanks
025200060515     C                   EVAL      OSIE3ERR  = '1'
025300060516     C                   EVAL      OSIE3ERRL = '91'
025400060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento depot ' +
025500060515     C                             ' partenza. (AZORG01L)'
025600060515     C                   ENDIF
025700060515     C*
025800060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
025900060515     C                   IF        OSIE3ERR <> *blanks
026000060520     C                   SETON                                        70
026100060515     C                   ENDIF
026200060515     C*
026300060515     C                   ENDSR
026400060515     C*------------------------------------------------------------------------*
026500060515     C* CHKDEP - Routine di verifica esistenza depot DPD partenza su file cappario DPD
026600060515     C*------------------------------------------------------------------------*
026700060515     C     CHKDEP        BEGSR
026800060515     C*
026900060606     C                   CLEAR                   DPCDP000
027000060607     C                   MOVEL     *blanks       wIATALikeCode     3
027100060606     C*
027200060515     C                   EVAL      cdpVER = SIE2VER
027300060606     C                   EVAL      cdpDPC = wCurrentDepot
027400060515     C     KEYcdp01_C    CHAIN     DPCDP01L
027500060515     C                   IF        %found(DPCDP01L)
027600060515     C                   IF        cdpATB = *blanks
027700060607     C                   MOVEL(P)  cdpIATA       wIATALikeCode
027800060515     C                   ELSE
027900060515     C                   EVAL      OSIE3ERR  = '1'
028000060516     C                   EVAL      OSIE3ERRL = '94'
028100060627     C  N60              EVAL      OSIE3ERRD = 'Errore: depot partenza ' +
028200060515     C                             'annullato. (DPCDP01L)'
028300060627     C   60              EVAL      OSIE3ERRD = 'Errore: depot arrivo ' +
028400060627     C                             'annullato. (DPCDP01L)'
028500060515     C                   ENDIF
028600060515     C                   ELSE
028700060515     C                   EVAL      OSIE3ERR  = '1'
028800060516     C                   EVAL      OSIE3ERRL = '94'
028900060627     C  N60              EVAL      OSIE3ERRD = 'Errore: depot partenza ' +
029000060515     C                             'inesistente. (DPCDP01L)'
029100060627     C   60              EVAL      OSIE3ERRD = 'Errore: depot arrivo ' +
029200060627     C                             'inesistente. (DPCDP01L)'
029300060515     C                   ENDIF
029400060515     C*
029500060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
029600060515     C                   IF        OSIE3ERR <> *blanks
029700060520     C                   SETON                                        70
029800060515     C                   ENDIF
029900060515     C*
030000060515     C                   ENDSR
030100060515     C*------------------------------------------------------------------------*
030200060515     C* REPNAZ - Routine di reperimento dati nazione destinatario
030300060515     C*------------------------------------------------------------------------*
030400060515     C     REPNAZ        BEGSR
030500060515     C*
030600060515     C                   Z-ADD     *zeros        wNazIsoN          3 0
030700060515     C                   MOVEL     *blanks       wNazIso2          2
030800060515     C                   MOVEL     *blanks       wNazIso3          3
030900060519     C                   MOVEL     *blanks       wNazPTC           1 0
031000060515     C*
031100060515     C                   CLEAR                   DS15
031200060515     C                   EVAL      tblKUT = 1
031300060515     C                   EVAL      tblCOD = '15'
031400060515     C                   EVAL      tblKEY = ISIE3NZD
031500060515     C     KEYtabel_C    CHAIN     TABEL00F
031600060515     C                   IF        %found(TABEL00F)
031700060515     C                   IF        tblFLG = *blanks
031800060515     C                   EVAL      DS15 = tblUNI
031900060515     C                   EVAL      cccVER  = SIE2VER
032000060515     C                   EVAL      cccISOK = §15CIE
032100060515     C     KEYccc01_C    CHAIN     DPCCC01L
032200060606     C                   IF        %found(DPCCC01L)
032300060515     C                   IF        cccATB = *blanks
032400060515     C                   EVAL      wNazIsoN = cccISOK
032500060515     C                   EVAL      wNazIso2 = cccISO2
032600060515     C                   EVAL      wNazIso3 = cccISO3
032700060519     C                   EVAL      wNazPTC  = cccPTC
032800060515     C                   ENDIF
032900060515     C                   ENDIF
033000060515     C                   ENDIF
033100060515     C                   ENDIF
033200060515     C*
033300060515     C* Se nn ok => imposto subito l'errore generale d procedura
033400060515     C                   IF        wNazIsoN = *zeros  OR
033500060515     C                             wNazIso2 = *blanks OR
033600060515     C                             wNazIso3 = *blanks
033700060515     C                   EVAL      OSIE3ERR  = '1'
033800060516     C                   EVAL      OSIE3ERRL = '93'
033900060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento nazione.'+
034000060515     C                             ' (DPCCC01L)'
034100060515     C                   ENDIF
034200060515     C*
034300060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
034400060515     C                   IF        OSIE3ERR <> *blanks
034500060520     C                   SETON                                        70
034600060515     C                   ENDIF
034700060515     C*
034800060515     C                   ENDSR
034900060519     C*------------------------------------------------------------------------*
035000060519     C* CHKPTCSYS - Verifica se nazione destino gestisce il cappario (vedi Irlanda)
035100060519     C*------------------------------------------------------------------------*
035200060519     C     CHKPTCSYS     BEGSR
035300060519     C*
035400060519     C* Se nazione nn gestisce il cappario => forzo a *blanks il campo CAP destinatario
035500060519     C* ricevuto in input
035600060519     C                   IF        wNazPTC = 1
035700060531     C                   EVAL      USIE3CAD = *blanks
035800060519     C                   ENDIF
035900060519     C*
036000060519     C* Eseguo forzatura => in caso d nazione destinatario Irlanda forzo il CAP
036100060519     C* destinatario a '1' fisso
036200060519     C                   IF        ISIE3NZD = 'IRL'
036300060531     C                   EVAL      USIE3CAD = '1'
036400060519     C                   ENDIF
036500060519     C*
036600060519     C                   ENDSR
036700060515     C*------------------------------------------------------------------------*
036800060515     C* REPSRV - Routine di verifica e reperimento dati relativi al servizio DPD
036900060515     C*------------------------------------------------------------------------*
037000060515     C     REPSRV        BEGSR
037100060515     C*
037200060515     C                   MOVEL     *blanks       wSrvMark          1
037300060515     C                   MOVEL     *blanks       wSrvDes          16
037400060515     C*
037500060515     C                   EVAL      cscVER  = SIE2VER
037600060515     C                   EVAL      cscSRVC = ISIE3SRV
037700060515     C     KEYcsc01_C    CHAIN     DPCSC01L
037800060515     C                   IF        %found(DPCSC01L)
037900060515     C                   IF        cscATB = *blanks
038000060515     C                   EVAL      wSrvMark = cscSRVM
038100060515     C                   EVAL      wSrvDes  = CSCSRVD
038200060515     C                   ENDIF
038300060515     C                   ENDIF
038400060515     C*
038500060515     C* Se nn ok => imposto subito l'errore generale d procedura
038600060520     C                   IF        wSrvMark = *blanks AND
038700060515     C                             wSrvDes  = *blanks
038800060515     C                   EVAL      OSIE3ERR  = '1'
038900060516     C                   EVAL      OSIE3ERRL = '95'
039000060520     C                   EVAL      OSIE3ERRD = 'Errore in reperimento servizio'+
039100060515     C                             '. (DPCSC01L)'
039200060515     C                   ENDIF
039300060515     C*
039400060515     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
039500060515     C                   IF        OSIE3ERR <> *blanks
039600060520     C                   SETON                                        70
039700060515     C                   ENDIF
039800060515     C*
039900060515     C                   ENDSR
040000060519     C*------------------------------------------------------------------------*
040100060519     C* REPCTY - Routine di verifica esistenza ecezione x area/località DPD
040200060519     C*------------------------------------------------------------------------*
040300060530     C     REPCTY        BEGSR
040400060519     C*
040500060519     C* Se ricevuti in ingresso AREA e/o LOCALITA "provo" d gestire
040600060519     C                   IF        ISIE3AREA <> *blanks OR
040700060519     C                             ISIE3CTY  <> *blanks
040800060519     C*
040900060519     C                   EVAL      cloVER  = SIE2VER
041000060520     C                   EVAL      cloISO2 = wNazIso2
041100060519     C                   EVAL      cloAREA = ISIE3AREA
041200060519     C                   EVAL      cloCTY  = ISIE3CTY
041300060519     C     KEYclo01_C    CHAIN     DPCLO01L
041400060519     C                   IF        %found(DPCLO01L)
041500060519     C                   IF        cloATB = *blanks
041600060519     C*
041700060519     C* Se trovata eccezione x AREA/LOCALITA forzo il valore dell'ecezione sul CAP
041800060519     C* destinatario ricevuto in input
041900060531     C                   EVAL      USIE3CAD = cloPTC
042000060519     C                   ENDIF
042100060519     C                   ENDIF
042200060519     C*
042300060519     C                   ENDIF
042400060519     C*
042500060519     C                   ENDSR
042600060516     C*------------------------------------------------------------------------*
042700060516     C* REPROUTE - Routine di reperimento dati relativi all'instradamento DPD
042800060516     C*------------------------------------------------------------------------*
042900060516     C     REPROUTE      BEGSR
043000060516     C*
043100060530     C                   CLEAR                   ds_dpcro
043200060516     C                   MOVEL     *blanks       wO_Sort           4
043300060516     C                   MOVEL     *blanks       wD_Depot          4
043400060516     C                   MOVEL     *blanks       wD_Sort           4
043500060516     C                   MOVEL     *blanks       wPty_Group        1
043600060516     C                   Z-ADD     *zeros        wBarcodeID        3 0
043700060516     C                   MOVEL     *blanks       wBarcodeID_Prt    1
043800060516     C*
043900060516     C/EXEC SQL
044000060622     C*+ declare C1 sensitive cursor for
044100060622     C+ declare C1 cursor for
044200060721     C+ select * from dpcro01l
044300060516     C+ where croVER = :SIE2VER and croISO2 = :wNazIso2 and
044400060516     C+ (croSRVC = :ISIE3SRV or croSRVC = 0)  and
044500060608     C+ (croROUP = :wR_Depot or croROUP = '0000') and
044600060531     C+ :USIE3CAD between croPTCB and croPTCE and
044700060530     C+ croATB = ' '
044800060516     C+ order by croSRVC desc, croROUP desc
044900060622     C+ for read only
045000060516     C/END-EXEC
045100060516     C
045200060516     C/EXEC SQL
045300060516     C+ open C1
045400060516     C/END-EXEC
045500060516     C
045600060516     C/EXEC SQL
045700060622     C+ Fetch next from C1 into :ds_dpcro
045800060516     C/END-EXEC
045900060516     C*
046000060516     C* Se reperito qualcosa => analizzo
046100060516     C* ....altrimenti trattasi d errore di instradamento
046200060516     C                   if        sqlcod <> *zeros
046300060516     C                   EVAL      OSIE3ERR  = '1'
046400060516     C                   EVAL      OSIE3ERRL = '96'
046500060520     C                   EVAL      OSIE3ERRD = 'Errore in calcolo ' +
046600060516     C                             'instradamento. (DPCRO01L)'
046700060516     C*
046800060516     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
046900060516     C                   IF        OSIE3ERR <> *blanks
047000060520     C                   SETON                                        70
047100060516     C                   ENDIF
047200060516     C*
047300060516     C                   else
047400060516     C*
047500060516     C* Valorizzo i dati dell'instradamento così reperito
047600060516     C                   eval      wO_Sort        = croOSRT
047700060516     C                   eval      wD_Depot       = croDDEP
047800060516     C                   eval      wD_Sort        = croDSRT
047900060516     C                   eval      wPty_Group     = croPTYG
048000060516     C                   eval      wBarcodeID     = croBID
048100060516     C                   eval      wBarcodeID_Prt = croBIDP
048200060516     C*
048300060516     C                   endif
048400060516     C*
048500060516     C/EXEC SQL
048600060516     C+ close C1
048700060516     C/END-EXEC
048800060516     C*
048900060516     C                   ENDSR
049000060530     C*------------------------------------------------------------------------*
049100060530     C* CHKALWDNY - Routine di verifica instradamento calcolato rispetto a ALLOW e DENY
049200060530     C*------------------------------------------------------------------------*
049300060530     C     CHKALWDNY     BEGSR
049400060530     C*
049500060530     C                   CLEAR                   ds_dpcal
049600060530     C                   CLEAR                   ds_dpcdy
049700060530     C*
049800060530     C/EXEC SQL
049900060629     C*+ declare C2 cursor for
050000060629     C*+ select * from dpcal01l
050100060629     C*+ where calVER = :SIE2VER and
050200060629     C*+ (calRULF = :wR_Depot or calRULF = '0000')  and
050300060629     C*+ (calSRVC = :ISIE3SRV or calSRVC = 0)  and
050400060629     C*+ ((calISO2 = :wNazIso2 and :USIE3CAD between calPTCB and calPTCE) or
050500060629     C*+  (calRULT = :wD_Depot or calRULT = '0000')) and
050600060629     C*+ calATB = ' '
050700060629     C*+ order by calRULF desc, calSRVC desc, calRULT desc, calISO2 desc
050800060629     C*+ for read only
050900060629     C+ declare C2 cursor for
051000060629     C+ select * from dpcal01l
051100060629     C+ where calVER = :SIE2VER and
051200060629     C+ calRULF = :wR_Depot and
051300060629     C+ calSRVC = :ISIE3SRV and
051400060629     C+ ((calISO2 = :wNazIso2 and :USIE3CAD between calPTCB and calPTCE) or
051500060629     C+ calRULT = :wD_Depot) and
051600060629     C+ calATB = ' '
051700060530     C+ order by calRULF desc, calSRVC desc, calRULT desc, calISO2 desc
051800060622     C+ for read only
051900060530     C/END-EXEC
052000060530     C
052100060530     C/EXEC SQL
052200060530     C+ open C2
052300060530     C/END-EXEC
052400060530     C
052500060530     C/EXEC SQL
052600060622     C+ Fetch next from C2 into :ds_dpcal
052700060530     C/END-EXEC
052800060530     C*
052900060530     C* Se reperito qualcosa => analizzo
053000060530     C* ....altrimenti trattasi d errore di permesso instradamento
053100060530     C                   if        sqlcod <> *zeros
053200060530     C                   EVAL      OSIE3ERR  = '1'
053300060530     C                   EVAL      OSIE3ERRL = '97'
053400060530     C                   EVAL      OSIE3ERRD = 'Instradamento non ' +
053500060530     C                             'consentito. (DPCAL01L)'
053600060530     C*
053700060530     C* Gestisco il salto incondizionato solo xchè costretto da esigenze d prestazioni
053800060530     C                   IF        OSIE3ERR <> *blanks
053900060530     C                   SETON                                        70
054000060530     C                   ENDIF
054100060530     C*
054200060530     C                   else
054300060530     C*
054400060530     C/EXEC SQL
054500060629     C*+ declare C3 cursor for
054600060629     C*+ select * from dpcdy01l
054700060629     C*+ where cdyVER = :SIE2VER and cdyID = :calID and
054800060629     C*+ (cdyRULF = :wR_Depot or cdyRULF = '0000')  and
054900060629     C*+ (cdySRVC = :ISIE3SRV or cdySRVC = 0)  and
055000060629     C*+ ((cdyISO2 = :wNazIso2 and :USIE3CAD between cdyPTCB and cdyPTCE) or
055100060629     C*+  (cdyRULT = :wD_Depot or cdyRULT = '0000')) and
055200060629     C*+ cdyATB = ' '
055300060629     C*+ order by cdyRULF desc, cdySRVC desc, cdyRULT desc, cdyISO2 desc
055400060629     C*+ for read only
055500060629     C+ declare C3 cursor for
055600060629     C+ select * from dpcdy01l
055700060629     C+ where cdyVER = :SIE2VER and cdyID = :calID and
055800060629     C+ cdyRULF = :wR_Depot and
055900060629     C+ cdySRVC = :ISIE3SRV and
056000060629     C+ ((cdyISO2 = :wNazIso2 and :USIE3CAD between cdyPTCB and cdyPTCE) or
056100060629     C+ cdyRULT = :wD_Depot) and
056200060629     C+ cdyATB = ' '
056300060629     C+ order by cdyRULF desc, cdySRVC desc, cdyRULT desc, cdyISO2 desc
056400060629     C+ for read only
056500060530     C/END-EXEC
056600060530     C
056700060530     C/EXEC SQL
056800060530     C+ open C3
056900060530     C/END-EXEC
057000060530     C
057100060530     C/EXEC SQL
057200060622     C+ Fetch next from C3 into :ds_dpcdy
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
060000060530     C                   ENDSR
060100060515     C*------------------------------------------------------------------------*
060200060515     C* VALOUT - Routine di valorizzazione dati ouput procedura corrente
060300060515     C*------------------------------------------------------------------------*
060400060515     C     VALOUT        BEGSR
060500060515     C*
060600060515     C                   EVAL      OSIE3VER  = SIE2VER
060700060515     C                   EVAL      OSIE3VERD = SIE2VERD
060800060515     C                   EVAL      OSIE3DDE  = SIE2DDE
060900060606     C*
061000060606     C* Effettuo forzatura => in stampa etichetta deve essere indicata la nazione del depot
061100060606     C* d arrivo e nn la nazione d destino della spedizione (strano ma vero !!!)
061200060606     C                   MOVEL(P)  wD_Depot      wCurrentDepot     4
061300060627     C                   SETON                                        60
061400060606     C                   EXSR      CHKDEP
061500060607     C                   EVAL      OSIE3IATAD = wIATALikeCode
061600060606     C                   IF        cdpISO2 <> *blanks
061700060606     C                   EVAL      cccVER  = SIE2VER
061800060606     C                   EVAL      cccISO2 = cdpISO2
061900060606     C     KEYccc02_C    CHAIN     DPCCC02L
062000060606     C                   IF        %found(DPCCC02L)
062100060606     C                   IF        cccATB = *blanks
062200060607     C                   EVAL      OSIE3NAZND = cccISOK
062300060607     C                   EVAL      OSIE3NAZ2D = cccISO2
062400060607     C                   EVAL      OSIE3NAZ3D = cccISO3
062500060606     C                   ENDIF
062600060606     C                   ENDIF
062700060606     C                   ENDIF
062800060606     C*
062900060515     C                   EVAL      OSIE3NAZN = wNazIsoN
063000060516     C                   EVAL      OSIE3NAZ2 = wNazIso2
063100060516     C                   EVAL      OSIE3NAZ3 = wNazIso3
063200060516     C                   EVAL      OSIE3SRVM = wSrvMark
063300060516     C                   EVAL      OSIE3SRVD = wSrvDes
063400060516     C                   EVAL      OSIE3OSRT = wO_Sort
063500060516     C                   EVAL      OSIE3RDEP = wR_Depot
063600060516     C                   EVAL      OSIE3DDEP = wD_Depot
063700060516     C                   EVAL      OSIE3DSRT = wD_Sort
063800060516     C                   EVAL      OSIE3PTYG = wPty_Group
063900060516     C                   EVAL      OSIE3BID  = wBarcodeID
064000060516     C                   EVAL      OSIE3BIDP = wBarcodeID_Prt
064100060530     C                   EVAL      OSIE3ALWID= calID
064200060515     C*
064300060515     C                   ENDSR
064400060515     C*--------------------------------------------------------------------------------------------*
064500060515     C* *inzsr - operazioni iniziali
064600060515     C*--------------------------------------------------------------------------------------------*
064700000000     C     *inzsr        BEGSR
064800060515     C*
064900060515     C* Ricevimento parametri
065000060515     C     *ENTRY        PLIST
065100060515     C                   PARM                    TISIE3DS
065200060515     C*
065300060515     C* Definizione chiavi di lettura
065400060515     C*
065500060515     C* TABEL00F - Completa
065600060515     C     KEYtabel_C    KLIST
065700060515     C                   KFLD                    tblKUT
065800060515     C                   KFLD                    tblCOD
065900060515     C                   KFLD                    tblKEY
066000060515     C*
066100060515     C* DPCCC01L - Completa
066200060515     C     KEYccc01_C    KLIST
066300060515     C                   KFLD                    cccVER
066400060515     C                   KFLD                    cccISOK
066500060606     C*
066600060606     C* DPCCC02L - Completa
066700060606     C     KEYccc02_C    KLIST
066800060606     C                   KFLD                    cccVER
066900060606     C                   KFLD                    cccISO2
067000060515     C*
067100060515     C* DPCSC01L - Completa
067200060515     C     KEYcsc01_C    KLIST
067300060515     C                   KFLD                    cscVER
067400060515     C                   KFLD                    cscSRVC
067500060515     C*
067600060515     C* DPCDP01L - Completa
067700060515     C     KEYcdp01_C    KLIST
067800060515     C                   KFLD                    cdpVER
067900060515     C                   KFLD                    cdpDPC
068000060519     C*
068100060519     C* DPCLO01L - Completa
068200060519     C     KEYclo01_C    KLIST
068300060519     C                   KFLD                    cloVER
068400060520     C                   KFLD                    cloISO2
068500060519     C                   KFLD                    cloAREA
068600060519     C                   KFLD                    cloCTY
068700060510     C*
068800060510     C* CALCOLA LA DATA CORRENTE
068900060510     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
069000060510     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
069100060510     C                   Z-ADD     WNUM8         G08DAT
069200060510     C                   Z-ADD     *ZEROS        G08INV
069300060510     C                   MOVEL     '0'           G08ERR
069400060510     C                   CALL      'XSRDA8'
069500060510     C                   PARM                    WLBDA8
069600060510     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
069700060622     C*
069800060622     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
069900060622     C
070000060622     C/EXEC SQL
070100060622     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
070200060622     C/END-EXEC
070300060515     C*
070400060515     C                   ENDSR
