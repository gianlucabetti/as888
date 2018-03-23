000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120427
000400120427     FTNTBE11L  IF   E           K DISK
000500110927     FTABEL00F  IF   E           K DISK
000600110103     FTISTB03L  UF   E           K DISK    commit
000700160520     FTISTBD3I  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900031103
001000031103     D*-----------
001100120427     D* SCHIERE
001200031103     D*-----------
001300120427     D skCAUBAR        S              3    DIM(999)
001400120427     D skCAUDES        S             30    DIM(999)
001500120427     D jCAU            S              3  0 INZ(*zeros)
001600120427
001700030924
001800041015     D*-----------
001900041015     D* VARIABILI D WRK
002000041015     D*-----------
002100031103     D prmppi          S             50
002200030924     D prmesito        S              1
002300030924     D wrkesito        S                   like(prmesito)
002400170608     D wLNP            S                   like(tasLNP)
002500170608     D wLNA            S                   like(tasLNA)
002600110929
002700041015
002800041015     D*-----------
002900041015     D* DEFINIZIONE DS ESTERNE
003000041015     D*-----------
003100111116     D DS2Z          E DS
003200120427     D DICE          E DS
003300031103
003400031103     D*-----------
003500031103     D* VARIABILI RIFERITE AL DATA-BASE
003600031103     D*-----------
003700080618     D wKstbKSU        S                   like(stbKSU)
003800080618     D wKstbFTR        S                   like(stbFTR)
003900030924
004000110103
004100031103
004200031103     D*-----------
004300031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
004400031103     D*-----------
004500031103     D DSPPI           DS
004600080618     D  w§PPIKSU                      7  0
004700160520     D  w§PPILIN                      2
004800160520     D  w§PPIDELTXT                   1
004900160520     D  w§PPISEPFLD                   1
005000160520     D  w§PPILINK                     1
005100031103
005200110928
005300110928     D*------------------
005400110928     D* LINKING A DEFINIZIONI ESTERNE
005500110928     D*------------------
005600110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005700110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005800160524     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
005900160524     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
006000170608     D/COPY GAITRASRC/SRCPROTOPI,UBRTVSPE
006100110928
006200030924
006300030325
006400030924     C* MAIN/
006500041015     C*
006600041015     C* Carica i dati tabellati
006700041015     C                   EXSR      cartab
006800041015     C*
006900041015     C* Esegue traduzione
007000991027     C                   EXSR      traduci
007100110929     C*
007200921023     C                   SETON                                        LR
007300111121
007400111121
007500111121
007600111121     C     cartab        BEGSR
007700111121     C*
007800120427     C* Carico causali evento 2Z e descrizioni in lingua (ICE)
007900120427     C                   clear                   skCAUBAR
008000120427     C                   clear                   skCAUDES
008100120427     C                   z-add     *zeros        jCAU
008200111121     C                   eval      tblKUT = 1
008300120427     C                   eval      tblCOD = '2Z'
008400111121     C     KEYtab00P     setll     tabel00f
008500111121     C     KEYtab00P     reade     tabel00f
008600111121     C                   dow       not %eof(tabel00f)
008700120427     C                   add       1             jCAU
008800120427     C                   movel(p)  tblUNI        DS2Z
008900120427     C                   eval      skCAUBAR(jCAU) = tblKEY
009000120427     C*
009100160520     C* Verifico che lingua richiesta
009200160520     C                   select
009300160520     C                   when      w§PPILIN = 'en' OR w§PPILIN = 'EN'
009400160520     C                   eval      w§PPILIN = 'en'
009500160520     C                   eval      tbeLIN   = '2'
009600160520     C                   when      w§PPILIN = 'fr' OR w§PPILIN = 'FR'
009700160520     C                   eval      w§PPILIN = 'fr'
009800160520     C                   eval      tbeLIN   = '3'
009900160520     C                   when      w§PPILIN = 'de' OR w§PPILIN = 'DE'
010000160520     C                   eval      w§PPILIN = 'de'
010100160520     C                   eval      tbeLIN   = '4'
010200160520     C                   other
010300160520     C                   eval      w§PPILIN = 'it'
010400160520     C                   eval      tbeLIN   = *blanks
010500160520     C                   endsl
010600160520     C*
010700120427     C                   eval      tbeLIN = w§PPILIN
010800120427     C                   eval      tbeCOD = 'ICE'
010900120427     C                   eval      tbeKE1 = tblKEY
011000120427     C     KEYtbe11P     chain     tntbe11l
011100120427     C                   if        %found(tntbe11l)
011200120427     C                   eval      DICE = tbeUNI
011300120427     C                   eval      skCAUDES(jCAU) = §ICEDEP
011400120427     C                   else
011500120427     C                   eval      skCAUDES(jCAU) = '-'
011600120427     C                   endif
011700120427     C*
011800111121     C     KEYtab00P     reade     tabel00f
011900111121     C                   enddo
012000120427     C*
012100111121     C                   ENDSR
012200991027
012300030924
012400030924
012500991027     C     traduci       BEGSR
012600031103     C*
012700031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
012800031103     C* di traduzione (trattasi d "post-traduzione")
012900120427     C                   eval      wKstbKSU = w§PPIKSU
013000120427     C                   eval      wKstbFTR = *blanks
013100120427     C     KEYstb03P     setll     tistb03l
013200120427     C                   if        %equal(tistb03l)
013300120427     C     KEYstb03P     reade     tistb03l
013400991027     C*
013500031103     C* Ciclo x tuttii record del cliente da trasmettere
013600120427     C                   dow       not %eof(tistb03l)
013700080618     C*
013800120427     C                   seton                                        50
013900031103     C*
014000031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
014100031103     C* deve restituire al cliente nel file tradotto
014200120427     C                   if        stbFTR = *blanks                             * ridondante
014300160729     C*
014400160729     C* Non elaboro gli stati di tipo '9'/'4'
014500160729     C                   if        stbTIS='9' and stbPRS='4'
014600160729     C                   else
014700110103     C*
014800120427     C                   setoff                                       50
014900120427     C*
015000160520     C                   z-add     *zeros        wPO               3 0
015100160520     C                   movel     *blanks       oScanEvent      256
015200160520     C                   movel     *blanks       oScanEventDes   256
015300160520     C                   movel     *zeros        oScanDatOra      14
015400160826     C                   movel     *blanks       wNUMSPED         12
015500160520     C                   movel     *blanks       oLinkDir        300
015600160520     C*
015700160520     C* Valorizzo subito la data/ora dello status corrente
015800160524     C                   eval      oScanDatOra =
015900160524     C                              UBFMTDATE_Convert(
016000160524     C                                 %trim(%char(stbDAS*10000+stbORS))+'00' :
016100160524     C                                 'YYYYMMDDHHMISS' : 'YYYYMMDDHHMISSZ')
016200170608     C*
016300170608     C* Reperisco i dati di testata
016400170608     C                   call      'UBRTVSPE'
016500170608     C                   parm      'APS'         iRTVSPE_Opz
016600170608     C                   parm      stbAAS        iRTVSPE_AAS
016700170608     C                   parm      stbLNP        iRTVSPE_LNP
016800170608     C                   parm      stbNRS        iRTVSPE_NRS
016900170608     C                   parm      stbNSP        iRTVSPE_NSP
017000170608     C                   parm                    oRTVSPE_TASDS
017100170608     C                   parm                    oRTVSPE_ARBDS
017200170608     C                   parm                    oRTVSPE_BLPDS
017300170608     C                   parm                    oRTVSPE_Esito
017400170608     C*
017500170608     C* Valorizzo il buffer d outout
017600170608     C                   select
017700170608     C*
017800170608     C                   when      %subst(oRTVSPE_Esito:1:2) = ' P'
017900170608     C                   eval      wLNP = blpLNP
018000170608     C                   eval      wLNA = blpLNA
018100170608     C*
018200170608     C                   when      %subst(oRTVSPE_Esito:1:1) = 'A'
018300170608     C                   eval      wLNP = arbLNP
018400170608     C                   eval      wLNA = arbLNA
018500170608     C*
018600170608     C                   when      %subst(oRTVSPE_Esito:3:1) = 'S'
018700170608     C                   eval      wLNP = tasLNP
018800170608     C                   eval      wLNA = tasLNA
018900170608     C*
019000170608     C                   other
019100170608     C                   clear                   wLNP
019200170608     C                   clear                   wLNA
019300170608     C                   seton                                        50
019400170608     C*
019500170608     C                   endsl
019600120427     C*
019700120427     C* Innanzitutto valorizzo i dati "correnti"
019800120427     C                   select
019900120427     C*
020000120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
020100120427     C                   eval      oScanEvent = 'INCARICO'
020200170608     C                   eval      wPO = wLNP
020300120427     C*
020400120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
020500120427     C                   eval      oScanEvent = 'ARRPRICOL'
020600170608     C                   eval      wPO = wLNA
020700120427     C*
020800120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
020900120427     C                   eval      oScanEvent = 'RESO'
021000170608     C                   eval      wPO = wLNA
021100120427     C*
021200120427     C                   when      %subst(stbCOS:1:6) = 'CONSOK'
021300120427     C                   eval      oScanEvent = 'CONSOK'
021400170608     C                   eval      wPO = wLNA
021500160520     C*
021600160520     C                   when      %subst(stbCOS:1:9) = 'GIACNODIS'
021700160520     C                   eval      oScanEvent = 'GIACNODIS'
021800170608     C                   eval      wPO = wLNA
021900160520     C*
022000160520     C                   when      %subst(stbCOS:1:9) = 'GIACSIDIS'
022100160520     C                   eval      oScanEvent = 'GIACSIDIS'
022200170608     C                   eval      wPO = wLNA
022300160520     C*
022400160520     C                   when      %subst(stbCOS:1:9) = 'COLNOTAFF'
022500160520     C                   eval      oScanEvent = 'COLNOTAFF'
022600170608     C                   eval      wPO = wLNP
022700160520     C*
022800160520     C                   when      %subst(stbCOS:1:9) = 'SPUNTNPG5'
022900160520     C                   eval      oScanEvent = 'SPUNTNPG5'
023000170608     C                   eval      wPO = wLNP
023100160520     C                   exsr      RTVDETT
023200120427     C*
023300120427     C                   other
023400120427     C                   movel     stbCOS        wCOS              3
023500120427     C                   z-add     1             jCAU
023600120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
023700120427     C                   if        %found
023800120427     C                   eval      oScanEvent    = skCAUBAR(jCAU)
023900120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
024000170608     C                   eval      wPO = wLNA
024100120427     C                   else
024200120427     C                   seton                                        50
024300110927     C                   endif
024400120427     C*
024500120427     C                   endsl
024600110927     C*
024700160729     C                   endif
024800120427     C                   endif
024900080618     C*
025000080618     C* Scarico il buffer d output
025100120427     C  N50              exsr      WRIREC
025200031103     C*
025300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
025400120427     C                   eval      stbFTR = 'T'
025500120427     C                   update    tistb000
025600991027     C*
025700120427     C     KEYstb03P     reade     tistb03l
025800120427     C                   setoff                                       50
025900120427     C                   enddo
026000120427     C                   endif
026100991027     C*
026200120427     C                   eval      wrkesito = '0'
026300991027     C*
026400910830     C                   ENDSR
026500031103
026600031103
026700031103
026800110103     C     wrirec        BEGSR
026900110928     C*
027000120427     C* Considero solamente le bolle originali
027100111103     C                   movel     *blanks       wEsito1           1
027200111103     C*
027300111103     C* Chiamata metodo GetLblTyp
027400111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
027500111103     C                                                stbAAS
027600111103     C                                               :stbLNP
027700111103     C                                               :stbNRS
027800111103     C                                               :stbNSP
027900111103     C                                               :pOutLblTyp
028000111103     C                                               :pOutAnnoBO
028100111103     C                                               :pOutLineaParBO
028200111103     C                                               :pOutSerieBO
028300111103     C                                               :pOutNumSpedBO
028400111103     C                                               :pOutDcmBO
028500111103     C                                               :pOutCcaBO
028600111103     C                                               :pOutRblBO))
028700111103     C*
028800120427     C                   if        pOutLblTyp = 'O' and
028900120427     C                             not *IN50                                    * ridondante
029000160826     C*
029100160826     C* Compongo il numero spedizione
029200160826     C                   eval      wNUMSPED = %editc(stbLNP:'X')+
029300160826     C                                        %editc(stbNRS:'X')+
029400160826     C                                        %editc(stbNSP:'X')
029500160520     C*
029600160520     C* Se richiesto nei parametri ritorno e compongo il link dirertto della sped. corrrente
029700160520     C                   if        w§PPILINK = 'S'
029800160927     C                   eval      oLinkDir = 'http://vas.brt.it/vas/' +
029900160520     C                                        'sped_det_show.hsm?nspediz='+
030000160826     C                                        wNUMSPED+'&lang='+w§PPILIN
030100160520     C                   endif
030200160520     C*
030300160520     C* Scarico il buffer di output così composto
030400160520     C                   eval      VAWDTA =
030500160520     C                             %trim(w§PPIDELTXT)+%trim('1')             +
030600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030700160826     C***                          %trim(w§PPIDELTXT)+%trim(stbRMA)          +
030800160826     C                             %trim(w§PPIDELTXT)+%trim(wNUMSPED)        +
030900160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031000160520     C                             %trim(w§PPIDELTXT)+%trim(%char(stbRMN))   +
031100160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031200160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031300160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031400160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031500160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031600160524     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031700160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031800160520     C                             %trim(w§PPIDELTXT)+%trim(oScanDatOra)     +
031900160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032000160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
032100160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032200160530     C***                          %trim(w§PPIDELTXT)+%trim(%char(w§PPIKSU)) +
032300160530     C                             %trim(w§PPIDELTXT)+%trim(%char(stbKSC))   +
032400160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032500160520     C                             %trim(w§PPIDELTXT)+%trim('738332')        +
032600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032700160520     C                             %trim(w§PPIDELTXT)+%trim('BARTOLINI')     +
032800160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032900160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
033000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
033100160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
033200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
033300160520     C                             %trim(w§PPIDELTXT)+%trim(oLinkDir)        +
033400160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
033500160524     C                             %trim(w§PPIDELTXT)+%trim(oScanEvent)      +
033600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
033700160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
033800160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
033900160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
034000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
034100160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
034200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
034300160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
034400160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
034500160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
034600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
034700160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
034800160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
034900160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
035000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
035100160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
035200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
035300160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
035400160520     C                             %trim(w§PPIDELTXT)
035500110927     C*
035600120427     C                   write     TIVAW000
035700120427     C                   endif
035800031103     C*
035900031103     C                   ENDSR
036000160520
036100160520
036200160520
036300160520     C     RTVDETT       BEGSR
036400160520     C*
036500160520     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
036600160520     C     KEYstbd3P     SETLL     tistbd3i
036700160520     C                   IF        %equal(tistbd3i)
036800160520     C     KEYstbd3P     READE     tistbd3i
036900160520     C                   DOW       not %eof(tistbd3i)
037000160520     C* Filtro unicamente i segnacolli "BRT"
037100160520     C                   IF        stbdTIS = 'BRT'
037200160524     C                   eval      oScanDatOra =
037300160524     C                              UBFMTDATE_Convert(
037400160524     C                                 %trim(%char(stbdDAD*1000000+stbDORD))  :
037500160524     C                                 'YYYYMMDDHHMISS' : 'YYYYMMDDHHMISSZ')
037600160520     C                   ENDIF
037700160520     C     KEYstbd3P     READE     tistbd3i
037800160520     C                   ENDDO
037900160520     C                   ENDIF
038000160520     C*
038100160520     C                   ENDSR
038200031103
038300031103
038400991027
038500991027      /TITLE Operazioni iniziali.
038600991027     C     *inzsr        BEGSR
038700991027     C*
038800991027     C     *ENTRY        PLIST
038900031103     C                   parm                    prmppi
039000991027     C     wrkesito      parm      wrkesito      prmesito
039100031103     C*
039200031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
039300031103     C                   EVAL      DSPPI = prmppi
039400030709     C*
039500030709     C* Definizione chiavi
039600110927     C*
039700110927     C* Chiave su TABEL00F - Parziale
039800110927     C     KEYtab00P     KLIST
039900110927     C                   KFLD                    tblKUT
040000110927     C                   KFLD                    tblCOD
040100120427     C*
040200120427     C* Chiave su TNTBE00F - Parziale
040300120427     C     KEYtbe11P     KLIST
040400120427     C                   KFLD                    tbeLIN
040500120427     C                   KFLD                    tbeCOD
040600120427     C                   KFLD                    tbeKE1
040700030924     C*
040800110103     C* Chiave su TISTB03L - Parziale
040900110103     C     KEYstb03P     KLIST
041000031104     C                   KFLD                    wKstbKSU
041100031103     C                   KFLD                    wKstbFTR
041200160520     C*
041300160520     C* Chiave su TISTBD3I - Parziale
041400160520     C     KEYstbd3P     KLIST
041500160520     C                   KFLD                    wKstbFTR
041600160520     C                   KFLD                    stbKSU
041700160520     C                   KFLD                    stbAAS
041800160520     C                   KFLD                    stbLNP
041900160520     C                   KFLD                    stbNRS
042000160520     C                   KFLD                    stbNSP
042100160520     C                   KFLD                    stbDAS
042200160520     C                   KFLD                    stbORS
042300160520     C                   KFLD                    stbCOS
042400991027     C*
042500991027     C                   ENDSR
