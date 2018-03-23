000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120427
000400160520     FTITAS30C  IF   E           K DISK
000500120427     FTNTBE11L  IF   E           K DISK
000600110927     FTABEL00F  IF   E           K DISK
000700110103     FTISTB03L  UF   E           K DISK    commit
000800160520     FTISTBD3I  UF   E           K DISK    commit
000900090323     FTIVAWWWT  UF A E             DISK    commit
001000031103
001100031103     D*-----------
001200120427     D* SCHIERE
001300031103     D*-----------
001400120427     D skCAUBAR        S              3    DIM(999)
001500120427     D skCAUDES        S             30    DIM(999)
001600120427     D jCAU            S              3  0 INZ(*zeros)
001700120427
001800030924
001900041015     D*-----------
002000041015     D* VARIABILI D WRK
002100041015     D*-----------
002200031103     D prmppi          S             50
002300030924     D prmesito        S              1
002400030924     D wrkesito        S                   like(prmesito)
002500110929
002600041015
002700041015     D*-----------
002800041015     D* DEFINIZIONE DS ESTERNE
002900041015     D*-----------
003000111116     D DS2Z          E DS
003100120427     D DICE          E DS
003200031103
003300031103     D*-----------
003400031103     D* VARIABILI RIFERITE AL DATA-BASE
003500031103     D*-----------
003600080618     D wKstbKSU        S                   like(stbKSU)
003700080618     D wKstbFTR        S                   like(stbFTR)
003800030924
003900110103
004000031103
004100031103     D*-----------
004200031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
004300031103     D*-----------
004400031103     D DSPPI           DS
004500080618     D  w§PPIKSU                      7  0
004600160520     D  w§PPILIN                      2
004700160520     D  w§PPIDELTXT                   1
004800160520     D  w§PPISEPFLD                   1
004900160520     D  w§PPILINK                     1
005000031103
005100110928
005200110928     D*------------------
005300110928     D* LINKING A DEFINIZIONI ESTERNE
005400110928     D*------------------
005500110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005600110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005700160524     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
005800160524     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
005900110928
006000030924
006100030325
006200030924     C* MAIN/
006300041015     C*
006400041015     C* Carica i dati tabellati
006500041015     C                   EXSR      cartab
006600041015     C*
006700041015     C* Esegue traduzione
006800991027     C                   EXSR      traduci
006900110929     C*
007000921023     C                   SETON                                        LR
007100111121
007200111121
007300111121
007400111121     C     cartab        BEGSR
007500111121     C*
007600120427     C* Carico causali evento 2Z e descrizioni in lingua (ICE)
007700120427     C                   clear                   skCAUBAR
007800120427     C                   clear                   skCAUDES
007900120427     C                   z-add     *zeros        jCAU
008000111121     C                   eval      tblKUT = 1
008100120427     C                   eval      tblCOD = '2Z'
008200111121     C     KEYtab00P     setll     tabel00f
008300111121     C     KEYtab00P     reade     tabel00f
008400111121     C                   dow       not %eof(tabel00f)
008500120427     C                   add       1             jCAU
008600120427     C                   movel(p)  tblUNI        DS2Z
008700120427     C                   eval      skCAUBAR(jCAU) = tblKEY
008800120427     C*
008900160520     C* Verifico che lingua richiesta
009000160520     C                   select
009100160520     C                   when      w§PPILIN = 'en' OR w§PPILIN = 'EN'
009200160520     C                   eval      w§PPILIN = 'en'
009300160520     C                   eval      tbeLIN   = '2'
009400160520     C                   when      w§PPILIN = 'fr' OR w§PPILIN = 'FR'
009500160520     C                   eval      w§PPILIN = 'fr'
009600160520     C                   eval      tbeLIN   = '3'
009700160520     C                   when      w§PPILIN = 'de' OR w§PPILIN = 'DE'
009800160520     C                   eval      w§PPILIN = 'de'
009900160520     C                   eval      tbeLIN   = '4'
010000160520     C                   other
010100160520     C                   eval      w§PPILIN = 'it'
010200160520     C                   eval      tbeLIN   = *blanks
010300160520     C                   endsl
010400160520     C*
010500120427     C                   eval      tbeLIN = w§PPILIN
010600120427     C                   eval      tbeCOD = 'ICE'
010700120427     C                   eval      tbeKE1 = tblKEY
010800120427     C     KEYtbe11P     chain     tntbe11l
010900120427     C                   if        %found(tntbe11l)
011000120427     C                   eval      DICE = tbeUNI
011100120427     C                   eval      skCAUDES(jCAU) = §ICEDEP
011200120427     C                   else
011300120427     C                   eval      skCAUDES(jCAU) = '-'
011400120427     C                   endif
011500120427     C*
011600111121     C     KEYtab00P     reade     tabel00f
011700111121     C                   enddo
011800120427     C*
011900111121     C                   ENDSR
012000991027
012100030924
012200030924
012300991027     C     traduci       BEGSR
012400031103     C*
012500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
012600031103     C* di traduzione (trattasi d "post-traduzione")
012700120427     C                   eval      wKstbKSU = w§PPIKSU
012800120427     C                   eval      wKstbFTR = *blanks
012900120427     C     KEYstb03P     setll     tistb03l
013000120427     C                   if        %equal(tistb03l)
013100120427     C     KEYstb03P     reade     tistb03l
013200991027     C*
013300031103     C* Ciclo x tuttii record del cliente da trasmettere
013400120427     C                   dow       not %eof(tistb03l)
013500080618     C*
013600120427     C                   seton                                        50
013700031103     C*
013800031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
013900031103     C* deve restituire al cliente nel file tradotto
014000120427     C                   if        stbFTR = *blanks                             * ridondante
014100160729     C*
014200160729     C* Non elaboro gli stati di tipo '9'/'4'
014300160729     C                   if        stbTIS='9' and stbPRS='4'
014400160729     C                   else
014500110103     C*
014600120427     C                   setoff                                       50
014700160520     C*
014800160520     C* Innanzitutto chaino la bolla direttamente su TITAS
014900160520     C     KEYtas30P     chain     titas30c
015000160520     C                   if        %found(titas30c)
015100120427     C*
015200160520     C                   z-add     *zeros        wPO               3 0
015300160520     C                   movel     *blanks       oScanEvent      256
015400160520     C                   movel     *blanks       oScanEventDes   256
015500160520     C                   movel     *zeros        oScanDatOra      14
015600160826     C                   movel     *blanks       wNUMSPED         12
015700160520     C                   movel     *blanks       oLinkDir        300
015800160520     C*
015900160520     C* Valorizzo subito la data/ora dello status corrente
016000160524     C                   eval      oScanDatOra =
016100160524     C                              UBFMTDATE_Convert(
016200160524     C                                 %trim(%char(stbDAS*10000+stbORS))+'00' :
016300160524     C                                 'YYYYMMDDHHMISS' : 'YYYYMMDDHHMISSZ')
016400120427     C*
016500120427     C* Innanzitutto valorizzo i dati "correnti"
016600120427     C                   select
016700120427     C*
016800120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
016900120427     C                   eval      oScanEvent = 'INCARICO'
017000120427     C                   eval      wPO = tasLNP
017100120427     C*
017200120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
017300120427     C                   eval      oScanEvent = 'ARRPRICOL'
017400120427     C                   eval      wPO = tasLNA
017500120427     C*
017600120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
017700120427     C                   eval      oScanEvent = 'RESO'
017800120427     C                   eval      wPO = tasLNA
017900120427     C*
018000120427     C                   when      %subst(stbCOS:1:6) = 'CONSOK'
018100120427     C                   eval      oScanEvent = 'CONSOK'
018200120427     C                   eval      wPO = tasLNA
018300160520     C*
018400160520     C                   when      %subst(stbCOS:1:9) = 'GIACNODIS'
018500160520     C                   eval      oScanEvent = 'GIACNODIS'
018600160520     C                   eval      wPO = tasLNA
018700160520     C*
018800160520     C                   when      %subst(stbCOS:1:9) = 'GIACSIDIS'
018900160520     C                   eval      oScanEvent = 'GIACSIDIS'
019000160520     C                   eval      wPO = tasLNA
019100160520     C*
019200160520     C                   when      %subst(stbCOS:1:9) = 'COLNOTAFF'
019300160520     C                   eval      oScanEvent = 'COLNOTAFF'
019400160520     C                   eval      wPO = tasLNP
019500160520     C*
019600160520     C                   when      %subst(stbCOS:1:9) = 'SPUNTNPG5'
019700160520     C                   eval      oScanEvent = 'SPUNTNPG5'
019800160520     C                   eval      wPO = tasLNP
019900160520     C                   exsr      RTVDETT
020000120427     C*
020100120427     C                   other
020200120427     C                   movel     stbCOS        wCOS              3
020300120427     C                   z-add     1             jCAU
020400120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
020500120427     C                   if        %found
020600120427     C                   eval      oScanEvent    = skCAUBAR(jCAU)
020700120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
020800120427     C                   eval      wPO = tasLNA
020900120427     C                   else
021000120427     C                   seton                                        50
021100110927     C                   endif
021200120427     C*
021300120427     C                   endsl
021400110927     C*
021500120427     C                   else
021600120427     C                   seton                                        50
021700120427     C                   endif
021800160729     C                   endif
021900120427     C                   endif
022000080618     C*
022100080618     C* Scarico il buffer d output
022200120427     C  N50              exsr      WRIREC
022300031103     C*
022400031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
022500120427     C                   eval      stbFTR = 'T'
022600120427     C                   update    tistb000
022700991027     C*
022800120427     C     KEYstb03P     reade     tistb03l
022900120427     C                   setoff                                       50
023000120427     C                   enddo
023100120427     C                   endif
023200991027     C*
023300120427     C                   eval      wrkesito = '0'
023400991027     C*
023500910830     C                   ENDSR
023600031103
023700031103
023800031103
023900110103     C     wrirec        BEGSR
024000110928     C*
024100120427     C* Considero solamente le bolle originali
024200111103     C                   movel     *blanks       wEsito1           1
024300111103     C*
024400111103     C* Chiamata metodo GetLblTyp
024500111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
024600111103     C                                                stbAAS
024700111103     C                                               :stbLNP
024800111103     C                                               :stbNRS
024900111103     C                                               :stbNSP
025000111103     C                                               :pOutLblTyp
025100111103     C                                               :pOutAnnoBO
025200111103     C                                               :pOutLineaParBO
025300111103     C                                               :pOutSerieBO
025400111103     C                                               :pOutNumSpedBO
025500111103     C                                               :pOutDcmBO
025600111103     C                                               :pOutCcaBO
025700111103     C                                               :pOutRblBO))
025800111103     C*
025900120427     C                   if        pOutLblTyp = 'O' and
026000120427     C                             not *IN50                                    * ridondante
026100160826     C*
026200160826     C* Compongo il numero spedizione
026300160826     C                   eval      wNUMSPED = %editc(stbLNP:'X')+
026400160826     C                                        %editc(stbNRS:'X')+
026500160826     C                                        %editc(stbNSP:'X')
026600160520     C*
026700160520     C* Se richiesto nei parametri ritorno e compongo il link dirertto della sped. corrrente
026800160520     C                   if        w§PPILINK = 'S'
026900160927     C                   eval      oLinkDir = 'http://vas.brt.it/vas/' +
027000160520     C                                        'sped_det_show.hsm?nspediz='+
027100160826     C                                        wNUMSPED+'&lang='+w§PPILIN
027200160520     C                   endif
027300160520     C*
027400160520     C* Scarico il buffer di output così composto
027500160520     C                   eval      VAWDTA =
027600160520     C                             %trim(w§PPIDELTXT)+%trim('1')             +
027700160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
027800160826     C***                          %trim(w§PPIDELTXT)+%trim(stbRMA)          +
027900160826     C                             %trim(w§PPIDELTXT)+%trim(wNUMSPED)        +
028000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028100160520     C                             %trim(w§PPIDELTXT)+%trim(%char(stbRMN))   +
028200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028300160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
028400160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028500160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
028600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028700160524     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
028800160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028900160520     C                             %trim(w§PPIDELTXT)+%trim(oScanDatOra)     +
029000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029100160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
029200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029300160530     C***                          %trim(w§PPIDELTXT)+%trim(%char(w§PPIKSU)) +
029400160530     C                             %trim(w§PPIDELTXT)+%trim(%char(stbKSC))   +
029500160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029600160520     C                             %trim(w§PPIDELTXT)+%trim('738332')        +
029700160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029800160520     C                             %trim(w§PPIDELTXT)+%trim('BARTOLINI')     +
029900160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030000160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030100160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030200160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030300160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030400160520     C                             %trim(w§PPIDELTXT)+%trim(oLinkDir)        +
030500160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030600160524     C                             %trim(w§PPIDELTXT)+%trim(oScanEvent)      +
030700160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030800160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030900160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031000160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031100160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031200160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031300160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031400160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031500160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031600160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031700160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031800160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031900160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032000160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
032100160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032200160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
032300160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
032400160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
032500160520     C                             %trim(w§PPIDELTXT)
032600110927     C*
032700120427     C                   write     TIVAW000
032800120427     C                   endif
032900031103     C*
033000031103     C                   ENDSR
033100160520
033200160520
033300160520
033400160520     C     RTVDETT       BEGSR
033500160520     C*
033600160520     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
033700160520     C     KEYstbd3P     SETLL     tistbd3i
033800160520     C                   IF        %equal(tistbd3i)
033900160520     C     KEYstbd3P     READE     tistbd3i
034000160520     C                   DOW       not %eof(tistbd3i)
034100160520     C* Filtro unicamente i segnacolli "BRT"
034200160520     C                   IF        stbdTIS = 'BRT'
034300160524     C                   eval      oScanDatOra =
034400160524     C                              UBFMTDATE_Convert(
034500160524     C                                 %trim(%char(stbdDAD*1000000+stbDORD))  :
034600160524     C                                 'YYYYMMDDHHMISS' : 'YYYYMMDDHHMISSZ')
034700160520     C                   ENDIF
034800160520     C     KEYstbd3P     READE     tistbd3i
034900160520     C                   ENDDO
035000160520     C                   ENDIF
035100160520     C*
035200160520     C                   ENDSR
035300031103
035400031103
035500991027
035600991027      /TITLE Operazioni iniziali.
035700991027     C     *inzsr        BEGSR
035800991027     C*
035900991027     C     *ENTRY        PLIST
036000031103     C                   parm                    prmppi
036100991027     C     wrkesito      parm      wrkesito      prmesito
036200031103     C*
036300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
036400031103     C                   EVAL      DSPPI = prmppi
036500030709     C*
036600030709     C* Definizione chiavi
036700110927     C*
036800110927     C* Chiave su TABEL00F - Parziale
036900110927     C     KEYtab00P     KLIST
037000110927     C                   KFLD                    tblKUT
037100110927     C                   KFLD                    tblCOD
037200120427     C*
037300120427     C* Chiave su TNTBE00F - Parziale
037400120427     C     KEYtbe11P     KLIST
037500120427     C                   KFLD                    tbeLIN
037600120427     C                   KFLD                    tbeCOD
037700120427     C                   KFLD                    tbeKE1
037800030924     C*
037900110103     C* Chiave su TISTB03L - Parziale
038000110103     C     KEYstb03P     KLIST
038100031104     C                   KFLD                    wKstbKSU
038200031103     C                   KFLD                    wKstbFTR
038300031103     C*
038400031103     C* Chiave su TITAS30C - Parziale
038500031103     C     KEYtas30P     KLIST
038600031103     C                   KFLD                    stbAAS
038700031103     C                   KFLD                    stbLNP
038800031103     C                   KFLD                    stbNRS
038900031103     C                   KFLD                    stbNSP
039000160520     C*
039100160520     C* Chiave su TISTBD3I - Parziale
039200160520     C     KEYstbd3P     KLIST
039300160520     C                   KFLD                    wKstbFTR
039400160520     C                   KFLD                    stbKSU
039500160520     C                   KFLD                    stbAAS
039600160520     C                   KFLD                    stbLNP
039700160520     C                   KFLD                    stbNRS
039800160520     C                   KFLD                    stbNSP
039900160520     C                   KFLD                    stbDAS
040000160520     C                   KFLD                    stbORS
040100160520     C                   KFLD                    stbCOS
040200991027     C*
040300991027     C                   ENDSR
