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
014100110103     C*
014200120427     C                   setoff                                       50
014300160520     C*
014400160520     C* Innanzitutto chaino la bolla direttamente su TITAS
014500160520     C     KEYtas30P     chain     titas30c
014600160520     C                   if        %found(titas30c)
014700120427     C*
014800160520     C                   z-add     *zeros        wPO               3 0
014900160520     C                   movel     *blanks       oScanEvent      256
015000160520     C                   movel     *blanks       oScanEventDes   256
015100160520     C                   movel     *zeros        oScanDatOra      14
015200160520     C                   movel     *blanks       oLinkDir        300
015300160520     C*
015400160520     C* Valorizzo subito la data/ora dello status corrente
015500160524     C                   eval      oScanDatOra =
015600160524     C                              UBFMTDATE_Convert(
015700160524     C                                 %trim(%char(stbDAS*10000+stbORS))+'00' :
015800160524     C                                 'YYYYMMDDHHMISS' : 'YYYYMMDDHHMISSZ')
015900120427     C*
016000120427     C* Innanzitutto valorizzo i dati "correnti"
016100120427     C                   select
016200120427     C*
016300120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
016400120427     C                   eval      oScanEvent = 'INCARICO'
016500120427     C                   eval      wPO = tasLNP
016600120427     C*
016700120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
016800120427     C                   eval      oScanEvent = 'ARRPRICOL'
016900120427     C                   eval      wPO = tasLNA
017000120427     C*
017100120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
017200120427     C                   eval      oScanEvent = 'RESO'
017300120427     C                   eval      wPO = tasLNA
017400120427     C*
017500120427     C                   when      %subst(stbCOS:1:6) = 'CONSOK'
017600120427     C                   eval      oScanEvent = 'CONSOK'
017700120427     C                   eval      wPO = tasLNA
017800160520     C*
017900160520     C                   when      %subst(stbCOS:1:9) = 'GIACNODIS'
018000160520     C                   eval      oScanEvent = 'GIACNODIS'
018100160520     C                   eval      wPO = tasLNA
018200160520     C*
018300160520     C                   when      %subst(stbCOS:1:9) = 'GIACSIDIS'
018400160520     C                   eval      oScanEvent = 'GIACSIDIS'
018500160520     C                   eval      wPO = tasLNA
018600160520     C*
018700160520     C                   when      %subst(stbCOS:1:9) = 'COLNOTAFF'
018800160520     C                   eval      oScanEvent = 'COLNOTAFF'
018900160520     C                   eval      wPO = tasLNP
019000160520     C*
019100160520     C                   when      %subst(stbCOS:1:9) = 'SPUNTNPG5'
019200160520     C                   eval      oScanEvent = 'SPUNTNPG5'
019300160520     C                   eval      wPO = tasLNP
019400160520     C                   exsr      RTVDETT
019500120427     C*
019600120427     C                   other
019700120427     C                   movel     stbCOS        wCOS              3
019800120427     C                   z-add     1             jCAU
019900120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
020000120427     C                   if        %found
020100120427     C                   eval      oScanEvent    = skCAUBAR(jCAU)
020200120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
020300120427     C                   eval      wPO = tasLNA
020400120427     C                   else
020500120427     C                   seton                                        50
020600110927     C                   endif
020700120427     C*
020800120427     C                   endsl
020900110927     C*
021000120427     C                   else
021100120427     C                   seton                                        50
021200120427     C                   endif
021300120427     C                   endif
021400080618     C*
021500080618     C* Scarico il buffer d output
021600120427     C  N50              exsr      WRIREC
021700031103     C*
021800031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
021900120427     C                   eval      stbFTR = 'T'
022000120427     C                   update    tistb000
022100991027     C*
022200120427     C     KEYstb03P     reade     tistb03l
022300120427     C                   setoff                                       50
022400120427     C                   enddo
022500120427     C                   endif
022600991027     C*
022700120427     C                   eval      wrkesito = '0'
022800991027     C*
022900910830     C                   ENDSR
023000031103
023100031103
023200031103
023300110103     C     wrirec        BEGSR
023400110928     C*
023500120427     C* Considero solamente le bolle originali
023600111103     C                   movel     *blanks       wEsito1           1
023700111103     C*
023800111103     C* Chiamata metodo GetLblTyp
023900111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
024000111103     C                                                stbAAS
024100111103     C                                               :stbLNP
024200111103     C                                               :stbNRS
024300111103     C                                               :stbNSP
024400111103     C                                               :pOutLblTyp
024500111103     C                                               :pOutAnnoBO
024600111103     C                                               :pOutLineaParBO
024700111103     C                                               :pOutSerieBO
024800111103     C                                               :pOutNumSpedBO
024900111103     C                                               :pOutDcmBO
025000111103     C                                               :pOutCcaBO
025100111103     C                                               :pOutRblBO))
025200111103     C*
025300120427     C                   if        pOutLblTyp = 'O' and
025400120427     C                             not *IN50                                    * ridondante
025500160520     C*
025600160520     C* Se richiesto nei parametri ritorno e compongo il link dirertto della sped. corrrente
025700160520     C                   if        w§PPILINK = 'S'
025800160927     C                   eval      oLinkDir = 'http://vas.brt.it/vas/' +
025900160520     C                                        'sped_det_show.hsm?nspediz='+
026000160520     C                                        %editc(stbLNP:'X')+
026100160520     C                                        %editc(stbNRS:'X')+
026200160520     C                                        %editc(stbNSP:'X')+
026300160520     C                                        '&lang='+w§PPILIN
026400160520     C                   endif
026500160520     C*
026600160520     C* Scarico il buffer di output così composto
026700160520     C                   eval      VAWDTA =
026800160520     C                             %trim(w§PPIDELTXT)+%trim('1')             +
026900160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
027000160520     C                             %trim(w§PPIDELTXT)+%trim(stbRMA)          +
027100160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
027200160520     C                             %trim(w§PPIDELTXT)+%trim(%char(stbRMN))   +
027300160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
027400160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
027500160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
027600160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
027700160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
027800160524     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
027900160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028000160520     C                             %trim(w§PPIDELTXT)+%trim(oScanDatOra)     +
028100160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028200160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
028300160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028400160530     C***                          %trim(w§PPIDELTXT)+%trim(%char(w§PPIKSU)) +
028500160530     C                             %trim(w§PPIDELTXT)+%trim(%char(stbKSC))   +
028600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028700160520     C                             %trim(w§PPIDELTXT)+%trim('738332')        +
028800160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
028900160520     C                             %trim(w§PPIDELTXT)+%trim('BARTOLINI')     +
029000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029100160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
029200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029300160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
029400160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029500160520     C                             %trim(w§PPIDELTXT)+%trim(oLinkDir)        +
029600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029700160524     C                             %trim(w§PPIDELTXT)+%trim(oScanEvent)      +
029800160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
029900160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030100160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030300160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030400160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030500160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030600160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030700160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
030800160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
030900160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031000160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031100160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031200160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031300160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031400160520     C                             %trim(w§PPIDELTXT)+w§PPISEPFLD+
031500160520     C                             %trim(w§PPIDELTXT)+%trim(' ')             +
031600160520     C                             %trim(w§PPIDELTXT)
031700110927     C*
031800120427     C                   write     TIVAW000
031900120427     C                   endif
032000031103     C*
032100031103     C                   ENDSR
032200160520
032300160520
032400160520
032500160520     C     RTVDETT       BEGSR
032600160520     C*
032700160520     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
032800160520     C     KEYstbd3P     SETLL     tistbd3i
032900160520     C                   IF        %equal(tistbd3i)
033000160520     C     KEYstbd3P     READE     tistbd3i
033100160520     C                   DOW       not %eof(tistbd3i)
033200160520     C* Filtro unicamente i segnacolli "BRT"
033300160520     C                   IF        stbdTIS = 'BRT'
033400160524     C                   eval      oScanDatOra =
033500160524     C                              UBFMTDATE_Convert(
033600160524     C                                 %trim(%char(stbdDAD*1000000+stbDORD))  :
033700160524     C                                 'YYYYMMDDHHMISS' : 'YYYYMMDDHHMISSZ')
033800160520     C                   ENDIF
033900160520     C     KEYstbd3P     READE     tistbd3i
034000160520     C                   ENDDO
034100160520     C                   ENDIF
034200160520     C*
034300160520     C                   ENDSR
034400031103
034500031103
034600991027
034700991027      /TITLE Operazioni iniziali.
034800991027     C     *inzsr        BEGSR
034900991027     C*
035000991027     C     *ENTRY        PLIST
035100031103     C                   parm                    prmppi
035200991027     C     wrkesito      parm      wrkesito      prmesito
035300031103     C*
035400031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
035500031103     C                   EVAL      DSPPI = prmppi
035600030709     C*
035700030709     C* Definizione chiavi
035800110927     C*
035900110927     C* Chiave su TABEL00F - Parziale
036000110927     C     KEYtab00P     KLIST
036100110927     C                   KFLD                    tblKUT
036200110927     C                   KFLD                    tblCOD
036300120427     C*
036400120427     C* Chiave su TNTBE00F - Parziale
036500120427     C     KEYtbe11P     KLIST
036600120427     C                   KFLD                    tbeLIN
036700120427     C                   KFLD                    tbeCOD
036800120427     C                   KFLD                    tbeKE1
036900030924     C*
037000110103     C* Chiave su TISTB03L - Parziale
037100110103     C     KEYstb03P     KLIST
037200031104     C                   KFLD                    wKstbKSU
037300031103     C                   KFLD                    wKstbFTR
037400031103     C*
037500031103     C* Chiave su TITAS30C - Parziale
037600031103     C     KEYtas30P     KLIST
037700031103     C                   KFLD                    stbAAS
037800031103     C                   KFLD                    stbLNP
037900031103     C                   KFLD                    stbNRS
038000031103     C                   KFLD                    stbNSP
038100160520     C*
038200160520     C* Chiave su TISTBD3I - Parziale
038300160520     C     KEYstbd3P     KLIST
038400160520     C                   KFLD                    wKstbFTR
038500160520     C                   KFLD                    stbKSU
038600160520     C                   KFLD                    stbAAS
038700160520     C                   KFLD                    stbLNP
038800160520     C                   KFLD                    stbNRS
038900160520     C                   KFLD                    stbNSP
039000160520     C                   KFLD                    stbDAS
039100160520     C                   KFLD                    stbORS
039200160520     C                   KFLD                    stbCOS
039300991027     C*
039400991027     C                   ENDSR
