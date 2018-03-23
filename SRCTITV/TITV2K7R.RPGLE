000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120427
000400110103     FAZORG01L  IF   E           K DISK
000500120427     FTNTBE11L  IF   E           K DISK
000600110927     FTABEL00F  IF   E           K DISK
000700031103     FTITAS30C  IF   E           K DISK
000800120427     FTITAH30C  IF   E           K DISK
000900110103     FTISTB03L  UF   E           K DISK    commit
001000090323     FTIVAWWWT  UF A E             DISK    commit
001100031103
001200031103     D*-----------
001300120427     D* SCHIERE
001400031103     D*-----------
001500120427     D skCAUBAR        S              3    DIM(999)
001600120427     D skCAUDPD        S              9    DIM(999)
001700120427     D skCAUDES        S             30    DIM(999)
001800120427     D jCAU            S              3  0 INZ(*zeros)
001900120427     D skPOBAR         S              3  0 DIM(999)
002000120427     D skPODPD         S             25    DIM(999)
002100120427     D skPODES         S             20    DIM(999)
002200120427     D jPO             S              3  0 INZ(*zeros)
002300120427
002400030924
002500041015     D*-----------
002600041015     D* VARIABILI D WRK
002700041015     D*-----------
002800031103     D prmppi          S             50
002900030924     D prmesito        S              1
003000030924     D wrkesito        S                   like(prmesito)
003100110929
003200041015
003300041015     D*-----------
003400041015     D* DEFINIZIONE DS ESTERNE
003500041015     D*-----------
003600111116     D DS2Z          E DS
003700120427     D DICE          E DS
003800120427     D OG143         E DS
003900031103
004000031103     D*-----------
004100031103     D* VARIABILI RIFERITE AL DATA-BASE
004200031103     D*-----------
004300080618     D wKstbKSU        S                   like(stbKSU)
004400080618     D wKstbFTR        S                   like(stbFTR)
004500030924
004600110103
004700031103
004800031103     D*-----------
004900031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005000031103     D*-----------
005100031103     D DSPPI           DS
005200080618     D  w§PPIKSU                      7  0
005300120427     D  w§PPILIN                      1
005400031103
005500110928
005600110928     D*------------------
005700110928     D* LINKING A DEFINIZIONI ESTERNE
005800110928     D*------------------
005900110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006000110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
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
008000120427     C                   clear                   skCAUDPD
008100120427     C                   clear                   skCAUDES
008200120427     C                   z-add     *zeros        jCAU
008300111121     C                   eval      tblKUT = 1
008400120427     C                   eval      tblCOD = '2Z'
008500111121     C     KEYtab00P     setll     tabel00f
008600111121     C     KEYtab00P     reade     tabel00f
008700111121     C                   dow       not %eof(tabel00f)
008800120427     C                   add       1             jCAU
008900120427     C                   movel(p)  tblUNI        DS2Z
009000120427     C                   eval      skCAUBAR(jCAU) = tblKEY
009100120427     C                   eval      skCAUDPD(jCAU) = §2ZEVD+§2ZCED
009200120427     C*
009300120427     C                   eval      tbeLIN = w§PPILIN
009400120427     C                   eval      tbeCOD = 'ICE'
009500120427     C                   eval      tbeKE1 = tblKEY
009600120427     C     KEYtbe11P     chain     tntbe11l
009700120427     C                   if        %found(tntbe11l)
009800120427     C                   eval      DICE = tbeUNI
009900120427     C                   eval      skCAUDES(jCAU) = §ICEDEP
010000120427     C                   else
010100120427     C                   eval      skCAUDES(jCAU) = '-'
010200120427     C                   endif
010300120427     C*
010400111121     C     KEYtab00P     reade     tabel00f
010500111121     C                   enddo
010600120427     C*
010700120427     C* Caricamento organigramma BAR e DPD
010800120427     C                   clear                   skPOBAR
010900120427     C                   clear                   skPODPD
011000120427     C                   clear                   skPODES
011100120427     C                   z-add     *zeros        jPO
011200120427     C     *loval        setll     azorg01l
011300120427     C                   read      azorg01l
011400120427     C                   dow       not %eof(azorg01l)
011500120427     C                   if        orgDE3 <> *blanks
011600120427     C*
011700120427     C                   add       1             jPO
011800120427     C*
011900120427     C                   movel     orgFIL        skPOBAR(jPO)
012000120427     C                   movel     orgDE3        skPODPD(jPO)
012100120427     C                   movel     orgDES        skPODES(jPO)
012200120427     C*
012300120427     C                   endif
012400111121     C*
012500120427     C                   read      azorg01l
012600120427     C                   enddo
012700120427     C*
012800111121     C                   ENDSR
012900991027
013000030924
013100030924
013200991027     C     traduci       BEGSR
013300031103     C*
013400031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
013500031103     C* di traduzione (trattasi d "post-traduzione")
013600120427     C                   eval      wKstbKSU = w§PPIKSU
013700120427     C                   eval      wKstbFTR = *blanks
013800120427     C     KEYstb03P     setll     tistb03l
013900120427     C                   if        %equal(tistb03l)
014000120427     C     KEYstb03P     reade     tistb03l
014100991027     C*
014200031103     C* Ciclo x tuttii record del cliente da trasmettere
014300120427     C                   dow       not %eof(tistb03l)
014400080618     C*
014500120427     C                   seton                                        50
014600031103     C*
014700031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
014800031103     C* deve restituire al cliente nel file tradotto
014900120427     C                   if        stbFTR = *blanks                             * ridondante
015000110103     C*
015100120427     C                   setoff                                       50
015200031103     C*
015300031103     C* Innanzitutto chaino la bolla direttamente su TITAS
015400120427     C     KEYtas30P     chain     titas30c
015500120427     C                   if        %found(titas30c)
015600110103     C*
015700120427     C* Quindi aggancio l'estensione dettagli segnacolli
015800120427     C                   clear                   tahNOT
015900120427     C                   eval      tahTRC = 'C'
016000120427     C     KEYtah30P     chain     titah30c
016100110927     C*
016200120427     C                   z-add     *zeros        wPO               3 0
016300120427     C*
016400120427     C                   movel     *blanks       oParcelNbr      256
016500120427     C                   movel     *blanks       oScanEvent      256
016600120427     C                   movel     *blanks       oException      256
016700120427     C                   movel     *blanks       oScanEventDes   256
016800120427     C                   movel     *blanks       oScanDptCode    256
016900120427     C                   movel     *blanks       oScanDptName    256
017000120427     C                   movel     *blanks       oEventDatTim    256
017100120427     C                   movel     *blanks       oConsignName    256
017200120427     C                   movel     *blanks       oConsignStrt    256
017300120427     C                   movel     *blanks       oConsignCntry   256
017400120427     C                   movel     *blanks       oConsignZip     256
017500120427     C                   movel     *blanks       oConsignCity    256
017600120427     C                   movel     *blanks       oAddService01   256
017700120427     C                   movel     *blanks       oAddService02   256
017800120427     C                   movel     *blanks       oAddService03   256
017900120427     C                   movel     *blanks       oWeight         256
018000120620     C                   movel     *zeros        oSenderRefID     14
018100120427     C                   movel     *blanks       oPOD_Image      256
018200120427     C                   movel     *blanks       oReceivrName    256
018300120427     C*
018400120427     C* Innanzitutto valorizzo i dati "correnti"
018500120427     C                   eval      oParcelNbr    = tahNOT
018600120427     C                   eval      oEventDatTim  = %trim(%editc(stbDAS:'X'))+
018700120427     C                                             %trim(%editc(stbORS:'X'))
018800120427     C                   eval      oConsignName  = tasRSD
018900120427     C                   eval      oConsignStrt  = tasIND
019000120427     C                   eval      oConsignCntry = '380'
019100120427     C                   eval      oConsignZip   = tasCAD
019200120427     C                   eval      oConsignCity  = tasLOD
019300120427     C                   eval      oWeight       = %editw(tasPKB:'     0 , ')
019400120620     C                   evalr     oSenderRefID  = %trim(%editc(tasRMN:'X'))
019500120427     C*
019600120427     C                   select
019700120427     C*
019800120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
019900120427     C                   eval      oScanEvent = '01'
020000120427     C                   eval      wPO = tasLNP
020100120427     C*
020200120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
020300120427     C                   eval      oScanEvent = '02'
020400120427     C                   eval      wPO = tasLNA
020500120427     C*
020600120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
020700120427     C                   eval      oScanEvent = '06'
020800120427     C                   eval      wPO = tasLNA
020900120427     C*
021000120620     C                   when      %trim(stbCOS) = 'CONSOK'
021100120427     C                   eval      oScanEvent = '13'
021200120427     C                   eval      wPO = tasLNA
021300120427     C*
021400120427     C                   other
021500120427     C                   movel     stbCOS        wCOS              3
021600120427     C                   z-add     1             jCAU
021700120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
021800120427     C                   if        %found and skCAUDPD(jCAU) <> *blanks
021900120427     C                   eval      oScanEvent    = %subst(skCAUDPD(jCAU):1:3)
022000120427     C                   eval      oException    = %subst(skCAUDPD(jCAU):4:6)
022100120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
022200120427     C                   eval      wPO = tasLNA
022300120427     C                   else
022400120427     C                   seton                                        50
022500110927     C                   endif
022600120427     C*
022700120427     C                   endsl
022800120427     C*
022900120427     C                   clear                   OG143
023000120427     C                   z-add     1             jPO
023100120427     C     wPO           lookup    skPOBAR(jPO)                           55
023200120427     C                   if        %found
023300120427     C                   movel     skPODPD(jPO)  OG143
023400120427     C                   eval      oScanDptCode  = §OGDP1
023500120427     C                   eval      oScanDptName  = skPODES(jPO)
023600120427     C                   endif
023700110927     C*
023800120427     C                   else
023900120427     C                   seton                                        50
024000120427     C                   endif
024100120427     C                   endif
024200080618     C*
024300080618     C* Scarico il buffer d output
024400120427     C  N50              exsr      WRIREC
024500031103     C*
024600031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
024700120427     C                   eval      stbFTR = 'T'
024800120427     C                   update    tistb000
024900991027     C*
025000120427     C     KEYstb03P     reade     tistb03l
025100120427     C                   setoff                                       50
025200120427     C                   enddo
025300120427     C                   endif
025400991027     C*
025500120427     C                   eval      wrkesito = '0'
025600991027     C*
025700910830     C                   ENDSR
025800031103
025900031103
026000031103
026100110103     C     wrirec        BEGSR
026200110928     C*
026300120427     C* Considero solamente le bolle originali
026400111103     C                   movel     *blanks       wEsito1           1
026500111103     C*
026600111103     C* Chiamata metodo GetLblTyp
026700111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
026800111103     C                                                stbAAS
026900111103     C                                               :stbLNP
027000111103     C                                               :stbNRS
027100111103     C                                               :stbNSP
027200111103     C                                               :pOutLblTyp
027300111103     C                                               :pOutAnnoBO
027400111103     C                                               :pOutLineaParBO
027500111103     C                                               :pOutSerieBO
027600111103     C                                               :pOutNumSpedBO
027700111103     C                                               :pOutDcmBO
027800111103     C                                               :pOutCcaBO
027900111103     C                                               :pOutRblBO))
028000111103     C*
028100120427     C                   if        pOutLblTyp = 'O' and
028200120427     C                             not *IN50                                    * ridondante
028300031103     C*
028400031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
028500120427     C                   eval      VAWDTA =
028600120427     C                                    %trim(oParcelNbr)
028700120427     C                               +';'+%trim(oScanEvent)
028800120427     C                               +';'+%trim(oException)
028900120427     C                               +';'+%trim(oScanEventDes)
029000120427     C                               +';'+%trim(oScanDptCode)
029100120427     C                               +';'+%trim(oScanDptName)
029200120427     C                               +';'+%trim(oEventDatTim)
029300120427     C                               +';'+%trim(oConsignName)
029400120427     C                               +';'+%trim(oConsignStrt)
029500120427     C                               +';'+%trim(oConsignCntry)
029600120427     C                               +';'+%trim(oConsignZip)
029700120427     C                               +';'+%trim(oConsignCity)
029800120427     C                               +';'+%trim(oAddService01)
029900120427     C                               +';'+%trim(oAddService02)
030000120427     C                               +';'+%trim(oAddService03)
030100120427     C                               +';'+%trim(oWeight)
030200120427     C                               +';'+%trim(oSenderRefID)
030300120427     C                               +';'+%trim(oPOD_Image)
030400120427     C                               +';'+%trim(oReceivrName)
030500120427     C                               +';'
030600110927     C*
030700120427     C                   write     TIVAW000
030800120427     C                   endif
030900031103     C*
031000031103     C                   ENDSR
031100031103
031200031103
031300991027
031400991027      /TITLE Operazioni iniziali.
031500991027     C     *inzsr        BEGSR
031600991027     C*
031700991027     C     *ENTRY        PLIST
031800031103     C                   parm                    prmppi
031900991027     C     wrkesito      parm      wrkesito      prmesito
032000031103     C*
032100031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
032200031103     C                   EVAL      DSPPI = prmppi
032300030709     C*
032400030709     C* Definizione chiavi
032500110927     C*
032600110927     C* Chiave su TABEL00F - Parziale
032700110927     C     KEYtab00P     KLIST
032800110927     C                   KFLD                    tblKUT
032900110927     C                   KFLD                    tblCOD
033000120427     C*
033100120427     C* Chiave su TNTBE00F - Parziale
033200120427     C     KEYtbe11P     KLIST
033300120427     C                   KFLD                    tbeLIN
033400120427     C                   KFLD                    tbeCOD
033500120427     C                   KFLD                    tbeKE1
033600030924     C*
033700110103     C* Chiave su TISTB03L - Parziale
033800110103     C     KEYstb03P     KLIST
033900031104     C                   KFLD                    wKstbKSU
034000031103     C                   KFLD                    wKstbFTR
034100031103     C*
034200031103     C* Chiave su TITAS30C - Parziale
034300031103     C     KEYtas30P     KLIST
034400031103     C                   KFLD                    stbAAS
034500031103     C                   KFLD                    stbLNP
034600031103     C                   KFLD                    stbNRS
034700031103     C                   KFLD                    stbNSP
034800120427     C*
034900120427     C* Chiave su TITAH30C - Parziale
035000120427     C     KEYtah30P     KLIST
035100120427     C                   KFLD                    stbAAS
035200120427     C                   KFLD                    stbLNP
035300120427     C                   KFLD                    stbNRS
035400120427     C                   KFLD                    stbNSP
035500120427     C                   KFLD                    tahTRC
035600991027     C*
035700991027     C                   ENDSR
