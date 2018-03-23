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
001600120427     D skCAUDES        S             30    DIM(999)
001700120427     D jCAU            S              3  0 INZ(*zeros)
001800120427     D skPOBAR         S              3  0 DIM(999)
001900120427     D skPODES         S             20    DIM(999)
002000120427     D jPO             S              3  0 INZ(*zeros)
002100120427
002200030924
002300041015     D*-----------
002400041015     D* VARIABILI D WRK
002500041015     D*-----------
002600031103     D prmppi          S             50
002700030924     D prmesito        S              1
002800030924     D wrkesito        S                   like(prmesito)
002900110929
003000041015
003100041015     D*-----------
003200041015     D* DEFINIZIONE DS ESTERNE
003300041015     D*-----------
003400111116     D DS2Z          E DS
003500120427     D DICE          E DS
003600031103
003700031103     D*-----------
003800031103     D* VARIABILI RIFERITE AL DATA-BASE
003900031103     D*-----------
004000080618     D wKstbKSU        S                   like(stbKSU)
004100080618     D wKstbFTR        S                   like(stbFTR)
004200030924
004300110103
004400031103
004500031103     D*-----------
004600031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
004700031103     D*-----------
004800031103     D DSPPI           DS
004900080618     D  w§PPIKSU                      7  0
005000120427     D  w§PPILIN                      1
005100031103
005200110928
005300110928     D*------------------
005400110928     D* LINKING A DEFINIZIONI ESTERNE
005500110928     D*------------------
005600110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005700110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005800110928
005900030924
006000030325
006100030924     C* MAIN/
006200041015     C*
006300041015     C* Carica i dati tabellati
006400041015     C                   EXSR      cartab
006500041015     C*
006600041015     C* Esegue traduzione
006700991027     C                   EXSR      traduci
006800110929     C*
006900921023     C                   SETON                                        LR
007000111121
007100111121
007200111121
007300111121     C     cartab        BEGSR
007400111121     C*
007500120427     C* Carico causali evento 2Z e descrizioni in lingua (ICE)
007600120427     C                   clear                   skCAUBAR
007700120427     C                   clear                   skCAUDES
007800120427     C                   z-add     *zeros        jCAU
007900111121     C                   eval      tblKUT = 1
008000120427     C                   eval      tblCOD = '2Z'
008100111121     C     KEYtab00P     setll     tabel00f
008200111121     C     KEYtab00P     reade     tabel00f
008300111121     C                   dow       not %eof(tabel00f)
008400120427     C                   add       1             jCAU
008500120427     C                   movel(p)  tblUNI        DS2Z
008600120427     C                   eval      skCAUBAR(jCAU) = tblKEY
008700120427     C*
008800120427     C                   eval      tbeLIN = w§PPILIN
008900120427     C                   eval      tbeCOD = 'ICE'
009000120427     C                   eval      tbeKE1 = tblKEY
009100120427     C     KEYtbe11P     chain     tntbe11l
009200120427     C                   if        %found(tntbe11l)
009300120427     C                   eval      DICE = tbeUNI
009400120427     C                   eval      skCAUDES(jCAU) = §ICEDEP
009500120427     C                   else
009600120427     C                   eval      skCAUDES(jCAU) = '-'
009700120427     C                   endif
009800120427     C*
009900111121     C     KEYtab00P     reade     tabel00f
010000111121     C                   enddo
010100120427     C*
010200120427     C* Caricamento organigramma BAR
010300120427     C                   clear                   skPOBAR
010400120427     C                   clear                   skPODES
010500120427     C                   z-add     *zeros        jPO
010600120427     C     *loval        setll     azorg01l
010700120427     C                   read      azorg01l
010800120427     C                   dow       not %eof(azorg01l)
010900120427     C*
011000120427     C                   add       1             jPO
011100120427     C                   movel     orgFIL        skPOBAR(jPO)
011200120427     C                   movel     orgDES        skPODES(jPO)
011300111121     C*
011400120427     C                   read      azorg01l
011500120427     C                   enddo
011600120427     C*
011700111121     C                   ENDSR
011800991027
011900030924
012000030924
012100991027     C     traduci       BEGSR
012200031103     C*
012300031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
012400031103     C* di traduzione (trattasi d "post-traduzione")
012500120427     C                   eval      wKstbKSU = w§PPIKSU
012600120427     C                   eval      wKstbFTR = *blanks
012700120427     C     KEYstb03P     setll     tistb03l
012800120427     C                   if        %equal(tistb03l)
012900120427     C     KEYstb03P     reade     tistb03l
013000991027     C*
013100031103     C* Ciclo x tuttii record del cliente da trasmettere
013200120427     C                   dow       not %eof(tistb03l)
013300080618     C*
013400120427     C                   seton                                        50
013500031103     C*
013600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
013700031103     C* deve restituire al cliente nel file tradotto
013800120427     C                   if        stbFTR = *blanks                             * ridondante
013900110103     C*
014000120427     C                   setoff                                       50
014100031103     C*
014200031103     C* Innanzitutto chaino la bolla direttamente su TITAS
014300120427     C     KEYtas30P     chain     titas30c
014400120427     C                   if        %found(titas30c)
014500110103     C*
014600120427     C* Quindi aggancio l'estensione dettagli segnacolli
014700120427     C                   clear                   tahNOT
014800120427     C                   eval      tahTRC = 'C'
014900120427     C     KEYtah30P     chain     titah30c
015000110927     C*
015100120427     C                   z-add     *zeros        wPO               3 0
015200120427     C*
015300120427     C                   movel     *blanks       oParcelNbr      256
015400120427     C                   movel     *blanks       oScanEvent      256
015500120427     C                   movel     *blanks       oScanEventDes   256
015600120427     C                   movel     *blanks       oScanDptCode    256
015700120427     C                   movel     *blanks       oScanDptName    256
015800120427     C                   movel     *blanks       oEventDatTim    256
015900120427     C                   movel     *blanks       oConsignName    256
016000120427     C                   movel     *blanks       oConsignStrt    256
016100120427     C                   movel     *blanks       oConsignCntry   256
016200120427     C                   movel     *blanks       oConsignZip     256
016300120427     C                   movel     *blanks       oConsignCity    256
016400120427     C                   movel     *blanks       oWeight         256
016500120427     C                   movel     *blanks       oSenderRefID    256
016600120427     C*
016700120427     C* Innanzitutto valorizzo i dati "correnti"
016800120427     C                   eval      oParcelNbr    = tahNOT
016900120427     C                   eval      oEventDatTim  = %trim(%editc(stbDAS:'X'))+
017000120427     C                                             %trim(%editc(stbORS:'X'))
017100120427     C                   eval      oConsignName  = tasRSD
017200120427     C                   eval      oConsignStrt  = tasIND
017300120427     C                   eval      oConsignCntry = '380'
017400120427     C                   eval      oConsignZip   = tasCAD
017500120427     C                   eval      oConsignCity  = tasLOD
017600120427     C                   eval      oWeight       = %editw(tasPKB:'     0 , ')
017700120427     C                   eval      oSenderRefID  = %editc(tasRMN:'4')
017800120427     C*
017900120427     C                   select
018000120427     C*
018100120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
018200120427     C                   eval      oScanEvent = 'INCARICO'
018300120427     C                   eval      wPO = tasLNP
018400120427     C*
018500120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
018600120427     C                   eval      oScanEvent = 'ARRPRICOL'
018700120427     C                   eval      wPO = tasLNA
018800120427     C*
018900120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
019000120427     C                   eval      oScanEvent = 'RESO'
019100120427     C                   eval      wPO = tasLNA
019200120427     C*
019300120427     C                   when      %subst(stbCOS:1:6) = 'CONSOK'
019400120427     C                   eval      oScanEvent = 'CONSOK'
019500120427     C                   eval      wPO = tasLNA
019600120427     C*
019700120427     C                   other
019800120427     C                   movel     stbCOS        wCOS              3
019900120427     C                   z-add     1             jCAU
020000120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
020100120427     C                   if        %found
020200120427     C                   eval      oScanEvent    = skCAUBAR(jCAU)
020300120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
020400120427     C                   eval      wPO = tasLNA
020500120427     C                   else
020600120427     C                   seton                                        50
020700110927     C                   endif
020800120427     C*
020900120427     C                   endsl
021000120427     C*
021100120427     C                   z-add     1             jPO
021200120427     C     wPO           lookup    skPOBAR(jPO)                           55
021300120427     C                   if        %found
021400120427     C                   eval      oScanDptCode  = %editc(skPOBAR(jPO):'X')
021500120427     C                   eval      oScanDptName  = skPODES(jPO)
021600120427     C                   endif
021700110927     C*
021800120427     C                   else
021900120427     C                   seton                                        50
022000120427     C                   endif
022100120427     C                   endif
022200080618     C*
022300080618     C* Scarico il buffer d output
022400120427     C  N50              exsr      WRIREC
022500031103     C*
022600031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
022700120427     C                   eval      stbFTR = 'T'
022800120427     C                   update    tistb000
022900991027     C*
023000120427     C     KEYstb03P     reade     tistb03l
023100120427     C                   setoff                                       50
023200120427     C                   enddo
023300120427     C                   endif
023400991027     C*
023500120427     C                   eval      wrkesito = '0'
023600991027     C*
023700910830     C                   ENDSR
023800031103
023900031103
024000031103
024100110103     C     wrirec        BEGSR
024200110928     C*
024300120427     C* Considero solamente le bolle originali
024400111103     C                   movel     *blanks       wEsito1           1
024500111103     C*
024600111103     C* Chiamata metodo GetLblTyp
024700111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
024800111103     C                                                stbAAS
024900111103     C                                               :stbLNP
025000111103     C                                               :stbNRS
025100111103     C                                               :stbNSP
025200111103     C                                               :pOutLblTyp
025300111103     C                                               :pOutAnnoBO
025400111103     C                                               :pOutLineaParBO
025500111103     C                                               :pOutSerieBO
025600111103     C                                               :pOutNumSpedBO
025700111103     C                                               :pOutDcmBO
025800111103     C                                               :pOutCcaBO
025900111103     C                                               :pOutRblBO))
026000111103     C*
026100120427     C                   if        pOutLblTyp = 'O' and
026200120427     C                             not *IN50                                    * ridondante
026300031103     C*
026400031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
026500120427     C                   eval      VAWDTA =
026600120427     C                                    %trim(oParcelNbr)
026700120427     C                               +';'+%trim(oScanEvent)
026800120427     C                               +';'+%trim(oScanEventDes)
026900120427     C                               +';'+%trim(oScanDptCode)
027000120427     C                               +';'+%trim(oScanDptName)
027100120427     C                               +';'+%trim(oEventDatTim)
027200120427     C                               +';'+%trim(oConsignName)
027300120427     C                               +';'+%trim(oConsignStrt)
027400120427     C                               +';'+%trim(oConsignCntry)
027500120427     C                               +';'+%trim(oConsignZip)
027600120427     C                               +';'+%trim(oConsignCity)
027700120427     C                               +';'+%trim(oWeight)
027800120427     C                               +';'+%trim(oSenderRefID)
027900120427     C                               +';'
028000110927     C*
028100120427     C                   write     TIVAW000
028200120427     C                   endif
028300031103     C*
028400031103     C                   ENDSR
028500031103
028600031103
028700991027
028800991027      /TITLE Operazioni iniziali.
028900991027     C     *inzsr        BEGSR
029000991027     C*
029100991027     C     *ENTRY        PLIST
029200031103     C                   parm                    prmppi
029300991027     C     wrkesito      parm      wrkesito      prmesito
029400031103     C*
029500031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
029600031103     C                   EVAL      DSPPI = prmppi
029700030709     C*
029800030709     C* Definizione chiavi
029900110927     C*
030000110927     C* Chiave su TABEL00F - Parziale
030100110927     C     KEYtab00P     KLIST
030200110927     C                   KFLD                    tblKUT
030300110927     C                   KFLD                    tblCOD
030400120427     C*
030500120427     C* Chiave su TNTBE00F - Parziale
030600120427     C     KEYtbe11P     KLIST
030700120427     C                   KFLD                    tbeLIN
030800120427     C                   KFLD                    tbeCOD
030900120427     C                   KFLD                    tbeKE1
031000030924     C*
031100110103     C* Chiave su TISTB03L - Parziale
031200110103     C     KEYstb03P     KLIST
031300031104     C                   KFLD                    wKstbKSU
031400031103     C                   KFLD                    wKstbFTR
031500031103     C*
031600031103     C* Chiave su TITAS30C - Parziale
031700031103     C     KEYtas30P     KLIST
031800031103     C                   KFLD                    stbAAS
031900031103     C                   KFLD                    stbLNP
032000031103     C                   KFLD                    stbNRS
032100031103     C                   KFLD                    stbNSP
032200120427     C*
032300120427     C* Chiave su TITAH30C - Parziale
032400120427     C     KEYtah30P     KLIST
032500120427     C                   KFLD                    stbAAS
032600120427     C                   KFLD                    stbLNP
032700120427     C                   KFLD                    stbNRS
032800120427     C                   KFLD                    stbNSP
032900120427     C                   KFLD                    tahTRC
033000991027     C*
033100991027     C                   ENDSR
