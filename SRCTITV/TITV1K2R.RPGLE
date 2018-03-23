000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200081120     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400070625     Ftabel00f  if   e           k disk
000500070625     Ftitas30c  if   e           k disk
000600081120     Ftncsb03l  if   e           k disk
000700070625     Ftigcp51l  if   e           k disk
000800070625     Ffnlbl01l  if   e           k disk
000900070625     Ffnevb01l  if   e           k disk
001000060410     FTIVGDTMP  UF   E             DISK
001100060410     Ftivawwwt  uF A E             DISK
001200081120     FFIAR401L  if   e           k disk    usropn
001300081120     F                                     extfile(LibFileAR4)
001400991027
001500991027     D prmppt          s             50
001600991027     D prmesito        s              1
001700991027     D wrkesito        s                   like(prmesito)
001800030325
001900060504     D wfdscds       e ds                  extname(wfdsc00f)
002000070625     D ds2a          e ds
002100070625     D*
002200060410     D wPKF            s              8
002300060410     D wVLF            s              6
002400051007     D wCAS            s             14
002500060410     D wIFT            s             14
002600081120     D wPKB            s              8
002700081120     D wVLB            s              8
002800081120     D wCASB           s             14
002900070625     D*
003000070625     D wMOTIVO         s             75    inz(*blanks)
003100070625     D*------------
003200070625     D* SCHIERE DI WRK
003300070625     D*------------
003400080903     D sk2Alavgiac     s              3    dim(200)
003500080903     D sk2AlavgiacD    s             75    dim(200)
003600081120
003700081120
003800081120     D LibFileAR4      s             21A   inz
003900081120     D currSysNeta     s              8A   inz('*NULL')
004000081120
004100081120
004200081120     D*------------------
004300081120     D* PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
004400081120     D*------------------
004500081120     D  PiOutSysNeta   S              8A
004600081120
004700081120     D*------------------
004800081120     D* LINKING A DEFINIZIONI ESTERNE
004900081120     D*------------------
005000081120     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
005100081120
005200081120
005300081120
005400081120     D*------------------
005500081120     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
005600081120     D*------------------
005700081120     D InAnno          S              4S 0
005800081120     D InLineaPar      S              3S 0
005900081120     D InSerie         S              2S 0
006000081120     D InNumSped       S              7S 0
006100081120     D OutLblTyp       S              1A
006200081120     D OutAnnoBO       S              4S 0
006300081120     D OutLineaParBO   S              3S 0
006400081120     D OutSerieBO      S              2S 0
006500081120     D OutNumSpedBO    S              7S 0
006600081120     D OutDcmBO        S              8S 0
006700081120     D OutCcaBO        S              1A
006800081120     D OutRblBO        S              1A
006900081120     D OutAnnoFI       S              4S 0
007000081120     D OutLineaParFI   S              3S 0
007100081120     D OutSerieFI      S              2S 0
007200081120     D OutNumSpedFI    S              7S 0
007300081120     D OutDcmFI        S              8S 0
007400081120     D OutCcaFI        S              1A
007500081120
007600081120
007700081120     D*------------------
007800081120     D* LINKING A DEFINIZIONI ESTERNE
007900081120     D*------------------
008000081120     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008100081120
008200081120
008300991027
008400081120     C*
008500081120     C* Reperisco il sistema AS/400 corrente
008600081120     C                   callp     UBRTVNETA_Rtv(currSysNeta)
008700081120     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
008800081120     C                   eval      LibFileAR4 = 'FILTRA201/FIAR401L'
008900081120     C                   else
009000081120     C                   eval      LibFileAR4 = 'FILTRAPRD/FIAR401L'
009100081120     C                   endif
009200081120     C*
009300081120     C                   if        not %open(FIAR401L)
009400081120     C                   open      FIAR401L
009500081120     C                   endif
009600070625     C*
009700070625     C                   EXSR      cartab
009800030908     C                   EXSR      chkRECFILMBR
009900991027     C                   EXSR      traduci
010000921023     C                   SETON                                        LR
010100081120
010200070625
010300070625
010400030908     C     chkRECFILMBR  BEGSR
010500030908     C*
010600060410     C                   READ      TIVAWWWT                               55
010700030908     C*
010800030908     C                   ENDSR
010900081120
011000081120
011100081120
011200081120     C     rtvNOTE       BEGSR
011300081120     C*
011400081120     C                   movel     *blanks       wNOTE_8          35
011500081120     C                   movel     *blanks       wNOTE_9          35
011600081120     C*
011700081120     C                   eval      ar4TRC = '8'
011800081120     C     KeyAR401_C    chain     fiar401l
011900081120     C                   if        %found(fiar401l)
012000081120     C                   eval      wNOTE_8 = ar4NOT
012100081120     C                   endif
012200081120     C*
012300081120     C                   eval      ar4TRC = '9'
012400081120     C     KeyAR401_C    chain     fiar401l
012500081120     C                   if        %found(fiar401l)
012600081120     C                   eval      wNOTE_9 = ar4NOT
012700081120     C                   endif
012800081120     C*
012900081120     C                   ENDSR
013000070625
013100070625
013200070625
013300081120     C     rtvMOTIVO     BEGSR
013400070625     C*
013500070625     C                   movel     *blanks       wMOTIVO
013600070625     C*
013700070625     C* Effettuo le considerazioni solo se la bolla corrente è 1 bolla d reso
013800070625     C     KeyTAS30C     chain     titas30c
013900070625     C                   if        %found(titas30c)
014000070625     C                   if        tasFBR = 'S'
014100070626     C*
014200070626     C                   eval      wMOTIVO = 'NON SPECIFICATO MOTIVO'
014300070626     C*
014400070625     C* Dai legami reperisco la bolla originale
014500070625     C     KeyLBL01C     chain     fnlbl01l
014600070625     C                   if        %found(fnlbl01l)
014700070626     C*
014800070626     C* Verifico se occorsa effettiva apertura giacenza su bolla originale
014900070626     C                   eval      gcpFRG = *zeros
015000070626     C     KeyGCP51C     chain     tigcp51l
015100070626     C                   if        %found(tigcp51l)
015200070626     C                   z-add     1             i
015300070626     C     gcpCMC        lookup    sk2Alavgiac(i)                         20
015400070626     C                   if        *in20 = *on
015500070626     C                   eval      wMOTIVO = sk2AlavgiacD(i)
015600070626     C                   endif
015700070626     C*
015800070626     C                   else
015900070626     C*
016000070626     C* Se nn trovata giacenza aperta reperisco l'ultimo evento d tipo l.avv/giac sulla bolla orig.
016100070626     C     KeyEVB01P     setgt     fnevb01l
016200070626     C     KeyEVB01P     readpe    fnevb01l
016300070626     C                   dow       not %eof(fnevb01l)
016400070626     C*
016500070626     C                   z-add     1             i
016600070626     C     evbCEV        lookup    sk2Alavgiac(i)                         20
016700070626     C                   if        *in20 = *on
016800070626     C                   eval      wMOTIVO = sk2AlavgiacD(i)
016900070626     C                   leave
017000070626     C                   endif
017100070626     C     KeyEVB01P     readpe    fnevb01l
017200070626     C                   enddo
017300070626     C                   endif
017400070626     C*
017500070626     C                   endif
017600070625     C                   endif
017700070625     C                   endif
017800070625     C*
017900070625     C                   ENDSR
018000081120
018100081120
018200081120
018300081120     C     rtvBOLLAORI   BEGSR
018400081120     C*
018500081120     C* Chiamata metodo GetLblTyp
018600081120     C                   if        UBLBLSPE_GetLblTyp(dscAAS
018700081120     C                                               :dscLNP
018800081120     C                                               :dscNRS
018900081120     C                                               :dscNSP
019000081120     C                                               :OutLblTyp
019100081120     C                                               :OutAnnoBO
019200081120     C                                               :OutLineaParBO
019300081120     C                                               :OutSerieBO
019400081120     C                                               :OutNumSpedBO
019500081120     C                                               :OutDcmBO
019600081120     C                                               :OutCcaBO
019700081120     C                                               :OutRblBO) = 0
019800081120     C                   if        OutLblTyp <> 'O'
019900081120     C*
020000081120     C* Effettuo le considerazioni solo se la bolla corrente è 1 bolla originale
020100081120     C     KeyTAS30C_2   chain     titas30c
020200081120     C                   if        %found(titas30c)
020300081120     C*
020400081120     C* Aggancio quindi il contrassegno relativo alla bolla originale
020500081120     C     KeyCSB03C     chain     tncsb03l
020600081120     C                   if        %found(tncsb03l)
020700081126     C                   else
020800081126     C                   eval      csbcas = *zeros
020900081127     C                   eval      csbvca = *blanks
021000081120     C                   endif
021100081120     C                   endif
021200081120     C*
021300081120     C                   endif
021400081120     C                   endif
021500081120     C*
021600081120     C                   ENDSR
021700070625
021800070625
021900070625
022000991027     C     traduci       BEGSR
022100051007     C*
022200051007     C* Verifico se richiesto un particolare carattere d separatore campi
022300051007     C                   IF        %subst(prmppt:3:1) <> *blanks
022400051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
022500051007     C                   ELSE
022600051007     C                   MOVEL     ','           wSepCampi         1
022700051007     C                   ENDIF
022800051007     C*
022900051007     C* Verifico se richiesto un particolare carattere d separatore decimale
023000051007     C                   SETON                                        60
023100051007     C                   IF        %subst(prmppt:4:1) = ','
023200051007     C                   SETOFF                                       60
023300051007     C                   ENDIF
023400030325     C*
023500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
023600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
023700060410     C                   EVAL      VAWDTA  = '"DSCAAS"'+wSepCampi+
023800060410     C                                       '"DSCLNP"'+wSepCampi+
023900060410     C                                       '"DSCNRS"'+wSepCampi+
024000060410     C                                       '"DSCNSP"'+wSepCampi+
024100060410     C                                       '"DSCFNS"'+wSepCampi+
024200060410     C                                       '"DSCLNA"'+wSepCampi+
024300060410     C                                       '"DSCZNC"'+wSepCampi+
024400060410     C                                       '"DSCRSM"'+wSepCampi+
024500060410     C                                       '"DSCINM"'+wSepCampi+
024600060410     C                                       '"DSCCAM"'+wSepCampi+
024700060410     C                                       '"DSCLOM"'+wSepCampi+
024800060410     C                                       '"DSCPRM"'+wSepCampi+
024900060410     C                                       '"DSCFAP"'+wSepCampi+
025000060410     C                                       '"DSCNZM"'+wSepCampi+
025100060410     C                                       '"DSCRSD"'+wSepCampi+
025200060410     C                                       '"DSCIND"'+wSepCampi+
025300060410     C                                       '"DSCCAD"'+wSepCampi+
025400060410     C                                       '"DSCLOD"'+wSepCampi+
025500060410     C                                       '"DSCPRD"'+wSepCampi+
025600060410     C                                       '"DSCNZD"'+wSepCampi+
025700060410     C                                       '"DSCFIN"'+wSepCampi+
025800060410     C                                       '"DSCTSP"'+wSepCampi+
025900060410     C                                       '"DSCFBR"'+wSepCampi+
026000060410     C                                       '"DSCNCL"'+wSepCampi+
026100060410     C                                       '"DSCPKF"'+wSepCampi+
026200060410     C                                       '"DSCVLF"'+wSepCampi+
026300060410     C                                       '"DSCRMN"'+wSepCampi+
026400060410     C                                       '"DSCRMA"'+wSepCampi+
026500060410     C                                       '"DSCTCR"'+wSepCampi+
026600060410     C                                       '"DSCDCR"'+wSepCampi+
026700060410     C                                       '"DSCHCR"'+wSepCampi+
026800060410     C                                       '"DSCPDC"'+wSepCampi+
026900060410     C                                       '"DSCFGS"'+wSepCampi+
027000060410     C                                       '"DSCNDC"'+wSepCampi+
027100060410     C                                       '"DSCDDC"'+wSepCampi+
027200060410     C                                       '"DSCTC1"'+wSepCampi+
027300060410     C                                       '"DSCTC2"'+wSepCampi+
027400060410     C                                       '"DSCGGA"'+wSepCampi+
027500060410     C                                       '"DSCGMA"'+wSepCampi+
027600060410     C                                       '"DSCGVA"'+wSepCampi+
027700060410     C                                       '"DSCNGD"'+wSepCampi+
027800060410     C                                       '"DSCNSS"'+wSepCampi+
027900060410     C                                       '"DSCTIC"'+wSepCampi+
028000060410     C                                       '"DSCCAS"'+wSepCampi+
028100060410     C                                       '"DSCVCA"'+wSepCampi+
028200060410     C                                       '"DSCFIV"'+wSepCampi+
028300060410     C                                       '"DSCNFT"'+wSepCampi+
028400060410     C                                       '"DSCDFT"'+wSepCampi+
028500060410     C                                       '"DSCIFT"'+wSepCampi+
028600060410     C                                       '"DSCDIV"'+wSepCampi+
028700060410     C                                       '"DSCFLS"'+wSepCampi+
028800060410     C                                       '"DSCNCD"'+wSepCampi+
028900060410     C                                       '"DSCNCA"'+wSepCampi+
029000060410     C                                       '"DSCS01"'+wSepCampi+
029100060410     C                                       '"DSCS02"'+wSepCampi+
029200060410     C                                       '"DSCS03"'+wSepCampi+
029300060410     C                                       '"DSCS04"'+wSepCampi+
029400060410     C                                       '"DSCS05"'+wSepCampi+
029500060410     C                                       '"DSCS06"'+wSepCampi+
029600060410     C                                       '"DSCS07"'+wSepCampi+
029700060410     C                                       '"DSCS08"'+wSepCampi+
029800060410     C                                       '"DSCS09"'+wSepCampi+
029900060410     C                                       '"DSCS10"'+wSepCampi+
030000060410     C                                       '"DSCDGE"'+wSepCampi+
030100070626     C                                       '"DSCHMC"'+wSepCampi+
030200081120     C                                       '"CAUSALE RESO"'+wSepCampi+
030300081120     C                                       '"VABCCM"'+wSepCampi+
030400081120     C                                       '"VABLNP"'+wSepCampi+
030500081120     C                                       '"VABCTR"'+wSepCampi+
030600081120     C                                       '"VABRSD"'+wSepCampi+
030700081120     C                                       '"VABIND"'+wSepCampi+
030800081120     C                                       '"VABCAD"'+wSepCampi+
030900081120     C                                       '"VABLOD"'+wSepCampi+
031000081120     C                                       '"VABPRD"'+wSepCampi+
031100081120     C                                       '"VABNZD"'+wSepCampi+
031200081120     C                                       '"VABNCL"'+wSepCampi+
031300081120     C                                       '"VABPKB"'+wSepCampi+
031400081120     C                                       '"VABRMN"'+wSepCampi+
031500081120     C                                       '"VABRMA"'+wSepCampi+
031600081120     C                                       '"VABNOT"'+wSepCampi+
031700081120     C                                       '"VABNT2"'+wSepCampi+
031800081120     C                                       '"VABCAS"'+wSepCampi+
031900081120     C                                       '"VABVCA"'
032000030325     C*
032100060410     C                   WRITE     TIVAW000
032200030325     C                   ENDIF
032300991027     C*
032400060410     C                   READ      TIVGDTMP
032500060410     C                   DOW       not %eof(TIVGDTMP)
032600081126     C*
032700081126     C* Inizializzo i formati record
032800081126     C                   CLEAR                   wPKF
032900081126     C                   CLEAR                   wVLF
033000081126     C                   CLEAR                   wCAS
033100081126     C                   CLEAR                   wIFT
033200081126     C                   CLEAR                   wPKB
033300081126     C                   CLEAR                   wVLB
033400081126     C                   CLEAR                   wCASB
033500060411     C*
033600060411     C* Valorizzo il buffer dal campo flat di inout
033700060411     C                   EVAL      wfdscds = vgdDTA
033800070626     C*
033900070626     C* Se trattasi d bolla d reso ne reperisco il motivo
034000081120     C                   EXSR      rtvMOTIVO
034100081120     C*
034200081120     C* Reperisco la bolla originale
034300081120     C                   EXSR      rtvBOLLAORI
034400081120     C*
034500081120     C* Reperisco le note bolla
034600081120     C                   EXSR      rtvNOTE
034700991027     C*
034800060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
034900060411     C***  '"':''''      XLATE     wfdscds       wfdscds
035000051007     C*
035100051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
035200060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
035300060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
035400060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
035500060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
035600060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
035700060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
035800060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
035900060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
036000081120     C  N60              EVAL      wPKB = %trim(%editw(TASPKB:'    0 , '))
036100081120     C   60              EVAL      wPKB = %trim(%editw(TASPKB:'    0 . '))
036200081120     C  N60              EVAL      wVLB = %trim(%editw(TASVLB:'    0 , '))
036300081120     C   60              EVAL      wVLB = %trim(%editw(TASVLB:'    0 . '))
036400081120     C  N60              EVAL      wCASB= %trim(%editw(CSBCAS:'        0 ,   '))
036500081120     C   60              EVAL      wCASB= %trim(%editw(CSBCAS:'        0 .   '))
036600051007     C*
036700051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
036800040206     C                   IF        %subst(prmppt:2:1) = 'D'
036900060411     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
037000060411     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
037100060411     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
037200060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
037300060411     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
037400060411     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
037500060411     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
037600060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
037700060411     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
037800060411     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
037900060411     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
038000060411     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
038100060411     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
038200060411     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
038300060411     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
038400060411     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
038500060411     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
038600060411     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
038700060411     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
038800060411     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
038900060411     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
039000060411     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
039100060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
039200060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
039300060411     C                                                    %trim(wPKF)+wSepCampi+
039400060411     C                                                    %trim(wVLF)+wSepCampi+
039500060411     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
039600060411     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
039700060411     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
039800060411     C                             %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
039900060411     C                                  %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
040000060411     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
040100060411     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
040200060411     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
040300060411     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
040400060411     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
040500060411     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
040600060411     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
040700060411     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
040800060411     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
040900060411     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
041000060411     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
041100060411     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
041200060411     C                                                    %trim(wCAS)+wSepCampi+
041300060411     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
041400060411     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
041500060411     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
041600060411     C                             %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
041700060411     C                                                    %trim(wIFT)+wSepCampi+
041800060411     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
041900060411     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
042000060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
042100060411     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
042200060411     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
042300060411     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
042400060411     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
042500060411     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
042600060411     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
042700060411     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
042800060411     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
042900060411     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
043000060411     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
043100060411     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
043200060411     C                             %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
043300070626     C                                  %trim(%editw(DSCHMC:'  :  '))+wSepCampi+
043400081120     C                                        '"'+%trim(wMOTIVO)+'"'
043500081120     C* ... dati bolla ...
043600081120     C                                                               +wSepCampi+
043700081120     C                                            %editc(TASCCM:'X') +wSepCampi+
043800081120     C                                            %editc(TASLNP:'X') +wSepCampi+
043900081120     C                                            %editc(TASCTR:'X') +wSepCampi+
044000081120     C                                         '"'+%trim(TASRSD)+'"' +wSepCampi+
044100081120     C                                         '"'+%trim(TASIND)+'"' +wSepCampi+
044200081120     C                                         '"'+%trim(TASCAD)+'"' +wSepCampi+
044300081120     C                                         '"'+%trim(TASLOD)+'"' +wSepCampi+
044400081120     C                                         '"'+%trim(TASPRD)+'"' +wSepCampi+
044500081120     C                                         '"'+%trim(TASNZD)+'"' +wSepCampi+
044600081120     C                                      %trim(%editc(TASNCL:'Q'))+wSepCampi+
044700081120     C                                                    %trim(wPKB)+wSepCampi+
044800081120     C                                      %trim(%editc(TASRMN:'Q'))+wSepCampi+
044900081120     C                                                                      '"'+
045000081120     C                             %editc(DSCNCD:'X')+'-'+%editc(DSCNCA:'X')+
045100081120     C                                                                      '"'+
045200081120     C                                                                wSepCampi+
045300081120     C                                        '"'+%trim(wNOTE_8)+'"' +wSepCampi+
045400081120     C                                        '"'+%trim(wNOTE_9)+'"' +wSepCampi+
045500081120     C                                                   %trim(wCASB)+wSepCampi+
045600081120     C                                         '"'+%trim(CSBVCA)+'"' +wSepCampi
045700060411     C                   ELSE
045800060411     C*
045900060410     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
046000060410     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
046100060410     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
046200060410     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
046300060410     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
046400060410     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
046500060410     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
046600060410     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
046700060410     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
046800060410     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
046900060410     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
047000060410     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
047100060410     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
047200060410     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
047300060410     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
047400060410     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
047500060410     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
047600060410     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
047700060410     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
047800060410     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
047900060410     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
048000060410     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
048100060410     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
048200060410     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
048300060410     C                                                    %trim(wPKF)+wSepCampi+
048400060410     C                                                    %trim(wVLF)+wSepCampi+
048500060410     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
048600060410     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
048700060410     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
048800060410     C                                      %trim(%editc(DSCDCR:'Q'))+wSepCampi+
048900060410     C                                      %trim(%editc(DSCHCR:'Q'))+wSepCampi+
049000060410     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
049100060410     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
049200060410     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
049300060410     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
049400060410     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
049500060410     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
049600060410     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
049700060410     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
049800060410     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
049900060410     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
050000060410     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
050100060410     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
050200060410     C                                                    %trim(wCAS)+wSepCampi+
050300060410     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
050400060410     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
050500060410     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
050600060410     C                                      %trim(%editc(DSCDFT:'Q'))+wSepCampi+
050700060410     C                                                    %trim(wIFT)+wSepCampi+
050800060410     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
050900060410     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
051000060410     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
051100060410     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
051200060410     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
051300060410     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
051400060410     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
051500060410     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
051600060410     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
051700060410     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
051800060410     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
051900060410     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
052000060410     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
052100060410     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
052200060410     C                                      %trim(%editc(DSCDGE:'Q'))+wSepCampi+
052300070626     C                                      %trim(%editc(DSCHMC:'Q'))+wSepCampi+
052400070626     C                                        '"'+%trim(wMOTIVO)+'"'
052500081120     C* ... dati bolla ...
052600081120     C                                                               +wSepCampi+
052700081120     C                                            %editc(TASCCM:'X') +wSepCampi+
052800081120     C                                            %editc(TASLNP:'X') +wSepCampi+
052900081120     C                                            %editc(TASCTR:'X') +wSepCampi+
053000081120     C                                         '"'+%trim(TASRSD)+'"' +wSepCampi+
053100081120     C                                         '"'+%trim(TASIND)+'"' +wSepCampi+
053200081120     C                                         '"'+%trim(TASCAD)+'"' +wSepCampi+
053300081120     C                                         '"'+%trim(TASLOD)+'"' +wSepCampi+
053400081120     C                                         '"'+%trim(TASPRD)+'"' +wSepCampi+
053500081120     C                                         '"'+%trim(TASNZD)+'"' +wSepCampi+
053600081120     C                                      %trim(%editc(TASNCL:'Q'))+wSepCampi+
053700081120     C                                                    %trim(wPKB)+wSepCampi+
053800081120     C                                      %trim(%editc(TASRMN:'Q'))+wSepCampi+
053900081120     C                                                                      '"'+
054000081120     C                             %editc(DSCNCD:'X')+'-'+%editc(DSCNCA:'X')+
054100081120     C                                                                      '"'+
054200081120     C                                                                wSepCampi+
054300081120     C                                        '"'+%trim(wNOTE_8)+'"' +wSepCampi+
054400081120     C                                        '"'+%trim(wNOTE_9)+'"' +wSepCampi+
054500081120     C                                                   %trim(wCASB)+wSepCampi+
054600081120     C                                         '"'+%trim(CSBVCA)+'"' +wSepCampi
054700040206     C*
054800040206     C                   ENDIF
054900930409     C*
055000060410     C                   WRITE     TIVAW000
055100060410     C                   DELETE    TIVGD000
055200991027     C*
055300060410     C                   READ      TIVGDTMP
055400030325     C                   ENDDO
055500991027     C*
055600030325     C                   EVAL      wrkesito = '0'
055700991027     C*
055800910830     C                   ENDSR
055900070625
056000070625
056100070625
056200070625     C     cartab        BEGSR
056300070625     C*
056400070625     C                   clear                   sk2Alavgiac
056500070625     C                   clear                   sk2AlavgiacD
056600070625     C                   z-add     *zeros        i                 3 0
056700070625     C                   eval      tblKUT = 1
056800070625     C                   eval      tblCOD = '2A'
056900070625     C     KeyTABP       setll     tabel00f
057000070625     C     KeyTABP       reade     tabel00f
057100070625     C                   dow       not %eof(tabel00f)
057200070625     C                   movel(p)  tblUNI        ds2a
057300070625     C                   if        §2aFTC = 'G' or §2aFTC = 'A'
057400070625     C                   add       1             i
057500070625     C                   eval      sk2Alavgiac(i)  = tblKEY
057600070625     C                   eval      sk2AlavgiacD(i) = §2ADES
057700070625     C                   endif
057800070625     C     KeyTABP       reade     tabel00f
057900070625     C                   enddo
058000070625     C*
058100070625     C                   ENDSR
058200070625
058300070625
058400991027
058500991027      /TITLE Operazioni iniziali.
058600991027     C     *inzsr        BEGSR
058700991027     C*
058800991027     C     *ENTRY        PLIST
058900991027     C                   parm                    prmppt
059000991027     C     wrkesito      parm      wrkesito      prmesito
059100070625     C*
059200070625     C* DEFINIZIONE CHIAVI
059300070625     C*
059400070625     C* Chiave su TABEL00F - Parziale
059500070625     C     KeyTABP       klist
059600070625     C                   kfld                    tblKUT
059700070625     C                   kfld                    tblCOD
059800070625     C*
059900070625     C* Chiave su TITAS30C - Completa
060000070625     C     KeyTAS30C     klist
060100070625     C                   kfld                    dscAAS
060200070625     C                   kfld                    dscLNP
060300070625     C                   kfld                    dscNRS
060400070625     C                   kfld                    dscNSP
060500081120     C*
060600081120     C* Chiave su TITAS30C - Completa (2°)
060700081120     C     KeyTAS30C_2   klist
060800081120     C                   kfld                    OutAnnoBO
060900081120     C                   kfld                    OutLineaParBO
061000081120     C                   kfld                    OutSerieBO
061100081120     C                   kfld                    OutNumSpedBO
061200081120     C*
061300081120     C* Chiave su TNCSB03L - Completa
061400081120     C     KeyCSB03C     klist
061500081120     C                   kfld                    OutAnnoBO
061600081120     C                   kfld                    OutLineaParBO
061700081120     C                   kfld                    OutSerieBO
061800081120     C                   kfld                    OutNumSpedBO
061900070625     C*
062000070625     C* Chiave su TIGCP51L - Completa
062100070625     C     KeyGCP51C     klist
062200070626     C                   kfld                    lblAAO
062300070626     C                   kfld                    lblLPO
062400070626     C                   kfld                    lblNRO
062500070626     C                   kfld                    lblNSO
062600070625     C                   kfld                    gcpFRG
062700070625     C*
062800070625     C* Chiave su FNEVB01L - Parziale
062900070625     C     KeyEVB01P     klist
063000070626     C                   kfld                    lblAAO
063100070626     C                   kfld                    lblLPO
063200070626     C                   kfld                    lblNRO
063300070626     C                   kfld                    lblNSO
063400070625     C*
063500070625     C* Chiave su FNLBL01L - Completa
063600070626     C     KeyLBL01C     klist
063700070625     C                   kfld                    dscAAS
063800070625     C                   kfld                    dscLNP
063900070625     C                   kfld                    dscNRS
064000070625     C                   kfld                    dscNSP
064100081120     C*
064200081120     C* Chiave su FIAR401L - Completa
064300081120     C     KeyAR401_C    klist
064400081120     C                   kfld                    tasAAS
064500081120     C                   kfld                    tasLNP
064600081120     C                   kfld                    tasNRS
064700081120     C                   kfld                    tasNSP
064800081120     C                   kfld                    ar4TRC
064900991027     C*
065000991027     C                   ENDSR
