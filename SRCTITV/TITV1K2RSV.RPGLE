000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200081117     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500060410     Ftivawwwt  uF A E             DISK
000600081117     FFIAR401L  if   e           k disk    usropn
000700081117     F                                     extfile(LibFileAR4)
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300081117     D LibFileAR4      s             21A   inz
001400081117     D currSysNeta     s              8A   inz('*NULL')
001500081117     D*
001600081117     D wfdscds       e ds                  extname(wfdsc00f)
001700070625     D*
001800060410     D wPKF            s              8
001900060410     D wVLF            s              6
002000051007     D wCAS            s             14
002100060410     D wIFT            s             14
002200081117
002300081117
002400081117     D*------------------
002500081117     D* PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
002600081117     D*------------------
002700081117     D  PiOutSysNeta   S              8A
002800081117
002900081117
003000081117     D*------------------
003100081117     D* LINKING A DEFINIZIONI ESTERNE
003200081117     D*------------------
003300081117     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003400081117
003500081117
003600991027
003700081117
003800081117
003900081117     C*
004000081117     C* Reperisco il sistema AS/400 corrente
004100081117     C                   callp     UBRTVNETA_Rtv(currSysNeta)
004200081117     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
004300081117     C                   eval      LibFileAR4 = 'FILTRA201/FIAR401L'
004400081117     C                   else
004500081117     C                   eval      LibFileAR4 = 'FILTRAPRD/FIAR401L'
004600081117     C                   endif
004700081117     C*
004800081117     C                   if        not %open(FIAR401L)
004900081117     C                   open      FIAR401L
005000081117     C                   endif
005100070625     C*
005200030908     C                   EXSR      chkRECFILMBR
005300991027     C                   EXSR      traduci
005400921023     C                   SETON                                        LR
005500030908
005600070625
005700070625
005800030908     C     chkRECFILMBR  BEGSR
005900030908     C*
006000060410     C                   READ      TIVAWWWT                               55
006100030908     C*
006200030908     C                   ENDSR
006300070625
006400070625
006500070625
006600081117     C     rtvNOTE       BEGSR
006700081117     C*
006800081117     C                   movel     *blanks       wNOTE_8          35
006900081117     C                   movel     *blanks       wNOTE_9          35
007000081117     C*
007100081117     C                   eval      ar4TRC = '8'
007200081117     C     KeyAR401_C    chain     fiar401l
007300081117     C                   if        %found(fiar401l)
007400081117     C                   eval      wNOTE_8 = ar4NOT
007500081117     C                   endif
007600081117     C*
007700081117     C                   eval      ar4TRC = '9'
007800081117     C     KeyAR401_C    chain     fiar401l
007900081117     C                   if        %found(fiar401l)
008000081117     C                   eval      wNOTE_9 = ar4NOT
008100081117     C                   endif
008200081117     C*
008300081117     C                   ENDSR
008400081117
008500081117
008600081117
008700991027     C     traduci       BEGSR
008800051007     C*
008900051007     C* Verifico se richiesto un particolare carattere d separatore campi
009000051007     C                   IF        %subst(prmppt:3:1) <> *blanks
009100051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
009200051007     C                   ELSE
009300051007     C                   MOVEL     ','           wSepCampi         1
009400051007     C                   ENDIF
009500051007     C*
009600051007     C* Verifico se richiesto un particolare carattere d separatore decimale
009700051007     C                   SETON                                        60
009800051007     C                   IF        %subst(prmppt:4:1) = ','
009900051007     C                   SETOFF                                       60
010000051007     C                   ENDIF
010100030325     C*
010200030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010300030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010400081117     C                   EVAL      VAWDTA  = '"VABAAS"'+wSepCampi+
010500081117     C                                       '"VABLNP"'+wSepCampi+
010600081117     C                                       '"VABNRS"'+wSepCampi+
010700081117     C                                       '"VABNSP"'+wSepCampi+
010800060410     C                                       '"DSCFNS"'+wSepCampi+
010900081117     C                                       '"VABLNA"'+wSepCampi+
011000081117     C                                       '"VABZNC"'+wSepCampi+
011100060410     C                                       '"DSCRSM"'+wSepCampi+
011200060410     C                                       '"DSCINM"'+wSepCampi+
011300060410     C                                       '"DSCCAM"'+wSepCampi+
011400060410     C                                       '"DSCLOM"'+wSepCampi+
011500060410     C                                       '"DSCPRM"'+wSepCampi+
011600060410     C                                       '"DSCFAP"'+wSepCampi+
011700060410     C                                       '"DSCNZM"'+wSepCampi+
011800081117     C                                       '"VABRSD"'+wSepCampi+
011900081117     C                                       '"VABIND"'+wSepCampi+
012000081117     C                                       '"VABCAD"'+wSepCampi+
012100081117     C                                       '"VABLOD"'+wSepCampi+
012200081117     C                                       '"VABPRD"'+wSepCampi+
012300081117     C                                       '"VABNZD"'+wSepCampi+
012400060410     C                                       '"DSCFIN"'+wSepCampi+
012500081117     C                                       '"VABTSP"'+wSepCampi+
012600060410     C                                       '"DSCFBR"'+wSepCampi+
012700081117     C                                       '"VABNCL"'+wSepCampi+
012800081117     C                                       '"VABPKF"'+wSepCampi+
012900081117     C                                       '"VABVLF"'+wSepCampi+
013000081117     C                                       '"VABRMN"'+wSepCampi+
013100081117     C                                       '"VABRMA"'+wSepCampi+
013200081117     C                                       '"VABTCR"'+wSepCampi+
013300081117     C                                       '"VABDCR"'+wSepCampi+
013400081117     C                                       '"VABHCR"'+wSepCampi+
013500060410     C                                       '"DSCPDC"'+wSepCampi+
013600060410     C                                       '"DSCFGS"'+wSepCampi+
013700060410     C                                       '"DSCNDC"'+wSepCampi+
013800060410     C                                       '"DSCDDC"'+wSepCampi+
013900081117     C                                       '"VABTC1"'+wSepCampi+
014000081117     C                                       '"VABTC2"'+wSepCampi+
014100081117     C                                       '"VABGGA"'+wSepCampi+
014200081117     C                                       '"VABGMA"'+wSepCampi+
014300081117     C                                       '"VABGVA"'+wSepCampi+
014400060410     C                                       '"DSCNGD"'+wSepCampi+
014500060410     C                                       '"DSCNSS"'+wSepCampi+
014600081117     C                                       '"VABTIC"'+wSepCampi+
014700081117     C                                       '"VABCAS"'+wSepCampi+
014800081117     C                                       '"VABVCA"'+wSepCampi+
014900060410     C                                       '"DSCFIV"'+wSepCampi+
015000060410     C                                       '"DSCNFT"'+wSepCampi+
015100060410     C                                       '"DSCDFT"'+wSepCampi+
015200060410     C                                       '"DSCIFT"'+wSepCampi+
015300060410     C                                       '"DSCDIV"'+wSepCampi+
015400060410     C                                       '"DSCFLS"'+wSepCampi+
015500060410     C                                       '"DSCNCD"'+wSepCampi+
015600060410     C                                       '"DSCNCA"'+wSepCampi+
015700060410     C                                       '"DSCS01"'+wSepCampi+
015800060410     C                                       '"DSCS02"'+wSepCampi+
015900060410     C                                       '"DSCS03"'+wSepCampi+
016000060410     C                                       '"DSCS04"'+wSepCampi+
016100060410     C                                       '"DSCS05"'+wSepCampi+
016200060410     C                                       '"DSCS06"'+wSepCampi+
016300060410     C                                       '"DSCS07"'+wSepCampi+
016400060410     C                                       '"DSCS08"'+wSepCampi+
016500060410     C                                       '"DSCS09"'+wSepCampi+
016600060410     C                                       '"DSCS10"'+wSepCampi+
016700060410     C                                       '"DSCDGE"'+wSepCampi+
016800070626     C                                       '"DSCHMC"'+wSepCampi+
016900081117     C                                       '"VABNOT"'+wSepCampi+
017000081117     C                                       '"VABNT2"'
017100030325     C*
017200060410     C                   WRITE     TIVAW000
017300030325     C                   ENDIF
017400991027     C*
017500060410     C                   READ      TIVGDTMP
017600060410     C                   DOW       not %eof(TIVGDTMP)
017700060411     C*
017800060411     C* Valorizzo il buffer dal campo flat di inout
017900081117     C                   EVAL      wfdscds = vgdDTA
018000081117     C*
018100081117     C* Reperisco le note bolla
018200081117     C                   EXSR      rtvNOTE
018300051007     C*
018400051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
018500060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
018600060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
018700060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
018800060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
018900060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
019000060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
019100060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
019200060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
019300051007     C*
019400051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
019500040206     C                   IF        %subst(prmppt:2:1) = 'D'
019600060411     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
019700060411     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
019800060411     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
019900060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
020000060411     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
020100060411     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
020200060411     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
020300060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
020400060411     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
020500060411     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
020600060411     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
020700060411     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
020800060411     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
020900060411     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
021000060411     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
021100060411     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
021200060411     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
021300060411     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
021400060411     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
021500060411     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
021600060411     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
021700060411     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
021800060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
021900060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
022000060411     C                                                    %trim(wPKF)+wSepCampi+
022100060411     C                                                    %trim(wVLF)+wSepCampi+
022200060411     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
022300060411     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
022400060411     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
022500060411     C                             %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
022600060411     C                                  %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
022700060411     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
022800060411     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
022900060411     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
023000060411     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
023100060411     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
023200060411     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
023300060411     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
023400060411     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
023500060411     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
023600060411     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
023700060411     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
023800060411     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
023900060411     C                                                    %trim(wCAS)+wSepCampi+
024000060411     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
024100060411     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
024200060411     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
024300060411     C                             %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
024400060411     C                                                    %trim(wIFT)+wSepCampi+
024500060411     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
024600060411     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
024700060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
024800060411     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
024900060411     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
025000060411     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
025100060411     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
025200060411     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
025300060411     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
025400060411     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
025500060411     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
025600060411     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
025700060411     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
025800060411     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
025900060411     C                             %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
026000070626     C                                  %trim(%editw(DSCHMC:'  :  '))+wSepCampi+
026100081117     C                                        '"'+%trim(wNOTE_8)+'"' +wSepCampi+
026200081117     C                                        '"'+%trim(wNOTE_9)+'"'
026300060411     C                   ELSE
026400060411     C*
026500060410     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
026600060410     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
026700060410     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
026800060410     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
026900060410     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
027000060410     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
027100060410     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
027200060410     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
027300060410     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
027400060410     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
027500060410     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
027600060410     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
027700060410     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
027800060410     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
027900060410     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
028000060410     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
028100060410     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
028200060410     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
028300060410     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
028400060410     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
028500060410     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
028600060410     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
028700060410     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
028800060410     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
028900060410     C                                                    %trim(wPKF)+wSepCampi+
029000060410     C                                                    %trim(wVLF)+wSepCampi+
029100060410     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
029200060410     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
029300060410     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
029400060410     C                                      %trim(%editc(DSCDCR:'Q'))+wSepCampi+
029500060410     C                                      %trim(%editc(DSCHCR:'Q'))+wSepCampi+
029600060410     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
029700060410     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
029800060410     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
029900060410     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
030000060410     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
030100060410     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
030200060410     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
030300060410     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
030400060410     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
030500060410     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
030600060410     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
030700060410     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
030800060410     C                                                    %trim(wCAS)+wSepCampi+
030900060410     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
031000060410     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
031100060410     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
031200060410     C                                      %trim(%editc(DSCDFT:'Q'))+wSepCampi+
031300060410     C                                                    %trim(wIFT)+wSepCampi+
031400060410     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
031500060410     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
031600060410     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
031700060410     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
031800060410     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
031900060410     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
032000060410     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
032100060410     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
032200060410     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
032300060410     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
032400060410     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
032500060410     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
032600060410     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
032700060410     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
032800060410     C                                      %trim(%editc(DSCDGE:'Q'))+wSepCampi+
032900070626     C                                      %trim(%editc(DSCHMC:'Q'))+wSepCampi+
033000081117     C                                        '"'+%trim(wNOTE_8)+'"' +wSepCampi+
033100081117     C                                        '"'+%trim(wNOTE_9)+'"'
033200040206     C*
033300040206     C                   ENDIF
033400930409     C*
033500060410     C                   WRITE     TIVAW000
033600060410     C                   DELETE    TIVGD000
033700991027     C*
033800060410     C                   READ      TIVGDTMP
033900030325     C                   ENDDO
034000991027     C*
034100030325     C                   EVAL      wrkesito = '0'
034200991027     C*
034300910830     C                   ENDSR
034400070625
034500081117
034600070625
034700991027
034800991027      /TITLE Operazioni iniziali.
034900991027     C     *inzsr        BEGSR
035000991027     C*
035100991027     C     *ENTRY        PLIST
035200991027     C                   parm                    prmppt
035300991027     C     wrkesito      parm      wrkesito      prmesito
035400070625     C*
035500070625     C* DEFINIZIONE CHIAVI
035600070625     C*
035700081117     C* Chiave su FIAR401L - Completa
035800081117     C     KeyAR401_C    klist
035900070625     C                   kfld                    dscAAS
036000070625     C                   kfld                    dscLNP
036100070625     C                   kfld                    dscNRS
036200070625     C                   kfld                    dscNSP
036300081117     C                   kfld                    ar4TRC
036400991027     C*
036500991027     C                   ENDSR
