000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  uF A E             DISK    commit
000600130503     FTITA430C  IF   E           K DISK
000700130503     FFNORM01L  IF   E           K DISK
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300130429     D wfdsc1ds      e ds                  extname(wfdsc00f1)
001400130503     D dta4m         e ds
001500060410     D wPKF            s              8
001600060410     D wVLF            s              6
001700051007     D wCAS            s             14
001800060410     D wIFT            s             14
001900991027
002000030908     C                   EXSR      chkRECFILMBR
002100991027     C                   EXSR      traduci
002200921023     C                   SETON                                        LR
002300030908
002400030908     C     chkRECFILMBR  BEGSR
002500030908     C*
002600060410     C                   READ      TIVAWWWT                               55
002700030908     C*
002800030908     C                   ENDSR
002900991027
003000991027     C     traduci       BEGSR
003100051007     C*
003200051007     C* Verifico se richiesto un particolare carattere d separatore campi
003300051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003400051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003500051007     C                   ELSE
003600051007     C                   MOVEL     ','           wSepCampi         1
003700051007     C                   ENDIF
003800051007     C*
003900051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004000051007     C                   SETON                                        60
004100051007     C                   IF        %subst(prmppt:4:1) = ','
004200051007     C                   SETOFF                                       60
004300051007     C                   ENDIF
004400030325     C*
004500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004700060410     C                   EVAL      VAWDTA  = '"DSCAAS"'+wSepCampi+
004800060410     C                                       '"DSCLNP"'+wSepCampi+
004900060410     C                                       '"DSCNRS"'+wSepCampi+
005000060410     C                                       '"DSCNSP"'+wSepCampi+
005100060410     C                                       '"DSCFNS"'+wSepCampi+
005200060410     C                                       '"DSCLNA"'+wSepCampi+
005300060410     C                                       '"DSCZNC"'+wSepCampi+
005400060410     C                                       '"DSCRSM"'+wSepCampi+
005500060410     C                                       '"DSCINM"'+wSepCampi+
005600060410     C                                       '"DSCCAM"'+wSepCampi+
005700060410     C                                       '"DSCLOM"'+wSepCampi+
005800060410     C                                       '"DSCPRM"'+wSepCampi+
005900060410     C                                       '"DSCFAP"'+wSepCampi+
006000060410     C                                       '"DSCNZM"'+wSepCampi+
006100060410     C                                       '"DSCRSD"'+wSepCampi+
006200060410     C                                       '"DSCIND"'+wSepCampi+
006300060410     C                                       '"DSCCAD"'+wSepCampi+
006400060410     C                                       '"DSCLOD"'+wSepCampi+
006500060410     C                                       '"DSCPRD"'+wSepCampi+
006600060410     C                                       '"DSCNZD"'+wSepCampi+
006700060410     C                                       '"DSCFIN"'+wSepCampi+
006800060410     C                                       '"DSCTSP"'+wSepCampi+
006900060410     C                                       '"DSCFBR"'+wSepCampi+
007000060410     C                                       '"DSCNCL"'+wSepCampi+
007100060410     C                                       '"DSCPKF"'+wSepCampi+
007200060410     C                                       '"DSCVLF"'+wSepCampi+
007300060410     C                                       '"DSCRMN"'+wSepCampi+
007400060410     C                                       '"DSCRMA"'+wSepCampi+
007500060410     C                                       '"DSCTCR"'+wSepCampi+
007600060410     C                                       '"DSCDCR"'+wSepCampi+
007700060410     C                                       '"DSCHCR"'+wSepCampi+
007800060410     C                                       '"DSCPDC"'+wSepCampi+
007900060410     C                                       '"DSCFGS"'+wSepCampi+
008000060410     C                                       '"DSCNDC"'+wSepCampi+
008100060410     C                                       '"DSCDDC"'+wSepCampi+
008200060410     C                                       '"DSCTC1"'+wSepCampi+
008300060410     C                                       '"DSCTC2"'+wSepCampi+
008400060410     C                                       '"DSCGGA"'+wSepCampi+
008500060410     C                                       '"DSCGMA"'+wSepCampi+
008600060410     C                                       '"DSCGVA"'+wSepCampi+
008700060410     C                                       '"DSCNGD"'+wSepCampi+
008800060410     C                                       '"DSCNSS"'+wSepCampi+
008900060410     C                                       '"DSCTIC"'+wSepCampi+
009000060410     C                                       '"DSCCAS"'+wSepCampi+
009100060410     C                                       '"DSCVCA"'+wSepCampi+
009200060410     C                                       '"DSCFIV"'+wSepCampi+
009300060410     C                                       '"DSCNFT"'+wSepCampi+
009400060410     C                                       '"DSCDFT"'+wSepCampi+
009500060410     C                                       '"DSCIFT"'+wSepCampi+
009600060410     C                                       '"DSCDIV"'+wSepCampi+
009700060410     C                                       '"DSCFLS"'+wSepCampi+
009800060410     C                                       '"DSCNCD"'+wSepCampi+
009900060410     C                                       '"DSCNCA"'+wSepCampi+
010000060410     C                                       '"DSCS01"'+wSepCampi+
010100060410     C                                       '"DSCS02"'+wSepCampi+
010200060410     C                                       '"DSCS03"'+wSepCampi+
010300060410     C                                       '"DSCS04"'+wSepCampi+
010400060410     C                                       '"DSCS05"'+wSepCampi+
010500060410     C                                       '"DSCS06"'+wSepCampi+
010600060410     C                                       '"DSCS07"'+wSepCampi+
010700060410     C                                       '"DSCS08"'+wSepCampi+
010800060410     C                                       '"DSCS09"'+wSepCampi+
010900060410     C                                       '"DSCS10"'+wSepCampi+
011000060410     C                                       '"DSCDGE"'+wSepCampi+
011100130503     C                                       '"DSCHMC"'+wSepCampi+
011200130503     C                                       '"DSCNO1"'+wSepCampi+
011300130503     C                                       '"DSCNO2"'+wSepCampi+
011400130503     C                                       '"DSCNAM"'+wSepCampi
011500130503     C*
011600060410     C                   WRITE     TIVAW000
011700030325     C                   ENDIF
011800991027     C*
011900060410     C                   READ      TIVGDTMP
012000060410     C                   DOW       not %eof(TIVGDTMP)
012100060411     C*
012200060411     C* Valorizzo il buffer dal campo flat di inout
012300130429     C                   EVAL      wfdsc1ds = vgdDTA
012400130503     C*
012500130503     C                   clear                   dscNO1
012600130503     C                   clear                   dscNO2
012700130503     C                   clear                   dscNAM
012800130503     C*
012900130503     C* Reperisco le note immesse sul ORM da internet
013000130503     C                   eval      ta4TRC = 'M'
013100130503     C     KEYta430_C    chain     tita430c
013200130503     C                   if        %found(tita430c)
013300130503     C                   eval      dta4m = ta4NOT
013400130503     C*
013500130503     C* Quindi aggancio l'orm legato
013600130503     C     KEYorm01_C    chain     fnorm01l
013700130503     C                   if        %found(fnorm01l)
013800130503     C                   eval      dscNO1 = ormNO1
013900130503     C                   eval      dscNO2 = ormNO2
014000130503     C                   eval      dscNAM = ormNAM
014100130503     C                   endif
014200130503     C                   endif
014300991027     C*
014400060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
014500130429     C***  '"':''''      XLATE     wfdsc1ds       wfdsc1ds
014600051007     C*
014700051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
014800060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
014900060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
015000060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
015100060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
015200060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
015300060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
015400060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
015500060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
015600051007     C*
015700051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
015800040206     C                   IF        %subst(prmppt:2:1) = 'D'
015900060411     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
016000060411     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
016100060411     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
016200060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
016300060411     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
016400060411     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
016500060411     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
016600060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
016700060411     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
016800060411     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
016900060411     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
017000060411     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
017100060411     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
017200060411     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
017300060411     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
017400060411     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
017500060411     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
017600060411     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
017700060411     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
017800060411     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
017900060411     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
018000060411     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
018100060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
018200060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
018300060411     C                                                    %trim(wPKF)+wSepCampi+
018400060411     C                                                    %trim(wVLF)+wSepCampi+
018500060411     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
018600060411     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
018700060411     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
018800060411     C                             %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
018900060411     C                                  %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
019000060411     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
019100060411     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
019200060411     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
019300060411     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
019400060411     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
019500060411     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
019600060411     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
019700060411     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
019800060411     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
019900060411     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
020000060411     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
020100060411     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
020200060411     C                                                    %trim(wCAS)+wSepCampi+
020300060411     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
020400060411     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
020500060411     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
020600060411     C                             %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
020700060411     C                                                    %trim(wIFT)+wSepCampi+
020800060411     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
020900060411     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
021000060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
021100060411     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
021200060411     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
021300060411     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
021400060411     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
021500060411     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
021600060411     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
021700060411     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
021800060411     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
021900060411     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
022000060411     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
022100060411     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
022200060411     C                             %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
022300130429     C                                  %trim(%editw(DSCHMC:'  :  '))+wSepCampi+
022400130429     C                                         '"'+%trim(DSCNO1)+'"' +wSepCampi+
022500130429     C                                         '"'+%trim(DSCNO2)+'"' +wSepCampi+
022600130429     C                                         '"'+%trim(DSCNAM)+'"' +wSepCampi
022700060411     C                   ELSE
022800060411     C*
022900060410     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
023000060410     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
023100060410     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
023200060410     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
023300060410     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
023400060410     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
023500060410     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
023600060410     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
023700060410     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
023800060410     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
023900060410     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
024000060410     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
024100060410     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
024200060410     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
024300060410     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
024400060410     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
024500060410     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
024600060410     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
024700060410     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
024800060410     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
024900060410     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
025000060410     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
025100060410     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
025200060410     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
025300060410     C                                                    %trim(wPKF)+wSepCampi+
025400060410     C                                                    %trim(wVLF)+wSepCampi+
025500060410     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
025600060410     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
025700060410     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
025800060410     C                                      %trim(%editc(DSCDCR:'Q'))+wSepCampi+
025900060410     C                                      %trim(%editc(DSCHCR:'Q'))+wSepCampi+
026000060410     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
026100060410     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
026200060410     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
026300060410     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
026400060410     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
026500060410     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
026600060410     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
026700060410     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
026800060410     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
026900060410     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
027000060410     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
027100060410     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
027200060410     C                                                    %trim(wCAS)+wSepCampi+
027300060410     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
027400060410     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
027500060410     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
027600060410     C                                      %trim(%editc(DSCDFT:'Q'))+wSepCampi+
027700060410     C                                                    %trim(wIFT)+wSepCampi+
027800060410     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
027900060410     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
028000060410     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
028100060410     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
028200060410     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
028300060410     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
028400060410     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
028500060410     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
028600060410     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
028700060410     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
028800060410     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
028900060410     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
029000060410     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
029100060410     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
029200060410     C                                      %trim(%editc(DSCDGE:'Q'))+wSepCampi+
029300130429     C                                      %trim(%editc(DSCHMC:'Q'))+wSepCampi+
029400130429     C                                         '"'+%trim(DSCNO1)+'"' +wSepCampi+
029500130429     C                                         '"'+%trim(DSCNO2)+'"' +wSepCampi+
029600130429     C                                         '"'+%trim(DSCNAM)+'"' +wSepCampi
029700040206     C*
029800040206     C                   ENDIF
029900930409     C*
030000060410     C                   WRITE     TIVAW000
030100060410     C                   DELETE    TIVGD000
030200991027     C*
030300060410     C                   READ      TIVGDTMP
030400030325     C                   ENDDO
030500991027     C*
030600030325     C                   EVAL      wrkesito = '0'
030700991027     C*
030800910830     C                   ENDSR
030900991027
031000991027      /TITLE Operazioni iniziali.
031100991027     C     *inzsr        BEGSR
031200991027     C*
031300991027     C     *ENTRY        PLIST
031400991027     C                   parm                    prmppt
031500991027     C     wrkesito      parm      wrkesito      prmesito
031600130503     C*
031700130503     C* Chiave su TITA430C - Completa
031800130503     C     KEYta430_C    KLIST
031900130503     C                   KFLD                    dscAAS
032000130503     C                   KFLD                    dscLNP
032100130503     C                   KFLD                    dscNRS
032200130503     C                   KFLD                    dscNSP
032300130503     C                   KFLD                    ta4TRC
032400130503     C*
032500130503     C* Chiave su FNORM01L - Completa
032600130503     C     KEYorm01_C    KLIST
032700130503     C                   KFLD                    §TA4MPOE
032800130503     C                   KFLD                    §TA4MNSR
032900130503     C                   KFLD                    §TA4MNOR
033000130503     C                   KFLD                    §TA4MNRV
033100991027     C*
033200991027     C                   ENDSR
