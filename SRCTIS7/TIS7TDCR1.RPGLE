000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  uF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000030325
001100060504     D wfdscds       e ds                  extname(wfdsc00f)
001200060410     D wPKF            s              8
001300060410     D wVLF            s              6
001400051007     D wCAS            s             14
001500060410     D wIFT            s             14
001600991027
001700030908     C                   EXSR      chkRECFILMBR
001800991027     C                   EXSR      traduci
001900921023     C                   SETON                                        LR
002000030908
002100030908     C     chkRECFILMBR  BEGSR
002200030908     C*
002300060410     C                   READ      TIVAWWWT                               55
002400030908     C*
002500030908     C                   ENDSR
002600991027
002700991027     C     traduci       BEGSR
002800051007     C*
002900051007     C* Verifico se richiesto un particolare carattere d separatore campi
003000051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003100051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003200051007     C                   ELSE
003300051007     C                   MOVEL     ','           wSepCampi         1
003400051007     C                   ENDIF
003500051007     C*
003600051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003700051007     C                   SETON                                        60
003800051007     C                   IF        %subst(prmppt:4:1) = ','
003900051007     C                   SETOFF                                       60
004000051007     C                   ENDIF
004100030325     C*
004200030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004300030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004400060410     C                   EVAL      VAWDTA  = '"DSCAAS"'+wSepCampi+
004500060410     C                                       '"DSCLNP"'+wSepCampi+
004600060410     C                                       '"DSCNRS"'+wSepCampi+
004700060410     C                                       '"DSCNSP"'+wSepCampi+
004800060410     C                                       '"DSCFNS"'+wSepCampi+
004900060410     C                                       '"DSCLNA"'+wSepCampi+
005000060410     C                                       '"DSCZNC"'+wSepCampi+
005100060410     C                                       '"DSCRSM"'+wSepCampi+
005200060410     C                                       '"DSCINM"'+wSepCampi+
005300060410     C                                       '"DSCCAM"'+wSepCampi+
005400060410     C                                       '"DSCLOM"'+wSepCampi+
005500060410     C                                       '"DSCPRM"'+wSepCampi+
005600060410     C                                       '"DSCFAP"'+wSepCampi+
005700060410     C                                       '"DSCNZM"'+wSepCampi+
005800060410     C                                       '"DSCRSD"'+wSepCampi+
005900060410     C                                       '"DSCIND"'+wSepCampi+
006000060410     C                                       '"DSCCAD"'+wSepCampi+
006100060410     C                                       '"DSCLOD"'+wSepCampi+
006200060410     C                                       '"DSCPRD"'+wSepCampi+
006300060410     C                                       '"DSCNZD"'+wSepCampi+
006400060410     C                                       '"DSCFIN"'+wSepCampi+
006500060410     C                                       '"DSCTSP"'+wSepCampi+
006600060410     C                                       '"DSCFBR"'+wSepCampi+
006700060410     C                                       '"DSCNCL"'+wSepCampi+
006800060410     C                                       '"DSCPKF"'+wSepCampi+
006900060410     C                                       '"DSCVLF"'+wSepCampi+
007000060410     C                                       '"DSCRMN"'+wSepCampi+
007100060410     C                                       '"DSCRMA"'+wSepCampi+
007200060410     C                                       '"DSCTCR"'+wSepCampi+
007300060410     C                                       '"DSCDCR"'+wSepCampi+
007400060410     C                                       '"DSCHCR"'+wSepCampi+
007500060410     C                                       '"DSCPDC"'+wSepCampi+
007600060410     C                                       '"DSCFGS"'+wSepCampi+
007700060410     C                                       '"DSCNDC"'+wSepCampi+
007800060410     C                                       '"DSCDDC"'+wSepCampi+
007900060410     C                                       '"DSCTC1"'+wSepCampi+
008000060410     C                                       '"DSCTC2"'+wSepCampi+
008100060410     C                                       '"DSCGGA"'+wSepCampi+
008200060410     C                                       '"DSCGMA"'+wSepCampi+
008300060410     C                                       '"DSCGVA"'+wSepCampi+
008400060410     C                                       '"DSCNGD"'+wSepCampi+
008500060410     C                                       '"DSCNSS"'+wSepCampi+
008600060410     C                                       '"DSCTIC"'+wSepCampi+
008700060410     C                                       '"DSCCAS"'+wSepCampi+
008800060410     C                                       '"DSCVCA"'+wSepCampi+
008900060410     C                                       '"DSCFIV"'+wSepCampi+
009000060410     C                                       '"DSCNFT"'+wSepCampi+
009100060410     C                                       '"DSCDFT"'+wSepCampi+
009200060410     C                                       '"DSCIFT"'+wSepCampi+
009300060410     C                                       '"DSCDIV"'+wSepCampi+
009400060410     C                                       '"DSCFLS"'+wSepCampi+
009500060410     C                                       '"DSCNCD"'+wSepCampi+
009600060410     C                                       '"DSCNCA"'+wSepCampi+
009700060410     C                                       '"DSCS01"'+wSepCampi+
009800060410     C                                       '"DSCS02"'+wSepCampi+
009900060410     C                                       '"DSCS03"'+wSepCampi+
010000060410     C                                       '"DSCS04"'+wSepCampi+
010100060410     C                                       '"DSCS05"'+wSepCampi+
010200060410     C                                       '"DSCS06"'+wSepCampi+
010300060410     C                                       '"DSCS07"'+wSepCampi+
010400060410     C                                       '"DSCS08"'+wSepCampi+
010500060410     C                                       '"DSCS09"'+wSepCampi+
010600060410     C                                       '"DSCS10"'+wSepCampi+
010700060410     C                                       '"DSCDGE"'+wSepCampi+
010800060410     C                                       '"DSCHMC"'
010900030325     C*
011000060410     C                   WRITE     TIVAW000
011100030325     C                   ENDIF
011200991027     C*
011300060410     C                   READ      TIVGDTMP
011400060410     C                   DOW       not %eof(TIVGDTMP)
011500060411     C*
011600060411     C* Valorizzo il buffer dal campo flat di inout
011700060411     C                   EVAL      wfdscds = vgdDTA
011800991027     C*
011900060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
012000060411     C***  '"':''''      XLATE     wfdscds       wfdscds
012100051007     C*
012200051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
012300060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
012400060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
012500060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
012600060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
012700060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
012800060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
012900060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
013000060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
013100051007     C*
013200051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
013300040206     C                   IF        %subst(prmppt:2:1) = 'D'
013400060411     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
013500060411     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
013600060411     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
013700060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
013800060411     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
013900060411     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
014000060411     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
014100060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
014200060411     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
014300060411     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
014400060411     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
014500060411     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
014600060411     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
014700060411     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
014800060411     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
014900060411     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
015000060411     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
015100060411     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
015200060411     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
015300060411     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
015400060411     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
015500060411     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
015600060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
015700060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
015800060411     C                                                    %trim(wPKF)+wSepCampi+
015900060411     C                                                    %trim(wVLF)+wSepCampi+
016000060411     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
016100060411     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
016200060411     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
016300060411     C                             %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
016400060411     C                                  %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
016500060411     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
016600060411     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
016700060411     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
016800060411     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
016900060411     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
017000060411     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
017100060411     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
017200060411     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
017300060411     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
017400060411     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
017500060411     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
017600060411     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
017700060411     C                                                    %trim(wCAS)+wSepCampi+
017800060411     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
017900060411     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
018000060411     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
018100060411     C                             %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
018200060411     C                                                    %trim(wIFT)+wSepCampi+
018300060411     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
018400060411     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
018500060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
018600060411     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
018700060411     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
018800060411     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
018900060411     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
019000060411     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
019100060411     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
019200060411     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
019300060411     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
019400060411     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
019500060411     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
019600060411     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
019700060411     C                             %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
019800060411     C                                      %trim(%editw(DSCHMC:'  :  '))
019900060411     C                   ELSE
020000060411     C*
020100060410     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
020200060410     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
020300060410     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
020400060410     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
020500060410     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
020600060410     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
020700060410     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
020800060410     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
020900060410     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
021000060410     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
021100060410     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
021200060410     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
021300060410     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
021400060410     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
021500060410     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
021600060410     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
021700060410     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
021800060410     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
021900060410     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
022000060410     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
022100060410     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
022200060410     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
022300060410     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
022400060410     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
022500060410     C                                                    %trim(wPKF)+wSepCampi+
022600060410     C                                                    %trim(wVLF)+wSepCampi+
022700060410     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
022800060410     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
022900060410     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
023000060410     C                                      %trim(%editc(DSCDCR:'Q'))+wSepCampi+
023100060410     C                                      %trim(%editc(DSCHCR:'Q'))+wSepCampi+
023200060410     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
023300060410     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
023400060410     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
023500060410     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
023600060410     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
023700060410     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
023800060410     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
023900060410     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
024000060410     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
024100060410     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
024200060410     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
024300060410     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
024400060410     C                                                    %trim(wCAS)+wSepCampi+
024500060410     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
024600060410     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
024700060410     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
024800060410     C                                      %trim(%editc(DSCDFT:'Q'))+wSepCampi+
024900060410     C                                                    %trim(wIFT)+wSepCampi+
025000060410     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
025100060410     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
025200060410     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
025300060410     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
025400060410     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
025500060410     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
025600060410     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
025700060410     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
025800060410     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
025900060410     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
026000060410     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
026100060410     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
026200060410     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
026300060410     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
026400060410     C                                      %trim(%editc(DSCDGE:'Q'))+wSepCampi+
026500060410     C                                      %trim(%editc(DSCHMC:'Q'))
026600040206     C*
026700040206     C                   ENDIF
026800930409     C*
026900060410     C                   WRITE     TIVAW000
027000060410     C                   DELETE    TIVGD000
027100991027     C*
027200060410     C                   READ      TIVGDTMP
027300030325     C                   ENDDO
027400991027     C*
027500030325     C                   EVAL      wrkesito = '0'
027600991027     C*
027700910830     C                   ENDSR
027800991027
027900991027      /TITLE Operazioni iniziali.
028000991027     C     *inzsr        BEGSR
028100991027     C*
028200991027     C     *ENTRY        PLIST
028300991027     C                   parm                    prmppt
028400991027     C     wrkesito      parm      wrkesito      prmesito
028500991027     C*
028600991027     C                   ENDSR
