000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400100225     FTITAS30C  IF   E           K DISK
000500060410     FTIVGDTMP  UF   E             DISK
000600090323     Ftivawwwt  uF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100030325
001200060504     D wfdscds       e ds                  extname(wfdsc00f)
001300060410     D wPKF            s              8
001400060410     D wVLF            s              6
001500051007     D wCAS            s             14
001600060410     D wIFT            s             14
001700991027
001800030908     C                   EXSR      chkRECFILMBR
001900991027     C                   EXSR      traduci
002000921023     C                   SETON                                        LR
002100030908
002200030908     C     chkRECFILMBR  BEGSR
002300030908     C*
002400060410     C                   READ      TIVAWWWT                               55
002500030908     C*
002600030908     C                   ENDSR
002700991027
002800991027     C     traduci       BEGSR
002900051007     C*
003000051007     C* Verifico se richiesto un particolare carattere d separatore campi
003100051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003200051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003300051007     C                   ELSE
003400051007     C                   MOVEL     ','           wSepCampi         1
003500051007     C                   ENDIF
003600051007     C*
003700051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003800051007     C                   SETON                                        60
003900051007     C                   IF        %subst(prmppt:4:1) = ','
004000051007     C                   SETOFF                                       60
004100051007     C                   ENDIF
004200030325     C*
004300030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004500100517     C***                EVAL      VAWDTA  = '"DSCAAS"'+wSepCampi+
004600100517     C***                                    '"DSCLNP"'+wSepCampi+
004700100517     C***                                    '"DSCNRS"'+wSepCampi+
004800100517     C***                                    '"DSCNSP"'+wSepCampi+
004900100517     C***                                    '"DSCFNS"'+wSepCampi+
005000100517     C***                                    '"DSCLNA"'+wSepCampi+
005100100517     C***                                    '"DSCZNC"'+wSepCampi+
005200100517     C***                                    '"DSCRSM"'+wSepCampi+
005300100517     C***                                    '"DSCINM"'+wSepCampi+
005400100517     C***                                    '"DSCCAM"'+wSepCampi+
005500100517     C***                                    '"DSCLOM"'+wSepCampi+
005600100517     C***                                    '"DSCPRM"'+wSepCampi+
005700100517     C***                                    '"DSCFAP"'+wSepCampi+
005800100517     C***                                    '"DSCNZM"'+wSepCampi+
005900100517     C***                                    '"DSCRSD"'+wSepCampi+
006000100517     C***                                    '"DSCIND"'+wSepCampi+
006100100517     C***                                    '"DSCCAD"'+wSepCampi+
006200100517     C***                                    '"DSCLOD"'+wSepCampi+
006300100517     C***                                    '"DSCPRD"'+wSepCampi+
006400100517     C***                                    '"DSCNZD"'+wSepCampi+
006500100517     C***                                    '"DSCFIN"'+wSepCampi+
006600100517     C***                                    '"DSCTSP"'+wSepCampi+
006700100517     C***                                    '"DSCFBR"'+wSepCampi+
006800100517     C***                                    '"DSCNCL"'+wSepCampi+
006900100517     C***                                    '"DSCPKF"'+wSepCampi+
007000100517     C***                                    '"DSCVLF"'+wSepCampi+
007100100517     C***                                    '"DSCRMN"'+wSepCampi+
007200100517     C***                                    '"DSCRMA"'+wSepCampi+
007300100517     C***                                    '"DSCTCR"'+wSepCampi+
007400100517     C***                                    '"DSCDCR"'+wSepCampi+
007500100517     C***                                    '"DSCHCR"'+wSepCampi+
007600100517     C***                                    '"DSCPDC"'+wSepCampi+
007700100517     C***                                    '"DSCFGS"'+wSepCampi+
007800100517     C***                                    '"DSCNDC"'+wSepCampi+
007900100517     C***                                    '"DSCDDC"'+wSepCampi+
008000100517     C***                                    '"DSCTC1"'+wSepCampi+
008100100517     C***                                    '"DSCTC2"'+wSepCampi+
008200100517     C***                                    '"DSCGGA"'+wSepCampi+
008300100517     C***                                    '"DSCGMA"'+wSepCampi+
008400100517     C***                                    '"DSCGVA"'+wSepCampi+
008500100517     C***                                    '"DSCNGD"'+wSepCampi+
008600100517     C***                                    '"DSCNSS"'+wSepCampi+
008700100517     C***                                    '"DSCTIC"'+wSepCampi+
008800100517     C***                                    '"DSCCAS"'+wSepCampi+
008900100517     C***                                    '"DSCVCA"'+wSepCampi+
009000100517     C***                                    '"DSCFIV"'+wSepCampi+
009100100517     C***                                    '"DSCNFT"'+wSepCampi+
009200100517     C***                                    '"DSCDFT"'+wSepCampi+
009300100517     C***                                    '"DSCIFT"'+wSepCampi+
009400100517     C***                                    '"DSCDIV"'+wSepCampi+
009500100517     C***                                    '"DSCFLS"'+wSepCampi+
009600100517     C***                                    '"DSCNCD"'+wSepCampi+
009700100517     C***                                    '"DSCNCA"'+wSepCampi+
009800100517     C***                                    '"DSCS01"'+wSepCampi+
009900100517     C***                                    '"DSCS02"'+wSepCampi+
010000100517     C***                                    '"DSCS03"'+wSepCampi+
010100100517     C***                                    '"DSCS04"'+wSepCampi+
010200100517     C***                                    '"DSCS05"'+wSepCampi+
010300100517     C***                                    '"DSCS06"'+wSepCampi+
010400100517     C***                                    '"DSCS07"'+wSepCampi+
010500100517     C***                                    '"DSCS08"'+wSepCampi+
010600100517     C***                                    '"DSCS09"'+wSepCampi+
010700100517     C***                                    '"DSCS10"'+wSepCampi+
010800100517     C***                                    '"DSCDGE"'+wSepCampi+
010900100517     C***                                    '"DSCHMC"'+wSepCampi+
011000100517     C***                                    '"DSCKSC"'
011100100517     C                   EVAL      VAWDTA  = '"DSCRSM"'+wSepCampi+
011200100517     C                                       '"DSCINM"'+wSepCampi+
011300100517     C                                       '"DSCCAM"'+wSepCampi+
011400100517     C                                       '"DSCLOM"'+wSepCampi+
011500100517     C                                       '"DSCPRM"'+wSepCampi+
011600100517     C                                       '"DSCRSD"'+wSepCampi+
011700100517     C                                       '"DSCKSC"'+wSepCampi+
011800100517     C                                       '"DSCNCL"'+wSepCampi+
011900120716     C                                       '"DSCRMA"'+wSepCampi+
012000120716     C                                       '"DSCRMN"'
012100030325     C*
012200060410     C                   WRITE     TIVAW000
012300030325     C                   ENDIF
012400991027     C*
012500060410     C                   READ      TIVGDTMP
012600060410     C                   DOW       not %eof(TIVGDTMP)
012700060411     C*
012800060411     C* Valorizzo il buffer dal campo flat di inout
012900060411     C                   EVAL      wfdscds = vgdDTA
013000991027     C*
013100060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
013200060411     C***  '"':''''      XLATE     wfdscds       wfdscds
013300100225     C*
013400100225     C* Innanzitutto chaino la bolla direttamente su TITAS
013500100225     C                   movel     *all'9'       wKSC              7
013600100225     C     KEYtas30P     chain     titas30c
013700100225     C                   if        %found(titas30c)
013800100225     C                   move(p)   tasKSC        wKSC
013900100225     C                   endif
014000051007     C*
014100051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
014200060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
014300060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
014400060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
014500060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
014600060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
014700060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
014800060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
014900060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
015000051007     C*
015100051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
015200100517     C***                IF        %subst(prmppt:2:1) = 'D'
015300100517     C***                EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
015400100517     C***                                   %trim(%editc(DSCLNP:'Q'))+wSepCampi+
015500100517     C***                                   %trim(%editc(DSCNRS:'Q'))+wSepCampi+
015600100517     C***                                   %trim(%editc(DSCNSP:'Q'))+wSepCampi+
015700100517     C***                                      '"'+%trim(DSCFNS)+'"' +wSepCampi+
015800100517     C***                                   %trim(%editc(DSCLNA:'Q'))+wSepCampi+
015900100517     C***                                   %trim(%editc(DSCZNC:'Q'))+wSepCampi+
016000100517     C***                                      '"'+%trim(DSCRSM)+'"' +wSepCampi+
016100100517     C***                                      '"'+%trim(DSCINM)+'"' +wSepCampi+
016200100517     C***                                      '"'+%trim(DSCCAM)+'"' +wSepCampi+
016300100517     C***                                      '"'+%trim(DSCLOM)+'"' +wSepCampi+
016400100517     C***                                      '"'+%trim(DSCPRM)+'"' +wSepCampi+
016500100517     C***                                      '"'+%trim(DSCFAP)+'"' +wSepCampi+
016600100517     C***                                      '"'+%trim(DSCNZM)+'"' +wSepCampi+
016700100517     C***                                      '"'+%trim(DSCRSD)+'"' +wSepCampi+
016800100517     C***                                      '"'+%trim(DSCIND)+'"' +wSepCampi+
016900100517     C***                                      '"'+%trim(DSCCAD)+'"' +wSepCampi+
017000100517     C***                                      '"'+%trim(DSCLOD)+'"' +wSepCampi+
017100100517     C***                                      '"'+%trim(DSCPRD)+'"' +wSepCampi+
017200100517     C***                                      '"'+%trim(DSCNZD)+'"' +wSepCampi+
017300100517     C***                                      '"'+%trim(DSCFIN)+'"' +wSepCampi+
017400100517     C***                                      '"'+%trim(DSCTSP)+'"' +wSepCampi+
017500100517     C***                                      '"'+%trim(DSCFBR)+'"' +wSepCampi+
017600100517     C***                                   %trim(%editc(DSCNCL:'Q'))+wSepCampi+
017700100517     C***                                                 %trim(wPKF)+wSepCampi+
017800100517     C***                                                 %trim(wVLF)+wSepCampi+
017900100517     C***                                   %trim(%editc(DSCRMN:'Q'))+wSepCampi+
018000100517     C***                                      '"'+%trim(DSCRMA)+'"' +wSepCampi+
018100100517     C***                                      '"'+%trim(DSCTCR)+'"' +wSepCampi+
018200100517     C***                          %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
018300100517     C***                               %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
018400100517     C***                                   %trim(%editc(DSCPDC:'Q'))+wSepCampi+
018500100517     C***                                   %trim(%editc(DSCFGS:'Q'))+wSepCampi+
018600100517     C***                                   %trim(%editc(DSCNDC:'Q'))+wSepCampi+
018700100517     C***                          %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
018800100517     C***                                      '"'+%trim(DSCTC1)+'"' +wSepCampi+
018900100517     C***                                      '"'+%trim(DSCTC2)+'"' +wSepCampi+
019000100517     C***                                      '"'+%trim(DSCGGA)+'"' +wSepCampi+
019100100517     C***                                      '"'+%trim(DSCGMA)+'"' +wSepCampi+
019200100517     C***                                      '"'+%trim(DSCGVA)+'"' +wSepCampi+
019300100517     C***                                   %trim(%editc(DSCNGD:'Q'))+wSepCampi+
019400100517     C***                                   %trim(%editc(DSCNSS:'Q'))+wSepCampi+
019500100517     C***                                      '"'+%trim(DSCTIC)+'"' +wSepCampi+
019600100517     C***                                                 %trim(wCAS)+wSepCampi+
019700100517     C***                                      '"'+%trim(DSCVCA)+'"' +wSepCampi+
019800100517     C***                                   %trim(%editc(DSCFIV:'Q'))+wSepCampi+
019900100517     C***                                   %trim(%editc(DSCNFT:'Q'))+wSepCampi+
020000100517     C***                          %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
020100100517     C***                                                 %trim(wIFT)+wSepCampi+
020200100517     C***                                      '"'+%trim(DSCDIV)+'"' +wSepCampi+
020300100517     C***                                   %trim(%editc(DSCFLS:'Q'))+wSepCampi+
020400100517     C***                                   %trim(%editc(DSCNCD:'Q'))+wSepCampi+
020500100517     C***                                   %trim(%editc(DSCNCA:'Q'))+wSepCampi+
020600100517     C***                                   %trim(%editc(DSCS01:'Q'))+wSepCampi+
020700100517     C***                                   %trim(%editc(DSCS02:'Q'))+wSepCampi+
020800100517     C***                                   %trim(%editc(DSCS03:'Q'))+wSepCampi+
020900100517     C***                                   %trim(%editc(DSCS04:'Q'))+wSepCampi+
021000100517     C***                                   %trim(%editc(DSCS05:'Q'))+wSepCampi+
021100100517     C***                                   %trim(%editc(DSCS06:'Q'))+wSepCampi+
021200100517     C***                                   %trim(%editc(DSCS07:'Q'))+wSepCampi+
021300100517     C***                                   %trim(%editc(DSCS08:'Q'))+wSepCampi+
021400100517     C***                                   %trim(%editc(DSCS09:'Q'))+wSepCampi+
021500100517     C***                                   %trim(%editc(DSCS10:'Q'))+wSepCampi+
021600100517     C***                          %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
021700100517     C***                          %trim(%editw(DSCHMC:'  :  '))     +wSepCampi+
021800100517     C***                                                 %trim(wKSC)
021900100517     C***                ELSE
022000060411     C*
022100100517     C***                EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
022200100517     C***                                   %trim(%editc(DSCLNP:'Q'))+wSepCampi+
022300100517     C***                                   %trim(%editc(DSCNRS:'Q'))+wSepCampi+
022400100517     C***                                   %trim(%editc(DSCNSP:'Q'))+wSepCampi+
022500100517     C***                                      '"'+%trim(DSCFNS)+'"' +wSepCampi+
022600100517     C***                                   %trim(%editc(DSCLNA:'Q'))+wSepCampi+
022700100517     C***                                   %trim(%editc(DSCZNC:'Q'))+wSepCampi+
022800100517     C***                                      '"'+%trim(DSCRSM)+'"' +wSepCampi+
022900100517     C***                                      '"'+%trim(DSCINM)+'"' +wSepCampi+
023000100517     C***                                      '"'+%trim(DSCCAM)+'"' +wSepCampi+
023100100517     C***                                      '"'+%trim(DSCLOM)+'"' +wSepCampi+
023200100517     C***                                      '"'+%trim(DSCPRM)+'"' +wSepCampi+
023300100517     C***                                      '"'+%trim(DSCFAP)+'"' +wSepCampi+
023400100517     C***                                      '"'+%trim(DSCNZM)+'"' +wSepCampi+
023500100517     C***                                      '"'+%trim(DSCRSD)+'"' +wSepCampi+
023600100517     C***                                      '"'+%trim(DSCIND)+'"' +wSepCampi+
023700100517     C***                                      '"'+%trim(DSCCAD)+'"' +wSepCampi+
023800100517     C***                                      '"'+%trim(DSCLOD)+'"' +wSepCampi+
023900100517     C***                                      '"'+%trim(DSCPRD)+'"' +wSepCampi+
024000100517     C***                                      '"'+%trim(DSCNZD)+'"' +wSepCampi+
024100100517     C***                                      '"'+%trim(DSCFIN)+'"' +wSepCampi+
024200100517     C***                                      '"'+%trim(DSCTSP)+'"' +wSepCampi+
024300100517     C***                                      '"'+%trim(DSCFBR)+'"' +wSepCampi+
024400100517     C***                                   %trim(%editc(DSCNCL:'Q'))+wSepCampi+
024500100517     C***                                                 %trim(wPKF)+wSepCampi+
024600100517     C***                                                 %trim(wVLF)+wSepCampi+
024700100517     C***                                   %trim(%editc(DSCRMN:'Q'))+wSepCampi+
024800100517     C***                                      '"'+%trim(DSCRMA)+'"' +wSepCampi+
024900100517     C***                                      '"'+%trim(DSCTCR)+'"' +wSepCampi+
025000100517     C***                                   %trim(%editc(DSCDCR:'Q'))+wSepCampi+
025100100517     C***                                   %trim(%editc(DSCHCR:'Q'))+wSepCampi+
025200100517     C***                                   %trim(%editc(DSCPDC:'Q'))+wSepCampi+
025300100517     C***                                   %trim(%editc(DSCFGS:'Q'))+wSepCampi+
025400100517     C***                                   %trim(%editc(DSCNDC:'Q'))+wSepCampi+
025500100517     C***                                   %trim(%editc(DSCDDC:'Q'))+wSepCampi+
025600100517     C***                                      '"'+%trim(DSCTC1)+'"' +wSepCampi+
025700100517     C***                                      '"'+%trim(DSCTC2)+'"' +wSepCampi+
025800100517     C***                                      '"'+%trim(DSCGGA)+'"' +wSepCampi+
025900100517     C***                                      '"'+%trim(DSCGMA)+'"' +wSepCampi+
026000100517     C***                                      '"'+%trim(DSCGVA)+'"' +wSepCampi+
026100100517     C***                                   %trim(%editc(DSCNGD:'Q'))+wSepCampi+
026200100517     C***                                   %trim(%editc(DSCNSS:'Q'))+wSepCampi+
026300100517     C***                                      '"'+%trim(DSCTIC)+'"' +wSepCampi+
026400100517     C***                                                 %trim(wCAS)+wSepCampi+
026500100517     C***                                      '"'+%trim(DSCVCA)+'"' +wSepCampi+
026600100517     C***                                   %trim(%editc(DSCFIV:'Q'))+wSepCampi+
026700100517     C***                                   %trim(%editc(DSCNFT:'Q'))+wSepCampi+
026800100517     C***                                   %trim(%editc(DSCDFT:'Q'))+wSepCampi+
026900100517     C***                                                 %trim(wIFT)+wSepCampi+
027000100517     C***                                      '"'+%trim(DSCDIV)+'"' +wSepCampi+
027100100517     C***                                   %trim(%editc(DSCFLS:'Q'))+wSepCampi+
027200100517     C***                                   %trim(%editc(DSCNCD:'Q'))+wSepCampi+
027300100517     C***                                   %trim(%editc(DSCNCA:'Q'))+wSepCampi+
027400100517     C***                                   %trim(%editc(DSCS01:'Q'))+wSepCampi+
027500100517     C***                                   %trim(%editc(DSCS02:'Q'))+wSepCampi+
027600100517     C***                                   %trim(%editc(DSCS03:'Q'))+wSepCampi+
027700100517     C***                                   %trim(%editc(DSCS04:'Q'))+wSepCampi+
027800100517     C***                                   %trim(%editc(DSCS05:'Q'))+wSepCampi+
027900100517     C***                                   %trim(%editc(DSCS06:'Q'))+wSepCampi+
028000100517     C***                                   %trim(%editc(DSCS07:'Q'))+wSepCampi+
028100100517     C***                                   %trim(%editc(DSCS08:'Q'))+wSepCampi+
028200100517     C***                                   %trim(%editc(DSCS09:'Q'))+wSepCampi+
028300100517     C***                                   %trim(%editc(DSCS10:'Q'))+wSepCampi+
028400100517     C***                                   %trim(%editc(DSCDGE:'Q'))+wSepCampi+
028500100517     C***                                   %trim(%editc(DSCHMC:'Q'))+wSepCampi+
028600100517     C***                                                 %trim(wKSC)
028700040206     C*
028800100517     C***                ENDIF
028900100517     C                   EVAL      VAWDTA =    '"'+%trim(DSCRSM)+'"' +wSepCampi+
029000100517     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
029100100517     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
029200100517     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
029300100517     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
029400100517     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
029500100517     C                                         '"'+%trim(wKSC)  +'"' +wSepCampi+
029600100517     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
029700120716     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
029800120716     C                                      %trim(%editc(DSCRMN:'Q'))
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
031600100225     C*
031700100225     C* Definizione chiavi
031800100225     C*
031900100225     C* Chiave su TITAS30C - Parziale
032000100225     C     KEYtas30P     KLIST
032100100225     C                   KFLD                    dscAAS
032200100225     C                   KFLD                    dscLNP
032300100225     C                   KFLD                    dscNRS
032400100225     C                   KFLD                    dscNSP
032500991027     C*
032600991027     C                   ENDSR
