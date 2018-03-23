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
004500130516     C                   EVAL      VAWDTA  = '"DSCAAS"'+wSepCampi+
004600130516     C                                       '"DSCLNP"'+wSepCampi+
004700130516     C                                       '"DSCNRS"'+wSepCampi+
004800130516     C                                       '"DSCNSP"'+wSepCampi+
004900130516     C                                       '"DSCFNS"'+wSepCampi+
005000130516     C                                       '"DSCLNA"'+wSepCampi+
005100130516     C                                       '"DSCZNC"'+wSepCampi+
005200130516     C                                       '"DSCRSM"'+wSepCampi+
005300130516     C                                       '"DSCINM"'+wSepCampi+
005400130516     C                                       '"DSCCAM"'+wSepCampi+
005500130516     C                                       '"DSCLOM"'+wSepCampi+
005600130516     C                                       '"DSCPRM"'+wSepCampi+
005700130516     C                                       '"DSCFAP"'+wSepCampi+
005800130516     C                                       '"DSCNZM"'+wSepCampi+
005900130516     C                                       '"DSCRSD"'+wSepCampi+
006000130516     C                                       '"DSCIND"'+wSepCampi+
006100130516     C                                       '"DSCCAD"'+wSepCampi+
006200130516     C                                       '"DSCLOD"'+wSepCampi+
006300130516     C                                       '"DSCPRD"'+wSepCampi+
006400130516     C                                       '"DSCNZD"'+wSepCampi+
006500130516     C                                       '"DSCFIN"'+wSepCampi+
006600130516     C                                       '"DSCTSP"'+wSepCampi+
006700130516     C                                       '"DSCFBR"'+wSepCampi+
006800130516     C                                       '"DSCNCL"'+wSepCampi+
006900130516     C                                       '"DSCPKF"'+wSepCampi+
007000130516     C                                       '"DSCVLF"'+wSepCampi+
007100130516     C                                       '"DSCRMN"'+wSepCampi+
007200130516     C                                       '"DSCRMA"'+wSepCampi+
007300130516     C                                       '"DSCTCR"'+wSepCampi+
007400130516     C                                       '"DSCDCR"'+wSepCampi+
007500130516     C                                       '"DSCHCR"'+wSepCampi+
007600130516     C                                       '"DSCPDC"'+wSepCampi+
007700130516     C                                       '"DSCFGS"'+wSepCampi+
007800130516     C                                       '"DSCNDC"'+wSepCampi+
007900130516     C                                       '"DSCDDC"'+wSepCampi+
008000130516     C                                       '"DSCTC1"'+wSepCampi+
008100130516     C                                       '"DSCTC2"'+wSepCampi+
008200130516     C                                       '"DSCGGA"'+wSepCampi+
008300130516     C                                       '"DSCGMA"'+wSepCampi+
008400130516     C                                       '"DSCGVA"'+wSepCampi+
008500130516     C                                       '"DSCNGD"'+wSepCampi+
008600130516     C                                       '"DSCNSS"'+wSepCampi+
008700130516     C                                       '"DSCTIC"'+wSepCampi+
008800130516     C                                       '"DSCCAS"'+wSepCampi+
008900130516     C                                       '"DSCVCA"'+wSepCampi+
009000130516     C                                       '"DSCFIV"'+wSepCampi+
009100130516     C                                       '"DSCNFT"'+wSepCampi+
009200130516     C                                       '"DSCDFT"'+wSepCampi+
009300130516     C                                       '"DSCIFT"'+wSepCampi+
009400130516     C                                       '"DSCDIV"'+wSepCampi+
009500130516     C                                       '"DSCFLS"'+wSepCampi+
009600130516     C                                       '"DSCNCD"'+wSepCampi+
009700130516     C                                       '"DSCNCA"'+wSepCampi+
009800130516     C                                       '"DSCS01"'+wSepCampi+
009900130516     C                                       '"DSCS02"'+wSepCampi+
010000130516     C                                       '"DSCS03"'+wSepCampi+
010100130516     C                                       '"DSCS04"'+wSepCampi+
010200130516     C                                       '"DSCS05"'+wSepCampi+
010300130516     C                                       '"DSCS06"'+wSepCampi+
010400130516     C                                       '"DSCS07"'+wSepCampi+
010500130516     C                                       '"DSCS08"'+wSepCampi+
010600130516     C                                       '"DSCS09"'+wSepCampi+
010700130516     C                                       '"DSCS10"'+wSepCampi+
010800130516     C                                       '"DSCDGE"'+wSepCampi+
010900130516     C                                       '"DSCHMC"'+wSepCampi+
011000140711     C                                       '"DSCKSC"'+wSepCampi+
011100140711     C                                       '"DSCCBO"'
011200030325     C*
011300060410     C                   WRITE     TIVAW000
011400030325     C                   ENDIF
011500991027     C*
011600060410     C                   READ      TIVGDTMP
011700060410     C                   DOW       not %eof(TIVGDTMP)
011800060411     C*
011900060411     C* Valorizzo il buffer dal campo flat di inout
012000060411     C                   EVAL      wfdscds = vgdDTA
012100991027     C*
012200060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
012300060411     C***  '"':''''      XLATE     wfdscds       wfdscds
012400100225     C*
012500100225     C* Innanzitutto chaino la bolla direttamente su TITAS
012600140711     C                   movel     *blanks       wCBO              2
012700100225     C                   movel     *all'9'       wKSC              7
012800100225     C     KEYtas30P     chain     titas30c
012900100225     C                   if        %found(titas30c)
013000100225     C                   move(p)   tasKSC        wKSC
013100140711     C                   movel(p)  tasCBO        wCBO
013200100225     C                   endif
013300051007     C*
013400051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
013500060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
013600060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
013700060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
013800060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
013900060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
014000060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
014100060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
014200060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
014300130516     C*
014400130516     C* FORZATURA CLIENTE => solo dati in assegnato su di lui (reverse logistics)
014500131113     C                   IF        wKSC = %subst(prmppt:5:7) OR
014600131113     C                                    %subst(prmppt:5:7) = *blanks
014700051007     C*
014800051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
014900130516     C                   IF        %subst(prmppt:2:1) = 'D'
015000130516     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
015100130516     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
015200130516     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
015300130516     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
015400130516     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
015500130516     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
015600130516     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
015700130516     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
015800130516     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
015900130516     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
016000130516     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
016100130516     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
016200130516     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
016300130516     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
016400130516     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
016500130516     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
016600130516     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
016700130516     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
016800130516     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
016900130516     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
017000130516     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
017100130516     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
017200130516     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
017300130516     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
017400130516     C                                                    %trim(wPKF)+wSepCampi+
017500130516     C                                                    %trim(wVLF)+wSepCampi+
017600130516     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
017700130516     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
017800130516     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
017900130516     C                             %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
018000130516     C                                  %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
018100130516     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
018200130516     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
018300130516     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
018400130516     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
018500130516     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
018600130516     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
018700130516     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
018800130516     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
018900130516     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
019000130516     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
019100130516     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
019200130516     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
019300130516     C                                                    %trim(wCAS)+wSepCampi+
019400130516     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
019500130516     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
019600130516     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
019700130516     C                             %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
019800130516     C                                                    %trim(wIFT)+wSepCampi+
019900130516     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
020000130516     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
020100130516     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
020200130516     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
020300130516     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
020400130516     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
020500130516     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
020600130516     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
020700130516     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
020800130516     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
020900130516     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
021000130516     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
021100130516     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
021200130516     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
021300130516     C                             %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
021400130516     C                             %trim(%editw(DSCHMC:'  :  '))     +wSepCampi+
021500140711     C                                                    %trim(wKSC)+wSepCampi+
021600140711     C                                                    %trim(wCBO)
021700130516     C                   ELSE
021800060411     C*
021900130516     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
022000130516     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
022100130516     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
022200130516     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
022300130516     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
022400130516     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
022500130516     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
022600130516     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
022700130516     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
022800130516     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
022900130516     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
023000130516     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
023100130516     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
023200130516     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
023300130516     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
023400130516     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
023500130516     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
023600130516     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
023700130516     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
023800130516     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
023900130516     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
024000130516     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
024100130516     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
024200130516     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
024300130516     C                                                    %trim(wPKF)+wSepCampi+
024400130516     C                                                    %trim(wVLF)+wSepCampi+
024500130516     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
024600130516     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
024700130516     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
024800130516     C                                      %trim(%editc(DSCDCR:'Q'))+wSepCampi+
024900130516     C                                      %trim(%editc(DSCHCR:'Q'))+wSepCampi+
025000130516     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
025100130516     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
025200130516     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
025300130516     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
025400130516     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
025500130516     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
025600130516     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
025700130516     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
025800130516     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
025900130516     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
026000130516     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
026100130516     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
026200130516     C                                                    %trim(wCAS)+wSepCampi+
026300130516     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
026400130516     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
026500130516     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
026600130516     C                                      %trim(%editc(DSCDFT:'Q'))+wSepCampi+
026700130516     C                                                    %trim(wIFT)+wSepCampi+
026800130516     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
026900130516     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
027000130516     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
027100130516     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
027200130516     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
027300130516     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
027400130516     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
027500130516     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
027600130516     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
027700130516     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
027800130516     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
027900130516     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
028000130516     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
028100130516     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
028200130516     C                                      %trim(%editc(DSCDGE:'Q'))+wSepCampi+
028300130516     C                                      %trim(%editc(DSCHMC:'Q'))+wSepCampi+
028400140711     C                                                    %trim(wKSC)+wSepCampi+
028500140711     C                                                    %trim(wCBO)
028600040206     C*
028700130516     C                   ENDIF
028800930409     C*
028900060410     C                   WRITE     TIVAW000
029000130516     C*
029100130516     C                   ENDIF
029200130516     C*
029300060410     C                   DELETE    TIVGD000
029400991027     C*
029500060410     C                   READ      TIVGDTMP
029600030325     C                   ENDDO
029700991027     C*
029800030325     C                   EVAL      wrkesito = '0'
029900991027     C*
030000910830     C                   ENDSR
030100991027
030200130516
030300130516
030400991027      /TITLE Operazioni iniziali.
030500991027     C     *inzsr        BEGSR
030600991027     C*
030700991027     C     *ENTRY        PLIST
030800991027     C                   parm                    prmppt
030900991027     C     wrkesito      parm      wrkesito      prmesito
031000100225     C*
031100100225     C* Definizione chiavi
031200100225     C*
031300100225     C* Chiave su TITAS30C - Parziale
031400100225     C     KEYtas30P     KLIST
031500100225     C                   KFLD                    dscAAS
031600100225     C                   KFLD                    dscLNP
031700100225     C                   KFLD                    dscNRS
031800100225     C                   KFLD                    dscNSP
031900991027     C*
032000991027     C                   ENDSR
