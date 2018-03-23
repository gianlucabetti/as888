000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400081110     FTITAS30C  IF   E           K DISK
000500081110     FTITAT30C  IF   E           K DISK
000600060410     FTIVGDTMP  UF   E             DISK
000700090323     Ftivawwwt  uF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300060504     D wfdscds       e ds                  extname(wfdsc00f)
001400060410     D wPKF            s              8
001500060410     D wVLF            s              6
001600051007     D wCAS            s             14
001700060410     D wIFT            s             14
001800991027
001900030908     C                   EXSR      chkRECFILMBR
002000991027     C                   EXSR      traduci
002100921023     C                   SETON                                        LR
002200030908
002300030908     C     chkRECFILMBR  BEGSR
002400030908     C*
002500060410     C                   READ      TIVAWWWT                               55
002600030908     C*
002700030908     C                   ENDSR
002800991027
002900991027     C     traduci       BEGSR
003000051007     C*
003100051007     C* Verifico se richiesto un particolare carattere d separatore campi
003200051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003300051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003400051007     C                   ELSE
003500051007     C                   MOVEL     ','           wSepCampi         1
003600051007     C                   ENDIF
003700051007     C*
003800051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003900051007     C                   SETON                                        60
004000051007     C                   IF        %subst(prmppt:4:1) = ','
004100051007     C                   SETOFF                                       60
004200051007     C                   ENDIF
004300030325     C*
004400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004600060410     C                   EVAL      VAWDTA  = '"DSCAAS"'+wSepCampi+
004700060410     C                                       '"DSCLNP"'+wSepCampi+
004800060410     C                                       '"DSCNRS"'+wSepCampi+
004900060410     C                                       '"DSCNSP"'+wSepCampi+
005000060410     C                                       '"DSCFNS"'+wSepCampi+
005100060410     C                                       '"DSCLNA"'+wSepCampi+
005200060410     C                                       '"DSCZNC"'+wSepCampi+
005300060410     C                                       '"DSCRSM"'+wSepCampi+
005400060410     C                                       '"DSCINM"'+wSepCampi+
005500060410     C                                       '"DSCCAM"'+wSepCampi+
005600060410     C                                       '"DSCLOM"'+wSepCampi+
005700060410     C                                       '"DSCPRM"'+wSepCampi+
005800060410     C                                       '"DSCFAP"'+wSepCampi+
005900060410     C                                       '"DSCNZM"'+wSepCampi+
006000060410     C                                       '"DSCRSD"'+wSepCampi+
006100060410     C                                       '"DSCIND"'+wSepCampi+
006200060410     C                                       '"DSCCAD"'+wSepCampi+
006300060410     C                                       '"DSCLOD"'+wSepCampi+
006400060410     C                                       '"DSCPRD"'+wSepCampi+
006500060410     C                                       '"DSCNZD"'+wSepCampi+
006600060410     C                                       '"DSCFIN"'+wSepCampi+
006700060410     C                                       '"DSCTSP"'+wSepCampi+
006800060410     C                                       '"DSCFBR"'+wSepCampi+
006900060410     C                                       '"DSCNCL"'+wSepCampi+
007000060410     C                                       '"DSCPKF"'+wSepCampi+
007100060410     C                                       '"DSCVLF"'+wSepCampi+
007200060410     C                                       '"DSCRMN"'+wSepCampi+
007300060410     C                                       '"DSCRMA"'+wSepCampi+
007400060410     C                                       '"DSCTCR"'+wSepCampi+
007500060410     C                                       '"DSCDCR"'+wSepCampi+
007600060410     C                                       '"DSCHCR"'+wSepCampi+
007700060410     C                                       '"DSCPDC"'+wSepCampi+
007800060410     C                                       '"DSCFGS"'+wSepCampi+
007900060410     C                                       '"DSCNDC"'+wSepCampi+
008000060410     C                                       '"DSCDDC"'+wSepCampi+
008100060410     C                                       '"DSCTC1"'+wSepCampi+
008200060410     C                                       '"DSCTC2"'+wSepCampi+
008300060410     C                                       '"DSCGGA"'+wSepCampi+
008400060410     C                                       '"DSCGMA"'+wSepCampi+
008500060410     C                                       '"DSCGVA"'+wSepCampi+
008600060410     C                                       '"DSCNGD"'+wSepCampi+
008700060410     C                                       '"DSCNSS"'+wSepCampi+
008800060410     C                                       '"DSCTIC"'+wSepCampi+
008900060410     C                                       '"DSCCAS"'+wSepCampi+
009000060410     C                                       '"DSCVCA"'+wSepCampi+
009100060410     C                                       '"DSCFIV"'+wSepCampi+
009200060410     C                                       '"DSCNFT"'+wSepCampi+
009300060410     C                                       '"DSCDFT"'+wSepCampi+
009400060410     C                                       '"DSCIFT"'+wSepCampi+
009500060410     C                                       '"DSCDIV"'+wSepCampi+
009600060410     C                                       '"DSCFLS"'+wSepCampi+
009700060410     C                                       '"DSCNCD"'+wSepCampi+
009800060410     C                                       '"DSCNCA"'+wSepCampi+
009900060410     C                                       '"DSCS01"'+wSepCampi+
010000060410     C                                       '"DSCS02"'+wSepCampi+
010100060410     C                                       '"DSCS03"'+wSepCampi+
010200060410     C                                       '"DSCS04"'+wSepCampi+
010300060410     C                                       '"DSCS05"'+wSepCampi+
010400060410     C                                       '"DSCS06"'+wSepCampi+
010500060410     C                                       '"DSCS07"'+wSepCampi+
010600060410     C                                       '"DSCS08"'+wSepCampi+
010700060410     C                                       '"DSCS09"'+wSepCampi+
010800060410     C                                       '"DSCS10"'+wSepCampi+
010900060410     C                                       '"DSCDGE"'+wSepCampi+
011000081110     C                                       '"DSCHMC"'+wSepCampi+
011100081110     C                                       '"SEGNAC"'
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
012400051007     C*
012500051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
012600060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
012700060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
012800060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
012900060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
013000060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
013100060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
013200060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
013300060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
013400051007     C*
013500051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
013600040206     C                   IF        %subst(prmppt:2:1) = 'D'
013700060411     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
013800060411     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
013900060411     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
014000060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
014100060411     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
014200060411     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
014300060411     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
014400060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
014500060411     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
014600060411     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
014700060411     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
014800060411     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
014900060411     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
015000060411     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
015100060411     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
015200060411     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
015300060411     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
015400060411     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
015500060411     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
015600060411     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
015700060411     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
015800060411     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
015900060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
016000060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
016100060411     C                                                    %trim(wPKF)+wSepCampi+
016200060411     C                                                    %trim(wVLF)+wSepCampi+
016300060411     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
016400060411     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
016500060411     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
016600060411     C                             %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
016700060411     C                                  %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
016800060411     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
016900060411     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
017000060411     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
017100060411     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
017200060411     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
017300060411     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
017400060411     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
017500060411     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
017600060411     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
017700060411     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
017800060411     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
017900060411     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
018000060411     C                                                    %trim(wCAS)+wSepCampi+
018100060411     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
018200060411     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
018300060411     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
018400060411     C                             %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
018500060411     C                                                    %trim(wIFT)+wSepCampi+
018600060411     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
018700060411     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
018800060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
018900060411     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
019000060411     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
019100060411     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
019200060411     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
019300060411     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
019400060411     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
019500060411     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
019600060411     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
019700060411     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
019800060411     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
019900060411     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
020000060411     C                             %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
020100060411     C                                      %trim(%editw(DSCHMC:'  :  '))
020200060411     C                   ELSE
020300060411     C*
020400060410     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
020500060410     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
020600060410     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
020700060410     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
020800060410     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
020900060410     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
021000060410     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
021100060410     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
021200060410     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
021300060410     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
021400060410     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
021500060410     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
021600060410     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
021700060410     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
021800060410     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
021900060410     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
022000060410     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
022100060410     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
022200060410     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
022300060410     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
022400060410     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
022500060410     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
022600060410     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
022700060410     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
022800060410     C                                                    %trim(wPKF)+wSepCampi+
022900060410     C                                                    %trim(wVLF)+wSepCampi+
023000060410     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
023100060410     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
023200060410     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
023300060410     C                                      %trim(%editc(DSCDCR:'Q'))+wSepCampi+
023400060410     C                                      %trim(%editc(DSCHCR:'Q'))+wSepCampi+
023500060410     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
023600060410     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
023700060410     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
023800060410     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
023900060410     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
024000060410     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
024100060410     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
024200060410     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
024300060410     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
024400060410     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
024500060410     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
024600060410     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
024700060410     C                                                    %trim(wCAS)+wSepCampi+
024800060410     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
024900060410     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
025000060410     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
025100060410     C                                      %trim(%editc(DSCDFT:'Q'))+wSepCampi+
025200060410     C                                                    %trim(wIFT)+wSepCampi+
025300060410     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
025400060410     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
025500060410     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
025600060410     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
025700060410     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
025800060410     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
025900060410     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
026000060410     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
026100060410     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
026200060410     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
026300060410     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
026400060410     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
026500060410     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
026600060410     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
026700060410     C                                      %trim(%editc(DSCDGE:'Q'))+wSepCampi+
026800060410     C                                      %trim(%editc(DSCHMC:'Q'))
026900040206     C*
027000040206     C                   ENDIF
027100081111     C*
027200081111     C* Salvo il campo buffer out
027300081111     C                   MOVEL     VAWDTA        wVAWDTA        1024
027400081110     C*
027500081110     C* Quindi verticalizzo il dettaglio segnacolli
027600081110     C*
027700081110     C* Aggancio innanzitutto la bolla
027800081110     C     KEYtasP       CHAIN     titas30c
027900081110     C                   if        %found(titas30c)
028000081110     C*
028100081110     C* Effettuo considerazioni su segnacolli sequenziali e nn
028200081110     C                   if        tasFNS = 'S'
028300081110     C*
028400081110     C* Se nn sequenziali aggancio il segnacollo Bartolini (da TITAT)
028500081110     C                   exsr      exeTITAT
028600081110     C*
028700081110     C                   else
028800081110     C*
028900081110     C* Se sequenziali effettuo sviluppo da segnacollo "da" x <n> colli
029000081110     C                   exsr      exeSEGNSEQ
029100081110     C*
029200081110     C                   endif
029300081110     C                   endif
029400930409     C*
029500060410     C                   DELETE    TIVGD000
029600991027     C*
029700060410     C                   READ      TIVGDTMP
029800030325     C                   ENDDO
029900991027     C*
030000030325     C                   EVAL      wrkesito = '0'
030100991027     C*
030200910830     C                   ENDSR
030300081110
030400081110
030500081110
030600081110
030700081110     C     exeTITAT      BEGSR
030800081110     C*
030900081110     C* Aggancio il segnacollo Bartolini
031000081110     C     KEYtasP       SETLL     titat30c
031100081110     C                   IF        %equal(titat30c)
031200081110     C     KEYtasP       READE     titat30c
031300081110     C                   DOW       not %eof(titat30c)
031400081110     C*
031500081110     C                   MOVE(P)   TATNSC        wAlfaNSC          7
031600081110     C*
031700081111     C                   EVAL      VAWDTA = %trim(wVAWDTA)+wSepCampi+
031800081110     C                                      %trim(wAlfaNSC)
031900081110     C*
032000081110     C                   WRITE     TIVAW000
032100081110     C     KEYtasP       READE     titat30c
032200081110     C                   ENDDO
032300081110     C                   ELSE
032400081111     C                   EVAL      VAWDTA = %trim(wVAWDTA)+wSepCampi
032500081110     C                   WRITE     TIVAW000
032600081110     C                   ENDIF
032700081110     C*
032800081110     C                   ENDSR
032900081110
033000081110
033100081110
033200081110     C     exeSEGNSEQ    BEGSR
033300081110     C*
033400081110     C                   z-add     tasNCD        wBartNcd          7 0
033500081110     C                   dow       wBartNcd <= tasNCD + tasNCL - 1
033600081110     C*
033700081110     C                   MOVE(P)   wBartNcd      wAlfaNCD          7
033800081110     C*
033900081111     C                   EVAL      VAWDTA = %trim(wVAWDTA)+wSepCampi+
034000081110     C                                      %trim(wAlfaNCD)
034100081110     C*
034200081110     C                   WRITE     TIVAW000
034300081110     C*
034400081110     C                   eval      wBartNcd = wBartNcd + 1
034500081110     C                   enddo
034600081110     C*
034700081110     C                   ENDSR
034800991027
034900081110
035000081110
035100991027      /TITLE Operazioni iniziali.
035200991027     C     *inzsr        BEGSR
035300991027     C*
035400991027     C     *ENTRY        PLIST
035500991027     C                   parm                    prmppt
035600991027     C     wrkesito      parm      wrkesito      prmesito
035700081110     C*
035800081110     C* Chiave su TITAS30C/TITAT30C - parziale
035900081110     C     KEYtasP       KLIST
036000081110     C                   KFLD                    dscAAS
036100081110     C                   KFLD                    dscLNP
036200081110     C                   KFLD                    dscNRS
036300081110     C                   KFLD                    dscNSP
036400991027     C*
036500991027     C                   ENDSR
