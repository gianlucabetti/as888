000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400070625     Ftabel00f  if   e           k disk
000500070625     Ftitas30c  if   e           k disk
000600070625     Ftigcp51l  if   e           k disk
000700070625     Ffnlbl01l  if   e           k disk
000800070625     Ffnevb01l  if   e           k disk
000900060410     FTIVGDTMP  UF   E             DISK
001000090323     Ftivawwwt  uF A E             DISK    commit
001100991027
001200991027     D prmppt          s             50
001300991027     D prmesito        s              1
001400991027     D wrkesito        s                   like(prmesito)
001500030325
001600060504     D wfdscds       e ds                  extname(wfdsc00f)
001700070625     D ds2a          e ds
001800070625     D*
001900060410     D wPKF            s              8
002000060410     D wVLF            s              6
002100051007     D wCAS            s             14
002200060410     D wIFT            s             14
002300070625     D*
002400070625     D wMOTIVO         s             75    inz(*blanks)
002500070625     D*------------
002600070625     D* SCHIERE DI WRK
002700070625     D*------------
002800080903     D sk2Alavgiac     s              3    dim(200)
002900080903     D sk2AlavgiacD    s             75    dim(200)
003000991027
003100070625     C*
003200070625     C                   EXSR      cartab
003300030908     C                   EXSR      chkRECFILMBR
003400991027     C                   EXSR      traduci
003500921023     C                   SETON                                        LR
003600030908
003700070625
003800070625
003900030908     C     chkRECFILMBR  BEGSR
004000030908     C*
004100060410     C                   READ      TIVAWWWT                               55
004200030908     C*
004300030908     C                   ENDSR
004400070625
004500070625
004600070625
004700070625     C     rtvMOTIVO     BEGSR
004800070625     C*
004900070625     C                   movel     *blanks       wMOTIVO
005000070625     C*
005100070625     C* Effettuo le considerazioni solo se la bolla corrente è 1 bolla d reso
005200070625     C     KeyTAS30C     chain     titas30c
005300070625     C                   if        %found(titas30c)
005400070625     C                   if        tasFBR = 'S'
005500070626     C*
005600070626     C                   eval      wMOTIVO = 'NON SPECIFICATO MOTIVO'
005700070626     C*
005800070625     C* Dai legami reperisco la bolla originale
005900070625     C     KeyLBL01C     chain     fnlbl01l
006000070625     C                   if        %found(fnlbl01l)
006100070626     C*
006200070626     C* Verifico se occorsa effettiva apertura giacenza su bolla originale
006300070626     C                   eval      gcpFRG = *zeros
006400070626     C     KeyGCP51C     chain     tigcp51l
006500070626     C                   if        %found(tigcp51l)
006600070626     C                   z-add     1             i
006700070626     C     gcpCMC        lookup    sk2Alavgiac(i)                         20
006800070626     C                   if        *in20 = *on
006900070626     C                   eval      wMOTIVO = sk2AlavgiacD(i)
007000070626     C                   endif
007100070626     C*
007200070626     C                   else
007300070626     C*
007400070626     C* Se nn trovata giacenza aperta reperisco l'ultimo evento d tipo l.avv/giac sulla bolla orig.
007500070626     C     KeyEVB01P     setgt     fnevb01l
007600070626     C     KeyEVB01P     readpe    fnevb01l
007700070626     C                   dow       not %eof(fnevb01l)
007800070626     C*
007900070626     C                   z-add     1             i
008000070626     C     evbCEV        lookup    sk2Alavgiac(i)                         20
008100070626     C                   if        *in20 = *on
008200070626     C                   eval      wMOTIVO = sk2AlavgiacD(i)
008300070626     C                   leave
008400070626     C                   endif
008500070626     C     KeyEVB01P     readpe    fnevb01l
008600070626     C                   enddo
008700070626     C                   endif
008800070626     C*
008900070626     C                   endif
009000070625     C                   endif
009100070625     C                   endif
009200070625     C*
009300070625     C                   ENDSR
009400070625
009500070625
009600070625
009700991027     C     traduci       BEGSR
009800051007     C*
009900051007     C* Verifico se richiesto un particolare carattere d separatore campi
010000051007     C                   IF        %subst(prmppt:3:1) <> *blanks
010100051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
010200051007     C                   ELSE
010300051007     C                   MOVEL     ','           wSepCampi         1
010400051007     C                   ENDIF
010500051007     C*
010600051007     C* Verifico se richiesto un particolare carattere d separatore decimale
010700051007     C                   SETON                                        60
010800051007     C                   IF        %subst(prmppt:4:1) = ','
010900051007     C                   SETOFF                                       60
011000051007     C                   ENDIF
011100030325     C*
011200030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
011300030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
011400060410     C                   EVAL      VAWDTA  = '"DSCAAS"'+wSepCampi+
011500060410     C                                       '"DSCLNP"'+wSepCampi+
011600060410     C                                       '"DSCNRS"'+wSepCampi+
011700060410     C                                       '"DSCNSP"'+wSepCampi+
011800060410     C                                       '"DSCFNS"'+wSepCampi+
011900060410     C                                       '"DSCLNA"'+wSepCampi+
012000060410     C                                       '"DSCZNC"'+wSepCampi+
012100060410     C                                       '"DSCRSM"'+wSepCampi+
012200060410     C                                       '"DSCINM"'+wSepCampi+
012300060410     C                                       '"DSCCAM"'+wSepCampi+
012400060410     C                                       '"DSCLOM"'+wSepCampi+
012500060410     C                                       '"DSCPRM"'+wSepCampi+
012600060410     C                                       '"DSCFAP"'+wSepCampi+
012700060410     C                                       '"DSCNZM"'+wSepCampi+
012800060410     C                                       '"DSCRSD"'+wSepCampi+
012900060410     C                                       '"DSCIND"'+wSepCampi+
013000060410     C                                       '"DSCCAD"'+wSepCampi+
013100060410     C                                       '"DSCLOD"'+wSepCampi+
013200060410     C                                       '"DSCPRD"'+wSepCampi+
013300060410     C                                       '"DSCNZD"'+wSepCampi+
013400060410     C                                       '"DSCFIN"'+wSepCampi+
013500060410     C                                       '"DSCTSP"'+wSepCampi+
013600060410     C                                       '"DSCFBR"'+wSepCampi+
013700060410     C                                       '"DSCNCL"'+wSepCampi+
013800060410     C                                       '"DSCPKF"'+wSepCampi+
013900060410     C                                       '"DSCVLF"'+wSepCampi+
014000060410     C                                       '"DSCRMN"'+wSepCampi+
014100060410     C                                       '"DSCRMA"'+wSepCampi+
014200060410     C                                       '"DSCTCR"'+wSepCampi+
014300060410     C                                       '"DSCDCR"'+wSepCampi+
014400060410     C                                       '"DSCHCR"'+wSepCampi+
014500060410     C                                       '"DSCPDC"'+wSepCampi+
014600060410     C                                       '"DSCFGS"'+wSepCampi+
014700060410     C                                       '"DSCNDC"'+wSepCampi+
014800060410     C                                       '"DSCDDC"'+wSepCampi+
014900060410     C                                       '"DSCTC1"'+wSepCampi+
015000060410     C                                       '"DSCTC2"'+wSepCampi+
015100060410     C                                       '"DSCGGA"'+wSepCampi+
015200060410     C                                       '"DSCGMA"'+wSepCampi+
015300060410     C                                       '"DSCGVA"'+wSepCampi+
015400060410     C                                       '"DSCNGD"'+wSepCampi+
015500060410     C                                       '"DSCNSS"'+wSepCampi+
015600060410     C                                       '"DSCTIC"'+wSepCampi+
015700060410     C                                       '"DSCCAS"'+wSepCampi+
015800060410     C                                       '"DSCVCA"'+wSepCampi+
015900060410     C                                       '"DSCFIV"'+wSepCampi+
016000060410     C                                       '"DSCNFT"'+wSepCampi+
016100060410     C                                       '"DSCDFT"'+wSepCampi+
016200060410     C                                       '"DSCIFT"'+wSepCampi+
016300060410     C                                       '"DSCDIV"'+wSepCampi+
016400060410     C                                       '"DSCFLS"'+wSepCampi+
016500060410     C                                       '"DSCNCD"'+wSepCampi+
016600060410     C                                       '"DSCNCA"'+wSepCampi+
016700060410     C                                       '"DSCS01"'+wSepCampi+
016800060410     C                                       '"DSCS02"'+wSepCampi+
016900060410     C                                       '"DSCS03"'+wSepCampi+
017000060410     C                                       '"DSCS04"'+wSepCampi+
017100060410     C                                       '"DSCS05"'+wSepCampi+
017200060410     C                                       '"DSCS06"'+wSepCampi+
017300060410     C                                       '"DSCS07"'+wSepCampi+
017400060410     C                                       '"DSCS08"'+wSepCampi+
017500060410     C                                       '"DSCS09"'+wSepCampi+
017600060410     C                                       '"DSCS10"'+wSepCampi+
017700060410     C                                       '"DSCDGE"'+wSepCampi+
017800070626     C                                       '"DSCHMC"'+wSepCampi+
017900070626     C                                       '"CAUSALE RESO"'
018000030325     C*
018100060410     C                   WRITE     TIVAW000
018200030325     C                   ENDIF
018300991027     C*
018400060410     C                   READ      TIVGDTMP
018500060410     C                   DOW       not %eof(TIVGDTMP)
018600060411     C*
018700060411     C* Valorizzo il buffer dal campo flat di inout
018800060411     C                   EVAL      wfdscds = vgdDTA
018900070626     C*
019000070626     C* Se trattasi d bolla d reso ne reperisco il motivo
019100070626     C                   EXSR      rtvMOTIVO
019200991027     C*
019300060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
019400060411     C***  '"':''''      XLATE     wfdscds       wfdscds
019500051007     C*
019600051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
019700060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
019800060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
019900060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
020000060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
020100060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
020200060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
020300060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
020400060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
020500051007     C*
020600051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
020700040206     C                   IF        %subst(prmppt:2:1) = 'D'
020800060411     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
020900060411     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
021000060411     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
021100060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
021200060411     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
021300060411     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
021400060411     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
021500060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
021600060411     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
021700060411     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
021800060411     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
021900060411     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
022000060411     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
022100060411     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
022200060411     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
022300060411     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
022400060411     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
022500060411     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
022600060411     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
022700060411     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
022800060411     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
022900060411     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
023000060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
023100060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
023200060411     C                                                    %trim(wPKF)+wSepCampi+
023300060411     C                                                    %trim(wVLF)+wSepCampi+
023400060411     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
023500060411     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
023600060411     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
023700060411     C                             %trim(%editw(DSCDCR:'    /  /  '))+wSepCampi+
023800060411     C                                  %trim(%editw(DSCHCR:'  :  '))+wSepCampi+
023900060411     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
024000060411     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
024100060411     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
024200060411     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
024300060411     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
024400060411     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
024500060411     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
024600060411     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
024700060411     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
024800060411     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
024900060411     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
025000060411     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
025100060411     C                                                    %trim(wCAS)+wSepCampi+
025200060411     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
025300060411     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
025400060411     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
025500060411     C                             %trim(%editw(DSCDFT:'    /  /  '))+wSepCampi+
025600060411     C                                                    %trim(wIFT)+wSepCampi+
025700060411     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
025800060411     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
025900060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
026000060411     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
026100060411     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
026200060411     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
026300060411     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
026400060411     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
026500060411     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
026600060411     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
026700060411     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
026800060411     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
026900060411     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
027000060411     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
027100060411     C                             %trim(%editw(DSCDGE:'    /  /  '))+wSepCampi+
027200070626     C                                  %trim(%editw(DSCHMC:'  :  '))+wSepCampi+
027300070626     C                                        '"'+%trim(wMOTIVO)+'"'
027400060411     C                   ELSE
027500060411     C*
027600060410     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
027700060410     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
027800060410     C                                      %trim(%editc(DSCNRS:'Q'))+wSepCampi+
027900060410     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
028000060410     C                                         '"'+%trim(DSCFNS)+'"' +wSepCampi+
028100060410     C                                      %trim(%editc(DSCLNA:'Q'))+wSepCampi+
028200060410     C                                      %trim(%editc(DSCZNC:'Q'))+wSepCampi+
028300060410     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
028400060410     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
028500060410     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
028600060410     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
028700060410     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
028800060410     C                                         '"'+%trim(DSCFAP)+'"' +wSepCampi+
028900060410     C                                         '"'+%trim(DSCNZM)+'"' +wSepCampi+
029000060410     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
029100060410     C                                         '"'+%trim(DSCIND)+'"' +wSepCampi+
029200060410     C                                         '"'+%trim(DSCCAD)+'"' +wSepCampi+
029300060410     C                                         '"'+%trim(DSCLOD)+'"' +wSepCampi+
029400060410     C                                         '"'+%trim(DSCPRD)+'"' +wSepCampi+
029500060410     C                                         '"'+%trim(DSCNZD)+'"' +wSepCampi+
029600060410     C                                         '"'+%trim(DSCFIN)+'"' +wSepCampi+
029700060410     C                                         '"'+%trim(DSCTSP)+'"' +wSepCampi+
029800060410     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
029900060410     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
030000060410     C                                                    %trim(wPKF)+wSepCampi+
030100060410     C                                                    %trim(wVLF)+wSepCampi+
030200060410     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
030300060410     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
030400060410     C                                         '"'+%trim(DSCTCR)+'"' +wSepCampi+
030500060410     C                                      %trim(%editc(DSCDCR:'Q'))+wSepCampi+
030600060410     C                                      %trim(%editc(DSCHCR:'Q'))+wSepCampi+
030700060410     C                                      %trim(%editc(DSCPDC:'Q'))+wSepCampi+
030800060410     C                                      %trim(%editc(DSCFGS:'Q'))+wSepCampi+
030900060410     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
031000060410     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
031100060410     C                                         '"'+%trim(DSCTC1)+'"' +wSepCampi+
031200060410     C                                         '"'+%trim(DSCTC2)+'"' +wSepCampi+
031300060410     C                                         '"'+%trim(DSCGGA)+'"' +wSepCampi+
031400060410     C                                         '"'+%trim(DSCGMA)+'"' +wSepCampi+
031500060410     C                                         '"'+%trim(DSCGVA)+'"' +wSepCampi+
031600060410     C                                      %trim(%editc(DSCNGD:'Q'))+wSepCampi+
031700060410     C                                      %trim(%editc(DSCNSS:'Q'))+wSepCampi+
031800060410     C                                         '"'+%trim(DSCTIC)+'"' +wSepCampi+
031900060410     C                                                    %trim(wCAS)+wSepCampi+
032000060410     C                                         '"'+%trim(DSCVCA)+'"' +wSepCampi+
032100060410     C                                      %trim(%editc(DSCFIV:'Q'))+wSepCampi+
032200060410     C                                      %trim(%editc(DSCNFT:'Q'))+wSepCampi+
032300060410     C                                      %trim(%editc(DSCDFT:'Q'))+wSepCampi+
032400060410     C                                                    %trim(wIFT)+wSepCampi+
032500060410     C                                         '"'+%trim(DSCDIV)+'"' +wSepCampi+
032600060410     C                                      %trim(%editc(DSCFLS:'Q'))+wSepCampi+
032700060410     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
032800060410     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
032900060410     C                                      %trim(%editc(DSCS01:'Q'))+wSepCampi+
033000060410     C                                      %trim(%editc(DSCS02:'Q'))+wSepCampi+
033100060410     C                                      %trim(%editc(DSCS03:'Q'))+wSepCampi+
033200060410     C                                      %trim(%editc(DSCS04:'Q'))+wSepCampi+
033300060410     C                                      %trim(%editc(DSCS05:'Q'))+wSepCampi+
033400060410     C                                      %trim(%editc(DSCS06:'Q'))+wSepCampi+
033500060410     C                                      %trim(%editc(DSCS07:'Q'))+wSepCampi+
033600060410     C                                      %trim(%editc(DSCS08:'Q'))+wSepCampi+
033700060410     C                                      %trim(%editc(DSCS09:'Q'))+wSepCampi+
033800060410     C                                      %trim(%editc(DSCS10:'Q'))+wSepCampi+
033900060410     C                                      %trim(%editc(DSCDGE:'Q'))+wSepCampi+
034000070626     C                                      %trim(%editc(DSCHMC:'Q'))+wSepCampi+
034100070626     C                                        '"'+%trim(wMOTIVO)+'"'
034200040206     C*
034300040206     C                   ENDIF
034400930409     C*
034500060410     C                   WRITE     TIVAW000
034600060410     C                   DELETE    TIVGD000
034700991027     C*
034800060410     C                   READ      TIVGDTMP
034900030325     C                   ENDDO
035000991027     C*
035100030325     C                   EVAL      wrkesito = '0'
035200991027     C*
035300910830     C                   ENDSR
035400070625
035500070625
035600070625
035700070625     C     cartab        BEGSR
035800070625     C*
035900070625     C                   clear                   sk2Alavgiac
036000070625     C                   clear                   sk2AlavgiacD
036100070625     C                   z-add     *zeros        i                 3 0
036200070625     C                   eval      tblKUT = 1
036300070625     C                   eval      tblCOD = '2A'
036400070625     C     KeyTABP       setll     tabel00f
036500070625     C     KeyTABP       reade     tabel00f
036600070625     C                   dow       not %eof(tabel00f)
036700070625     C                   movel(p)  tblUNI        ds2a
036800070625     C                   if        §2aFTC = 'G' or §2aFTC = 'A'
036900070625     C                   add       1             i
037000070625     C                   eval      sk2Alavgiac(i)  = tblKEY
037100070625     C                   eval      sk2AlavgiacD(i) = §2ADES
037200070625     C                   endif
037300070625     C     KeyTABP       reade     tabel00f
037400070625     C                   enddo
037500070625     C*
037600070625     C                   ENDSR
037700070625
037800070625
037900991027
038000991027      /TITLE Operazioni iniziali.
038100991027     C     *inzsr        BEGSR
038200991027     C*
038300991027     C     *ENTRY        PLIST
038400991027     C                   parm                    prmppt
038500991027     C     wrkesito      parm      wrkesito      prmesito
038600070625     C*
038700070625     C* DEFINIZIONE CHIAVI
038800070625     C*
038900070625     C* Chiave su TABEL00F - Parziale
039000070625     C     KeyTABP       klist
039100070625     C                   kfld                    tblKUT
039200070625     C                   kfld                    tblCOD
039300070625     C*
039400070625     C* Chiave su TITAS30C - Completa
039500070625     C     KeyTAS30C     klist
039600070625     C                   kfld                    dscAAS
039700070625     C                   kfld                    dscLNP
039800070625     C                   kfld                    dscNRS
039900070625     C                   kfld                    dscNSP
040000070625     C*
040100070625     C* Chiave su TIGCP51L - Completa
040200070625     C     KeyGCP51C     klist
040300070626     C                   kfld                    lblAAO
040400070626     C                   kfld                    lblLPO
040500070626     C                   kfld                    lblNRO
040600070626     C                   kfld                    lblNSO
040700070625     C                   kfld                    gcpFRG
040800070625     C*
040900070625     C* Chiave su FNEVB01L - Parziale
041000070625     C     KeyEVB01P     klist
041100070626     C                   kfld                    lblAAO
041200070626     C                   kfld                    lblLPO
041300070626     C                   kfld                    lblNRO
041400070626     C                   kfld                    lblNSO
041500070625     C*
041600070625     C* Chiave su FNLBL01L - Completa
041700070626     C     KeyLBL01C     klist
041800070625     C                   kfld                    dscAAS
041900070625     C                   kfld                    dscLNP
042000070625     C                   kfld                    dscNRS
042100070625     C                   kfld                    dscNSP
042200991027     C*
042300991027     C                   ENDSR
