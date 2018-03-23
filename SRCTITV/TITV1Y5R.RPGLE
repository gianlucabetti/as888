000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090323     FTIVGDTMP  uF   E             DISK
000500100706     FTIVAWWWT  uF A E             DISK    commit
000600100706     FtitaA30c  if   E           k DISK
000700100706     Fcnaco00f  if   E           k DISK
000800100706     Fcnind00f  if   E           k DISK
000900100706     Ftita432c  iF   E           K DISK    rename(tita4000:tita4032)
001000100706     F                                     rename(tita4P00:tita4P32)
001100100706     Ftitas30c  iF   E           K DISK
001200100706
001300991027
001400991027     D prmppt          s             50
001500991027     D prmesito        s              1
001600991027     D wrkesito        s                   like(prmesito)
001700090323     D fnvapds       e ds                  extname(fnvap00t)
001800100706
001900100706
002000100706     D wPKB            s              8    inz
002100100706
002200100706     D wTAARSC         s                   like(TAARSC)
002300100706     D wTAAIND         s                   like(TAAIND)
002400100706     D wTAACAP         s                   like(TAACAP)
002500100706     D wTAALOC         s                   like(TAALOC)
002600100706     D wTAAPRV         s                   like(TAAPRV)
002700100706     D wTASRSD         s                   like(TASRSD)
002800100706     D wTASIND         s                   like(TASIND)
002900100706     D wTASCAD         s                   like(TASCAD)
003000100706     D wTASLOD         s                   like(TASLOD)
003100100706     D wTASPRD         s                   like(TASPRD)
003200100706     D wTASNZD         s                   like(TASNZD)
003300100706     D wTASNCL         s                   like(TASNCL)
003400100706     D wTASPKB         s                   like(TASPKB)
003500100706
003600100706
003700100706     D*------------
003800100706     D* DS di ridefinizione chiave esterna ORM
003900100706     D*------------
004000100706     D                 DS
004100100706     D x_vapPOE                       3  0
004200100706     D x_vapNSR                       2  0
004300100706     D x_vapNOR                       7  0
004400100706     D x_vapNRV                       2  0
004500100706     D keyORM                  1     14
004600100706
004700100706
004800991027
004900030908     C                   EXSR      chkRECFILMBR
005000991027     C                   EXSR      traduci
005100921023     C                   SETON                                        LR
005200100706
005300100706
005400030908
005500030908     C     chkRECFILMBR  BEGSR
005600100706     C*
005700100706     C                   READ      TIVAWWWT                               55
005800030908     C*
005900030908     C                   ENDSR
006000100706
006100100706
006200991027
006300991027     C     traduci       BEGSR
006400051007     C*
006500051007     C* Verifico se richiesto un particolare carattere d separatore campi
006600051007     C                   IF        %subst(prmppt:3:1) <> *blanks
006700051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
006800051007     C                   ELSE
006900100706     C                   MOVEL     ';'           wSepCampi         1
007000051007     C                   ENDIF
007100051007     C*
007200051007     C* Verifico se richiesto un particolare carattere d separatore decimale
007300051007     C                   SETON                                        60
007400100706     C                   IF        %subst(prmppt:4:1) = ','
007500100706     C                   SETOFF                                       60
007600051007     C                   ENDIF
007700030325     C*
007800030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
008000100706     C                   EVAL      VAWDTA  = '"VAPIDC"'+wSepCampi+
008100060719     C                                       '"VAPPOE"'+wSepCampi+
008200060719     C                                       '"VAPDPE"'+wSepCampi+
008300060719     C                                       '"VAPNSR"'+wSepCampi+
008400060719     C                                       '"VAPNOR"'+wSepCampi+
008500060719     C                                       '"VAPNRV"'+wSepCampi+
008600060719     C                                       '"VAPRFA"'+wSepCampi+
008700060719     C                                       '"VAPPOG"'+wSepCampi+
008800060719     C                                       '"VAPDPG"'+wSepCampi+
008900060719     C                                       '"VAPDAE"'+wSepCampi+
009000060719     C                                       '"VAPORE"'+wSepCampi+
009100060719     C                                       '"VAPFAR"'+wSepCampi+
009200060719     C                                       '"VAPDFA"'+wSepCampi+
009300060719     C                                       '"VAPCAR"'+wSepCampi+
009400060719     C                                       '"VAPDCA"'+wSepCampi+
009500100706     C                                       '"VAPNOT"'+wSepCampi+
009600100706     C                                       '"MITRSC"'+wSepCampi+
009700100706     C                                       '"MITIND"'+wSepCampi+
009800100706     C                                       '"MITCAP"'+wSepCampi+
009900100706     C                                       '"MITLOC"'+wSepCampi+
010000100706     C                                       '"MITPRV"'+wSepCampi+
010100100706     C                                       '"DESRSD"'+wSepCampi+
010200100706     C                                       '"DESIND"'+wSepCampi+
010300100706     C                                       '"DESCAD"'+wSepCampi+
010400100706     C                                       '"DESLOD"'+wSepCampi+
010500100706     C                                       '"DESPRD"'+wSepCampi+
010600100706     C                                       '"DESNZD"'+wSepCampi+
010700100706     C                                       '"BOLNCL"'+wSepCampi+
010800100706     C                                       '"BOLPKB"'
010900030325     C*
011000100706     C                   WRITE     TIVAW000
011100030325     C                   ENDIF
011200991027     C*
011300090323     C                   READ      TIVGDTMP
011400090323     C                   DOW       not %eof(TIVGDTMP)
011500090323     C                   eval      fnvapds = vgdDTA
011600120127     C*
011700120127     C                   setoff                                       40
011800120127     C*
011900120127     C
012000120127     C                   if        %subst(prmppt:5:4) <> *blanks
012100120127     C                   if        %subst(prmppt:5:1) = 'S' and
012200120127     C                             %dec(%subst(prmppt:5+1:3):3:0) = VAPFAR
012300120127     C                   else
012400120127     C                   seton                                        40
012500120127     C                   endif
012600120127     C*
012700120127     C                   if        %subst(prmppt:5:1) = 'E' and
012800120127     C                             %dec(%subst(prmppt:5+1:3):3:0) = VAPFAR
012900120127     C                   seton                                        40
013000120127     C                   endif
013100120127     C                   endif
013200120127     C*
013300120127     C* Restituisco i dati solo se rispondono ai requisiti richiesti nei parametri
013400120127     C                   if        not *in40
013500100706     C*
013600100706     C* Reperisco alcuni dati direttamente dalla bolla legata al ritiro
013700100706     C                   exsr      repTAS
013800100706     C*
013900100706     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
014000100706     C  N60              EVAL      wPKB = %trim(%editw(wTASPKB:'    0 , '))
014100100706     C   60              EVAL      wPKB = %trim(%editw(wTASPKB:'    0 . '))
014200051007     C*
014300051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
014400040206     C                   IF        %subst(prmppt:2:1) = 'D'
014500100706     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
014600060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
014700060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
014800060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
014900060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
015000060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
015100060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
015200060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
015300060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
015400060719     C                             %trim(%editw(VAPDAE:'    /  /  '))+wSepCampi+
015500060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
015600060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
015700060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
015800060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
015900060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
016000100708     C                                         '"'+%trim(VAPNOT)+'"'
016100040206     C                   ELSE
016200060719     C*
016300100706     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
016400060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
016500060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
016600060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
016700060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
016800060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
016900060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
017000060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
017100060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
017200060719     C                                      %trim(%editc(VAPDAE:'Q'))+wSepCampi+
017300060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
017400060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
017500060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
017600060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
017700060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
017800100708     C                                         '"'+%trim(VAPNOT)+'"'
017900040206     C*
018000040206     C                   ENDIF
018100100706     C*
018200100706     C* Dati aggiuntivi...
018300100708     C                   EVAL      VAWDTA  = %trim(VAWDTA)           +wSepCampi+
018400100706     C                                        '"'+%trim(wTAARSC)+'"' +wSepCampi+
018500100706     C                                        '"'+%trim(wTAAIND)+'"' +wSepCampi+
018600100706     C                                        '"'+%trim(wTAACAP)+'"' +wSepCampi+
018700100706     C                                        '"'+%trim(wTAALOC)+'"' +wSepCampi+
018800100706     C                                        '"'+%trim(wTAAPRV)+'"' +wSepCampi+
018900100706     C                                        '"'+%trim(wTASRSD)+'"' +wSepCampi+
019000100706     C                                        '"'+%trim(wTASIND)+'"' +wSepCampi+
019100100706     C                                        '"'+%trim(wTASCAD)+'"' +wSepCampi+
019200100706     C                                        '"'+%trim(wTASLOD)+'"' +wSepCampi+
019300100706     C                                        '"'+%trim(wTASPRD)+'"' +wSepCampi+
019400100706     C                                        '"'+%trim(wTASNZD)+'"' +wSepCampi+
019500100706     C                                     %trim(%editc(wTASNCL:'Q'))+wSepCampi+
019600100706     C                                               %trim(wPKB)
019700100706     C
019800930409     C*
019900100706     C                   WRITE     TIVAW000
020000120127     C                   endif
020100120127     C*
020200090323     C                   DELETE    TIVGD000
020300991027     C*
020400090323     C                   READ      TIVGDTMP
020500030325     C                   ENDDO
020600991027     C*
020700030325     C                   EVAL      wrkesito = '0'
020800991027     C*
020900910830     C                   ENDSR
021000100706
021100100706
021200100706
021300100706      /TITLE Reperimento dati dalle bolle
021400100706     C     REPTAS        BEGSR
021500100706     C*
021600100706     C                   clear                   wTASRSD
021700100706     C                   clear                   wTASIND
021800100706     C                   clear                   wTASCAD
021900100706     C                   clear                   wTASLOD
022000100706     C                   clear                   wTASPRD
022100100706     C                   clear                   wTASNZD
022200100706     C                   clear                   wTASNCL
022300100706     C                   clear                   wTASPKB
022400100706     C*
022500100706     C* Effettuo prima lettura (e implicitamente posizionamento) "fuori ciclo"
022600100706     C                   EVAL      x_vapPOE = vapPOE
022700100706     C                   EVAL      x_vapNSR = vapNSR
022800100706     C                   EVAL      x_vapNOR = vapNOR
022900100706     C                   EVAL      x_vapNRV = vapNRV
023000100706     C                   EVAL      ta4n14 = keyORM
023100100706     C     KEYta432_C    CHAIN     tita432c
023200100706     C                   IF        %found(tita432c)
023300100706     C                   DOW       not %eof(tita432c)
023400100706     C     KEYtas30_P    CHAIN     titas30c
023500100706     C                   IF        %found(titas30c)
023600100706     C                   eval      wTASRSD = TASRSD
023700100706     C                   eval      wTASIND = TASIND
023800100706     C                   eval      wTASCAD = TASCAD
023900100706     C                   eval      wTASLOD = TASLOD
024000100706     C                   eval      wTASPRD = TASPRD
024100100706     C                   eval      wTASNZD = TASNZD
024200100706     C                   eval      wTASNCL = TASNCL
024300100706     C                   eval      wTASPKB = TASPKB
024400100706     C*
024500100706     C* Reperisco i dati del mittente ...
024600100706     C                   exsr      repTAA
024700100706     C*
024800100706     C                   ENDIF
024900100706     C     KEYta432_C    READE     tita432c
025000100706     C                   ENDDO
025100100706     C                   ENDIF
025200100706     C*
025300100706     C                   ENDSR
025400100706
025500100706
025600100706
025700100706
025800100706     C*
025900100706     C     REPTAA        BEGSR
026000100706     C*
026100100706     C                   clear                   wTAARSC
026200100706     C                   clear                   wTAAIND
026300100706     C                   clear                   wTAACAP
026400100706     C                   clear                   wTAALOC
026500100706     C                   clear                   wTAAPRV
026600100706     C                   movel     *blanks       wTIPCCM           4
026700100706     C*
026800100706     C                   move(p)   tasCCM        wTIPCCM
026900100706     C                   if        wTIPCCM = '8888' or
027000100706     C                             wTIPCCM = '9999'
027100100706     C*
027200100706     C                   eval      taaTRC = 'M'
027300100706     C*
027400100706     C* Posizionamento diretto
027500100706     C     KEYtaA30_C    chain     titaA30c
027600100706     C                   if        %found(titaA30c)
027700100706     C                   movel     taaRSC        wTAARSC
027800100706     C                   movel     taaIND        wTAAIND
027900100706     C                   movel     taaCAP        wTAACAP
028000100706     C                   movel     taaLOC        wTAALOC
028100100706     C                   movel     taaPRV        wTAAPRV
028200100706     C                   endif
028300100706     C*
028400100706     C                   else
028500100706     C                   eval      acokut = 1
028600100706     C     KEYaco00_C    chain     cnaco00f
028700100706     C                   if        %found(cnaco00f)
028800100706     C                   movel     acoRAG        wTAARSC
028900100706     C     KEYind00_C    chain     cnind00f
029000100706     C                   if        %found(cnind00f)
029100100706     C                   movel     indVIA        wTAAIND
029200100706     C                   movel     indCAP        wTAACAP
029300100706     C                   movel     indCIT        wTAALOC
029400100706     C                   movel     indPRV        wTAAPRV
029500100706     C                   endif
029600100706     C                   endif
029700100706     C                   endif
029800100706     C*
029900100706     C                   ENDSR
030000100706
030100100706
030200991027
030300100706
030400991027      /TITLE Operazioni iniziali.
030500991027     C     *inzsr        BEGSR
030600991027     C*
030700991027     C     *ENTRY        PLIST
030800991027     C                   parm                    prmppt
030900991027     C     wrkesito      parm      wrkesito      prmesito
031000100706     C*
031100100706     C                   eval      acokcc = 151
031200100706     C*
031300100706     C     KEYtas30_P    KLIST
031400100706     C                   KFLD                    ta4AAS
031500100706     C                   KFLD                    ta4LNP
031600100706     C                   KFLD                    ta4NRS
031700100706     C                   KFLD                    ta4NSP
031800100706     C*
031900100706     C     KEYta432_C    KLIST
032000100706     C                   KFLD                    ta4N14
032100100706     C*
032200100706     C     KEYtaA30_C    KLIST
032300100706     C                   KFLD                    tasaas
032400100706     C                   KFLD                    taslnp
032500100706     C                   KFLD                    tasnrs
032600100706     C                   KFLD                    tasnsp
032700100706     C                   KFLD                    taatrc
032800100706     C*
032900100706     C     KEYaco00_C    KLIST
033000100706     C                   KFLD                    acokut
033100100706     C                   KFLD                    acokcc
033200100706     C                   KFLD                    tasccm
033300100706     C*
033400100706     C     KEYind00_C    KLIST
033500100706     C                   KFLD                    acokut
033600100706     C                   KFLD                    acokcc
033700100706     C                   KFLD                    tasccm
033800991027     C*
033900991027     C                   ENDSR
