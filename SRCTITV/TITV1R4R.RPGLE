000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090323     FTIVGDTMP  uF   E             DISK
000500090803     Ffnorn02l  IF   E           K DISK
000600100402     FTIVAWWWT  uF A E             DISK    commit
000700100402     Ffnorm01l  IF   E           K DISK
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090323     D fnvapds       e ds                  extname(fnvap00t)
001300991027
001400100402     D wORMRSR         s                   like(ORMRSR)
001500100402     D wORMINR         s                   like(ORMINR)
001600100402     D wORMCAR         s                   like(ORMCAR)
001700100402     D wORMLOR         s                   like(ORMLOR)
001800100402     D wORMPRR         s                   like(ORMPRR)
001900100402     D wORMNAR         s                   like(ORMNAR)
002000100402     D wORMRER         s                   like(ORMRER)
002100100402     D wORMTER         s                   like(ORMTER)
002200100402     D wORMRSC         s                   like(ORMRSC)
002300100402     D wORMINC         s                   like(ORMINC)
002400100402     D wORMCAC         s                   like(ORMCAC)
002500100402     D wORMLOC         s                   like(ORMLOC)
002600100402     D wORMPRC         s                   like(ORMPRC)
002700100402     D wORMNAC         s                   like(ORMNAC)
002800090803
002900100402
003000090803
003100030908     C                   EXSR      chkRECFILMBR
003200991027     C                   EXSR      traduci
003300921023     C                   SETON                                        LR
003400030908
003500090803
003600090803
003700030908     C     chkRECFILMBR  BEGSR
003800030908     C*
003900100402     C                   READ      TIVAWWWT                               55
004000030908     C*
004100030908     C                   ENDSR
004200090803
004300090803
004400991027
004500991027     C     traduci       BEGSR
004600051007     C*
004700051007     C* Verifico se richiesto un particolare carattere d separatore campi
004800051007     C                   IF        %subst(prmppt:3:1) <> *blanks
004900051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
005000051007     C                   ELSE
005100051007     C                   MOVEL     ','           wSepCampi         1
005200051007     C                   ENDIF
005300051007     C*
005400051007     C* Verifico se richiesto un particolare carattere d separatore decimale
005500051007     C                   SETON                                        60
005600051007     C                   IF        %subst(prmppt:4:1) = ','
005700051007     C                   SETOFF                                       60
005800051007     C                   ENDIF
005900030325     C*
006000030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006100030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
006200100402     C                   EVAL      VAWDTA  = '"VAPIDC"'+wSepCampi+
006300060719     C                                       '"VAPPOE"'+wSepCampi+
006400060719     C                                       '"VAPDPE"'+wSepCampi+
006500060719     C                                       '"VAPNSR"'+wSepCampi+
006600060719     C                                       '"VAPNOR"'+wSepCampi+
006700060719     C                                       '"VAPNRV"'+wSepCampi+
006800060719     C                                       '"VAPRFA"'+wSepCampi+
006900060719     C                                       '"VAPPOG"'+wSepCampi+
007000060719     C                                       '"VAPDPG"'+wSepCampi+
007100060719     C                                       '"VAPDAE"'+wSepCampi+
007200060719     C                                       '"VAPORE"'+wSepCampi+
007300060719     C                                       '"VAPFAR"'+wSepCampi+
007400060719     C                                       '"VAPDFA"'+wSepCampi+
007500060719     C                                       '"VAPCAR"'+wSepCampi+
007600060719     C                                       '"VAPDCA"'+wSepCampi+
007700100402     C                                       '"VAPNOT"'
007800100402     C*
007900100402     C                   if        %subst(prmppt:6:1) = 'S'
008000100402     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
008100100402     C                                      '"MITRSR"'+wSepCampi+
008200100402     C                                      '"MITINR"'+wSepCampi+
008300100402     C                                      '"MITCAR"'+wSepCampi+
008400100402     C                                      '"MITLOR"'+wSepCampi+
008500100402     C                                      '"MITPRR"'+wSepCampi+
008600100402     C                                      '"MITNAR"'+wSepCampi+
008700100402     C                                      '"MITRER"'+wSepCampi+
008800100402     C                                      '"MITTER"'+wSepCampi+
008900100402     C                                      '"DESRSC"'+wSepCampi+
009000100402     C                                      '"DESINC"'+wSepCampi+
009100100402     C                                      '"DESCAC"'+wSepCampi+
009200100402     C                                      '"DESLOC"'+wSepCampi+
009300100402     C                                      '"DESPRC"'+wSepCampi+
009400100402     C                                      '"DESNAC"'
009500100402     C                   endif
009600030325     C*
009700100402     C                   WRITE     TIVAW000
009800030325     C                   ENDIF
009900991027     C*
010000090323     C                   READ      TIVGDTMP
010100090323     C                   DOW       not %eof(TIVGDTMP)
010200090803     C*
010300090803     C* Ridefinizione dati flat
010400090803     C                   eval      fnvapds = vgdDTA
010500130115     C*
010600130115     C                   setoff                                       40
010700090729     C*
010800090729     C* Personalizzazione cliente => solamente fasi 999 (ANNULLATI)
010900130115     C                   if        (%subst(prmppt:5:1) = 'N' AND vapFAR <> 999)
011000130115     C                   seton                                        40
011100130115     C                   endif
011200130115     C*
011300130115     C* Personalizzazione cliente => solamente fasi 900 (BOLLETTATI) e 999 (ANNULLATI)
011400130115     C                   if        (%subst(prmppt:5:1) = '9' AND
011500130115     C                              vapFAR <> 900 AND vapFAR <> 999)
011600130115     C                   seton                                        40
011700130115     C                   endif
011800130115     C*
011900130115     C* Personalizzazione cliente => solamente fasi > 400 + "RICEVUTO"
012000130115     C                   if        (%subst(prmppt:5:1) = '4' AND
012100130115     C                              vapFAR <= 400 AND vapFAR <> 000)
012200130115     C                   seton                                        40
012300130115     C                   endif
012400130115     C*
012500130115     C* Se OK => procedo
012600130115     C                   if        not *in40
012700090803     C*
012800090803     C* Reperimento note orm
012900090803     C                   exsr      repNOTE
013000051007     C*
013100051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
013200040206     C                   IF        %subst(prmppt:2:1) = 'D'
013300100402     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
013400060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
013500060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
013600060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
013700060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
013800060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
013900060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
014000060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
014100060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
014200060719     C                             %trim(%editw(VAPDAE:'    /  /  '))+wSepCampi+
014300060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
014400060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
014500060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
014600060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
014700060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
014800100402     C                                         '"'+%trim(VAPNOT)+'"'
014900040206     C                   ELSE
015000060719     C*
015100100402     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
015200060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
015300060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
015400060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
015500060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
015600060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
015700060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
015800060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
015900060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
016000060719     C                                      %trim(%editc(VAPDAE:'Q'))+wSepCampi+
016100060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
016200060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
016300060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
016400060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
016500060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
016600100402     C                                         '"'+%trim(VAPNOT)+'"'
016700040206     C*
016800040206     C                   ENDIF
016900100402     C*
017000100402     C* Se richiesto nei parametri d ingresso ritorno anche i dati del mittente originale
017100100402     C                   if        %subst(prmppt:6:1) = 'S'
017200100402     C                   exsr      repORM
017300100402     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
017400100402     C                                        '"'+%trim(wORMRSR)+'"' +wSepCampi+
017500100402     C                                        '"'+%trim(wORMINR)+'"' +wSepCampi+
017600100402     C                                        '"'+%trim(wORMCAR)+'"' +wSepCampi+
017700100402     C                                        '"'+%trim(wORMLOR)+'"' +wSepCampi+
017800100402     C                                        '"'+%trim(wORMPRR)+'"' +wSepCampi+
017900100402     C                                        '"'+%trim(wORMNAR)+'"' +wSepCampi+
018000100402     C                                        '"'+%trim(wORMRER)+'"' +wSepCampi+
018100100402     C                                        '"'+%trim(wORMTER)+'"' +wSepCampi+
018200100402     C                                        '"'+%trim(wORMRSC)+'"' +wSepCampi+
018300100402     C                                        '"'+%trim(wORMINC)+'"' +wSepCampi+
018400100402     C                                        '"'+%trim(wORMCAC)+'"' +wSepCampi+
018500100402     C                                        '"'+%trim(wORMLOC)+'"' +wSepCampi+
018600100402     C                                        '"'+%trim(wORMPRC)+'"' +wSepCampi+
018700100402     C                                        '"'+%trim(wORMNAC)+'"'
018800100402     C                   endif
018900930409     C*
019000100402     C                   WRITE     TIVAW000
019100090729     C*
019200130115     C                   endif                                                  * personal. FAR
019300090729     C*
019400090323     C                   DELETE    TIVGD000
019500991027     C*
019600090323     C                   READ      TIVGDTMP
019700030325     C                   ENDDO
019800991027     C*
019900030325     C                   EVAL      wrkesito = '0'
020000991027     C*
020100910830     C                   ENDSR
020200090803
020300090803
020400090803
020500090803     C     repNOTE       BEGSR
020600090803     C*
020700090803     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
020800090803     C     KEYorn02_P    SETLL     fnorn02l
020900090803     C                   IF        %equal(fnorn02l)
021000090803     C* Inizializzo contatore x ritorno max 4 note
021100090803     C                   Z-ADD     1             wContaORN         1 0
021200090803     C* Lettura note
021300090803     C     KEYorn02_P    READE     fnorn02l
021400090803     C                   DOW       not %eof(fnorn02l) AND
021500090803     C                             wContaORN <= 4
021600090803     C* Solo note con data/ora > data/ora esecuzione fase relativa
021700090803     C                   IF        orndai*1000000+ornori >=
021800090803     C                             vapdae*1000000+vapore
021900090803     C*
022000090803     C                   EVAL      vapNOT = %trim(vapNOT) + ' ' + %trim(ornNOT)
022100090803     C                   ADD       1             wContaORN
022200090803     C                   ENDIF
022300090803     C*
022400090803     C     KEYorn02_P    READE     fnorn02l
022500090803     C                   ENDDO
022600090803     C                   ENDIF
022700090803     C*
022800090803     C                   ENDSR
022900100402
023000100402
023100100402
023200100402     C*
023300100402     C     repORM        BEGSR
023400100402     C*
023500100402     C                   clear                   wORMRSR
023600100402     C                   clear                   wORMINR
023700100402     C                   clear                   wORMCAR
023800100402     C                   clear                   wORMLOR
023900100402     C                   clear                   wORMPRR
024000100402     C                   clear                   wORMNAR
024100100402     C                   clear                   wORMRER
024200100402     C                   clear                   wORMTER
024300100402     C                   clear                   wORMRSC
024400100402     C                   clear                   wORMINC
024500100402     C                   clear                   wORMCAC
024600100402     C                   clear                   wORMLOC
024700100402     C                   clear                   wORMPRC
024800100402     C                   clear                   wORMNAC
024900100402     C*
025000100402     C* Aggancio l'orm relativo alla fase corrente
025100100402     C     KEYorm01_C    chain     fnorm01l
025200100402     C                   if        %found(fnorm01l)
025300100402     C                   movel     ORMRSR        wORMRSR
025400100402     C                   movel     ORMINR        wORMINR
025500100402     C                   movel     ORMCAR        wORMCAR
025600100402     C                   movel     ORMLOR        wORMLOR
025700100402     C                   movel     ORMPRR        wORMPRR
025800100402     C                   movel     ORMNAR        wORMNAR
025900100402     C                   movel     ORMRER        wORMRER
026000100402     C                   movel     ORMTER        wORMTER
026100100402     C                   movel     ORMRSC        wORMRSC
026200100402     C                   movel     ORMINC        wORMINC
026300100402     C                   movel     ORMCAC        wORMCAC
026400100402     C                   movel     ORMLOC        wORMLOC
026500100402     C                   movel     ORMPRC        wORMPRC
026600100402     C                   movel     ORMNAC        wORMNAC
026700100402     C                   endif
026800100402     C*
026900100402     C                   ENDSR
027000100402
027100090803
027200090803
027300991027
027400991027      /TITLE Operazioni iniziali.
027500991027     C     *inzsr        BEGSR
027600991027     C*
027700991027     C     *ENTRY        PLIST
027800991027     C                   parm                    prmppt
027900991027     C     wrkesito      parm      wrkesito      prmesito
028000991027     C*
028100090803     C* Definizione chiavi
028200090803     C*
028300090803     C* File: FNORN02L
028400090803     C     KEYorn02_P    KLIST
028500090803     C                   KFLD                    VAPPOE
028600090803     C                   KFLD                    VAPNSR
028700090803     C                   KFLD                    VAPNOR
028800090803     C                   KFLD                    VAPNRV
028900090803     C                   KFLD                    VAPFAR
029000100402     C*
029100100402     C     KEYorm01_C    KLIST
029200100402     C                   KFLD                    VAPPOE
029300100402     C                   KFLD                    VAPNSR
029400100402     C                   KFLD                    VAPNOR
029500100402     C                   KFLD                    VAPNRV
029600090803     C*
029700991027     C                   ENDSR
