000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090323     FTIVGDTMP  uF   E             DISK
000500100706     FTIVAWWWT  uF A E             DISK    commit
000600120130     Ftiori05l  iF   E           K DISK
000700120130     Ffnacr01l  iF   E           K DISK
000800100706
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300090323     D fnvapds       e ds                  extname(fnvap00t)
001400100706
001500100706
001600100706     D wPKB            s              8    inz
001700100706
001800100706
001900991027
002000030908     C                   EXSR      chkRECFILMBR
002100991027     C                   EXSR      traduci
002200921023     C                   SETON                                        LR
002300100706
002400100706
002500030908
002600030908     C     chkRECFILMBR  BEGSR
002700100706     C*
002800100706     C                   READ      TIVAWWWT                               55
002900030908     C*
003000030908     C                   ENDSR
003100100706
003200100706
003300991027
003400991027     C     traduci       BEGSR
003500051007     C*
003600051007     C* Verifico se richiesto un particolare carattere d separatore campi
003700051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003800051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003900051007     C                   ELSE
004000100706     C                   MOVEL     ';'           wSepCampi         1
004100051007     C                   ENDIF
004200051007     C*
004300051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004400051007     C                   SETON                                        60
004500100706     C                   IF        %subst(prmppt:4:1) = ','
004600100706     C                   SETOFF                                       60
004700051007     C                   ENDIF
004800030325     C*
004900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005100100706     C                   EVAL      VAWDTA  = '"VAPIDC"'+wSepCampi+
005200060719     C                                       '"VAPPOE"'+wSepCampi+
005300060719     C                                       '"VAPDPE"'+wSepCampi+
005400060719     C                                       '"VAPNSR"'+wSepCampi+
005500060719     C                                       '"VAPNOR"'+wSepCampi+
005600060719     C                                       '"VAPNRV"'+wSepCampi+
005700060719     C                                       '"VAPRFA"'+wSepCampi+
005800060719     C                                       '"VAPPOG"'+wSepCampi+
005900060719     C                                       '"VAPDPG"'+wSepCampi+
006000060719     C                                       '"VAPDAE"'+wSepCampi+
006100060719     C                                       '"VAPORE"'+wSepCampi+
006200060719     C                                       '"VAPFAR"'+wSepCampi+
006300060719     C                                       '"VAPDFA"'+wSepCampi+
006400060719     C                                       '"VAPCAR"'+wSepCampi+
006500060719     C                                       '"VAPDCA"'+wSepCampi+
006600100706     C                                       '"VAPNOT"'+wSepCampi+
006700100706     C                                       '"MITRSC"'+wSepCampi+
006800100706     C                                       '"MITIND"'+wSepCampi+
006900100706     C                                       '"MITCAP"'+wSepCampi+
007000100706     C                                       '"MITLOC"'+wSepCampi+
007100100706     C                                       '"MITPRV"'+wSepCampi+
007200120130     C                                       '"MITNAR"'+wSepCampi+
007300100706     C                                       '"DESRSD"'+wSepCampi+
007400100706     C                                       '"DESIND"'+wSepCampi+
007500100706     C                                       '"DESCAD"'+wSepCampi+
007600100706     C                                       '"DESLOD"'+wSepCampi+
007700100706     C                                       '"DESPRD"'+wSepCampi+
007800100706     C                                       '"DESNZD"'+wSepCampi+
007900100706     C                                       '"BOLNCL"'+wSepCampi+
008000100706     C                                       '"BOLPKB"'
008100030325     C*
008200100706     C                   WRITE     TIVAW000
008300030325     C                   ENDIF
008400991027     C*
008500090323     C                   READ      TIVGDTMP
008600090323     C                   DOW       not %eof(TIVGDTMP)
008700090323     C                   eval      fnvapds = vgdDTA
008800120127     C*
008900120127     C                   setoff                                       40
009000120127     C*
009100120127     C
009200120127     C                   if        %subst(prmppt:5:4) <> *blanks
009300120127     C                   if        %subst(prmppt:5:1) = 'S' and
009400120127     C                             %dec(%subst(prmppt:5+1:3):3:0) = VAPFAR
009500120127     C                   else
009600120127     C                   seton                                        40
009700120127     C                   endif
009800120127     C*
009900120127     C                   if        %subst(prmppt:5:1) = 'E' and
010000120127     C                             %dec(%subst(prmppt:5+1:3):3:0) = VAPFAR
010100120127     C                   seton                                        40
010200120127     C                   endif
010300120127     C                   endif
010400120127     C*
010500120127     C* Restituisco i dati solo se rispondono ai requisiti richiesti nei parametri
010600120127     C                   if        not *in40
010700100706     C*
010800120130     C* Reperisco i dati mancanti sul VAP
010900120130     C                   exsr      repORI
011000120130     C*
011100120130     C                   if        *in20
011200100706     C*
011300100706     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
011400120130     C  N60              EVAL      wPKB = %trim(%editw(vaoPKG:'    0 , '))
011500120130     C   60              EVAL      wPKB = %trim(%editw(vaoPKG:'    0 . '))
011600051007     C*
011700051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
011800040206     C                   IF        %subst(prmppt:2:1) = 'D'
011900100706     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
012000060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
012100060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
012200060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
012300060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
012400060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
012500060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
012600060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
012700060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
012800060719     C                             %trim(%editw(VAPDAE:'    /  /  '))+wSepCampi+
012900060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
013000060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
013100060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
013200060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
013300060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
013400100708     C                                         '"'+%trim(VAPNOT)+'"'
013500040206     C                   ELSE
013600060719     C*
013700100706     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
013800060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
013900060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
014000060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
014100060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
014200060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
014300060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
014400060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
014500060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
014600060719     C                                      %trim(%editc(VAPDAE:'Q'))+wSepCampi+
014700060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
014800060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
014900060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
015000060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
015100060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
015200100708     C                                         '"'+%trim(VAPNOT)+'"'
015300040206     C*
015400040206     C                   ENDIF
015500100706     C*
015600120130     C* Dati aggiuntivi...
015700120130     C                   EVAL      VAWDTA  = %trim(VAWDTA)          +wSepCampi+
015800120130     C                                        '"'+%trim(vaoRSR)+'"' +wSepCampi+
015900120130     C                                        '"'+%trim(vaoINR)+'"' +wSepCampi+
016000120130     C                                        '"'+%trim(vaoCAR)+'"' +wSepCampi+
016100120130     C                                        '"'+%trim(vaoLOR)+'"' +wSepCampi+
016200120130     C                                        '"'+%trim(vaoPRR)+'"' +wSepCampi+
016300120130     C                                        '"'+%trim(vaoNAR)+'"' +wSepCampi+
016400120130     C                                        '"'+%trim(vaoRSC)+'"' +wSepCampi+
016500120130     C                                        '"'+%trim(vaoINC)+'"' +wSepCampi+
016600120130     C                                        '"'+%trim(vaoCAC)+'"' +wSepCampi+
016700120130     C                                        '"'+%trim(vaoLOC)+'"' +wSepCampi+
016800120130     C                                        '"'+%trim(vaoPRC)+'"' +wSepCampi+
016900120130     C                                        '"'+%trim(vaoNAC)+'"' +wSepCampi+
017000120130     C                                     %trim(%editc(vaoNCL:'Q'))+wSepCampi+
017100100706     C                                               %trim(wPKB)
017200100706     C
017300930409     C*
017400100706     C                   WRITE     TIVAW000
017500120130     C                   endif
017600120127     C                   endif
017700120127     C*
017800090323     C                   DELETE    TIVGD000
017900991027     C*
018000090323     C                   READ      TIVGDTMP
018100030325     C                   ENDDO
018200991027     C*
018300030325     C                   EVAL      wrkesito = '0'
018400991027     C*
018500910830     C                   ENDSR
018600100706
018700100706
018800100706
018900120130      /TITLE Reperimento dati da file ORI
019000120130     C     REPORI        BEGSR
019100120130     C*
019200120130     C                   setoff                                       20
019300120130     C                   clear                   tiori000
019400120228     C*
019500120228     C* Solo x orm confermati
019600120228     C                   if        vapPOE > *zeros AND
019700120228     C                             vapNOR > *zeros
019800100706     C*
019900120130     C     KEYori05_C    CHAIN     tiori05l
020000120228     C                   IF        %found(tiori05l) AND vaoATB = *blanks
020100120130     C                   seton                                        20
020200100706     C*
020300100706     C* Reperisco i dati del mittente ...
020400120130     C                   IF        vaoCRA > 0
020500120130     C                   z-add     vaoCRA        wCRO             10 0
020600120130     C                   exsr      repACR
020700120130     C   30              eval      vaoRSR = acrRSR
020800120130     C   30              eval      vaoINR = acrINR
020900120130     C   30              eval      vaoCAR = acrCAR
021000120130     C   30              eval      vaoLOR = acrLOR
021100120130     C   30              eval      vaoPRR = acrPRR
021200120130     C   30              eval      vaoNAR = acrNAR
021300100706     C                   ENDIF
021400120130     C*
021500120130     C* Reperisco i dati del destinatario ...
021600120130     C                   IF        vaoCRC > 0
021700120130     C                   z-add     vaoCRC        wCRO             10 0
021800120130     C                   exsr      repACR
021900120130     C   30              eval      vaoRSC = acrRSR
022000120130     C   30              eval      vaoINC = acrINR
022100120130     C   30              eval      vaoCAC = acrCAR
022200120130     C   30              eval      vaoLOC = acrLOR
022300120130     C   30              eval      vaoPRC = acrPRR
022400120130     C   30              eval      vaoNAC = acrNAR
022500120130     C                   ENDIF
022600120130     C*
022700100706     C                   ENDIF
022800120228     C*
022900120228     C                   endif
023000100706     C*
023100100706     C                   ENDSR
023200100706
023300100706
023400100706
023500100706
023600100706     C*
023700120130     C     REPACR        BEGSR
023800120130     C*
023900120130     C                   setoff                                       30
024000100706     C*
024100100706     C* Posizionamento diretto
024200120130     C     wCRO          chain     fnacr01l
024300120130     C                   if        %found(fnacr01l)
024400120130     C                   seton                                        30
024500120130     C                   endif
024600100706     C*
024700100706     C                   ENDSR
024800100706
024900100706
025000991027
025100100706
025200991027      /TITLE Operazioni iniziali.
025300991027     C     *inzsr        BEGSR
025400991027     C*
025500991027     C     *ENTRY        PLIST
025600991027     C                   parm                    prmppt
025700991027     C     wrkesito      parm      wrkesito      prmesito
025800100706     C*
025900120130     C     KEYori05_C    KLIST
026000120130     C                   KFLD                    vapPOE
026100120130     C                   KFLD                    vapNSR
026200120130     C                   KFLD                    vapNOR
026300120130     C                   KFLD                    vapNRV
026400991027     C*
026500991027     C                   ENDSR
