000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090123     Ftivgdtmp  uF   E             DISK
000500090123     Ftivawwwt  uf a E             DISK
000600090123     Ffnorn02l  IF   E           K DISK
000700991027
000800090123     D fnvapds       e ds                  extname(fnvap00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090123
001300991027
001400030908     C                   EXSR      chkRECFILMBR
001500991027     C                   EXSR      traduci
001600921023     C                   SETON                                        LR
001700030908
001800090123
001900030908     C     chkRECFILMBR  BEGSR
002000030908     C*
002100090123     C                   READ      TIVAWWWT                               55
002200030908     C*
002300030908     C                   ENDSR
002400991027
002500090123
002600991027     C     traduci       BEGSR
002700051007     C*
002800051007     C* Verifico se richiesto un particolare carattere d separatore campi
002900051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003000051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003100051007     C                   ELSE
003200051007     C                   MOVEL     ','           wSepCampi         1
003300051007     C                   ENDIF
003400051007     C*
003500051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003600051007     C                   SETON                                        60
003700051007     C                   IF        %subst(prmppt:4:1) = ','
003800051007     C                   SETOFF                                       60
003900051007     C                   ENDIF
004000030325     C*
004100030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004200030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004300090123     C                   EVAL      VAWDTA  = '"VAPIDC"'+wSepCampi+
004400060719     C                                       '"VAPPOE"'+wSepCampi+
004500060719     C                                       '"VAPDPE"'+wSepCampi+
004600060719     C                                       '"VAPNSR"'+wSepCampi+
004700060719     C                                       '"VAPNOR"'+wSepCampi+
004800060719     C                                       '"VAPNRV"'+wSepCampi+
004900060719     C                                       '"VAPRFA"'+wSepCampi+
005000060719     C                                       '"VAPPOG"'+wSepCampi+
005100060719     C                                       '"VAPDPG"'+wSepCampi+
005200060719     C                                       '"VAPDAE"'+wSepCampi+
005300060719     C                                       '"VAPORE"'+wSepCampi+
005400060719     C                                       '"VAPFAR"'+wSepCampi+
005500060719     C                                       '"VAPDFA"'+wSepCampi+
005600060719     C                                       '"VAPCAR"'+wSepCampi+
005700060719     C                                       '"VAPDCA"'+wSepCampi+
005800090123     C                                       '"VAPNOT"'+wSepCampi+
005900090123     C                                       '"NOTE"'  +wSepCampi
006000030325     C*
006100090123     C                   WRITE     TIVAW000
006200030325     C                   ENDIF
006300991027     C*
006400090123     C                   READ      tivgdtmp
006500090123     C                   DOW       not %eof(tivgdtmp)
006600090123     C                   movel     vgddta        fnvapds
006700090123     C*
006800090123     C* Filtro le fasi nn volute dal cliente
006900090123     C                   if        vapfar = 000 or
007000090123     C                             vapfar = 400 or
007100090123     C                             vapfar = 500 or
007200090123     C                             vapfar = 900 or
007300090123     C                             vapfar = 910 or
007400090123     C                             vapfar = 999
007500090123     C*
007600090123     C* Reperisco le note della fase corrente
007700090123     C                   exsr      reporn
007800051007     C*
007900051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
008000040206     C                   IF        %subst(prmppt:2:1) = 'D'
008100090123     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
008200060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
008300060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
008400060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
008500060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
008600060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
008700060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
008800060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
008900060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
009000060719     C                             %trim(%editw(VAPDAE:'    /  /  '))+wSepCampi+
009100060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
009200060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
009300060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
009400060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
009500060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
009600090123     C                                         '"'+%trim(VAPNOT)+'"' +wSepCampi+
009700090123     C                                         '"'+%trim(wNoteFase)+'"'
009800040206     C                   ELSE
009900060719     C*
010000090123     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
010100060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
010200060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
010300060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
010400060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
010500060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
010600060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
010700060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
010800060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
010900060719     C                                      %trim(%editc(VAPDAE:'Q'))+wSepCampi+
011000060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
011100060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
011200060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
011300060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
011400060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
011500090123     C                                         '"'+%trim(VAPNOT)+'"' +wSepCampi+
011600090123     C                                         '"'+%trim(wNoteFase)+'"'
011700040206     C*
011800040206     C                   ENDIF
011900930409     C*
012000090123     C                   WRITE     TIVAW000
012100090123     C*
012200090123     C                   endif
012300090123     C*
012400090123     C                   DELETE    tivgd000
012500991027     C*
012600090123     C                   READ      tivgdtmp
012700030325     C                   ENDDO
012800991027     C*
012900030325     C                   EVAL      wrkesito = '0'
013000991027     C*
013100910830     C                   ENDSR
013200991027
013300090123
013400090123
013500090123     C     REPORN        BEGSR
013600090123     C*
013700090123     C                   MOVEL     *blanks       wNoteFase       150
013800090123     C*
013900090123     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
014000090123     C     KEYorn2P      SETLL     fnorn02l
014100090123     C                   IF        %equal(fnorn02l)
014200090123     C*
014300090123     C* Lettura note
014400090123     C     KEYorn2P      READE     fnorn02l
014500090123     C                   DOW       not %eof(fnorn02l) AND
014600090123     C                             %len(%trim(wNoteFase)) <=
014700090123     C                             %size(wNoteFase) - 35
014800090123     C*
014900090123     C* Solo note con data/ora > data/ora esecuzione fase relativa
015000090123     C                   IF        orndai*1000000+ornori >=
015100090123     C                             vapdae*1000000+vapore
015200090123     C*
015300090123     C* Aggiungo al buffer del VAP la nota fase ORM corrente
015400090123     C                   EVAL      wNoteFase = %trim(wNoteFase) + ' ' +
015500090123     C                             %trim(ornnot)
015600090123     C                   ENDIF
015700090123     C*
015800090123     C     KEYorn2P      READE     fnorn02l
015900090123     C                   ENDDO
016000090123     C                   ENDIF
016100090123     C*
016200090123     C                   ENDSR
016300090123
016400090123
016500090123
016600991027      /TITLE Operazioni iniziali.
016700991027     C     *inzsr        BEGSR
016800991027     C*
016900991027     C     *ENTRY        PLIST
017000991027     C                   parm                    prmppt
017100991027     C     wrkesito      parm      wrkesito      prmesito
017200090123     C*
017300090123     C* Definizione chiavi
017400090123     C*
017500090123     C* File: FNORN02L
017600090123     C     KEYorn2P      KLIST
017700090123     C                   KFLD                    VAPPOE
017800090123     C                   KFLD                    VAPNSR
017900090123     C                   KFLD                    VAPNOR
018000090123     C                   KFLD                    VAPNRV
018100090123     C                   KFLD                    VAPFAR
018200991027     C*
018300991027     C                   ENDSR
