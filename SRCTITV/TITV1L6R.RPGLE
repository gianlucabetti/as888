000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600030908     FFNVACXLS  uF A E             DISK
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100030325
001200030331     D dsvac         e ds                  extname(fnvac00t)
001300051007     D wIAS            s             14
001400051007     D wPKB            s              8
001500051007     D wVLB            s              6
001600051007     D wQFT            s             14
001700051007     D wCAS            s             14
001800991027
001900030908     C                   EXSR      chkRECFILMBR
002000991027     C                   EXSR      traduci
002100921023     C                   SETON                                        LR
002200030908
002300030908     C     chkRECFILMBR  BEGSR
002400030908     C*
002500030908     C                   READ      FNVACXLS                               55
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
004600051007     C                   EVAL      VACXDTA = '"VACAAS"'+wSepCampi+
004700051007     C                                       '"VACLNP"'+wSepCampi+
004800051007     C                                       '"VACNRS"'+wSepCampi+
004900051007     C                                       '"VACNSP"'+wSepCampi+
005000051007     C                                       '"VACMGS"'+wSepCampi+
005100051007     C                                       '"VACCBO"'+wSepCampi+
005200051007     C                                       '"VACLNA"'+wSepCampi+
005300051007     C                                       '"VACRSD"'+wSepCampi+
005400051007     C                                       '"VACPRD"'+wSepCampi+
005500051007     C                                       '"VACGC1"'+wSepCampi+
005600051007     C                                       '"VACGC2"'+wSepCampi+
005700051007     C                                       '"VACCTR"'+wSepCampi+
005800051007     C                                       '"VACCTS"'+wSepCampi+
005900051007     C                                       '"VACFTM"'+wSepCampi+
006000051007     C                                       '"VACFIN"'+wSepCampi+
006100051007     C                                       '"VACFAP"'+wSepCampi+
006200051007     C                                       '"VACTSP"'+wSepCampi+
006300051007     C                                       '"VACIAS"'+wSepCampi+
006400051007     C                                       '"VACVAS"'+wSepCampi+
006500051007     C                                       '"VACNAS"'+wSepCampi+
006600051007     C                                       '"VACNCL"'+wSepCampi+
006700051007     C                                       '"VACPKB"'+wSepCampi+
006800051007     C                                       '"VACVLB"'+wSepCampi+
006900051007     C                                       '"VACQFT"'+wSepCampi+
007000051007     C                                       '"VACTIC"'+wSepCampi+
007100051007     C                                       '"VACCAS"'+wSepCampi+
007200051007     C                                       '"VACVCA"'+wSepCampi+
007300051007     C                                       '"VACCCM"'+wSepCampi+
007400051007     C                                       '"VACRMN"'+wSepCampi+
007500051007     C                                       '"VACRMA"'+wSepCampi+
007600051007     C                                       '"VACRMO"'+wSepCampi+
007700051007     C                                       '"VACFFD"'+wSepCampi+
007800051007     C                                       '"VACTCR"'+wSepCampi+
007900051007     C                                       '"VACDCR"'+wSepCampi+
008000051007     C                                       '"VACHCR"'+wSepCampi+
008100051007     C                                       '"VACDCM"'+wSepCampi+
008200051007     C                                       '"VACHMC"'+wSepCampi+
008300051007     C                                       '"VACTC1"'+wSepCampi+
008400051007     C                                       '"VACTC2"'+wSepCampi+
008500051007     C                                       '"VACCCA"'+wSepCampi+
008600051007     C                                       '"VACDLA"'+wSepCampi+
008700070426     C                                       '"VACDAG"'
008800070426     C*
008900070426     C                   IF        %subst(prmppt:5:1) = 'I'
009000070426     C                   EVAL      VACXDTA = %trim(VACXDTA) + wSepCampi+
009100070426     C                                       '"VACIND"'+wSepCampi+
009200070426     C                                       '"VACCAD"'+wSepCampi+
009300070426     C                                       '"VACLOD"'
009400070426     C                   ENDIF
009500030325     C*
009600030331     C                   WRITE     FNVACX00
009700030325     C                   ENDIF
009800991027     C*
009900080924     C                   READ      tivgdtmp
010000080924     C                   DOW       not %eof(tivgdtmp)
010100080924     c                   movel     vgddta        dsvac
010200991027     C*
010300030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
010400060411     C***  '"':''''      XLATE     dsvac         dsvac
010500051007     C*
010600051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
010700051007     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
010800051007     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
010900051007     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
011000051007     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
011100051007     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
011200051007     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
011300051007     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
011400051007     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
011500051007     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
011600051007     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
011700051007     C*
011800051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
011900040206     C                   IF        %subst(prmppt:2:1) = 'D'
012000051007     C                   EVAL      VACXDTA =%trim(%editc(VACAAS:'Q'))+wSepCampi+
012100051007     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
012200051007     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
012300051007     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
012400051007     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
012500051007     C                                         '"'+%trim(VACCBO)+'"' +wSepCampi+
012600051007     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
012700051007     C                                         '"'+%trim(VACRSD)+'"' +wSepCampi+
012800051007     C                                         '"'+%trim(VACPRD)+'"' +wSepCampi+
012900051007     C                                         '"'+%trim(VACGC1)+'"' +wSepCampi+
013000051007     C                                         '"'+%trim(VACGC2)+'"' +wSepCampi+
013100051007     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
013200051007     C                                         '"'+%trim(VACCTS)+'"' +wSepCampi+
013300051007     C                                         '"'+%trim(VACFTM)+'"' +wSepCampi+
013400051007     C                                         '"'+%trim(VACFIN)+'"' +wSepCampi+
013500051007     C                                         '"'+%trim(VACFAP)+'"' +wSepCampi+
013600051007     C                                         '"'+%trim(VACTSP)+'"' +wSepCampi+
013700051007     C                                                    %trim(wIAS)+wSepCampi+
013800051007     C                                         '"'+%trim(VACVAS)+'"' +wSepCampi+
013900051007     C                                         '"'+%trim(VACNAS)+'"' +wSepCampi+
014000051007     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
014100051007     C                                                    %trim(wPKB)+wSepCampi+
014200051007     C                                                    %trim(wVLB)+wSepCampi+
014300051007     C                                                    %trim(wQFT)+wSepCampi+
014400051007     C                                         '"'+%trim(VACTIC)+'"' +wSepCampi+
014500051007     C                                                    %trim(wCAS)+wSepCampi+
014600051007     C                                         '"'+%trim(VACVCA)+'"' +wSepCampi+
014700051007     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
014800051007     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
014900051007     C                                         '"'+%trim(VACRMA)+'"' +wSepCampi+
015000051007     C                                         '"'+%trim(VACRMO)+'"' +wSepCampi+
015100051007     C                                         '"'+%trim(VACFFD)+'"' +wSepCampi+
015200051007     C                                         '"'+%trim(VACTCR)+'"' +wSepCampi+
015300051007     C                             %trim(%editw(VACDCR:'    /  /  '))+wSepCampi+
015400051007     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
015500051007     C                             %trim(%editw(VACDCM:'    /  /  '))+wSepCampi+
015600051007     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
015700051007     C                                         '"'+%trim(VACTC1)+'"' +wSepCampi+
015800051007     C                                         '"'+%trim(VACTC2)+'"' +wSepCampi+
015900051007     C                                         '"'+%trim(VACCCA)+'"' +wSepCampi+
016000051007     C                             %trim(%editw(VACDLA:'    /  /  '))+wSepCampi+
016100051007     C                             %trim(%editw(VACDAG:'    /  /  '))
016200040206     C                   ELSE
016300040206     C*
016400051007     C                   EVAL      VACXDTA =%trim(%editc(VACAAS:'Q'))+wSepCampi+
016500051007     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
016600051007     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
016700051007     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
016800051007     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
016900051007     C                                          '"'+%trim(VACCBO)+'"'+wSepCampi+
017000051007     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
017100051007     C                                          '"'+%trim(VACRSD)+'"'+wSepCampi+
017200051007     C                                          '"'+%trim(VACPRD)+'"'+wSepCampi+
017300051007     C                                          '"'+%trim(VACGC1)+'"'+wSepCampi+
017400051007     C                                          '"'+%trim(VACGC2)+'"'+wSepCampi+
017500051007     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
017600051007     C                                          '"'+%trim(VACCTS)+'"'+wSepCampi+
017700051007     C                                          '"'+%trim(VACFTM)+'"'+wSepCampi+
017800051007     C                                          '"'+%trim(VACFIN)+'"'+wSepCampi+
017900051007     C                                          '"'+%trim(VACFAP)+'"'+wSepCampi+
018000051007     C                                          '"'+%trim(VACTSP)+'"'+wSepCampi+
018100051007     C                                                    %trim(wIAS)+wSepCampi+
018200051007     C                                          '"'+%trim(VACVAS)+'"'+wSepCampi+
018300051007     C                                          '"'+%trim(VACNAS)+'"'+wSepCampi+
018400051007     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
018500051007     C                                                    %trim(wPKB)+wSepCampi+
018600051007     C                                                    %trim(wVLB)+wSepCampi+
018700051007     C                                                    %trim(wQFT)+wSepCampi+
018800051007     C                                          '"'+%trim(VACTIC)+'"'+wSepCampi+
018900051007     C                                                    %trim(wCAS)+wSepCampi+
019000051007     C                                          '"'+%trim(VACVCA)+'"'+wSepCampi+
019100051007     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
019200051007     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
019300051007     C                                          '"'+%trim(VACRMA)+'"'+wSepCampi+
019400051007     C                                          '"'+%trim(VACRMO)+'"'+wSepCampi+
019500051007     C                                          '"'+%trim(VACFFD)+'"'+wSepCampi+
019600051007     C                                          '"'+%trim(VACTCR)+'"'+wSepCampi+
019700051007     C                                      %trim(%editc(VACDCR:'Q'))+wSepCampi+
019800051007     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
019900051007     C                                      %trim(%editc(VACDCM:'Q'))+wSepCampi+
020000051007     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
020100051007     C                                          '"'+%trim(VACTC1)+'"'+wSepCampi+
020200051007     C                                          '"'+%trim(VACTC2)+'"'+wSepCampi+
020300051007     C                                          '"'+%trim(VACCCA)+'"'+wSepCampi+
020400051007     C                                      %trim(%editc(VACDLA:'Q'))+wSepCampi+
020500051007     C                                      %trim(%editc(VACDAG:'Q'))
020600040206     C                   ENDIF
020700070426     C*
020800070426     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
020900070426     C                   IF        %subst(prmppt:5:1) = 'I'
021000070426     C*
021100070426     C* Inizializzo i campi "estensione"
021200070426     C                   EVAL      TASIND = *blanks
021300070426     C                   EVAL      TASCAD = *blanks
021400070426     C                   EVAL      TASLOD = *blanks
021500070426     C*
021600070426     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
021700070426     C     KEYtas30C     CHAIN     titas30c
021800070426     C                   IF        %found(titas30c)
021900070426     C*
022000070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
022100070426     C                   EVAL      VACXDTA = %trim(VACXDTA) + wSepCampi+
022200070426     C                                          '"'+%trim(TASIND)+'"'+wSepCampi+
022300070426     C                                          '"'+%trim(TASCAD)+'"'+wSepCampi+
022400070426     C                                          '"'+%trim(TASLOD)+'"'
022500070426     C                   ENDIF
022600070426     C                   ENDIF
022700930409     C*
022800081223     C* Forzatura x no ritorno sped: 2008-006-25-1542829
022900081223     C                   IF        (VACAAS = 2008    and
023000081223     C                              VACLNP = 006     and
023100081223     C                              VACNRS = 25      and
023200081223     C                              VACNSP = 1542829) or
023300081223     C                             (VACAAS = 2008       and
023400081223     C                              VACRMN = 8489922    and
023500081223     C                              VACRMA = 'P8489922' and
023600081223     C                              VACCCM = 0061602)
023700081223     C                   ELSE
023800030331     C                   WRITE     FNVACX00
023900081223     C                   ENDIF
024000081223     C*
024100080924     C                   DELETE    tivgd000
024200991027     C*
024300080924     C                   READ      tivgdtmp
024400030325     C                   ENDDO
024500991027     C*
024600030325     C                   EVAL      wrkesito = '0'
024700991027     C*
024800910830     C                   ENDSR
024900991027
025000991027      /TITLE Operazioni iniziali.
025100991027     C     *inzsr        BEGSR
025200991027     C*
025300991027     C     *ENTRY        PLIST
025400991027     C                   parm                    prmppt
025500991027     C     wrkesito      parm      wrkesito      prmesito
025600070426     C*
025700070426     C* Definizione chiavi
025800070426     C*
025900070426     C* Chiave su TITAS30C - Completa
026000070426     C     KEYtas30C     KLIST
026100070426     C                   KFLD                    vacAAS
026200070426     C                   KFLD                    vacLNP
026300070426     C                   KFLD                    vacNRS
026400070426     C                   KFLD                    vacNSP
026500991027     C*
026600991027     C                   ENDSR
