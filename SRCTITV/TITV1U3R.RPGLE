000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600100118     FTIVAWWWT  uF A E             DISK    commit
000700100118     FTIVGD00F  O    E             DISK    prefix(w_) rename(TIVGD000:TIVGDWWW)
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300030331     D dsvac         e ds                  extname(fnvac00t)
001400051007     D wIAS            s             14
001500051007     D wPKB            s              8
001600051007     D wVLB            s              6
001700051007     D wQFT            s             14
001800051007     D wCAS            s             14
001900991027
002000030908     C                   EXSR      chkRECFILMBR
002100991027     C                   EXSR      traduci
002200921023     C                   SETON                                        LR
002300030908
002400030908     C     chkRECFILMBR  BEGSR
002500030908     C*
002600100118     C                   READ      TIVAWWWT                               55
002700030908     C*
002800030908     C                   ENDSR
002900991027
003000991027     C     traduci       BEGSR
003100051007     C*
003200051007     C* Verifico se richiesto un particolare carattere d separatore campi
003300051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003400051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003500051007     C                   ELSE
003600051007     C                   MOVEL     ','           wSepCampi         1
003700051007     C                   ENDIF
003800051007     C*
003900051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004000051007     C                   SETON                                        60
004100051007     C                   IF        %subst(prmppt:4:1) = ','
004200051007     C                   SETOFF                                       60
004300051007     C                   ENDIF
004400030325     C*
004500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004700100118     C                   EVAL      VAWDTA  = '"VACAAS"'+wSepCampi+
004800051007     C                                       '"VACLNP"'+wSepCampi+
004900051007     C                                       '"VACNRS"'+wSepCampi+
005000051007     C                                       '"VACNSP"'+wSepCampi+
005100051007     C                                       '"VACMGS"'+wSepCampi+
005200051007     C                                       '"VACCBO"'+wSepCampi+
005300051007     C                                       '"VACLNA"'+wSepCampi+
005400051007     C                                       '"VACRSD"'+wSepCampi+
005500051007     C                                       '"VACPRD"'+wSepCampi+
005600051007     C                                       '"VACGC1"'+wSepCampi+
005700051007     C                                       '"VACGC2"'+wSepCampi+
005800051007     C                                       '"VACCTR"'+wSepCampi+
005900051007     C                                       '"VACCTS"'+wSepCampi+
006000051007     C                                       '"VACFTM"'+wSepCampi+
006100051007     C                                       '"VACFIN"'+wSepCampi+
006200051007     C                                       '"VACFAP"'+wSepCampi+
006300051007     C                                       '"VACTSP"'+wSepCampi+
006400051007     C                                       '"VACIAS"'+wSepCampi+
006500051007     C                                       '"VACVAS"'+wSepCampi+
006600051007     C                                       '"VACNAS"'+wSepCampi+
006700051007     C                                       '"VACNCL"'+wSepCampi+
006800051007     C                                       '"VACPKB"'+wSepCampi+
006900051007     C                                       '"VACVLB"'+wSepCampi+
007000051007     C                                       '"VACQFT"'+wSepCampi+
007100051007     C                                       '"VACTIC"'+wSepCampi+
007200051007     C                                       '"VACCAS"'+wSepCampi+
007300051007     C                                       '"VACVCA"'+wSepCampi+
007400051007     C                                       '"VACCCM"'+wSepCampi+
007500051007     C                                       '"VACRMN"'+wSepCampi+
007600051007     C                                       '"VACRMA"'+wSepCampi+
007700051007     C                                       '"VACRMO"'+wSepCampi+
007800051007     C                                       '"VACFFD"'+wSepCampi+
007900051007     C                                       '"VACTCR"'+wSepCampi+
008000051007     C                                       '"VACDCR"'+wSepCampi+
008100051007     C                                       '"VACHCR"'+wSepCampi+
008200051007     C                                       '"VACDCM"'+wSepCampi+
008300051007     C                                       '"VACHMC"'+wSepCampi+
008400051007     C                                       '"VACTC1"'+wSepCampi+
008500051007     C                                       '"VACTC2"'+wSepCampi+
008600051007     C                                       '"VACCCA"'+wSepCampi+
008700051007     C                                       '"VACDLA"'+wSepCampi+
008800070426     C                                       '"VACDAG"'
008900070426     C*
009000070426     C                   IF        %subst(prmppt:5:1) = 'I'
009100100118     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
009200070426     C                                       '"VACIND"'+wSepCampi+
009300070426     C                                       '"VACCAD"'+wSepCampi+
009400070426     C                                       '"VACLOD"'
009500070426     C                   ENDIF
009600030325     C*
009700100118     C                   WRITE     TIVAW000
009800030325     C                   ENDIF
009900991027     C*
010000080924     C                   READ      tivgdtmp
010100080924     C                   DOW       not %eof(tivgdtmp)
010200080924     c                   movel     vgddta        dsvac
010300991027     C*
010400030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
010500060411     C***  '"':''''      XLATE     dsvac         dsvac
010600100118     C*
010700100118     C* Restituisco anche 1 altro file con i soli esiti d NON CONSEGNA
010800100118     C                   if         vacDCM =  *zeros OR
010900100118     C                             (vacDCM <> *zeros AND vacCCA = 'C') OR
011000100118     C                             (vacDCM <> *zeros AND vacCCA = 'A')
011100100118     C                   exsr      wrivgd
011200100118     C                   endif
011300100118     C*
011400051007     C*
011500051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
011600051007     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
011700051007     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
011800051007     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
011900051007     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
012000051007     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
012100051007     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
012200051007     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
012300051007     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
012400051007     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
012500051007     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
012600051007     C*
012700051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
012800040206     C                   IF        %subst(prmppt:2:1) = 'D'
012900100118     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
013000051007     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
013100051007     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
013200051007     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
013300051007     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
013400051007     C                                         '"'+%trim(VACCBO)+'"' +wSepCampi+
013500051007     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
013600051007     C                                         '"'+%trim(VACRSD)+'"' +wSepCampi+
013700051007     C                                         '"'+%trim(VACPRD)+'"' +wSepCampi+
013800051007     C                                         '"'+%trim(VACGC1)+'"' +wSepCampi+
013900051007     C                                         '"'+%trim(VACGC2)+'"' +wSepCampi+
014000051007     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
014100051007     C                                         '"'+%trim(VACCTS)+'"' +wSepCampi+
014200051007     C                                         '"'+%trim(VACFTM)+'"' +wSepCampi+
014300051007     C                                         '"'+%trim(VACFIN)+'"' +wSepCampi+
014400051007     C                                         '"'+%trim(VACFAP)+'"' +wSepCampi+
014500051007     C                                         '"'+%trim(VACTSP)+'"' +wSepCampi+
014600051007     C                                                    %trim(wIAS)+wSepCampi+
014700051007     C                                         '"'+%trim(VACVAS)+'"' +wSepCampi+
014800051007     C                                         '"'+%trim(VACNAS)+'"' +wSepCampi+
014900051007     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
015000051007     C                                                    %trim(wPKB)+wSepCampi+
015100051007     C                                                    %trim(wVLB)+wSepCampi+
015200051007     C                                                    %trim(wQFT)+wSepCampi+
015300051007     C                                         '"'+%trim(VACTIC)+'"' +wSepCampi+
015400051007     C                                                    %trim(wCAS)+wSepCampi+
015500051007     C                                         '"'+%trim(VACVCA)+'"' +wSepCampi+
015600051007     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
015700051007     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
015800051007     C                                         '"'+%trim(VACRMA)+'"' +wSepCampi+
015900051007     C                                         '"'+%trim(VACRMO)+'"' +wSepCampi+
016000051007     C                                         '"'+%trim(VACFFD)+'"' +wSepCampi+
016100051007     C                                         '"'+%trim(VACTCR)+'"' +wSepCampi+
016200051007     C                             %trim(%editw(VACDCR:'    /  /  '))+wSepCampi+
016300051007     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
016400051007     C                             %trim(%editw(VACDCM:'    /  /  '))+wSepCampi+
016500051007     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
016600051007     C                                         '"'+%trim(VACTC1)+'"' +wSepCampi+
016700051007     C                                         '"'+%trim(VACTC2)+'"' +wSepCampi+
016800051007     C                                         '"'+%trim(VACCCA)+'"' +wSepCampi+
016900051007     C                             %trim(%editw(VACDLA:'    /  /  '))+wSepCampi+
017000051007     C                             %trim(%editw(VACDAG:'    /  /  '))
017100040206     C                   ELSE
017200040206     C*
017300100118     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
017400051007     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
017500051007     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
017600051007     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
017700051007     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
017800051007     C                                          '"'+%trim(VACCBO)+'"'+wSepCampi+
017900051007     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
018000051007     C                                          '"'+%trim(VACRSD)+'"'+wSepCampi+
018100051007     C                                          '"'+%trim(VACPRD)+'"'+wSepCampi+
018200051007     C                                          '"'+%trim(VACGC1)+'"'+wSepCampi+
018300051007     C                                          '"'+%trim(VACGC2)+'"'+wSepCampi+
018400051007     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
018500051007     C                                          '"'+%trim(VACCTS)+'"'+wSepCampi+
018600051007     C                                          '"'+%trim(VACFTM)+'"'+wSepCampi+
018700051007     C                                          '"'+%trim(VACFIN)+'"'+wSepCampi+
018800051007     C                                          '"'+%trim(VACFAP)+'"'+wSepCampi+
018900051007     C                                          '"'+%trim(VACTSP)+'"'+wSepCampi+
019000051007     C                                                    %trim(wIAS)+wSepCampi+
019100051007     C                                          '"'+%trim(VACVAS)+'"'+wSepCampi+
019200051007     C                                          '"'+%trim(VACNAS)+'"'+wSepCampi+
019300051007     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
019400051007     C                                                    %trim(wPKB)+wSepCampi+
019500051007     C                                                    %trim(wVLB)+wSepCampi+
019600051007     C                                                    %trim(wQFT)+wSepCampi+
019700051007     C                                          '"'+%trim(VACTIC)+'"'+wSepCampi+
019800051007     C                                                    %trim(wCAS)+wSepCampi+
019900051007     C                                          '"'+%trim(VACVCA)+'"'+wSepCampi+
020000051007     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
020100051007     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
020200051007     C                                          '"'+%trim(VACRMA)+'"'+wSepCampi+
020300051007     C                                          '"'+%trim(VACRMO)+'"'+wSepCampi+
020400051007     C                                          '"'+%trim(VACFFD)+'"'+wSepCampi+
020500051007     C                                          '"'+%trim(VACTCR)+'"'+wSepCampi+
020600051007     C                                      %trim(%editc(VACDCR:'Q'))+wSepCampi+
020700051007     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
020800051007     C                                      %trim(%editc(VACDCM:'Q'))+wSepCampi+
020900051007     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
021000051007     C                                          '"'+%trim(VACTC1)+'"'+wSepCampi+
021100051007     C                                          '"'+%trim(VACTC2)+'"'+wSepCampi+
021200051007     C                                          '"'+%trim(VACCCA)+'"'+wSepCampi+
021300051007     C                                      %trim(%editc(VACDLA:'Q'))+wSepCampi+
021400051007     C                                      %trim(%editc(VACDAG:'Q'))
021500040206     C                   ENDIF
021600070426     C*
021700070426     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
021800070426     C                   IF        %subst(prmppt:5:1) = 'I'
021900070426     C*
022000070426     C* Inizializzo i campi "estensione"
022100070426     C                   EVAL      TASIND = *blanks
022200070426     C                   EVAL      TASCAD = *blanks
022300070426     C                   EVAL      TASLOD = *blanks
022400070426     C*
022500070426     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
022600070426     C     KEYtas30C     CHAIN     titas30c
022700070426     C                   IF        %found(titas30c)
022800070426     C*
022900070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
023000100118     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
023100070426     C                                          '"'+%trim(TASIND)+'"'+wSepCampi+
023200070426     C                                          '"'+%trim(TASCAD)+'"'+wSepCampi+
023300070426     C                                          '"'+%trim(TASLOD)+'"'
023400070426     C                   ENDIF
023500070426     C                   ENDIF
023600930409     C*
023700100118     C                   WRITE     TIVAW000
023800080924     C                   DELETE    tivgd000
023900991027     C*
024000080924     C                   READ      tivgdtmp
024100030325     C                   ENDDO
024200991027     C*
024300030325     C                   EVAL      wrkesito = '0'
024400991027     C*
024500910830     C                   ENDSR
024600100118
024700100118
024800100118
024900100118      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
025000100118     C     wriVGD        BEGSR
025100100118     C*
025200100118     C                   clear                   tivgdwww
025300100118     C                   eval      w_vgdDTA = vgddta
025400100118     C                   eval      w_vgdTIP = '03'
025500100118     C                   eval      w_vgdKSU = %subst(prmppt:6:8)
025600100118     C                   eval      w_vgdTSC = 'WW'
025700100118     C                   eval      w_vgdDAT = datcor
025800100118     C                   eval      w_vgdPGM = 'TITV1U3R'
025900100118     C                   write     tivgdwww
026000100118     C*
026100100118     C                   ENDSR
026200100118     C*------------------------------------------------------------------------*
026300100118
026400100118
026500100118
026600991027
026700991027      /TITLE Operazioni iniziali.
026800991027     C     *inzsr        BEGSR
026900991027     C*
027000991027     C     *ENTRY        PLIST
027100991027     C                   parm                    prmppt
027200991027     C     wrkesito      parm      wrkesito      prmesito
027300070426     C*
027400070426     C* Definizione chiavi
027500070426     C*
027600070426     C* Chiave su TITAS30C - Completa
027700070426     C     KEYtas30C     KLIST
027800070426     C                   KFLD                    vacAAS
027900070426     C                   KFLD                    vacLNP
028000070426     C                   KFLD                    vacNRS
028100070426     C                   KFLD                    vacNSP
028200100118     C*
028300100118     C                   z-add     *zeros        datcor            8 0
028400100118     C                   eval      datcor = %dec(%date() : *ISO)
028500991027     C*
028600991027     C                   ENDSR
