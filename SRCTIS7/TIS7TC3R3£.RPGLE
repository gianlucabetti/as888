000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090720     H DFTACTGRP(*NO) BNDDIR('UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600090825     FFNBLT01L  IF   E           K DISK    usropn
000700090720     F                                     extfile(LibFileBLT01)
000800090720     FTITAH30C  IF   E           K DISK
000900090720     FTIVAWWWT  uF A E             DISK    commit
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400030325
001500030331     D dsvac         e ds                  extname(fnvac00t)
001600051007     D wIAS            s             14
001700051007     D wPKB            s              8
001800051007     D wVLB            s              6
001900051007     D wQFT            s             14
002000051007     D wCAS            s             14
002100090720     D wNNC            s             35
002200991027
002300090720     D LibFileBLT01    s             21A   inz
002400090720     D currSysNeta     s              8A   inz('*NULL')
002500090720
002600090720
002700090720     D*------------------
002800090720     D* LINKING A DEFINIZIONI ESTERNE
002900090720     D*------------------
003000090720     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003100090720     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003200090720
003300090720
003400090720     C*
003500090720     C* Reperisco il sistema AS/400 corrente
003600090720     C                   callp     UBRTVNETA_Rtv(currSysNeta)
003700090720     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
003800090720     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
003900090720     C                   else
004000090720     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
004100090720     C                   endif
004200090720     C*
004300090720     C* Apertura file "overraidati"
004400090720     C                   if        not %open(FNBLT01L)
004500090720     C                   open      FNBLT01L
004600090720     C                   endif
004700090720     C*
004800090720     C* Effettuo traduzione
004900030908     C                   EXSR      chkRECFILMBR
005000991027     C                   EXSR      traduci
005100090720     C*
005200090720     C* Chiusura file "overraidati"
005300090720     C                   if        %open(FNBLT01L)
005400090720     C                   close     FNBLT01L
005500090720     C                   endif
005600090720     C*
005700921023     C                   SETON                                        LR
005800090720
005900090720
006000030908
006100030908     C     chkRECFILMBR  BEGSR
006200030908     C*
006300090720     C                   READ      TIVAW000                               55
006400030908     C*
006500030908     C                   ENDSR
006600090720
006700090720
006800991027
006900991027     C     traduci       BEGSR
007000051007     C*
007100051007     C* Verifico se richiesto un particolare carattere d separatore campi
007200051007     C                   IF        %subst(prmppt:3:1) <> *blanks
007300051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
007400051007     C                   ELSE
007500051007     C                   MOVEL     ','           wSepCampi         1
007600051007     C                   ENDIF
007700051007     C*
007800051007     C* Verifico se richiesto un particolare carattere d separatore decimale
007900051007     C                   SETON                                        60
008000051007     C                   IF        %subst(prmppt:4:1) = ','
008100051007     C                   SETOFF                                       60
008200051007     C                   ENDIF
008300030325     C*
008400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
008600090720     C                   EVAL      VAWDTA  = '"VACAAS"'+wSepCampi+
008700051007     C                                       '"VACLNP"'+wSepCampi+
008800051007     C                                       '"VACNRS"'+wSepCampi+
008900051007     C                                       '"VACNSP"'+wSepCampi+
009000051007     C                                       '"VACMGS"'+wSepCampi+
009100051007     C                                       '"VACCBO"'+wSepCampi+
009200051007     C                                       '"VACLNA"'+wSepCampi+
009300051007     C                                       '"VACRSD"'+wSepCampi+
009400051007     C                                       '"VACPRD"'+wSepCampi+
009500051007     C                                       '"VACGC1"'+wSepCampi+
009600051007     C                                       '"VACGC2"'+wSepCampi+
009700051007     C                                       '"VACCTR"'+wSepCampi+
009800051007     C                                       '"VACCTS"'+wSepCampi+
009900051007     C                                       '"VACFTM"'+wSepCampi+
010000051007     C                                       '"VACFIN"'+wSepCampi+
010100051007     C                                       '"VACFAP"'+wSepCampi+
010200051007     C                                       '"VACTSP"'+wSepCampi+
010300051007     C                                       '"VACIAS"'+wSepCampi+
010400051007     C                                       '"VACVAS"'+wSepCampi+
010500051007     C                                       '"VACNAS"'+wSepCampi+
010600051007     C                                       '"VACNCL"'+wSepCampi+
010700051007     C                                       '"VACPKB"'+wSepCampi+
010800051007     C                                       '"VACVLB"'+wSepCampi+
010900051007     C                                       '"VACQFT"'+wSepCampi+
011000051007     C                                       '"VACTIC"'+wSepCampi+
011100051007     C                                       '"VACCAS"'+wSepCampi+
011200051007     C                                       '"VACVCA"'+wSepCampi+
011300051007     C                                       '"VACCCM"'+wSepCampi+
011400051007     C                                       '"VACRMN"'+wSepCampi+
011500051007     C                                       '"VACRMA"'+wSepCampi+
011600051007     C                                       '"VACRMO"'+wSepCampi+
011700051007     C                                       '"VACFFD"'+wSepCampi+
011800051007     C                                       '"VACTCR"'+wSepCampi+
011900051007     C                                       '"VACDCR"'+wSepCampi+
012000051007     C                                       '"VACHCR"'+wSepCampi+
012100051007     C                                       '"VACDCM"'+wSepCampi+
012200051007     C                                       '"VACHMC"'+wSepCampi+
012300051007     C                                       '"VACTC1"'+wSepCampi+
012400051007     C                                       '"VACTC2"'+wSepCampi+
012500051007     C                                       '"VACCCA"'+wSepCampi+
012600051007     C                                       '"VACDLA"'+wSepCampi+
012700090720     C                                       '"VACDAG"'+wSepCampi+
012800090720     C                                       '"SEGNAC."'
012900070426     C*
013000070426     C                   IF        %subst(prmppt:5:1) = 'I'
013100090720     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
013200070426     C                                       '"VACIND"'+wSepCampi+
013300070426     C                                       '"VACCAD"'+wSepCampi+
013400070426     C                                       '"VACLOD"'
013500070426     C                   ENDIF
013600030325     C*
013700090720     C                   WRITE     TIVAW000
013800030325     C                   ENDIF
013900991027     C*
014000080924     C                   READ      tivgdtmp
014100080924     C                   DOW       not %eof(tivgdtmp)
014200090720     C                   movel     vgddta        dsvac
014300090720     C*
014400090720     C                   CLEAR                   wNNC
014500090720     C*
014600090720     C* Aggancio il dettaglio colli
014700090720     C                   EVAL      tahTRC = 'C'
014800090720     C     KEYtah30_C    SETLL     titah30c
014900090720     C                   IF        %equal(titah30c)
015000090720     C     KEYtah30_C    READE     titah30c
015100090720     C                   DOW       not %eof(titah30c)
015200090720     C                   EVAL      wNNC = tahNOT
015300090720     C*
015400090720     C* Se riesco considero la data consegna merce d ogni singolo collo
015500090720     C                   EVAL      bltNSC = tahNSC
015600090720     C     KEYblt01_C    CHAIN     fnblt01l
015700090720     C                   IF        %found(fnblt01l)
015800090720     C                   IF        bltDCM > *zeros
015900090720     C                   MOVE      bltDCM        VACDCM
016000090720     C                   ENDIF
016100090720     C                   ENDIF
016200090720     C*
016300090720     C* Valorizzo il buffer d output
016400090720     C                   EXSR      ValBuffOut
016500090720     C*
016600090720     C     KEYtah30_C    READE     titah30c
016700090720     C                   ENDDO
016800090720     C                   ELSE
016900090720     C*
017000090720     C* Valorizzo il buffer d output
017100090720     C                   EXSR      ValBuffOut
017200090720     C*
017300090720     C                   ENDIF
017400090720     C*
017500090720     C* Proseguo con la lettura del file
017600080924     C                   DELETE    tivgd000
017700080924     C                   READ      tivgdtmp
017800090720     C*
017900030325     C                   ENDDO
018000991027     C*
018100030325     C                   EVAL      wrkesito = '0'
018200991027     C*
018300910830     C                   ENDSR
018400090720
018500090720
018600090720
018700090720     C     ValBuffOut    BEGSR
018800090720     C*
018900090720     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
019000090720     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
019100090720     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
019200090720     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
019300090720     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
019400090720     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
019500090720     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
019600090720     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
019700090720     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
019800090720     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
019900090720     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
020000090720     C*
020100090720     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
020200090720     C                   IF        %subst(prmppt:2:1) = 'D'
020300090720     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
020400090720     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
020500090720     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
020600090720     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
020700090720     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
020800090720     C                                         '"'+%trim(VACCBO)+'"' +wSepCampi+
020900090720     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
021000090720     C                                         '"'+%trim(VACRSD)+'"' +wSepCampi+
021100090720     C                                         '"'+%trim(VACPRD)+'"' +wSepCampi+
021200090720     C                                         '"'+%trim(VACGC1)+'"' +wSepCampi+
021300090720     C                                         '"'+%trim(VACGC2)+'"' +wSepCampi+
021400090720     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
021500090720     C                                         '"'+%trim(VACCTS)+'"' +wSepCampi+
021600090720     C                                         '"'+%trim(VACFTM)+'"' +wSepCampi+
021700090720     C                                         '"'+%trim(VACFIN)+'"' +wSepCampi+
021800090720     C                                         '"'+%trim(VACFAP)+'"' +wSepCampi+
021900090720     C                                         '"'+%trim(VACTSP)+'"' +wSepCampi+
022000090720     C                                                    %trim(wIAS)+wSepCampi+
022100090720     C                                         '"'+%trim(VACVAS)+'"' +wSepCampi+
022200090720     C                                         '"'+%trim(VACNAS)+'"' +wSepCampi+
022300090720     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
022400090720     C                                                    %trim(wPKB)+wSepCampi+
022500090720     C                                                    %trim(wVLB)+wSepCampi+
022600090720     C                                                    %trim(wQFT)+wSepCampi+
022700090720     C                                         '"'+%trim(VACTIC)+'"' +wSepCampi+
022800090720     C                                                    %trim(wCAS)+wSepCampi+
022900090720     C                                         '"'+%trim(VACVCA)+'"' +wSepCampi+
023000090720     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
023100090720     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
023200090720     C                                         '"'+%trim(VACRMA)+'"' +wSepCampi+
023300090720     C                                         '"'+%trim(VACRMO)+'"' +wSepCampi+
023400090720     C                                         '"'+%trim(VACFFD)+'"' +wSepCampi+
023500090720     C                                         '"'+%trim(VACTCR)+'"' +wSepCampi+
023600090720     C                             %trim(%editw(VACDCR:'    /  /  '))+wSepCampi+
023700090720     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
023800090720     C                             %trim(%editw(VACDCM:'    /  /  '))+wSepCampi+
023900090720     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
024000090720     C                                         '"'+%trim(VACTC1)+'"' +wSepCampi+
024100090720     C                                         '"'+%trim(VACTC2)+'"' +wSepCampi+
024200090720     C                                         '"'+%trim(VACCCA)+'"' +wSepCampi+
024300090720     C                             %trim(%editw(VACDLA:'    /  /  '))+wSepCampi+
024400090720     C                             %trim(%editw(VACDAG:'    /  /  '))+wSepCampi+
024500090720     C                                                '"'+%trim(wNNC)+'"'
024600090720     C                   ELSE
024700090720     C*
024800090720     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
024900090720     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
025000090720     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
025100090720     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
025200090720     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
025300090720     C                                          '"'+%trim(VACCBO)+'"'+wSepCampi+
025400090720     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
025500090720     C                                          '"'+%trim(VACRSD)+'"'+wSepCampi+
025600090720     C                                          '"'+%trim(VACPRD)+'"'+wSepCampi+
025700090720     C                                          '"'+%trim(VACGC1)+'"'+wSepCampi+
025800090720     C                                          '"'+%trim(VACGC2)+'"'+wSepCampi+
025900090720     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
026000090720     C                                          '"'+%trim(VACCTS)+'"'+wSepCampi+
026100090720     C                                          '"'+%trim(VACFTM)+'"'+wSepCampi+
026200090720     C                                          '"'+%trim(VACFIN)+'"'+wSepCampi+
026300090720     C                                          '"'+%trim(VACFAP)+'"'+wSepCampi+
026400090720     C                                          '"'+%trim(VACTSP)+'"'+wSepCampi+
026500090720     C                                                    %trim(wIAS)+wSepCampi+
026600090720     C                                          '"'+%trim(VACVAS)+'"'+wSepCampi+
026700090720     C                                          '"'+%trim(VACNAS)+'"'+wSepCampi+
026800090720     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
026900090720     C                                                    %trim(wPKB)+wSepCampi+
027000090720     C                                                    %trim(wVLB)+wSepCampi+
027100090720     C                                                    %trim(wQFT)+wSepCampi+
027200090720     C                                          '"'+%trim(VACTIC)+'"'+wSepCampi+
027300090720     C                                                    %trim(wCAS)+wSepCampi+
027400090720     C                                          '"'+%trim(VACVCA)+'"'+wSepCampi+
027500090720     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
027600090720     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
027700090720     C                                          '"'+%trim(VACRMA)+'"'+wSepCampi+
027800090720     C                                          '"'+%trim(VACRMO)+'"'+wSepCampi+
027900090720     C                                          '"'+%trim(VACFFD)+'"'+wSepCampi+
028000090720     C                                          '"'+%trim(VACTCR)+'"'+wSepCampi+
028100090720     C                                      %trim(%editc(VACDCR:'Q'))+wSepCampi+
028200090720     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
028300090720     C                                      %trim(%editc(VACDCM:'Q'))+wSepCampi+
028400090720     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
028500090720     C                                          '"'+%trim(VACTC1)+'"'+wSepCampi+
028600090720     C                                          '"'+%trim(VACTC2)+'"'+wSepCampi+
028700090720     C                                          '"'+%trim(VACCCA)+'"'+wSepCampi+
028800090720     C                                      %trim(%editc(VACDLA:'Q'))+wSepCampi+
028900090720     C                                      %trim(%editc(VACDAG:'Q'))+wSepCampi+
029000090720     C                                                '"'+%trim(wNNC)+'"'
029100090720     C                   ENDIF
029200090720     C*
029300090720     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
029400090720     C                   IF        %subst(prmppt:5:1) = 'I'
029500090720     C*
029600090720     C* Inizializzo i campi "estensione"
029700090720     C                   EVAL      TASIND = *blanks
029800090720     C                   EVAL      TASCAD = *blanks
029900090720     C                   EVAL      TASLOD = *blanks
030000090720     C*
030100090720     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
030200090720     C     KEYtas30_P    CHAIN     titas30c
030300090720     C                   IF        %found(titas30c)
030400090720     C*
030500090720     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
030600090720     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
030700090720     C                                          '"'+%trim(TASIND)+'"'+wSepCampi+
030800090720     C                                          '"'+%trim(TASCAD)+'"'+wSepCampi+
030900090720     C                                          '"'+%trim(TASLOD)+'"'
031000090720     C                   ENDIF
031100090720     C                   ENDIF
031200090720     C*
031300090720     C                   WRITE     TIVAW000
031400090720     C*
031500090720     C                   ENDSR
031600991027
031700090720
031800090720
031900991027      /TITLE Operazioni iniziali.
032000991027     C     *inzsr        BEGSR
032100991027     C*
032200991027     C     *ENTRY        PLIST
032300991027     C                   parm                    prmppt
032400991027     C     wrkesito      parm      wrkesito      prmesito
032500070426     C*
032600070426     C* Definizione chiavi
032700070426     C*
032800090720     C* Chiave su TITAS30C - Parziale
032900090720     C     KEYtas30_P    KLIST
033000070426     C                   KFLD                    vacAAS
033100070426     C                   KFLD                    vacLNP
033200070426     C                   KFLD                    vacNRS
033300070426     C                   KFLD                    vacNSP
033400090720     C*
033500090720     C* Chiave su TITAH30C - completa
033600090720     C     KEYtah30_C    KLIST
033700090720     C                   KFLD                    vacAAS
033800090720     C                   KFLD                    vacLNP
033900090720     C                   KFLD                    vacNRS
034000090720     C                   KFLD                    vacNSP
034100090720     C                   KFLD                    tahTRC
034200090720     C*
034300090720     C* Chiave su FNBLT01L - completa
034400090720     C     KEYblt01_C    KLIST
034500090720     C                   KFLD                    vacAAS
034600090720     C                   KFLD                    vacLNP
034700090720     C                   KFLD                    vacNRS
034800090720     C                   KFLD                    vacNSP
034900090720     C                   KFLD                    bltNSC
035000991027     C*
035100991027     C                   ENDSR
