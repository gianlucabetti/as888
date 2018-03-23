000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090720     H DFTACTGRP(*NO) BNDDIR('UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600090720     FTIVAWWWT  uF A E             DISK    commit
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
001800140204     D wVAWDTA         s                   like(vawDTA)
001900140204     D wX              s              5  0
002000090720
002100140204
002200140204     d pIn_AAS         s              4s 0
002300140204     d pIn_LNP         s              3s 0
002400140204     d pIn_NRS         s              2s 0
002500140204     d pIn_NSP         s              7s 0
002600140204     d pIn_NCL         s              5s 0
002700140204     d pOut_SGN        s             18    dim(1000)
002800140204     d pOut_NrSGN      s              3s 0
002900140204     d pOut_Esito      s              1
003000140204
003100140204     d UBRTVSGNR       pr                  extpgm('UBRTVSGNR')
003200140204     d pIn_AAS                        4s 0
003300140204     d pIn_LNP                        3s 0
003400140204     d pIn_NRS                        2s 0
003500140204     d pIn_NSP                        7s 0
003600140204     d pIn_NCL                        5s 0
003700140204     d pOut_SGN                      18    dim(1000)
003800140204     d pOut_NrSGN                     3s 0
003900140204     d pOut_Esito                     1
004000090720
004100090720
004200090720     C*
004300090720     C* Effettuo traduzione
004400030908     C                   EXSR      chkRECFILMBR
004500991027     C                   EXSR      traduci
004600090720     C*
004700921023     C                   SETON                                        LR
004800090720
004900090720
005000030908
005100030908     C     chkRECFILMBR  BEGSR
005200030908     C*
005300090720     C                   READ      TIVAW000                               55
005400030908     C*
005500030908     C                   ENDSR
005600090720
005700090720
005800991027
005900991027     C     traduci       BEGSR
006000051007     C*
006100051007     C* Verifico se richiesto un particolare carattere d separatore campi
006200051007     C                   IF        %subst(prmppt:3:1) <> *blanks
006300051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
006400051007     C                   ELSE
006500051007     C                   MOVEL     ','           wSepCampi         1
006600051007     C                   ENDIF
006700051007     C*
006800051007     C* Verifico se richiesto un particolare carattere d separatore decimale
006900051007     C                   SETON                                        60
007000051007     C                   IF        %subst(prmppt:4:1) = ','
007100051007     C                   SETOFF                                       60
007200051007     C                   ENDIF
007300030325     C*
007400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
007600090720     C                   EVAL      VAWDTA  = '"VACAAS"'+wSepCampi+
007700051007     C                                       '"VACLNP"'+wSepCampi+
007800051007     C                                       '"VACNRS"'+wSepCampi+
007900051007     C                                       '"VACNSP"'+wSepCampi+
008000051007     C                                       '"VACMGS"'+wSepCampi+
008100051007     C                                       '"VACCBO"'+wSepCampi+
008200051007     C                                       '"VACLNA"'+wSepCampi+
008300051007     C                                       '"VACRSD"'+wSepCampi+
008400051007     C                                       '"VACPRD"'+wSepCampi+
008500051007     C                                       '"VACGC1"'+wSepCampi+
008600051007     C                                       '"VACGC2"'+wSepCampi+
008700051007     C                                       '"VACCTR"'+wSepCampi+
008800051007     C                                       '"VACCTS"'+wSepCampi+
008900051007     C                                       '"VACFTM"'+wSepCampi+
009000051007     C                                       '"VACFIN"'+wSepCampi+
009100051007     C                                       '"VACFAP"'+wSepCampi+
009200051007     C                                       '"VACTSP"'+wSepCampi+
009300051007     C                                       '"VACIAS"'+wSepCampi+
009400051007     C                                       '"VACVAS"'+wSepCampi+
009500051007     C                                       '"VACNAS"'+wSepCampi+
009600051007     C                                       '"VACNCL"'+wSepCampi+
009700051007     C                                       '"VACPKB"'+wSepCampi+
009800051007     C                                       '"VACVLB"'+wSepCampi+
009900051007     C                                       '"VACQFT"'+wSepCampi+
010000051007     C                                       '"VACTIC"'+wSepCampi+
010100051007     C                                       '"VACCAS"'+wSepCampi+
010200051007     C                                       '"VACVCA"'+wSepCampi+
010300051007     C                                       '"VACCCM"'+wSepCampi+
010400051007     C                                       '"VACRMN"'+wSepCampi+
010500051007     C                                       '"VACRMA"'+wSepCampi+
010600051007     C                                       '"VACRMO"'+wSepCampi+
010700051007     C                                       '"VACFFD"'+wSepCampi+
010800051007     C                                       '"VACTCR"'+wSepCampi+
010900051007     C                                       '"VACDCR"'+wSepCampi+
011000051007     C                                       '"VACHCR"'+wSepCampi+
011100051007     C                                       '"VACDCM"'+wSepCampi+
011200051007     C                                       '"VACHMC"'+wSepCampi+
011300051007     C                                       '"VACTC1"'+wSepCampi+
011400051007     C                                       '"VACTC2"'+wSepCampi+
011500051007     C                                       '"VACCCA"'+wSepCampi+
011600051007     C                                       '"VACDLA"'+wSepCampi+
011700090720     C                                       '"VACDAG"'+wSepCampi+
011800140204     C                                       '"SEGNAC BRT"'
011900070426     C*
012000070426     C                   IF        %subst(prmppt:5:1) = 'I'
012100090720     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
012200070426     C                                       '"VACIND"'+wSepCampi+
012300070426     C                                       '"VACCAD"'+wSepCampi+
012400070426     C                                       '"VACLOD"'
012500070426     C                   ENDIF
012600030325     C*
012700090720     C                   WRITE     TIVAW000
012800030325     C                   ENDIF
012900991027     C*
013000080924     C                   READ      tivgdtmp
013100080924     C                   DOW       not %eof(tivgdtmp)
013200090720     C                   movel     vgddta        dsvac
013300090720     C*
013400090720     C* Valorizzo il buffer d output
013500090720     C                   EXSR      ValBuffOut
013600090720     C*
013700090720     C* Proseguo con la lettura del file
013800080924     C                   DELETE    tivgd000
013900080924     C                   READ      tivgdtmp
014000090720     C*
014100030325     C                   ENDDO
014200991027     C*
014300030325     C                   EVAL      wrkesito = '0'
014400991027     C*
014500910830     C                   ENDSR
014600140204
014700140204
014800140204
014900140204     C     rtvSegnac     BEGSR
015000140204     C*
015100140204     C                   eval      pIn_AAS     = vacAAS
015200140204     C                   eval      pIn_LNP     = vacLNP
015300140204     C                   eval      pIn_NRS     = vacNRS
015400140204     C                   eval      pIn_NSP     = vacNSP
015500140204     C                   eval      pIn_NCL     = vacNCL
015600140204     C*
015700140204     C                   CALL      'UBRTVSGNR'
015800140204     C                   PARM                    pIn_AAS
015900140204     C                   PARM                    pIn_LNP
016000140204     C                   PARM                    pIn_NRS
016100140204     C                   PARM                    pIn_NSP
016200140204     C                   PARM                    pIn_NCL
016300140204     C                   PARM                    pOut_SGN
016400140204     C                   PARM                    pOut_NrSGN
016500140204     C                   PARM                    pOut_Esito
016600140204     C*
016700140204     C                   ENDSR
016800090720
016900090720
017000090720
017100090720     C     ValBuffOut    BEGSR
017200090720     C*
017300090720     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
017400090720     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
017500090720     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
017600090720     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
017700090720     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
017800090720     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
017900090720     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
018000090720     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
018100090720     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
018200090720     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
018300090720     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
018400090720     C*
018500090720     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
018600090720     C                   IF        %subst(prmppt:2:1) = 'D'
018700090720     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
018800090720     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
018900090720     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
019000090720     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
019100090720     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
019200090720     C                                         '"'+%trim(VACCBO)+'"' +wSepCampi+
019300090720     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
019400090720     C                                         '"'+%trim(VACRSD)+'"' +wSepCampi+
019500090720     C                                         '"'+%trim(VACPRD)+'"' +wSepCampi+
019600090720     C                                         '"'+%trim(VACGC1)+'"' +wSepCampi+
019700090720     C                                         '"'+%trim(VACGC2)+'"' +wSepCampi+
019800090720     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
019900090720     C                                         '"'+%trim(VACCTS)+'"' +wSepCampi+
020000090720     C                                         '"'+%trim(VACFTM)+'"' +wSepCampi+
020100090720     C                                         '"'+%trim(VACFIN)+'"' +wSepCampi+
020200090720     C                                         '"'+%trim(VACFAP)+'"' +wSepCampi+
020300090720     C                                         '"'+%trim(VACTSP)+'"' +wSepCampi+
020400090720     C                                                    %trim(wIAS)+wSepCampi+
020500090720     C                                         '"'+%trim(VACVAS)+'"' +wSepCampi+
020600090720     C                                         '"'+%trim(VACNAS)+'"' +wSepCampi+
020700090720     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
020800090720     C                                                    %trim(wPKB)+wSepCampi+
020900090720     C                                                    %trim(wVLB)+wSepCampi+
021000090720     C                                                    %trim(wQFT)+wSepCampi+
021100090720     C                                         '"'+%trim(VACTIC)+'"' +wSepCampi+
021200090720     C                                                    %trim(wCAS)+wSepCampi+
021300090720     C                                         '"'+%trim(VACVCA)+'"' +wSepCampi+
021400090720     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
021500090720     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
021600090720     C                                         '"'+%trim(VACRMA)+'"' +wSepCampi+
021700090720     C                                         '"'+%trim(VACRMO)+'"' +wSepCampi+
021800090720     C                                         '"'+%trim(VACFFD)+'"' +wSepCampi+
021900090720     C                                         '"'+%trim(VACTCR)+'"' +wSepCampi+
022000090720     C                             %trim(%editw(VACDCR:'    /  /  '))+wSepCampi+
022100090720     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
022200090720     C                             %trim(%editw(VACDCM:'    /  /  '))+wSepCampi+
022300090720     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
022400090720     C                                         '"'+%trim(VACTC1)+'"' +wSepCampi+
022500090720     C                                         '"'+%trim(VACTC2)+'"' +wSepCampi+
022600090720     C                                         '"'+%trim(VACCCA)+'"' +wSepCampi+
022700090720     C                             %trim(%editw(VACDLA:'    /  /  '))+wSepCampi+
022800140204     C                             %trim(%editw(VACDAG:'    /  /  '))
022900090720     C                   ELSE
023000090720     C*
023100090720     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
023200090720     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
023300090720     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
023400090720     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
023500090720     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
023600090720     C                                          '"'+%trim(VACCBO)+'"'+wSepCampi+
023700090720     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
023800090720     C                                          '"'+%trim(VACRSD)+'"'+wSepCampi+
023900090720     C                                          '"'+%trim(VACPRD)+'"'+wSepCampi+
024000090720     C                                          '"'+%trim(VACGC1)+'"'+wSepCampi+
024100090720     C                                          '"'+%trim(VACGC2)+'"'+wSepCampi+
024200090720     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
024300090720     C                                          '"'+%trim(VACCTS)+'"'+wSepCampi+
024400090720     C                                          '"'+%trim(VACFTM)+'"'+wSepCampi+
024500090720     C                                          '"'+%trim(VACFIN)+'"'+wSepCampi+
024600090720     C                                          '"'+%trim(VACFAP)+'"'+wSepCampi+
024700090720     C                                          '"'+%trim(VACTSP)+'"'+wSepCampi+
024800090720     C                                                    %trim(wIAS)+wSepCampi+
024900090720     C                                          '"'+%trim(VACVAS)+'"'+wSepCampi+
025000090720     C                                          '"'+%trim(VACNAS)+'"'+wSepCampi+
025100090720     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
025200090720     C                                                    %trim(wPKB)+wSepCampi+
025300090720     C                                                    %trim(wVLB)+wSepCampi+
025400090720     C                                                    %trim(wQFT)+wSepCampi+
025500090720     C                                          '"'+%trim(VACTIC)+'"'+wSepCampi+
025600090720     C                                                    %trim(wCAS)+wSepCampi+
025700090720     C                                          '"'+%trim(VACVCA)+'"'+wSepCampi+
025800090720     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
025900090720     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
026000090720     C                                          '"'+%trim(VACRMA)+'"'+wSepCampi+
026100090720     C                                          '"'+%trim(VACRMO)+'"'+wSepCampi+
026200090720     C                                          '"'+%trim(VACFFD)+'"'+wSepCampi+
026300090720     C                                          '"'+%trim(VACTCR)+'"'+wSepCampi+
026400090720     C                                      %trim(%editc(VACDCR:'Q'))+wSepCampi+
026500090720     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
026600090720     C                                      %trim(%editc(VACDCM:'Q'))+wSepCampi+
026700090720     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
026800090720     C                                          '"'+%trim(VACTC1)+'"'+wSepCampi+
026900090720     C                                          '"'+%trim(VACTC2)+'"'+wSepCampi+
027000090720     C                                          '"'+%trim(VACCCA)+'"'+wSepCampi+
027100090720     C                                      %trim(%editc(VACDLA:'Q'))+wSepCampi+
027200140204     C                                      %trim(%editc(VACDAG:'Q'))
027300090720     C                   ENDIF
027400090720     C*
027500090720     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
027600090720     C                   IF        %subst(prmppt:5:1) = 'I'
027700090720     C*
027800090720     C* Inizializzo i campi "estensione"
027900090720     C                   EVAL      TASIND = *blanks
028000090720     C                   EVAL      TASCAD = *blanks
028100090720     C                   EVAL      TASLOD = *blanks
028200090720     C*
028300140204     C* Reperisco i dati NON presenti sul FNVAC00F dal TITAS30C
028400090720     C     KEYtas30_P    CHAIN     titas30c
028500090720     C                   IF        %found(titas30c)
028600090720     C*
028700090720     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
028800090720     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
028900090720     C                                          '"'+%trim(TASIND)+'"'+wSepCampi+
029000090720     C                                          '"'+%trim(TASCAD)+'"'+wSepCampi+
029100090720     C                                          '"'+%trim(TASLOD)+'"'
029200090720     C                   ENDIF
029300090720     C                   ENDIF
029400140204     C*
029500140204     C* Salvo il valore corrente del buffer di output
029600140204     C                   EVAL      wVAWDTA  = %trim(VAWDTA)
029700090720     C*
029800140204     C* Ciclo per tutti i colli della spedizione aggiungendo il segnacollo BRT completo
029900140204     C                   exsr      rtvSegnac
030000140204     C*
030100140204     C                   if        pOut_Esito = 'S' OR
030200140204     C                             pOut_Esito = 'N'
030300140204     C                   for       wX = 1 to %elem(pOut_SGN)
030400140204     C                   if        pOut_SGN(wX) <> *blanks
030500140204     C                   EVAL      VAWDTA  = %trim(wVAWDTA) + wSepCampi+
030600140204     C                                       %trim(pOut_SGN(wX))
030700140204     C                   WRITE     TIVAW000
030800140204     C                   else
030900140204     C                   leave
031000140204     C                   endif
031100140204     C                   endfor
031200140204     C                   else
031300140204     C                   WRITE     TIVAW000
031400140204     C                   endif
031500090720     C*
031600090720     C                   ENDSR
031700991027
031800090720
031900090720
032000991027      /TITLE Operazioni iniziali.
032100991027     C     *inzsr        BEGSR
032200991027     C*
032300991027     C     *ENTRY        PLIST
032400991027     C                   parm                    prmppt
032500991027     C     wrkesito      parm      wrkesito      prmesito
032600070426     C*
032700070426     C* Definizione chiavi
032800070426     C*
032900090720     C* Chiave su TITAS30C - Parziale
033000090720     C     KEYtas30_P    KLIST
033100070426     C                   KFLD                    vacAAS
033200070426     C                   KFLD                    vacLNP
033300070426     C                   KFLD                    vacNRS
033400070426     C                   KFLD                    vacNSP
033500991027     C*
033600991027     C                   ENDSR
