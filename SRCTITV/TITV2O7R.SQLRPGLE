000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200130318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500130318     FTITAS30C  IF   E           K DISK
000600100118     FTIVAWWWT  uF A E             DISK    commit
000700100118     FTIVGD00F  O    E             DISK    prefix(w_) rename(TIVGD000:TIVGDWWW)
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300030331     D dsvac         e ds                  extname(fnvac00t)
001400130318     D dorm01        e ds
001500051007     D wIAS            s             14
001600051007     D wPKB            s              8
001700051007     D wVLB            s              6
001800051007     D wQFT            s             14
001900051007     D wCAS            s             14
002000130318
002100130318
002200130318     D*------------------
002300130318     D* LINKING A DEFINIZIONI ESTERNE
002400130318     D*------------------
002500130318     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002600130318     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002700991027
002800130318
002900030908     C                   EXSR      chkRECFILMBR
003000991027     C                   EXSR      traduci
003100921023     C                   SETON                                        LR
003200030908
003300130318
003400030908     C     chkRECFILMBR  BEGSR
003500030908     C*
003600100118     C                   READ      TIVAWWWT                               55
003700030908     C*
003800030908     C                   ENDSR
003900991027
004000130318
004100991027     C     traduci       BEGSR
004200051007     C*
004300051007     C* Verifico se richiesto un particolare carattere d separatore campi
004400051007     C                   IF        %subst(prmppt:3:1) <> *blanks
004500051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
004600051007     C                   ELSE
004700051007     C                   MOVEL     ','           wSepCampi         1
004800051007     C                   ENDIF
004900051007     C*
005000051007     C* Verifico se richiesto un particolare carattere d separatore decimale
005100051007     C                   SETON                                        60
005200051007     C                   IF        %subst(prmppt:4:1) = ','
005300051007     C                   SETOFF                                       60
005400051007     C                   ENDIF
005500030325     C*
005600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005800100118     C                   EVAL      VAWDTA  = '"VACAAS"'+wSepCampi+
005900051007     C                                       '"VACLNP"'+wSepCampi+
006000051007     C                                       '"VACNRS"'+wSepCampi+
006100051007     C                                       '"VACNSP"'+wSepCampi+
006200051007     C                                       '"VACMGS"'+wSepCampi+
006300051007     C                                       '"VACCBO"'+wSepCampi+
006400051007     C                                       '"VACLNA"'+wSepCampi+
006500051007     C                                       '"VACRSD"'+wSepCampi+
006600051007     C                                       '"VACPRD"'+wSepCampi+
006700051007     C                                       '"VACGC1"'+wSepCampi+
006800051007     C                                       '"VACGC2"'+wSepCampi+
006900051007     C                                       '"VACCTR"'+wSepCampi+
007000051007     C                                       '"VACCTS"'+wSepCampi+
007100051007     C                                       '"VACFTM"'+wSepCampi+
007200051007     C                                       '"VACFIN"'+wSepCampi+
007300051007     C                                       '"VACFAP"'+wSepCampi+
007400051007     C                                       '"VACTSP"'+wSepCampi+
007500051007     C                                       '"VACIAS"'+wSepCampi+
007600051007     C                                       '"VACVAS"'+wSepCampi+
007700051007     C                                       '"VACNAS"'+wSepCampi+
007800051007     C                                       '"VACNCL"'+wSepCampi+
007900051007     C                                       '"VACPKB"'+wSepCampi+
008000051007     C                                       '"VACVLB"'+wSepCampi+
008100051007     C                                       '"VACQFT"'+wSepCampi+
008200051007     C                                       '"VACTIC"'+wSepCampi+
008300051007     C                                       '"VACCAS"'+wSepCampi+
008400051007     C                                       '"VACVCA"'+wSepCampi+
008500051007     C                                       '"VACCCM"'+wSepCampi+
008600051007     C                                       '"VACRMN"'+wSepCampi+
008700051007     C                                       '"VACRMA"'+wSepCampi+
008800051007     C                                       '"VACRMO"'+wSepCampi+
008900051007     C                                       '"VACFFD"'+wSepCampi+
009000051007     C                                       '"VACTCR"'+wSepCampi+
009100051007     C                                       '"VACDCR"'+wSepCampi+
009200051007     C                                       '"VACHCR"'+wSepCampi+
009300051007     C                                       '"VACDCM"'+wSepCampi+
009400051007     C                                       '"VACHMC"'+wSepCampi+
009500051007     C                                       '"VACTC1"'+wSepCampi+
009600051007     C                                       '"VACTC2"'+wSepCampi+
009700051007     C                                       '"VACCCA"'+wSepCampi+
009800051007     C                                       '"VACDLA"'+wSepCampi+
009900070426     C                                       '"VACDAG"'
010000070426     C*
010100070426     C                   IF        %subst(prmppt:5:1) = 'I'
010200100118     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
010300070426     C                                       '"VACIND"'+wSepCampi+
010400070426     C                                       '"VACCAD"'+wSepCampi+
010500070426     C                                       '"VACLOD"'
010600070426     C                   ENDIF
010700030325     C*
010800100118     C                   WRITE     TIVAW000
010900030325     C                   ENDIF
011000991027     C*
011100080924     C                   READ      tivgdtmp
011200080924     C                   DOW       not %eof(tivgdtmp)
011300080924     c                   movel     vgddta        dsvac
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
021700070426     C* Inizializzo i campi "estensione"
021800070426     C                   EVAL      TASIND = *blanks
021900070426     C                   EVAL      TASCAD = *blanks
022000070426     C                   EVAL      TASLOD = *blanks
022100070426     C*
022200070426     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
022300070426     C     KEYtas30C     CHAIN     titas30c
022400070426     C                   IF        %found(titas30c)
022500130318     C*
022600130318     C* X le spedizioni con serie consegnate scrivo il relativo record "ORM"
022700130318     C                   if        vacNRS > *zeros AND
022800130318     C                             vacDCM > *zeros AND vacCCA = *blanks
022900130318     C*
023000130318     C* Considero solo se bolla originale
023100130318     C                   movel     *blanks       wEsito1           1
023200130318     C*
023300130318     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
023400130318     C                                                vacAAS
023500130318     C                                               :vacLNP
023600130318     C                                               :vacNRS
023700130318     C                                               :vacNSP
023800130318     C                                               :pOutLblTyp
023900130318     C                                               :pOutAnnoBO
024000130318     C                                               :pOutLineaParBO
024100130318     C                                               :pOutSerieBO
024200130318     C                                               :pOutNumSpedBO
024300130318     C                                               :pOutDcmBO
024400130318     C                                               :pOutCcaBO
024500130318     C                                               :pOutRblBO))
024600130318     C*
024700130318     C                   if        pOutLblTyp = 'O'
024800130320     C*
024900130320     C* Considero solo se la data consegna non è più vecchia di 3 gg
025000130320     C                   if        vacDCM >= %dec(%date()-%days(3))
025100130320     C*
025200130318     C                   movel     *blanks       vaoFLO           20
025300130318     C                   clear                   dorm01
025400130318     C                   eval      §ORMCB  = 'N'
025500130318     C                   eval      §ORMFR  = 'S'
025600130318     C                   eval      §ORMKS  = 'S'
025700130318     C                   eval      §ORMFD  = 'S'
025800130318     C                   eval      §ORCOMC = 'N'
025900130318     C                   eval      vaoFLO = dorm01
026000130318     C*
026100130319     C* "calcolo" la data di ritiro richiesta in base alla data consegna
026200130319     C                   z-add     *zeros        dayWeek           1 0
026300130319     C                   z-add     *zeros        dataRIT           8 0
026400130319     C/EXEC SQL
026500130319     C+ SET :dayWeek = dayofweek_iso(current_date)
026600130319     C/END-EXEC
026700130319     C*
026800130319     C* Se venerdì rimando il ritiro al martedì in quanto destinatari in questo caso
026900130319     C* sono tutti parrucchieri
027000130320     C                   select
027100130320     C                   when      dayWeek = 5
027200130319     C                   eval      dataRIT = %dec(%date()+%days(4) : *ISO)
027300130320     C                   when      dayWeek = 6
027400130320     C                   eval      dataRIT = %dec(%date()+%days(3) : *ISO)
027500130320     C                   when      dayWeek = 7
027600130320     C                   eval      dataRIT = %dec(%date()+%days(2) : *ISO)
027700130320     C                   other
027800130320     C                   eval      dataRIT = %dec(%date()+%days(1) : *ISO)
027900130320     C                   endsl
028000130319     C*
028100130318     C                   clear                   tivgdwww
028200130318     C                   eval      w_vgddta =
028300130319     C                             '050'                             +wSepCampi+
028400130318     C                             'S'                               +wSepCampi+
028500130318     C                             'F'                               +wSepCampi+
028600130318     C                             %editc(datcor:'X')                +wSepCampi+
028700130318     C                             %editc(oracor:'X')                +wSepCampi+
028800130319     C                             %subst(prmppt:14:10)              +wSepCampi+
028900130319     C                             tasRSD                            +wSepCampi+
029000130319     C                             tasIND                            +wSepCampi+
029100130319     C                             tasCAD                            +wSepCampi+
029200130319     C                             tasLOD                            +wSepCampi+
029300130319     C                             tasPRD                            +wSepCampi+
029400130319     C                             tasNZD                            +wSepCampi+
029500171019     C                             %subst(prmppt:24:10)              +wSepCampi+
029600130319     C                             '0900'                            +wSepCampi+
029700130318     C                             'VARIE'                           +wSepCampi+
029800130319     C                             %editc(vacNCL:'X')                +wSepCampi+
029900130408     C                             %editc(5:'X')                     +wSepCampi+
030000130318     C                             'O'                               +wSepCampi+
030100130319     C                             %editc(vacCCM:'X')                +wSepCampi+
030200130318     C                             'N'                               +wSepCampi+
030300130319     C                             %editc(vacCTR:'X')                +wSepCampi+
030400130319     C                             %editc(dataRIT:'X')               +wSepCampi+
030500130318     C                             vacRMA                            +wSepCampi+
030600130318     C                             vaoFLO                            +wSepCampi
030700130318     C                   exsr      wriVGD
030800130320     C                   endif
030900130318     C                   endif
031000130318     C                   endif
031100130318     C*
031200130318     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
031300130318     C                   IF        %subst(prmppt:5:1) = 'I'
031400070426     C*
031500070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
031600100118     C                   EVAL      VAWDTA  = %trim(VAWDTA) + wSepCampi+
031700070426     C                                          '"'+%trim(TASIND)+'"'+wSepCampi+
031800070426     C                                          '"'+%trim(TASCAD)+'"'+wSepCampi+
031900070426     C                                          '"'+%trim(TASLOD)+'"'
032000070426     C                   ENDIF
032100070426     C                   ENDIF
032200930409     C*
032300100118     C                   WRITE     TIVAW000
032400080924     C                   DELETE    tivgd000
032500991027     C*
032600080924     C                   READ      tivgdtmp
032700030325     C                   ENDDO
032800991027     C*
032900030325     C                   EVAL      wrkesito = '0'
033000991027     C*
033100910830     C                   ENDSR
033200100118
033300100118
033400100118
033500100118      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
033600100118     C     wriVGD        BEGSR
033700100118     C*
033800130318     C                   eval      w_vgdTIP = 'FC'
033900100118     C                   eval      w_vgdKSU = %subst(prmppt:6:8)
034000100118     C                   eval      w_vgdTSC = 'WW'
034100100118     C                   eval      w_vgdDAT = datcor
034200130318     C                   eval      w_vgdPGM = 'TITV2O7R'
034300100118     C                   write     tivgdwww
034400100118     C*
034500100118     C                   ENDSR
034600100118     C*------------------------------------------------------------------------*
034700100118
034800100118
034900100118
035000991027
035100991027      /TITLE Operazioni iniziali.
035200991027     C     *inzsr        BEGSR
035300991027     C*
035400991027     C     *ENTRY        PLIST
035500991027     C                   parm                    prmppt
035600991027     C     wrkesito      parm      wrkesito      prmesito
035700070426     C*
035800070426     C* Definizione chiavi
035900070426     C*
036000070426     C* Chiave su TITAS30C - Completa
036100070426     C     KEYtas30C     KLIST
036200070426     C                   KFLD                    vacAAS
036300070426     C                   KFLD                    vacLNP
036400070426     C                   KFLD                    vacNRS
036500070426     C                   KFLD                    vacNSP
036600100118     C*
036700130318     C                   time                    wn14             14 0
036800130318     C                   movel     wn14          oracor            6 0
036900100118     C                   z-add     *zeros        datcor            8 0
037000100118     C                   eval      datcor = %dec(%date() : *ISO)
037100991027     C*
037200991027     C                   ENDSR
