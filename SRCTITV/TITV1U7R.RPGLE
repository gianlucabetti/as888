000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100205     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400090325     Ftivgdtmp  uF   E             DISK
000500100205     FTIVAWWWT  UF A E             DISK
000600100205     FTIGCP51L  IF   E           K DISK
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100090325     D tivafds       e ds                  extname(tivaf00t)
001200070315     D sepFld          s              1
001300100205
001400100205     D*------------------
001500100205     D* LINKING A DEFINIZIONI ESTERNE
001600100205     D*------------------
001700100205     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
001800100205     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
001900100205
002000100210     D wOutDcm         s                   like(pOutDcmBO)
002100100210     D wOutCca         s                   like(pOutCcaBO)
002200100211     D wGcpDur         s                   like(gcpDUR)
002300100205
002400991027
002500030908     C                   EXSR      chkRECFILMBR
002600991027     C                   EXSR      traduci
002700921023     C                   SETON                                        LR
002800030908
002900030908     C     chkRECFILMBR  BEGSR
003000030908     C*
003100100205     C                   READ      TIVAWWWT                               55
003200030908     C*
003300030908     C                   ENDSR
003400991027
003500991027     C     traduci       BEGSR
003600030325     C*
003700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
003800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
003900100205     C                   EVAL      VAWDTA  = '"VAFAAS"'+sepFld+
004000070315     C                                       '"VAFLNP"'+sepFld+
004100070315     C                                       '"VAFNRS"'+sepFld+
004200070315     C                                       '"VAFNSP"'+sepFld+
004300070315     C                                       '"VAFTBL"'+sepFld+
004400070315     C                                       '"VAFMGS"'+sepFld+
004500070315     C                                       '"VAFKSC"'+sepFld+
004600070315     C                                       '"VAFLNA"'+sepFld+
004700070315     C                                       '"VAFNCL"'+sepFld+
004800070315     C                                       '"VAFPKB"'+sepFld+
004900070315     C                                       '"VAFVLF"'+sepFld+
005000070315     C                                       '"VAFQFT"'+sepFld+
005100070315     C                                       '"VAFPOR"'+sepFld+
005200070315     C                                       '"VAFSV1"'+sepFld+
005300070315     C                                       '"VAFVA1"'+sepFld+
005400070315     C                                       '"VAFSV2"'+sepFld+
005500070315     C                                       '"VAFVA2"'+sepFld+
005600070315     C                                       '"VAFSV3"'+sepFld+
005700070315     C                                       '"VAFVA3"'+sepFld+
005800070315     C                                       '"VAFSV4"'+sepFld+
005900070315     C                                       '"VAFVA4"'+sepFld+
006000070315     C                                       '"VAFSV5"'+sepFld+
006100070315     C                                       '"VAFVA5"'+sepFld+
006200070315     C                                       '"VAFSV6"'+sepFld+
006300070315     C                                       '"VAFVA6"'+sepFld+
006400070315     C                                       '"VAFSV7"'+sepFld+
006500070315     C                                       '"VAFVA7"'+sepFld+
006600070315     C                                       '"VAFSV8"'+sepFld+
006700070315     C                                       '"VAFVA8"'+sepFld+
006800070315     C                                       '"VAFVAX"'+sepFld+
006900070315     C                                       '"VAFIMV"'+sepFld+
007000070315     C                                       '"VAFDFT"'+sepFld+
007100070315     C                                       '"VAFNFT"'+sepFld+
007200070315     C                                       '"VAFFIV"'+sepFld+
007300070315     C                                       '"VAFDIV"'+sepFld+
007400070315     C                                       '"VAFDRT"'+sepFld+
007500070315     C                                       '"VAFRMN"'+sepFld+
007600070315     C                                       '"VAFRMA"'+sepFld+
007700070315     C                                       '"VAFRMX"'+sepFld+
007800070315     C                                       '"VAFFTC"'+sepFld+
007900070315     C                                       '"VAFTC2"'+sepFld+
008000070315     C                                       '"VAFTSP"'+sepFld+
008100070315     C                                       '"VAFFAP"'+sepFld+
008200070315     C                                       '"VAFFIN"'+sepFld+
008300070315     C                                       '"VAFCTR"'+sepFld+
008400070315     C                                       '"VAFNAS"'+sepFld+
008500070315     C                                       '"VAFCTS"'+sepFld+
008600070315     C                                       '"VAFCAD"'+sepFld+
008700070315     C                                       '"VAFLOD"'+sepFld+
008800070315     C                                       '"VAFPRD"'+sepFld+
008900070315     C                                       '"VAFNZD"'+sepFld+
009000070315     C                                       '"VAFVAS"'+sepFld+
009100070315     C                                       '"VAFIAS"'+sepFld+
009200070315     C                                       '"VAFFPC"'+sepFld+
009300070315     C                                       '"VAFPKC"'+sepFld+
009400070315     C                                       '"VAFFVC"'+sepFld+
009500100205     C                                       '"VAFVLC"'+sepFld+
009600100205     C                                        '"STATO"'+sepFld+
009700100208     C                                   '"DATA_STATO"'
009800030325     C*
009900100205     C                   WRITE     TIVAW000
010000030325     C                   ENDIF
010100101112     C*
010200101112     C                   z-add     *zeros        wCntRec           6 0
010300991027     C*
010400090325     C                   READ      tivgdtmp
010500090325     C                   DOW       not %eof
010600101112     C                   add       1             wCntRec
010700090325     c                   movel     vgddta        tivafds
010800100205     C*
010900100205     C* Reprisco "status" della bolla
011000100205     C                   movel     *blanks       wSTATUS          10
011100100205     C                   z-add     *zeros        wDATASTATUS       8 0
011200100210     C*
011300100210     C                   clear                   wOutDcm
011400100210     C                   clear                   wOutCca
011500100211     C                   clear                   wGcpDur
011600100205     C*
011700100205     C                   exsr      rtvStatus
011800100205     C                   exsr      rtvGiac
011900100211     C*
012000100211     C                   if        wGcpDur > wOutDcm
012100100211     C                   eval      wDATASTATUS = wGcpDur
012200100211     C                   eval      wSTATUS     = 'GIACENZA'
012300100211     C                   else
012400100211     C*
012500100211     C                   if        wOutDcm > *zeros
012600100210     C                   eval      wDATASTATUS = wOutDcm
012700100211     C                   if        wOutCca     = *blanks
012800100211     C                   eval      wSTATUS     = 'CONSEGNATA'
012900100210     C                   endif
013000100211     C                   if        wOutCca     = '2'
013100100211     C                   eval      wSTATUS     = 'RESO'
013200100210     C                   endif
013300100211     C                   if        wOutCca     = '5'
013400100211     C                   eval      wSTATUS     = *blanks
013500100211     C                   endif
013600100211     C                   if        wOutCca     = '6'
013700100211     C                   eval      wSTATUS     = 'RESO'
013800100211     C                   endif
013900100210     C                   endif
014000100211     C                   endif
014100030325     C*
014200101112     C                   IF        wCntRec = 1
014300101112     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'P'))+sepFld+
014400101112     C                                       %trim(%editc(VAFLNP:'X'))+sepFld+
014500101112     C                                       %trim(%editc(VAFNRS:'X'))+sepFld+
014600101112     C                                       %trim(%editc(VAFNSP:'P'))+sepFld+
014700101112     C                                          '"'+%trim(VAFTBL)+'"' +sepFld+
014800101112     C                                       %trim(%editc(VAFMGS:'X'))+sepFld+
014900101112     C                                       %trim(%editc(VAFKSC:'P'))+sepFld+
015000101112     C                                       %trim(%editc(VAFLNA:'X'))+sepFld+
015100101112     C                                       %trim(%editc(VAFNCL:'P'))+sepFld+
015200101112     C                                       %trim(%editc(VAFPKB:'P'))+sepFld+
015300101112     C                                       %trim(%editc(VAFVLF:'P'))+sepFld+
015400101112     C                                       %trim(%editc(VAFQFT:'P'))+sepFld+
015500101112     C                                       %trim(%editc(VAFPOR:'P'))+sepFld+
015600101112     C                                          '"'+%trim(VAFSV1)+'"'+sepFld+
015700101112     C                                       %trim(%editc(VAFVA1:'P'))+sepFld+
015800101112     C                                          '"'+%trim(VAFSV2)+'"'+sepFld+
015900101112     C                                       %trim(%editc(VAFVA2:'P'))+sepFld+
016000101112     C                                          '"'+%trim(VAFSV3)+'"'+sepFld+
016100101112     C                                       %trim(%editc(VAFVA3:'P'))+sepFld+
016200101112     C                                          '"'+%trim(VAFSV4)+'"'+sepFld+
016300101112     C                                       %trim(%editc(VAFVA4:'P'))+sepFld+
016400101112     C                                          '"'+%trim(VAFSV5)+'"'+sepFld+
016500101112     C                                       %trim(%editc(VAFVA5:'P'))+sepFld+
016600101112     C                                          '"'+%trim(VAFSV6)+'"'+sepFld+
016700101112     C                                       %trim(%editc(VAFVA6:'P'))+sepFld+
016800101112     C                                          '"'+%trim(VAFSV7)+'"'+sepFld+
016900101112     C                                       %trim(%editc(VAFVA7:'P'))+sepFld+
017000101112     C                                          '"'+%trim(VAFSV8)+'"'+sepFld+
017100101112     C                                       %trim(%editc(VAFVA8:'P'))+sepFld+
017200101112     C                                       %trim(%editc(VAFVAX:'P'))+sepFld+
017300101112     C                                       %trim(%editc(VAFIMV:'P'))+sepFld+
017400101112     C                                       %trim(%editc(VAFDFT:'P'))+sepFld+
017500101112     C                                       %trim(%editc(VAFNFT:'P'))+sepFld+
017600101112     C                                       %trim(%editc(VAFFIV:'P'))+sepFld+
017700101112     C                                          '"'+%trim(VAFDIV)+'"'+sepFld+
017800101112     C                                       %trim(%editc(VAFDRT:'P'))+sepFld+
017900101112     C                                       %trim(%editc(VAFRMN:'P'))+sepFld+
018000101112     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
018100101112     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
018200101112     C                                          '"'+%trim(VAFFTC)+'"'+sepFld+
018300101112     C                                          '"'+%trim(VAFTC2)+'"'+sepFld+
018400101112     C                                          '"'+%trim(VAFTSP)+'"'+sepFld+
018500101112     C                                          '"'+%trim(VAFFAP)+'"'+sepFld+
018600101112     C                                          '"'+%trim(VAFFIN)+'"'+sepFld+
018700101112     C                                       %trim(%editc(VAFCTR:'P'))+sepFld+
018800101112     C                                          '"'+%trim(VAFNAS)+'"'+sepFld+
018900101112     C                                          '"'+%trim(VAFCTS)+'"'+sepFld+
019000101112     C                                          '"'+%trim(VAFCAD)+'"'+sepFld+
019100101112     C                                          '"'+%trim(VAFLOD)+'"'+sepFld+
019200101112     C                                          '"'+%trim(VAFPRD)+'"'+sepFld+
019300101112     C                                          '"'+%trim(VAFNZD)+'"'+sepFld+
019400101112     C                                          '"'+%trim(VAFVAS)+'"'+sepFld+
019500101112     C                                       %trim(%editc(VAFIAS:'P'))+sepFld+
019600101112     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
019700101112     C                                       %trim(%editc(VAFPKC:'P'))+sepFld+
019800101112     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
019900101112     C                                       %trim(%editc(VAFVLC:'P'))+sepFld+
020000101112     C                                          '"'+%trim(wSTATUS)+'"'+sepFld+
020100101112     C                                  %trim(%editc(wDATASTATUS:'P'))
020200101112     C                   ELSE
020300100205     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+sepFld+
020400100210     C                                       %trim(%editc(VAFLNP:'X'))+sepFld+
020500100210     C                                       %trim(%editc(VAFNRS:'X'))+sepFld+
020600100210     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
020700100210     C                                          '"'+%trim(VAFTBL)+'"' +sepFld+
020800100210     C                                       %trim(%editc(VAFMGS:'X'))+sepFld+
020900070315     C                                       %trim(%editc(VAFKSC:'Q'))+sepFld+
021000100210     C                                       %trim(%editc(VAFLNA:'X'))+sepFld+
021100070315     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
021200070315     C                                       %trim(%editc(VAFPKB:'Q'))+sepFld+
021300070315     C                                       %trim(%editc(VAFVLF:'Q'))+sepFld+
021400070315     C                                       %trim(%editc(VAFQFT:'Q'))+sepFld+
021500070315     C                                       %trim(%editc(VAFPOR:'Q'))+sepFld+
021600070315     C                                          '"'+%trim(VAFSV1)+'"'+sepFld+
021700070315     C                                       %trim(%editc(VAFVA1:'Q'))+sepFld+
021800070315     C                                          '"'+%trim(VAFSV2)+'"'+sepFld+
021900070315     C                                       %trim(%editc(VAFVA2:'Q'))+sepFld+
022000070315     C                                          '"'+%trim(VAFSV3)+'"'+sepFld+
022100070315     C                                       %trim(%editc(VAFVA3:'Q'))+sepFld+
022200070315     C                                          '"'+%trim(VAFSV4)+'"'+sepFld+
022300070315     C                                       %trim(%editc(VAFVA4:'Q'))+sepFld+
022400070315     C                                          '"'+%trim(VAFSV5)+'"'+sepFld+
022500070315     C                                       %trim(%editc(VAFVA5:'Q'))+sepFld+
022600070315     C                                          '"'+%trim(VAFSV6)+'"'+sepFld+
022700070315     C                                       %trim(%editc(VAFVA6:'Q'))+sepFld+
022800070315     C                                          '"'+%trim(VAFSV7)+'"'+sepFld+
022900070315     C                                       %trim(%editc(VAFVA7:'Q'))+sepFld+
023000070315     C                                          '"'+%trim(VAFSV8)+'"'+sepFld+
023100070315     C                                       %trim(%editc(VAFVA8:'Q'))+sepFld+
023200070315     C                                       %trim(%editc(VAFVAX:'Q'))+sepFld+
023300070315     C                                       %trim(%editc(VAFIMV:'Q'))+sepFld+
023400070315     C                                       %trim(%editc(VAFDFT:'Q'))+sepFld+
023500070315     C                                       %trim(%editc(VAFNFT:'Q'))+sepFld+
023600070315     C                                       %trim(%editc(VAFFIV:'Q'))+sepFld+
023700070315     C                                          '"'+%trim(VAFDIV)+'"'+sepFld+
023800070315     C                                       %trim(%editc(VAFDRT:'Q'))+sepFld+
023900070315     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
024000070315     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
024100070315     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
024200070315     C                                          '"'+%trim(VAFFTC)+'"'+sepFld+
024300070315     C                                          '"'+%trim(VAFTC2)+'"'+sepFld+
024400070315     C                                          '"'+%trim(VAFTSP)+'"'+sepFld+
024500070315     C                                          '"'+%trim(VAFFAP)+'"'+sepFld+
024600070315     C                                          '"'+%trim(VAFFIN)+'"'+sepFld+
024700070315     C                                       %trim(%editc(VAFCTR:'Q'))+sepFld+
024800070315     C                                          '"'+%trim(VAFNAS)+'"'+sepFld+
024900070315     C                                          '"'+%trim(VAFCTS)+'"'+sepFld+
025000070315     C                                          '"'+%trim(VAFCAD)+'"'+sepFld+
025100070315     C                                          '"'+%trim(VAFLOD)+'"'+sepFld+
025200070315     C                                          '"'+%trim(VAFPRD)+'"'+sepFld+
025300070315     C                                          '"'+%trim(VAFNZD)+'"'+sepFld+
025400070315     C                                          '"'+%trim(VAFVAS)+'"'+sepFld+
025500070315     C                                       %trim(%editc(VAFIAS:'Q'))+sepFld+
025600070315     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
025700070315     C                                       %trim(%editc(VAFPKC:'Q'))+sepFld+
025800100205     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
025900100205     C                                       %trim(%editc(VAFVLC:'Q'))+sepFld+
026000100210     C                                          '"'+%trim(wSTATUS)+'"'+sepFld+
026100100210     C                                  %trim(%editc(wDATASTATUS:'Q'))
026200101112     C                   ENDIF
026300930409     C*
026400100205     C                   WRITE     TIVAW000
026500090325     C                   delete    tivgd000
026600991027     C*
026700090325     C                   READ      tivgdtmp
026800030325     C                   ENDDO
026900991027     C*
027000030325     C                   EVAL      wrkesito = '0'
027100991027     C*
027200910830     C                   ENDSR
027300100205
027400100205
027500100205     C     rtvStatus     BEGSR
027600100205     C*
027700100205     C*
027800100205     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
027900100205     C                   movel     *blanks       wEsito1           1
028000100205     C                   movel     *blanks       wEsito2           1
028100100205     C*
028200100205     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
028300100205     C                                                vafAAS
028400100205     C                                               :vafLNP
028500100205     C                                               :vafNRS
028600100205     C                                               :vafNSP
028700100205     C                                               :pOutLblTyp
028800100205     C                                               :pOutAnnoBO
028900100205     C                                               :pOutLineaParBO
029000100205     C                                               :pOutSerieBO
029100100205     C                                               :pOutNumSpedBO
029200100205     C                                               :pOutDcmBO
029300100205     C                                               :pOutCcaBO
029400100205     C                                               :pOutRblBO))
029500100205     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
029600100210     C                   if        pOutDcmBO > *zeros
029700100210     C                   eval      wOutDcm = pOutDcmBO
029800100210     C                   eval      wOutCca = pOutCcaBO
029900100210     C                   endif
030000100205     C*
030100100205     C                   if        pOutLblTyp = 'O'
030200100205     C*
030300100205     C* Chiamata metodo GetLastChild
030400100205     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
030500100205     C                                                vafAAS
030600100205     C                                               :vafLNP
030700100205     C                                               :vafNRS
030800100205     C                                               :vafNSP
030900100205     C                                               :pOutAnnoFI
031000100205     C                                               :pOutLineaParFI
031100100205     C                                               :pOutSerieFI
031200100205     C                                               :pOutNumSpedFI
031300100205     C                                               :pOutDcmFI
031400100205     C                                               :pOutCcaFI))
031500100210     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
031600100210     C                   if        pOutDcmFI > *zeros
031700100210     C                   eval      wOutDcm = pOutDcmFI
031800100210     C                   eval      wOutCca = pOutCcaFI
031900100210     C                   endif
032000100205     C                   endif
032100100205     C*
032200100205     C                   ENDSR
032300100205
032400100205
032500100205
032600100205     C     rtvGiac       BEGSR
032700100205     C*
032800100205     C* Costruisco la chiave sul file TIGCP51L
032900100205     C                   EVAL      gcpFRG = *zeros
033000100205     C*
033100100205     C* Reperisco le giacenze relative alla bolla corrente
033200100205     C     KEYgcp51_C    SETLL     tigcp51l
033300100205     C                   IF        %equal(tigcp51l)
033400100205     C     KEYgcp51_C    READE     tigcp51l
033500100205     C                   DOW       not %eof(tigcp51l)
033600100205     C*
033700100205     C* Se giacenza NN annullata la considero
033800100205     C                   IF        gcpATB = *blanks
033900100205     C                   IF        gcpDUR > *zeros
034000100211     C                   eval      wGcpDur = gcpDUR
034100100205     C                   ENDIF
034200100205     C                   ENDIF
034300100205     C*
034400100205     C* Proseguo la lettura delle giacenze x la bolla corrente
034500100205     C     KEYgcp51_C    READE     tigcp51l
034600100205     C                   ENDDO
034700100205     C                   ENDIF
034800100205     C*
034900100205     C                   ENDSR
035000100205
035100991027
035200100205
035300100205
035400991027      /TITLE Operazioni iniziali.
035500991027     C     *inzsr        BEGSR
035600991027     C*
035700991027     C     *ENTRY        PLIST
035800991027     C                   parm                    prmppt
035900991027     C     wrkesito      parm      wrkesito      prmesito
036000100205     C*
036100100205     C* Chiave su TIGCP51L - Completa
036200100205     C     KEYgcp51_C    KLIST
036300100205     C                   KFLD                    vafAAS
036400100205     C                   KFLD                    vafLNP
036500100205     C                   KFLD                    vafNRS
036600100205     C                   KFLD                    vafNSP
036700100205     C                   KFLD                    gcpFRG
036800070315     C*
036900070315     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
037000070315     C                   eval      sepFld = %subst(prmppt:2:1)
037100070315     C                   if        sepFld = *blanks
037200070315     C                   eval      sepFld = ','
037300070315     C                   endif
037400991027     C*
037500991027     C                   ENDSR
