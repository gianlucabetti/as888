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
010100991027     C*
010200090325     C                   READ      tivgdtmp
010300090325     C                   DOW       not %eof
010400090325     c                   movel     vgddta        tivafds
010500100205     C*
010600100205     C* Reprisco "status" della bolla
010700100205     C                   movel     *blanks       wSTATUS          10
010800100205     C                   z-add     *zeros        wDATASTATUS       8 0
010900100210     C*
011000100210     C                   clear                   wOutDcm
011100100210     C                   clear                   wOutCca
011200100211     C                   clear                   wGcpDur
011300100205     C*
011400100205     C                   exsr      rtvStatus
011500100205     C                   exsr      rtvGiac
011600100211     C*
011700100211     C                   if        wGcpDur > wOutDcm
011800100211     C                   eval      wDATASTATUS = wGcpDur
011900100211     C                   eval      wSTATUS     = 'GIACENZA'
012000100211     C                   else
012100100211     C*
012200100211     C                   if        wOutDcm > *zeros
012300100210     C                   eval      wDATASTATUS = wOutDcm
012400100211     C                   if        wOutCca     = *blanks
012500100211     C                   eval      wSTATUS     = 'CONSEGNATA'
012600100210     C                   endif
012700100211     C                   if        wOutCca     = '2'
012800100211     C                   eval      wSTATUS     = 'RESO'
012900100210     C                   endif
013000100211     C                   if        wOutCca     = '5'
013100100211     C                   eval      wSTATUS     = *blanks
013200100211     C                   endif
013300100211     C                   if        wOutCca     = '6'
013400100211     C                   eval      wSTATUS     = 'RESO'
013500100211     C                   endif
013600100210     C                   endif
013700100211     C                   endif
013800030325     C*
013900100205     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+sepFld+
014000100210     C                                       %trim(%editc(VAFLNP:'X'))+sepFld+
014100100210     C                                       %trim(%editc(VAFNRS:'X'))+sepFld+
014200100210     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
014300100210     C                                          '"'+%trim(VAFTBL)+'"' +sepFld+
014400100210     C                                       %trim(%editc(VAFMGS:'X'))+sepFld+
014500070315     C                                       %trim(%editc(VAFKSC:'Q'))+sepFld+
014600100210     C                                       %trim(%editc(VAFLNA:'X'))+sepFld+
014700070315     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
014800070315     C                                       %trim(%editc(VAFPKB:'Q'))+sepFld+
014900070315     C                                       %trim(%editc(VAFVLF:'Q'))+sepFld+
015000070315     C                                       %trim(%editc(VAFQFT:'Q'))+sepFld+
015100070315     C                                       %trim(%editc(VAFPOR:'Q'))+sepFld+
015200070315     C                                          '"'+%trim(VAFSV1)+'"'+sepFld+
015300070315     C                                       %trim(%editc(VAFVA1:'Q'))+sepFld+
015400070315     C                                          '"'+%trim(VAFSV2)+'"'+sepFld+
015500070315     C                                       %trim(%editc(VAFVA2:'Q'))+sepFld+
015600070315     C                                          '"'+%trim(VAFSV3)+'"'+sepFld+
015700070315     C                                       %trim(%editc(VAFVA3:'Q'))+sepFld+
015800070315     C                                          '"'+%trim(VAFSV4)+'"'+sepFld+
015900070315     C                                       %trim(%editc(VAFVA4:'Q'))+sepFld+
016000070315     C                                          '"'+%trim(VAFSV5)+'"'+sepFld+
016100070315     C                                       %trim(%editc(VAFVA5:'Q'))+sepFld+
016200070315     C                                          '"'+%trim(VAFSV6)+'"'+sepFld+
016300070315     C                                       %trim(%editc(VAFVA6:'Q'))+sepFld+
016400070315     C                                          '"'+%trim(VAFSV7)+'"'+sepFld+
016500070315     C                                       %trim(%editc(VAFVA7:'Q'))+sepFld+
016600070315     C                                          '"'+%trim(VAFSV8)+'"'+sepFld+
016700070315     C                                       %trim(%editc(VAFVA8:'Q'))+sepFld+
016800070315     C                                       %trim(%editc(VAFVAX:'Q'))+sepFld+
016900070315     C                                       %trim(%editc(VAFIMV:'Q'))+sepFld+
017000070315     C                                       %trim(%editc(VAFDFT:'Q'))+sepFld+
017100070315     C                                       %trim(%editc(VAFNFT:'Q'))+sepFld+
017200070315     C                                       %trim(%editc(VAFFIV:'Q'))+sepFld+
017300070315     C                                          '"'+%trim(VAFDIV)+'"'+sepFld+
017400070315     C                                       %trim(%editc(VAFDRT:'Q'))+sepFld+
017500070315     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
017600070315     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
017700070315     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
017800070315     C                                          '"'+%trim(VAFFTC)+'"'+sepFld+
017900070315     C                                          '"'+%trim(VAFTC2)+'"'+sepFld+
018000070315     C                                          '"'+%trim(VAFTSP)+'"'+sepFld+
018100070315     C                                          '"'+%trim(VAFFAP)+'"'+sepFld+
018200070315     C                                          '"'+%trim(VAFFIN)+'"'+sepFld+
018300070315     C                                       %trim(%editc(VAFCTR:'Q'))+sepFld+
018400070315     C                                          '"'+%trim(VAFNAS)+'"'+sepFld+
018500070315     C                                          '"'+%trim(VAFCTS)+'"'+sepFld+
018600070315     C                                          '"'+%trim(VAFCAD)+'"'+sepFld+
018700070315     C                                          '"'+%trim(VAFLOD)+'"'+sepFld+
018800070315     C                                          '"'+%trim(VAFPRD)+'"'+sepFld+
018900070315     C                                          '"'+%trim(VAFNZD)+'"'+sepFld+
019000070315     C                                          '"'+%trim(VAFVAS)+'"'+sepFld+
019100070315     C                                       %trim(%editc(VAFIAS:'Q'))+sepFld+
019200070315     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
019300070315     C                                       %trim(%editc(VAFPKC:'Q'))+sepFld+
019400100205     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
019500100205     C                                       %trim(%editc(VAFVLC:'Q'))+sepFld+
019600100210     C                                          '"'+%trim(wSTATUS)+'"'+sepFld+
019700100210     C                                  %trim(%editc(wDATASTATUS:'Q'))
019800930409     C*
019900100205     C                   WRITE     TIVAW000
020000090325     C                   delete    tivgd000
020100991027     C*
020200090325     C                   READ      tivgdtmp
020300030325     C                   ENDDO
020400991027     C*
020500030325     C                   EVAL      wrkesito = '0'
020600991027     C*
020700910830     C                   ENDSR
020800100205
020900100205
021000100205     C     rtvStatus     BEGSR
021100100205     C*
021200100205     C*
021300100205     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
021400100205     C                   movel     *blanks       wEsito1           1
021500100205     C                   movel     *blanks       wEsito2           1
021600100205     C*
021700100205     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
021800100205     C                                                vafAAS
021900100205     C                                               :vafLNP
022000100205     C                                               :vafNRS
022100100205     C                                               :vafNSP
022200100205     C                                               :pOutLblTyp
022300100205     C                                               :pOutAnnoBO
022400100205     C                                               :pOutLineaParBO
022500100205     C                                               :pOutSerieBO
022600100205     C                                               :pOutNumSpedBO
022700100205     C                                               :pOutDcmBO
022800100205     C                                               :pOutCcaBO
022900100205     C                                               :pOutRblBO))
023000100205     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
023100100210     C                   if        pOutDcmBO > *zeros
023200100210     C                   eval      wOutDcm = pOutDcmBO
023300100210     C                   eval      wOutCca = pOutCcaBO
023400100210     C                   endif
023500100205     C*
023600100205     C                   if        pOutLblTyp = 'O'
023700100205     C*
023800100205     C* Chiamata metodo GetLastChild
023900100205     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
024000100205     C                                                vafAAS
024100100205     C                                               :vafLNP
024200100205     C                                               :vafNRS
024300100205     C                                               :vafNSP
024400100205     C                                               :pOutAnnoFI
024500100205     C                                               :pOutLineaParFI
024600100205     C                                               :pOutSerieFI
024700100205     C                                               :pOutNumSpedFI
024800100205     C                                               :pOutDcmFI
024900100205     C                                               :pOutCcaFI))
025000100210     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
025100100210     C                   if        pOutDcmFI > *zeros
025200100210     C                   eval      wOutDcm = pOutDcmFI
025300100210     C                   eval      wOutCca = pOutCcaFI
025400100210     C                   endif
025500100205     C                   endif
025600100205     C*
025700100205     C                   ENDSR
025800100205
025900100205
026000100205
026100100205     C     rtvGiac       BEGSR
026200100205     C*
026300100205     C* Costruisco la chiave sul file TIGCP51L
026400100205     C                   EVAL      gcpFRG = *zeros
026500100205     C*
026600100205     C* Reperisco le giacenze relative alla bolla corrente
026700100205     C     KEYgcp51_C    SETLL     tigcp51l
026800100205     C                   IF        %equal(tigcp51l)
026900100205     C     KEYgcp51_C    READE     tigcp51l
027000100205     C                   DOW       not %eof(tigcp51l)
027100100205     C*
027200100205     C* Se giacenza NN annullata la considero
027300100205     C                   IF        gcpATB = *blanks
027400100205     C                   IF        gcpDUR > *zeros
027500100211     C                   eval      wGcpDur = gcpDUR
027600100205     C                   ENDIF
027700100205     C                   ENDIF
027800100205     C*
027900100205     C* Proseguo la lettura delle giacenze x la bolla corrente
028000100205     C     KEYgcp51_C    READE     tigcp51l
028100100205     C                   ENDDO
028200100205     C                   ENDIF
028300100205     C*
028400100205     C                   ENDSR
028500100205
028600991027
028700100205
028800100205
028900991027      /TITLE Operazioni iniziali.
029000991027     C     *inzsr        BEGSR
029100991027     C*
029200991027     C     *ENTRY        PLIST
029300991027     C                   parm                    prmppt
029400991027     C     wrkesito      parm      wrkesito      prmesito
029500100205     C*
029600100205     C* Chiave su TIGCP51L - Completa
029700100205     C     KEYgcp51_C    KLIST
029800100205     C                   KFLD                    vafAAS
029900100205     C                   KFLD                    vafLNP
030000100205     C                   KFLD                    vafNRS
030100100205     C                   KFLD                    vafNSP
030200100205     C                   KFLD                    gcpFRG
030300070315     C*
030400070315     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
030500070315     C                   eval      sepFld = %subst(prmppt:2:1)
030600070315     C                   if        sepFld = *blanks
030700070315     C                   eval      sepFld = ','
030800070315     C                   endif
030900991027     C*
031000991027     C                   ENDSR
