000100090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200030325     H DECEDIT('0.') DATEDIT(*DMY.)
000300991027
000400090320     Ftivgdtmp  uF   E             DISK
000500100212     FTIVAWWWT  uF A E             DISK    commit
000600080527     Ftitas30c  if   E           k DISK
000700100212     FtitaA30c  if   E           k DISK
000800100216     Fcnaco00f  if   E           k DISK
000900100216     Fcnind00f  if   E           k DISK
001000120406     Ffnblt01l  IF   E           K DISK    usropn
001100120406     F                                     extfile(LibFileBLT01)
001200120406     Ffiars01l  IF   E           K DISK    usropn
001300120406     F                                     extfile(LibFileARS01)
001400991027
001500991027     D prmppt          s             50
001600991027     D prmesito        s              1
001700991027     D wrkesito        s                   like(prmesito)
001800090320     D fnvabds       e ds                  extname(fnvab00t)
001900100202     D wIAS            s             14
002000100202     D wPKB            s              8
002100141104     D wPKB_RIL        s              8
002200100202     D wVLB            s              6
002300141104     D wVLB_RIL        s              9
002400100202     D wQFT            s             14
002500100202     D wCAS            s             14
002600100202     D wVMD            s             14
002700120406     D wVAWDTA         s                   like(VAWDTA)
002800100212     D wTAARSC         s                   like(TAARSC)
002900100212     D wTAAIND         s                   like(TAAIND)
003000100212     D wTAACAP         s                   like(TAACAP)
003100100212     D wTAALOC         s                   like(TAALOC)
003200100212     D wTAAPRV         s                   like(TAAPRV)
003300120406     D wARSNOT_C       s                   like(ARSNOT)
003400100212
003500120406     D LibFileBLT01    s             21A   inz
003600120406     D LibFileARS01    s             21A   inz
003700120406     D currSysNeta     s              8A   inz('*NULL')
003800080613
003900120406
004000080613     D*------------------
004100080613     D* LINKING A DEFINIZIONI ESTERNE
004200080613     D*------------------
004300080613     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
004400100212     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
004500120406     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004600120406     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
004700120406
004800991027
004900120406     C*
005000120406     C* Reperisco il sistema AS/400 corrente
005100120406     C                   callp     UBRTVNETA_Rtv(currSysNeta)
005200120406     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
005300120406     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
005400120406     C                   eval      LibFileARS01 = 'FILTRA201/FIARS01L'
005500120406     C                   else
005600120406     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
005700120406     C                   eval      LibFileARS01 = 'FILTRAPRD/FIARS01L'
005800120406     C                   endif
005900120406     C*
006000120406     C                   if        not %open(FNBLT01L)
006100120406     C                   open      FNBLT01L
006200120406     C                   endif
006300120406     C*
006400120406     C                   if        not %open(FIARS01L)
006500120406     C                   open      FIARS01L
006600120406     C                   endif
006700120406     C*
006800030908     C                   EXSR      chkRECFILMBR
006900991027     C                   EXSR      traduci
007000120406     C*
007100120406     C                   if        %open(FNBLT01L)
007200120406     C                   close     FNBLT01L
007300120406     C                   endif
007400120406     C*
007500120406     C                   if        %open(FIARS01L)
007600120406     C                   close     FIARS01L
007700120406     C                   endif
007800120406     C*
007900921023     C                   SETON                                        LR
008000120406
008100120406
008200030908
008300030908     C     chkRECFILMBR  BEGSR
008400030908     C*
008500100212     C                   READ      TIVAWWWT                               55
008600030908     C*
008700030908     C                   ENDSR
008800120406
008900120406
009000991027
009100991027     C     traduci       BEGSR
009200100202     C*
009300100202     C* Verifico se richiesto un particolare carattere d separatore campi
009400100202     C                   IF        %subst(prmppt:2:1) <> *blanks
009500100202     C                   EVAL      wSepCampi = %subst(prmppt:2:1)
009600100202     C                   ELSE
009700100202     C                   MOVEL     ','           wSepCampi         1
009800100202     C                   ENDIF
009900100202     C*
010000100202     C* Verifico se richiesto un particolare carattere d separatore decimale
010100100202     C                   SETON                                        60
010200100202     C                   IF        %subst(prmppt:3:1) = ','
010300100202     C                   SETOFF                                       60
010400100202     C                   ENDIF
010500030325     C*
010600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010800100212     C                   EVAL      VAWDTA  = '"VABATB"'+wSepCampi+
010900100202     C                                       '"VABCCM"'+wSepCampi+
011000100202     C                                       '"VABLNP"'+wSepCampi+
011100100202     C                                       '"VABAAS"'+wSepCampi+
011200100202     C                                       '"VABMGS"'+wSepCampi+
011300100202     C                                       '"VABNRS"'+wSepCampi+
011400100202     C                                       '"VABNSP"'+wSepCampi+
011500100202     C                                       '"VABCBO"'+wSepCampi+
011600100202     C                                       '"VABLNA"'+wSepCampi+
011700100202     C                                       '"VABRSD"'+wSepCampi+
011800100202     C                                       '"VABRD2"'+wSepCampi+
011900100202     C                                       '"VABIND"'+wSepCampi+
012000100202     C                                       '"VABCAD"'+wSepCampi+
012100100202     C                                       '"VABLOD"'+wSepCampi+
012200100202     C                                       '"VABPRD"'+wSepCampi+
012300100202     C                                       '"VABNZD"'+wSepCampi+
012400100202     C                                       '"VABGC1"'+wSepCampi+
012500100202     C                                       '"VABGC2"'+wSepCampi+
012600100202     C                                       '"VABCTR"'+wSepCampi+
012700100202     C                                       '"VABTSP"'+wSepCampi+
012800100202     C                                       '"VABIAS"'+wSepCampi+
012900100202     C                                       '"VABVAS"'+wSepCampi+
013000100202     C                                       '"VABNAS"'+wSepCampi+
013100100202     C                                       '"VABNCL"'+wSepCampi+
013200100202     C                                       '"VABPKB"'+wSepCampi+
013300100202     C                                       '"VABVLB"'+wSepCampi+
013400100202     C                                       '"VABQFT"'+wSepCampi+
013500100202     C                                       '"VABCAS"'+wSepCampi+
013600100202     C                                       '"VABTIC"'+wSepCampi+
013700100202     C                                       '"VABVCA"'+wSepCampi+
013800100202     C                                       '"VABGCA"'+wSepCampi+
013900100202     C                                       '"VABRMN"'+wSepCampi+
014000100202     C                                       '"VABRMA"'+wSepCampi+
014100100202     C                                       '"VABNCD"'+wSepCampi+
014200100202     C                                       '"VABNCA"'+wSepCampi+
014300100202     C                                       '"VABXCO"'+wSepCampi+
014400100202     C                                       '"VABNOT"'+wSepCampi+
014500100202     C                                       '"VABNT2"'+wSepCampi+
014600100202     C                                       '"VABZNC"'+wSepCampi+
014700100202     C                                       '"VABCTM"'+wSepCampi+
014800100202     C                                       '"VABFFD"'+wSepCampi+
014900100202     C                                       '"VABDCR"'+wSepCampi+
015000100202     C                                       '"VABTCR"'+wSepCampi+
015100100202     C                                       '"VABHCR"'+wSepCampi+
015200100202     C                                       '"VABCTS"'+wSepCampi+
015300100202     C                                       '"VABFTM"'+wSepCampi+
015400100202     C                                       '"VABVMD"'+wSepCampi+
015500100202     C                                       '"VABVAD"'+wSepCampi+
015600100202     C                                       '"VABGMA"'+wSepCampi+
015700100202     C                                       '"VABGGA"'+wSepCampi+
015800100202     C                                       '"VABGVA"'+wSepCampi+
015900100202     C                                       '"VABTC1"'+wSepCampi+
016000100202     C                                       '"VABTC2"'+wSepCampi+
016100100202     C                                       '"VABSCL"'+wSepCampi+
016200100202     C                                       '"VABANT"'+wSepCampi+
016300100202     C                                       '"VABRMO"'+wSepCampi+
016400100202     C                                       '"VABCMO"'+wSepCampi+
016500141118     C                                       '"VABNMO"'+wSepCampi+
016600141118     C                                       '"SIGLA_OP"'
016700100212     C*
016800100212     C                   if        %subst(prmppt:8:1) = 'S'
016900100212     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
017000100212     C                                      '"TAARSC"'+wSepCampi+
017100100212     C                                      '"TAAIND"'+wSepCampi+
017200100212     C                                      '"TAACAP"'+wSepCampi+
017300100212     C                                      '"TAALOC"'+wSepCampi+
017400100212     C                                      '"TAAPRV"'
017500120406     C                   endif
017600120406     C*
017700120406     C                   if        %subst(prmppt:9:1) = 'S'
017800120406     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
017900120406     C                                      '"SEGNACOLLO_CLI"'
018000141104     C*
018100141104     C                   if        %subst(prmppt:10:1) = 'S'
018200141104     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
018300141104     C                                      '"PESO_RIL"'  +wSepCampi+
018400141104     C                                      '"VOLUME_RIL"'
018500120406     C                   endif
018600141104     C                   endif
018700120406     C*
018800100212     C                   WRITE     TIVAW000
018900030325     C                   ENDIF
019000991027     C*
019100090320     C                   READ      tivgdtmp
019200090320     C                   DOW       not %eof(tivgdtmp)
019300090320     c                   movel     vgddta        fnvabds
019400080613     C*
019500080613     C                   setoff                                       31
019600080613     C*
019700080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
019800080613     C                   if        %subst(prmppt:6:1) = 'O'
019900080613     C*
020000080613     C* Chiamata metodo GetLblTyp
020100080613     C                   if        UBLBLSPE_GetLblTyp(VABAAS
020200080613     C                                               :VABLNP
020300080613     C                                               :VABNRS
020400080613     C                                               :VABNSP
020500100212     C                                               :pOutLblTyp
020600100212     C                                               :pOutAnnoBO
020700100212     C                                               :pOutLineaParBO
020800100212     C                                               :pOutSerieBO
020900100212     C                                               :pOutNumSpedBO
021000100212     C                                               :pOutDcmBO
021100100212     C                                               :pOutCcaBO
021200100212     C                                               :pOutRblBO) = 0
021300100212     C                   if        pOutLblTyp <> 'O'
021400080613     C                   seton                                        31
021500080613     C                   endif
021600080613     C                   endif
021700080613     C                   endif
021800080618     C*
021900080618     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
022000080618     C                   if        %subst(prmppt:7:1) = 'S'
022100080618     C                   if        VABNRS = *zeros
022200080618     C                   seton                                        31
022300080618     C                   endif
022400080618     C                   endif
022500080613     C*
022600080613     C* Se tutto ok => procedo
022700080613     C                   if        *in31 = *off
022800080527     C*
022900080527     C* Se richiesto nei parametri d ingresso valorizzo da data consegna richiesta con il
023000080527     C* proprio valore
023100080527     C                   z-add     VABDCR        savVABDCR         8 0
023200080527     C                   if        %subst(prmppt:5:1) = 'R'
023300080527     C                   exsr      reptas
023400080527     C                   endif
023500100225     C*
023600100225     C* Se richiesto nei parametri d ingresso ritorno anche i dati del mittente originale
023700100225     C                   if        %subst(prmppt:8:1) = 'S'
023800100225     C                   exsr      reptaA
023900100225     C                   endif
024000100202     C*
024100100202     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
024200100202     C  N60              EVAL      wIAS = %trim(%editw(VABIAS:'        0 ,   '))
024300100202     C   60              EVAL      wIAS = %trim(%editw(VABIAS:'        0 .   '))
024400100202     C  N60              EVAL      wPKB = %trim(%editw(VABPKB:'    0 , '))
024500100202     C   60              EVAL      wPKB = %trim(%editw(VABPKB:'    0 . '))
024600100202     C  N60              EVAL      wVLB = %trim(%editw(VABVLB:'0 ,   '))
024700100202     C   60              EVAL      wVLB = %trim(%editw(VABVLB:'0 .   '))
024800100202     C  N60              EVAL      wQFT = %trim(%editw(VABQFT:'        0 ,   '))
024900100202     C   60              EVAL      wQFT = %trim(%editw(VABQFT:'        0 .   '))
025000100202     C  N60              EVAL      wCAS = %trim(%editw(VABCAS:'        0 ,   '))
025100100202     C   60              EVAL      wCAS = %trim(%editw(VABCAS:'        0 .   '))
025200100202     C  N60              EVAL      wVMD = %trim(%editw(VABVMD:'        0 ,   '))
025300100202     C   60              EVAL      wVMD = %trim(%editw(VABVMD:'        0 .   '))
025400100202     C*
025500030325     C*
025600100212     C                   EVAL      VAWDTA  =   '"'+%trim(VABATB)+'"' +wSepCampi+
025700100202     C                                      %trim(%editc(VABCCM:'Q'))+wSepCampi+
025800100202     C                                      %trim(%editc(VABLNP:'Q'))+wSepCampi+
025900100202     C                                      %trim(%editc(VABAAS:'Q'))+wSepCampi+
026000100202     C                                      %trim(%editc(VABMGS:'Q'))+wSepCampi+
026100100202     C                                      %trim(%editc(VABNRS:'Q'))+wSepCampi+
026200100202     C                                      %trim(%editc(VABNSP:'Q'))+wSepCampi+
026300100202     C                                         '"'+%trim(VABCBO)+'"' +wSepCampi+
026400100202     C                                      %trim(%editc(VABLNA:'Q'))+wSepCampi+
026500100202     C                                         '"'+%trim(VABRSD)+'"' +wSepCampi+
026600100202     C                                         '"'+%trim(VABRD2)+'"' +wSepCampi+
026700100202     C                                         '"'+%trim(VABIND)+'"' +wSepCampi+
026800100202     C                                         '"'+%trim(VABCAD)+'"' +wSepCampi+
026900100202     C                                         '"'+%trim(VABLOD)+'"' +wSepCampi+
027000100202     C                                         '"'+%trim(VABPRD)+'"' +wSepCampi+
027100100202     C                                         '"'+%trim(VABNZD)+'"' +wSepCampi+
027200100202     C                                         '"'+%trim(VABGC1)+'"' +wSepCampi+
027300100202     C                                         '"'+%trim(VABGC2)+'"' +wSepCampi+
027400100202     C                                      %trim(%editc(VABCTR:'Q'))+wSepCampi+
027500100202     C                                         '"'+%trim(VABTSP)+'"' +wSepCampi+
027600100202     C                                                    %trim(wIAS)+wSepCampi+
027700100202     C                                         '"'+%trim(VABVAS)+'"' +wSepCampi+
027800100202     C                                         '"'+%trim(VABNAS)+'"' +wSepCampi+
027900100202     C                                      %trim(%editc(VABNCL:'Q'))+wSepCampi+
028000100202     C                                                    %trim(wPKB)+wSepCampi+
028100100202     C                                                    %trim(wVLB)+wSepCampi+
028200100202     C                                                    %trim(wQFT)+wSepCampi+
028300100202     C                                                    %trim(wCAS)+wSepCampi+
028400100202     C                                         '"'+%trim(VABTIC)+'"' +wSepCampi+
028500100202     C                                         '"'+%trim(VABVCA)+'"' +wSepCampi+
028600100202     C                                         '"'+%trim(VABGCA)+'"' +wSepCampi+
028700100202     C                                      %trim(%editc(VABRMN:'Q'))+wSepCampi+
028800100202     C                                         '"'+%trim(VABRMA)+'"' +wSepCampi+
028900100202     C                                      %trim(%editc(VABNCD:'Q'))+wSepCampi+
029000100202     C                                      %trim(%editc(VABNCA:'Q'))+wSepCampi+
029100100202     C                                         '"'+%trim(VABXCO)+'"' +wSepCampi+
029200100202     C                                         '"'+%trim(VABNOT)+'"' +wSepCampi+
029300100202     C                                         '"'+%trim(VABNT2)+'"' +wSepCampi+
029400100202     C                                      %trim(%editc(VABZNC:'Q'))+wSepCampi+
029500100202     C                                         '"'+%trim(VABCTM)+'"' +wSepCampi+
029600100202     C                                         '"'+%trim(VABFFD)+'"' +wSepCampi+
029700100202     C                                      %trim(%editc(VABDCR:'Q'))+wSepCampi+
029800100202     C                                         '"'+%trim(VABTCR)+'"' +wSepCampi+
029900100202     C                                      %trim(%editc(VABHCR:'Q'))+wSepCampi+
030000100202     C                                         '"'+%trim(VABCTS)+'"' +wSepCampi+
030100100202     C                                         '"'+%trim(VABFTM)+'"' +wSepCampi+
030200100202     C                                                    %trim(wVMD)+wSepCampi+
030300100202     C                                         '"'+%trim(VABVAD)+'"' +wSepCampi+
030400100202     C                                         '"'+%trim(VABGMA)+'"' +wSepCampi+
030500100202     C                                         '"'+%trim(VABGGA)+'"' +wSepCampi+
030600100202     C                                         '"'+%trim(VABGVA)+'"' +wSepCampi+
030700100202     C                                         '"'+%trim(VABTC1)+'"' +wSepCampi+
030800100202     C                                         '"'+%trim(VABTC2)+'"' +wSepCampi+
030900100202     C                                         '"'+%trim(VABSCL)+'"' +wSepCampi+
031000100202     C                                      %trim(%editc(VABANT:'Q'))+wSepCampi+
031100100202     C                                         '"'+%trim(VABRMO)+'"' +wSepCampi+
031200100202     C                                         '"'+%trim(VABCMO)+'"' +wSepCampi+
031300141118     C                                         '"'+%trim(VABNMO)+'"' +wSepCampi+
031400141118     C                                         '"'+%trim(tasSOP)+'"'
031500100212     C*
031600100212     C                   if        %subst(prmppt:8:1) = 'S'
031700100212     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
031800100212     C                                        '"'+%trim(wTAARSC)+'"' +wSepCampi+
031900100212     C                                        '"'+%trim(wTAAIND)+'"' +wSepCampi+
032000100212     C                                        '"'+%trim(wTAACAP)+'"' +wSepCampi+
032100100212     C                                        '"'+%trim(wTAALOC)+'"' +wSepCampi+
032200100212     C                                        '"'+%trim(wTAAPRV)+'"'
032300100212     C                   endif
032400080527     C*
032500080527     C                   z-add     savVABDCR     VABDCR
032600120406     C*
032700120406     C* Se richiesto nei parametri d ingresso ritorno anche i dati del segnacollo cliente
032800120406     C                   if        %subst(prmppt:9:1) = 'S'
032900120406     C*
033000120406     C                   eval      wVAWDTA = VAWDTA
033100120406     C*
033200120406     C* Aggancio i dettagli colli e ciclo x ciascuno
033300120406     C     KEYblt01_P    setll     fnblt01l
033400120406     C                   if        %found(fnblt01l)
033500120406     C     KEYblt01_P    reade     fnblt01l
033600120406     C                   dow       not %eof(fnblt01l)
033700120406     C*
033800120406     C                   clear                   wARSNOT_C
033900120406     C*
034000120406     C* Reperisco il segnacollo cliente ("chi sono")
034100120406     C                   eval      arsTRC = 'C'
034200120406     C     KEYars01_C    chain     fiars01l
034300120406     C                   if        %found(fiars01l)
034400120406     C                   eval      wARSNOT_C = arsNOT
034500120406     C                   endif
034600120406     C*
034700120406     C                   EVAL      VAWDTA  = %trim(wVAWDTA)+wSepCampi+
034800141104     C                                        '"'+%trim(wARSNOT_C)+'"'
034900141104     C                   if        %subst(prmppt:10:1) = 'S'
035000141104     C                   if        bltFVC <> 'P'
035100141104     C                   eval      bltVUC = *zeros
035200141104     C                   endif
035300141104     C                   if        bltFPC <> 'P'
035400141104     C                   eval      bltPUC = *zeros
035500141104     C                   endif
035600141104     C  N60              EVAL      wPKB_RIL = %trim(%editw(bltPUC:'  0 ,   '))
035700141104     C   60              EVAL      wPKB_RIL = %trim(%editw(bltPUC:'  0 .   '))
035800141104     C  N60              EVAL      wVLB_RIL = %trim(%editw(bltVUC:'0 ,      '))
035900141104     C   60              EVAL      wVLB_RIL = %trim(%editw(bltVUC:'0 .      '))
036000141104     C                   EVAL      VAWDTA  = %trim(VAWDTA)  +wSepCampi+
036100141104     C                                       %trim(wPKB_RIL)+wSepCampi+
036200141104     C                                       %trim(wVLB_RIL)
036300141104     C                   endif
036400141104     C*
036500120406     C                   WRITE     TIVAW000
036600120406     C*
036700120406     C* Scorro il segnacollo successivo
036800120406     C     KEYtas30_P    reade     fnblt01l
036900120406     C                   enddo
037000120406     C*
037100120406     C* Se nn trovo il dettaglio segnacolli BRT in filiale scrivo almeno un record
037200120406     C                   else
037300120406     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+'""'
037400120406     C                   WRITE     TIVAW000
037500120406     C                   endif
037600930409     C*
037700120406     C                   else
037800100212     C                   WRITE     TIVAW000
037900120406     C                   endif
038000120406     C*
038100080613     C                   endif
038200080613     C*
038300090320     C                   delete    tivgd000
038400991027     C*
038500090320     C                   READ      tivgdtmp
038600030325     C                   ENDDO
038700991027     C*
038800030325     C                   EVAL      wrkesito = '0'
038900991027     C*
039000910830     C                   ENDSR
039100080527
039200080527
039300080527
039400080527     C*
039500080527     C     reptas        BEGSR
039600080527     C*
039700080527     C* Posizionamento diretto
039800080527     C     KEYtas30_P    chain     titas30c
039900080527     C                   if        %found(titas30c)
040000080527     C                   move      tasDCR        VABDCR
040100080527     C                   endif
040200080527     C*
040300080527     C                   ENDSR
040400100212
040500100212
040600100212
040700100212     C*
040800100212     C     reptaA        BEGSR
040900100212     C*
041000100212     C                   clear                   wTAARSC
041100100212     C                   clear                   wTAAIND
041200100212     C                   clear                   wTAACAP
041300100212     C                   clear                   wTAALOC
041400100212     C                   clear                   wTAAPRV
041500100216     C                   movel     *blanks       wTIPCCM           4
041600100216     C*
041700100225     C                   move(p)   tasCCM        wTIPCCM
041800100216     C                   if        wTIPCCM = '8888' or
041900100216     C                             wTIPCCM = '9999'
042000100212     C*
042100100212     C                   eval      taaTRC = 'M'
042200100212     C*
042300100212     C* Posizionamento diretto
042400100212     C     KEYtaA30_C    chain     titaA30c
042500100212     C                   if        %found(titaA30c)
042600100216     C                   movel     taaRSC        wTAARSC
042700100216     C                   movel     taaIND        wTAAIND
042800100216     C                   movel     taaCAP        wTAACAP
042900100216     C                   movel     taaLOC        wTAALOC
043000100216     C                   movel     taaPRV        wTAAPRV
043100100212     C                   endif
043200100216     C*
043300100216     C                   else
043400100216     C                   eval      acokut = 1
043500100216     C     KEYaco00_C    chain     cnaco00f
043600100216     C                   if        %found(cnaco00f)
043700100216     C                   movel     acoRAG        wTAARSC
043800100216     C     KEYind00_C    chain     cnind00f
043900100216     C                   if        %found(cnind00f)
044000100216     C                   movel     indVIA        wTAAIND
044100100216     C                   movel     indCAP        wTAACAP
044200100216     C                   movel     indCIT        wTAALOC
044300100216     C                   movel     indPRV        wTAAPRV
044400100216     C                   endif
044500100216     C                   endif
044600100216     C                   endif
044700100212     C*
044800100212     C                   ENDSR
044900080527
045000080527
045100991027
045200991027      /TITLE Operazioni iniziali.
045300991027     C     *inzsr        BEGSR
045400991027     C*
045500991027     C     *ENTRY        PLIST
045600991027     C                   parm                    prmppt
045700991027     C     wrkesito      parm      wrkesito      prmesito
045800100222     C*
045900100222     C                   eval      acokcc = 151
046000080527     C*
046100080527     C     KEYtas30_P    KLIST
046200080527     C                   KFLD                    vabaas
046300080527     C                   KFLD                    vablnp
046400080527     C                   KFLD                    vabnrs
046500080527     C                   KFLD                    vabnsp
046600100212     C*
046700100212     C     KEYtaA30_C    KLIST
046800100212     C                   KFLD                    vabaas
046900100212     C                   KFLD                    vablnp
047000100212     C                   KFLD                    vabnrs
047100100212     C                   KFLD                    vabnsp
047200100212     C                   KFLD                    taatrc
047300100216     C*
047400100216     C     KEYaco00_C    KLIST
047500100216     C                   KFLD                    acokut
047600100216     C                   KFLD                    acokcc
047700100216     C                   KFLD                    vabccm
047800100216     C*
047900100216     C     KEYind00_C    KLIST
048000100216     C                   KFLD                    acokut
048100100216     C                   KFLD                    acokcc
048200100216     C                   KFLD                    vabccm
048300120406     C*
048400120406     C     KEYblt01_P    KLIST
048500120406     C                   KFLD                    vabaas
048600120406     C                   KFLD                    vablnp
048700120406     C                   KFLD                    vabnrs
048800120406     C                   KFLD                    vabnsp
048900120406     C*
049000120406     C     KEYars01_C    KLIST
049100120406     C                   KFLD                    bltFLS
049200120406     C                   KFLD                    bltLNA
049300120406     C                   KFLD                    bltNRS
049400120406     C                   KFLD                    bltNSC
049500120406     C                   KFLD                    arsTRC
049600991027     C*
049700991027     C                   ENDSR
