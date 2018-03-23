000100090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200090709     H DECEDIT('0,') DATEDIT(*DMY.)
000300991027
000400090320     Ftivgdtmp  uF   E             DISK
000500090708     FTIVAWWWT  uF A E             DISK    commit
000600080527     Ftitas30c  if   E           k DISK
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100090708     D fnvabds       e ds                  extname(fnvab00t)
001200080613
001300080613     D*------------------
001400080613     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
001500080613     D*------------------
001600080613     D InAnno                         4S 0
001700080613     D InLineaPar                     3S 0
001800080613     D InSerie                        2S 0
001900080613     D InNumSped                      7S 0
002000080613     D OutLblTyp                      1A
002100080613     D OutAnnoBO                      4S 0
002200080613     D OutLineaParBO                  3S 0
002300080613     D OutSerieBO                     2S 0
002400080613     D OutNumSpedBO                   7S 0
002500080613     D OutDcmBO                       8S 0
002600080613     D OutCcaBO                       1A
002700080806     D OutRblBO                       1A
002800080613     D OutAnnoFI                      4S 0
002900080613     D OutLineaParFI                  3S 0
003000080613     D OutSerieFI                     2S 0
003100080613     D OutNumSpedFI                   7S 0
003200080613     D OutDcmFI                       8S 0
003300080613     D OutCcaFI                       1A
003400080613     D*------------------
003500080613     D* LINKING A DEFINIZIONI ESTERNE
003600080613     D*------------------
003700080613     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003800991027
003900030908     C                   EXSR      chkRECFILMBR
004000991027     C                   EXSR      traduci
004100921023     C                   SETON                                        LR
004200030908
004300030908     C     chkRECFILMBR  BEGSR
004400030908     C*
004500090708     C                   READ      TIVAWWWT                               55
004600030908     C*
004700030908     C                   ENDSR
004800991027
004900991027     C     traduci       BEGSR
005000030325     C*
005100030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005200030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005300090709     C                   EVAL      VAWDTA =  '"VABATB"'+';'+
005400090709     C                                       '"VABCCM"'+';'+
005500090709     C                                       '"VABLNP"'+';'+
005600090709     C                                       '"VABAAS"'+';'+
005700090709     C                                       '"VABMGS"'+';'+
005800090709     C                                       '"VABNRS"'+';'+
005900090709     C                                       '"VABNSP"'+';'+
006000090709     C                                       '"VABCBO"'+';'+
006100090709     C                                       '"VABLNA"'+';'+
006200090709     C                                       '"VABRSD"'+';'+
006300090709     C                                       '"VABRD2"'+';'+
006400090709     C                                       '"VABIND"'+';'+
006500090709     C                                       '"VABCAD"'+';'+
006600090709     C                                       '"VABLOD"'+';'+
006700090709     C                                       '"VABPRD"'+';'+
006800090709     C                                       '"VABNZD"'+';'+
006900090709     C                                       '"VABGC1"'+';'+
007000090709     C                                       '"VABGC2"'+';'+
007100090709     C                                       '"VABCTR"'+';'+
007200090709     C                                       '"VABTSP"'+';'+
007300090709     C                                       '"VABIAS"'+';'+
007400090709     C                                       '"VABVAS"'+';'+
007500090709     C                                       '"VABNAS"'+';'+
007600090709     C                                       '"VABNCL"'+';'+
007700090709     C                                       '"VABPKB"'+';'+
007800090709     C                                       '"VABVLB"'+';'+
007900090709     C                                       '"VABQFT"'+';'+
008000090709     C                                       '"VABCAS"'+';'+
008100090709     C                                       '"VABTIC"'+';'+
008200090709     C                                       '"VABVCA"'+';'+
008300090709     C                                       '"VABGCA"'+';'+
008400090709     C                                       '"VABRMN"'+';'+
008500090709     C                                       '"VABRMA"'+';'+
008600090709     C                                       '"VABNCD"'+';'+
008700090709     C                                       '"VABNCA"'+';'+
008800090709     C                                       '"VABXCO"'+';'+
008900090709     C                                       '"VABNOT"'+';'+
009000090709     C                                       '"VABNT2"'+';'+
009100090709     C                                       '"VABZNC"'+';'+
009200090709     C                                       '"VABCTM"'+';'+
009300090709     C                                       '"VABFFD"'+';'+
009400090709     C                                       '"VABDCR"'+';'+
009500090709     C                                       '"VABTCR"'+';'+
009600090709     C                                       '"VABHCR"'+';'+
009700090709     C                                       '"VABCTS"'+';'+
009800090709     C                                       '"VABFTM"'+';'+
009900090709     C                                       '"VABVMD"'+';'+
010000090709     C                                       '"VABVAD"'+';'+
010100090709     C                                       '"VABGMA"'+';'+
010200090709     C                                       '"VABGGA"'+';'+
010300090709     C                                       '"VABGVA"'+';'+
010400090709     C                                       '"VABTC1"'+';'+
010500090709     C                                       '"VABTC2"'+';'+
010600090709     C                                       '"VABSCL"'+';'+
010700090709     C                                       '"VABANT"'+';'+
010800090709     C                                       '"VABRMO"'+';'+
010900090709     C                                       '"VABCMO"'+';'+
011000030331     C                                       '"VABNMO"'
011100030325     C*
011200090708     C                   WRITE     TIVAW000
011300030325     C                   ENDIF
011400991027     C*
011500090320     C                   READ      tivgdtmp
011600090320     C                   DOW       not %eof(tivgdtmp)
011700090320     c                   movel     vgddta        fnvabds
011800080613     C*
011900080613     C                   setoff                                       31
012000090708     C*
012100090708     C* Ritorno al cliente solo le bolle in contrassegno
012200090708     C                   if        vabCAS = *zeros
012300090708     C                   seton                                        31
012400090708     C                   endif
012500080613     C*
012600080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
012700080613     C                   if        %subst(prmppt:6:1) = 'O'
012800080613     C*
012900080613     C* Chiamata metodo GetLblTyp
013000080613     C                   if        UBLBLSPE_GetLblTyp(VABAAS
013100080613     C                                               :VABLNP
013200080613     C                                               :VABNRS
013300080613     C                                               :VABNSP
013400080613     C                                               :OutLblTyp
013500080613     C                                               :OutAnnoBO
013600080613     C                                               :OutLineaParBO
013700080613     C                                               :OutSerieBO
013800080613     C                                               :OutNumSpedBO
013900080613     C                                               :OutDcmBO
014000080806     C                                               :OutCcaBO
014100080806     C                                               :OutRblBO) = 0
014200080613     C                   if        OutLblTyp <> 'O'
014300080613     C                   seton                                        31
014400080613     C                   endif
014500080613     C                   endif
014600080613     C                   endif
014700080618     C*
014800080618     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
014900080618     C                   if        %subst(prmppt:7:1) = 'S'
015000080618     C                   if        VABNRS = *zeros
015100080618     C                   seton                                        31
015200080618     C                   endif
015300080618     C                   endif
015400080613     C*
015500080613     C* Se tutto ok => procedo
015600080613     C                   if        *in31 = *off
015700080527     C*
015800080527     C* Se richiesto nei parametri d ingresso valorizzo da data consegna richiesta con il
015900080527     C* proprio valore
016000080527     C                   z-add     VABDCR        savVABDCR         8 0
016100080527     C                   if        %subst(prmppt:5:1) = 'R'
016200080527     C                   exsr      reptas
016300080527     C                   endif
016400030325     C*
016500090709     C                   EVAL      VAWDTA =     '"'+%trim(VABATB)+'"'+';'+
016600090709     C                                       %trim(%editc(VABCCM:'Q'))+';'+
016700090709     C                                       %trim(%editc(VABLNP:'Q'))+';'+
016800090709     C                                       %trim(%editc(VABAAS:'Q'))+';'+
016900090709     C                                       %trim(%editc(VABMGS:'Q'))+';'+
017000090709     C                                       %trim(%editc(VABNRS:'Q'))+';'+
017100090709     C                                       %trim(%editc(VABNSP:'Q'))+';'+
017200090709     C                                          '"'+%trim(VABCBO)+'"'+';'+
017300090709     C                                       %trim(%editc(VABLNA:'Q'))+';'+
017400090709     C                                          '"'+%trim(VABRSD)+'"'+';'+
017500090709     C                                          '"'+%trim(VABRD2)+'"'+';'+
017600090709     C                                          '"'+%trim(VABIND)+'"'+';'+
017700090709     C                                          '"'+%trim(VABCAD)+'"'+';'+
017800090709     C                                          '"'+%trim(VABLOD)+'"'+';'+
017900090709     C                                          '"'+%trim(VABPRD)+'"'+';'+
018000090709     C                                          '"'+%trim(VABNZD)+'"'+';'+
018100090709     C                                          '"'+%trim(VABGC1)+'"'+';'+
018200090709     C                                          '"'+%trim(VABGC2)+'"'+';'+
018300090709     C                                       %trim(%editc(VABCTR:'Q'))+';'+
018400090709     C                                          '"'+%trim(VABTSP)+'"'+';'+
018500090709     C                                       %trim(%editc(VABIAS:'Q'))+';'+
018600090709     C                                          '"'+%trim(VABVAS)+'"'+';'+
018700090709     C                                          '"'+%trim(VABNAS)+'"'+';'+
018800090709     C                                       %trim(%editc(VABNCL:'Q'))+';'+
018900090709     C                                       %trim(%editc(VABPKB:'Q'))+';'+
019000090709     C                                       %trim(%editc(VABVLB:'Q'))+';'+
019100090709     C                                       %trim(%editc(VABQFT:'Q'))+';'+
019200090709     C                                       %trim(%editc(VABCAS:'Q'))+';'+
019300090709     C                                          '"'+%trim(VABTIC)+'"'+';'+
019400090709     C                                          '"'+%trim(VABVCA)+'"'+';'+
019500090709     C                                          '"'+%trim(VABGCA)+'"'+';'+
019600090709     C                                       %trim(%editc(VABRMN:'Q'))+';'+
019700090709     C                                          '"'+%trim(VABRMA)+'"'+';'+
019800090709     C                                       %trim(%editc(VABNCD:'Q'))+';'+
019900090709     C                                       %trim(%editc(VABNCA:'Q'))+';'+
020000090709     C                                          '"'+%trim(VABXCO)+'"'+';'+
020100090709     C                                          '"'+%trim(VABNOT)+'"'+';'+
020200090709     C                                          '"'+%trim(VABNT2)+'"'+';'+
020300090709     C                                       %trim(%editc(VABZNC:'Q'))+';'+
020400090709     C                                          '"'+%trim(VABCTM)+'"'+';'+
020500090709     C                                          '"'+%trim(VABFFD)+'"'+';'+
020600090709     C                                       %trim(%editc(VABDCR:'Q'))+';'+
020700090709     C                                          '"'+%trim(VABTCR)+'"'+';'+
020800090709     C                                       %trim(%editc(VABHCR:'Q'))+';'+
020900090709     C                                          '"'+%trim(VABCTS)+'"'+';'+
021000090709     C                                          '"'+%trim(VABFTM)+'"'+';'+
021100090709     C                                       %trim(%editc(VABVMD:'Q'))+';'+
021200090709     C                                          '"'+%trim(VABVAD)+'"'+';'+
021300090709     C                                          '"'+%trim(VABGMA)+'"'+';'+
021400090709     C                                          '"'+%trim(VABGGA)+'"'+';'+
021500090709     C                                          '"'+%trim(VABGVA)+'"'+';'+
021600090709     C                                          '"'+%trim(VABTC1)+'"'+';'+
021700090709     C                                          '"'+%trim(VABTC2)+'"'+';'+
021800090709     C                                          '"'+%trim(VABSCL)+'"'+';'+
021900090709     C                                       %trim(%editc(VABANT:'Q'))+';'+
022000090709     C                                          '"'+%trim(VABRMO)+'"'+';'+
022100090709     C                                          '"'+%trim(VABCMO)+'"'+';'+
022200030331     C                                          '"'+%trim(VABNMO)+'"'
022300080527     C*
022400080527     C                   z-add     savVABDCR     VABDCR
022500930409     C*
022600090708     C                   WRITE     TIVAW000
022700080613     C                   endif
022800080613     C*
022900090320     C                   delete    tivgd000
023000991027     C*
023100090320     C                   READ      tivgdtmp
023200030325     C                   ENDDO
023300991027     C*
023400030325     C                   EVAL      wrkesito = '0'
023500991027     C*
023600910830     C                   ENDSR
023700080527
023800080527
023900080527
024000080527     C*
024100080527     C     reptas        BEGSR
024200080527     C*
024300080527     C* Posizionamento diretto
024400080527     C     KEYtas30_P    chain     titas30c
024500080527     C                   if        %found(titas30c)
024600080527     C                   move      tasDCR        VABDCR
024700080527     C                   endif
024800080527     C*
024900080527     C                   ENDSR
025000080527
025100080527
025200991027
025300991027      /TITLE Operazioni iniziali.
025400991027     C     *inzsr        BEGSR
025500991027     C*
025600991027     C     *ENTRY        PLIST
025700991027     C                   parm                    prmppt
025800991027     C     wrkesito      parm      wrkesito      prmesito
025900080527     C*
026000080527     C     KEYtas30_P    KLIST
026100080527     C                   KFLD                    vabaas
026200080527     C                   KFLD                    vablnp
026300080527     C                   KFLD                    vabnrs
026400080527     C                   KFLD                    vabnsp
026500991027     C*
026600991027     C                   ENDSR
