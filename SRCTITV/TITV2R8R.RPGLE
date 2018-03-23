000100090709     H DECEDIT('0,') DATEDIT(*DMY.)
000200140217     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400090320     Ftivgdtmp  uF   E             DISK
000500090708     FTIVAWWWT  uF A E             DISK    commit
000600140217     FFIAR501L  if   E           K DISK    usropn
000700140217     F                                     extfile(LibFileAR5)
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090708     D fnvabds       e ds                  extname(fnvab00t)
001300140217     D dar5emd       e ds
001400140217
001500140217
001600140217     D LibFileAR5      s             21A   inz
001700140217     D currSysNeta     s              8A   inz('*NULL')
001800140217
001900140217     D*------------------
002000140217     D* LINKING A DEFINIZIONI ESTERNE
002100140217     D*------------------
002200140217     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
002300140217     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
002400140217     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002500140217     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002600130516
002700130516
002800991027
002900140217
003000140217     C*
003100140217     C* Reperisco il sistema AS/400 corrente
003200140217     C                   callp     UBRTVNETA_Rtv(currSysNeta)
003300140217     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
003400140217     C                   eval      LibFileAR5 = 'FILTRA201/FIAR501L'
003500140217     C                   else
003600140217     C                   eval      LibFileAR5 = 'FILTRAPRD/FIAR501L'
003700140217     C                   endif
003800140217     C*
003900140217     C                   if        not %open(FIAR501L)
004000140217     C                   open      FIAR501L
004100140217     C                   endif
004200140217     C*
004300030908     C                   EXSR      chkRECFILMBR
004400991027     C                   EXSR      traduci
004500140217     C*
004600140217     C                   if        %open(FIAR501L)
004700140217     C                   close     FIAR501L
004800140217     C                   endif
004900140217     C*
005000921023     C                   SETON                                        LR
005100130516
005200130516
005300030908
005400030908     C     chkRECFILMBR  BEGSR
005500030908     C*
005600090708     C                   READ      TIVAWWWT                               55
005700030908     C*
005800030908     C                   ENDSR
005900130516
006000130516
006100991027
006200991027     C     traduci       BEGSR
006300030325     C*
006400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
006600130516     C                   EVAL      VAWDTA =
006700140217     C                                       '"DATA SPED."'+';'+
006800140217     C                                       '"RIF.MITT. SPED 1"'+';'+
006900140217     C                                       '"RIF.MITT. SPED 2"'+';'+
007000140217     C                                       '"ID SPED BRT"'+';'+
007100140217     C                                       '"RAG.SOC. DEST"'+';'+
007200140217     C                                       '"INDIRIZZO DEST"'+';'+
007300140217     C                                       '"CAP DEST"'+';'+
007400140217     C                                       '"LOCALITA DEST"'+';'+
007500140217     C                                       '"PROVINCIA DEST"'+';'+
007600140217     C                                       '"CELL. DEST"'+';'
007700030325     C*
007800090708     C                   WRITE     TIVAW000
007900030325     C                   ENDIF
008000991027     C*
008100090320     C                   READ      tivgdtmp
008200090320     C                   DOW       not %eof(tivgdtmp)
008300090320     c                   movel     vgddta        fnvabds
008400080613     C*
008500080613     C                   setoff                                       31
008600080613     C*
008700080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
008800080613     C                   if        %subst(prmppt:6:1) = 'O'
008900080613     C*
009000080613     C* Chiamata metodo GetLblTyp
009100080613     C                   if        UBLBLSPE_GetLblTyp(VABAAS
009200080613     C                                               :VABLNP
009300080613     C                                               :VABNRS
009400080613     C                                               :VABNSP
009500130516     C                                               :pOutLblTyp
009600130516     C                                               :pOutAnnoBO
009700130516     C                                               :pOutLineaParBO
009800130516     C                                               :pOutSerieBO
009900130516     C                                               :pOutNumSpedBO
010000130516     C                                               :pOutDcmBO
010100130516     C                                               :pOutCcaBO
010200130516     C                                               :pOutRblBO) = 0
010300130516     C                   if        pOutLblTyp <> 'O'
010400080613     C                   seton                                        31
010500080613     C                   endif
010600080613     C                   endif
010700080613     C                   endif
010800080618     C*
010900080618     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
011000080618     C                   if        %subst(prmppt:7:1) = 'S'
011100080618     C                   if        VABNRS = *zeros
011200080618     C                   seton                                        31
011300080618     C                   endif
011400080618     C                   endif
011500080613     C*
011600080613     C* Se tutto ok => procedo
011700080613     C                   if        *in31 = *off
011800140217     C*
011900140217     C* Reperisco iinfo su SMS
012000140217     C                   exsr      repSMS
012100030325     C*
012200130516     C                   EVAL      VAWDTA =
012300140217     C                               %trim(%editc(VABAAS*10000+VABMGS:'X'))+';'+
012400140217     C                                       %trim(%editc(VABRMN:'X'))+';'+
012500130516     C                                          '"'+%trim(VABRMA)+'"'+';'+
012600130516     C                                       %trim(%editc(VABLNP:'X'))+
012700130516     C                                       %trim(%editc(VABNRS:'X'))+
012800130516     C                                       %trim(%editc(VABNSP:'X'))+';'+
012900090709     C                                          '"'+%trim(VABRSD)+'"'+';'+
013000090709     C                                          '"'+%trim(VABIND)+'"'+';'+
013100130516     C                                          '"'+%trim(VABCAD)+'"'+';'+
013200090709     C                                          '"'+%trim(VABLOD)+'"'+';'+
013300140217     C                                          '"'+%trim(VABPRD)+'"'+';'+
013400140218     C                                          '"'''+%trim(wSMS)+'"'+';'
013500930409     C*
013600090708     C                   WRITE     TIVAW000
013700080613     C                   endif
013800080613     C*
013900090320     C                   delete    tivgd000
014000991027     C*
014100090320     C                   READ      tivgdtmp
014200030325     C                   ENDDO
014300991027     C*
014400030325     C                   EVAL      wrkesito = '0'
014500991027     C*
014600910830     C                   ENDSR
014700140217
014800140217
014900140217
015000140217     C     repSMS        BEGSR
015100140217     C*
015200140217     C                   movel     *blanks       wSMS             25
015300140217     C*
015400140217     C                   eval      ar5trd = 'EMD'
015500140217     C     KeyAR501_P    chain     fiar501l
015600140217     C                   if        %found(fiar501l)
015700140217     C                   eval      dar5emd = AR5UNI
015800140217     C                   eval      wSMS = §AR5TEL
015900140217     C                   endif
016000140217     C*
016100140217     C                   ENDSR
016200140217
016300080527
016400080527
016500991027
016600991027      /TITLE Operazioni iniziali.
016700991027     C     *inzsr        BEGSR
016800991027     C*
016900991027     C     *ENTRY        PLIST
017000991027     C                   parm                    prmppt
017100991027     C     wrkesito      parm      wrkesito      prmesito
017200140217     C*
017300140217     C* Chiave su FIAR501L - Parziale
017400140217     C     KeyAR501_P    klist
017500140217     C                   kfld                    vabaas
017600140217     C                   kfld                    vablnp
017700140217     C                   kfld                    vabnrs
017800140217     C                   kfld                    vabnsp
017900140217     C                   kfld                    ar5trd
018000080527     C*
018100991027     C                   ENDSR
