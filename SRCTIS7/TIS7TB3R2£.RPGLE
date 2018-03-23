000100161213     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200030325     H DECEDIT('0.') DATEDIT(*DMY.)
000300991027
000400161213     Ftivgdtmp  if   E             DISK
000500161216     FTIVAWWWT  uf A E             DISK    commit
000600161216     F                                     prefix(O_)
000700161213     Ftntbe01l  if   E           k DISK
000800080527     Ftitas30c  if   E           k DISK
000900100212     FtitaA30c  if   E           k DISK
001000161213     Ffirab01l  if   E           K DISK
001100150317     Ftncsb03l  if   E           k DISK
001200100216     Fcnaco00f  if   E           k DISK
001300100216     Fcnind00f  if   E           k DISK
001400161213     Ffnblt01l  if   E           k DISK    usropn
001500120406     F                                     extfile(LibFileBLT01)
001600161213     Ffiars01l  if   E           k DISK    usropn
001700120406     F                                     extfile(LibFileARS01)
001800161213
001900991027
002000991027     D prmppt          s             50
002100991027     D prmesito        s              1
002200161213     D wrkesito        s                   like(prmesito)
002300161213     D DVPC          e ds                  inz
002400090320     D fnvabds       e ds                  extname(fnvab00t)
002500161213     D wIAS            s             14    inz
002600161213     D wPKB            s              8    inz
002700161213     D wPKB_RIL        s              8    inz
002800161213     D wVLB            s              6    inz
002900161213     D wVLB_RIL        s              9    inz
003000161213     D wQFT            s             14    inz
003100161213     D wCAS            s             14    inz
003200161213     D wCASOR          s             14    inz
003300161213     D wVMD            s             14    inz
003400161216     D VAWDTA          s                   like(O_VAWDTA)
003500161213     D wVAWDTA         s                   like(VAWDTA)
003600100212     D wTAARSC         s                   like(TAARSC)
003700100212     D wTAAIND         s                   like(TAAIND)
003800100212     D wTAACAP         s                   like(TAACAP)
003900100212     D wTAALOC         s                   like(TAALOC)
004000100212     D wTAAPRV         s                   like(TAAPRV)
004100120406     D wARSNOT_C       s                   like(ARSNOT)
004200150317     D wDCM            s                   like(pOutDcmFI)
004300150317     D wCCA            s                   like(pOutCcaFI)
004400150317     D cCAS            s                   like(csbCAS)
004500150317     D cVCA            s                   like(csbVCA)
004600161213     D wVABRMNR        s                   like(vabRMN) inz
004700161213     D wVABRMAR        s                   like(vabRMA) inz
004800170117     D pCSV            s              1    inz
004900170117     D pDLM            s              1    inz varying
005000161214     D iINTI           s               N   inz
005100161214     D iINTS           s               N   inz
005200161213     D iRAB            s               N   inz
005300161213     D iDCR            s               N   inz
005400161213     D iORIG           s               N   inz
005500161213     D iNRS            s               N   inz
005600161213     D iMITT           s               N   inz
005700161213     D iSEGN           s               N   inz
005800161213     D iRIL            s               N   inz
005900161213     D iCONS           s               N   inz
006000161213     D iCAS            s               N   inz
006100161213
006200100212
006300120406     D LibFileBLT01    s             21A   inz
006400120406     D LibFileARS01    s             21A   inz
006500120406     D currSysNeta     s              8A   inz('*NULL')
006600161213
006700161213
006800161213     D*-----------
006900161213     D* SCHIERE
007000161213     D*-----------
007100161213     D skVPC           S              7  0 DIM(500)
007200161213     D jVPC            S              3  0 INZ(*zeros)
007300080613
007400120406
007500080613     D*------------------
007600080613     D* LINKING A DEFINIZIONI ESTERNE
007700080613     D*------------------
007800080613     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007900100212     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008000120406     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
008100120406     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
008200120406
008300991027
008400120406     C*
008500120406     C* Reperisco il sistema AS/400 corrente
008600120406     C                   callp     UBRTVNETA_Rtv(currSysNeta)
008700120406     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
008800120406     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
008900120406     C                   eval      LibFileARS01 = 'FILTRA201/FIARS01L'
009000120406     C                   else
009100120406     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
009200120406     C                   eval      LibFileARS01 = 'FILTRAPRD/FIARS01L'
009300120406     C                   endif
009400120406     C*
009500120406     C                   if        not %open(FNBLT01L)
009600120406     C                   open      FNBLT01L
009700120406     C                   endif
009800120406     C*
009900120406     C                   if        not %open(FIARS01L)
010000120406     C                   open      FIARS01L
010100120406     C                   endif
010200170118     C*
010300170118     C* Avvio il monitoring del intero flusso
010400170118     C                   monitor
010500170118     C*
010600170118     C* Effettuo traduzione
010700161213     C                   EXSR      cartab
010800030908     C                   EXSR      chkRECFILMBR
010900991027     C                   EXSR      traduci
011000170118     C*
011100170118     C* Gestisco eventuale errore
011200170118     C                   on-error
011300170118     C*
011400170118     C* Imposto esito transazione a errore
011500170118     C                   EVAL      wrkesito = '2'
011600170118     C*
011700170118     C* Se errore => emetto operazione d ROLLBACK
011800170118     C                   ROLBK
011900170118     C*
012000170118     C* Arresto il monitoring
012100170118     C                   endmon
012200120406     C*
012300120406     C                   if        %open(FNBLT01L)
012400120406     C                   close     FNBLT01L
012500120406     C                   endif
012600120406     C*
012700120406     C                   if        %open(FIARS01L)
012800120406     C                   close     FIARS01L
012900120406     C                   endif
013000120406     C*
013100921023     C                   SETON                                        LR
013200120406
013300120406
013400030908
013500030908     C     chkRECFILMBR  BEGSR
013600030908     C*
013700100212     C                   READ      TIVAWWWT                               55
013800030908     C*
013900030908     C                   ENDSR
014000120406
014100120406
014200991027
014300991027     C     traduci       BEGSR
014400161213     C*
014500161213     C* Inizializzazione indicatori di procedure per parametri input
014600161216     C                   setoff                                       20
014700161214     C                   eval      iINTI = *off
014800161214     C                   eval      iINTS = *off
014900161213     C                   eval      iRAB  = *off
015000161213     C                   eval      iDCR  = *off
015100161213     C                   eval      iORIG = *off
015200161213     C                   eval      iNRS  = *off
015300161213     C                   eval      iMITT = *off
015400161213     C                   eval      iSEGN = *off
015500161213     C                   eval      iRIL  = *off
015600161213     C                   eval      iCONS = *off
015700161213     C                   eval      iCAS  = *off
015800161214     C*
015900161214     C* Se richiesta intestazione
016000161214     C                   select
016100161214     C                   when      %subst(prmppt:1:1) = 'I'
016200161214     C                   eval      iINTI = *on
016300161214     C                   when      %subst(prmppt:1:1) = 'S'
016400161214     C                   eval      iINTS = *on
016500161214     C                   endsl
016600161214     C*
016700161214     C* Se richiesto un particolare carattere d separatore campi
016800161214     C                   if        %subst(prmppt:2:1) <> *blanks
016900170117     C                   eval      pCSV = %subst(prmppt:2:1)
017000161214     C                   else
017100170117     C                   movel     ','           pCSV
017200161214     C                   endif
017300161214     C*
017400161214     C* Se richiesto un particolare carattere d separatore decimale
017500161214     C                   if        %subst(prmppt:3:1) = ','
017600161214     C                   setoff                                       60
017700161214     C                   else
017800161214     C                   seton                                        60
017900161214     C                   endif
018000170117     C*
018100170117     C* Verifico il carattere delimitatore test indicato nei parametri del cliente/tipo file
018200170117     C                   select
018300170117     C                   when      %subst(prmppt:4:1) = *blanks
018400170117     C                   eval      pDLM = '"'
018500170117     C                   when      %subst(prmppt:4:1) = 'N'
018600170117     C                   eval      pDLM = ''
018700170117     C                   other
018800170117     C                   eval      pDLM = %subst(prmppt:4:1)
018900170117     C                   endsl
019000170111     C*
019100170111     C* Se richiesto sviluppo per "raccordo accorpamento" (FIRAB)
019200170117     C                   if        %subst(prmppt:5:1) = 'S'
019300170111     C                   eval      iRAB = *on
019400170111     C                   endif
019500161214     C*
019600161214     C* Se richiesto valorizzo da data consegna richiesta con il proprio valore
019700170117     C                   if        %subst(prmppt:6:1) = 'R'
019800161214     C                   eval      iDCR = *on
019900161214     C                   endif
020000161214     C*
020100161214     C* Se richiesto ritorno solamente le bolle originali
020200170117     C                   if        %subst(prmppt:7:1) = 'O'
020300161214     C                   eval      iORIG = *on
020400161214     C                   endif
020500161214     C*
020600161214     C* Se richiesto ritorno solamente le bolle con serie
020700170117     C                   if        %subst(prmppt:8:1) = 'S'
020800161214     C                   eval      iNRS = *on
020900161214     C                   endif
021000170111     C*
021100170111     C* Se richiesto ritorno anche i dati relativi al mittente
021200170117     C                   if        %subst(prmppt:9:1) = 'S'
021300170111     C                   eval      iMITT = *on
021400170111     C                   endif
021500170111     C*
021600170111     C* Se richiesto ritorno anche i dati del segnacollo cliente
021700170117     C                   if        %subst(prmppt:10:1) = 'S'
021800170111     C                   eval      iSEGN = *on
021900170111     C                   endif
022000170111     C*
022100170111     C* Se richiesto ritorno anche i dati relativi a peso e volume rilevati
022200170117     C                   if        %subst(prmppt:11:1) = 'S'
022300170111     C                   eval      iRIL = *on
022400170111     C                   endif
022500170111     C*
022600170111     C* Se richiesto ritorno anche i dati relativi a data consegna e codice consegna anomala
022700170117     C                   if        %subst(prmppt:12:1) = 'S'
022800170111     C                   eval      iCONS = *on
022900170111     C                   endif
023000170111     C*
023100170117     C                   if        %subst(prmppt:13:1) = 'S'
023200170111     C                   eval      iCAS = *on
023300170111     C                   endif
023400991027     C*
023500161213     C* Scorro tutto il file
023600161213     C                   read      tivgdtmp
023700161213     C                   dow       not %eof(tivgdtmp)
023800170119     C*
023900170119     C* Inizializzo buffer output
024000170119     C                   clear                   TIVAW000
024100161213     C*
024200161213     C* Valorizzo dati "corrrenti"
024300161213     C                   movel     vgddta        fnvabds
024400080613     C*
024500161213     C* Inizializzo indicatore di record corrente OK
024600080613     C                   setoff                                       31
024700080613     C*
024800080613     C* Chiamata metodo GetLblTyp
024900161213     C                   if        iORIG
025000161213     C                   if        UBLBLSPE_GetLblTyp(VABAAS
025100080613     C                                               :VABLNP
025200080613     C                                               :VABNRS
025300080613     C                                               :VABNSP
025400100212     C                                               :pOutLblTyp
025500100212     C                                               :pOutAnnoBO
025600100212     C                                               :pOutLineaParBO
025700100212     C                                               :pOutSerieBO
025800100212     C                                               :pOutNumSpedBO
025900100212     C                                               :pOutDcmBO
026000100212     C                                               :pOutCcaBO
026100100212     C                                               :pOutRblBO) = 0
026200100212     C                   if        pOutLblTyp <> 'O'
026300080613     C                   seton                                        31
026400080613     C                   endif
026500080613     C                   endif
026600161213     C                   endif
026700080618     C*
026800080618     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
026900161213     C                   if        iNRS
027000161213     C                   if        VABNRS = *zeros
027100080618     C                   seton                                        31
027200080618     C                   endif
027300161213     C                   endif
027400080613     C*
027500161213     C* Se record ok => procedo
027600080613     C                   if        *in31 = *off
027700161213     C*
027800161213     C* Se richiesto reperimento dati bolla da Sede
027900161213     C                   z-add     VABDCR        savVABDCR         8 0
028000161213     C                   if        iDCR
028100161213     C                   exsr      reptas
028200161213     C                   endif
028300161213     C*
028400161213     C* Se richiesto nei parametri d ingresso ritorno anche i dati del mittente originale
028500161213     C                   if        iMITT
028600161213     C                   exsr      reptaA
028700161213     C                   endif
028800161213     C*
028900161213     C* Se richiesto nei parametri d ingresso ritorno anche dati esito consegna
029000161213     C                   clear                   wDCM
029100161213     C                   clear                   wCCA
029200161213     C                   if        iCONS
029300161213     C* Chiamata metodo GetLastChild
029400161213     C                   movel     *blanks       wEsito2           1
029500161213     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
029600161213     C                                                VABAAS
029700161213     C                                               :VABLNP
029800161213     C                                               :VABNRS
029900161213     C                                               :VABNSP
030000161213     C                                               :pOutAnnoFI
030100161213     C                                               :pOutLineaParFI
030200161213     C                                               :pOutSerieFI
030300161213     C                                               :pOutNumSpedFI
030400161213     C                                               :pOutDcmFI
030500161213     C                                               :pOutCcaFI))
030600161213     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
030700161213     C                   if        pOutDcmFI > *zeros
030800161213     C                   move      pOutCcaFI     wCCA
030900161213     C                   move      pOutDcmFI     wDCM
031000161213     C                   endif
031100161213     C                   endif
031200161213     C*
031300161213     C* Se richiesto nei parametri d ingresso ritorno anche dati contrassegno originale
031400161213     C                   clear                   wCASOR
031500161213     C                   clear                   cCAS
031600161213     C                   clear                   cVCA
031700161213     C                   if        iCAS
031800161213     C                   exsr      repCSB
031900161213     C                   endif
032000161213     C*
032100161213     C* Se richiesta sviluppo riferimenti "accorpati" => verifico file raccordo FIRAB
032200161213     C                   clear                   wVABRMNR
032300161213     C                   clear                   wVABRMAR
032400161213     C                   if        iRAB
032500161213     C     KEYrab01_C    setll     firab01l
032600161213     C                   if        %equal(firab01l)
032700161213     C     KEYrab01_C    reade     firab01l
032800161213     C                   dow       not %eof(firab01l)
032900161213     C                   eval      wVABRMNR = rabRMNR
033000161213     C                   eval      wVABRMAR = rabRMAR
033100161216     C                   exsr      valOUT
033200161213     C     KEYrab01_C    reade     firab01l
033300161213     C                   enddo
033400161213     C*
033500161213     C                   else
033600161216     C                   exsr      valOUT
033700161213     C                   endif
033800161213     C*
033900161213     C                   else
034000161216     C                   exsr      valOUT
034100161213     C                   endif
034200161213     C*
034300161213     C* Se partcolarità abilitata in tabella 'VPC' => eseguo
034400161213     C     vabCCM        lookup    skVPC                                  05
034500161213     C                   if        %found
034600161213     C                   call      'TIS7T99R'
034700161213     C                   parm      vabCCM        I99CCM            7 0
034800161213     C                   parm      vabAAS        I99AAS            4 0
034900161213     C                   parm      vabLNP        I99LNP            3 0
035000161213     C                   parm      vabNRS        I99NRS            2 0
035100161213     C                   parm      vabNSP        I99NSP            7 0
035200161213     C                   endif
035300120406     C*
035400080613     C                   endif
035500991027     C*
035600161213     C                   read      tivgdtmp
035700161213     C                   enddo
035800991027     C*
035900161213     C                   eval      wrkesito = '0'
036000991027     C*
036100910830     C                   ENDSR
036200161216
036300161216
036400161216
036500161216     C*
036600161216     C     wriINT        BEGSR
036700161216     C*
036800161216     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
036900161216     C                   if        (iINTI OR iINTS) AND *in55 = *on
037000161216     C                   if        iINTI
037100170117     C                   eval      O_VAWDTA =
037200170117     C                             pDLM+'VABATB'                     +pDLM+pCSV+
037300170117     C                             pDLM+'VABCCM'                     +pDLM+pCSV+
037400170117     C                             pDLM+'VABLNP'                     +pDLM+pCSV+
037500170117     C                             pDLM+'VABAAS'                     +pDLM+pCSV+
037600170117     C                             pDLM+'VABMGS'                     +pDLM+pCSV+
037700170117     C                             pDLM+'VABNRS'                     +pDLM+pCSV+
037800170117     C                             pDLM+'VABNSP'                     +pDLM+pCSV+
037900170117     C                             pDLM+'VABCBO'                     +pDLM+pCSV+
038000170117     C                             pDLM+'VABLNA'                     +pDLM+pCSV+
038100170117     C                             pDLM+'VABRSD'                     +pDLM+pCSV+
038200170117     C                             pDLM+'VABRD2'                     +pDLM+pCSV+
038300170117     C                             pDLM+'VABIND'                     +pDLM+pCSV+
038400170117     C                             pDLM+'VABCAD'                     +pDLM+pCSV+
038500170117     C                             pDLM+'VABLOD'                     +pDLM+pCSV+
038600170117     C                             pDLM+'VABPRD'                     +pDLM+pCSV+
038700170117     C                             pDLM+'VABNZD'                     +pDLM+pCSV+
038800170117     C                             pDLM+'VABGC1'                     +pDLM+pCSV+
038900170117     C                             pDLM+'VABGC2'                     +pDLM+pCSV+
039000170117     C                             pDLM+'VABCTR'                     +pDLM+pCSV+
039100170117     C                             pDLM+'VABTSP'                     +pDLM+pCSV+
039200170117     C                             pDLM+'VABIAS'                     +pDLM+pCSV+
039300170117     C                             pDLM+'VABVAS'                     +pDLM+pCSV+
039400170117     C                             pDLM+'VABNAS'                     +pDLM+pCSV+
039500170117     C                             pDLM+'VABNCL'                     +pDLM+pCSV+
039600170117     C                             pDLM+'VABPKB'                     +pDLM+pCSV+
039700170117     C                             pDLM+'VABVLB'                     +pDLM+pCSV+
039800170117     C                             pDLM+'VABQFT'                     +pDLM+pCSV+
039900170117     C                             pDLM+'VABCAS'                     +pDLM+pCSV+
040000170117     C                             pDLM+'VABTIC'                     +pDLM+pCSV+
040100170117     C                             pDLM+'VABVCA'                     +pDLM+pCSV+
040200170117     C                             pDLM+'VABGCA'                     +pDLM+pCSV+
040300170117     C                             pDLM+'VABRMN'                     +pDLM+pCSV+
040400170117     C                             pDLM+'VABRMA'                     +pDLM+pCSV+
040500170117     C                             pDLM+'VABNCD'                     +pDLM+pCSV+
040600170117     C                             pDLM+'VABNCA'                     +pDLM+pCSV+
040700170117     C                             pDLM+'VABXCO'                     +pDLM+pCSV+
040800170117     C                             pDLM+'VABNOT'                     +pDLM+pCSV+
040900170117     C                             pDLM+'VABNT2'                     +pDLM+pCSV+
041000170117     C                             pDLM+'VABZNC'                     +pDLM+pCSV+
041100170117     C                             pDLM+'VABCTM'                     +pDLM+pCSV+
041200170117     C                             pDLM+'VABFFD'                     +pDLM+pCSV+
041300170117     C                             pDLM+'VABDCR'                     +pDLM+pCSV+
041400170117     C                             pDLM+'VABTCR'                     +pDLM+pCSV+
041500170117     C                             pDLM+'VABHCR'                     +pDLM+pCSV+
041600170117     C                             pDLM+'VABCTS'                     +pDLM+pCSV+
041700170117     C                             pDLM+'VABFTM'                     +pDLM+pCSV+
041800170117     C                             pDLM+'VABVMD'                     +pDLM+pCSV+
041900170117     C                             pDLM+'VABVAD'                     +pDLM+pCSV+
042000170117     C                             pDLM+'VABGMA'                     +pDLM+pCSV+
042100170117     C                             pDLM+'VABGGA'                     +pDLM+pCSV+
042200170117     C                             pDLM+'VABGVA'                     +pDLM+pCSV+
042300170117     C                             pDLM+'VABTC1'                     +pDLM+pCSV+
042400170117     C                             pDLM+'VABTC2'                     +pDLM+pCSV+
042500170117     C                             pDLM+'VABSCL'                     +pDLM+pCSV+
042600170117     C                             pDLM+'VABANT'                     +pDLM+pCSV+
042700170117     C                             pDLM+'VABRMO'                     +pDLM+pCSV+
042800170117     C                             pDLM+'VABCMO'                     +pDLM+pCSV+
042900170117     C                             pDLM+'VABNMO'                     +pDLM
043000161216     C                   endif
043100161216     C*
043200161216     C                   if        iINTS
043300170117     C                   eval      O_VAWDTA =
043400170117     C                             pDLM+'VABCCM'                     +pDLM+pCSV+
043500170117     C                             pDLM+'VABAAS'                     +pDLM+pCSV+
043600170117     C                             pDLM+'VABMGS'                     +pDLM+pCSV+
043700170117     C                             pDLM+'VABRMA'                     +pDLM+pCSV+
043800170117     C                             pDLM+'VABRMN'                     +pDLM+pCSV+
043900170117     C                             pDLM+'VABSPED'                    +pDLM+pCSV+
044000170117     C                             pDLM+'VABRSD'                     +pDLM+pCSV+
044100170117     C                             pDLM+'VABIND'                     +pDLM+pCSV+
044200170117     C                             pDLM+'VABCAD'                     +pDLM+pCSV+
044300170117     C                             pDLM+'VABLOD'                     +pDLM+pCSV+
044400170117     C                             pDLM+'VABPRD'                     +pDLM
044500161216     C                   endif
044600161216     C*
044700161216     C* Verifico se richiesto lancio per "raccordo accorpamento" (FIRAB)
044800170111     C                   if        iRAB
044900170117     C                   eval      O_VAWDTA =
045000170117     C                                 %trim(O_VAWDTA)                    +pCSV+
045100170117     C                             pDLM+'VABRMNR'                    +pDLM+pCSV+
045200170117     C                             pDLM+'VABRMAR'                    +pDLM
045300161216     C                   endif
045400161216     C*
045500170111     C                   if        iMITT
045600170117     C                   eval      O_VAWDTA =
045700170117     C                                 %trim(O_VAWDTA)                    +pCSV+
045800170117     C                             pDLM+'TAARSC'                     +pDLM+pCSV+
045900170117     C                             pDLM+'TAAIND'                     +pDLM+pCSV+
046000170117     C                             pDLM+'TAACAP'                     +pDLM+pCSV+
046100170117     C                             pDLM+'TAALOC'                     +pDLM+pCSV+
046200170117     C                             pDLM+'TAAPRV'                     +pDLM
046300161216     C                   endif
046400161216     C*
046500170111     C                   if        iSEGN
046600170117     C                   eval      O_VAWDTA =
046700170117     C                                       %trim(O_VAWDTA)              +pCSV+
046800170117     C                             pDLM+'SEGNACOLLO_CLI'             +pDLM
046900161216     C*
047000170111     C                   if        iRIL
047100170117     C                   eval      O_VAWDTA =
047200170117     C                                       %trim(O_VAWDTA)              +pCSV+
047300170117     C                             pDLM+'PESO_RIL'                   +pDLM+pCSV+
047400170117     C                             pDLM+'VOLUME_RIL'                 +pDLM
047500161216     C                   endif
047600161216     C                   endif
047700161216     C*
047800170111     C                   if        iCONS
047900170117     C                   eval      O_VAWDTA =
048000170117     C                                       %trim(O_VAWDTA)              +pCSV+
048100170117     C                             pDLM+'DATA_CONS'                  +pDLM+pCSV+
048200170117     C                             pDLM+'CCA'                        +pDLM
048300161216     C                   endif
048400161216     C*
048500170111     C                   if        iCAS
048600170117     C                   eval      O_VAWDTA =
048700170117     C                                       %trim(O_VAWDTA)              +pCSV+
048800170117     C                             pDLM+'CAS_ORIG'                   +pDLM+pCSV+
048900170117     C                             pDLM+'VCA_ORIG'                   +pDLM
049000161216     C                   endif
049100161216     C*
049200161216     C                   write     TIVAW000
049300161216     C                   endif
049400161216     C*
049500161216     C                   ENDSR
049600161213
049700161213
049800161213
049900161213     C*
050000161216     C     valOUT        BEGSR
050100161213     C*
050200161213     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
050300161213     C  N60              EVAL      wIAS = %trim(%editw(VABIAS:'        0 ,   '))
050400161213     C   60              EVAL      wIAS = %trim(%editw(VABIAS:'        0 .   '))
050500161213     C  N60              EVAL      wPKB = %trim(%editw(VABPKB:'    0 , '))
050600161213     C   60              EVAL      wPKB = %trim(%editw(VABPKB:'    0 . '))
050700161213     C  N60              EVAL      wVLB = %trim(%editw(VABVLB:'0 ,   '))
050800161213     C   60              EVAL      wVLB = %trim(%editw(VABVLB:'0 .   '))
050900161213     C  N60              EVAL      wQFT = %trim(%editw(VABQFT:'        0 ,   '))
051000161213     C   60              EVAL      wQFT = %trim(%editw(VABQFT:'        0 .   '))
051100161213     C  N60              EVAL      wCAS = %trim(%editw(VABCAS:'        0 ,   '))
051200161213     C   60              EVAL      wCAS = %trim(%editw(VABCAS:'        0 .   '))
051300161213     C  N60              EVAL      wCASOR = %trim(%editw(cCAS:'        0 ,   '))
051400161213     C   60              EVAL      wCASOR = %trim(%editw(cCAS:'        0 .   '))
051500161213     C  N60              EVAL      wVMD = %trim(%editw(VABVMD:'        0 ,   '))
051600161213     C   60              EVAL      wVMD = %trim(%editw(VABVMD:'        0 .   '))
051700161213     C*
051800161214     C                   if        iINTI OR (not iINTI AND not iINTS)
051900170117     C                   eval      VAWDTA  =
052000170117     C                             pDLM+           %trim(VABATB)     +pDLM+pCSV+
052100170117     C                                      %trim(%editc(VABCCM:'Q'))     +pCSV+
052200170117     C                                      %trim(%editc(VABLNP:'Q'))     +pCSV+
052300170117     C                                      %trim(%editc(VABAAS:'Q'))     +pCSV+
052400170117     C                                      %trim(%editc(VABMGS:'Q'))     +pCSV+
052500170117     C                                      %trim(%editc(VABNRS:'Q'))     +pCSV+
052600170117     C                                      %trim(%editc(VABNSP:'Q'))     +pCSV+
052700170117     C                             pDLM+           %trim(VABCBO)     +pDLM+pCSV+
052800170117     C                                      %trim(%editc(VABLNA:'Q'))     +pCSV+
052900170117     C                             pDLM+           %trim(VABRSD)     +pDLM+pCSV+
053000170117     C                             pDLM+           %trim(VABRD2)     +pDLM+pCSV+
053100170117     C                             pDLM+           %trim(VABIND)     +pDLM+pCSV+
053200170117     C                             pDLM+           %trim(VABCAD)     +pDLM+pCSV+
053300170117     C                             pDLM+           %trim(VABLOD)     +pDLM+pCSV+
053400170117     C                             pDLM+           %trim(VABPRD)     +pDLM+pCSV+
053500170117     C                             pDLM+           %trim(VABNZD)     +pDLM+pCSV+
053600170117     C                             pDLM+           %trim(VABGC1)     +pDLM+pCSV+
053700170117     C                             pDLM+           %trim(VABGC2)     +pDLM+pCSV+
053800170117     C                                      %trim(%editc(VABCTR:'Q'))     +pCSV+
053900170117     C                             pDLM+           %trim(VABTSP)     +pDLM+pCSV+
054000170117     C                                                    %trim(wIAS)     +pCSV+
054100170117     C                             pDLM+           %trim(VABVAS)     +pDLM+pCSV+
054200170117     C                             pDLM+           %trim(VABNAS)     +pDLM+pCSV+
054300170117     C                                      %trim(%editc(VABNCL:'Q'))     +pCSV+
054400170117     C                                                    %trim(wPKB)     +pCSV+
054500170117     C                                                    %trim(wVLB)     +pCSV+
054600170117     C                                                    %trim(wQFT)     +pCSV+
054700170117     C                                                    %trim(wCAS)     +pCSV+
054800170117     C                             pDLM+           %trim(VABTIC)     +pDLM+pCSV+
054900170117     C                             pDLM+           %trim(VABVCA)     +pDLM+pCSV+
055000170117     C                             pDLM+           %trim(VABGCA)     +pDLM+pCSV+
055100170117     C                                      %trim(%editc(VABRMN:'Q'))     +pCSV+
055200170117     C                             pDLM+           %trim(VABRMA)     +pDLM+pCSV+
055300170117     C                                      %trim(%editc(VABNCD:'Q'))     +pCSV+
055400170117     C                                      %trim(%editc(VABNCA:'Q'))     +pCSV+
055500170117     C                             pDLM+           %trim(VABXCO)     +pDLM+pCSV+
055600170117     C                             pDLM+           %trim(VABNOT)     +pDLM+pCSV+
055700170117     C                             pDLM+           %trim(VABNT2)     +pDLM+pCSV+
055800170117     C                                      %trim(%editc(VABZNC:'Q'))     +pCSV+
055900170117     C                             pDLM+           %trim(VABCTM)     +pDLM+pCSV+
056000170117     C                             pDLM+           %trim(VABFFD)     +pDLM+pCSV+
056100170117     C                                      %trim(%editc(VABDCR:'Q'))     +pCSV+
056200170117     C                             pDLM+           %trim(VABTCR)     +pDLM+pCSV+
056300170117     C                                      %trim(%editc(VABHCR:'Q'))     +pCSV+
056400170117     C                             pDLM+           %trim(VABCTS)     +pDLM+pCSV+
056500170117     C                             pDLM+           %trim(VABFTM)     +pDLM+pCSV+
056600170117     C                                                    %trim(wVMD)     +pCSV+
056700170117     C                             pDLM+           %trim(VABVAD)     +pDLM+pCSV+
056800170117     C                             pDLM+           %trim(VABGMA)     +pDLM+pCSV+
056900170117     C                             pDLM+           %trim(VABGGA)     +pDLM+pCSV+
057000170117     C                             pDLM+           %trim(VABGVA)     +pDLM+pCSV+
057100170117     C                             pDLM+           %trim(VABTC1)     +pDLM+pCSV+
057200170117     C                             pDLM+           %trim(VABTC2)     +pDLM+pCSV+
057300170117     C                             pDLM+           %trim(VABSCL)     +pDLM+pCSV+
057400170117     C                                      %trim(%editc(VABANT:'Q'))     +pCSV+
057500170117     C                             pDLM+           %trim(VABRMO)     +pDLM+pCSV+
057600170117     C                             pDLM+           %trim(VABCMO)     +pDLM+pCSV+
057700170117     C                             pDLM+           %trim(VABNMO)     +pDLM
057800161214     C                   endif
057900161214     C*
058000161214     C                   if        iINTS
058100170117     C                   eval      VAWDTA = %trim(%editc(VABCCM:'X'))     +pCSV+
058200170117     C                                      %trim(%editc(VABAAS:'X'))     +pCSV+
058300170117     C                                      %trim(%editc(VABMGS:'X'))     +pCSV+
058400170117     C                             pDLM+           %trim(VABRMA)     +pDLM+pCSV+
058500170117     C                                      %trim(%editc(VABRMN:'Q'))     +pCSV+
058600161214     C                                      %trim(%editc(VABLNP:'X'))+
058700161214     C                                      %trim(%editc(VABNRS:'X'))+
058800170117     C                                      %trim(%editc(VABNSP:'X'))     +pCSV+
058900170117     C                             pDLM+           %trim(VABRSD)     +pDLM+pCSV+
059000170117     C                             pDLM+           %trim(VABIND)     +pDLM+pCSV+
059100170117     C                             pDLM+           %trim(VABCAD)     +pDLM+pCSV+
059200170117     C                             pDLM+           %trim(VABLOD)     +pDLM+pCSV+
059300170117     C                             pDLM+           %trim(VABPRD)     +pDLM
059400161214     C                   endif
059500161213     C*
059600161213     C                   if        iRAB
059700170117     C                   eval      VAWDTA  =
059800170117     C                                       %trim(VAWDTA)                +pCSV+
059900170124     C                                    %trim(%editc(wVABRMNR:'Q'))     +pCSV+
060000170117     C                             pDLM+     %trim(wVABRMAR)         +pDLM
060100161213     C                   endif
060200161213     C*
060300161213     C                   if        iMITT
060400170117     C                   eval      VAWDTA  =
060500170117     C                                       %trim(VAWDTA)                +pCSV+
060600170117     C                             pDLM+     %trim(wTAARSC)          +pDLM+pCSV+
060700170117     C                             pDLM+     %trim(wTAAIND)          +pDLM+pCSV+
060800170117     C                             pDLM+     %trim(wTAACAP)          +pDLM+pCSV+
060900170117     C                             pDLM+     %trim(wTAALOC)          +pDLM+pCSV+
061000170117     C                             pDLM+     %trim(wTAAPRV)          +pDLM
061100161213     C                   endif
061200161213     C*
061300161213     C                   if        iCONS
061400170117     C                   eval      VAWDTA  =
061500170117     C                                       %trim(VAWDTA)                +pCSV+
061600170124     C                                       %trim(%editc(wDCM:'Q'))      +pCSV+
061700170117     C                             pDLM+     %trim(wCCA)             +pDLM
061800161213     C                   endif
061900161213     C*
062000161213     C                   if        iCAS
062100170117     C                   eval      VAWDTA  =
062200170117     C                                       %trim(VAWDTA)                +pCSV+
062300170124     C                                       %trim(wCASOR)                +pCSV+
062400170117     C                             pDLM+     %trim(cVCA)             +pDLM
062500161213     C                   endif
062600161213     C*
062700161213     C* Se richiesto nei parametri d ingresso ritorno anche i dati del segnacollo cliente
062800161213     C                   if        iSEGN
062900161213     C                   eval      wVAWDTA = VAWDTA
063000161213     C*
063100161213     C* Aggancio i dettagli colli e ciclo x ciascuno
063200161213     C     KEYblt01_P    setll     fnblt01l
063300161213     C                   if        %found(fnblt01l)
063400161213     C     KEYblt01_P    reade     fnblt01l
063500161213     C                   dow       not %eof(fnblt01l)
063600161213     C*
063700161213     C                   clear                   wARSNOT_C
063800161213     C*
063900161213     C* Reperisco il segnacollo cliente ("chi sono")
064000161213     C                   eval      arsTRC = 'C'
064100161213     C     KEYars01_C    chain     fiars01l
064200161213     C                   if        %found(fiars01l)
064300161213     C                   eval      wARSNOT_C = arsNOT
064400161213     C                   endif
064500161213     C*
064600170117     C                   eval      VAWDTA  = %trim(wVAWDTA)+pCSV+
064700170117     C                                       pDLM+%trim(wARSNOT_C)+pDLM
064800161213     C                   if        iRIL
064900161213     C                   if        bltFVC <> 'P'
065000161213     C                   eval      bltVUC = *zeros
065100161213     C                   endif
065200161213     C                   if        bltFPC <> 'P'
065300161213     C                   eval      bltPUC = *zeros
065400161213     C                   endif
065500161213     C  N60              EVAL      wPKB_RIL = %trim(%editw(bltPUC:'  0 ,   '))
065600161213     C   60              EVAL      wPKB_RIL = %trim(%editw(bltPUC:'  0 .   '))
065700161213     C  N60              EVAL      wVLB_RIL = %trim(%editw(bltVUC:'0 ,      '))
065800161213     C   60              EVAL      wVLB_RIL = %trim(%editw(bltVUC:'0 .      '))
065900170117     C                   EVAL      VAWDTA  = %trim(VAWDTA)  +pCSV+
066000170117     C                                       %trim(wPKB_RIL)+pCSV+
066100161213     C                                       %trim(wVLB_RIL)
066200161213     C                   endif
066300161213     C*
066400161216     C                   exsr      wriOUT
066500161213     C*
066600161213     C* Scorro il segnacollo successivo
066700161213     C     KEYtas30_P    reade     fnblt01l
066800161213     C                   enddo
066900161213     C*
067000161213     C* Se nn trovo il dettaglio segnacolli BRT in filiale scrivo almeno un record
067100161213     C                   else
067200170117     C                   eval      VAWDTA  = %trim(VAWDTA)+pCSV+pDLM+pDLM
067300161216     C                   exsr      wriOUT
067400161213     C                   endif
067500161213     C*
067600161213     C                   else
067700161216     C                   exsr      wriOUT
067800161213     C                   endif
067900161213     C*
068000161213     C                   ENDSR
068100161216
068200161216
068300161216
068400161216     C*
068500161216     C     wriOUT        BEGSR
068600161216     C*
068700161216     C  N20              exsr      wriINT
068800161216     C                   eval      O_VAWDTA = VAWDTA
068900161216     C                   write     TIVAW000
069000161216     C                   seton                                        20
069100161216     C*
069200161216     C                   ENDSR
069300080527
069400080527
069500080527
069600080527     C*
069700080527     C     reptas        BEGSR
069800080527     C*
069900080527     C* Posizionamento diretto
070000080527     C     KEYtas30_P    chain     titas30c
070100080527     C                   if        %found(titas30c)
070200080527     C                   move      tasDCR        VABDCR
070300080527     C                   endif
070400080527     C*
070500080527     C                   ENDSR
070600100212
070700100212
070800100212
070900100212     C*
071000100212     C     reptaA        BEGSR
071100100212     C*
071200100212     C                   clear                   wTAARSC
071300100212     C                   clear                   wTAAIND
071400100212     C                   clear                   wTAACAP
071500100212     C                   clear                   wTAALOC
071600100212     C                   clear                   wTAAPRV
071700100216     C                   movel     *blanks       wTIPCCM           4
071800100216     C*
071900100225     C                   move(p)   tasCCM        wTIPCCM
072000100216     C                   if        wTIPCCM = '8888' or
072100100216     C                             wTIPCCM = '9999'
072200100212     C*
072300100212     C                   eval      taaTRC = 'M'
072400100212     C*
072500100212     C* Posizionamento diretto
072600100212     C     KEYtaA30_C    chain     titaA30c
072700100212     C                   if        %found(titaA30c)
072800100216     C                   movel     taaRSC        wTAARSC
072900100216     C                   movel     taaIND        wTAAIND
073000100216     C                   movel     taaCAP        wTAACAP
073100100216     C                   movel     taaLOC        wTAALOC
073200100216     C                   movel     taaPRV        wTAAPRV
073300100212     C                   endif
073400100216     C*
073500100216     C                   else
073600100216     C                   eval      acokut = 1
073700100216     C     KEYaco00_C    chain     cnaco00f
073800100216     C                   if        %found(cnaco00f)
073900100216     C                   movel     acoRAG        wTAARSC
074000100216     C     KEYind00_C    chain     cnind00f
074100100216     C                   if        %found(cnind00f)
074200100216     C                   movel     indVIA        wTAAIND
074300100216     C                   movel     indCAP        wTAACAP
074400100216     C                   movel     indCIT        wTAALOC
074500100216     C                   movel     indPRV        wTAAPRV
074600100216     C                   endif
074700100216     C                   endif
074800100216     C                   endif
074900100212     C*
075000100212     C                   ENDSR
075100150317
075200150317
075300150317
075400150317     C*
075500150317     C     repcsb        BEGSR
075600150317     C*
075700150317     C* Posizionamento diretto
075800150317     C     KEYcsb03_C    chain     tncsb03l
075900150317     C                   if        %found(tncsb03l)
076000150317     C                   z-add     csbCAS        cCAS
076100150317     C                   movel     csbVCA        cVCA
076200150317     C                   endif
076300150317     C*
076400150317     C                   ENDSR
076500161213
076600161213
076700161213
076800161213     C     CARTAB        BEGSR
076900161213     C*
077000161213     C     'VPC'         SETLL     tntbe01l
077100161213     C                   IF        %equal(tntbe01l)
077200161213     C     'VPC'         READE     tntbe01l
077300161213     C                   DOW       not %eof(tntbe01l)
077400161213     C                   EVAL      DVPC = tbeUNI
077500161213     C                   IF        §VPCVTA = 'S'
077600161213     C                   ADD       1             jVPC
077700161213     C                   EVAL      skVPC(jVPC) = %dec(%subst(tbeKE1:1:7):7:0)
077800161213     C                   ENDIF
077900161213     C     'VPC'         READE     tntbe01l
078000161213     C                   ENDDO
078100161213     C                   ENDIF
078200161213     C*
078300161213     C                   ENDSR
078400080527
078500080527
078600991027
078700991027      /TITLE Operazioni iniziali.
078800991027     C     *inzsr        BEGSR
078900991027     C*
079000991027     C     *ENTRY        PLIST
079100991027     C                   parm                    prmppt
079200991027     C     wrkesito      parm      wrkesito      prmesito
079300100222     C*
079400100222     C                   eval      acokcc = 151
079500080527     C*
079600080527     C     KEYtas30_P    KLIST
079700080527     C                   KFLD                    vabaas
079800080527     C                   KFLD                    vablnp
079900080527     C                   KFLD                    vabnrs
080000080527     C                   KFLD                    vabnsp
080100100212     C*
080200100212     C     KEYtaA30_C    KLIST
080300100212     C                   KFLD                    vabaas
080400100212     C                   KFLD                    vablnp
080500100212     C                   KFLD                    vabnrs
080600100212     C                   KFLD                    vabnsp
080700100212     C                   KFLD                    taatrc
080800100216     C*
080900100216     C     KEYaco00_C    KLIST
081000100216     C                   KFLD                    acokut
081100100216     C                   KFLD                    acokcc
081200100216     C                   KFLD                    vabccm
081300100216     C*
081400100216     C     KEYind00_C    KLIST
081500100216     C                   KFLD                    acokut
081600100216     C                   KFLD                    acokcc
081700100216     C                   KFLD                    vabccm
081800120406     C*
081900120406     C     KEYblt01_P    KLIST
082000120406     C                   KFLD                    vabaas
082100120406     C                   KFLD                    vablnp
082200120406     C                   KFLD                    vabnrs
082300120406     C                   KFLD                    vabnsp
082400150317     C*
082500150317     C     KEYcsb03_C    KLIST
082600150317     C                   KFLD                    vabaas
082700150317     C                   KFLD                    vablnp
082800150317     C                   KFLD                    vabnrs
082900150317     C                   KFLD                    vabnsp
083000120406     C*
083100120406     C     KEYars01_C    KLIST
083200120406     C                   KFLD                    bltFLS
083300120406     C                   KFLD                    bltLNA
083400120406     C                   KFLD                    bltNRS
083500120406     C                   KFLD                    bltNSC
083600120406     C                   KFLD                    arsTRC
083700161213     C*
083800161213     C     KEYrab01_C    KLIST
083900161213     C                   KFLD                    vabAAS
084000161213     C                   KFLD                    vabLNP
084100161213     C                   KFLD                    vabNRS
084200161213     C                   KFLD                    vabNSP
084300991027     C*
084400991027     C                   ENDSR
