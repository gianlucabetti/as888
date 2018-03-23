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
001800180119     Ffnblp01l  if   E           k DISK    usropn
001900180119     F                                     extfile(LibFileBLP01)
002000180119     Ffipnd01l  if   E           k DISK
002100161213
002200991027
002300991027     D prmppt          s             50
002400991027     D prmesito        s              1
002500161213     D wrkesito        s                   like(prmesito)
002600161213     D DVPC          e ds                  inz
002700090320     D fnvabds       e ds                  extname(fnvab00t)
002800161213     D wIAS            s             14    inz
002900161213     D wPKB            s              8    inz
003000161213     D wPKB_RIL        s              8    inz
003100161213     D wVLB            s              6    inz
003200161213     D wVLB_RIL        s              9    inz
003300161213     D wQFT            s             14    inz
003400161213     D wCAS            s             14    inz
003500161213     D wCASOR          s             14    inz
003600161213     D wVMD            s             14    inz
003700161216     D VAWDTA          s                   like(O_VAWDTA)
003800161213     D wVAWDTA         s                   like(VAWDTA)
003900100212     D wTAARSC         s                   like(TAARSC)
004000100212     D wTAAIND         s                   like(TAAIND)
004100100212     D wTAACAP         s                   like(TAACAP)
004200100212     D wTAALOC         s                   like(TAALOC)
004300100212     D wTAAPRV         s                   like(TAAPRV)
004400120406     D wARSNOT_C       s                   like(ARSNOT)
004500150317     D wDCM            s                   like(pOutDcmFI)
004600150317     D wCCA            s                   like(pOutCcaFI)
004700150317     D cCAS            s                   like(csbCAS)
004800150317     D cVCA            s                   like(csbVCA)
004900161213     D wVABRMNR        s                   like(vabRMN) inz
005000161213     D wVABRMAR        s                   like(vabRMA) inz
005100170117     D pCSV            s              1    inz
005200170117     D pDLM            s              1    inz varying
005300161214     D iINTI           s               N   inz
005400161214     D iINTS           s               N   inz
005500161213     D iRAB            s               N   inz
005600161213     D iDCR            s               N   inz
005700161213     D iORIG           s               N   inz
005800161213     D iNRS            s               N   inz
005900161213     D iMITT           s               N   inz
006000161213     D iSEGN           s               N   inz
006100161213     D iRIL            s               N   inz
006200161213     D iCONS           s               N   inz
006300161213     D iCAS            s               N   inz
006400180119     D ipnd            s               N   inz
006500180214     D wpndipn         s             14    inz
006600161213
006700100212
006800120406     D LibFileBLT01    s             21A   inz
006900120406     D LibFileARS01    s             21A   inz
007000180119     D LibFileBLP01    s             21A   inz
007100180119     D LibFileAR401    s             21A   inz
007200120406     D currSysNeta     s              8A   inz('*NULL')
007300161213
007400161213
007500161213     D*-----------
007600161213     D* SCHIERE
007700161213     D*-----------
007800161213     D skVPC           S              7  0 DIM(500)
007900161213     D jVPC            S              3  0 INZ(*zeros)
008000080613
008100120406
008200080613     D*------------------
008300080613     D* LINKING A DEFINIZIONI ESTERNE
008400080613     D*------------------
008500080613     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008600100212     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008700120406     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
008800120406     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
008900120406
009000991027
009100120406     C*
009200120406     C* Reperisco il sistema AS/400 corrente
009300120406     C                   callp     UBRTVNETA_Rtv(currSysNeta)
009400120406     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
009500120406     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
009600120406     C                   eval      LibFileARS01 = 'FILTRA201/FIARS01L'
009700180119     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
009800120406     C                   else
009900120406     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
010000120406     C                   eval      LibFileARS01 = 'FILTRAPRD/FIARS01L'
010100180119     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
010200120406     C                   endif
010300120406     C*
010400120406     C                   if        not %open(FNBLT01L)
010500120406     C                   open      FNBLT01L
010600120406     C                   endif
010700120406     C*
010800120406     C                   if        not %open(FIARS01L)
010900120406     C                   open      FIARS01L
011000120406     C                   endif
011100180119     C                   if        not %open(Fnblp01L)
011200180119     C                   open      Fnblp01L
011300180119     C                   endif
011400180119     C*
011500170118     C*
011600170118     C* Avvio il monitoring del intero flusso
011700170118     C                   monitor
011800170118     C*
011900170118     C* Effettuo traduzione
012000161213     C                   EXSR      cartab
012100030908     C                   EXSR      chkRECFILMBR
012200991027     C                   EXSR      traduci
012300170118     C*
012400170118     C* Gestisco eventuale errore
012500170118     C                   on-error
012600170118     C*
012700170118     C* Imposto esito transazione a errore
012800170118     C                   EVAL      wrkesito = '2'
012900170118     C*
013000170118     C* Se errore => emetto operazione d ROLLBACK
013100170118     C                   ROLBK
013200170118     C*
013300170118     C* Arresto il monitoring
013400170118     C                   endmon
013500120406     C*
013600120406     C                   if        %open(FNBLT01L)
013700120406     C                   close     FNBLT01L
013800120406     C                   endif
013900120406     C*
014000120406     C                   if        %open(FIARS01L)
014100120406     C                   close     FIARS01L
014200120406     C                   endif
014300180119     C                   if        %open(Fnblp01L)
014400180119     C                   close     FNBLP01L
014500180119     C                   endif
014600120406     C*
014700921023     C                   SETON                                        LR
014800120406
014900120406
015000030908
015100030908     C     chkRECFILMBR  BEGSR
015200030908     C*
015300100212     C                   READ      TIVAWWWT                               55
015400030908     C*
015500030908     C                   ENDSR
015600120406
015700120406
015800991027
015900991027     C     traduci       BEGSR
016000161213     C*
016100161213     C* Inizializzazione indicatori di procedure per parametri input
016200161216     C                   setoff                                       20
016300161214     C                   eval      iINTI = *off
016400161214     C                   eval      iINTS = *off
016500161213     C                   eval      iRAB  = *off
016600161213     C                   eval      iDCR  = *off
016700161213     C                   eval      iORIG = *off
016800161213     C                   eval      iNRS  = *off
016900161213     C                   eval      iMITT = *off
017000161213     C                   eval      iSEGN = *off
017100161213     C                   eval      iRIL  = *off
017200161213     C                   eval      iCONS = *off
017300161213     C                   eval      iCAS  = *off
017400180119     C                   eval      ipnd  = *off
017500161214     C*
017600161214     C* Se richiesta intestazione
017700161214     C                   select
017800161214     C                   when      %subst(prmppt:1:1) = 'I'
017900161214     C                   eval      iINTI = *on
018000161214     C                   when      %subst(prmppt:1:1) = 'S'
018100161214     C                   eval      iINTS = *on
018200161214     C                   endsl
018300161214     C*
018400161214     C* Se richiesto un particolare carattere d separatore campi
018500161214     C                   if        %subst(prmppt:2:1) <> *blanks
018600170117     C                   eval      pCSV = %subst(prmppt:2:1)
018700161214     C                   else
018800170117     C                   movel     ','           pCSV
018900161214     C                   endif
019000161214     C*
019100161214     C* Se richiesto un particolare carattere d separatore decimale
019200161214     C                   if        %subst(prmppt:3:1) = ','
019300161214     C                   setoff                                       60
019400161214     C                   else
019500161214     C                   seton                                        60
019600161214     C                   endif
019700170117     C*
019800170117     C* Verifico il carattere delimitatore test indicato nei parametri del cliente/tipo file
019900170117     C                   select
020000170117     C                   when      %subst(prmppt:4:1) = *blanks
020100170117     C                   eval      pDLM = '"'
020200170117     C                   when      %subst(prmppt:4:1) = 'N'
020300170117     C                   eval      pDLM = ''
020400170117     C                   other
020500170117     C                   eval      pDLM = %subst(prmppt:4:1)
020600170117     C                   endsl
020700170111     C*
020800170111     C* Se richiesto sviluppo per "raccordo accorpamento" (FIRAB)
020900170117     C                   if        %subst(prmppt:5:1) = 'S'
021000170111     C                   eval      iRAB = *on
021100170111     C                   endif
021200161214     C*
021300161214     C* Se richiesto valorizzo da data consegna richiesta con il proprio valore
021400170117     C                   if        %subst(prmppt:6:1) = 'R'
021500161214     C                   eval      iDCR = *on
021600161214     C                   endif
021700161214     C*
021800161214     C* Se richiesto ritorno solamente le bolle originali
021900170117     C                   if        %subst(prmppt:7:1) = 'O'
022000161214     C                   eval      iORIG = *on
022100161214     C                   endif
022200161214     C*
022300161214     C* Se richiesto ritorno solamente le bolle con serie
022400170117     C                   if        %subst(prmppt:8:1) = 'S'
022500161214     C                   eval      iNRS = *on
022600161214     C                   endif
022700170111     C*
022800170111     C* Se richiesto ritorno anche i dati relativi al mittente
022900170117     C                   if        %subst(prmppt:9:1) = 'S'
023000170111     C                   eval      iMITT = *on
023100170111     C                   endif
023200170111     C*
023300170111     C* Se richiesto ritorno anche i dati del segnacollo cliente
023400170117     C                   if        %subst(prmppt:10:1) = 'S'
023500170111     C                   eval      iSEGN = *on
023600170111     C                   endif
023700170111     C*
023800170111     C* Se richiesto ritorno anche i dati relativi a peso e volume rilevati
023900170117     C                   if        %subst(prmppt:11:1) = 'S'
024000170111     C                   eval      iRIL = *on
024100170111     C                   endif
024200170111     C*
024300170111     C* Se richiesto ritorno anche i dati relativi a data consegna e codice consegna anomala
024400170117     C                   if        %subst(prmppt:12:1) = 'S'
024500170111     C                   eval      iCONS = *on
024600170111     C                   endif
024700170111     C*
024800170117     C                   if        %subst(prmppt:13:1) = 'S'
024900170111     C                   eval      iCAS = *on
025000170111     C                   endif
025100180119     C* Se richiesto passo anche parcel DPD
025200180119     C                   if        %subst(prmppt:15:1) = 'S'
025300180119     C                   eval      ipnd  = *on
025400180119     C                   endif
025500991027     C*
025600161213     C* Scorro tutto il file
025700161213     C                   read      tivgdtmp
025800161213     C                   dow       not %eof(tivgdtmp)
025900170119     C*
026000170119     C* Inizializzo buffer output
026100170119     C                   clear                   TIVAW000
026200161213     C*
026300161213     C* Valorizzo dati "corrrenti"
026400161213     C                   movel     vgddta        fnvabds
026500080613     C*
026600161213     C* Inizializzo indicatore di record corrente OK
026700080613     C                   setoff                                       31
026800080613     C*
026900080613     C* Chiamata metodo GetLblTyp
027000161213     C                   if        iORIG
027100161213     C                   if        UBLBLSPE_GetLblTyp(VABAAS
027200080613     C                                               :VABLNP
027300080613     C                                               :VABNRS
027400080613     C                                               :VABNSP
027500100212     C                                               :pOutLblTyp
027600100212     C                                               :pOutAnnoBO
027700100212     C                                               :pOutLineaParBO
027800100212     C                                               :pOutSerieBO
027900100212     C                                               :pOutNumSpedBO
028000100212     C                                               :pOutDcmBO
028100100212     C                                               :pOutCcaBO
028200100212     C                                               :pOutRblBO) = 0
028300100212     C                   if        pOutLblTyp <> 'O'
028400080613     C                   seton                                        31
028500080613     C                   endif
028600080613     C                   endif
028700161213     C                   endif
028800080618     C*
028900080618     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
029000161213     C                   if        iNRS
029100161213     C                   if        VABNRS = *zeros
029200080618     C                   seton                                        31
029300080618     C                   endif
029400161213     C                   endif
029500080613     C*
029600161213     C* Se record ok => procedo
029700080613     C                   if        *in31 = *off
029800161213     C*
029900161213     C* Se richiesto reperimento dati bolla da Sede
030000161213     C                   z-add     VABDCR        savVABDCR         8 0
030100161213     C                   if        iDCR
030200161213     C                   exsr      reptas
030300161213     C                   endif
030400161213     C*
030500161213     C* Se richiesto nei parametri d ingresso ritorno anche i dati del mittente originale
030600161213     C                   if        iMITT
030700161213     C                   exsr      reptaA
030800161213     C                   endif
030900161213     C*
031000161213     C* Se richiesto nei parametri d ingresso ritorno anche dati esito consegna
031100161213     C                   clear                   wDCM
031200161213     C                   clear                   wCCA
031300161213     C                   if        iCONS
031400161213     C* Chiamata metodo GetLastChild
031500161213     C                   movel     *blanks       wEsito2           1
031600161213     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
031700161213     C                                                VABAAS
031800161213     C                                               :VABLNP
031900161213     C                                               :VABNRS
032000161213     C                                               :VABNSP
032100161213     C                                               :pOutAnnoFI
032200161213     C                                               :pOutLineaParFI
032300161213     C                                               :pOutSerieFI
032400161213     C                                               :pOutNumSpedFI
032500161213     C                                               :pOutDcmFI
032600161213     C                                               :pOutCcaFI))
032700161213     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
032800161213     C                   if        pOutDcmFI > *zeros
032900161213     C                   move      pOutCcaFI     wCCA
033000161213     C                   move      pOutDcmFI     wDCM
033100161213     C                   endif
033200161213     C                   endif
033300180213     c*
033400180213     c* Se richiesto reperisco parcel DPD
033500180214     c                   clear                   wpndipn
033600180213     c                   if        ipnd
033700180213     c                   exsr      Reppnd
033800180213     c                   endif
033900161213     C*
034000161213     C* Se richiesto nei parametri d ingresso ritorno anche dati contrassegno originale
034100161213     C                   clear                   wCASOR
034200161213     C                   clear                   cCAS
034300161213     C                   clear                   cVCA
034400161213     C                   if        iCAS
034500161213     C                   exsr      repCSB
034600161213     C                   endif
034700161213     C*
034800161213     C* Se richiesta sviluppo riferimenti "accorpati" => verifico file raccordo FIRAB
034900161213     C                   clear                   wVABRMNR
035000161213     C                   clear                   wVABRMAR
035100161213     C                   if        iRAB
035200161213     C     KEYrab01_C    setll     firab01l
035300161213     C                   if        %equal(firab01l)
035400161213     C     KEYrab01_C    reade     firab01l
035500161213     C                   dow       not %eof(firab01l)
035600161213     C                   eval      wVABRMNR = rabRMNR
035700161213     C                   eval      wVABRMAR = rabRMAR
035800161216     C                   exsr      valOUT
035900161213     C     KEYrab01_C    reade     firab01l
036000161213     C                   enddo
036100161213     C*
036200161213     C                   else
036300161216     C                   exsr      valOUT
036400161213     C                   endif
036500161213     C*
036600161213     C                   else
036700161216     C                   exsr      valOUT
036800161213     C                   endif
036900161213     C*
037000161213     C* Se partcolarità abilitata in tabella 'VPC' => eseguo
037100161213     C     vabCCM        lookup    skVPC                                  05
037200161213     C                   if        %found
037300161213     C                   call      'TIS7T99R'
037400161213     C                   parm      vabCCM        I99CCM            7 0
037500161213     C                   parm      vabAAS        I99AAS            4 0
037600161213     C                   parm      vabLNP        I99LNP            3 0
037700161213     C                   parm      vabNRS        I99NRS            2 0
037800161213     C                   parm      vabNSP        I99NSP            7 0
037900161213     C                   endif
038000120406     C*
038100080613     C                   endif
038200991027     C*
038300161213     C                   read      tivgdtmp
038400161213     C                   enddo
038500991027     C*
038600161213     C                   eval      wrkesito = '0'
038700991027     C*
038800910830     C                   ENDSR
038900161216
039000161216
039100161216
039200161216     C*
039300161216     C     wriINT        BEGSR
039400161216     C*
039500161216     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
039600161216     C                   if        (iINTI OR iINTS) AND *in55 = *on
039700161216     C                   if        iINTI
039800170117     C                   eval      O_VAWDTA =
039900170117     C                             pDLM+'VABATB'                     +pDLM+pCSV+
040000170117     C                             pDLM+'VABCCM'                     +pDLM+pCSV+
040100170117     C                             pDLM+'VABLNP'                     +pDLM+pCSV+
040200170117     C                             pDLM+'VABAAS'                     +pDLM+pCSV+
040300170117     C                             pDLM+'VABMGS'                     +pDLM+pCSV+
040400170117     C                             pDLM+'VABNRS'                     +pDLM+pCSV+
040500170117     C                             pDLM+'VABNSP'                     +pDLM+pCSV+
040600170117     C                             pDLM+'VABCBO'                     +pDLM+pCSV+
040700170117     C                             pDLM+'VABLNA'                     +pDLM+pCSV+
040800170117     C                             pDLM+'VABRSD'                     +pDLM+pCSV+
040900170117     C                             pDLM+'VABRD2'                     +pDLM+pCSV+
041000170117     C                             pDLM+'VABIND'                     +pDLM+pCSV+
041100170117     C                             pDLM+'VABCAD'                     +pDLM+pCSV+
041200170117     C                             pDLM+'VABLOD'                     +pDLM+pCSV+
041300170117     C                             pDLM+'VABPRD'                     +pDLM+pCSV+
041400170117     C                             pDLM+'VABNZD'                     +pDLM+pCSV+
041500170117     C                             pDLM+'VABGC1'                     +pDLM+pCSV+
041600170117     C                             pDLM+'VABGC2'                     +pDLM+pCSV+
041700170117     C                             pDLM+'VABCTR'                     +pDLM+pCSV+
041800170117     C                             pDLM+'VABTSP'                     +pDLM+pCSV+
041900170117     C                             pDLM+'VABIAS'                     +pDLM+pCSV+
042000170117     C                             pDLM+'VABVAS'                     +pDLM+pCSV+
042100170117     C                             pDLM+'VABNAS'                     +pDLM+pCSV+
042200170117     C                             pDLM+'VABNCL'                     +pDLM+pCSV+
042300170117     C                             pDLM+'VABPKB'                     +pDLM+pCSV+
042400170117     C                             pDLM+'VABVLB'                     +pDLM+pCSV+
042500170117     C                             pDLM+'VABQFT'                     +pDLM+pCSV+
042600170117     C                             pDLM+'VABCAS'                     +pDLM+pCSV+
042700170117     C                             pDLM+'VABTIC'                     +pDLM+pCSV+
042800170117     C                             pDLM+'VABVCA'                     +pDLM+pCSV+
042900170117     C                             pDLM+'VABGCA'                     +pDLM+pCSV+
043000170117     C                             pDLM+'VABRMN'                     +pDLM+pCSV+
043100170117     C                             pDLM+'VABRMA'                     +pDLM+pCSV+
043200170117     C                             pDLM+'VABNCD'                     +pDLM+pCSV+
043300170117     C                             pDLM+'VABNCA'                     +pDLM+pCSV+
043400170117     C                             pDLM+'VABXCO'                     +pDLM+pCSV+
043500170117     C                             pDLM+'VABNOT'                     +pDLM+pCSV+
043600170117     C                             pDLM+'VABNT2'                     +pDLM+pCSV+
043700170117     C                             pDLM+'VABZNC'                     +pDLM+pCSV+
043800170117     C                             pDLM+'VABCTM'                     +pDLM+pCSV+
043900170117     C                             pDLM+'VABFFD'                     +pDLM+pCSV+
044000170117     C                             pDLM+'VABDCR'                     +pDLM+pCSV+
044100170117     C                             pDLM+'VABTCR'                     +pDLM+pCSV+
044200170117     C                             pDLM+'VABHCR'                     +pDLM+pCSV+
044300170117     C                             pDLM+'VABCTS'                     +pDLM+pCSV+
044400170117     C                             pDLM+'VABFTM'                     +pDLM+pCSV+
044500170117     C                             pDLM+'VABVMD'                     +pDLM+pCSV+
044600170117     C                             pDLM+'VABVAD'                     +pDLM+pCSV+
044700170117     C                             pDLM+'VABGMA'                     +pDLM+pCSV+
044800170117     C                             pDLM+'VABGGA'                     +pDLM+pCSV+
044900170117     C                             pDLM+'VABGVA'                     +pDLM+pCSV+
045000170117     C                             pDLM+'VABTC1'                     +pDLM+pCSV+
045100170117     C                             pDLM+'VABTC2'                     +pDLM+pCSV+
045200170117     C                             pDLM+'VABSCL'                     +pDLM+pCSV+
045300170117     C                             pDLM+'VABANT'                     +pDLM+pCSV+
045400170117     C                             pDLM+'VABRMO'                     +pDLM+pCSV+
045500170117     C                             pDLM+'VABCMO'                     +pDLM+pCSV+
045600170117     C                             pDLM+'VABNMO'                     +pDLM
045700161216     C                   endif
045800161216     C*
045900161216     C                   if        iINTS
046000170117     C                   eval      O_VAWDTA =
046100170117     C                             pDLM+'VABCCM'                     +pDLM+pCSV+
046200170117     C                             pDLM+'VABAAS'                     +pDLM+pCSV+
046300170117     C                             pDLM+'VABMGS'                     +pDLM+pCSV+
046400170117     C                             pDLM+'VABRMA'                     +pDLM+pCSV+
046500170117     C                             pDLM+'VABRMN'                     +pDLM+pCSV+
046600170117     C                             pDLM+'VABSPED'                    +pDLM+pCSV+
046700170117     C                             pDLM+'VABRSD'                     +pDLM+pCSV+
046800170117     C                             pDLM+'VABIND'                     +pDLM+pCSV+
046900170117     C                             pDLM+'VABCAD'                     +pDLM+pCSV+
047000170117     C                             pDLM+'VABLOD'                     +pDLM+pCSV+
047100170117     C                             pDLM+'VABPRD'                     +pDLM
047200161216     C                   endif
047300161216     C*
047400161216     C* Verifico se richiesto lancio per "raccordo accorpamento" (FIRAB)
047500170111     C                   if        iRAB
047600170117     C                   eval      O_VAWDTA =
047700170117     C                                 %trim(O_VAWDTA)                    +pCSV+
047800170117     C                             pDLM+'VABRMNR'                    +pDLM+pCSV+
047900170117     C                             pDLM+'VABRMAR'                    +pDLM
048000161216     C                   endif
048100161216     C*
048200170111     C                   if        iMITT
048300170117     C                   eval      O_VAWDTA =
048400170117     C                                 %trim(O_VAWDTA)                    +pCSV+
048500170117     C                             pDLM+'TAARSC'                     +pDLM+pCSV+
048600170117     C                             pDLM+'TAAIND'                     +pDLM+pCSV+
048700170117     C                             pDLM+'TAACAP'                     +pDLM+pCSV+
048800170117     C                             pDLM+'TAALOC'                     +pDLM+pCSV+
048900170117     C                             pDLM+'TAAPRV'                     +pDLM
049000161216     C                   endif
049100161216     C*
049200170111     C                   if        iSEGN
049300170117     C                   eval      O_VAWDTA =
049400170117     C                                       %trim(O_VAWDTA)              +pCSV+
049500170117     C                             pDLM+'SEGNACOLLO_CLI'             +pDLM
049600161216     C*
049700170111     C                   if        iRIL
049800170117     C                   eval      O_VAWDTA =
049900170117     C                                       %trim(O_VAWDTA)              +pCSV+
050000170117     C                             pDLM+'PESO_RIL'                   +pDLM+pCSV+
050100170117     C                             pDLM+'VOLUME_RIL'                 +pDLM
050200161216     C                   endif
050300161216     C                   endif
050400161216     C*
050500170111     C                   if        iCONS
050600170117     C                   eval      O_VAWDTA =
050700170117     C                                       %trim(O_VAWDTA)              +pCSV+
050800170117     C                             pDLM+'DATA_CONS'                  +pDLM+pCSV+
050900170117     C                             pDLM+'CCA'                        +pDLM
051000161216     C                   endif
051100161216     C*
051200170111     C                   if        iCAS
051300170117     C                   eval      O_VAWDTA =
051400170117     C                                       %trim(O_VAWDTA)              +pCSV+
051500170117     C                             pDLM+'CAS_ORIG'                   +pDLM+pCSV+
051600170117     C                             pDLM+'VCA_ORIG'                   +pDLM
051700161216     C                   endif
051800180119     C                   if        ipnd
051900180119     C                   eval      O_VAWDTA =
052000180119     C                                       %trim(O_VAWDTA)              +pCSV+
052100180119     C                             pDLM+'PARCEL'                     +pDLM
052200180119     C                   endif
052300161216     C*
052400161216     C                   write     TIVAW000
052500161216     C                   endif
052600161216     C*
052700161216     C                   ENDSR
052800161213
052900161213
053000161213
053100161213     C*
053200161216     C     valOUT        BEGSR
053300161213     C*
053400161213     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
053500161213     C  N60              EVAL      wIAS = %trim(%editw(VABIAS:'        0 ,   '))
053600161213     C   60              EVAL      wIAS = %trim(%editw(VABIAS:'        0 .   '))
053700161213     C  N60              EVAL      wPKB = %trim(%editw(VABPKB:'    0 , '))
053800161213     C   60              EVAL      wPKB = %trim(%editw(VABPKB:'    0 . '))
053900161213     C  N60              EVAL      wVLB = %trim(%editw(VABVLB:'0 ,   '))
054000161213     C   60              EVAL      wVLB = %trim(%editw(VABVLB:'0 .   '))
054100161213     C  N60              EVAL      wQFT = %trim(%editw(VABQFT:'        0 ,   '))
054200161213     C   60              EVAL      wQFT = %trim(%editw(VABQFT:'        0 .   '))
054300161213     C  N60              EVAL      wCAS = %trim(%editw(VABCAS:'        0 ,   '))
054400161213     C   60              EVAL      wCAS = %trim(%editw(VABCAS:'        0 .   '))
054500161213     C  N60              EVAL      wCASOR = %trim(%editw(cCAS:'        0 ,   '))
054600161213     C   60              EVAL      wCASOR = %trim(%editw(cCAS:'        0 .   '))
054700161213     C  N60              EVAL      wVMD = %trim(%editw(VABVMD:'        0 ,   '))
054800161213     C   60              EVAL      wVMD = %trim(%editw(VABVMD:'        0 .   '))
054900161213     C*
055000161214     C                   if        iINTI OR (not iINTI AND not iINTS)
055100170117     C                   eval      VAWDTA  =
055200170117     C                             pDLM+           %trim(VABATB)     +pDLM+pCSV+
055300170117     C                                      %trim(%editc(VABCCM:'Q'))     +pCSV+
055400170117     C                                      %trim(%editc(VABLNP:'Q'))     +pCSV+
055500170117     C                                      %trim(%editc(VABAAS:'Q'))     +pCSV+
055600170117     C                                      %trim(%editc(VABMGS:'Q'))     +pCSV+
055700170117     C                                      %trim(%editc(VABNRS:'Q'))     +pCSV+
055800170117     C                                      %trim(%editc(VABNSP:'Q'))     +pCSV+
055900170117     C                             pDLM+           %trim(VABCBO)     +pDLM+pCSV+
056000170117     C                                      %trim(%editc(VABLNA:'Q'))     +pCSV+
056100170117     C                             pDLM+           %trim(VABRSD)     +pDLM+pCSV+
056200170117     C                             pDLM+           %trim(VABRD2)     +pDLM+pCSV+
056300170117     C                             pDLM+           %trim(VABIND)     +pDLM+pCSV+
056400170117     C                             pDLM+           %trim(VABCAD)     +pDLM+pCSV+
056500170117     C                             pDLM+           %trim(VABLOD)     +pDLM+pCSV+
056600170117     C                             pDLM+           %trim(VABPRD)     +pDLM+pCSV+
056700170117     C                             pDLM+           %trim(VABNZD)     +pDLM+pCSV+
056800170117     C                             pDLM+           %trim(VABGC1)     +pDLM+pCSV+
056900170117     C                             pDLM+           %trim(VABGC2)     +pDLM+pCSV+
057000170117     C                                      %trim(%editc(VABCTR:'Q'))     +pCSV+
057100170117     C                             pDLM+           %trim(VABTSP)     +pDLM+pCSV+
057200170117     C                                                    %trim(wIAS)     +pCSV+
057300170117     C                             pDLM+           %trim(VABVAS)     +pDLM+pCSV+
057400170117     C                             pDLM+           %trim(VABNAS)     +pDLM+pCSV+
057500170117     C                                      %trim(%editc(VABNCL:'Q'))     +pCSV+
057600170117     C                                                    %trim(wPKB)     +pCSV+
057700170117     C                                                    %trim(wVLB)     +pCSV+
057800170117     C                                                    %trim(wQFT)     +pCSV+
057900170117     C                                                    %trim(wCAS)     +pCSV+
058000170117     C                             pDLM+           %trim(VABTIC)     +pDLM+pCSV+
058100170117     C                             pDLM+           %trim(VABVCA)     +pDLM+pCSV+
058200170117     C                             pDLM+           %trim(VABGCA)     +pDLM+pCSV+
058300170117     C                                      %trim(%editc(VABRMN:'Q'))     +pCSV+
058400170117     C                             pDLM+           %trim(VABRMA)     +pDLM+pCSV+
058500170117     C                                      %trim(%editc(VABNCD:'Q'))     +pCSV+
058600170117     C                                      %trim(%editc(VABNCA:'Q'))     +pCSV+
058700170117     C                             pDLM+           %trim(VABXCO)     +pDLM+pCSV+
058800170117     C                             pDLM+           %trim(VABNOT)     +pDLM+pCSV+
058900170117     C                             pDLM+           %trim(VABNT2)     +pDLM+pCSV+
059000170117     C                                      %trim(%editc(VABZNC:'Q'))     +pCSV+
059100170117     C                             pDLM+           %trim(VABCTM)     +pDLM+pCSV+
059200170117     C                             pDLM+           %trim(VABFFD)     +pDLM+pCSV+
059300170117     C                                      %trim(%editc(VABDCR:'Q'))     +pCSV+
059400170117     C                             pDLM+           %trim(VABTCR)     +pDLM+pCSV+
059500170117     C                                      %trim(%editc(VABHCR:'Q'))     +pCSV+
059600170117     C                             pDLM+           %trim(VABCTS)     +pDLM+pCSV+
059700170117     C                             pDLM+           %trim(VABFTM)     +pDLM+pCSV+
059800170117     C                                                    %trim(wVMD)     +pCSV+
059900170117     C                             pDLM+           %trim(VABVAD)     +pDLM+pCSV+
060000170117     C                             pDLM+           %trim(VABGMA)     +pDLM+pCSV+
060100170117     C                             pDLM+           %trim(VABGGA)     +pDLM+pCSV+
060200170117     C                             pDLM+           %trim(VABGVA)     +pDLM+pCSV+
060300170117     C                             pDLM+           %trim(VABTC1)     +pDLM+pCSV+
060400170117     C                             pDLM+           %trim(VABTC2)     +pDLM+pCSV+
060500170117     C                             pDLM+           %trim(VABSCL)     +pDLM+pCSV+
060600170117     C                                      %trim(%editc(VABANT:'Q'))     +pCSV+
060700170117     C                             pDLM+           %trim(VABRMO)     +pDLM+pCSV+
060800170117     C                             pDLM+           %trim(VABCMO)     +pDLM+pCSV+
060900170117     C                             pDLM+           %trim(VABNMO)     +pDLM
061000161214     C                   endif
061100161214     C*
061200161214     C                   if        iINTS
061300170117     C                   eval      VAWDTA = %trim(%editc(VABCCM:'X'))     +pCSV+
061400170117     C                                      %trim(%editc(VABAAS:'X'))     +pCSV+
061500170117     C                                      %trim(%editc(VABMGS:'X'))     +pCSV+
061600170117     C                             pDLM+           %trim(VABRMA)     +pDLM+pCSV+
061700170117     C                                      %trim(%editc(VABRMN:'Q'))     +pCSV+
061800161214     C                                      %trim(%editc(VABLNP:'X'))+
061900161214     C                                      %trim(%editc(VABNRS:'X'))+
062000170117     C                                      %trim(%editc(VABNSP:'X'))     +pCSV+
062100170117     C                             pDLM+           %trim(VABRSD)     +pDLM+pCSV+
062200170117     C                             pDLM+           %trim(VABIND)     +pDLM+pCSV+
062300170117     C                             pDLM+           %trim(VABCAD)     +pDLM+pCSV+
062400170117     C                             pDLM+           %trim(VABLOD)     +pDLM+pCSV+
062500170117     C                             pDLM+           %trim(VABPRD)     +pDLM
062600161214     C                   endif
062700161213     C*
062800161213     C                   if        iRAB
062900170117     C                   eval      VAWDTA  =
063000170117     C                                       %trim(VAWDTA)                +pCSV+
063100170124     C                                    %trim(%editc(wVABRMNR:'Q'))     +pCSV+
063200170117     C                             pDLM+     %trim(wVABRMAR)         +pDLM
063300161213     C                   endif
063400161213     C*
063500161213     C                   if        iMITT
063600170117     C                   eval      VAWDTA  =
063700170117     C                                       %trim(VAWDTA)                +pCSV+
063800170117     C                             pDLM+     %trim(wTAARSC)          +pDLM+pCSV+
063900170117     C                             pDLM+     %trim(wTAAIND)          +pDLM+pCSV+
064000170117     C                             pDLM+     %trim(wTAACAP)          +pDLM+pCSV+
064100170117     C                             pDLM+     %trim(wTAALOC)          +pDLM+pCSV+
064200170117     C                             pDLM+     %trim(wTAAPRV)          +pDLM
064300161213     C                   endif
064400161213     C*
064500161213     C                   if        iCONS
064600170117     C                   eval      VAWDTA  =
064700170117     C                                       %trim(VAWDTA)                +pCSV+
064800170124     C                                       %trim(%editc(wDCM:'Q'))      +pCSV+
064900170117     C                             pDLM+     %trim(wCCA)             +pDLM
065000161213     C                   endif
065100161213     C*
065200161213     C                   if        iCAS
065300170117     C                   eval      VAWDTA  =
065400170117     C                                       %trim(VAWDTA)                +pCSV+
065500170124     C                                       %trim(wCASOR)                +pCSV+
065600170117     C                             pDLM+     %trim(cVCA)             +pDLM
065700161213     C                   endif
065800161213     C*
065900161213     C* Se richiesto nei parametri d ingresso ritorno anche i dati del segnacollo cliente
066000161213     C                   if        iSEGN
066100161213     C                   eval      wVAWDTA = VAWDTA
066200161213     C*
066300161213     C* Aggancio i dettagli colli e ciclo x ciascuno
066400161213     C     KEYblt01_P    setll     fnblt01l
066500161213     C                   if        %found(fnblt01l)
066600161213     C     KEYblt01_P    reade     fnblt01l
066700161213     C                   dow       not %eof(fnblt01l)
066800161213     C*
066900161213     C                   clear                   wARSNOT_C
067000161213     C*
067100161213     C* Reperisco il segnacollo cliente ("chi sono")
067200161213     C                   eval      arsTRC = 'C'
067300161213     C     KEYars01_C    chain     fiars01l
067400161213     C                   if        %found(fiars01l)
067500161213     C                   eval      wARSNOT_C = arsNOT
067600161213     C                   endif
067700161213     C*
067800170117     C                   eval      VAWDTA  = %trim(wVAWDTA)+pCSV+
067900170117     C                                       pDLM+%trim(wARSNOT_C)+pDLM
068000161213     C                   if        iRIL
068100161213     C                   if        bltFVC <> 'P'
068200161213     C                   eval      bltVUC = *zeros
068300161213     C                   endif
068400161213     C                   if        bltFPC <> 'P'
068500161213     C                   eval      bltPUC = *zeros
068600161213     C                   endif
068700161213     C  N60              EVAL      wPKB_RIL = %trim(%editw(bltPUC:'  0 ,   '))
068800161213     C   60              EVAL      wPKB_RIL = %trim(%editw(bltPUC:'  0 .   '))
068900161213     C  N60              EVAL      wVLB_RIL = %trim(%editw(bltVUC:'0 ,      '))
069000161213     C   60              EVAL      wVLB_RIL = %trim(%editw(bltVUC:'0 .      '))
069100170117     C                   EVAL      VAWDTA  = %trim(VAWDTA)  +pCSV+
069200170117     C                                       %trim(wPKB_RIL)+pCSV+
069300161213     C                                       %trim(wVLB_RIL)
069400180214     c                   if        ipnd
069500180214     C                   EVAL      VAWDTA  = %trim(VAWDTA)  +pCSV+
069600180214     C                                       pDLM+%trim(wpndipn)+pDLM
069700161213     C                   endif
069800180214     C                   endif
069900161213     C*
070000161216     C                   exsr      wriOUT
070100161213     C*
070200161213     C* Scorro il segnacollo successivo
070300161213     C     KEYtas30_P    reade     fnblt01l
070400161213     C                   enddo
070500161213     C*
070600161213     C* Se nn trovo il dettaglio segnacolli BRT in filiale scrivo almeno un record
070700161213     C                   else
070800170117     C                   eval      VAWDTA  = %trim(VAWDTA)+pCSV+pDLM+pDLM
070900180214     c* parcel DPD
071000180214     c                   if        ipnd
071100180214     C                   EVAL      VAWDTA  = %trim(VAWDTA)  +pCSV+
071200180214     C                                       pDLM+%trim(wpndipn)+pDLM
071300180214     C                   endif
071400180214
071500161216     C                   exsr      wriOUT
071600161213     C                   endif
071700161213     C*
071800161213     C                   else
071900180214
072000180214     c                   if        ipnd
072100180214     C                   EVAL      VAWDTA  = %trim(VAWDTA)  +pCSV+
072200180214     C                                       pDLM+%trim(wpndipn)+pDLM
072300180214     C                   endif
072400180214
072500161216     C                   exsr      wriOUT
072600161213     C                   endif
072700161213     C*
072800161213     C                   ENDSR
072900161216
073000161216
073100161216
073200161216     C*
073300161216     C     wriOUT        BEGSR
073400161216     C*
073500161216     C  N20              exsr      wriINT
073600161216     C                   eval      O_VAWDTA = VAWDTA
073700161216     C                   write     TIVAW000
073800161216     C                   seton                                        20
073900161216     C*
074000161216     C                   ENDSR
074100080527
074200080527
074300080527
074400080527     C*
074500080527     C     reptas        BEGSR
074600080527     C*
074700080527     C* Posizionamento diretto
074800080527     C     KEYtas30_P    chain     titas30c
074900080527     C                   if        %found(titas30c)
075000080527     C                   move      tasDCR        VABDCR
075100180119     c                   else
075200180119     c
075300180119     C     KEYtas30_P    chain     fnblp01l
075400180119     C                   if        %found(fnblp01l)
075500180119     C                   move      blpDCR        vabDCR
075600180119     C                   endif
075700080527     C                   endif
075800080527     C*
075900080527     C                   ENDSR
076000180213     c
076100180213     c
076200180213     C     reppnd        BEGSR
076300180213     c                   clear                   wpndipn
076400180213     C*
076500180213     C* Posizionamento diretto
076600180213     C     KEYtas30_P    chain     fipnd01l
076700180213     c                   if        %found(fipnd01l)
076800180213     c                   eval      wpndipn=pndipn
076900180213     c                   endif
077000180213     c                   ENDSR
077100180213
077200100212
077300100212
077400100212     C*
077500100212     C     reptaA        BEGSR
077600100212     C*
077700100212     C                   clear                   wTAARSC
077800100212     C                   clear                   wTAAIND
077900100212     C                   clear                   wTAACAP
078000100212     C                   clear                   wTAALOC
078100100212     C                   clear                   wTAAPRV
078200100216     C                   movel     *blanks       wTIPCCM           4
078300100216     C*
078400100225     C                   move(p)   tasCCM        wTIPCCM
078500100216     C                   if        wTIPCCM = '8888' or
078600100216     C                             wTIPCCM = '9999'
078700100212     C*
078800100212     C                   eval      taaTRC = 'M'
078900100212     C*
079000100212     C* Posizionamento diretto
079100100212     C     KEYtaA30_C    chain     titaA30c
079200100212     C                   if        %found(titaA30c)
079300100216     C                   movel     taaRSC        wTAARSC
079400100216     C                   movel     taaIND        wTAAIND
079500100216     C                   movel     taaCAP        wTAACAP
079600100216     C                   movel     taaLOC        wTAALOC
079700100216     C                   movel     taaPRV        wTAAPRV
079800100212     C                   endif
079900100216     C*
080000100216     C                   else
080100100216     C                   eval      acokut = 1
080200100216     C     KEYaco00_C    chain     cnaco00f
080300100216     C                   if        %found(cnaco00f)
080400100216     C                   movel     acoRAG        wTAARSC
080500100216     C     KEYind00_C    chain     cnind00f
080600100216     C                   if        %found(cnind00f)
080700100216     C                   movel     indVIA        wTAAIND
080800100216     C                   movel     indCAP        wTAACAP
080900100216     C                   movel     indCIT        wTAALOC
081000100216     C                   movel     indPRV        wTAAPRV
081100100216     C                   endif
081200100216     C                   endif
081300100216     C                   endif
081400100212     C*
081500100212     C                   ENDSR
081600150317
081700150317
081800150317
081900150317     C*
082000150317     C     repcsb        BEGSR
082100150317     C*
082200150317     C* Posizionamento diretto
082300150317     C     KEYcsb03_C    chain     tncsb03l
082400150317     C                   if        %found(tncsb03l)
082500150317     C                   z-add     csbCAS        cCAS
082600150317     C                   movel     csbVCA        cVCA
082700150317     C                   endif
082800150317     C*
082900150317     C                   ENDSR
083000161213
083100161213
083200161213
083300161213     C     CARTAB        BEGSR
083400161213     C*
083500161213     C     'VPC'         SETLL     tntbe01l
083600161213     C                   IF        %equal(tntbe01l)
083700161213     C     'VPC'         READE     tntbe01l
083800161213     C                   DOW       not %eof(tntbe01l)
083900161213     C                   EVAL      DVPC = tbeUNI
084000161213     C                   IF        §VPCVTA = 'S'
084100161213     C                   ADD       1             jVPC
084200161213     C                   EVAL      skVPC(jVPC) = %dec(%subst(tbeKE1:1:7):7:0)
084300161213     C                   ENDIF
084400161213     C     'VPC'         READE     tntbe01l
084500161213     C                   ENDDO
084600161213     C                   ENDIF
084700161213     C*
084800161213     C                   ENDSR
084900080527
085000080527
085100991027
085200991027      /TITLE Operazioni iniziali.
085300991027     C     *inzsr        BEGSR
085400991027     C*
085500991027     C     *ENTRY        PLIST
085600991027     C                   parm                    prmppt
085700991027     C     wrkesito      parm      wrkesito      prmesito
085800100222     C*
085900100222     C                   eval      acokcc = 151
086000080527     C*
086100080527     C     KEYtas30_P    KLIST
086200080527     C                   KFLD                    vabaas
086300080527     C                   KFLD                    vablnp
086400080527     C                   KFLD                    vabnrs
086500080527     C                   KFLD                    vabnsp
086600100212     C*
086700100212     C     KEYtaA30_C    KLIST
086800100212     C                   KFLD                    vabaas
086900100212     C                   KFLD                    vablnp
087000100212     C                   KFLD                    vabnrs
087100100212     C                   KFLD                    vabnsp
087200100212     C                   KFLD                    taatrc
087300100216     C*
087400100216     C     KEYaco00_C    KLIST
087500100216     C                   KFLD                    acokut
087600100216     C                   KFLD                    acokcc
087700100216     C                   KFLD                    vabccm
087800100216     C*
087900100216     C     KEYind00_C    KLIST
088000100216     C                   KFLD                    acokut
088100100216     C                   KFLD                    acokcc
088200100216     C                   KFLD                    vabccm
088300120406     C*
088400120406     C     KEYblt01_P    KLIST
088500120406     C                   KFLD                    vabaas
088600120406     C                   KFLD                    vablnp
088700120406     C                   KFLD                    vabnrs
088800120406     C                   KFLD                    vabnsp
088900150317     C*
089000150317     C     KEYcsb03_C    KLIST
089100150317     C                   KFLD                    vabaas
089200150317     C                   KFLD                    vablnp
089300150317     C                   KFLD                    vabnrs
089400150317     C                   KFLD                    vabnsp
089500120406     C*
089600120406     C     KEYars01_C    KLIST
089700120406     C                   KFLD                    bltFLS
089800120406     C                   KFLD                    bltLNA
089900120406     C                   KFLD                    bltNRS
090000120406     C                   KFLD                    bltNSC
090100120406     C                   KFLD                    arsTRC
090200161213     C*
090300161213     C     KEYrab01_C    KLIST
090400161213     C                   KFLD                    vabAAS
090500161213     C                   KFLD                    vabLNP
090600161213     C                   KFLD                    vabNRS
090700161213     C                   KFLD                    vabNSP
090800991027     C*
090900991027     C                   ENDSR
