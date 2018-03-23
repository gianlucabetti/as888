000100161212     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200121218     H DECEDIT('0,') DATEDIT(*DMY.)
000300000629
000400161212     Ftivgdtmp  if   E             DISK
000500161213 xxx F***tivgd00f  uf   E             DISK    commit
000600161213 xxx F***                                     rename(tivgd000:F_tivgd)
000700161213 xxx F***                                     prefix(F_)
000800161212     FTIVAWWWT  O    E             DISK    commit
000900161212     Ftntbe01l  if   E           k DISK
001000161215     Ffiar401l  if   E           k DISK    usropn
001100161215     F                                     extfile(LibFileAR401)
001200010131     Ftita430c  if   E           k DISK
001300080527     Ftitas30c  if   E           k DISK
001400121218     Ffnblt01l  if   E           k DISK    usropn
001500121218     F                                     extfile(LibFileBLT01)
001600121218     Ffiars01l  if   E           k DISK    usropn
001700121218     F                                     extfile(LibFileARS01)
001800991027
001900991027     D prmppt          s             50
002000991027     D prmesito        s              1
002100161213     D wrkesito        s                   like(prmesito)
002200161212     D DVPC          e ds                  inz
002300161213 xxx D*** DVGD001       e ds                  inz
002400161212     D fnvabDS       e ds                  extname(fnvab00t)
002500161212     D fnvabAsciDS   e ds                  extname(fnvabwwt) prefix(A_)
002600161212     D fnvabEbcdDS   e ds                  extname(fnvab1wt) prefix(E_)
002700161212     D fnvacAsciDS   e ds                  extname(fnvacwwt) prefix(A_)
002800121218
002900161212     D wARSNOT_C       s                   like(ARSNOT) inz
003000161212     D wVAWDTA         s                   like(VAWDTA) inz
003100161212     D l_VB_A          s              4  0 inz(%size(fnvabAsciDS))
003200161212     D l_VB_E          s              4  0 inz(%size(fnvabEbcdDS))
003300161212     D l_VC_A          s              4  0 inz(%size(fnvacAsciDS))
003400161213     D iVAB            s               N   inz
003500161213     D iVAC            s               N   inz
003600161213     D iASCI           s               N   inz
003700161213     D iEBCD           s               N   inz
003800161215     D iS7VB           s               N   inz
003900121218
004000121218     D LibFileBLT01    s             21A   inz
004100121218     D LibFileARS01    s             21A   inz
004200161215     D LibFileAR401    s             21A   inz
004300161212     D currSysNeta     s              8A   inz('*NULL')
004400161212
004500161212
004600161212     D*-----------
004700161212     D* SCHIERE
004800161212     D*-----------
004900161212     D skVPC           S              7  0 DIM(500)
005000161212     D jVPC            S              3  0 INZ(*zeros)
005100121218
005200161212
005300000629     D*----------
005400000629     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
005500161212     D*----------
005600000629     D* 10+3
005700000629     D                 DS
005800000629     D  WS15                   1      1
005900000629     D  WI15                   2     11
006000000629     D  WV15                  12     12
006100000629     D  WD15                  13     15
006200000629     D  WASC15                 1     15
006300000629     D*  6+1
006400000629     D                 DS
006500000629     D  WS09                   1      1
006600000629     D  WI09                   2      7
006700000629     D  WV09                   8      8
006800000629     D  WD09                   9      9
006900000629     D  WASC09                 1      9
007000000629     D*  2+3
007100000629     D                 DS
007200000629     D  WS07                   1      1
007300000629     D  WI07                   2      3
007400000629     D  WV07                   4      4
007500000629     D  WD07                   5      7
007600000629     D  WASC07                 1      7
007700121218
007800161212
007900010131     D*-------------------
008000010131     D* DS ESTENSIONE BOLLE TIPO RECORD "J" (X BOLE INGOMBRANTI NON DICHIARATE)
008100010131     D*-------------------
008200010131     D DSBL4J        E DS
008300991027
008400121218
008500121218     D*------------------
008600121218     D* LINKING A DEFINIZIONI ESTERNE
008700121218     D*------------------
008800121218     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
008900121218     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009000161212     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
009100161212     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
009200121218
009300161212
009400010131
009500010131
009600121218     C*
009700121218     C* Reperisco il sistema AS/400 corrente
009800121218     C                   callp     UBRTVNETA_Rtv(currSysNeta)
009900121218     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
010000121218     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
010100121218     C                   eval      LibFileARS01 = 'FILTRA201/FIARS01L'
010200161215     C                   eval      LibFileAR401 = 'FILTRA201/FIAR401L'
010300121218     C                   else
010400121218     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
010500121218     C                   eval      LibFileARS01 = 'FILTRAPRD/FIARS01L'
010600161215     C                   eval      LibFileAR401 = 'FILTRAPRD/FIAR401L'
010700121218     C                   endif
010800121218     C*
010900121218     C                   if        not %open(FNBLT01L)
011000121218     C                   open      FNBLT01L
011100121218     C                   endif
011200121218     C*
011300121218     C                   if        not %open(FIARS01L)
011400121218     C                   open      FIARS01L
011500121218     C                   endif
011600161215     C*
011700161215     C                   if        not %open(FIAR401L)
011800161215     C                   open      FIAR401L
011900161215     C                   endif
012000170118     C*
012100170118     C* Avvio il monitoring del intero flusso
012200170118     C                   monitor
012300170118     C*
012400170118     C* Effettuo traduzione
012500991027     C                   exsr      traduci
012600170118     C*
012700170118     C* Gestisco eventuale errore
012800170118     C                   on-error
012900170118     C*
013000170118     C* Imposto esito transazione a errore
013100170118     C                   EVAL      wrkesito = '2'
013200170118     C*
013300170118     C* Se errore => emetto operazione d ROLLBACK
013400170118     C                   ROLBK
013500170118     C*
013600170118     C* Arresto il monitoring
013700170118     C                   endmon
013800121218     C*
013900121218     C                   if        %open(FNBLT01L)
014000121218     C                   close     FNBLT01L
014100121218     C                   endif
014200121218     C*
014300121218     C                   if        %open(FIARS01L)
014400121218     C                   close     FIARS01L
014500121218     C                   endif
014600161215     C*
014700161215     C                   if        %open(FIAR401L)
014800161215     C                   close     FIAR401L
014900161215     C                   endif
015000121218     C*
015100991027     C                   seton                                        lr
015200000629
015300121218
015400000629
015500161212     C     traduci       BEGSR
015600161212     C*
015700161212     C* Carico tabelle di procedura
015800161212     C                   exsr      cartab
015900000629     C*
016000161212     C* Inizializzazione indicatori di procedure per parametri input
016100161213     C                   eval      *in45 = *off
016200161213     C                   eval      *in46 = *off
016300161213     C                   eval      *in47 = *off
016400161213     C                   eval      *in49 = *off
016500161213     C                   eval      iVAB  = *off
016600161213     C                   eval      iVAC  = *off
016700161213     C                   eval      iASCI = *off
016800161213     C                   eval      iEBCD = *off
016900161215     C                   eval      iS7VB = *off
017000161212     C*
017100161212     C* Se richiesto valorizzo da data consegna richiesta con il proprio valore
017200161212     C                   if        %subst(prmppt:5:1) = 'R'
017300161212     C                   seton                                        45
017400161212     C                   endif
017500161212     C*
017600161212     C* Se richiesto ritorno solamente le bolle originali
017700161212     C                   if        %subst(prmppt:6:1) = 'O'
017800161212     C                   seton                                        46
017900161212     C                   endif
018000161212     C*
018100161213     C* Se richiesto ritorno solamente le bolle con serie
018200161212     C                   if        %subst(prmppt:7:1) = 'S'
018300161213     C                   seton                                        47
018400161212     C                   endif
018500161212     C*
018600161212     C* Se richiesto ritorno anche i dati del segnacollo cliente
018700161212     C                   if        %subst(prmppt:9:1) = 'S'
018800161212     C                   seton                                        49
018900161212     C                   endif
019000161212     C*
019100161212     C* Se richiesto ritorno dati in formato EBCDIC
019200161212     C                   if        %subst(prmppt:10:1) = 'S'
019300161213     C                   eval      iEBCD = *on
019400161213     C                   else
019500161213     C                   eval      iASCI = *on
019600161212     C                   endif
019700161212     C*
019800161212     C* Se richiesto ritorno dati in formato "VAC"
019900161212     C                   if        %subst(prmppt:11:1) = 'S'
020000161213     C                   eval      iVAC = *on
020100161213     C                   else
020200161213     C                   eval      iVAB = *on
020300161213     C                   endif
020400161215     C*
020500161215     C* Se richiesto filtro solo dati da *pgm TIS7VBR
020600161215     C                   if        %subst(prmppt:12:1) = 'S'
020700161215     C                   eval      iS7VB = *on
020800161215     C                   endif
020900161212     C*
021000161213     C* Scorro tutto il file
021100161212     C                   read      tivgdtmp
021200161213     C                   dow       not %eof(tivgdtmp)
021300161212     C*
021400161212     C* Inizializzo DS di output
021500170119     C                   clear                   TIVAW000
021600161212     C                   clear                   fnvabAsciDS
021700161212     C                   clear                   fnvabEbcdDS
021800161212     C                   clear                   fnvacAsciDS
021900161212     C*
022000161212     C* Valorizzo dati "corrrenti"
022100161212     C                   exsr      defcam
022200161212     C                   movel     vgddta        fnvabDS
022300170111     C*
022400170111     C* Eseguo considerazioni su pacchi ingombranti aggiunti da Bartolini
022500170111     C                   exsr      repTA4
022600170111     C*
022700170111     C* Reperimento note 8 e 9
022800170111     C                   exsr      repNOT
022900170111     C*
023000170111     C* Reperimento dati bolla da Sede
023100170111     C   45              exsr      repTAS
023200161213     C*
023300161213     C* Gestisco formato richiesto VAB/VAC e ASCII/EBCDIC
023400161213     C                   select
023500161213     C                   when      iVAB and iASCI
023600161213     C                   exsr      fmtVAB_ASCI
023700161213     C                   when      iVAB and iEBCD
023800161213     C                   exsr      fmtVAB_EBCD
023900161213     C                   when      iVAC and iASCI
024000161213     C                   exsr      fmtVAC_ASCI
024100161213     C                   other
024200161213     C                   eval      wrkesito = '2'
024300161213     C                   endsl
024400161213     C*
024500161213     C* Se tutto ok => procedo
024600161213     C                   if        wrkesito = *blanks
024700080613     C*
024800161212     C* Inizializzo indicatore di record corrente OK
024900080613     C                   setoff                                       31
025000161212     C*
025100161212     C* Chiamata metodo GetLblTyp
025200161212     C   46              if        UBLBLSPE_GetLblTyp(vabAAS
025300161212     C                                               :vabLNP
025400161212     C                                               :vabNRS
025500161212     C                                               :vabNSP
025600161212     C                                               :pOutLblTyp
025700161212     C                                               :pOutAnnoBO
025800161212     C                                               :pOutLineaParBO
025900161212     C                                               :pOutSerieBO
026000161212     C                                               :pOutNumSpedBO
026100161212     C                                               :pOutDcmBO
026200161212     C                                               :pOutCcaBO
026300161212     C                                               :pOutRblBO) = 0
026400161212     C                   if        pOutLblTyp <> 'O'
026500161212     C                   seton                                        31
026600161212     C                   endif
026700161212     C                   endif
026800161212     C*
026900161212     C* Solo bolle con serie
027000161212     C   47              if        vabNRS = *zeros
027100161212     C                   seton                                        31
027200161212     C                   endif
027300161215     C*
027400161215     C* Solo record generati da *pgm TIS7VBR
027500161215     C                   if        iS7VB AND vgdpgm <> 'TIS7VBR'
027600161215     C                   seton                                        31
027700161215     C                   endif
027800080613     C*
027900161213     C* Se record ok => procedo
028000080613     C                   if        *in31 = *off
028100121218     C*
028200161212     C* Scarico il buffer di output formato richiesto VAB/VAC e ASCII/EBCDIC
028300161213     C                   select
028400161213     C                   when      iVAB and iASCI
028500161213     C                   eval      VAWDTA = fnvabAsciDS
028600161213     C                   when      iVAB and iEBCD
028700161213     C                   eval      VAWDTA = fnvabEbcdDS
028800161213     C                   when      iVAC and iASCI
028900161213     C                   eval      VAWDTA = fnvacAsciDS
029000161213     C                   endsl
029100121218     C*
029200121218     C* Se richiesto nei parametri d ingresso ritorno anche i dati del segnacollo cliente
029300161215     C                   if        *in49
029400121218     C*
029500121218     C                   eval      wVAWDTA = VAWDTA
029600121218     C*
029700121218     C* Aggancio i dettagli colli e ciclo x ciascuno
029800121218     C     KEYblt01_P    setll     fnblt01l
029900121218     C                   if        %found(fnblt01l)
030000121218     C     KEYblt01_P    reade     fnblt01l
030100121218     C                   dow       not %eof(fnblt01l)
030200121218     C*
030300121218     C                   clear                   wARSNOT_C
030400121218     C*
030500121218     C* Reperisco il segnacollo cliente ("chi sono")
030600121218     C                   eval      arsTRC = 'C'
030700121218     C     KEYars01_C    chain     fiars01l
030800121218     C                   if        %found(fiars01l)
030900121218     C                   eval      wARSNOT_C = arsNOT
031000121218     C                   endif
031100161213     C*
031200161213     C                   select
031300161213     C                   when      iVAB and iASCI
031400161213     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_A)+wARSNOT_C+'.'
031500161213     C                   when      iVAB and iEBCD
031600161213     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_E)+wARSNOT_C+'.'
031700161213     C                   when      iVAC and iASCI
031800161213     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VC_A)+wARSNOT_C+'.'
031900161213     C                   endsl
032000161212     C*
032100161212     C                   write     TIVAW000
032200121218     C*
032300121218     C* Scorro il segnacollo successivo
032400121218     C     KEYtas30_P    reade     fnblt01l
032500121218     C                   enddo
032600121218     C*
032700161212     C* Se non trovo il dettaglio segnacolli BRT in filiale scrivo almeno un record
032800121218     C                   else
032900161212     C                   write     TIVAW000
033000161212     C                   endif
033100121218     C*
033200121218     C                   else
033300161212     C                   write     TIVAW000
033400121218     C                   endif
033500000629     C*
033600161212     C* Se partcolarità abilitata in tabella 'VPC' => eseguo
033700161212     C     vabCCM        lookup    skVPC                                  05
033800161212     C                   if        %found
033900161212     C                   call      'TIS7T99R'
034000161212     C                   parm      vabCCM        I99CCM            7 0
034100161212     C                   parm      vabAAS        I99AAS            4 0
034200161212     C                   parm      vabLNP        I99LNP            3 0
034300161212     C                   parm      vabNRS        I99NRS            2 0
034400161212     C                   parm      vabNSP        I99NSP            7 0
034500161212     C                   endif
034600161212     C*
034700161212     C                   endif
034800161213     C*
034900161213     C                   endif
035000161213     C*
035100161213 xxx C* Aggiorno lo stato sul TIVGD00F
035200161213 xxx C***                eval      DVGD001 = vgdATTRIB
035300161213 xxx C***                if        §VGD001RRN > *zeros
035400161213 xxx C***  §VGD001RRN    chain(e)  tivgd00f
035500161213 xxx C***                if        %found(tivgd00f)
035600161213 xxx C***                eval      F_vgdSTO = 'S'
035700161213 xxx C***                update(e) F_tivgd
035800161213 xxx C***                endif
035900161213 xxx C***                endif
036000161212     C*
036100161212     C* Proseguo con il record successivo
036200161212     C                   read      tivgdtmp
036300161212     C                   enddo
036400000629     C*
036500161212     C                   if        wrkesito <> '2'
036600161212     C                   eval      wrkesito  = '0'
036700161212     C                   endif
036800000629     C*
036900000629     C                   ENDSR
037000161212
037100161212
037200161212
037300161212     C     defcam        BEGSR
037400161212     C*
037500161212     C                   MOVE      *BLANK        WS15
037600161212     C                   MOVE      ','           WV15
037700161212     C                   MOVE      *BLANK        WS07
037800161212     C                   MOVE      ','           WV07
037900161212     C                   MOVE      *BLANK        WS09
038000161212     C                   MOVE      ','           WV09
038100161212     C*
038200161212     C                   ENDSR
038300161212
038400161212
038500161212
038600161212     C*
038700161212     C     fmtVAB_ASCI   BEGSR
038800161212     C*
038900161212     C                   MOVE      VABAAS        A_VHBAAS
039000161212     C                   MOVE      VABLNP        A_VHBLNP
039100161212     C                   MOVE      VABNRS        A_VHBNRS
039200161212     C                   MOVE      VABNSP        A_VHBNSP
039300161212     C                   MOVE      VABMGS        A_VHBMGS
039400161212     C                   MOVE      VABCBO        A_VHBCBO
039500161212     C                   MOVE      VABLNA        A_VHBLNA
039600161212     C                   MOVE      VABRSD        A_VHBRSD
039700161212     C                   MOVE      VABPRD        A_VHBPRD
039800161212     C                   MOVE      VABGC1        A_VHBGC1
039900161212     C                   MOVE      VABGC2        A_VHBGC2
040000161212     C                   MOVE      VABCTR        A_VHBCTR
040100161212     C                   MOVE      VABCTS        A_VHBCTS
040200161212     C                   MOVE      VABFTM        A_VHBFTM
040300161212     C                   MOVE      VABTSP        A_VHBTSP
040400161212     C*
040500161212     C                   MOVE      VABIAS        WD15
040600161212     C                   MOVEL     VABIAS        WI15
040700161212     C                   MOVE      WASC15        A_VHBIAS
040800161212     C*
040900161212     C                   MOVE      VABVAS        A_VHBVAS
041000161212     C                   MOVE      VABNAS        A_VHBNAS
041100161212     C                   MOVE      VABNCL        A_VHBNCL
041200161212     C*
041300161212     C                   MOVE      VABPKB        WD09
041400161212     C                   MOVEL     VABPKB        WI09
041500161212     C                   MOVE      WASC09        A_VHBPKB
041600161212     C*
041700161212     C                   MOVE      VABVLB        WD07
041800161212     C                   MOVEL     VABVLB        WI07
041900161212     C                   MOVE      WASC07        A_VHBVLB
042000161212     C*
042100161212     C                   MOVE      VABQFT        WD15
042200161212     C                   MOVEL     VABQFT        WI15
042300161212     C                   MOVE      WASC15        A_VHBQFT
042400161212     C*
042500161212     C                   MOVE      VABVMD        WD15
042600161212     C                   MOVEL     VABVMD        WI15
042700161212     C                   MOVE      WASC15        A_VHBVMD
042800161212     C*
042900161212     C                   MOVE      VABTIC        A_VHBTIC
043000161212     C*
043100161212     C                   MOVE      VABCAS        WD15
043200161212     C                   MOVEL     VABCAS        WI15
043300161212     C                   MOVE      WASC15        A_VHBCAS
043400161212     C*
043500161212     C                   MOVE      VABVCA        A_VHBVCA
043600161212     C                   MOVE      VABCCM        A_VHBCCM
043700161212     C                   MOVE      VABRMN        A_VHBRMN
043800161212     C                   MOVE      VABRMA        A_VHBRMA
043900161212     C                   MOVE      VABRMO        A_VHBRMO
044000161212     C                   MOVE      VABFFD        A_VHBFFD
044100161212     C                   MOVE      VABTCR        A_VHBTCR
044200161212     C                   MOVE      VABDCR        A_VHBDCR
044300161212     C                   MOVE      VABHCR        A_VHBHCR
044400161212     C                   MOVE      VABTC1        A_VHBTC1
044500161212     C                   MOVE      VABTC2        A_VHBTC2
044600161212     C                   MOVE      VABATB        A_VHBATB
044700161212     C                   MOVE      VABRD2        A_VHBRD2
044800161212     C                   MOVE      VABIND        A_VHBIND
044900161212     C                   MOVE      VABCAD        A_VHBCAD
045000161212     C                   MOVE      VABLOD        A_VHBLOD
045100161212     C                   MOVE      VABNZD        A_VHBNZD
045200161212     C                   MOVE      VABGCA        A_VHBGCA
045300161212     C                   MOVE      VABNCD        A_VHBNCD
045400161212     C                   MOVE      VABNCA        A_VHBNCA
045500161212     C                   MOVE      VABXCO        A_VHBXCO
045600161212     C                   MOVE      VABNOT        A_VHBNOT
045700161212     C                   MOVE      VABNT2        A_VHBNT2
045800161215     C*
045900161215     C                   IF        wNOTE_8 <> *blanks
046000161215     C                   MOVE      wNOTE_8       A_VHBNOT
046100161215     C                   ENDIF
046200161215     C                   IF        wNOTE_9 <> *blanks
046300161215     C                   MOVE      wNOTE_9       A_VHBNT2
046400161215     C                   ENDIF
046500161215     C*
046600161212     C                   MOVE      VABZNC        A_VHBZNC
046700161212     C                   MOVE      VABCTM        A_VHBCTM
046800161212     C                   MOVE      VABVAD        A_VHBVAD
046900161212     C                   MOVE      VABGMA        A_VHBGMA
047000161212     C                   MOVE      VABGGA        A_VHBGGA
047100161212     C                   MOVE      VABGVA        A_VHBGVA
047200161212     C                   MOVE      VABSCL        A_VHBSCL
047300161212     C                   MOVE      VABANT        A_VHBANT
047400161212     C                   MOVE      VABCMO        A_VHBCMO
047500161212     C                   MOVE      VABNMO        A_VHBNMO
047600161212     C*
047700161212     C                   ENDSR
047800161212
047900161212
048000161212
048100161212     C*
048200161212     C     fmtVAB_EBCD   BEGSR
048300161212     C*
048400161212     C                   MOVE      VABAAS        E_VHBAAS
048500161212     C                   MOVE      VABLNP        E_VHBLNP
048600161212     C                   MOVE      VABNRS        E_VHBNRS
048700161212     C                   MOVE      VABNSP        E_VHBNSP
048800161212     C                   MOVE      VABMGS        E_VHBMGS
048900161212     C                   MOVE      VABCBO        E_VHBCBO
049000161212     C                   MOVE      VABLNA        E_VHBLNA
049100161212     C                   MOVE      VABRSD        E_VHBRSD
049200161212     C                   MOVE      VABPRD        E_VHBPRD
049300161212     C                   MOVE      VABGC1        E_VHBGC1
049400161212     C                   MOVE      VABGC2        E_VHBGC2
049500161212     C                   MOVE      VABCTR        E_VHBCTR
049600161212     C                   MOVE      VABCTS        E_VHBCTS
049700161212     C                   MOVE      VABFTM        E_VHBFTM
049800161212     C                   MOVE      VABTSP        E_VHBTSP
049900161212     C*
050000161212     C                   MOVE      VABIAS        E_VHBIAS
050100161212     C                   MOVE      VABVAS        E_VHBVAS
050200161212     C                   MOVE      VABNAS        E_VHBNAS
050300161212     C                   MOVE      VABNCL        E_VHBNCL
050400161212     C                   MOVE      VABPKB        E_VHBPKB
050500161212     C                   MOVE      VABVLB        E_VHBVLB
050600161212     C                   MOVE      VABQFT        E_VHBQFT
050700161212     C                   MOVE      VABVMD        E_VHBVMD
050800161212     C                   MOVE      VABTIC        E_VHBTIC
050900161212     C                   MOVE      VABCAS        E_VHBCAS
051000161212     C                   MOVE      VABVCA        E_VHBVCA
051100161212     C                   MOVE      VABCCM        E_VHBCCM
051200161212     C                   MOVE      VABRMN        E_VHBRMN
051300161212     C                   MOVE      VABRMA        E_VHBRMA
051400161212     C                   MOVE      VABRMO        E_VHBRMO
051500161212     C                   MOVE      VABFFD        E_VHBFFD
051600161212     C                   MOVE      VABTCR        E_VHBTCR
051700161212     C                   MOVE      VABDCR        E_VHBDCR
051800161212     C                   MOVE      VABHCR        E_VHBHCR
051900161212     C                   MOVE      VABTC1        E_VHBTC1
052000161212     C                   MOVE      VABTC2        E_VHBTC2
052100161212     C                   MOVE      VABATB        E_VHBATB
052200161212     C                   MOVE      VABRD2        E_VHBRD2
052300161212     C                   MOVE      VABIND        E_VHBIND
052400161212     C                   MOVE      VABCAD        E_VHBCAD
052500161212     C                   MOVE      VABLOD        E_VHBLOD
052600161212     C                   MOVE      VABNZD        E_VHBNZD
052700161212     C                   MOVE      VABGCA        E_VHBGCA
052800161212     C                   MOVE      VABNCD        E_VHBNCD
052900161212     C                   MOVE      VABNCA        E_VHBNCA
053000161212     C                   MOVE      VABXCO        E_VHBXCO
053100161212     C                   MOVE      VABNOT        E_VHBNOT
053200161215     C                   MOVE      VABNT2        E_VHBNT2
053300161215     C*
053400161215     C                   IF        wNOTE_8 <> *blanks
053500161215     C                   MOVE      wNOTE_8       E_VHBNOT
053600161215     C                   ENDIF
053700161215     C                   IF        wNOTE_9 <> *blanks
053800161215     C                   MOVE      wNOTE_9       E_VHBNT2
053900161215     C                   ENDIF
054000161215     C*
054100161212     C                   MOVE      VABZNC        E_VHBZNC
054200161212     C                   MOVE      VABCTM        E_VHBCTM
054300161212     C                   MOVE      VABVAD        E_VHBVAD
054400161212     C                   MOVE      VABGMA        E_VHBGMA
054500161212     C                   MOVE      VABGGA        E_VHBGGA
054600161212     C                   MOVE      VABGVA        E_VHBGVA
054700161212     C                   MOVE      VABSCL        E_VHBSCL
054800161212     C                   MOVE      VABANT        E_VHBANT
054900161212     C                   MOVE      VABCMO        E_VHBCMO
055000161212     C                   MOVE      VABNMO        E_VHBNMO
055100161212     C*
055200161212     C                   ENDSR
055300161212
055400161212
055500161212
055600161212     C*
055700161212     C     fmtVAC_ASCI   BEGSR
055800161212     C*
055900161212     C                   MOVE      VABAAS        A_VHCAAS
056000161212     C                   MOVE      VABLNP        A_VHCLNP
056100161212     C                   MOVE      VABNRS        A_VHCNRS
056200161212     C                   MOVE      VABNSP        A_VHCNSP
056300161212     C                   MOVE      VABMGS        A_VHCMGS
056400161212     C                   MOVE      VABCBO        A_VHCCBO
056500161212     C                   MOVE      VABLNA        A_VHCLNA
056600161212     C                   MOVE      VABRSD        A_VHCRSD
056700161212     C                   MOVE      VABPRD        A_VHCPRD
056800161212     C                   MOVE      VABGC1        A_VHCGC1
056900161212     C                   MOVE      VABGC2        A_VHCGC2
057000161212     C                   MOVE      VABCTR        A_VHCCTR
057100161212     C                   MOVE      VABCTS        A_VHCCTS
057200161212     C                   MOVE      VABFTM        A_VHCFTM
057300161212     C                   MOVE      VABTSP        A_VHCTSP
057400161212     C*
057500161212     C                   MOVE      VABIAS        WD15
057600161212     C                   MOVEL     VABIAS        WI15
057700161212     C                   MOVE      WASC15        A_VHCIAS
057800161212     C*
057900161212     C                   MOVE      VABVAS        A_VHCVAS
058000161212     C                   MOVE      VABNAS        A_VHCNAS
058100161212     C                   MOVE      VABNCL        A_VHCNCL
058200161212     C*
058300161212     C                   MOVE      VABPKB        WD09
058400161212     C                   MOVEL     VABPKB        WI09
058500161212     C                   MOVE      WASC09        A_VHCPKB
058600161212     C*
058700161212     C                   MOVE      VABVLB        WD07
058800161212     C                   MOVEL     VABVLB        WI07
058900161212     C                   MOVE      WASC07        A_VHCVLB
059000161212     C*
059100161212     C                   MOVE      VABQFT        WD15
059200161212     C                   MOVEL     VABQFT        WI15
059300161212     C                   MOVE      WASC15        A_VHCQFT
059400161212     C*
059500161212     C                   MOVE      VABTIC        A_VHCTIC
059600161212     C*
059700161212     C                   MOVE      VABCAS        WD15
059800161212     C                   MOVEL     VABCAS        WI15
059900161212     C                   MOVE      WASC15        A_VHCCAS
060000161212     C*
060100161212     C                   MOVE      VABVCA        A_VHCVCA
060200161212     C                   MOVE      VABCCM        A_VHCCCM
060300161212     C                   MOVE      VABRMN        A_VHCRMN
060400161212     C                   MOVE      VABRMA        A_VHCRMA
060500161212     C                   MOVE      VABRMO        A_VHCRMO
060600161212     C                   MOVE      VABFFD        A_VHCFFD
060700161212     C                   MOVE      VABTCR        A_VHCTCR
060800161212     C                   MOVE      VABDCR        A_VHCDCR
060900161212     C                   MOVE      VABHCR        A_VHCHCR
061000161212     C                   MOVE      VABTC1        A_VHCTC1
061100161212     C                   MOVE      VABTC2        A_VHCTC2
061200161212     C*
061300161212     C* Forzatura valore CCA x stato "IN TRANSITO"
061400161212     C                   MOVEL     '*'           A_VHCCCA
061500161212     C*
061600161212     C                   MOVE      '00000000'    A_VHCDCM
061700161212     C                   MOVE      '0000'        A_VHCHMC
061800161212     C                   MOVE      '00000000'    A_VHCDLA
061900161212     C                   MOVE      '00000000'    A_VHCDAG
062000161212     C*
062100161212     C                   ENDSR
062200010131
062300010131
062400010131
062500010131     C*
062600010131     C     repta4        BEGSR
062700010131     C*
062800010131     C* Inizializzo i campi "fissi"
062900161215     C                   movel     'J'           ar4trc
063000161215     C*
063100010131     C* Posizionamento diretto
063200161215     C     KEYta430_C    chain     tita430c
063300010131     C                   if        %found(tita430c)
063400010131     C                   clear                   DSBL4J
063500010131     C                   movel     ta4not        DSBL4J
063600010131     C                   if        §B4BAI = 'S'
063700161212     C                   movel     'S'           A_VHBFTM
063800161212     C                   movel     'S'           E_VHBFTM
063900161212     C                   movel     'S'           A_VHCFTM
064000010131     C                   endif
064100010131     C                   endif
064200010131     C*
064300010131     C                   ENDSR
064400080527
064500080527
064600080527
064700080527     C*
064800080527     C     reptas        BEGSR
064900080527     C*
065000080527     C* Posizionamento diretto
065100080527     C     KEYtas30_P    chain     titas30c
065200080527     C                   if        %found(titas30c)
065300161212     C                   move      tasDCR        A_VHBDCR
065400161212     C                   move      tasDCR        E_VHBDCR
065500161212     C                   move      tasDCR        A_VHCDCR
065600080527     C                   endif
065700080527     C*
065800080527     C                   ENDSR
065900161215
066000161215
066100161215
066200161215     C     repNOT        BEGSR
066300161215     C*
066400161215     C                   movel     *blanks       wNOTE_8          35
066500161215     C                   movel     *blanks       wNOTE_9          35
066600161215     C*
066700161215     C                   eval      ar4trc = '8'
066800161215     C     KEYta430_C    chain     fiar401l
066900161215     C                   if        %found(fiar401l)
067000161215     C                   eval      wNOTE_8 = ar4NOT
067100161215     C                   else
067200161215     C                   eval      ar4trc = '8'
067300161215     C     KEYta430_C    chain     tita430c
067400161215     C                   if        %found(tita430c)
067500161215     C                   eval      wNOTE_8 = ta4NOT
067600161215     C                   endif
067700161215     C                   endif
067800161215     C*
067900161215     C                   eval      ar4trc = '9'
068000161215     C     KEYta430_C    chain     fiar401l
068100161215     C                   if        %found(fiar401l)
068200161215     C                   eval      wNOTE_9 = ar4NOT
068300161215     C                   else
068400161215     C                   eval      ar4trc = '9'
068500161215     C     KEYta430_C    chain     tita430c
068600161215     C                   if        %found(tita430c)
068700161215     C                   eval      wNOTE_9 = ta4NOT
068800161215     C                   endif
068900161215     C                   endif
069000161215     C*
069100161215     C                   ENDSR
069200161212
069300161212
069400161212
069500161212     C     CARTAB        BEGSR
069600161212     C*
069700161212     C     'VPC'         SETLL     tntbe01l
069800161212     C                   IF        %equal(tntbe01l)
069900161212     C     'VPC'         READE     tntbe01l
070000161212     C                   DOW       not %eof(tntbe01l)
070100161212     C                   EVAL      DVPC = tbeUNI
070200161212     C                   IF        §VPCVTA = 'S'
070300161212     C                   ADD       1             jVPC
070400161212     C                   EVAL      skVPC(jVPC) = %dec(%subst(tbeKE1:1:7):7:0)
070500161212     C                   ENDIF
070600161212     C     'VPC'         READE     tntbe01l
070700161212     C                   ENDDO
070800161212     C                   ENDIF
070900161212     C*
071000161212     C                   ENDSR
071100010528
071200000629
071300991027
071400991027      /TITLE Operazioni iniziali.
071500991027     C     *inzsr        BEGSR
071600991027     C*
071700991027     C     *ENTRY        PLIST
071800991027     C                   parm                    prmppt
071900991027     C     wrkesito      parm      wrkesito      prmesito
072000080527     C*
072100080527     C     KEYtas30_P    KLIST
072200080527     C                   KFLD                    vabaas
072300080527     C                   KFLD                    vablnp
072400080527     C                   KFLD                    vabnrs
072500080527     C                   KFLD                    vabnsp
072600121218     C*
072700121218     C     KEYblt01_P    KLIST
072800121218     C                   KFLD                    vabaas
072900121218     C                   KFLD                    vablnp
073000121218     C                   KFLD                    vabnrs
073100121218     C                   KFLD                    vabnsp
073200121218     C*
073300121218     C     KEYars01_C    KLIST
073400121218     C                   KFLD                    bltFLS
073500121218     C                   KFLD                    bltLNA
073600121218     C                   KFLD                    bltNRS
073700121218     C                   KFLD                    bltNSC
073800121218     C                   KFLD                    arsTRC
073900161215     C*
074000161215     C     KEYta430_C    KLIST
074100161215     C                   KFLD                    vabaas
074200161215     C                   KFLD                    vablnp
074300161215     C                   KFLD                    vabnrs
074400161215     C                   KFLD                    vabnsp
074500161215     C                   KFLD                    ar4trc
074600991027     C*
074700991027     C                   ENDSR
074800161215
