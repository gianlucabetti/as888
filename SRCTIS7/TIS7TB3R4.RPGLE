000100161212     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200121218     H DECEDIT('0,') DATEDIT(*DMY.)
000300000629
000400180118     FFNVAB00T  if   E             DISK
000500180118     FTIVAWWWT  O    E             DISK
000600161212     Ftntbe01l  if   E           k DISK
000700161215     Ffiar401l  if   E           k DISK    usropn
000800161215     F                                     extfile(LibFileAR401)
000900180118     Ftita430c  if   E           k DISK
001000080527     Ftitas30c  if   E           k DISK
001100121218     Ffnblt01l  if   E           k DISK    usropn
001200121218     F                                     extfile(LibFileBLT01)
001300121218     Ffiars01l  if   E           k DISK    usropn
001400121218     F                                     extfile(LibFileARS01)
001500180118     Ffnblp01l  if   E           k DISK    usropn
001600180118     F                                     extfile(LibFileBLP01)
001700171123     Ffipnd01l  if   E           k DISK
001800991027
001900991027     D prmppt          s             50
002000991027     D prmesito        s              1
002100161213     D wrkesito        s                   like(prmesito)
002200161212     D DVPC          e ds                  inz
002300161213 xxx D*** DVGD001       e ds                  inz
002400180118     D**** fnvabDS       e ds                  extname(fnvab00t)
002500161212     D fnvabAsciDS   e ds                  extname(fnvabwwt) prefix(A_)
002600161212     D fnvabEbcdDS   e ds                  extname(fnvab1wt) prefix(E_)
002700161212     D fnvacAsciDS   e ds                  extname(fnvacwwt) prefix(A_)
002800121218
002900161212     D wARSNOT_C       s                   like(ARSNOT) inz
003000171123     D wpndipn         s             15    inz
003100161212     D wVAWDTA         s                   like(VAWDTA) inz
003200161212     D l_VB_A          s              4  0 inz(%size(fnvabAsciDS))
003300161212     D l_VB_E          s              4  0 inz(%size(fnvabEbcdDS))
003400161212     D l_VC_A          s              4  0 inz(%size(fnvacAsciDS))
003500161213     D iVAB            s               N   inz
003600161213     D iVAC            s               N   inz
003700161213     D iASCI           s               N   inz
003800161213     D iEBCD           s               N   inz
003900161215     D iS7VB           s               N   inz
004000171123     D ipnd            s               N   inz
004100121218
004200121218     D LibFileBLT01    s             21A   inz
004300121218     D LibFileARS01    s             21A   inz
004400161215     D LibFileAR401    s             21A   inz
004500180118     D LibFileBLP01    s             21A   inz
004600161212     D currSysNeta     s              8A   inz('*NULL')
004700161212
004800161212
004900161212     D*-----------
005000161212     D* SCHIERE
005100161212     D*-----------
005200161212     D skVPC           S              7  0 DIM(500)
005300161212     D jVPC            S              3  0 INZ(*zeros)
005400121218
005500161212
005600000629     D*----------
005700000629     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
005800161212     D*----------
005900000629     D* 10+3
006000000629     D                 DS
006100000629     D  WS15                   1      1
006200000629     D  WI15                   2     11
006300000629     D  WV15                  12     12
006400000629     D  WD15                  13     15
006500000629     D  WASC15                 1     15
006600000629     D*  6+1
006700000629     D                 DS
006800000629     D  WS09                   1      1
006900000629     D  WI09                   2      7
007000000629     D  WV09                   8      8
007100000629     D  WD09                   9      9
007200000629     D  WASC09                 1      9
007300000629     D*  2+3
007400000629     D                 DS
007500000629     D  WS07                   1      1
007600000629     D  WI07                   2      3
007700000629     D  WV07                   4      4
007800000629     D  WD07                   5      7
007900000629     D  WASC07                 1      7
008000121218
008100161212
008200010131     D*-------------------
008300010131     D* DS ESTENSIONE BOLLE TIPO RECORD "J" (X BOLE INGOMBRANTI NON DICHIARATE)
008400010131     D*-------------------
008500010131     D DSBL4J        E DS
008600991027
008700121218
008800121218     D*------------------
008900121218     D* LINKING A DEFINIZIONI ESTERNE
009000121218     D*------------------
009100121218     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009200121218     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009300161212     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
009400161212     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
009500121218
009600161212
009700010131
009800010131
009900121218     C*
010000121218     C* Reperisco il sistema AS/400 corrente
010100121218     C                   callp     UBRTVNETA_Rtv(currSysNeta)
010200121218     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
010300121218     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
010400121218     C                   eval      LibFileARS01 = 'FILTRA201/FIARS01L'
010500161215     C                   eval      LibFileAR401 = 'FILTRA201/FIAR401L'
010600180118     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
010700121218     C                   else
010800121218     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
010900121218     C                   eval      LibFileARS01 = 'FILTRAPRD/FIARS01L'
011000161215     C                   eval      LibFileAR401 = 'FILTRAPRD/FIAR401L'
011100180118     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
011200121218     C                   endif
011300121218     C*
011400121218     C                   if        not %open(FNBLT01L)
011500121218     C                   open      FNBLT01L
011600121218     C                   endif
011700121218     C*
011800121218     C                   if        not %open(FIARS01L)
011900121218     C                   open      FIARS01L
012000121218     C                   endif
012100161215     C*
012200161215     C                   if        not %open(FIAR401L)
012300161215     C                   open      FIAR401L
012400161215     C                   endif
012500180118     C                   if        not %open(Fnblp01L)
012600180118     C                   open      Fnblp01L
012700180118     C                   endif
012800170118     C*
012900170118     C* Avvio il monitoring del intero flusso
013000170118     C                   monitor
013100170118     C*
013200170118     C* Effettuo traduzione
013300991027     C                   exsr      traduci
013400170118     C*
013500170118     C* Gestisco eventuale errore
013600170118     C                   on-error
013700170118     C*
013800170118     C* Imposto esito transazione a errore
013900170118     C                   EVAL      wrkesito = '2'
014000170118     C*
014100170118     C* Se errore => emetto operazione d ROLLBACK
014200170118     C                   ROLBK
014300170118     C*
014400170118     C* Arresto il monitoring
014500170118     C                   endmon
014600121218     C*
014700121218     C                   if        %open(FNBLT01L)
014800121218     C                   close     FNBLT01L
014900121218     C                   endif
015000121218     C*
015100121218     C                   if        %open(FIARS01L)
015200121218     C                   close     FIARS01L
015300121218     C                   endif
015400161215     C*
015500161215     C                   if        %open(FIAR401L)
015600161215     C                   close     FIAR401L
015700161215     C                   endif
015800121218     C*
015900991027     C                   seton                                        lr
016000000629
016100121218
016200000629
016300161212     C     traduci       BEGSR
016400161212     C*
016500161212     C* Carico tabelle di procedura
016600161212     C                   exsr      cartab
016700000629     C*
016800161212     C* Inizializzazione indicatori di procedure per parametri input
016900161213     C                   eval      *in45 = *off
017000161213     C                   eval      *in46 = *off
017100161213     C                   eval      *in47 = *off
017200161213     C                   eval      *in49 = *off
017300161213     C                   eval      iVAB  = *off
017400161213     C                   eval      iVAC  = *off
017500161213     C                   eval      iASCI = *off
017600161213     C                   eval      iEBCD = *off
017700161215     C                   eval      iS7VB = *off
017800171123     C                   eval      ipnd  = *off
017900161212     C*
018000161212     C* Se richiesto valorizzo da data consegna richiesta con il proprio valore
018100180118     C**                 if        %subst(prmppt:5:1) = 'R'
018200161212     C                   seton                                        45
018300180118     C**                 endif
018400161212     C*
018500161212     C* Se richiesto ritorno solamente le bolle originali
018600161212     C                   if        %subst(prmppt:6:1) = 'O'
018700161212     C                   seton                                        46
018800161212     C                   endif
018900161212     C*
019000161213     C* Se richiesto ritorno solamente le bolle con serie
019100161212     C                   if        %subst(prmppt:7:1) = 'S'
019200161213     C                   seton                                        47
019300161212     C                   endif
019400161212     C*
019500161212     C* Se richiesto ritorno anche i dati del segnacollo cliente
019600161212     C                   if        %subst(prmppt:9:1) = 'S'
019700161212     C                   seton                                        49
019800161212     C                   endif
019900161212     C*
020000161212     C* Se richiesto ritorno dati in formato EBCDIC
020100161212     C                   if        %subst(prmppt:10:1) = 'S'
020200161213     C                   eval      iEBCD = *on
020300161213     C                   else
020400161213     C                   eval      iASCI = *on
020500161212     C                   endif
020600161212     C*
020700161212     C* Se richiesto ritorno dati in formato "VAC"
020800161212     C                   if        %subst(prmppt:11:1) = 'S'
020900161213     C                   eval      iVAC = *on
021000161213     C                   else
021100161213     C                   eval      iVAB = *on
021200161213     C                   endif
021300161215     C*
021400161215     C* Se richiesto filtro solo dati da *pgm TIS7VBR
021500161215     C                   if        %subst(prmppt:12:1) = 'S'
021600161215     C                   eval      iS7VB = *on
021700161215     C                   endif
021800171123     C* Se richiesto passo anche parcel DPD
021900171123     C                   if        %subst(prmppt:15:1) = 'S'
022000171123     C                   eval      ipnd  = *on
022100171123     C                   endif
022200161212     C*
022300161213     C* Scorro tutto il file
022400180118     C                   read      FNVAB00T
022500180118     C                   dow       not %eof(FNVAB00T)
022600161212     C*
022700161212     C* Inizializzo DS di output
022800170119     C                   clear                   TIVAW000
022900161212     C                   clear                   fnvabAsciDS
023000161212     C                   clear                   fnvabEbcdDS
023100161212     C                   clear                   fnvacAsciDS
023200161212     C*
023300161212     C* Valorizzo dati "corrrenti"
023400161212     C                   exsr      defcam
023500170111     C*
023600170111     C* Eseguo considerazioni su pacchi ingombranti aggiunti da Bartolini
023700180118     c* cose delle poste che non ci sono più
023800180118     C****               exsr      repTA4
023900170111     C*
024000170111     C* Reperimento note 8 e 9
024100170111     C                   exsr      repNOT
024200170111     C*
024300170111     C* Reperimento dati bolla da Sede
024400170111     C   45              exsr      repTAS
024500171123
024600171123     C* Reperimento parcel dpd se presente
024700171123     c                   if        ipnd=*on
024800171123     C                   exsr      repPND
024900171123     c                   endif
025000161213     C*
025100161213     C* Gestisco formato richiesto VAB/VAC e ASCII/EBCDIC
025200161213     C                   select
025300161213     C                   when      iVAB and iASCI
025400161213     C                   exsr      fmtVAB_ASCI
025500161213     C                   when      iVAB and iEBCD
025600161213     C                   exsr      fmtVAB_EBCD
025700161213     C                   when      iVAC and iASCI
025800161213     C                   exsr      fmtVAC_ASCI
025900161213     C                   other
026000161213     C                   eval      wrkesito = '2'
026100161213     C                   endsl
026200161213     C*
026300161213     C* Se tutto ok => procedo
026400161213     C                   if        wrkesito = *blanks
026500080613     C*
026600161212     C* Inizializzo indicatore di record corrente OK
026700080613     C                   setoff                                       31
026800161212     C*
026900161212     C* Chiamata metodo GetLblTyp
027000161212     C   46              if        UBLBLSPE_GetLblTyp(vabAAS
027100161212     C                                               :vabLNP
027200161212     C                                               :vabNRS
027300161212     C                                               :vabNSP
027400161212     C                                               :pOutLblTyp
027500161212     C                                               :pOutAnnoBO
027600161212     C                                               :pOutLineaParBO
027700161212     C                                               :pOutSerieBO
027800161212     C                                               :pOutNumSpedBO
027900161212     C                                               :pOutDcmBO
028000161212     C                                               :pOutCcaBO
028100161212     C                                               :pOutRblBO) = 0
028200161212     C                   if        pOutLblTyp <> 'O'
028300161212     C                   seton                                        31
028400161212     C                   endif
028500161212     C                   endif
028600161212     C*
028700161212     C* Solo bolle con serie
028800161212     C   47              if        vabNRS = *zeros
028900161212     C                   seton                                        31
029000161212     C                   endif
029100161215     C*
029200161215     C* Solo record generati da *pgm TIS7VBR
029300080613     C*
029400161213     C* Se record ok => procedo
029500080613     C                   if        *in31 = *off
029600121218     C*
029700161212     C* Scarico il buffer di output formato richiesto VAB/VAC e ASCII/EBCDIC
029800161213     C                   select
029900161213     C                   when      iVAB and iASCI
030000161213     C                   eval      VAWDTA = fnvabAsciDS
030100161213     C                   when      iVAB and iEBCD
030200161213     C                   eval      VAWDTA = fnvabEbcdDS
030300161213     C                   when      iVAC and iASCI
030400161213     C                   eval      VAWDTA = fnvacAsciDS
030500161213     C                   endsl
030600171123
030700171123     c*  campo di comodo per comporre i dati in uscita
030800171123     C                   eval      wVAWDTA = VAWDTA
030900121218     C*
031000121218     C* Se richiesto nei parametri d ingresso ritorno anche i dati del segnacollo cliente
031100161215     C                   if        *in49
031200121218     C*
031300121218     C* Aggancio i dettagli colli e ciclo x ciascuno
031400121218     C     KEYblt01_P    setll     fnblt01l
031500121218     C                   if        %found(fnblt01l)
031600121218     C     KEYblt01_P    reade     fnblt01l
031700121218     C                   dow       not %eof(fnblt01l)
031800121218     C*
031900121218     C                   clear                   wARSNOT_C
032000121218     C*
032100121218     C* Reperisco il segnacollo cliente ("chi sono")
032200121218     C                   eval      arsTRC = 'C'
032300121218     C     KEYars01_C    chain     fiars01l
032400121218     C                   if        %found(fiars01l)
032500121218     C                   eval      wARSNOT_C = arsNOT
032600121218     C                   endif
032700161213     C*
032800171123     c                   exsr      riempiREC
032900171123     c
033000161212     C                   write     TIVAW000
033100121218     C*
033200121218     C* Scorro il segnacollo successivo
033300121218     C     KEYtas30_P    reade     fnblt01l
033400121218     C                   enddo
033500121218     C*
033600161212     C* Se non trovo il dettaglio segnacolli BRT in filiale scrivo almeno un record
033700121218     C                   else
033800171123     C*
033900171123     c                   if        ipnd=*on
034000171123     c                   exsr      riempirec
034100171123     c                   endif
034200171123
034300161212     C                   write     TIVAW000
034400161212     C                   endif
034500121218     C*
034600121218     C                   else
034700171123     c                   if        ipnd=*on
034800171123     c                   exsr      riempirec
034900171123     c                   endif
035000171123
035100161212     C                   write     TIVAW000
035200121218     C                   endif
035300000629     C*
035400161212     C* Se partcolarità abilitata in tabella 'VPC' => eseguo
035500161212     C     vabCCM        lookup    skVPC                                  05
035600161212     C                   if        %found
035700161212     C                   call      'TIS7T99R'
035800161212     C                   parm      vabCCM        I99CCM            7 0
035900161212     C                   parm      vabAAS        I99AAS            4 0
036000161212     C                   parm      vabLNP        I99LNP            3 0
036100161212     C                   parm      vabNRS        I99NRS            2 0
036200161212     C                   parm      vabNSP        I99NSP            7 0
036300161212     C                   endif
036400161212     C*
036500161212     C                   endif
036600161213     C*
036700161213     C                   endif
036800161213     C*
036900161213 xxx C* Aggiorno lo stato sul TIVGD00F
037000161213 xxx C***                eval      DVGD001 = vgdATTRIB
037100161213 xxx C***                if        §VGD001RRN > *zeros
037200161213 xxx C***  §VGD001RRN    chain(e)  tivgd00f
037300161213 xxx C***                if        %found(tivgd00f)
037400161213 xxx C***                eval      F_vgdSTO = 'S'
037500161213 xxx C***                update(e) F_tivgd
037600161213 xxx C***                endif
037700161213 xxx C***                endif
037800161212     C*
037900161212     C* Proseguo con il record successivo
038000180118     C                   read      FNVAB00T
038100161212     C                   enddo
038200000629     C*
038300161212     C                   if        wrkesito <> '2'
038400161212     C                   eval      wrkesito  = '0'
038500161212     C                   endif
038600000629     C*
038700000629     C                   ENDSR
038800171123     c
038900171123     c
039000171123     c     Riempirec     BEGSR
039100171123     C                   select
039200171123     C                   when      iVAB and iASCI
039300171123     c                   if        ipnd=*on
039400171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_A)+wARSNOT_C+
039500171123     c                             wpndipn
039600171123     c                   else
039700171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_A)+wARSNOT_C+'.'
039800171123     c                   endif
039900171123
040000171123     C                   when      iVAB and iEBCD
040100171123     c                   if        ipnd=*on
040200171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_E)+wARSNOT_C+
040300171123     c                             wpndipn
040400171123     c                   else
040500171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_E)+wARSNOT_C+'.'
040600171123     c                   endif
040700171123     c
040800171123     C                   when      iVAC and iASCI
040900171123     c                   if        ipnd=*on
041000171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VC_A)+wARSNOT_C+
041100171123     c                             wpndipn
041200171123     c                   else
041300171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VC_A)+wARSNOT_C+'.'
041400171123     c                   endif
041500171123     c
041600171123     C                   endsl
041700171123     c                   ENDSR
041800161212
041900161212
042000161212
042100161212     C     defcam        BEGSR
042200161212     C*
042300161212     C                   MOVE      *BLANK        WS15
042400161212     C                   MOVE      ','           WV15
042500161212     C                   MOVE      *BLANK        WS07
042600161212     C                   MOVE      ','           WV07
042700161212     C                   MOVE      *BLANK        WS09
042800161212     C                   MOVE      ','           WV09
042900161212     C*
043000161212     C                   ENDSR
043100161212
043200161212
043300161212
043400161212     C*
043500161212     C     fmtVAB_ASCI   BEGSR
043600161212     C*
043700161212     C                   MOVE      VABAAS        A_VHBAAS
043800161212     C                   MOVE      VABLNP        A_VHBLNP
043900161212     C                   MOVE      VABNRS        A_VHBNRS
044000161212     C                   MOVE      VABNSP        A_VHBNSP
044100161212     C                   MOVE      VABMGS        A_VHBMGS
044200161212     C                   MOVE      VABCBO        A_VHBCBO
044300161212     C                   MOVE      VABLNA        A_VHBLNA
044400161212     C                   MOVE      VABRSD        A_VHBRSD
044500161212     C                   MOVE      VABPRD        A_VHBPRD
044600161212     C                   MOVE      VABGC1        A_VHBGC1
044700161212     C                   MOVE      VABGC2        A_VHBGC2
044800161212     C                   MOVE      VABCTR        A_VHBCTR
044900161212     C                   MOVE      VABCTS        A_VHBCTS
045000161212     C                   MOVE      VABFTM        A_VHBFTM
045100161212     C                   MOVE      VABTSP        A_VHBTSP
045200161212     C*
045300161212     C                   MOVE      VABIAS        WD15
045400161212     C                   MOVEL     VABIAS        WI15
045500161212     C                   MOVE      WASC15        A_VHBIAS
045600161212     C*
045700161212     C                   MOVE      VABVAS        A_VHBVAS
045800161212     C                   MOVE      VABNAS        A_VHBNAS
045900161212     C                   MOVE      VABNCL        A_VHBNCL
046000161212     C*
046100161212     C                   MOVE      VABPKB        WD09
046200161212     C                   MOVEL     VABPKB        WI09
046300161212     C                   MOVE      WASC09        A_VHBPKB
046400161212     C*
046500161212     C                   MOVE      VABVLB        WD07
046600161212     C                   MOVEL     VABVLB        WI07
046700161212     C                   MOVE      WASC07        A_VHBVLB
046800161212     C*
046900161212     C                   MOVE      VABQFT        WD15
047000161212     C                   MOVEL     VABQFT        WI15
047100161212     C                   MOVE      WASC15        A_VHBQFT
047200161212     C*
047300161212     C                   MOVE      VABVMD        WD15
047400161212     C                   MOVEL     VABVMD        WI15
047500161212     C                   MOVE      WASC15        A_VHBVMD
047600161212     C*
047700161212     C                   MOVE      VABTIC        A_VHBTIC
047800161212     C*
047900161212     C                   MOVE      VABCAS        WD15
048000161212     C                   MOVEL     VABCAS        WI15
048100161212     C                   MOVE      WASC15        A_VHBCAS
048200161212     C*
048300161212     C                   MOVE      VABVCA        A_VHBVCA
048400161212     C                   MOVE      VABCCM        A_VHBCCM
048500161212     C                   MOVE      VABRMN        A_VHBRMN
048600161212     C                   MOVE      VABRMA        A_VHBRMA
048700161212     C                   MOVE      VABRMO        A_VHBRMO
048800161212     C                   MOVE      VABFFD        A_VHBFFD
048900161212     C                   MOVE      VABTCR        A_VHBTCR
049000161212     C                   MOVE      VABDCR        A_VHBDCR
049100161212     C                   MOVE      VABHCR        A_VHBHCR
049200161212     C                   MOVE      VABTC1        A_VHBTC1
049300161212     C                   MOVE      VABTC2        A_VHBTC2
049400161212     C                   MOVE      VABATB        A_VHBATB
049500161212     C                   MOVE      VABRD2        A_VHBRD2
049600161212     C                   MOVE      VABIND        A_VHBIND
049700161212     C                   MOVE      VABCAD        A_VHBCAD
049800161212     C                   MOVE      VABLOD        A_VHBLOD
049900161212     C                   MOVE      VABNZD        A_VHBNZD
050000161212     C                   MOVE      VABGCA        A_VHBGCA
050100161212     C                   MOVE      VABNCD        A_VHBNCD
050200161212     C                   MOVE      VABNCA        A_VHBNCA
050300161212     C                   MOVE      VABXCO        A_VHBXCO
050400161212     C                   MOVE      VABNOT        A_VHBNOT
050500161212     C                   MOVE      VABNT2        A_VHBNT2
050600161215     C*
050700161215     C                   IF        wNOTE_8 <> *blanks
050800161215     C                   MOVE      wNOTE_8       A_VHBNOT
050900161215     C                   ENDIF
051000161215     C                   IF        wNOTE_9 <> *blanks
051100161215     C                   MOVE      wNOTE_9       A_VHBNT2
051200161215     C                   ENDIF
051300161215     C*
051400161212     C                   MOVE      VABZNC        A_VHBZNC
051500161212     C                   MOVE      VABCTM        A_VHBCTM
051600161212     C                   MOVE      VABVAD        A_VHBVAD
051700161212     C                   MOVE      VABGMA        A_VHBGMA
051800161212     C                   MOVE      VABGGA        A_VHBGGA
051900161212     C                   MOVE      VABGVA        A_VHBGVA
052000161212     C                   MOVE      VABSCL        A_VHBSCL
052100161212     C                   MOVE      VABANT        A_VHBANT
052200161212     C                   MOVE      VABCMO        A_VHBCMO
052300161212     C                   MOVE      VABNMO        A_VHBNMO
052400161212     C*
052500161212     C                   ENDSR
052600161212
052700161212
052800161212
052900161212     C*
053000161212     C     fmtVAB_EBCD   BEGSR
053100161212     C*
053200161212     C                   MOVE      VABAAS        E_VHBAAS
053300161212     C                   MOVE      VABLNP        E_VHBLNP
053400161212     C                   MOVE      VABNRS        E_VHBNRS
053500161212     C                   MOVE      VABNSP        E_VHBNSP
053600161212     C                   MOVE      VABMGS        E_VHBMGS
053700161212     C                   MOVE      VABCBO        E_VHBCBO
053800161212     C                   MOVE      VABLNA        E_VHBLNA
053900161212     C                   MOVE      VABRSD        E_VHBRSD
054000161212     C                   MOVE      VABPRD        E_VHBPRD
054100161212     C                   MOVE      VABGC1        E_VHBGC1
054200161212     C                   MOVE      VABGC2        E_VHBGC2
054300161212     C                   MOVE      VABCTR        E_VHBCTR
054400161212     C                   MOVE      VABCTS        E_VHBCTS
054500161212     C                   MOVE      VABFTM        E_VHBFTM
054600161212     C                   MOVE      VABTSP        E_VHBTSP
054700161212     C*
054800161212     C                   MOVE      VABIAS        E_VHBIAS
054900161212     C                   MOVE      VABVAS        E_VHBVAS
055000161212     C                   MOVE      VABNAS        E_VHBNAS
055100161212     C                   MOVE      VABNCL        E_VHBNCL
055200161212     C                   MOVE      VABPKB        E_VHBPKB
055300161212     C                   MOVE      VABVLB        E_VHBVLB
055400161212     C                   MOVE      VABQFT        E_VHBQFT
055500161212     C                   MOVE      VABVMD        E_VHBVMD
055600161212     C                   MOVE      VABTIC        E_VHBTIC
055700161212     C                   MOVE      VABCAS        E_VHBCAS
055800161212     C                   MOVE      VABVCA        E_VHBVCA
055900161212     C                   MOVE      VABCCM        E_VHBCCM
056000161212     C                   MOVE      VABRMN        E_VHBRMN
056100161212     C                   MOVE      VABRMA        E_VHBRMA
056200161212     C                   MOVE      VABRMO        E_VHBRMO
056300161212     C                   MOVE      VABFFD        E_VHBFFD
056400161212     C                   MOVE      VABTCR        E_VHBTCR
056500161212     C                   MOVE      VABDCR        E_VHBDCR
056600161212     C                   MOVE      VABHCR        E_VHBHCR
056700161212     C                   MOVE      VABTC1        E_VHBTC1
056800161212     C                   MOVE      VABTC2        E_VHBTC2
056900161212     C                   MOVE      VABATB        E_VHBATB
057000161212     C                   MOVE      VABRD2        E_VHBRD2
057100161212     C                   MOVE      VABIND        E_VHBIND
057200161212     C                   MOVE      VABCAD        E_VHBCAD
057300161212     C                   MOVE      VABLOD        E_VHBLOD
057400161212     C                   MOVE      VABNZD        E_VHBNZD
057500161212     C                   MOVE      VABGCA        E_VHBGCA
057600161212     C                   MOVE      VABNCD        E_VHBNCD
057700161212     C                   MOVE      VABNCA        E_VHBNCA
057800161212     C                   MOVE      VABXCO        E_VHBXCO
057900161212     C                   MOVE      VABNOT        E_VHBNOT
058000161215     C                   MOVE      VABNT2        E_VHBNT2
058100161215     C*
058200161215     C                   IF        wNOTE_8 <> *blanks
058300161215     C                   MOVE      wNOTE_8       E_VHBNOT
058400161215     C                   ENDIF
058500161215     C                   IF        wNOTE_9 <> *blanks
058600161215     C                   MOVE      wNOTE_9       E_VHBNT2
058700161215     C                   ENDIF
058800161215     C*
058900161212     C                   MOVE      VABZNC        E_VHBZNC
059000161212     C                   MOVE      VABCTM        E_VHBCTM
059100161212     C                   MOVE      VABVAD        E_VHBVAD
059200161212     C                   MOVE      VABGMA        E_VHBGMA
059300161212     C                   MOVE      VABGGA        E_VHBGGA
059400161212     C                   MOVE      VABGVA        E_VHBGVA
059500161212     C                   MOVE      VABSCL        E_VHBSCL
059600161212     C                   MOVE      VABANT        E_VHBANT
059700161212     C                   MOVE      VABCMO        E_VHBCMO
059800161212     C                   MOVE      VABNMO        E_VHBNMO
059900161212     C*
060000161212     C                   ENDSR
060100161212
060200161212
060300161212
060400161212     C*
060500161212     C     fmtVAC_ASCI   BEGSR
060600161212     C*
060700161212     C                   MOVE      VABAAS        A_VHCAAS
060800161212     C                   MOVE      VABLNP        A_VHCLNP
060900161212     C                   MOVE      VABNRS        A_VHCNRS
061000161212     C                   MOVE      VABNSP        A_VHCNSP
061100161212     C                   MOVE      VABMGS        A_VHCMGS
061200161212     C                   MOVE      VABCBO        A_VHCCBO
061300161212     C                   MOVE      VABLNA        A_VHCLNA
061400161212     C                   MOVE      VABRSD        A_VHCRSD
061500161212     C                   MOVE      VABPRD        A_VHCPRD
061600161212     C                   MOVE      VABGC1        A_VHCGC1
061700161212     C                   MOVE      VABGC2        A_VHCGC2
061800161212     C                   MOVE      VABCTR        A_VHCCTR
061900161212     C                   MOVE      VABCTS        A_VHCCTS
062000161212     C                   MOVE      VABFTM        A_VHCFTM
062100161212     C                   MOVE      VABTSP        A_VHCTSP
062200161212     C*
062300161212     C                   MOVE      VABIAS        WD15
062400161212     C                   MOVEL     VABIAS        WI15
062500161212     C                   MOVE      WASC15        A_VHCIAS
062600161212     C*
062700161212     C                   MOVE      VABVAS        A_VHCVAS
062800161212     C                   MOVE      VABNAS        A_VHCNAS
062900161212     C                   MOVE      VABNCL        A_VHCNCL
063000161212     C*
063100161212     C                   MOVE      VABPKB        WD09
063200161212     C                   MOVEL     VABPKB        WI09
063300161212     C                   MOVE      WASC09        A_VHCPKB
063400161212     C*
063500161212     C                   MOVE      VABVLB        WD07
063600161212     C                   MOVEL     VABVLB        WI07
063700161212     C                   MOVE      WASC07        A_VHCVLB
063800161212     C*
063900161212     C                   MOVE      VABQFT        WD15
064000161212     C                   MOVEL     VABQFT        WI15
064100161212     C                   MOVE      WASC15        A_VHCQFT
064200161212     C*
064300161212     C                   MOVE      VABTIC        A_VHCTIC
064400161212     C*
064500161212     C                   MOVE      VABCAS        WD15
064600161212     C                   MOVEL     VABCAS        WI15
064700161212     C                   MOVE      WASC15        A_VHCCAS
064800161212     C*
064900161212     C                   MOVE      VABVCA        A_VHCVCA
065000161212     C                   MOVE      VABCCM        A_VHCCCM
065100161212     C                   MOVE      VABRMN        A_VHCRMN
065200161212     C                   MOVE      VABRMA        A_VHCRMA
065300161212     C                   MOVE      VABRMO        A_VHCRMO
065400161212     C                   MOVE      VABFFD        A_VHCFFD
065500161212     C                   MOVE      VABTCR        A_VHCTCR
065600161212     C                   MOVE      VABDCR        A_VHCDCR
065700161212     C                   MOVE      VABHCR        A_VHCHCR
065800161212     C                   MOVE      VABTC1        A_VHCTC1
065900161212     C                   MOVE      VABTC2        A_VHCTC2
066000161212     C*
066100161212     C* Forzatura valore CCA x stato "IN TRANSITO"
066200161212     C                   MOVEL     '*'           A_VHCCCA
066300161212     C*
066400161212     C                   MOVE      '00000000'    A_VHCDCM
066500161212     C                   MOVE      '0000'        A_VHCHMC
066600161212     C                   MOVE      '00000000'    A_VHCDLA
066700161212     C                   MOVE      '00000000'    A_VHCDAG
066800161212     C*
066900161212     C                   ENDSR
067000010131
067100010131
067200010131
067300010131     C*
067400180118     C**   repta4        BEGSR
067500010131     C*
067600010131     C* Inizializzo i campi "fissi"
067700180118     C**                 movel     'J'           ar4trc
067800161215     C*
067900010131     C* Posizionamento diretto
068000180118     C**   KEYta430_C    chain     tita430c
068100180118     C**                 if        %found(tita430c)
068200180118     C**                 clear                   DSBL4J
068300180118     C**                 movel     ta4not        DSBL4J
068400180118     C**                 if        §B4BAI = 'S'
068500180118     c*                  movel     'S'           A_VHBFTM
068600180118     C**                 movel     'S'           E_VHBFTM
068700180118     C**                 movel     'S'           A_VHCFTM
068800180118     C**                 endif
068900180118     C**                 endif
069000010131     C*
069100180118     C***                ENDSR
069200080527
069300080527
069400080527
069500080527     C*
069600080527     C     reptas        BEGSR
069700080527     C*
069800080527     C* Posizionamento diretto
069900080527     C     KEYtas30_P    chain     titas30c
070000080527     C                   if        %found(titas30c)
070100180118     C                   move      tasDCR        vabDCR
070200180118     c
070300180118     c                   else
070400180118     c
070500180118     C     KEYtas30_P    chain     fnblp01l
070600180118     C                   if        %found(fnblp01l)
070700180118     C                   move      blpDCR        vabDCR
070800080527     C                   endif
070900180118     C                   endif
071000080527     C*
071100080527     C                   ENDSR
071200161215
071300161215
071400161215
071500161215     C     repNOT        BEGSR
071600161215     C*
071700161215     C                   movel     *blanks       wNOTE_8          35
071800161215     C                   movel     *blanks       wNOTE_9          35
071900161215     C*
072000161215     C                   eval      ar4trc = '8'
072100161215     C     KEYta430_C    chain     fiar401l
072200161215     C                   if        %found(fiar401l)
072300161215     C                   eval      wNOTE_8 = ar4NOT
072400161215     C                   else
072500161215     C                   eval      ar4trc = '8'
072600161215     C     KEYta430_C    chain     tita430c
072700161215     C                   if        %found(tita430c)
072800161215     C                   eval      wNOTE_8 = ta4NOT
072900161215     C                   endif
073000161215     C                   endif
073100161215     C*
073200161215     C                   eval      ar4trc = '9'
073300161215     C     KEYta430_C    chain     fiar401l
073400161215     C                   if        %found(fiar401l)
073500161215     C                   eval      wNOTE_9 = ar4NOT
073600161215     C                   else
073700161215     C                   eval      ar4trc = '9'
073800161215     C     KEYta430_C    chain     tita430c
073900161215     C                   if        %found(tita430c)
074000161215     C                   eval      wNOTE_9 = ta4NOT
074100161215     C                   endif
074200161215     C                   endif
074300161215     C*
074400161215     C                   ENDSR
074500171123
074600171123     C     reppnd        BEGSR
074700171123     c                   clear                   wpndipn
074800171123     c                   clear                   warsnot_c
074900171123     C*
075000171123     C* Posizionamento diretto
075100171123     C     KEYtas30_P    chain     fipnd01l
075200171123     c                   if        %found(fipnd01l)
075300171123     c                   eval      wpndipn=pndipn
075400171123     c                   endif
075500171123     c                   move      '.'           wpndipn
075600171123     c                   ENDSR
075700161212
075800161212
075900161212     C     CARTAB        BEGSR
076000161212     C*
076100161212     C     'VPC'         SETLL     tntbe01l
076200161212     C                   IF        %equal(tntbe01l)
076300161212     C     'VPC'         READE     tntbe01l
076400161212     C                   DOW       not %eof(tntbe01l)
076500161212     C                   EVAL      DVPC = tbeUNI
076600161212     C                   IF        §VPCVTA = 'S'
076700161212     C                   ADD       1             jVPC
076800161212     C                   EVAL      skVPC(jVPC) = %dec(%subst(tbeKE1:1:7):7:0)
076900161212     C                   ENDIF
077000161212     C     'VPC'         READE     tntbe01l
077100161212     C                   ENDDO
077200161212     C                   ENDIF
077300161212     C*
077400161212     C                   ENDSR
077500010528
077600000629
077700991027
077800991027      /TITLE Operazioni iniziali.
077900991027     C     *inzsr        BEGSR
078000991027     C*
078100991027     C     *ENTRY        PLIST
078200991027     C                   parm                    prmppt
078300991027     C     wrkesito      parm      wrkesito      prmesito
078400080527     C*
078500080527     C     KEYtas30_P    KLIST
078600080527     C                   KFLD                    vabaas
078700080527     C                   KFLD                    vablnp
078800080527     C                   KFLD                    vabnrs
078900080527     C                   KFLD                    vabnsp
079000121218     C*
079100121218     C     KEYblt01_P    KLIST
079200121218     C                   KFLD                    vabaas
079300121218     C                   KFLD                    vablnp
079400121218     C                   KFLD                    vabnrs
079500121218     C                   KFLD                    vabnsp
079600121218     C*
079700121218     C     KEYars01_C    KLIST
079800121218     C                   KFLD                    bltFLS
079900121218     C                   KFLD                    bltLNA
080000121218     C                   KFLD                    bltNRS
080100121218     C                   KFLD                    bltNSC
080200121218     C                   KFLD                    arsTRC
080300161215     C*
080400161215     C     KEYta430_C    KLIST
080500161215     C                   KFLD                    vabaas
080600161215     C                   KFLD                    vablnp
080700161215     C                   KFLD                    vabnrs
080800161215     C                   KFLD                    vabnsp
080900161215     C                   KFLD                    ar4trc
081000991027     C*
081100991027     C                   ENDSR
081200161215
