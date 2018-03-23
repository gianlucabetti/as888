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
001200180118     Ftita430c  if   E           k DISK
001300080527     Ftitas30c  if   E           k DISK
001400121218     Ffnblt01l  if   E           k DISK    usropn
001500121218     F                                     extfile(LibFileBLT01)
001600121218     Ffiars01l  if   E           k DISK    usropn
001700121218     F                                     extfile(LibFileARS01)
001800180118     Ffnblp01l  if   E           k DISK    usropn
001900180118     F                                     extfile(LibFileBLP01)
002000171123     Ffipnd01l  if   E           k DISK
002100991027
002200991027     D prmppt          s             50
002300991027     D prmesito        s              1
002400161213     D wrkesito        s                   like(prmesito)
002500161212     D DVPC          e ds                  inz
002600161213 xxx D*** DVGD001       e ds                  inz
002700161212     D fnvabDS       e ds                  extname(fnvab00t)
002800161212     D fnvabAsciDS   e ds                  extname(fnvabwwt) prefix(A_)
002900161212     D fnvabEbcdDS   e ds                  extname(fnvab1wt) prefix(E_)
003000161212     D fnvacAsciDS   e ds                  extname(fnvacwwt) prefix(A_)
003100121218
003200161212     D wARSNOT_C       s                   like(ARSNOT) inz
003300171123     D wpndipn         s             15    inz
003400161212     D wVAWDTA         s                   like(VAWDTA) inz
003500161212     D l_VB_A          s              4  0 inz(%size(fnvabAsciDS))
003600161212     D l_VB_E          s              4  0 inz(%size(fnvabEbcdDS))
003700161212     D l_VC_A          s              4  0 inz(%size(fnvacAsciDS))
003800161213     D iVAB            s               N   inz
003900161213     D iVAC            s               N   inz
004000161213     D iASCI           s               N   inz
004100161213     D iEBCD           s               N   inz
004200161215     D iS7VB           s               N   inz
004300171123     D ipnd            s               N   inz
004400121218
004500121218     D LibFileBLT01    s             21A   inz
004600121218     D LibFileARS01    s             21A   inz
004700161215     D LibFileAR401    s             21A   inz
004800180118     D LibFileBLP01    s             21A   inz
004900161212     D currSysNeta     s              8A   inz('*NULL')
005000161212
005100161212
005200161212     D*-----------
005300161212     D* SCHIERE
005400161212     D*-----------
005500161212     D skVPC           S              7  0 DIM(500)
005600161212     D jVPC            S              3  0 INZ(*zeros)
005700121218
005800161212
005900000629     D*----------
006000000629     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
006100161212     D*----------
006200000629     D* 10+3
006300000629     D                 DS
006400000629     D  WS15                   1      1
006500000629     D  WI15                   2     11
006600000629     D  WV15                  12     12
006700000629     D  WD15                  13     15
006800000629     D  WASC15                 1     15
006900000629     D*  6+1
007000000629     D                 DS
007100000629     D  WS09                   1      1
007200000629     D  WI09                   2      7
007300000629     D  WV09                   8      8
007400000629     D  WD09                   9      9
007500000629     D  WASC09                 1      9
007600000629     D*  2+3
007700000629     D                 DS
007800000629     D  WS07                   1      1
007900000629     D  WI07                   2      3
008000000629     D  WV07                   4      4
008100000629     D  WD07                   5      7
008200000629     D  WASC07                 1      7
008300121218
008400161212
008500010131     D*-------------------
008600010131     D* DS ESTENSIONE BOLLE TIPO RECORD "J" (X BOLE INGOMBRANTI NON DICHIARATE)
008700010131     D*-------------------
008800010131     D DSBL4J        E DS
008900991027
009000121218
009100121218     D*------------------
009200121218     D* LINKING A DEFINIZIONI ESTERNE
009300121218     D*------------------
009400121218     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009500121218     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009600161212     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
009700161212     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
009800121218
009900161212
010000010131
010100010131
010200121218     C*
010300121218     C* Reperisco il sistema AS/400 corrente
010400121218     C                   callp     UBRTVNETA_Rtv(currSysNeta)
010500121218     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
010600121218     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
010700121218     C                   eval      LibFileARS01 = 'FILTRA201/FIARS01L'
010800161215     C                   eval      LibFileAR401 = 'FILTRA201/FIAR401L'
010900180118     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
011000121218     C                   else
011100121218     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
011200121218     C                   eval      LibFileARS01 = 'FILTRAPRD/FIARS01L'
011300161215     C                   eval      LibFileAR401 = 'FILTRAPRD/FIAR401L'
011400180118     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
011500121218     C                   endif
011600121218     C*
011700121218     C                   if        not %open(FNBLT01L)
011800121218     C                   open      FNBLT01L
011900121218     C                   endif
012000121218     C*
012100121218     C                   if        not %open(FIARS01L)
012200121218     C                   open      FIARS01L
012300121218     C                   endif
012400161215     C*
012500161215     C                   if        not %open(FIAR401L)
012600161215     C                   open      FIAR401L
012700161215     C                   endif
012800180118     C                   if        not %open(Fnblp01L)
012900180118     C                   open      Fnblp01L
013000180118     C                   endif
013100170118     C*
013200170118     C* Avvio il monitoring del intero flusso
013300170118     C                   monitor
013400170118     C*
013500170118     C* Effettuo traduzione
013600991027     C                   exsr      traduci
013700170118     C*
013800170118     C* Gestisco eventuale errore
013900170118     C                   on-error
014000170118     C*
014100170118     C* Imposto esito transazione a errore
014200170118     C                   EVAL      wrkesito = '2'
014300170118     C*
014400170118     C* Se errore => emetto operazione d ROLLBACK
014500170118     C                   ROLBK
014600170118     C*
014700170118     C* Arresto il monitoring
014800170118     C                   endmon
014900121218     C*
015000121218     C                   if        %open(FNBLT01L)
015100121218     C                   close     FNBLT01L
015200121218     C                   endif
015300121218     C*
015400121218     C                   if        %open(FIARS01L)
015500121218     C                   close     FIARS01L
015600121218     C                   endif
015700161215     C*
015800161215     C                   if        %open(FIAR401L)
015900161215     C                   close     FIAR401L
016000161215     C                   endif
016100180119     C                   if        %open(FNBLP01L)
016200180119     C                   close     FNBLP01L
016300180119     C                   endif
016400121218     C*
016500991027     C                   seton                                        lr
016600000629
016700121218
016800000629
016900161212     C     traduci       BEGSR
017000161212     C*
017100161212     C* Carico tabelle di procedura
017200161212     C                   exsr      cartab
017300000629     C*
017400161212     C* Inizializzazione indicatori di procedure per parametri input
017500161213     C                   eval      *in45 = *off
017600161213     C                   eval      *in46 = *off
017700161213     C                   eval      *in47 = *off
017800161213     C                   eval      *in49 = *off
017900161213     C                   eval      iVAB  = *off
018000161213     C                   eval      iVAC  = *off
018100161213     C                   eval      iASCI = *off
018200161213     C                   eval      iEBCD = *off
018300161215     C                   eval      iS7VB = *off
018400171123     C                   eval      ipnd  = *off
018500161212     C*
018600161212     C* Se richiesto valorizzo da data consegna richiesta con il proprio valore
018700180118     C                   if        %subst(prmppt:5:1) = 'R'
018800180118     C                   seton                                        45
018900180118     C                   endif
019000161212     C*
019100161212     C* Se richiesto ritorno solamente le bolle originali
019200161212     C                   if        %subst(prmppt:6:1) = 'O'
019300161212     C                   seton                                        46
019400161212     C                   endif
019500161212     C*
019600161213     C* Se richiesto ritorno solamente le bolle con serie
019700161212     C                   if        %subst(prmppt:7:1) = 'S'
019800161213     C                   seton                                        47
019900161212     C                   endif
020000161212     C*
020100161212     C* Se richiesto ritorno anche i dati del segnacollo cliente
020200161212     C                   if        %subst(prmppt:9:1) = 'S'
020300161212     C                   seton                                        49
020400161212     C                   endif
020500161212     C*
020600161212     C* Se richiesto ritorno dati in formato EBCDIC
020700161212     C                   if        %subst(prmppt:10:1) = 'S'
020800161213     C                   eval      iEBCD = *on
020900161213     C                   else
021000161213     C                   eval      iASCI = *on
021100161212     C                   endif
021200161212     C*
021300161212     C* Se richiesto ritorno dati in formato "VAC"
021400161212     C                   if        %subst(prmppt:11:1) = 'S'
021500161213     C                   eval      iVAC = *on
021600161213     C                   else
021700161213     C                   eval      iVAB = *on
021800161213     C                   endif
021900161215     C*
022000161215     C* Se richiesto filtro solo dati da *pgm TIS7VBR
022100161215     C                   if        %subst(prmppt:12:1) = 'S'
022200161215     C                   eval      iS7VB = *on
022300161215     C                   endif
022400171123     C* Se richiesto passo anche parcel DPD
022500171123     C                   if        %subst(prmppt:15:1) = 'S'
022600171123     C                   eval      ipnd  = *on
022700171123     C                   endif
022800161212     C*
022900161213     C* Scorro tutto il file
023000161212     C                   read      tivgdtmp
023100161213     C                   dow       not %eof(tivgdtmp)
023200161212     C*
023300161212     C* Inizializzo DS di output
023400170119     C                   clear                   TIVAW000
023500161212     C                   clear                   fnvabAsciDS
023600161212     C                   clear                   fnvabEbcdDS
023700161212     C                   clear                   fnvacAsciDS
023800161212     C*
023900161212     C* Valorizzo dati "corrrenti"
024000161212     C                   exsr      defcam
024100161212     C                   movel     vgddta        fnvabDS
024200170111     C*
024300170111     C* Eseguo considerazioni su pacchi ingombranti aggiunti da Bartolini
024400180118     c* cose delle poste che non ci sono più
024500180118     C****               exsr      repTA4
024600170111     C*
024700170111     C* Reperimento note 8 e 9
024800170111     C                   exsr      repNOT
024900170111     C*
025000170111     C* Reperimento dati bolla da Sede
025100170111     C   45              exsr      repTAS
025200171123
025300171123     C* Reperimento parcel dpd se presente
025400171123     c                   if        ipnd=*on
025500171123     C                   exsr      repPND
025600171123     c                   endif
025700161213     C*
025800161213     C* Gestisco formato richiesto VAB/VAC e ASCII/EBCDIC
025900161213     C                   select
026000161213     C                   when      iVAB and iASCI
026100161213     C                   exsr      fmtVAB_ASCI
026200161213     C                   when      iVAB and iEBCD
026300161213     C                   exsr      fmtVAB_EBCD
026400161213     C                   when      iVAC and iASCI
026500161213     C                   exsr      fmtVAC_ASCI
026600161213     C                   other
026700161213     C                   eval      wrkesito = '2'
026800161213     C                   endsl
026900161213     C*
027000161213     C* Se tutto ok => procedo
027100161213     C                   if        wrkesito = *blanks
027200080613     C*
027300161212     C* Inizializzo indicatore di record corrente OK
027400080613     C                   setoff                                       31
027500161212     C*
027600161212     C* Chiamata metodo GetLblTyp
027700161212     C   46              if        UBLBLSPE_GetLblTyp(vabAAS
027800161212     C                                               :vabLNP
027900161212     C                                               :vabNRS
028000161212     C                                               :vabNSP
028100161212     C                                               :pOutLblTyp
028200161212     C                                               :pOutAnnoBO
028300161212     C                                               :pOutLineaParBO
028400161212     C                                               :pOutSerieBO
028500161212     C                                               :pOutNumSpedBO
028600161212     C                                               :pOutDcmBO
028700161212     C                                               :pOutCcaBO
028800161212     C                                               :pOutRblBO) = 0
028900161212     C                   if        pOutLblTyp <> 'O'
029000161212     C                   seton                                        31
029100161212     C                   endif
029200161212     C                   endif
029300161212     C*
029400161212     C* Solo bolle con serie
029500161212     C   47              if        vabNRS = *zeros
029600161212     C                   seton                                        31
029700161212     C                   endif
029800161215     C*
029900161215     C* Solo record generati da *pgm TIS7VBR
030000161215     C                   if        iS7VB AND vgdpgm <> 'TIS7VBR'
030100161215     C                   seton                                        31
030200161215     C                   endif
030300080613     C*
030400161213     C* Se record ok => procedo
030500080613     C                   if        *in31 = *off
030600121218     C*
030700161212     C* Scarico il buffer di output formato richiesto VAB/VAC e ASCII/EBCDIC
030800161213     C                   select
030900161213     C                   when      iVAB and iASCI
031000161213     C                   eval      VAWDTA = fnvabAsciDS
031100161213     C                   when      iVAB and iEBCD
031200161213     C                   eval      VAWDTA = fnvabEbcdDS
031300161213     C                   when      iVAC and iASCI
031400161213     C                   eval      VAWDTA = fnvacAsciDS
031500161213     C                   endsl
031600171123
031700171123     c*  campo di comodo per comporre i dati in uscita
031800171123     C                   eval      wVAWDTA = VAWDTA
031900121218     C*
032000121218     C* Se richiesto nei parametri d ingresso ritorno anche i dati del segnacollo cliente
032100161215     C                   if        *in49
032200121218     C*
032300121218     C* Aggancio i dettagli colli e ciclo x ciascuno
032400121218     C     KEYblt01_P    setll     fnblt01l
032500121218     C                   if        %found(fnblt01l)
032600121218     C     KEYblt01_P    reade     fnblt01l
032700121218     C                   dow       not %eof(fnblt01l)
032800121218     C*
032900121218     C                   clear                   wARSNOT_C
033000121218     C*
033100121218     C* Reperisco il segnacollo cliente ("chi sono")
033200121218     C                   eval      arsTRC = 'C'
033300121218     C     KEYars01_C    chain     fiars01l
033400121218     C                   if        %found(fiars01l)
033500121218     C                   eval      wARSNOT_C = arsNOT
033600121218     C                   endif
033700161213     C*
033800171123     c                   exsr      riempiREC
033900171123     c
034000161212     C                   write     TIVAW000
034100121218     C*
034200121218     C* Scorro il segnacollo successivo
034300121218     C     KEYtas30_P    reade     fnblt01l
034400121218     C                   enddo
034500121218     C*
034600161212     C* Se non trovo il dettaglio segnacolli BRT in filiale scrivo almeno un record
034700121218     C                   else
034800171123     C*
034900171123     c                   if        ipnd=*on
035000171123     c                   exsr      riempirec
035100171123     c                   endif
035200171123
035300161212     C                   write     TIVAW000
035400161212     C                   endif
035500121218     C*
035600121218     C                   else
035700171123     c                   if        ipnd=*on
035800171123     c                   exsr      riempirec
035900171123     c                   endif
036000171123
036100161212     C                   write     TIVAW000
036200121218     C                   endif
036300000629     C*
036400161212     C* Se partcolarità abilitata in tabella 'VPC' => eseguo
036500161212     C     vabCCM        lookup    skVPC                                  05
036600161212     C                   if        %found
036700161212     C                   call      'TIS7T99R'
036800161212     C                   parm      vabCCM        I99CCM            7 0
036900161212     C                   parm      vabAAS        I99AAS            4 0
037000161212     C                   parm      vabLNP        I99LNP            3 0
037100161212     C                   parm      vabNRS        I99NRS            2 0
037200161212     C                   parm      vabNSP        I99NSP            7 0
037300161212     C                   endif
037400161212     C*
037500161212     C                   endif
037600161213     C*
037700161213     C                   endif
037800161213     C*
037900161213 xxx C* Aggiorno lo stato sul TIVGD00F
038000161213 xxx C***                eval      DVGD001 = vgdATTRIB
038100161213 xxx C***                if        §VGD001RRN > *zeros
038200161213 xxx C***  §VGD001RRN    chain(e)  tivgd00f
038300161213 xxx C***                if        %found(tivgd00f)
038400161213 xxx C***                eval      F_vgdSTO = 'S'
038500161213 xxx C***                update(e) F_tivgd
038600161213 xxx C***                endif
038700161213 xxx C***                endif
038800161212     C*
038900161212     C* Proseguo con il record successivo
039000161212     C                   read      tivgdtmp
039100161212     C                   enddo
039200000629     C*
039300161212     C                   if        wrkesito <> '2'
039400161212     C                   eval      wrkesito  = '0'
039500161212     C                   endif
039600000629     C*
039700000629     C                   ENDSR
039800171123     c
039900171123     c
040000171123     c     Riempirec     BEGSR
040100171123     C                   select
040200171123     C                   when      iVAB and iASCI
040300171123     c                   if        ipnd=*on
040400171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_A)+wARSNOT_C+
040500171123     c                             wpndipn
040600171123     c                   else
040700171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_A)+wARSNOT_C+'.'
040800171123     c                   endif
040900171123
041000171123     C                   when      iVAB and iEBCD
041100171123     c                   if        ipnd=*on
041200171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_E)+wARSNOT_C+
041300171123     c                             wpndipn
041400171123     c                   else
041500171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VB_E)+wARSNOT_C+'.'
041600171123     c                   endif
041700171123     c
041800171123     C                   when      iVAC and iASCI
041900171123     c                   if        ipnd=*on
042000171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VC_A)+wARSNOT_C+
042100171123     c                             wpndipn
042200171123     c                   else
042300171123     C                   eval      VAWDTA=%subst(wVAWDTA:1:l_VC_A)+wARSNOT_C+'.'
042400171123     c                   endif
042500171123     c
042600171123     C                   endsl
042700171123     c                   ENDSR
042800161212
042900161212
043000161212
043100161212     C     defcam        BEGSR
043200161212     C*
043300161212     C                   MOVE      *BLANK        WS15
043400161212     C                   MOVE      ','           WV15
043500161212     C                   MOVE      *BLANK        WS07
043600161212     C                   MOVE      ','           WV07
043700161212     C                   MOVE      *BLANK        WS09
043800161212     C                   MOVE      ','           WV09
043900161212     C*
044000161212     C                   ENDSR
044100161212
044200161212
044300161212
044400161212     C*
044500161212     C     fmtVAB_ASCI   BEGSR
044600161212     C*
044700161212     C                   MOVE      VABAAS        A_VHBAAS
044800161212     C                   MOVE      VABLNP        A_VHBLNP
044900161212     C                   MOVE      VABNRS        A_VHBNRS
045000161212     C                   MOVE      VABNSP        A_VHBNSP
045100161212     C                   MOVE      VABMGS        A_VHBMGS
045200161212     C                   MOVE      VABCBO        A_VHBCBO
045300161212     C                   MOVE      VABLNA        A_VHBLNA
045400161212     C                   MOVE      VABRSD        A_VHBRSD
045500161212     C                   MOVE      VABPRD        A_VHBPRD
045600161212     C                   MOVE      VABGC1        A_VHBGC1
045700161212     C                   MOVE      VABGC2        A_VHBGC2
045800161212     C                   MOVE      VABCTR        A_VHBCTR
045900161212     C                   MOVE      VABCTS        A_VHBCTS
046000161212     C                   MOVE      VABFTM        A_VHBFTM
046100161212     C                   MOVE      VABTSP        A_VHBTSP
046200161212     C*
046300161212     C                   MOVE      VABIAS        WD15
046400161212     C                   MOVEL     VABIAS        WI15
046500161212     C                   MOVE      WASC15        A_VHBIAS
046600161212     C*
046700161212     C                   MOVE      VABVAS        A_VHBVAS
046800161212     C                   MOVE      VABNAS        A_VHBNAS
046900161212     C                   MOVE      VABNCL        A_VHBNCL
047000161212     C*
047100161212     C                   MOVE      VABPKB        WD09
047200161212     C                   MOVEL     VABPKB        WI09
047300161212     C                   MOVE      WASC09        A_VHBPKB
047400161212     C*
047500161212     C                   MOVE      VABVLB        WD07
047600161212     C                   MOVEL     VABVLB        WI07
047700161212     C                   MOVE      WASC07        A_VHBVLB
047800161212     C*
047900161212     C                   MOVE      VABQFT        WD15
048000161212     C                   MOVEL     VABQFT        WI15
048100161212     C                   MOVE      WASC15        A_VHBQFT
048200161212     C*
048300161212     C                   MOVE      VABVMD        WD15
048400161212     C                   MOVEL     VABVMD        WI15
048500161212     C                   MOVE      WASC15        A_VHBVMD
048600161212     C*
048700161212     C                   MOVE      VABTIC        A_VHBTIC
048800161212     C*
048900161212     C                   MOVE      VABCAS        WD15
049000161212     C                   MOVEL     VABCAS        WI15
049100161212     C                   MOVE      WASC15        A_VHBCAS
049200161212     C*
049300161212     C                   MOVE      VABVCA        A_VHBVCA
049400161212     C                   MOVE      VABCCM        A_VHBCCM
049500161212     C                   MOVE      VABRMN        A_VHBRMN
049600161212     C                   MOVE      VABRMA        A_VHBRMA
049700161212     C                   MOVE      VABRMO        A_VHBRMO
049800161212     C                   MOVE      VABFFD        A_VHBFFD
049900161212     C                   MOVE      VABTCR        A_VHBTCR
050000161212     C                   MOVE      VABDCR        A_VHBDCR
050100161212     C                   MOVE      VABHCR        A_VHBHCR
050200161212     C                   MOVE      VABTC1        A_VHBTC1
050300161212     C                   MOVE      VABTC2        A_VHBTC2
050400161212     C                   MOVE      VABATB        A_VHBATB
050500161212     C                   MOVE      VABRD2        A_VHBRD2
050600161212     C                   MOVE      VABIND        A_VHBIND
050700161212     C                   MOVE      VABCAD        A_VHBCAD
050800161212     C                   MOVE      VABLOD        A_VHBLOD
050900161212     C                   MOVE      VABNZD        A_VHBNZD
051000161212     C                   MOVE      VABGCA        A_VHBGCA
051100161212     C                   MOVE      VABNCD        A_VHBNCD
051200161212     C                   MOVE      VABNCA        A_VHBNCA
051300161212     C                   MOVE      VABXCO        A_VHBXCO
051400161212     C                   MOVE      VABNOT        A_VHBNOT
051500161212     C                   MOVE      VABNT2        A_VHBNT2
051600161215     C*
051700161215     C                   IF        wNOTE_8 <> *blanks
051800161215     C                   MOVE      wNOTE_8       A_VHBNOT
051900161215     C                   ENDIF
052000161215     C                   IF        wNOTE_9 <> *blanks
052100161215     C                   MOVE      wNOTE_9       A_VHBNT2
052200161215     C                   ENDIF
052300161215     C*
052400161212     C                   MOVE      VABZNC        A_VHBZNC
052500161212     C                   MOVE      VABCTM        A_VHBCTM
052600161212     C                   MOVE      VABVAD        A_VHBVAD
052700161212     C                   MOVE      VABGMA        A_VHBGMA
052800161212     C                   MOVE      VABGGA        A_VHBGGA
052900161212     C                   MOVE      VABGVA        A_VHBGVA
053000161212     C                   MOVE      VABSCL        A_VHBSCL
053100161212     C                   MOVE      VABANT        A_VHBANT
053200161212     C                   MOVE      VABCMO        A_VHBCMO
053300161212     C                   MOVE      VABNMO        A_VHBNMO
053400161212     C*
053500161212     C                   ENDSR
053600161212
053700161212
053800161212
053900161212     C*
054000161212     C     fmtVAB_EBCD   BEGSR
054100161212     C*
054200161212     C                   MOVE      VABAAS        E_VHBAAS
054300161212     C                   MOVE      VABLNP        E_VHBLNP
054400161212     C                   MOVE      VABNRS        E_VHBNRS
054500161212     C                   MOVE      VABNSP        E_VHBNSP
054600161212     C                   MOVE      VABMGS        E_VHBMGS
054700161212     C                   MOVE      VABCBO        E_VHBCBO
054800161212     C                   MOVE      VABLNA        E_VHBLNA
054900161212     C                   MOVE      VABRSD        E_VHBRSD
055000161212     C                   MOVE      VABPRD        E_VHBPRD
055100161212     C                   MOVE      VABGC1        E_VHBGC1
055200161212     C                   MOVE      VABGC2        E_VHBGC2
055300161212     C                   MOVE      VABCTR        E_VHBCTR
055400161212     C                   MOVE      VABCTS        E_VHBCTS
055500161212     C                   MOVE      VABFTM        E_VHBFTM
055600161212     C                   MOVE      VABTSP        E_VHBTSP
055700161212     C*
055800161212     C                   MOVE      VABIAS        E_VHBIAS
055900161212     C                   MOVE      VABVAS        E_VHBVAS
056000161212     C                   MOVE      VABNAS        E_VHBNAS
056100161212     C                   MOVE      VABNCL        E_VHBNCL
056200161212     C                   MOVE      VABPKB        E_VHBPKB
056300161212     C                   MOVE      VABVLB        E_VHBVLB
056400161212     C                   MOVE      VABQFT        E_VHBQFT
056500161212     C                   MOVE      VABVMD        E_VHBVMD
056600161212     C                   MOVE      VABTIC        E_VHBTIC
056700161212     C                   MOVE      VABCAS        E_VHBCAS
056800161212     C                   MOVE      VABVCA        E_VHBVCA
056900161212     C                   MOVE      VABCCM        E_VHBCCM
057000161212     C                   MOVE      VABRMN        E_VHBRMN
057100161212     C                   MOVE      VABRMA        E_VHBRMA
057200161212     C                   MOVE      VABRMO        E_VHBRMO
057300161212     C                   MOVE      VABFFD        E_VHBFFD
057400161212     C                   MOVE      VABTCR        E_VHBTCR
057500161212     C                   MOVE      VABDCR        E_VHBDCR
057600161212     C                   MOVE      VABHCR        E_VHBHCR
057700161212     C                   MOVE      VABTC1        E_VHBTC1
057800161212     C                   MOVE      VABTC2        E_VHBTC2
057900161212     C                   MOVE      VABATB        E_VHBATB
058000161212     C                   MOVE      VABRD2        E_VHBRD2
058100161212     C                   MOVE      VABIND        E_VHBIND
058200161212     C                   MOVE      VABCAD        E_VHBCAD
058300161212     C                   MOVE      VABLOD        E_VHBLOD
058400161212     C                   MOVE      VABNZD        E_VHBNZD
058500161212     C                   MOVE      VABGCA        E_VHBGCA
058600161212     C                   MOVE      VABNCD        E_VHBNCD
058700161212     C                   MOVE      VABNCA        E_VHBNCA
058800161212     C                   MOVE      VABXCO        E_VHBXCO
058900161212     C                   MOVE      VABNOT        E_VHBNOT
059000161215     C                   MOVE      VABNT2        E_VHBNT2
059100161215     C*
059200161215     C                   IF        wNOTE_8 <> *blanks
059300161215     C                   MOVE      wNOTE_8       E_VHBNOT
059400161215     C                   ENDIF
059500161215     C                   IF        wNOTE_9 <> *blanks
059600161215     C                   MOVE      wNOTE_9       E_VHBNT2
059700161215     C                   ENDIF
059800161215     C*
059900161212     C                   MOVE      VABZNC        E_VHBZNC
060000161212     C                   MOVE      VABCTM        E_VHBCTM
060100161212     C                   MOVE      VABVAD        E_VHBVAD
060200161212     C                   MOVE      VABGMA        E_VHBGMA
060300161212     C                   MOVE      VABGGA        E_VHBGGA
060400161212     C                   MOVE      VABGVA        E_VHBGVA
060500161212     C                   MOVE      VABSCL        E_VHBSCL
060600161212     C                   MOVE      VABANT        E_VHBANT
060700161212     C                   MOVE      VABCMO        E_VHBCMO
060800161212     C                   MOVE      VABNMO        E_VHBNMO
060900161212     C*
061000161212     C                   ENDSR
061100161212
061200161212
061300161212
061400161212     C*
061500161212     C     fmtVAC_ASCI   BEGSR
061600161212     C*
061700161212     C                   MOVE      VABAAS        A_VHCAAS
061800161212     C                   MOVE      VABLNP        A_VHCLNP
061900161212     C                   MOVE      VABNRS        A_VHCNRS
062000161212     C                   MOVE      VABNSP        A_VHCNSP
062100161212     C                   MOVE      VABMGS        A_VHCMGS
062200161212     C                   MOVE      VABCBO        A_VHCCBO
062300161212     C                   MOVE      VABLNA        A_VHCLNA
062400161212     C                   MOVE      VABRSD        A_VHCRSD
062500161212     C                   MOVE      VABPRD        A_VHCPRD
062600161212     C                   MOVE      VABGC1        A_VHCGC1
062700161212     C                   MOVE      VABGC2        A_VHCGC2
062800161212     C                   MOVE      VABCTR        A_VHCCTR
062900161212     C                   MOVE      VABCTS        A_VHCCTS
063000161212     C                   MOVE      VABFTM        A_VHCFTM
063100161212     C                   MOVE      VABTSP        A_VHCTSP
063200161212     C*
063300161212     C                   MOVE      VABIAS        WD15
063400161212     C                   MOVEL     VABIAS        WI15
063500161212     C                   MOVE      WASC15        A_VHCIAS
063600161212     C*
063700161212     C                   MOVE      VABVAS        A_VHCVAS
063800161212     C                   MOVE      VABNAS        A_VHCNAS
063900161212     C                   MOVE      VABNCL        A_VHCNCL
064000161212     C*
064100161212     C                   MOVE      VABPKB        WD09
064200161212     C                   MOVEL     VABPKB        WI09
064300161212     C                   MOVE      WASC09        A_VHCPKB
064400161212     C*
064500161212     C                   MOVE      VABVLB        WD07
064600161212     C                   MOVEL     VABVLB        WI07
064700161212     C                   MOVE      WASC07        A_VHCVLB
064800161212     C*
064900161212     C                   MOVE      VABQFT        WD15
065000161212     C                   MOVEL     VABQFT        WI15
065100161212     C                   MOVE      WASC15        A_VHCQFT
065200161212     C*
065300161212     C                   MOVE      VABTIC        A_VHCTIC
065400161212     C*
065500161212     C                   MOVE      VABCAS        WD15
065600161212     C                   MOVEL     VABCAS        WI15
065700161212     C                   MOVE      WASC15        A_VHCCAS
065800161212     C*
065900161212     C                   MOVE      VABVCA        A_VHCVCA
066000161212     C                   MOVE      VABCCM        A_VHCCCM
066100161212     C                   MOVE      VABRMN        A_VHCRMN
066200161212     C                   MOVE      VABRMA        A_VHCRMA
066300161212     C                   MOVE      VABRMO        A_VHCRMO
066400161212     C                   MOVE      VABFFD        A_VHCFFD
066500161212     C                   MOVE      VABTCR        A_VHCTCR
066600161212     C                   MOVE      VABDCR        A_VHCDCR
066700161212     C                   MOVE      VABHCR        A_VHCHCR
066800161212     C                   MOVE      VABTC1        A_VHCTC1
066900161212     C                   MOVE      VABTC2        A_VHCTC2
067000161212     C*
067100161212     C* Forzatura valore CCA x stato "IN TRANSITO"
067200161212     C                   MOVEL     '*'           A_VHCCCA
067300161212     C*
067400161212     C                   MOVE      '00000000'    A_VHCDCM
067500161212     C                   MOVE      '0000'        A_VHCHMC
067600161212     C                   MOVE      '00000000'    A_VHCDLA
067700161212     C                   MOVE      '00000000'    A_VHCDAG
067800161212     C*
067900161212     C                   ENDSR
068000010131
068100010131
068200010131
068300010131     C*
068400180118     C**   repta4        BEGSR
068500010131     C*
068600010131     C* Inizializzo i campi "fissi"
068700180118     C**                 movel     'J'           ar4trc
068800161215     C*
068900010131     C* Posizionamento diretto
069000180118     C**   KEYta430_C    chain     tita430c
069100180118     C**                 if        %found(tita430c)
069200180118     C**                 clear                   DSBL4J
069300180118     C**                 movel     ta4not        DSBL4J
069400180118     C**                 if        §B4BAI = 'S'
069500180118     c*                  movel     'S'           A_VHBFTM
069600180118     C**                 movel     'S'           E_VHBFTM
069700180118     C**                 movel     'S'           A_VHCFTM
069800180118     C**                 endif
069900180118     C**                 endif
070000010131     C*
070100180118     C***                ENDSR
070200080527
070300080527
070400080527
070500080527     C*
070600080527     C     reptas        BEGSR
070700080527     C*
070800080527     C* Posizionamento diretto
070900080527     C     KEYtas30_P    chain     titas30c
071000080527     C                   if        %found(titas30c)
071100180118     C                   move      tasDCR        vabDCR
071200180118     c
071300180118     c                   else
071400180118     c
071500180118     C     KEYtas30_P    chain     fnblp01l
071600180118     C                   if        %found(fnblp01l)
071700180118     C                   move      blpDCR        vabDCR
071800080527     C                   endif
071900180118     C                   endif
072000080527     C*
072100080527     C                   ENDSR
072200161215
072300161215
072400161215
072500161215     C     repNOT        BEGSR
072600161215     C*
072700161215     C                   movel     *blanks       wNOTE_8          35
072800161215     C                   movel     *blanks       wNOTE_9          35
072900161215     C*
073000161215     C                   eval      ar4trc = '8'
073100161215     C     KEYta430_C    chain     fiar401l
073200161215     C                   if        %found(fiar401l)
073300161215     C                   eval      wNOTE_8 = ar4NOT
073400161215     C                   else
073500161215     C                   eval      ar4trc = '8'
073600161215     C     KEYta430_C    chain     tita430c
073700161215     C                   if        %found(tita430c)
073800161215     C                   eval      wNOTE_8 = ta4NOT
073900161215     C                   endif
074000161215     C                   endif
074100161215     C*
074200161215     C                   eval      ar4trc = '9'
074300161215     C     KEYta430_C    chain     fiar401l
074400161215     C                   if        %found(fiar401l)
074500161215     C                   eval      wNOTE_9 = ar4NOT
074600161215     C                   else
074700161215     C                   eval      ar4trc = '9'
074800161215     C     KEYta430_C    chain     tita430c
074900161215     C                   if        %found(tita430c)
075000161215     C                   eval      wNOTE_9 = ta4NOT
075100161215     C                   endif
075200161215     C                   endif
075300161215     C*
075400161215     C                   ENDSR
075500171123
075600171123     C     reppnd        BEGSR
075700171123     c                   clear                   wpndipn
075800171123     c                   clear                   warsnot_c
075900171123     C*
076000171123     C* Posizionamento diretto
076100171123     C     KEYtas30_P    chain     fipnd01l
076200171123     c                   if        %found(fipnd01l)
076300171123     c                   eval      wpndipn=pndipn
076400171123     c                   endif
076500171123     c                   move      '.'           wpndipn
076600171123     c                   ENDSR
076700161212
076800161212
076900161212     C     CARTAB        BEGSR
077000161212     C*
077100161212     C     'VPC'         SETLL     tntbe01l
077200161212     C                   IF        %equal(tntbe01l)
077300161212     C     'VPC'         READE     tntbe01l
077400161212     C                   DOW       not %eof(tntbe01l)
077500161212     C                   EVAL      DVPC = tbeUNI
077600161212     C                   IF        §VPCVTA = 'S'
077700161212     C                   ADD       1             jVPC
077800161212     C                   EVAL      skVPC(jVPC) = %dec(%subst(tbeKE1:1:7):7:0)
077900161212     C                   ENDIF
078000161212     C     'VPC'         READE     tntbe01l
078100161212     C                   ENDDO
078200161212     C                   ENDIF
078300161212     C*
078400161212     C                   ENDSR
078500010528
078600000629
078700991027
078800991027      /TITLE Operazioni iniziali.
078900991027     C     *inzsr        BEGSR
079000991027     C*
079100991027     C     *ENTRY        PLIST
079200991027     C                   parm                    prmppt
079300991027     C     wrkesito      parm      wrkesito      prmesito
079400080527     C*
079500080527     C     KEYtas30_P    KLIST
079600080527     C                   KFLD                    vabaas
079700080527     C                   KFLD                    vablnp
079800080527     C                   KFLD                    vabnrs
079900080527     C                   KFLD                    vabnsp
080000121218     C*
080100121218     C     KEYblt01_P    KLIST
080200121218     C                   KFLD                    vabaas
080300121218     C                   KFLD                    vablnp
080400121218     C                   KFLD                    vabnrs
080500121218     C                   KFLD                    vabnsp
080600121218     C*
080700121218     C     KEYars01_C    KLIST
080800121218     C                   KFLD                    bltFLS
080900121218     C                   KFLD                    bltLNA
081000121218     C                   KFLD                    bltNRS
081100121218     C                   KFLD                    bltNSC
081200121218     C                   KFLD                    arsTRC
081300161215     C*
081400161215     C     KEYta430_C    KLIST
081500161215     C                   KFLD                    vabaas
081600161215     C                   KFLD                    vablnp
081700161215     C                   KFLD                    vabnrs
081800161215     C                   KFLD                    vabnsp
081900161215     C                   KFLD                    ar4trc
082000991027     C*
082100991027     C                   ENDSR
082200161215
