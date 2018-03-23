000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100910     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500111104     FTIVGD00F  UF A E             DISK    commit prefix(O_)
000600111116     F                                     rename(tivgd000:tivgdout)
000700111104     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000800111104     F                                     PREFIX(f_)
000900030924
001000111104
001100111104     D*-----------
001200111104     D* DEFINIZIONE DS ESTERNE
001300111104     D*-----------
001400111104     D trul47ds      E DS
001500111104     D fnvac00t      e ds
001600111104
001700111104
001800111104     D*------------------
001900111104     D* RIDEFINIZIONE DS INPUT
002000111104     D*------------------
002100111118     D inKSU           s              8    inz('00434461')
002200111118     D inTIP           s              2    inz('EC')
002300111118     D inISV           s              2    inz('OB')
002400071121
002500071121
002600071121     D*------------
002700071121     D* VARIABILI D WRK
002800071121     D*------------
002900030924     D prmppt          S             50
003000030924     D prmesito        S              1
003100030924     D wrkesito        S                   like(prmesito)
003200100910     D wDataSts        S              8  0 inz(*zeros)
003300100907     D wConsOkSiNo     S              1    inz(*blanks)
003400100907     D wConsOkSiNoDes  S            300    inz(*blanks)
003500111104     D
003600111104     D wDlmT           s              1    inz('"')
003700111104     D wDlmF           s              1    inz(',')
003800071211     D*
003900100910
004000100910
004100100910     D*------------------
004200100910     D* LINKING A DEFINIZIONI ESTERNE
004300100910     D*------------------
004400100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
004500100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
004600100910
004700030924
004800030325
004900030924     C* MAIN/
005000111104     C*
005100111104     C* Avvio blocco elaborazione TIVGD
005200111104     C                   EXSR      strTRUL47
005300111104     C*
005400111104     C* Se ok a procedere => elaboro
005500111104     C                   if        wProcedi = 'S'
005600071121     C*
005700071121     C* Effettuo elaborazione
005800071121     C                   EXSR      traduci
005900111104     C*
006000111104     C                   endif
006100111104     C*
006200111104     C* Termino il blocco elaborazione TIVGD
006300111104     C                   EXSR      endTRUL47
006400071121     C*
006500921023     C                   SETON                                        LR
006600071121
006700071121
006800030924
006900991027     C     traduci       BEGSR
007000111116     C*
007100111104     C*
007200080924     C                   READ      TIVGDTMP
007300060119     C*
007400111104     C* Scorro tutto il file
007500080924     C                   DOW       not %eof(TIVGDTMP)
007600080924     C*
007700111104     C                   EVAL      fnvac00t = vgdDTA
007800111104     C*
007900111104     C* Inizializzazone formato e variabili x output
008000111104     C                   clear                   wConsOkSiNo
008100111104     C                   clear                   wConsOkSiNoDes
008200100910     C*
008300100910     C* Solo bolle originali e valide ai fini della spedizione
008400100910     C                   movel     *blanks       wEsito1           1
008500100910     C*
008600100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
008700100910     C                                                vacAAS
008800100910     C                                               :vacLNP
008900100910     C                                               :vacNRS
009000100910     C                                               :vacNSP
009100100910     C                                               :pOutLblTyp
009200100910     C                                               :pOutAnnoBO
009300100910     C                                               :pOutLineaParBO
009400100910     C                                               :pOutSerieBO
009500100910     C                                               :pOutNumSpedBO
009600100910     C                                               :pOutDcmBO
009700100910     C                                               :pOutCcaBO
009800100910     C                                               :pOutRblBO))
009900100910     C*
010000100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
010100111104     C*
010200111104     C* Record d testata
010300111104     C                   EXSR      CALPROG
010400111116     C                   CLEAR                   TIVGDOUT
010500111116     C                   EVAL      O_VGDDTA = wDlmT+'data inizio'+wDlmT+wDlmF+
010600111116     C                                        wDlmT+%char(datcor)+wDlmT
010700111104     C                   EXSR      EXEVGD
010800111116     C*
010900111116     C* Determino lo status della spedizione
011000111116     C                   EXSR      calSTS
011100111104     C*
011200111104     C* Record d corpo
011300111116     C                   CLEAR                   TIVGDOUT
011400111116     C                   EVAL      O_VGDDTA =
011500111116     C                                 wDlmT+%editc(vacAAS:'X')+
011600111116     C                                       %editc(vacLNP:'X')+
011700111116     C                                       %editc(vacNRS:'X')+
011800111116     C                                       %editc(vacNSP:'X')    +wDlmT+wDlmF+
011900111116     C                                 wDlmT+%char(wDataSts)       +wDlmT+wDlmF+
012000111116     C                                 wDlmT+%trim(wConsOkSiNoDes) +wDlmT+wDlmF+
012100111116     C                                 wDlmT+%trim(%char(vacNCL))  +wDlmT+wDlmF+
012200111116     C                                 wDlmT+%trim(vacRMA)         +wDlmT+wDlmF+
012300111116     C                                 wDlmT+'-'                   +wDlmT+wDlmF+
012400111116     C                                 wDlmT+'-'                   +wDlmT+wDlmF+
012500111118     C                                 wDlmT+%subst(%editc(vacHMC:'X'):1:2)    +
012600111118     C                                  ':' +%subst(%editc(vacHMC:'X'):3:2)    +
012700111118     C                                                              wDlmT+wDlmF+
012800111116     C                                 wDlmT+'-'                   +wDlmT
012900111104     C                   EXSR      EXEVGD
013000111104     C*
013100111104     C* Record d piede
013200111116     C                   CLEAR                   TIVGDOUT
013300111116     C                   EVAL      O_VGDDTA = wDlmT+'data fine'+wDlmT+wDlmF+
013400111116     C                                        wDlmT+%char(datcor)+wDlmT
013500111104     C                   EXSR      EXEVGD
013600100910     C*
013700100910     C                   endif
013800040719     C*
013900040719     C* In ogni caso storicizzo e cancello l'input sempre
014000080924     C                   DELETE    TIVGD000
014100040719     C*
014200040719     C* Proseguo con la lettura dell'input
014300080924     C                   READ      TIVGDTMP
014400060119     C*
014500030325     C                   ENDDO
014600991027     C*
014700030325     C                   EVAL      wrkesito = '0'
014800991027     C*
014900910830     C                   ENDSR
015000100907
015100100907
015200100907
015300100907     C     calSTS        BEGSR
015400100910     C*
015500100910     C                   EVAL      wDataSts       = vacDCM
015600100907     C*
015700100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
015800100907     C                   IF        VACDCM > *zeros AND
015900100907     C                             (VACCCA = *blanks OR VACCCA = '1')
016000100907     C                   EVAL      wConsOkSiNo    = 'S'
016100111116     C                   EVAL      wConsOkSiNoDes = '000'
016200100907     C                   ELSE
016300100907     C                   EVAL      wConsOkSiNo    = 'N'
016400100907     C*
016500100907     C* A questo punto stabilisco la descrizione
016600100910     C                   SELECT
016700100910     C                   WHEN      vacCCA = '1'
016800111116     C                   EVAL      wConsOkSiNoDes = 'SVI'
016900100910     C                   WHEN      vacCCA = '2'
017000111116     C                   EVAL      wConsOkSiNoDes = '020'
017100100907     C                   EVAL      wConsOkSiNo    = 'R'
017200100910     C                   WHEN      vacCCA = '3'
017300111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
017400100910     C                   WHEN      vacCCA = '4'
017500111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
017600100910     C                   WHEN      vacCCA = '5'
017700111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
017800100910     C                   WHEN      vacCCA = '6'
017900111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018000100910     C                   WHEN      vacCCA = 'A'
018100111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018200100910     C                   WHEN      vacCCA = 'C'
018300111116     C                   EVAL      wConsOkSiNoDes = 'GGG'
018400100910     C                   WHEN      vacCCA = 'R'
018500111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018600100910     C                   WHEN      vacCCA = 'S'
018700111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018800100910     C                   ENDSL
018900100907     C*
019000100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
019100100910     C                   EVAL      wDataSts       = vacDLA
019200111116     C                   EVAL      wConsOkSiNoDes = 'F24'
019300100907     C                   ENDIF
019400100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
019500100910     C                   EVAL      wDataSts       = vacDAG
019600111116     C                   EVAL      wConsOkSiNoDes = 'GIA'
019700100907     C                   ENDIF
019800100910     C                   IF        vacDCM = *zeros AND
019900100910     C                             vacDLA = *zeros AND
020000100910     C                             vacDAG = *zeros AND
020100100910     C                             vacCCA = *blanks
020200100910     C                   EVAL      wDataSts       = datcor
020300100910     C*
020400100910     C* Verifico la data consegna richiesta
020500100910     C                   IF        vacDCR > datcor
020600111116     C                   EVAL      wConsOkSiNoDes = '012'
020700111116     C***                EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
020800111116     C***                                           'on ' + %editc(vacDCR:'X')
020900100910     C                   ELSE
021000111116     C                   EVAL      wConsOkSiNoDes = '012'
021100100910     C                   ENDIF
021200100910     C                   ENDIF
021300100907     C*
021400100907     C                   ENDIF
021500100907     C*
021600100907     C                   ENDSR
021700071121
021800111116
021900111116
022000111116     C     EXEVGD        BEGSR
022100111116     C*
022200111116     C* Preparo la scrittura del file TIVGD
022300111118     C                   eval      o_vgdtip = inTIP
022400111118     C                   eval      o_vgdksu = inKSU
022500111116     C                   eval      o_vgdtsc = 'WW'
022600111116     C                   eval      o_vgddat = datcor
022700111116     C                   eval      o_vgdprg = f_tis7prgf
022800111116     C                   eval      o_vgdpgm = 'TITV2J0R'
022900111116     C*
023000111116     C                   write     tivgdout
023100111116     C*
023200111116     C                   ENDSR
023300111116
023400111116
023500111104
023600111104
023700111104     C     strTRUL47     BEGSR
023800111104     C*
023900111104     C                   movel     'N'           wProcedi          1
024000111104     C*
024100121017     C* Avvio il blocco elaborazione TIVGD x tipo file in questione
024200111104     C                   clear                   trul47ds
024300111104     C                   eval      d47opz  = 'I'
024400121017     C                   eval      d47tip  = inTIP
024500111104     C                   eval      d47lck  = 'N'
024600111104     C                   eval      d47chkj = 'S'
024700111104     C                   eval      d47pgm  = 'TITV2J0R'
024800111104     C                   call      'TRUL47R'
024900111104     C                   parm                    trul47ds
025000111104     C*
025100111104     C* Se elaborazione consentita => proseguo
025200111104     C                   if        d47sts <> 'A'
025300111104     C                   movel     'S'           wProcedi
025400111104     C                   endif
025500111104     C*
025600111104     C                   ENDSR
025700111104
025800111104
025900111104
026000111104     C     endTRUL47     BEGSR
026100111104     C*
026200111104     C                   if        wProcedi = 'S'
026300111104     C*
026400121017     C* Elimino il blocco elaborazione TIVGD x tipo file in questione
026500111104     C                   clear                   trul47ds
026600111104     C                   eval      d47opz  = 'F'
026700121017     C                   eval      d47tip  = inTIP
026800111104     C                   call      'TRUL47R'
026900111104     C                   parm                    trul47ds
027000111104     C*
027100111104     C                   endif
027200111104     C*
027300111104     C                   ENDSR
027400111104
027500111104
027600111104
027700111104      /TITLE Valorizzazione Progressivo Applicazione
027800111104     C     calprog       begsr
027900111104     C*
028000111104     C                   movel     *blanks       dwlisv            2
028100111104     C                   movel     *all'0'       dwlprg           10
028200111104     C                   z-add     *zeros        wrkprg            8 0
028300111104     C*
028400111116     C                   eval      dwlisv = inISV
028500111104     C*
028600170324     C     *start        setll     tis7prgf
028700111104     C                   read(e)   tis7prgf
028800111104     C                   if        not %error
028900111104     C                   eval      dwlprg = f_tis7prgf
029000111104     C*
029100111104     C                   move(p)   dwlprg        wrkprg
029200111104     C                   add       1             wrkprg
029300111104     C                   move(p)   wrkprg        dwlprg
029400111104     C                   movel     dwlisv        dwlprg
029500111104     C*
029600111104     C                   eval      f_tis7prgf = dwlprg
029700111104     C                   update    tis7prg0
029800170324     C                   else
029900170324     C                   eval      f_tis7prgf = 'XX00000000'
030000111104     C                   endif
030100111104     C*
030200111104     C                   endsr
030300071121
030400071121
030500991027
030600991027      /TITLE Operazioni iniziali.
030700991027     C     *inzsr        BEGSR
030800991027     C*
030900991027     C     *ENTRY        PLIST
031000991027     C                   parm                    prmppt
031100991027     C     wrkesito      parm      wrkesito      prmesito
031200100910     C*
031300100910     C* CALCOLA LA DATA CORRENTE
031400100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
031500100910     C                   eval      datcor = %dec(%date() : *ISO)
031600121017     C*
031700121017     C* VERIFICA PASSAGGIO PARAMETRI
031800121017     C                   if        %subst(prmppt:1:8) <> *blanks
031900121017     C                   eval      inKSU = %subst(prmppt:1:8)
032000121017     C                   endif
032100121017     C                   if        %subst(prmppt:9:2) <> *blanks
032200121017     C                   eval      inTIP = %subst(prmppt:9:2)
032300121017     C                   endif
032400121017     C                   if        %subst(prmppt:11:2) <> *blanks
032500121017     C                   eval      inISV = %subst(prmppt:11:2)
032600121017     C                   endif
032700991027     C*
032800991027     C                   ENDSR
032900071121     C*
