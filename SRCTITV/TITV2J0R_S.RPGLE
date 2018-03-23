000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100910     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500111104     FTIVGD00F  UF A E             DISK    commit prefix(O_)
000600111116     F                                     rename(tivgd000:tivgdout)
000700111104     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000800111104     F                                     PREFIX(f_)
000900111104     F                                     USROPN
001000030924
001100111104
001200111104     D*-----------
001300111104     D* DEFINIZIONE DS ESTERNE
001400111104     D*-----------
001500111104     D trul47ds      E DS
001600111104     D fnvac00t      e ds
001700111104
001800111104
001900111104     D*------------------
002000111104     D* RIDEFINIZIONE DS INPUT
002100111104     D*------------------
002200111118     D inKSU           s              8    inz('00434461')
002300111118     D inTIP           s              2    inz('EC')
002400111118     D inISV           s              2    inz('OB')
002500071121
002600071121
002700071121     D*------------
002800071121     D* VARIABILI D WRK
002900071121     D*------------
003000030924     D prmppt          S             50
003100030924     D prmesito        S              1
003200030924     D wrkesito        S                   like(prmesito)
003300100910     D wDataSts        S              8  0 inz(*zeros)
003400100907     D wConsOkSiNo     S              1    inz(*blanks)
003500100907     D wConsOkSiNoDes  S            300    inz(*blanks)
003600111104     D
003700111104     D wDlmT           s              1    inz('"')
003800111104     D wDlmF           s              1    inz(',')
003900071211     D*
004000100910
004100100910
004200100910     D*------------------
004300100910     D* LINKING A DEFINIZIONI ESTERNE
004400100910     D*------------------
004500100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
004600100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
004700100910
004800030924
004900030325
005000030924     C* MAIN/
005100111104     C*
005200111104     C* Avvio blocco elaborazione TIVGD
005300111104     C                   EXSR      strTRUL47
005400111104     C*
005500111104     C* Se ok a procedere => elaboro
005600111104     C                   if        wProcedi = 'S'
005700071121     C*
005800071121     C* Effettuo elaborazione
005900071121     C                   EXSR      traduci
006000111104     C*
006100111104     C                   endif
006200111104     C*
006300111104     C* Termino il blocco elaborazione TIVGD
006400111104     C                   EXSR      endTRUL47
006500071121     C*
006600921023     C                   SETON                                        LR
006700071121
006800071121
006900030924
007000991027     C     traduci       BEGSR
007100111116     C*
007200111104     C*
007300080924     C                   READ      TIVGDTMP
007400060119     C*
007500111104     C* Scorro tutto il file
007600080924     C                   DOW       not %eof(TIVGDTMP)
007700080924     C*
007800111104     C                   EVAL      fnvac00t = vgdDTA
007900111104     C*
008000111104     C* Inizializzazone formato e variabili x output
008100111104     C                   clear                   wConsOkSiNo
008200111104     C                   clear                   wConsOkSiNoDes
008300100910     C*
008400100910     C* Solo bolle originali e valide ai fini della spedizione
008500100910     C                   movel     *blanks       wEsito1           1
008600100910     C*
008700100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
008800100910     C                                                vacAAS
008900100910     C                                               :vacLNP
009000100910     C                                               :vacNRS
009100100910     C                                               :vacNSP
009200100910     C                                               :pOutLblTyp
009300100910     C                                               :pOutAnnoBO
009400100910     C                                               :pOutLineaParBO
009500100910     C                                               :pOutSerieBO
009600100910     C                                               :pOutNumSpedBO
009700100910     C                                               :pOutDcmBO
009800100910     C                                               :pOutCcaBO
009900100910     C                                               :pOutRblBO))
010000100910     C*
010100100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
010200111104     C*
010300111104     C* Record d testata
010400111104     C                   EXSR      CALPROG
010500111116     C                   CLEAR                   TIVGDOUT
010600111116     C                   EVAL      O_VGDDTA = wDlmT+'data inizio'+wDlmT+wDlmF+
010700111116     C                                        wDlmT+%char(datcor)+wDlmT
010800111104     C                   EXSR      EXEVGD
010900111116     C*
011000111116     C* Determino lo status della spedizione
011100111116     C                   EXSR      calSTS
011200111104     C*
011300111104     C* Record d corpo
011400111116     C                   CLEAR                   TIVGDOUT
011500111116     C                   EVAL      O_VGDDTA =
011600111116     C                                 wDlmT+%editc(vacAAS:'X')+
011700111116     C                                       %editc(vacLNP:'X')+
011800111116     C                                       %editc(vacNRS:'X')+
011900111116     C                                       %editc(vacNSP:'X')    +wDlmT+wDlmF+
012000111116     C                                 wDlmT+%char(wDataSts)       +wDlmT+wDlmF+
012100111116     C                                 wDlmT+%trim(wConsOkSiNoDes) +wDlmT+wDlmF+
012200111116     C                                 wDlmT+%trim(%char(vacNCL))  +wDlmT+wDlmF+
012300111116     C                                 wDlmT+%trim(vacRMA)         +wDlmT+wDlmF+
012400111116     C                                 wDlmT+'-'                   +wDlmT+wDlmF+
012500111116     C                                 wDlmT+'-'                   +wDlmT+wDlmF+
012600111118     C                                 wDlmT+%subst(%editc(vacHMC:'X'):1:2)    +
012700111118     C                                  ':' +%subst(%editc(vacHMC:'X'):3:2)    +
012800111118     C                                                              wDlmT+wDlmF+
012900111116     C                                 wDlmT+'-'                   +wDlmT
013000111104     C                   EXSR      EXEVGD
013100111104     C*
013200111104     C* Record d piede
013300111116     C                   CLEAR                   TIVGDOUT
013400111116     C                   EVAL      O_VGDDTA = wDlmT+'data fine'+wDlmT+wDlmF+
013500111116     C                                        wDlmT+%char(datcor)+wDlmT
013600111104     C                   EXSR      EXEVGD
013700100910     C*
013800100910     C                   endif
013900040719     C*
014000040719     C* In ogni caso storicizzo e cancello l'input sempre
014100080924     C                   DELETE    TIVGD000
014200040719     C*
014300040719     C* Proseguo con la lettura dell'input
014400080924     C                   READ      TIVGDTMP
014500060119     C*
014600030325     C                   ENDDO
014700991027     C*
014800030325     C                   EVAL      wrkesito = '0'
014900991027     C*
015000910830     C                   ENDSR
015100100907
015200100907
015300100907
015400100907     C     calSTS        BEGSR
015500100910     C*
015600100910     C                   EVAL      wDataSts       = vacDCM
015700100907     C*
015800100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
015900100907     C                   IF        VACDCM > *zeros AND
016000100907     C                             (VACCCA = *blanks OR VACCCA = '1')
016100100907     C                   EVAL      wConsOkSiNo    = 'S'
016200111116     C                   EVAL      wConsOkSiNoDes = '000'
016300100907     C                   ELSE
016400100907     C                   EVAL      wConsOkSiNo    = 'N'
016500100907     C*
016600100907     C* A questo punto stabilisco la descrizione
016700100910     C                   SELECT
016800100910     C                   WHEN      vacCCA = '1'
016900111116     C                   EVAL      wConsOkSiNoDes = 'SVI'
017000100910     C                   WHEN      vacCCA = '2'
017100111116     C                   EVAL      wConsOkSiNoDes = '020'
017200100907     C                   EVAL      wConsOkSiNo    = 'R'
017300100910     C                   WHEN      vacCCA = '3'
017400111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
017500100910     C                   WHEN      vacCCA = '4'
017600111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
017700100910     C                   WHEN      vacCCA = '5'
017800111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
017900100910     C                   WHEN      vacCCA = '6'
018000111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018100100910     C                   WHEN      vacCCA = 'A'
018200111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018300100910     C                   WHEN      vacCCA = 'C'
018400111116     C                   EVAL      wConsOkSiNoDes = 'GGG'
018500100910     C                   WHEN      vacCCA = 'R'
018600111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018700100910     C                   WHEN      vacCCA = 'S'
018800111116     C                   EVAL      wConsOkSiNoDes = 'SMA'
018900100910     C                   ENDSL
019000100907     C*
019100100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
019200100910     C                   EVAL      wDataSts       = vacDLA
019300111116     C                   EVAL      wConsOkSiNoDes = 'F24'
019400100907     C                   ENDIF
019500100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
019600100910     C                   EVAL      wDataSts       = vacDAG
019700111116     C                   EVAL      wConsOkSiNoDes = 'GIA'
019800100907     C                   ENDIF
019900100910     C                   IF        vacDCM = *zeros AND
020000100910     C                             vacDLA = *zeros AND
020100100910     C                             vacDAG = *zeros AND
020200100910     C                             vacCCA = *blanks
020300100910     C                   EVAL      wDataSts       = datcor
020400100910     C*
020500100910     C* Verifico la data consegna richiesta
020600100910     C                   IF        vacDCR > datcor
020700111116     C                   EVAL      wConsOkSiNoDes = '012'
020800111116     C***                EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
020900111116     C***                                           'on ' + %editc(vacDCR:'X')
021000100910     C                   ELSE
021100111116     C                   EVAL      wConsOkSiNoDes = '012'
021200100910     C                   ENDIF
021300100910     C                   ENDIF
021400100907     C*
021500100907     C                   ENDIF
021600100907     C*
021700100907     C                   ENDSR
021800071121
021900111116
022000111116
022100111116     C     EXEVGD        BEGSR
022200111116     C*
022300111116     C* Preparo la scrittura del file TIVGD
022400111118     C                   eval      o_vgdtip = inTIP
022500111118     C                   eval      o_vgdksu = inKSU
022600111116     C                   eval      o_vgdtsc = 'WW'
022700111116     C                   eval      o_vgddat = datcor
022800111116     C                   eval      o_vgdprg = f_tis7prgf
022900111116     C                   eval      o_vgdpgm = 'TITV2J0R'
023000111116     C*
023100111116     C                   write     tivgdout
023200111116     C*
023300111116     C                   ENDSR
023400111116
023500111116
023600111104
023700111104
023800111104     C     strTRUL47     BEGSR
023900111104     C*
024000111104     C                   movel     'N'           wProcedi          1
024100111104     C*
024200111118     C* Avvio il blocco elaborazione TIVGD x tipo file: 'EC'
024300111104     C                   clear                   trul47ds
024400111104     C                   eval      d47opz  = 'I'
024500111118     C                   eval      d47tip  = 'EC'
024600111104     C                   eval      d47lck  = 'N'
024700111104     C                   eval      d47chkj = 'S'
024800111104     C                   eval      d47pgm  = 'TITV2J0R'
024900111104     C                   call      'TRUL47R'
025000111104     C                   parm                    trul47ds
025100111104     C*
025200111104     C* Se elaborazione consentita => proseguo
025300111104     C                   if        d47sts <> 'A'
025400111104     C                   movel     'S'           wProcedi
025500111104     C                   endif
025600111104     C*
025700111104     C                   ENDSR
025800111104
025900111104
026000111104
026100111104     C     endTRUL47     BEGSR
026200111104     C*
026300111104     C                   if        wProcedi = 'S'
026400111104     C*
026500111118     C* Elimino il blocco elaborazione TIVGD x tipo file: 'EC'
026600111104     C                   clear                   trul47ds
026700111104     C                   eval      d47opz  = 'F'
026800111118     C                   eval      d47tip  = 'EC'
026900111104     C                   call      'TRUL47R'
027000111104     C                   parm                    trul47ds
027100111104     C*
027200111104     C                   endif
027300111104     C*
027400111104     C                   ENDSR
027500111104
027600111104
027700111104
027800111104      /TITLE Valorizzazione Progressivo Applicazione
027900111104     C     calprog       begsr
028000111104     C*
028100111104     C                   movel     *blanks       dwlisv            2
028200111104     C                   movel     *all'0'       dwlprg           10
028300111104     C                   z-add     *zeros        wrkprg            8 0
028400111104     C*
028500111116     C                   eval      dwlisv = inISV
028600111104     C*
028700111104     C                   open      tis7prgf
028800111104     C*
028900111104     C                   read(e)   tis7prgf
029000111104     C                   if        not %error
029100111104     C                   eval      dwlprg = f_tis7prgf
029200111104     C*
029300111104     C                   move(p)   dwlprg        wrkprg
029400111104     C                   add       1             wrkprg
029500111104     C                   move(p)   wrkprg        dwlprg
029600111104     C                   movel     dwlisv        dwlprg
029700111104     C*
029800111104     C                   eval      f_tis7prgf = dwlprg
029900111104     C                   update    tis7prg0
030000111104     C                   endif
030100111104     C*
030200111104     C                   close     tis7prgf
030300111104     C*
030400111104     C                   endsr
030500071121
030600071121
030700991027
030800991027      /TITLE Operazioni iniziali.
030900991027     C     *inzsr        BEGSR
031000991027     C*
031100991027     C     *ENTRY        PLIST
031200991027     C                   parm                    prmppt
031300991027     C     wrkesito      parm      wrkesito      prmesito
031400100910     C*
031500100910     C* CALCOLA LA DATA CORRENTE
031600100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
031700100910     C                   eval      datcor = %dec(%date() : *ISO)
031800991027     C*
031900991027     C                   ENDSR
032000071121     C*
