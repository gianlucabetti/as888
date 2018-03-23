000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200001218     FTABEL00F  IF   E           K DISK
000300100219     FWADLY05L  IF   E           K DISK
000400080213     FTITAS33C  IF   E           K DISK
000500100219     FWFCLN00F  UF A E           K DISK
000600001218     D*--------------------
000700001218     D* DS ESTERNE
000800001218     D*--------------------
000900900517     D KPJBA         E DS
001000100219     D DTASFLO       E DS
001100100219     D DSPR          E DS
001200100219     D DS14          E DS
001300001218     D*--------------------
001400001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
001500001218     D*--------------------
001600050825     D DSINPUT         DS
001700100219     D  PAR_CCM                       7  0
001800100219     D  PAR_NRS                       2  0
001900100219     D  PAR_TBL                       2
002000100222     D  PAR_AMGDA                     8  0
002100100222     D  PAR_AMGA                      8  0
002200010605     D*--------------------
002300010605     D* VARIABILI DI WRK
002400010605     D*--------------------
002500100219     D wANT            s              7  0 inz
002600100222     D wM12            s              7  0 inz
002700100219     D wOKS            s              7  0 inz
002800100219     D wRIT            s              7  0 inz
002900001218
003000010605
003100010605
003200920812     C*---------------------------------------------------------------*
003300001218     C* MAIN
003400001218     C*---------------------------------------------------------------*
003500050909     C*
003600001218     C                   exsr      procedi
003700001218     C*
003800001218     C                   seton                                        LR
003900001218
004000001218
004100001218
004200001218     C*------------------------------------------------------------------------*
004300001218     C* PROCEDI - Routine principale
004400001218     C*------------------------------------------------------------------------*
004500001218     C     PROCEDI       BEGSR
004600100219     C*
004700100219     C                   movel     'N'           FINE              1
004800001218     C*
004900080213     C* Mi posiziono a inizio file bolle (TITAS) per cliente
005000100219     C                   eval      tasCCM = PAR_CCM
005100100222     C                   movel     PAR_AMGDA     tasAAS
005200100222     C                   move      PAR_AMGDA     tasMGS
005300100219     C     KEYtas33_C    setll     titas33c
005400100219     C     KEYtas33_P    reade     titas33c
005500080213     C*
005600100219     C                   dow       not %eof(titas33c) AND
005700100219     C                             FINE <> 'S'
005800080213     C                   exsr      verifica
005900080213     C                   if        CHKREC = 'S'
006000100219     C                   exsr      scriviWF
006100080213     C                   endif
006200100219     C     KEYtas33_P    reade     titas33c
006300080213     C                   enddo
006400001218     C*
006500001218     C                   ENDSR
006600001218     C*------------------------------------------------------------------------*
006700001218
006800100219
006900001218
007000001218     C*------------------------------------------------------------------------*
007100001218     C* VERIFICA - Routine di verifica validità record corrente
007200001218     C*------------------------------------------------------------------------*
007300001218     C     VERIFICA      BEGSR
007400001218     C*
007500001218     C                   movel     'S'           CHKREC            1
007600100219     C*
007700100219     C                   movel     tasaas        wrkdata8          8 0
007800100219     C                   move      tasmgs        wrkdata8
007900100219     C                   movel     wrkdata8      wrkAAAAMM         6 0
008000100219     C*
008100100219     C* Verifico il codice cliente
008200100219     C                   if        CHKREC = 'S'
008300100219     C                   if        PAR_CCM <> *zeros
008400100219     C                   if        tasccm <> PAR_CCM
008500100219     C                   movel     'N'           CHKREC
008600100219     C                   endif
008700100219     C                   endif
008800100219     C                   endif
008900100219     C*
009000100219     C* Verifico la serie
009100100219     C                   if        CHKREC = 'S'
009200100219     C                   if        PAR_NRS <> *zeros
009300100219     C                   if        tasnrs <> PAR_NRS
009400100219     C                   movel     'N'           CHKREC
009500100219     C                   endif
009600100219     C                   endif
009700100219     C                   endif
009800100219     C*
009900100219     C* Verifico il tipo bolla
010000100219     C                   if        CHKREC = 'S'
010100100219     C                   if        PAR_TBL <> *blanks
010200100219     C                   if        tastbl <> PAR_TBL
010300100219     C                   movel     'N'           CHKREC
010400100219     C                   endif
010500100219     C                   endif
010600100219     C                   endif
010700001218     C*
010800100219     C* Verifico data DA
010900001218     C                   if        CHKREC = 'S'
011000100222     C                   if        PAR_AMGDA <> *zeros
011100100222     C                   if        wrkdata8  <  PAR_AMGDA
011200100219     C                   movel     'N'           CHKREC
011300100219     C                   endif
011400001218     C                   endif
011500001218     C                   endif
011600100219     C*
011700100219     C* Verifico data A
011800100219     C                   if        CHKREC = 'S'
011900100222     C                   if        PAR_AMGA <> *zeros
012000100222     C                   if        wrkdata8  > PAR_AMGA
012100100219     C                   movel     'N'           CHKREC
012200100219     C                   movel     'S'           FINE
012300100219     C                   endif
012400100219     C                   endif
012500100219     C                   endif
012600001218     C*
012700001218     C                   ENDSR
012800001218     C*------------------------------------------------------------------------*
012900001218
013000001218
013100001218
013200001218     C*------------------------------------------------------------------------*
013300100219     C* SCRIVIWF - Routine di scrittura WFCLN00F
013400001218     C*------------------------------------------------------------------------*
013500100219     C     SCRIVIWF      BEGSR
013600100219     C*
013700100219     C                   clear                   WFCLN000
013800100219     C*
013900100219     C                   eval      dlyLNP = tasLNP
014000100219     C                   eval      dlyTSP = tasTSP
014100100219     C                   eval      dlyNAR = tasNZD
014200100219     C                   eval      dlyCAP = tasCAD
014300100219     C     KEYdly05_P    chain     wadly05l
014400100219     C                   if        %found(wadly05l)
014500100219     C                   eval      WCLNDLYTRC = DLYTRC
014600100219     C                   endif
014700100219     C*
014800100219     C                   eval      tblcod = 'PR'
014900100219     C                   eval      tblkey = tasprd
015000100219     C     KEYtab_C      chain     tabel00f
015100100219     C                   if        %found(tabel00f)
015200100219     C                   eval      DSPR = tbluni
015300100219     C                   eval      tblcod = '14'
015400100219     C                   eval      tblkey = §PRCRE
015500100219     C     KEYtab_C      chain     tabel00f
015600100219     C                   if        %found(tabel00f)
015700100219     C                   eval      DS14 = tbluni
015800100219     C                   eval      WCLNREG = §14DES
015900100219     C                   endif
016000100219     C                   endif
016100100219     C*
016200100219     C                   z-add     *zeros        wANT
016300100222     C                   z-add     *zeros        wM12
016400100219     C                   z-add     *zeros        wOKS
016500100219     C                   z-add     *zeros        wRIT
016600001218     C*
016700100219     C* ...in anticipo
016800100219     C                   select
016900100219     C                   when      tasnrc >= -999 AND
017000100219     C                             tasnrc <= -24
017100100219     C                   add       1             wANT
017200100222     C*
017300100222     C* ...border-line
017400100222     C                   when      tasnrc >  -24  AND
017500100222     C                             tasnrc <= -12
017600100222     C                   add       1             wM12
017700100219     C*
017800100219     C* ...in orario
017900100222     C                   when      tasnrc >  -12  AND
018000100219     C                             tasnrc <=   0
018100100219     C                   add       1             wOKS
018200100219     C*
018300100219     C* ...in ritardo
018400100219     C                   when      tasnrc >    0
018500100219     C                   add       1             wRIT
018600100219     C                   endsl
018700100219     C*
018800100219     C* Conteggio spedizioni DIR/GIAC/CCA
018900100219     C                   eval      dtasflo = tasflo
019000100219     C                   if        tasCCA   <> *blanks OR
019100100219     C                             tasFGC   <> *blanks OR
019200100219     C                             §FLONAV  <> *blanks
019300100219     C                   eval      wclnANOM = 'S'
019400100219     C                   endif
019500100219     C*
019600100219     C                   eval      wclnCCM    = tasccm
019700100219     C                   eval      wclnAAAAMM = wrkAAAAMM
019800100219     C                   eval      wclnPRD    = tasprd
019900100219     C                   eval      wclnCAD    = tascad
020000100219     C                   eval      wclnISO    = tasfin
020100100219     C     KEYcln_C      chain     wfcln00f
020200100219     C                   if        %found(wfcln00f)
020300100219     C                   add       wANT          wclnANT
020400100222     C                   add       wM12          wclnM12
020500100219     C                   add       wOKS          wclnOKS
020600100219     C                   add       wRIT          wclnRIT
020700100219     C                   add       1             wclnTOT
020800100219     C                   update    WFCLN000
020900100219     C                   else
021000100219     C                   add       wANT          wclnANT
021100100222     C                   add       wM12          wclnM12
021200100219     C                   add       wOKS          wclnOKS
021300100219     C                   add       wRIT          wclnRIT
021400100219     C                   add       1             wclnTOT
021500100219     C                   write     WFCLN000
021600100219     C                   endif
021700001218     C*
021800001218     C                   ENDSR
021900001221     C*------------------------------------------------------------------------*
022000050825
022100001218
022200001218
022300001218     C*------------------------------------------------------------------------*
022400001218     C* *INZSR - ROUTINE INIZIALE
022500001218     C*------------------------------------------------------------------------*
022600001218     C     *INZSR        BEGSR
022700001218     C*
022800001218     C     *ENTRY        PLIST
022900100219     C                   PARM                    KPJBA
023000001218     C*
023100100219     C                   MOVEL     KPJBU         DSINPUT
023200100219     C                   EVAL      tblKUT = 1
023300001218     C*
023400001218     C* Definizioni chiavi
023500100219     C     KEYtas33_C    KLIST
023600100219     C                   KFLD                    tasCCM
023700100219     C                   KFLD                    tasAAS
023800100219     C                   KFLD                    tasMGS
023900100219     C*
024000100219     C     KEYtas33_P    KLIST
024100100219     C                   KFLD                    tasCCM
024200100219     C                   KFLD                    tasAAS
024300100219     C*
024400100219     C     KEYdly05_P    KLIST
024500100219     C                   KFLD                    dlyLNP
024600100219     C                   KFLD                    dlyTSP
024700100219     C                   KFLD                    dlyNAR
024800100219     C                   KFLD                    dlyCAP
024900001218     C*
025000100219     C     KEYtab_C      KLIST
025100100219     C                   KFLD                    tblKUT
025200100219     C                   KFLD                    tblCOD
025300100219     C                   KFLD                    tblKEY
025400030704     C*
025500100219     C     KEYcln_C      KLIST
025600100219     C                   KFLD                    wclnCCM
025700100219     C                   KFLD                    wclnAAAAMM
025800100219     C                   KFLD                    wclnPRD
025900100219     C                   KFLD                    wclnCAD
026000100219     C                   KFLD                    wclnISO
026100100219     C                   KFLD                    wclnANOM
026200100219     C*
026300001218     C                   ENDSR
