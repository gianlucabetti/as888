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
002200150210     D  PAR_CONS                      1
002300010605     D*--------------------
002400010605     D* VARIABILI DI WRK
002500010605     D*--------------------
002600100219     D wANT            s              7  0 inz
002700100222     D wM12            s              7  0 inz
002800100219     D wOKS            s              7  0 inz
002900150210     D wR12            s              7  0 inz
003000150210     D wR24            s              7  0 inz
003100150210     D wRIT            s              7  0 inz
003200001218
003300010605
003400010605
003500920812     C*---------------------------------------------------------------*
003600001218     C* MAIN
003700001218     C*---------------------------------------------------------------*
003800050909     C*
003900001218     C                   exsr      procedi
004000001218     C*
004100001218     C                   seton                                        LR
004200001218
004300001218
004400001218
004500001218     C*------------------------------------------------------------------------*
004600001218     C* PROCEDI - Routine principale
004700001218     C*------------------------------------------------------------------------*
004800001218     C     PROCEDI       BEGSR
004900100219     C*
005000100219     C                   movel     'N'           FINE              1
005100001218     C*
005200080213     C* Mi posiziono a inizio file bolle (TITAS) per cliente
005300100219     C                   eval      tasCCM = PAR_CCM
005400100222     C                   movel     PAR_AMGDA     tasAAS
005500100222     C                   move      PAR_AMGDA     tasMGS
005600100219     C     KEYtas33_C    setll     titas33c
005700100219     C     KEYtas33_P    reade     titas33c
005800080213     C*
005900100219     C                   dow       not %eof(titas33c) AND
006000100219     C                             FINE <> 'S'
006100080213     C                   exsr      verifica
006200080213     C                   if        CHKREC = 'S'
006300100219     C                   exsr      scriviWF
006400080213     C                   endif
006500100219     C     KEYtas33_P    reade     titas33c
006600080213     C                   enddo
006700001218     C*
006800001218     C                   ENDSR
006900001218     C*------------------------------------------------------------------------*
007000001218
007100100219
007200001218
007300001218     C*------------------------------------------------------------------------*
007400001218     C* VERIFICA - Routine di verifica validità record corrente
007500001218     C*------------------------------------------------------------------------*
007600001218     C     VERIFICA      BEGSR
007700001218     C*
007800001218     C                   movel     'S'           CHKREC            1
007900100219     C*
008000100219     C                   movel     tasaas        wrkdata8          8 0
008100100219     C                   move      tasmgs        wrkdata8
008200100219     C                   movel     wrkdata8      wrkAAAAMM         6 0
008300100219     C*
008400100219     C* Verifico il codice cliente
008500100219     C                   if        CHKREC = 'S'
008600100219     C                   if        PAR_CCM <> *zeros
008700100219     C                   if        tasccm <> PAR_CCM
008800100219     C                   movel     'N'           CHKREC
008900100219     C                   endif
009000100219     C                   endif
009100100219     C                   endif
009200100219     C*
009300100219     C* Verifico la serie
009400100219     C                   if        CHKREC = 'S'
009500100219     C                   if        PAR_NRS <> *zeros
009600100219     C                   if        tasnrs <> PAR_NRS
009700100219     C                   movel     'N'           CHKREC
009800100219     C                   endif
009900100219     C                   endif
010000100219     C                   endif
010100150210     C*
010200150210     C* Verifico se consegnata
010300150210     C                   if        CHKREC = 'S'
010400150210     C                   if        PAR_CONS = 'N'
010500150210     C                   if        tasdcm  = *zeros
010600150210     C                   movel     'N'           CHKREC
010700150210     C                   endif
010800150210     C                   endif
010900150210     C                   endif
011000100219     C*
011100100219     C* Verifico il tipo bolla
011200100219     C                   if        CHKREC = 'S'
011300100219     C                   if        PAR_TBL <> *blanks
011400100219     C                   if        tastbl <> PAR_TBL
011500100219     C                   movel     'N'           CHKREC
011600100219     C                   endif
011700100219     C                   endif
011800100219     C                   endif
011900001218     C*
012000100219     C* Verifico data DA
012100001218     C                   if        CHKREC = 'S'
012200100222     C                   if        PAR_AMGDA <> *zeros
012300100222     C                   if        wrkdata8  <  PAR_AMGDA
012400100219     C                   movel     'N'           CHKREC
012500100219     C                   endif
012600001218     C                   endif
012700001218     C                   endif
012800100219     C*
012900100219     C* Verifico data A
013000100219     C                   if        CHKREC = 'S'
013100100222     C                   if        PAR_AMGA <> *zeros
013200100222     C                   if        wrkdata8  > PAR_AMGA
013300100219     C                   movel     'N'           CHKREC
013400100219     C                   movel     'S'           FINE
013500100219     C                   endif
013600100219     C                   endif
013700100219     C                   endif
013800001218     C*
013900001218     C                   ENDSR
014000001218     C*------------------------------------------------------------------------*
014100001218
014200001218
014300001218
014400001218     C*------------------------------------------------------------------------*
014500100219     C* SCRIVIWF - Routine di scrittura WFCLN00F
014600001218     C*------------------------------------------------------------------------*
014700100219     C     SCRIVIWF      BEGSR
014800100219     C*
014900100219     C                   clear                   WFCLN000
015000100219     C*
015100100219     C                   eval      dlyLNP = tasLNP
015200100219     C                   eval      dlyTSP = tasTSP
015300100219     C                   eval      dlyNAR = tasNZD
015400100219     C                   eval      dlyCAP = tasCAD
015500100219     C     KEYdly05_P    chain     wadly05l
015600100219     C                   if        %found(wadly05l)
015700100219     C                   eval      WCLNDLYTRC = DLYTRC
015800100219     C                   endif
015900100219     C*
016000100219     C                   eval      tblcod = 'PR'
016100100219     C                   eval      tblkey = tasprd
016200100219     C     KEYtab_C      chain     tabel00f
016300100219     C                   if        %found(tabel00f)
016400100219     C                   eval      DSPR = tbluni
016500100219     C                   eval      tblcod = '14'
016600100219     C                   eval      tblkey = §PRCRE
016700100219     C     KEYtab_C      chain     tabel00f
016800100219     C                   if        %found(tabel00f)
016900100219     C                   eval      DS14 = tbluni
017000100219     C                   eval      WCLNREG = §14DES
017100100219     C                   endif
017200100219     C                   endif
017300100219     C*
017400100219     C                   z-add     *zeros        wANT
017500100222     C                   z-add     *zeros        wM12
017600100219     C                   z-add     *zeros        wOKS
017700150210     C                   z-add     *zeros        wR12
017800150210     C                   z-add     *zeros        wR24
017900100219     C                   z-add     *zeros        wRIT
018000001218     C*
018100100219     C* ...in anticipo
018200100219     C                   select
018300100219     C                   when      tasnrc >= -999 AND
018400100219     C                             tasnrc <= -24
018500100219     C                   add       1             wANT
018600100222     C*
018700100222     C* ...border-line
018800100222     C                   when      tasnrc >  -24  AND
018900100222     C                             tasnrc <= -12
019000100222     C                   add       1             wM12
019100100219     C*
019200100219     C* ...in orario
019300100222     C                   when      tasnrc >  -12  AND
019400100219     C                             tasnrc <=   0
019500100219     C                   add       1             wOKS
019600150210     C*
019700150210     C* ...in ritardo entro 12 ore
019800150210     C                   when      tasnrc >    0  AND
019900150210     C                             tasnrc <=  12
020000150210     C                   add       1             wR12
020100150210     C*
020200150210     C* ...in ritardo entro 24 ore
020300150210     C                   when      tasnrc >   12  AND
020400150210     C                             tasnrc <=  24
020500150210     C                   add       1             wR24
020600100219     C*
020700150210     C* ...in ritardo "molto"
020800150210     C                   when      tasnrc >   24
020900100219     C                   add       1             wRIT
021000100219     C                   endsl
021100100219     C*
021200100219     C* Conteggio spedizioni DIR/GIAC/CCA
021300100219     C                   eval      dtasflo = tasflo
021400100219     C                   if        tasCCA   <> *blanks OR
021500100219     C                             tasFGC   <> *blanks OR
021600100219     C                             §FLONAV  <> *blanks
021700100219     C                   eval      wclnANOM = 'S'
021800100219     C                   endif
021900100219     C*
022000100219     C                   eval      wclnCCM    = tasccm
022100100219     C                   eval      wclnAAAAMM = wrkAAAAMM
022200100219     C                   eval      wclnPRD    = tasprd
022300100219     C                   eval      wclnCAD    = tascad
022400100219     C                   eval      wclnISO    = tasfin
022500100219     C     KEYcln_C      chain     wfcln00f
022600100219     C                   if        %found(wfcln00f)
022700100219     C                   add       wANT          wclnANT
022800100222     C                   add       wM12          wclnM12
022900100219     C                   add       wOKS          wclnOKS
023000150210     C                   add       wR12          wclnR12
023100150210     C                   add       wR24          wclnR24
023200150210     C                   add       wRIT          wclnRIT
023300100219     C                   add       1             wclnTOT
023400100219     C                   update    WFCLN000
023500100219     C                   else
023600100219     C                   add       wANT          wclnANT
023700100222     C                   add       wM12          wclnM12
023800100219     C                   add       wOKS          wclnOKS
023900150210     C                   add       wR12          wclnR12
024000150210     C                   add       wR24          wclnR24
024100150210     C                   add       wRIT          wclnRIT
024200100219     C                   add       1             wclnTOT
024300100219     C                   write     WFCLN000
024400100219     C                   endif
024500001218     C*
024600001218     C                   ENDSR
024700001221     C*------------------------------------------------------------------------*
024800050825
024900001218
025000001218
025100001218     C*------------------------------------------------------------------------*
025200001218     C* *INZSR - ROUTINE INIZIALE
025300001218     C*------------------------------------------------------------------------*
025400001218     C     *INZSR        BEGSR
025500001218     C*
025600001218     C     *ENTRY        PLIST
025700100219     C                   PARM                    KPJBA
025800001218     C*
025900100219     C                   MOVEL     KPJBU         DSINPUT
026000100219     C                   EVAL      tblKUT = 1
026100001218     C*
026200001218     C* Definizioni chiavi
026300100219     C     KEYtas33_C    KLIST
026400100219     C                   KFLD                    tasCCM
026500100219     C                   KFLD                    tasAAS
026600100219     C                   KFLD                    tasMGS
026700100219     C*
026800100219     C     KEYtas33_P    KLIST
026900100219     C                   KFLD                    tasCCM
027000100219     C                   KFLD                    tasAAS
027100100219     C*
027200100219     C     KEYdly05_P    KLIST
027300100219     C                   KFLD                    dlyLNP
027400100219     C                   KFLD                    dlyTSP
027500100219     C                   KFLD                    dlyNAR
027600100219     C                   KFLD                    dlyCAP
027700001218     C*
027800100219     C     KEYtab_C      KLIST
027900100219     C                   KFLD                    tblKUT
028000100219     C                   KFLD                    tblCOD
028100100219     C                   KFLD                    tblKEY
028200030704     C*
028300100219     C     KEYcln_C      KLIST
028400100219     C                   KFLD                    wclnCCM
028500100219     C                   KFLD                    wclnAAAAMM
028600100219     C                   KFLD                    wclnPRD
028700100219     C                   KFLD                    wclnCAD
028800100219     C                   KFLD                    wclnISO
028900100219     C                   KFLD                    wclnANOM
029000100219     C*
029100001218     C                   ENDSR
