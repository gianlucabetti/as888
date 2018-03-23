000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300020613     H* STATISTICA EVENTI CLIENTE WIND
000400010509     H*-------------------------------------------------------------------------*
000500050629     FTNTBE01L  UF   E           K DISK
000600050629     FTABEL00F  IF   E           K DISK
000700001218     D*--------------------
000800001218     D* DS ESTERNE
000900001218     D*--------------------
001000900517     D KPJBA         E DS
001100050629     D DSDD          E DS
001200050629     D DS$3          E DS
001300050629     D TRUL52DS      E DS
001400050629     D*------------------
001500050629     D* DS "XSRDA8" - CONTROLLA DATA (8)
001600050629     D*------------------
001700050629     D WLBDA8          DS                  INZ
001800050629     D  G08DAT                 1      8  0
001900050629     D  G08INV                 9     16  0
002000050629     D  G08ERR                17     17
002100050629     D  G08TGI                18     22  0
002200050629     D*-------------------
002300050706     D* DS PARAMETRI DI LANCIO X 1° PGM: SISTL
002400050629     D*-------------------
002500050629     D PARAM_1         DS
002600050629     D  PARDAI_1               1      8  0
002700050629     D  PARDAF_1               9     16  0
002800050629     D  PARSNR_1              17     17
002900050629     D  PARDIV_1              18     18
003000050629     D  PARARE_1              19     21  0
003100050629     D  PARFIL_1              22     24  0
003200050629     D  PARKSC_1              25     31  0
003300050629     D  PARDET_1              32     32
003400050629     D  PARGIO_1              33     35  0
003500060216     D  PARDEL_1              36     43  0
003600050629     D*-------------------
003700050629     D* DS PARAMETRI DI LANCIO X 2° PGM: WFSUV
003800050629     D*-------------------
003900050629     D PARAM_2         DS
004000050629     D  PARDAI_2                      6  0
004100050629     D  PARDAF_2                      6  0
004200010606     C*---------------------------------------------------------------*
004300001218
004400001218
004500001218
004600920812     C*---------------------------------------------------------------*
004700001218     C* MAIN
004800001218     C*---------------------------------------------------------------*
004900050629     C                   exsr      lettbe
005000050629     C                   exsr      caldat
005100001218     C                   exsr      procedi
005200050629     C                   exsr      aggtbe
005300001218     C*
005400001218     C                   seton                                        LR
005500010509     C*---------------------------------------------------------------*
005600050629
005700050629
005800050629
005900050629
006000050629     C*------------------------------------------------------------------------*
006100050629     C* LETTBE - Routine di reperimento dati tabellati
006200050629     C*------------------------------------------------------------------------*
006300050629     C     LETTBE        BEGSR
006400050629     C*
006500050629     C                   clear                   DSDD
006600050629     C*
006700050629     C                   movel(p)  'SDD'         tbeCOD
006800050629     C                   movel(p)  '1'           tbeKE1
006900050629     C     KEYTBE01_P    chain     tntbe01l
007000050629     C                   if        %found(tntbe01l)
007100050629     C                   eval      DSDD = tbeUNI
007200050629     C                   endif
007300050629     C*
007400050629     C                   clear                   DS$3
007500050629     C*
007600050629     C                   movel(p)  1             tblKUT
007700050629     C                   movel(p)  '$3'          tblCOD
007800050629     C                   movel(p)  '1'           tblKEY
007900050629     C     KEYTAB00_C    chain     tabel00f
008000050629     C                   if        %found(tabel00f)
008100050629     C                   eval      DS$3 = tblUNI
008200050629     C                   endif
008300050629     C*
008400050629     C                   ENDSR
008500050629     C*---------------------------------------------------------------*
008600050629
008700050629
008800050629
008900050629
009000050629     C*------------------------------------------------------------------------*
009100050629     C* CALDAT - Routine di calcolo date
009200050629     C*------------------------------------------------------------------------*
009300050629     C     CALDAT        BEGSR
009400050629     C*
009500050629     C                   z-add     *zeros        dataDa            8 0
009600050629     C                   z-add     *zeros        dataA             8 0
009700060216     C                   z-add     *zeros        dataDelA          8 0
009800050704     C                   z-add     *zeros        annomeseDa        6 0
009900050704     C                   z-add     *zeros        annomeseA         6 0
010000050629     C*
010100050629     C* CALCOLA LA DATA CORRENTE
010200050629     C                   time                    wn14             14 0
010300050629     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
010400050629     C                   z-add     wn8           g08dat
010500050629     C                   z-add     *zeros        g08inv
010600050629     C                   movel     '0'           g08err
010700050629     C                   call      'XSRDA8'
010800050629     C                   parm                    wlbda8
010900050629     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
011000050720     C*
011100050720     C* DALLA DATA ULTIMA DATA ELABORATA AGGUNGE 1 GIORNO X CALCOLARE LA DATA D INIZIO ELABORAZIONE
011200050720     C                   clear                   trul52ds
011300050720     C                   eval      d52DIN = §SDDVAS2
011400050720     C                   eval      d52GIO = 1
011500050720     C                   eval      d52SGN = *blanks
011600050720     C                   call      'TRUL52R'
011700050720     C                   parm                    trul52ds
011800050720     C                   if        d52ERR = *blanks
011900050720     C                   eval      dataDa = d52DFI
012000050720     C                   else
012100050720     C                   eval      dataDa = §SDDVAS2
012200050720     C                   endif
012300050720     C*
012400050720     C* STABILISCO LE DATE INIZIO E FINE ELABORAZIONE CORRENTE
012500050704     C                   movel(p)  §$3D00        annomeseA
012600050704     C                   movel(p)  annomeseA     anno              4 0
012700050715     C                   move(p)   annomeseA     mese              2 0
012800050715     C                   if        mese = 12
012900050715     C                   eval      mese = 01
013000050715     C                   else
013100050715     C                   eval      mese = mese + 1
013200050715     C                   eval      anno = anno - 1
013300050715     C                   endif
013400050704     C                   movel(p)  anno          annomeseDa
013500050715     C***                move      '01'          annomeseDa
013600050715     C                   move      mese          annomeseDa
013700050629     C*
013800050629     C* DALLA DATA ODIERNA SOTTRAE 1 GIORNO X DETERMINARE LA DATA D FINE ELABORAZIONE
013900050629     C                   clear                   trul52ds
014000050629     C                   eval      d52DIN = datcor
014100050629     C                   eval      d52GIO = 1
014200050629     C                   eval      d52SGN = '-'
014300050629     C                   call      'TRUL52R'
014400050629     C                   parm                    trul52ds
014500050629     C                   if        d52ERR = *blanks
014600050629     C                   eval      dataA  = d52DFI
014700050629     C                   else
014800050629     C                   eval      dataA  = datcor
014900050629     C                   endif
015000060216     C*
015100060216     C* SE RICHIESTO IN TABELLA => OVVERO GIORNI MANTENIMENTO <> 999
015200060216     C* DALLA DATA "A" SOTTRAGGO IL NUMERO DI GIORNI DA TENERE NEL FILE X AVERE LA DATA
015300060216     C* FINO A CUI CANCELLARE IL FILE SISTL
015400060216     C                   if        §SDDVAS3 <> 999
015500060216     C                   clear                   trul52ds
015600060216     C                   eval      d52DIN = dataA
015700060216     C                   eval      d52GIO = §SDDVAS3
015800060216     C                   eval      d52SGN = '-'
015900060216     C                   call      'TRUL52R'
016000060216     C                   parm                    trul52ds
016100060216     C                   if        d52ERR   = *blanks
016200060216     C                   eval      dataDelA = d52DFI
016300060216     C                   else
016400060216     C                   eval      dataDelA = *zeros
016500060216     C                   endif
016600060216     C                   endif
016700050629     C*
016800050629     C                   ENDSR
016900050629     C*---------------------------------------------------------------*
017000050629
017100050629
017200050629
017300050629
017400050629     C*------------------------------------------------------------------------*
017500050629     C* AGGTBE - Routine di aggiornamento dati tabellati
017600050629     C*------------------------------------------------------------------------*
017700050629     C     AGGTBE        BEGSR
017800050629     C*
017900060216     C                   if        dataDelA <> *zeros
018000060216     C                   eval      §SDDVAS1 = dataDelA
018100060216     C                   endif
018200050629     C                   eval      §SDDVAS2 = dataA
018300050629     C                   eval      tbeUNI   = DSDD
018400050629     C                   update    tntbe000
018500050629     C*
018600050629     C                   ENDSR
018700050629     C*---------------------------------------------------------------*
018800050629
018900050629
019000050629
019100050629     C*------------------------------------------------------------------------*
019200050629     C* PROCEDI - Routine principale
019300050629     C*------------------------------------------------------------------------*
019400050629     C     PROCEDI       BEGSR
019500050629     C*
019600050706     C* 1° LANCIO X CREAZIONE SISTL:
019700050629     C                   clear                   PARAM_1
019800050629     C                   eval      PARDAI_1 = dataDa
019900050629     C                   eval      PARDAF_1 = dataA
020000050629     C                   eval      PARSNR_1 = 'S'
020100060216     C                   eval      PARDEL_1 = dataDelA
020200050629     C                   eval      KPJBU = PARAM_1
020300050629     C                   call      'TIST37C'
020400050629     C                   parm                    KPJBA
020500050629     C                   parm      '1'           parLancio         1
020600050629     C*
020700050629     C* 2° LANCIO X CREAZIONE WFSUV:
020800050629     C                   clear                   PARAM_2
020900050704     C                   eval      PARDAI_2 = annomeseDa
021000050704     C                   eval      PARDAF_2 = annomeseA
021100050629     C                   eval      KPJBU = PARAM_2
021200050629     C                   call      'TIST37C'
021300050629     C                   parm                    KPJBA
021400050629     C                   parm      '2'           parLancio         1
021500050629     C*
021600050629     C                   ENDSR
021700050629     C*------------------------------------------------------------------------*
021800010509
021900010509
022000001218
022100001218     C*------------------------------------------------------------------------*
022200001218     C* *INZSR - ROUTINE INIZIALE
022300001218     C*------------------------------------------------------------------------*
022400001218     C     *INZSR        BEGSR
022500001218     C*
022600010726     C     *ENTRY        PLIST
022700020613     C                   PARM                    KPJBA
022800001218     C*
022900001218     C* Definizioni chiavi
023000050629     C*
023100050629     C* TNTBE01L - PARZIALE
023200050629     C     KEYTBE01_P    KLIST
023300050629     C                   KFLD                    tbeCOD
023400050629     C                   KFLD                    tbeKE1
023500050629     C*
023600050629     C* TABEL00F - COMPLETA
023700050629     C     KEYTAB00_C    KLIST
023800050629     C                   KFLD                    tblKUT
023900050629     C                   KFLD                    tblCOD
024000050629     C                   KFLD                    tblKEY
024100001218     C*
024200001218     C                   ENDSR
