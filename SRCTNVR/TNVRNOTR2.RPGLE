000100010213     H DECEDIT('0,') DATEDIT(*YMD.)
000200010213
000300010213      **************************************************************************
000400010213      *                                                                        *
000500010213      *          Pulizia TFNTC00F prima di nuova gestione note                 *
000600010213      *                                                                        *
000700010213      *                        E S E C U Z I O N E                             *
000800010213      *                                                                        *
000900010213      **************************************************************************
001000010213
001100040929     FTFNTC01L  iF   E           K DISK
001200010219     FTABEL00F  IF   E           K DISK
001300010214     FTNVRNOTP  O    E             PRINTER OFLIND(*IN55)
001400010213
001500010213     D KPJBA         E DS
001600010213     D CNCR80        E DS
001700010213     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800010219     D DS1T          E DS
001900010216     D PARAM           DS
002000010302      * S=Simulazione
002100010302     D  VTIPOE                        1                                         S=Simulazione
002200010302      * Stampa: S=Si  N=No
002300010302     D  VSTAMP                        1                                         Stampa: S=Si  N=No
002400010213
002500010213     D rnt             S             60    DIM(300)                             Salvataggio Righe
002600010213     D wapl            s                   like(ntcapl)
002700010213     D wnk1            s                   like(ntcnk1)
002800010213     D wnk2            s                   like(ntcnk2)
002900010213     D wtnt            s                   like(ntctnt)
003000010219     D kcod            S                   LIKE(tblcod)
003100010219     D kkey            S                   LIKE(tblkey)
003200010213
003300010213      *------------------------------------------------------------------------*
003400010213      *                                                                        *
003500010213      *    Significato campo "wdelet" (causale annullamento record)            *
003600010213      *                                                                        *
003700010302      * x  D = Riga nota duplicata                                             *
003800010302      * x  N = Numero superiore a quello previsto in tabella (1T)              *
003900010302      * x  S = Sono in sede                                                    *
004000010213      *                                                                        *
004100010213      *------------------------------------------------------------------------*
004200010213
004300010213      *------------------------------------------------------------------------*
004400010213      *                                                                        *
004500010213      *    OPERAZIONI INIZIALI                                                 *
004600010213      *                                                                        *
004700010213      *------------------------------------------------------------------------*
004800010213
004900010213     C     *ENTRY        PLIST
005000010213     C                   PARM                    KPJBA
005100010216     C                   movel     KPJBU         PARAM
005200010213
005300010213      *    Recupero Ragione Sociale Ditta da Emettere in Stampa
005400010213
005500010213     C                   Z-ADD     1             CODUT
005600010213     C                   CALL      'X§PARUT'
005700010213     C                   PARM                    UT§DSE
005800010213     C                   MOVEL     RAGUT         RSUT             20
005900010213     C                   MOVEL     REC80         CNCR80
006000010214
006100010214      *    Accesso a CNACO
006200010214
006300010214     C     kcnaco        KLIST
006400010214     C                   kfld                    codut
006500010214     C                   kfld                    kkcc              4 0
006600010214     C                   kfld                    kksc              7 0
006700010214
006800010219      *    Accesso a TNTAM / TNOFM
006900010214
007000010214     C     ktntam        KLIST
007100010214     C                   kfld                    kksc
007200010214     C                   kfld                    kctr              3 0
007300010219
007400010219      *    Accesso a TABEL
007500010219
007600010219     C     Ktabel        klist
007700010219     C                   kfld                    codut
007800010219     C                   kfld                    kcod
007900010219     C                   kfld                    kkey
008000010219     C                   movel     '1T'          kcod
008100010213
008200010216     C     vstamp        ifeq      'S'
008300010216     C     vtipoe        ifeq      'S'
008400010214     C                   eval      psimu = '(Simulazione)'
008500010214     C                   endif
008600010214     C                   write     testa
008700010214     C                   z-add     0             num1              9 0
008800010214     C                   z-add     0             num2              9 0
008900010216     C                   endif
009000010214
009100010213      *------------------------------------------------------------------------*
009200010213      *                                                                        *
009300010213      *    ROUTINE PRINCIPALE                                                  *
009400010213      *                                                                        *
009500010213      *------------------------------------------------------------------------*
009600010213
009700010213      *    Lettura File Note
009800010213      *    =================
009900010213
010000010213     C     *loval        setll     TFNTC01L
010100010213     C                   do        *hival
010200010213     C                   read      TFNTC01L
010300010213     C                   if        %eof(tfntc01l)
010400010213     C                   leave
010500010213     C                   endif
010600010213
010700010213      *    Sflaggo la causale di annullamento record
010800010213
010900010213     C                   clear                   wdelet            1
011000010213
011100010213      *    A cambio tipo nota faccio reset della schiera delle righe
011200010219      *    e reperisco numeratore righe da tabella 1T
011300010213
011400010213     C     ntcapl        ifne      wapl
011500010213     C     ntcnk1        orne      wnk1
011600010213     C     ntcnk2        orne      wnk2
011700010213     C     ntctnt        orne      wtnt
011800010213     C                   movea     *blanks       rnt
011900010213     C                   z-add     0             x                 3 0
012000010219     C                   movel(p)  ntctnt        kkey
012100010219     C     ktabel        chain     tabel00f
012200010219     C                   if        %found(tabel00f)
012300010219     C                   movel     tbluni        ds1t
012400010219     C                   else
012500010219     C                   z-add     998           §1trig
012600010219     C                   endif
012700010213     C                   endif
012800010213
012900010213      *    Salvo valori per determinare cambio tipo nota
013000010213
013100010213     C                   eval      wapl = ntcapl
013200010213     C                   eval      wnk1 = ntcnk1
013300010213     C                   eval      wnk2 = ntcnk2
013400010213     C                   eval      wtnt = ntctnt
013500010214
013600010214      *    Controlli
013700010214      *    =========
013800010213
013900010213      *    Controllo riga duplicata
014000010213
014100010214     C     wdelet        ifeq      ' '
014200040929     c     ntcrnt        andne     *Blanks
014300010213     C     ntcrnt        lookup    rnt                                    90
014400010213     C     *in90         ifeq      *on
014500010213     C                   eval      wdelet = 'D'                                 Riga Duplicata
014600010213     C                   else
014700010213     C                   if        x < 300
014800010213     C                   add       1             x
014900010213     C                   move      ntcrnt        rnt(x)
015000010213     C                   endif
015100010213     C                   endif
015200010213     C                   endif
015300010219
015400010219      *    Controllo numero superiore a tabella 1T
015500010219
015600010219     C     wdelet        ifeq      ' '
015700010219     C     X             andgt     §1trig
015800010219     C                   eval      wdelet = 'N'                                 Nr. superiore a tab.
015900010219     C                   endif
016000010214
016100010214      *    Stampa note da Pulire
016200010214      *    =====================
016300010214
016400010216     C     vstamp        ifeq      'S'
016500010216     C                   add       1             num1
016600010214     C     wdelet        ifne      ' '
016700010214
016800010214     C                   add       1             num2
016900010214
017000010214     C     wdelet        ifeq      'D'
017100010214     C                   eval      errore = 'Riga nota duplicata      '
017200010214     C                             + '             '
017300010214     C                   endif
017400010219
017500010219     C     wdelet        ifeq      'N'
017600010219     C                   move      §1trig        walfa3            3
017700010219     C                   eval      errore = 'Numero riga superiore a n'
017800010219     C                             + 'umero max ' + walfa3
017900010219     C                   endif
018000010214
018100010214     C   55              write     testa
018200010214     C   55              eval      *in55 = *off
018300010214     C                   write     dett
018400010214
018500010214     C                   endif
018600010216     C                   endif
018700010213
018800010213     C                   enddo
018900010213
019000010213      *    Fine Programma
019100010606      *    ==============
019200010213
019300010216     C     vstamp        ifeq      'S'
019400010214     C                   write     fine
019500010216     C                   endif
019600010214
019700010213     C                   eval      *inlr = *on
019800010219
