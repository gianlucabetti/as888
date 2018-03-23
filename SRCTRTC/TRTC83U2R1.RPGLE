000100960530      ***********************************************************************
000200120828      *                                                                     *
000300120828      *   ?   - ESPORTA SU TIVGD - quanto caricato in QTEMP/EDIFCSUM    ?   *
000400120828      *                                                                     *
000500960530      ***********************************************************************
000600120823     H DECEDIT('0,') DATEDIT(*YMD/)
000700000207      *---------------------------------------------------------------------*
000800120822     FEDIFCSUM  IF   E           K DISK
000900120822      **?__________________________________________________________________ */
001000120822     ftivgd00f  if a E             DISK    commit
001100120822     Ftivir02L  IF   E           K DISK
001200120822     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001300120822     F                                     PREFIX(f_) usropn
001400120822      **?__________________________________________________________________ */
001500120822     D TipoF           C                   CONST('EU')
001600120822     D dwlprg          s             10    INZ(*all'0')
001700120822     D wrkprg          s              8  0 INZ(*zeros)
001800120822     d trul47ds      e ds
001900120822     D W0140           S             14  0 inz
002000120822     D Progressivo     S             10    inz
002100120822      *---------------------------------------------------------------------*
002200120822     D Var_ISV         s                   like(Virfi1)
002300120822      *---------------------------------------------------------------------*
002400120822     D TRTC82DS      E DS
002500120822     D KPJBA         E DS
002600120822     D WLBDA8          DS                  INZ
002700120822     D  G02DAT                 1      8  0
002800120822     D  G02INV                 9     16  0
002900120822     D  G02ERR                17     17
003000120822     D  G02TGI                18     22  0
003100180313     D*-------------
003200180313     D* Reperimento nome PGM
003300180313     D STATUS         SDS           333
003400180313     D  DSPGM            *PROC
003500180313     D*-------------
003600120822      **?__________________________________________________________________ */
003700120822     IRRIFCSUM
003800120822     I              RRIFCSUM                    SUMDAT
003900020222     C*---------------------------------------------------------------------*
004000960530     C* Ciclo principale                                                   -*
004100960530     C*---------------------------------------------------------------------*
004200010426     C     *ENTRY        PLIST
004300010426     C                   PARM                    KPJBA
004400120822     C                   eval       TRTC82DS = kpjbu
004500120822     C*
004600120822      * apre
004700120822      *   il TIVGD
004800120822     C                   EXSR      vgd_OPEN
004900010426     C*
005000120822      * Legge e scrive il TIVGD
005100120822     C                   READ      EDIFCSUM
005200020219      *
005300120822    3C                   dow       not %EoF(EDIFCSUM)
005400020219      * ------------
005500120822     c                   exsr      VGD_WRITE
005600120822      * ------------
005700120822     C                   READ      EDIFCSUM
005800120822    3C                   ENDdo
005900120822      * quindi chiude
006000120822      *   il TIVGD
006100120822     C                   EXSR      vgd_CLOSE
006200120822      *
006300120823     c                   COMMIT
006400120823      *
006500120823     C                   SETON                                        LR
006600950428     C*****************************************************************
006700950428     C* OPERAZIONI INIZIALI
006800950428     C*****************************************************************
006900950428     C     *INZSR        BEGSR
007000010426     C*
007100120822     c     kvir02        klist
007200120822     C                   kfld                    virKSC
007300120822     C                   kfld                    virTIP
007400120822      *
007500020227      *------------
007600020220     C* Inizializzo variabili totale
007700960530     C* Imposto data del giorno
007800960530     C                   TIME                    W0140            14 0
007900120822     C                   TIME                    ORADAT           14 0
008000120822     C                   MOVEL     ORADAT        ORATR             6 0
008100120822     c                   move      *date         udtymd            8 0
008200960530     C*
008300180313     C                   MOVEL     DSPGM         NOMPGM           10
008400180313     C*
008500960530     C                   ENDSR
008600120822      **?__________________________________________________________________ */
008700120822      *    Apertura del TIVGD
008800120822      **?__________________________________________________________________ */
008900120822     C     VGD_OPEN      Begsr
009000120822      *
009100120822      *  recupera il progressivo
009200120822     c                   exsr      Piglia_progr
009300120822      *
009400120822      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
009500120822     C                   clear                   trul47ds
009600120822     C                   eval      d47opz  = 'I'
009700120822     C                   eval      d47tip  = TipoF
009800120822     C                   eval      d47lck  = 'N'
009900120822     C                   eval      d47chkj = 'N'
010000180313     C                   eval      d47pgm  = NOMPGM
010100120822     C                   call      'TRUL47R'
010200120822     C                   parm                    trul47ds
010300120822      *
010400120822     c                   endsr
010500120822      **?__________________________________________________________________ */
010600120822      *   Scrittura del TIVGD
010700120822      **?__________________________________________________________________ */
010800120822     C     VGD_WRITE     Begsr
010900120822      *
011000120822     c                   clear                   tivgd000
011100120822     c                   eval      vgddta = %TrimR(SumDAT)
011200120822     c                   eval      vgdtip = TipoF
011300120822     c                   move      *all'0'       cliente           8
011400120822     c                   move      D82CPT        cliente
011500120822     c                   eval      vgdksu = cliente
011600120822     C                   eval      vgdprg = Progressivo
011700120822     c                   eval      vgdtsc = 'WW'
011800180313     c                   eval      vgdpgm = NOMPGM
011900120822     c                   eval      vgddat = udtymd
012000120822      *
012100120822     C                   WRITE     tivgd000
012200120822      *
012300120822     c                   endsr
012400120822      **?__________________________________________________________________ */
012500120822      *    Chiusura del TIVGD
012600120822      **?__________________________________________________________________ */
012700120822     C     VGD_CLOSE     Begsr
012800120822      *
012900120822     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
013000120822     C                   clear                   trul47ds
013100120822     C                   eval      d47opz  = 'F'
013200120822     C                   eval      d47tip  = TipoF
013300120822     C                   call      'TRUL47R'
013400120822     C                   parm                    trul47ds
013500120822      *
013600120822     c                   endsr
013700120822      **?__________________________________________________________________ */
013800120822      *   Prende il Progressivo
013900120822      **?__________________________________________________________________ */
014000120822     C     Piglia_progr  Begsr
014100120822      *
014200120822     c                   move      *all'0'       cliente           8
014300120822     c                   move      D82CPT        cliente
014400120822     C                   MOVE      Cliente       virKSC
014500120822     C                   MOVEL     TipoF         virTIP
014600120822     c     kvir02        chain     tivir02l
014700120822     c                   move      'OF'          Var_ISV
014800120822     c                   if        %Found(tivir02l)
014900120822     c                   move      VirFI1        Var_ISV
015000120822     C                   End
015100120822
015200120822      *       prende numeratore strategi
015300120822     C                   exsr      calprog
015400120822      *
015500120822     c                   endsr
015600120822     C*------------------------------------------------------*
015700120822     C*  prepara il file
015800120822     C*------------------------------------------------------*
015900120822     C     calprog       begsr
016000120822     C*
016100120822     C                   open      tis7prgf
016200120822     C                   read(e)   tis7prgf
016300120822     C*
016400120822     C                   if        not %error
016500120822     C                   eval      dwlprg = f_tis7prgf
016600120822     C*
016700120822     C                   move(p)   dwlprg        wrkprg
016800120822     C                   add       1             wrkprg
016900120822     C                   move(p)   wrkprg        dwlprg
017000120822     C                   movel     Var_ISV       dwlprg
017100120822     C*
017200120822     C                   eval       f_tis7prgf = dwlprg
017300120822     C                   eval      Progressivo = dwlprg
017400120822     C*
017500120822     C                   update    tis7prg0
017600120822     C                   endif
017700120822     C*
017800120822     C                   close     tis7prgf
017900120822     C*
018000120822     C                   endsr
018100120822      **?__________________________________________________________________ */
