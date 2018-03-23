000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000300000000     H*--------------------------------------------------------------*
000400120904     FFICNA9D   CF   E             WORKSTN
000500120904     Fazorg01l  IF   E           K DISK
000600120907     Ftntbe01l  IF   E           K DISK
000700120904     Fanrco98j  IF   E           K DISK
000800930707     F*
000900920810     D KPJBA         E DS
001000120907     D dbld          E DS
001100120907     D dblc          E DS
001200120907     D dblo          E DS
001300950125     D*
001400120904     d  data1          s              8  0
001500120904     d  fil1           s              3  0
001600120904     d  codf           s              8
001700120905     d  filpdr1        s              7  0
001800120905     d  filpdr2        s              7  0
001900120904      *
002000910423     D WLBDAT          DS
002100950125     D  G02DAT                 1      8  0
002200950125     D  G02INV                 9     16  0
002300950125     D  G02ERR                17     17
002400950125     D  G02TGI                18     22  0
002500120907     d dataiso         s               d   datfmt(*iso)
002600120907     d dataeur         s               d   datfmt(*eur)
002700120904     C*---------------------------------------------------------------*
002800000000     C     *ENTRY        PLIST
002900000000     C                   PARM                    KPJBA
003000931116     C*---------------------------------------------------------------*
003100120904     c     krco          klist
003200120905     c                   kfld                    v1sdfa            3
003300120905     c                   kfld                    natura            1
003400120905     c                   kfld                    codf
003500120905     c                   eval      natura = 'F'
003600930426     C*
003700120904     C                   do        *hival
003800120904     C                   EXFMT     cna9d01
003900931116     C*
004000920323     C** CMD3 - FINE LAVORO
004100120904     C   KC              leave
004200931015     C* CONTROLLI FORMATO1
004300900528     C                   EXSR      CONTR1
004400120904     C   90              iter
004500931116     C*
004600120904     c                   if        *inkf
004700120905     c                   movel     fil1          filpdr1
004800120905     c     filpdr1       add       9999          filpdr2
004900120904     c                   exsr      elabora
005000120904     c                   leave
005100120904     c                   endif
005200120904     C*
005300120904     c                   enddo
005400900611     C*
005500931027     C                   SETON                                        LR
005600900528     C*
005700120904     C*---------------------------------------------------------------*
005800900528     C     CONTR1        BEGSR
005900120904     C*---------------------------------------------------------------*
006000120904     c                   setoff                                       424341
006100120913     c                   setoff                                       404490
006200120907     C* D A T A  da tabella se c'è
006300120907     c                   select
006400120913     c                   when      vi1tsp = ' '
006500120913     c                   seton                                        4490
006600120913     c                   when      vi1tsp = 'A' or vi1tsp = 'R'
006700120907     c     'BLC'         chain     tntbe01l
006800120907     c                   if        %found(tntbe01l)
006900120907     c                   movel     tbeuni        dblc
007000120907     c                   movel     §blcdtp       dataiso
007100120907     c                   movel     dataiso       dataeur
007200120907     c                   movel     dataeur       vi1dd1
007300120907     c                   endif
007400120907      *
007500120907     c                   when      vi1tsp = 'D'
007600120907     c     'BLD'         chain     tntbe01l
007700120907     c                   if        %found(tntbe01l)
007800120907     c                   movel     tbeuni        dbld
007900120907     c                   movel     §blddtp       dataiso
008000120907     c                   movel     dataiso       dataeur
008100120907     c                   movel     dataeur       vi1dd1
008200120907     c                   endif
008300120907      *
008400120907     c                   when      vi1tsp = 'C'
008500120907     c     'BLO'         chain     tntbe01l
008600120907     c                   if        %found(tntbe01l)
008700120907     c                   movel     tbeuni        dblo
008800120907     c                   movel     §blodtp       dataiso
008900120907     c                   movel     dataiso       dataeur
009000120907     c                   movel     dataeur       vi1dd1
009100120907     c                   endif
009200120907     c                   endsl
009300120907      *
009400931126     C                   MOVE      VI1DD1        G02DAT
009500931126     C                   MOVEL     *BLANK        G02ERR
009600950125     C                   CALL      'XSRDA8'
009700931126     C                   PARM                    WLBDAT
009800931126    1C     G02ERR        IFEQ      '1'
009900931126     C                   SETON                                        43  90
010000931126    1C                   END
010100120904     C                   Z-ADD     G02INV        data1             8 0
010200950125     C                   Z-ADD     G02DAT        VI1DD1
010300120904      *
010400120904     c     vi1pf1        chain     azorg01l
010500120904     c                   if        %found(azorg01l)
010600120904     c                   movel     orgdes        vi1pf1d
010700120905     c                   movel     vi1pf1        fil1
010800120904     c                   else
010900120904     c                   clear                   vi1pf1d
011000120905     c                   clear                   fil1
011100120904     c                   seton                                        4290
011200120904     c                   endif
011300120904      *
011400120904     c                   if        v1cdf > 0 and v1sdf = 0 or
011500120904     c                             v1cdf = 0 and v1sdf > 0
011600120904     c                   seton                                        4090
011700120904     c                   endif
011800120905     c                   clear                   vi1cdfd
011900120904     c                   if        v1cdf > 0 and v1sdf > 0
012000120905     c                   move      v1sdf         v1sdfa
012100120905     c                   move      v1cdf         codf
012200120904     c     krco          chain     anrco98j
012300120904     c                   if        %found(anrco98j)
012400120904     c                   movel     sogdes        vi1cdfd
012500120904     c                   move      rcoksc        v1cdf
012600120904     c                   else
012700120904     c                   seton                                        4190
012800120904     c                   endif
012900120904     c                   endif
013000120907      *
013100931126     C                   ENDSR
013200120904      * ?___________________________________________________________________
013300120904     C     elabora       begsr
013400120904      * ?___________________________________________________________________
013500120904     c*
013600120904     c                   select
013700120904      *
013800120904      * autisti di città intera filiale
013900120904     c                   when      vi1tsp = 'A' and v1cdf = 0
014000120904     C/EXEC SQL
014100120904     C+ update fiftt00F set fttdcn = 0
014200120904     c+ where fttdft = :data1 and fttfgs = :fil1
014300120904     C/END-EXEC
014400120904      *
014500120904      * autisti di città fornitore specifico
014600120904     c                   when      vi1tsp = 'A' and v1cdf > 0
014700120904     C/EXEC SQL
014800120904     C+ update fiftt00F set fttdcn = 0
014900120904     c+ where fttdft = :data1 and fttfgs = :fil1 and fttcdf = :codf
015000120904     C/END-EXEC
015100120904      *
015200120904      * ?Coop
015300120904      *
015400120904      * Cooperative tutta la filiale
015500120904     c                   when      vi1tsp = 'C' and v1cdf = 0
015600120904     C/EXEC SQL
015700120904     C+ update fictt00F set cttdcn = 0
015800120904     c+ where cttdft = :data1 and cttfgs = :fil1
015900120904     C/END-EXEC
016000120904      *
016100120904      * Cooperative fornitore specifico
016200120904     c                   when      vi1tsp = 'C' and v1cdf > 0
016300120904     C/EXEC SQL
016400120904     C+ update fictt00F set cttdcn = 0
016500120904     c+ where cttdft = :data1 and cttfgs = :fil1 and cttcdf = :codf
016600120904     C/END-EXEC
016700120904      *
016800120904      * ?Affluenze/defluenze
016900120904      *
017000120904      * Aff/def tutta la filiale
017100120904     c                   when      vi1tsp = 'D' and v1cdf = 0
017200120904     C/EXEC SQL
017300120904     C+ update fiatt00F set attdcn = 0
017400120905     c+ where attdft = :data1 and attter = :fil1
017500120904     C/END-EXEC
017600120904      *
017700120904      * aff/def fornitore specifico
017800120904     c                   when      vi1tsp = 'D' and v1cdf > 0
017900120904     C/EXEC SQL
018000120904     C+ update fiatt00F set attdcn = 0
018100120905     c+ where attdft = :data1 and attter = :fil1 and attcdf = :codf
018200120904     C/END-EXEC
018300120904      *
018400120904      * ?Residuali
018500120904      * Residuali tutta la filiale
018600120904     c                   when      vi1tsp = 'R' and v1cdf = 0
018700120904     C/EXEC SQL
018800120904     C+ update fiott00F set ottdcn = 0
018900120905     c+ where ottdft = :data1 and ottpdr between :filpdr1 and :filpdr2
019000120904     C/END-EXEC
019100120904      *
019200120904      * residuali fornitore specifico
019300120904     c                   when      vi1tsp = 'R' and v1cdf > 0
019400120904     C/EXEC SQL
019500120904     C+ update fiott00F set ottdcn = 0
019600120905     c+ where ottdft = :data1  and ottcdf = :codf
019700120904     C/END-EXEC
019800120905      *
019900120905     c                   endsl
020000120904     c                   endsr
