000100030620     ************************************************************************
000200150319      * Genera Record NON importati da precedente giro IMPORTAZIONE dati DPD *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600150319     Ftitas30c  iF   E           k DISK
000700050907     FAzOrg01L  iF   E           k DISK    usropn
000800040909      *****************************************************************
000900050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
001000150319     D DsTA4         E DS                  EXTNAME(TITA400F)
001100030620      *
001200030624     D KPJBA         E DS
001300030625     D OG143         E DS
001400040909     D*
001500040909     D WLBDAT          DS
001600040909     D  G02DAT                 1      8  0
001700040909     D  G02INV                 9     16  0
001800040909     D  G02ERR                17     17
001900040909     D  G02TGI                18     22  0
002000150319      *
002100150319     D PAR_Entry       DS
002200150319     D   TpNtw                        3
002300150319     D   aData                        8  0
002400030625      *
002500030625     D parBolla        DS
002600050907     D   tasAASprm             1      4  0
002700050907     D   tasLnpprm             5      7  0
002800050907     D   tasNrsprm             8      9  0
002900050907     D   tasNspprm            10     16  0
003000050907     D   tasTblprm            17     18
003100030703     D   KNtw                 19     21
003200041013      *
003300050907     D  Ofil           s                   like(orgfil) DIM(999)
003400050907     D  Ode3           s                   like(orgde3) DIM(999)
003500050907     d  og             s              5  0
003600030620      *---------------------------------------------------------------------*
003700030624     C     *entry        plist
003800030624     C                   parm                    kpjba
003900041103      *--------------------------------------------------------------*
004000150319     c                   movel     kpjbu         par_entry
004100050907      *--------------------------------------------------------------*
004200050907      *  carica AZORG una volta per tutte:
004300050907     c                   clear                   oFil
004400050907     c                   clear                   oDe3
004500050907     c                   open      AzOrg01L
004600050907     c     *loval        setll     AzOrg01L
004700050907     c                   read      AzOrg01L
004800050907     c                   DOW       not %EoF(AzOrg01L)
004900050907     c                   add       1             og
005000050907     c                   move      orgfil        oFIL(og)
005100050907     c                   move      orgde3        oDE3(og)
005200050907     c                   read      AzOrg01L
005300050907     c                   enddo
005400050907     c                   close     AzOrg01L
005500050907      *--------------------------------------------------------------*
005600150319     c     KBolla        klist
005700150319     c                   kfld                    ta4aas
005800150319     c                   kfld                    ta4lnp
005900150319     c                   kfld                    ta4nrs
006000150319     c                   kfld                    ta4nsp
006100041103      *
006200150319     c                   clear                   almeno_uno        1
006300150319      ** -----------
006400150319     C/EXEC SQL
006500150319     C+ Declare TA4_BOLLA Cursor for
006600150319     c+   select * from tita400f
006700150319     c+   where ta4trc='I' and substr(ta4not, 1, 14) in
006800150319     c+   (
006900150319     c+   select substr(efrrif, 1, 14) from tnefr00f
007000150319     c+   where efrntw='DPD' and efrdte=0 and efrdft <= :aData
007100150319     c+   and efrrif not in
007200150319     c+   (
007300150319     c+   select ecfrif from tnecf00f
007400150319     c+   where ecfntw='DPD' and ecfdtg=0 and ecfdta =0
007500150319     c+   )
007600150319     c+   group by efrrif
007700150319     c+   )
007800150319     C/END-EXEC
007900150319
008000150319     C/EXEC SQL
008100150319     C+ OPEN TA4_BOLLA
008200150319     C/END-EXEC
008300150319
008400150319     C                   DOU       SqlCod <> 0
008500150319     C/EXEC SQL
008600150319     C+ FETCH NEXT FROM TA4_BOLLA INTO :dsTA4
008700150319     C/END-EXEC
008800150319     C                   SELECT
008900150319     c*
009000150319     c* a fine file Totali x rotture
009100150319     C                   WHEN      SqlCod = 100
009200150319     c                   if        almeno_uno ='S'
009300150319     c                   else
009400150319     c                   exsr      NO_dati
009500150319     c                   end
009600150319     c                   leave
009700150319     **
009800150319     C                   WHEN      SqlCod < 0
009900150319     C                   seton                                        H1
010000150319     c                   goto      fine
010100150319     c*
010200150319     C                   OTHER
010300150319     c* DETTAGLIO
010400150319     c     KBolla        chain     titas30c
010500150319     c                   if        %Found(titas30c)
010600150319     c                   clear                   ok                2
010700150319      *
010800150319     C                   clear                   OG143
010900150319     c                   z-add     1             og
011000150319     c     tasLNP        lookup    Ofil(og)                               99
011100150319     c                   if        %Equal
011200150319     C                   movel     Ode3(og)      OG143
011300150319     c                   end
011400150319     c                   IF         §ogNTW = 'DPD'
011500150319     c                   move      'OK'          ok
011600150319     C                   Else
011700150319      *
011800150319     C                   clear                   OG143
011900150319     c                   z-add     1             og
012000150319     c     tasLNA        lookup    Ofil(og)                               99
012100150319     c                   if        %Equal
012200150319     C                   movel     Ode3(og)      OG143
012300150319     c                   IF         §ogNTW = 'DPD'
012400150319     c                   move      'OK'          ok
012500150319     c                   end
012600150319     c                   end
012700150319      *
012800150319     C                   EndIF
012900150319      * se tutto OK
013000150319     c                   if        ok = 'OK'
013100150319     c                   move      'S'           almeno_uno
013200150319     c                   clear                   kpjbu
013300150319     c                   movel     TpNtw         KNtw
013400150319     c                   movel     tasAAS        tasAASprm
013500150319     c                   movel     tasLnp        tasLnpprm
013600150319     c                   movel     tasNrs        tasNrsprm
013700150319     c                   movel     tasNsp        tasNspprm
013800150319     c                   movel     tasTbl        tasTblprm
013900150319     c                   movel     parBolla      kpjbu
014000150319     c                   call      'FIEU78R1'
014100150319     c                   parm                    kpjba
014200150319     c                   parm                    DsTAS
014300150319     C                   EndIF
014400150319      *
014500150319     C                   EndIF
014600150319      *
014700150319      *
014800150319     C                   ENDSL
014900150319
015000150319     C                   ENDDO
015100150319     C/EXEC SQL
015200150319     C+ CLOSE ANA_CGB
015300150319     C/END-EXEC
015400150319     c                   if         almeno_uno = 'S'
015500150319      *
015600041103     c                   clear                   kpjbu
015700150319     c                   movel     par_entry     kpjbu
015800080619      *
015900080619      * Accorpamento dei Resi sulla bolla originale
016000080619     c                   call      'FIEU78R2'
016100080619     c                   parm                    kpjba
016200041103      *
016300150319     c                   endIF
016400161298      *----------------------------------------------------------------
016500040909     c     FINE          tag
016600030620     c                   seton                                        LR
016700150319      *----------------------------------------------------------------
016800150319     c     NO_DATI       begSR
016900150319      *
017000150319     c                   endSR
017100150319      *----------------------------------------------------------------
