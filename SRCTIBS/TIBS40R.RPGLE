000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020412
000300120517      * tibs40R  *---------------------------------------------------*
000400120517      * - Caricamento skiere filiali con GACICURxxx
000500020412      *--------------------------------------------------------------*
000600120517     fTNTBE01L  if   e           k disk
000700120517     fazcln01L  if   e           k disk
000800150625     fazorg01l  if   e           k disk
000900150625     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001000120517     d
001100150917     D wtip            s              1    DIM(600)
001200120517     d tibs40ds      e ds
001300120517     D wfil                    9   1808  0 DIM(600)
001400120517     D wlib                 1809   7808    DIM(600)
001500120517     d*
001600120517     D                 DS
001700120517     D  ds_giorno              7      8  0
001800120517     D  ds_mese                5      6  0
001900120517     D  ds_anno                1      4  0
002000120517     D  ds_data                1      8  0
002100120517
002200120517     D clnmat          DS
002300120517     D  mat                    1     31    dim(31)
002400120517     D clnpom          DS
002500120517     D  pom                    1     31    dim(31)
002600120517
002700120517
002800120517     d kpjba         e ds
002900120517      * - Tabella con parametro CURLIB x P.O.
003000120517     d dCUR          e ds                  inz
003100120517     d
003200120517     D trul06ds      E DS
003300120517     D  LIN                    1     90  0 DIM(30)
003400120517      * - Indici di schiera
003500120517     d xx              s              4  0 inz
003600120517     d ff              s              4  0 inz
003700120517     d
003800120517     d kcod            s                   like(tbecod)
003900150625     d kcar            s                   like(orgcar)
004000150625     d kdiv            s                   like(orgfl3)
004100120517     d Dataoggi        s              8  0
004200120517     d Dataiso         s               d   datfmt(*iso)
004300120517     d Datasys         s               d   datfmt(*iso) inz(*sys)
004400120517     D ktfp            S                   LIKE(CLNtfp) INZ
004500120517     D ktfa            S                   LIKE(CLNtfa) INZ
004600120517     D kann            S                   LIKE(CLNann)
004700120517     D kmes            S                   LIKE(CLNmes)
004800150917     D Indx            S              3  0
004900150917     D wsost           S              1
005000020412      *---------------------------------------------------------------
005100020412     c     *entry        plist
005200120517     c                   parm                    kpjba
005300120517     c                   parm                    tibs40ds
005400120517     c
005500120517     c                   clear                   wfil
005600120517     c                   clear                   wlib
005700120517     c
005800120517     c                   if        ibs40drf=0
005900120517     c* Calcolo la data di oggi
006000120517     c                   movel     datasys       dataiso
006100120517     c
006200120517     c* Se festivo cerco il primo giorno lavorativo
006300120517     c                   do        *hival
006400120517      * verifico se è un giorno lavorativo
006500120517     c     *iso          move      dataiso       ds_data
006600120517     C                   eval      kann = ds_anno
006700120517     C                   eval      kmes = ds_mese
006800120517     C     kazcln        chain     azcln01l
006900120517     C                   if        %found(azcln01l)
007000120517     C                   if        MAT(ds_giorno) =  'F'  or
007100120517     C                             POM(ds_giorno) =  'F'
007200120517      * se non va bene aggiungo 1 giorno
007300120517     c                   adddur    1:*d          dataiso
007400120517     C                   iter
007500120517     c                   else
007600120517     c                   leave
007700120517     C                   endif
007800120517     c                   else
007900120517     c                   leave
008000120517     C                   endif
008100120517
008200120517     c                   enddo
008300120517
008400120517     c                   eval      dataoggi=%dec(dataiso)
008500120517     c                   else
008600120517     c                   eval      dataoggi=ibs40drf
008700120517     c                   endif
008800020412
008900120517     c                   eval      kcod='CUR'
009000120517     c     kcod          setll     tntbe01l
009100120517     c     kcod          reade     tntbe01l
009200120517    1c                   dow       not %eof(tntbe01l)
009300120517    2c                   if        tbeatb=' '
009400120517     c                   movel     tbeuni        dcur
009500120517     c*
009600120517    3c                   if        §curdde<=dataoggi
009700150625     c* Richiesta per terminal o filiale
009800150625   3ac                   if        §curapl='T' or §curapl='F'
009900120517     c
010000120517     c* carico i secondi livello a oggi
010100120517     C                   CLEAR                   trul06ds
010200120517    4c                   if        §curapl='T'
010300120517     C                   MOVE      '£1'          D06COD
010400120517     c                   else
010500120517     C                   MOVE      '£6'          D06COD
010600120517    4c                   endif
010700120517     C                   MOVEL     §curfil       D06KEY
010800120517     C                   MOVEL     §curdde       D06drf
010900120517     C                   MOVEL     trul06ds      KPJBU
011000120517     C*
011100120517     C                   CALL      'TRUL06R'
011200120517     C                   PARM                    KPJBA
011300120517     C                   MOVEL     KPJBU         trul06ds
011400120517
011500120517     c                   eval      xx=1
011600120517    4c                   dow       lin(xx)> 0
011700150917     c                   eval      Indx=%lookup(lin(xx):wfil)
011800150917     c                   if        indx=0
011900120517     C                   add       1             ff
012000120517     c                   eval      wfil(ff)=lin(xx)
012100120517     c                   eval      wlib(ff)=tbeke1
012200150917     c                   eval      wtip(ff)=§curapl
012300150917     c                   else
012400150917     c* "A" vince su "T" che vince su "F"
012500150917     c                   eval      orgfil=lin(xx)
012600150917     c                   exsr      priority
012700150917     c                   endif
012800120517     c
012900120517     c                   add       1             xx
013000120517    4c                   enddo
013100150625    c
013200150625  x3ac                   else
013300150625     c* richiesta per area
013400150625     c                   exsr      carArea
013500120517     c
013600150625   3ac                   endif
013700150625    3c                   endif
013800120517    2c                   endif
013900120517     c*
014000120517     c     kcod          reade     tntbe01l
014100120517    1c                   enddo
014200120517     c
014300120523     c                   eval      ibs40drf=dataoggi
014400120517     c                   seton                                        LR
014500150917     C* Priorita di caricamento -----------------------------------*
014600150917     C     priority      BEGSR
014700150917     c                   eval      wsost='N'
014800150917     c                   select
014900150917     c                   when      §curapl='A'
015000150917     c                   eval      wsost='S'
015100150917     c                   when      §curapl='T'  and wtip(indx)='F'
015200150917     c                   eval      wsost='S'
015300150917     c                   endsl
015400150917     c                   if        wsost='S'
015500150917     c                   eval      wfil(indx)=orgfil
015600150917     c                   eval      wlib(indx)=tbeke1
015700150917     c                   eval      wtip(indx)=§curapl
015800150917     c                   endif
015900150917     c                   ENDSR
016000150625     C* Caricamento per Area  -------------------------------------*
016100150625     C     CarArea       BEGSR
016200150625     c* Prendo il codcie area della filiale
016300150625     c     §curfil       chain     azorg01l
016400150625    1c                   if        %found(azorg01l)
016500150625     c                   eval      kcar=orgcar
016600150625     c                   eval      kdiv=orgfl3
016700150625     c     korg2         setll     azorg02l
016800150625     c     korg2         reade     azorg02l
016900150625    2c                   dow       not %eof(azorg02l)
017000150625    3c                   if        (orgfag='A' or orgfag='F')
017100150625     c                             and orgfva=' '
017200150917     c                   eval      Indx=%lookup(orgfil:wfil)
017300150917     c                   if        indx=0
017400150625     C                   add       1             ff
017500150625     c                   eval      wfil(ff)=orgfil
017600150625     c                   eval      wlib(ff)=tbeke1
017700150917     c                   eval      wtip(ff)=§curapl
017800150917     c                   else
017900150917     c                   exsr      priority
018000150625    3c                   endif
018100150917    3c                   endif
018200150625     c
018300150625     c     korg2         reade     azorg02l
018400150625    2c                   enddo
018500150625     c
018600150625   x1c                   else
018700150917     c                   eval      Indx=%lookup(§curfil:wfil)
018800150917     c                   if        indx=0
018900150625     C                   add       1             ff
019000150625     c                   eval      wfil(ff)=§curfil
019100150625     c                   eval      wlib(ff)=tbeke1
019200150917     c                   eval      wtip(ff)=§curapl
019300150917     c                   else
019400150917     c                   eval      orgfil=§curfil
019500150917     c                   exsr      priority
019600150917    3c                   endif
019700150625    1c                   endif
019800150625     c
019900150625     C                   ENDSR
020000150625     C* IMPOSTAZIONI INIZIALI -------------------------------------*
020100150625     C     *INZSR        BEGSR
020200120517     C     kazcln        klist
020300120517     C                   kfld                    ktfp
020400120517     C                   kfld                    ktfa
020500120517     C                   kfld                    kann
020600120517     C                   kfld                    kmes
020700150625     C     korg2         klist
020800150625     C                   kfld                    kdiv
020900150625     C                   kfld                    kcar
021000120517     C
021100120517     C                   ENDSR
