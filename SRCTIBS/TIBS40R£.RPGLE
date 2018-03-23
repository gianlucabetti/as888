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
001100120517     d tibs40ds      e ds
001200120517     D wfil                    9   1808  0 DIM(600)
001300120517     D wlib                 1809   7808    DIM(600)
001400120517     d*
001500120517     D                 DS
001600120517     D  ds_giorno              7      8  0
001700120517     D  ds_mese                5      6  0
001800120517     D  ds_anno                1      4  0
001900120517     D  ds_data                1      8  0
002000120517
002100120517     D clnmat          DS
002200120517     D  mat                    1     31    dim(31)
002300120517     D clnpom          DS
002400120517     D  pom                    1     31    dim(31)
002500120517
002600120517
002700120517     d kpjba         e ds
002800120517      * - Tabella con parametro CURLIB x P.O.
002900120517     d dCUR          e ds                  inz
003000120517     d
003100120517     D trul06ds      E DS
003200120517     D  LIN                    1     90  0 DIM(30)
003300120517      * - Indici di schiera
003400120517     d xx              s              4  0 inz
003500120517     d ff              s              4  0 inz
003600120517     d
003700120517     d kcod            s                   like(tbecod)
003800150625     d kcar            s                   like(orgcar)
003900150625     d kdiv            s                   like(orgfl3)
004000120517     d Dataoggi        s              8  0
004100120517     d Dataiso         s               d   datfmt(*iso)
004200120517     d Datasys         s               d   datfmt(*iso) inz(*sys)
004300120517     D ktfp            S                   LIKE(CLNtfp) INZ
004400120517     D ktfa            S                   LIKE(CLNtfa) INZ
004500120517     D kann            S                   LIKE(CLNann)
004600120517     D kmes            S                   LIKE(CLNmes)
004700020412      *---------------------------------------------------------------
004800020412     c     *entry        plist
004900120517     c                   parm                    kpjba
005000120517     c                   parm                    tibs40ds
005100120517     c
005200120517     c                   clear                   wfil
005300120517     c                   clear                   wlib
005400120517     c
005500120517     c                   if        ibs40drf=0
005600120517     c* Calcolo la data di oggi
005700120517     c                   movel     datasys       dataiso
005800120517     c
005900120517     c* Se festivo cerco il primo giorno lavorativo
006000120517     c                   do        *hival
006100120517      * verifico se è un giorno lavorativo
006200120517     c     *iso          move      dataiso       ds_data
006300120517     C                   eval      kann = ds_anno
006400120517     C                   eval      kmes = ds_mese
006500120517     C     kazcln        chain     azcln01l
006600120517     C                   if        %found(azcln01l)
006700120517     C                   if        MAT(ds_giorno) =  'F'  or
006800120517     C                             POM(ds_giorno) =  'F'
006900120517      * se non va bene aggiungo 1 giorno
007000120517     c                   adddur    1:*d          dataiso
007100120517     C                   iter
007200120517     c                   else
007300120517     c                   leave
007400120517     C                   endif
007500120517     c                   else
007600120517     c                   leave
007700120517     C                   endif
007800120517
007900120517     c                   enddo
008000120517
008100120517     c                   eval      dataoggi=%dec(dataiso)
008200120517     c                   else
008300120517     c                   eval      dataoggi=ibs40drf
008400120517     c                   endif
008500020412
008600120517     c                   eval      kcod='CUR'
008700120517     c     kcod          setll     tntbe01l
008800120517     c     kcod          reade     tntbe01l
008900120517    1c                   dow       not %eof(tntbe01l)
009000120517    2c                   if        tbeatb=' '
009100120517     c                   movel     tbeuni        dcur
009200120517     c*
009300120517    3c                   if        §curdde<=dataoggi
009400150625     c* Richiesta per terminal o filiale
009500150625   3ac                   if        §curapl='T' or §curapl='F'
009600120517     c
009700120517     c* carico i secondi livello a oggi
009800120517     C                   CLEAR                   trul06ds
009900120517    4c                   if        §curapl='T'
010000120517     C                   MOVE      '£1'          D06COD
010100120517     c                   else
010200120517     C                   MOVE      '£6'          D06COD
010300120517    4c                   endif
010400120517     C                   MOVEL     §curfil       D06KEY
010500120517     C                   MOVEL     §curdde       D06drf
010600120517     C                   MOVEL     trul06ds      KPJBU
010700120517     C*
010800120517     C                   CALL      'TRUL06R'
010900120517     C                   PARM                    KPJBA
011000120517     C                   MOVEL     KPJBU         trul06ds
011100120517
011200120517     c                   eval      xx=1
011300120517    4c                   dow       lin(xx)> 0
011400120517     C                   add       1             ff
011500120517     c                   eval      wfil(ff)=lin(xx)
011600120517     c                   eval      wlib(ff)=tbeke1
011700120517     c
011800120517     c                   add       1             xx
011900120517    4c                   enddo
012000150625    c
012100150625  x3ac                   else
012200150625     c* richiesta per area
012300150625     c                   exsr      carArea
012400120517     c
012500150625   3ac                   endif
012600150625    3c                   endif
012700120517    2c                   endif
012800120517     c*
012900120517     c     kcod          reade     tntbe01l
013000120517    1c                   enddo
013100120517     c
013200120523     c                   eval      ibs40drf=dataoggi
013300120517     c                   seton                                        LR
013400150625     C* Caricamento per Area  -------------------------------------*
013500150625     C     CarArea       BEGSR
013600150625     c* Prendo il codcie area della filiale
013700150625     c     §curfil       chain     azorg01l
013800150625    1c                   if        %found(azorg01l)
013900150625     c                   eval      kcar=orgcar
014000150625     c                   eval      kdiv=orgfl3
014100150625     c     korg2         setll     azorg02l
014200150625     c     korg2         reade     azorg02l
014300150625    2c                   dow       not %eof(azorg02l)
014400150625    3c                   if        (orgfag='A' or orgfag='F')
014500150625     c                             and orgfva=' '
014600150625     C                   add       1             ff
014700150625     c                   eval      wfil(ff)=orgfil
014800150625     c                   eval      wlib(ff)=tbeke1
014900150625    3c                   endif
015000150625     c
015100150625     c     korg2         reade     azorg02l
015200150625    2c                   enddo
015300150625     c
015400150625   x1c                   else
015500150625     C                   add       1             ff
015600150625     c                   eval      wfil(ff)=§curfil
015700150625     c                   eval      wlib(ff)=tbeke1
015800150625    1c                   endif
015900150625     c
016000150625     C                   ENDSR
016100150625     C* IMPOSTAZIONI INIZIALI -------------------------------------*
016200150625     C     *INZSR        BEGSR
016300120517     C     kazcln        klist
016400120517     C                   kfld                    ktfp
016500120517     C                   kfld                    ktfa
016600120517     C                   kfld                    kann
016700120517     C                   kfld                    kmes
016800150625     C     korg2         klist
016900150625     C                   kfld                    kdiv
017000150625     C                   kfld                    kcar
017100120517     C
017200120517     C                   ENDSR
