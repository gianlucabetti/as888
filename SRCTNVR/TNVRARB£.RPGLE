000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300100111     Ftabel00f  iF   E           K DISK
000400100111     Fazcln01l  iF   E           K DISK
000500100111     FFNarb27L  iF   E           K DISK
000600100111     FFNarb01L  uF   E           K DISK    rename(fnarb000:fnarb001)
000700100111     f                                     prefix(x_)
000800100111     FFNblp01L  uF   E           K DISK
000900091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000091125
001100100111
001200100111     d ClnMat          ds
001300100111     d  Mat                    1     31    Dim(31)
001400100111     d ClnPom          ds
001500100111     d  Pom                    1     31    Dim(31)
001600100111
001700100111     dkpjba          e ds
001800100111     dtnsd99ds       e ds
001900100111     dds1y           e ds
002000100111     d Wdata           s               d   Datfmt(*iso) inz
002100100111     d ktfp            s                   like(clntfp) inz
002200100111     d ktfa            s                   like(clntfa) inz
002300100111     d kann            s                   like(clnann) inz
002400100111     d kmes            s                   like(clnmes) inz
002500100111     d Wgiorno         s              2  0 inz
002600100111     d COD             s                   like(TBLCOD) inz
002700100111     d CODUT           s                   like(TBLkut) inz(1)
002800100111     d KEY             s                   like(TBLKEY) inz
002900100111     d totale          s              7  0
003000021214      *--------------------------------------------------
003100100111     C     *ENTRY        PLIST
003200100111     C                   PARM                    kpjba
003300100111     c
003400100111     C                   movel     kpjbu         giorniadd         2
003500100111     c
003600100111     c                   if        giorniadd>*zeros
003700100111     c                   movel     giorniadd     wtotgg            2 0
003800100111     c                   else
003900100111     c                   eval      wtotgg=2
004000100111     c
004100100111     c                   endif
004200100111     c     kblp          klist
004300100111     c                   kfld                    arbaas
004400100111     c                   kfld                    arblnp
004500100111     c                   kfld                    arbnrs
004600100111     c                   kfld                    arbnsp
004700100111     c     karb          klist
004800091125     c                   kfld                    lnp               3 0
004900091125     c                   kfld                    aas               4 0
005000091125     c                   kfld                    mgs               4 0
005100100111     C
005200100111     C     KTAB          KLIST
005300100111     C                   KFLD                    CODut
005400100111     C                   KFLD                    COD
005500100111     C                   KFLD                    KEY
005600100111     c     Kazcln        Klist
005700100111     c                   Kfld                    Ktfp
005800100111     c                   Kfld                    Ktfa
005900100111     c                   Kfld                    Kann
006000100111     c                   Kfld                    Kmes
006100100111     C*
006200100111     c                   z-add     255           lnp
006300100111     c                   z-add     2010          aas
006400100111     c                   z-add     0107          mgs
006500100111     c
006600100111     c                   except    testata
006700100111
006800100111     c     karb          setll     fnarb27l
006900100111     c     karb          reade     fnarb27l
007000051110
007100100111    1c                   DOW       not %EOF(FNarb27l)
007200100111     c*
007300100111     c                   if        arbksc=0633315 and arbduc>20100107
007400100111     c                             and  arbtcr='D'
007500100111     c
007600100111     c* elaboora bolla
007700100111     c                   exsr      Elabolla
007800100111     c                   endif
007900091125     c
008000100111     c     karb          reade     fnarb27l
008100091125     c                   enddo
008200021216      *
008300100111     c                   except    fine
008400100111      *
008500021214     C                   eval       *inlr = *on
008600100111     c*----------------------------------------------------------
008700100111     c     Elabolla      BEGSR
008800100111     c*  Col  tnsd99 verifico che data cons richeista mettere
008900100111     c                   clear                   tnsd99ds
009000100111     c                   eval      d98tbo='A'
009100100111     c                   eval      d98aas=arbaas
009200100111     c                   eval      d98lnp=arblnp
009300100111     c                   eval      d98nrs=arbnrs
009400100111     c                   eval      d98nsp=arbnsp
009500100111     C                   CALL      'TNSD99R'
009600100111     C                   PARM                    tnsd99ds
009700100111     c
009800100111     c* richiamo di nuovo, togliendo la consegna richiesta
009900100111     c                   clear                   d98dcr
010000100111     c                   clear                   d98tcr
010100100111     c                   clear                   d98hcr
010200100111     c                   clear                   d98nsp
010300100111     c                   clear                   d98dti
010400100111     c                   clear                   d98hti
010500100111     C                   CALL      'TNSD99R'
010600100111     C                   PARM                    tnsd99ds
010700100111     C*
010800100111     c* In base alla consegna prevista, imposto la consegna richiesta
010900100111      * Calcolo la data eliminando i gg. che dal calendario risultano festivi
011000100111     c     *iso          Movel     d98dei        wdata
011100100111     c                   Clear                   Ktfp
011200100111     c                   Movel     arblna        Ktfa
011300100111     c                   Movel(p)  '1Y'          Cod
011400100111
011500100111Do  1c                   Do        wtotgg
011600100111     c     Festav        Tag
011700100111     c                   adddur    1:*D          Wdata
011800100111     c                   Extrct    Wdata:*y      Kann
011900100111     c                   Extrct    Wdata:*m      Kmes
012000100111     c                   Extrct    Wdata:*d      Wgiorno
012100100111     c     Kazcln        Chain     Azcln01l
012200100111If  2c                   If        %Found(Azcln01l)
012300100111If  3c                   If        Mat(Wgiorno) <> *blanks
012400100111     c                   Movel     Mat(Wgiorno)  Key
012500100111     c     Ktab          Chain     Tabel00f
012600100111If  4c                   If        %Found(Tabel00f)
012700100111     c                   Movel     Tbluni        Ds1y
012800100111E   4c                   EndIf
012900100111If  4c                   If        §1yfei = 'S'
013000100111     c                   Goto      Festav
013100100111E   4c                   EndIf
013200100111E   3c                   EndIf
013300100111If  3c                   If        Pom(Wgiorno) <> *blanks
013400100111     c                   Movel     Pom(Wgiorno)  Key
013500100111     c     Ktab          Chain     Tabel00f
013600100111If  4c                   If        %Found(Tabel00f)
013700100111     c                   Movel     Tbluni        Ds1y
013800100111E   4c                   EndIf
013900100111If  4c                   If        §1yfei = 'S'
014000100111     c                   Goto      Festav
014100100111E   4c                   EndIf
014200100111E   3c                   EndIf
014300100111E   2c                   EndIf
014400100111E   1c                   EndDo
014500100111
014600100111     c                   Move      Wdata         wdcr              8 0
014700100111     c
014800100111     c                   except    bolla
014900100111     c                   add       1             totale
015000100111     c* Aggiorno bolla
015100100111     c     kblp          chain     fnarb01l
015200100111     c                   if        %found(fnarb01l)
015300100111     c                   eval      x_arbdcr=wdcr
015400100111     c                   eval      x_arbtcr='P'
015500100111     c
015600100111     c                   update    fnarb001
015700100111     c                   endif
015800100111     c
015900100111     c* Aggancio blp per stesso aggiornamento
016000100111     c     kblp          chain     fnblp01l
016100100111     c
016200100111     c                   if        %found(fnblp01l)
016300100111     c* Aggiorno bolla
016400100111     c                   eval      blpdcr=wdcr
016500100111     c                   eval      blptcr='P'
016600100111     c
016700100111     c                   update    fnblp000
016800100111     c                   endif
016900100111     c
017000100111     c                   ENDSR
017100100111     c
017200100111     OQSYSPRT   E            TESTATA     1  1
017300100111     O                                              'AGGIORNAMENTO DATA CONS.'
017400100111     O                                        +   0 'RICHIESTA BOLLE DEL '
017500100111     O                                        +   0 'CLIENTE 0633315  '
017600100111     OQSYSPRT   E            bolla       1
017700100111     O                                              'Bolla:'
017800100111     O                       arbaas           +   1
017900100111     O                       arblnp           +   1
018000100111     O                       arbnrs           +   1
018100100111     O                       arbnsp        2  +   1
018200100111     O                       arblna           +   1
018300100111     O                                        +   5 'Data partenza'
018400100111     O                       arbduc           +   1 '    /  /  '
018500100111     O                                        +   5 'Data cons richiesta'
018600100111     O                                        +   2 'PRIMA DEL '
018700100111     O                       wdcr             +   1 '    /  /  '
018800100111     OQSYSPRT   E            FINE        2
018900100111     O                                              'Totale bolle aggiornate:'
019000100111     O                       totale        2  +   2
