000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200100317     Ffiar500f  uF   E             DISK    rename(fiar5000:fiar50fis)
000300100317     Ffiar531C  uF   E           K DISK    infds(ar501)
000400100317     FTITAS33C  iF   E           K DISK
000500100318     F                                     IGNORE(TITASP00)
000600090715     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000700090715     d
000800100317     d dAr5GEN       e ds                  Inz
000900100317     d KAr5Trd         s                   Like(Ar5Trd) Inz('GEN')
001000100317     d ST106           s            106
001100100317     d ST40            s             40
001200100317     d
001300100317     D ar501           DS
001400100317     D  ar5NRR               397    400B 0
001500100317
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
001900100317     c     *entry        plist
002000100317     c                   parm                    alfaksc           7
002100100318     c                   parm                    Aggiorna          1
002200100317     c
002300100317     c                   if        alfaksc>*zeros
002400100317     c                   movel     alfaksc       kksc
002500100317     c                   endif
002600100317     c
002700090715     c     ktas          klist
002800100317     c                   kfld                    kksc              7 0
002900100317     c                   kfld                    anno              4 0
003000090715     c                   kfld                    mmgg              4 0
003100100317     c
003200100317     c     ktas2         klist
003300100317     c                   kfld                    kksc              7 0
003400100317     c                   kfld                    anno              4 0
003500090715     c
003600100317     c                   z-add     2010          anno
003700100318     c                   z-add     0101          mmgg
003800100317     c
003900090715     c     KeyFiar5      Klist
004000090715     c                   Kfld                    tasAas
004100090715     c                   Kfld                    tasLnp
004200090715     c                   Kfld                    tasNrs
004300090715     c                   Kfld                    tasNsp
004400090715     c                   Kfld                    KAr5Trd
004500011003     C*
004600100317     C     ktas          SETLL     TITAS33c
004700100317     C     ktas2         reade     TITAS33c
004800090715     c
004900100317    1c                   dow       not %eof(titas33c)
005000090715     c
005100100317    2c                   if        tasctm<>'4 ' and tasctm<>'4F'
005200100317     c                             and tasgma='C1'
005300100317     c
005400090715     c* Controllo che non esiste già
005500100317     c                   clear                   conta             4 0
005600100317     c                   clear                   delrec            1
005700100317     c                   clear                   st106
005800100317     c                   clear                   st40
005900100317     c                   clear                   DRDT1             8
006000100318     c                   clear                   fDT1              1
006100100317     c                   clear                   DRDT2             8
006200100318     c                   clear                   fDT2              1
006300100317     c                   clear                   savnrr            9 0
006400100317     c
006500100317     c     keyFiar5      setll     Fiar531c
006600100318     c     keyFiar5      reade(n)  Fiar531c
006700100317    3c                   dow       not %eof(fiar531c)
006800100317     c                   eval      conta=conta+1
006900100317     c
007000100317     c* Verifico se quello che ha la data impostata ha tutto il resto vuoto
007100100317     c                   movel     ar5uni        dar5gen
007200100317    4c                   if        §ar5drdt<>*blanks
007300100317     c
007400100317     c* salvo le  date DDt se presenti neei primi 2 record
007500100317     c                   if        DRDT1=*blanks
007600100317     c                   movel     §ar5DRDT      drdt1
007700100318     c                   movel     §ar5fstd      fdt1
007800100317     c                   else
007900100317     c                   movel     §ar5DRDT      drdt2
008000100318     c                   movel     §ar5fstd      fdt2
008100100317     c                   endif
008200100317     c
008300100317    5c                   if        %subst(aR5uni: 1: 106)=*blanks  and
008400100317     c                              %subst(aR5uni: 116:30)=*blanks
008500100317     c                   eval      savnrr=ar5nrr
008600100317   x5c                   else
008700100317     c                   eval      st106=%subst(aR5uni: 1: 106)
008800100317     c                   eval      st40 =%subst(aR5uni: 116:40)
008900100317    5c                   endif
009000100317    4c                   endif
009100100317     c
009200100318     c     keyFiar5      reade(n)  Fiar531c
009300100317    3c                   enddo
009400100317     c
009500100317     c* Se ne ho trovato più di uno, quello coonla data deve essere vuoto
009600100317     c*  a parte la data
009700100317    3c                   select
009800100317    3c                   when      conta>2
009900100317     c                   except    Error2
010000100317     c*
010100100317    3c                   when      conta=2
010200100317    4c                   if        st106<>*blanks or st40<>*blanks
010300100317     c                   except    Error
010400100317   x4c                   else
010500100317     c
010600100317     c* Stampo il record da deletare
010700100317     c     savnrr        chain     fiar50fis
010800100317     c** Lo stampo
010900100317     c                   eval      st106=%subst(aR5uni: 1: 106)
011000100317     c                   eval      st40 =%subst(aR5uni: 107:40)
011100100317     c                   except    ar5del
011200100317     c*  lo deleto
011300100318     c                   if        Aggiorna='S'
011400100318     c                   delete    fiar50fis
011500100318     c                   endif
011600100317     c
011700100317     c** Chaino il record buono del GEN e lo aggiorno per la data se manca
011800100318     c                   if        Aggiorna = 'S'
011900100317     c                   exsr      AggioGEN
012000100317    4c                   endif
012100100318    4c                   endif
012200100317     c*
012300100317    3c                   when      conta=1
012400100318     c
012500100318     c* Se ha già la data, vado oltre
012600100317     c** Chaino il record buono del GEN e lo aggiorno per la data se manca
012700100318    7c                   if        drdt1=*blanks and drdt2=*blanks
012800100318     c                   if        Aggiorna = 'S'
012900100317     c                   exsr      AggioGEN
013000100318     c                   endif
013100100318     c                   endif
013200100318     c
013300100317    3c                   ENDSL
013400100317     c                   endif
013500090715     C*
013600100318     C     ktas2         reade     TITAS33c
013700090715    1C                   ENDDO
013800011003     C
013900011003     C                   SETON                                        LR
014000100317     c*------------------------------------------------------------------------
014100100317     c     AggioGEN      BEGSR
014200100317     c     keyFiar5      chain     Fiar531c
014300100317    5c                   if        %found(fiar531c)
014400100317     c
014500100317     c                   movel     ar5uni        dar5gen
014600100317     c                   eval      st106=%subst(aR5uni: 1: 106)
014700100317     c                   eval      st40 =%subst(aR5uni: 107:40)
014800100317     c                   select
014900100317     c* data OK
015000100317    6c                   when      §ar5drdt<>*blanks and §ar5drdt<>'99999999'
015100100317     c
015200100317     c* verifico se c'e' data buona da mettere
015300100317    6c                   when      §ar5drdt='99999999'
015400100317     c                   except    Aggdel
015500100318    7c                   if        drdt1<>*blanks and drdt1<>'99999999'
015600100317     c                   movel     drdt1         §ar5drdt
015700100318     c                   movel     fdt1          §ar5fstd
015800100317   x7c                   else
015900100318    8c                   if        drdt2<>*blanks and drdt2<>'99999999'
016000100317     c                   movel     drdt2         §ar5drdt
016100100318     c                   movel     fdt2          §ar5fstd
016200100317    8c                   endif
016300100317    7c                   endif
016400100317     c
016500100317    6c                   when      §ar5drdt=*blanks
016600100317     c                   except    Aggdel
016700100318    7c                   if        drdt1<>*blanks and drdt1<>'99999999'
016800100317     c                   movel     drdt1         §ar5drdt
016900100318     c                   movel     fdt1          §ar5fstd
017000100317   x7c                   else
017100100318    8c                   if        drdt2<>*blanks and drdt2<>'99999999'
017200100317     c                   movel     drdt2         §ar5drdt
017300100318     c                   movel     fdt2          §ar5fstd
017400100317   x8c                   else
017500100318     c                   movel     '99999999'    §ar5drdt
017600100317    8c                   endif
017700100317    7c                   endif
017800100317    6c                   endsl
017900100317     c
018000100317     c                   movel     dar5gen       ar5uni
018100100318     c                   update    fiar5000
018200100317   x5c                   else
018300100317     c                   except    NoAr5
018400100317    5c                   endif
018500100317     c                   ENDSR
018600100317     OPRTF198   E            Error       1
018700100317     O                       tasaas           +   1
018800100317     O                       taslnp           +   1
018900100317     O                       tasnrs           +   1
019000100317     O                       tasnsp        2  +   1
019100100317     o                                        +   1 'err GEN'
019200100317     O                       st106            +   1
019300100317     O                       st40             +   0
019400100317     OPRTF198   E            Error2      1
019500100317     O                       tasaas           +   1
019600100317     O                       taslnp           +   1
019700100317     O                       tasnrs           +   1
019800100317     O                       tasnsp        2  +   1
019900100317     o                                        +   1 'più di 2 record GEN!!'
020000100317     OPRTF198   E            ar5del      1
020100100317     O                       tasaas           +   1
020200100317     O                       taslnp           +   1
020300100317     O                       tasnrs           +   1
020400100317     O                       tasnsp        2  +   1
020500100317     o                                        +   1 'del GEN'
020600100317     O                       st106            +   1
020700100317     O                       st40             +   0
020800100317     OPRTF198   E            Noar5       1
020900100317     O                       tasaas           +   1
021000100317     O                       taslnp           +   1
021100100317     O                       tasnrs           +   1
021200100317     O                       tasnsp        2  +   1
021300100317     o                                        +   1 'Non trovato AR5 GEN ??!'
021400100317     OPRTF198   E            Aggdel      1
021500100317     O                       tasaas           +   1
021600100317     O                       taslnp           +   1
021700100317     O                       tasnrs           +   1
021800100317     O                       tasnsp        2  +   1
021900100317     o                                        +   1 'Agg GEN'
022000100317     O                       st106            +   1
022100100317     O                       st40             +   0
