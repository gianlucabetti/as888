000100030206     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206
000300030206      *****************************************************************
000400030206      *                                                               *
000500030910      *       CREO FILE IN EDPPRVFAT PER PROVE FATTURAZIONE           *
000600030206      *                                                               *
000700030206      *****************************************************************
000800030206     fTitas38c  if   e           k Disk
000900030723     f***                                  Ignore(Titas010)
001000030206     f                                     Ignore(Titasp00)
001100030509     fTitas31c  if   e           k Disk
001200030723     f****                                 Ignore(Titas010)
001300030509     f                                     Ignore(Titasp00)
001400030509     f                                     Rename(Titas000:Titas31)
001500030723     f                                     Rename(Titas010:Titas311)
001600030206     fTita730c  if   e           k Disk
001700030206     f                                     Ignore(Tita7p00)
001800030206     fTitaa30c  if   e           k Disk
001900030206     f                                     Ignore(Titaap00)
002000030206     fTita430c  if   e           k Disk
002100030206     f                                     Ignore(Tita4p00)
002200030206     fTncsb03l  if   e           k Disk
002300040531     fFiar531c  if   e           k Disk
002400030910     f                                     Ignore(Fiar5p00)
002500050930     fFnevb04l  if   e           k Disk
002501171129     fFipnd01l  if   e           k Disk
002600030206
002700030206     fTita700f  uf a e           k Disk    Rename(Tita7000:Tita7)
002800030206     fTitaa00f  uf a e           k Disk    Rename(Titaa000:Titaa)
002900030206     fTita400f  uf a e           k Disk    Rename(Tita4000:Tita4)
003000030206     fTncsb00f  uf a e           k Disk    Rename(Tncsb000:Tncsb)
003100030910     fFiar500f  uf a e           k Disk    Rename(Fiar5000:Fiar5)
003200050930     fFnevb00f  uf a e           k Disk    Rename(Fnevb000:Fnevb)
003300030206     fTitas00f  uf a e           k Disk    Rename(Titas000:Titas)
003301171129     fFipnd00f  uf a e           k Disk    Rename(Fipnd000:Fipnd)
003400030206
003500030509     D KPJBA         E DS
003600030509      *
003700030509     d PARAM           DS
003800030509     d  PARKSC                        7  0
003900030509     d  PARDAS                        4  0
004000030509     d  PARDMS                        4  0
004100030509     d  PARAAS                        4  0
004200030509     d  PARAMS                        4  0
004300030509     d  PARLNP                        3  0
004400030509     d  PARLNA                        3  0
004500050930
004600050930     d Kcev            s                   like(evbcev) inz('RIC')
004700030509      *
004800030509     C     *ENTRY        PLIST
004900030509     C                   PARM                    KPJBA
005000030509     c                   movel     kpjbu         param
005100030509
005200030509     c     kTas38        Klist
005300030509     c                   Kfld                    Pardas
005400030509     c                   Kfld                    Pardms
005500030509
005600030509     c     kTas38K       Klist
005700030509     c                   Kfld                    Pardas
005800030509     c                   Kfld                    Pardms
005900030509     c                   Kfld                    Parksc
006000030206
006100030509     c     kTas31        Klist
006200030509     c                   Kfld                    Parksc
006300030509
006400030206     c     kTassa        Klist
006500030206     c                   Kfld                    TasAas
006600030206     c                   Kfld                    TasLnp
006700030206     c                   Kfld                    TasNrs
006800030206     c                   Kfld                    TasNsp
006900050930
007000050930     c     kFnevb        Klist
007100050930     c                   Kfld                    TasAas
007200050930     c                   Kfld                    TasLnp
007300050930     c                   Kfld                    TasNrs
007400050930     c                   Kfld                    TasNsp
007500050930     c                   Kfld                    Kcev
007600030206
007700030509      * Se richiesta codice cliente accendo il 51
007800030509      * altrimenti accendo il 52
007900030509
008000030509     c                   EVAL      *IN51 = (PARKSC > 0)
008100030509     c                   EVAL      *IN52 = (PARKSC = 0)
008200030910
008300030910      * se non sono state fatte delle selezioni vado a fine PGM
008400030206
008500030910     c                   if        pardas = 0 and paraas = 0 and parksc = 0  and
008600030910     c                             pardms = 0 and params = 0 and parlnp = 0  and
008700030910     c                             parlna = 0
008800030910     c                   goto      fine
008900030910     c                   endif
009000030910      * Leggo Titas
009100030509     C   51kTas31        Setll     Titas31c
009200030509     C   52kTas38        Setll     Titas38c
009300030910     c                   Do        *Hival
009400030509
009500030509     c   52              Read      Titas38c
009600030509     c   51KTAS31        Reade     Titas31c
009700030206
009800030509     c                   If        *IN52 AND %Eof(Titas38c)
009900030206     c                   Leave
010000030206     c                   EndIf
010100030509
010200030509     c                   If        *IN51 AND %Eof(Titas31c)
010300030509     c                   Leave
010400030509     c                   EndIf
010500030509      * verifico se record valido in base ai parametri che mi sono stati passati
010600030509      *
010700030509      * Anno di spedizione
010800030509     c                   if        pardas > 0 and tasaas < pardas
010900030509     c                   iter
011000030509     c                   endif
011100030509
011200030509     c                   if        paraas > 0 and tasaas > paraas
011300030509     c                   iter
011400030509     c                   endif
011500030509      * Mese giorno spedizione
011600030509     c                   if        pardms > 0 and tasmgs < pardms
011700030509     c                   iter
011800030509     c                   endif
011900030509
012000030509     c                   if        params > 0 and tasmgs > params
012100030509     c                   iter
012200030509     c                   endif
012300030509      * LNP e LNA
012400030509     c                   if        parlnp > 0 and taslnp <> parlnp
012500030509     c                   iter
012600030509     c                   endif
012700030509
012800030509     c                   if        parlna > 0 and taslna <> parlna
012900030509     c                   iter
013000030509     c                   endif
013100030206
013200030206      * Leggo Tita7
013300030206     c     kTassa        Setll     Tita730c
013400030206     c                   Do        *Hival
013500030206     c     kTassa        Reade     Tita730c
013600030206     c                   If        %Eof(Tita730c)
013700030206     c                   Leave
013800030206     c                   EndIf
013900030206      * Scrivo Tita7
014000030206     c                   Write     Tita7
014100030206     c                   EndDo
014200030206
014300030206      * Leggo Titaa
014400030206     c     kTassa        Setll     Titaa30c
014500030206     c                   Do        *Hival
014600030206     c     kTassa        Reade     Titaa30c
014700030206     c                   If        %Eof(Titaa30c)
014800030206     c                   Leave
014900030206     c                   EndIf
015000030206      * Scrivo Titaa
015100030206     c                   Write     Titaa
015200030206     c                   EndDo
015300030206
015400030206      * Leggo Tita4
015500030206     c     kTassa        Setll     Tita430c
015600030206     c                   Do        *Hival
015700030206     c     kTassa        Reade     Tita430c
015800030206     c                   If        %Eof(Tita430c)
015900030206     c                   Leave
016000030206     c                   EndIf
016100030206      * Scrivo Tita4
016200030206     c                   Write     Tita4
016300030206     c                   EndDo
016400030206
016500030206      * Leggo Tncsb
016600030206     c     kTassa        Chain     Tncsb03l
016700030206     c                   If        %Found(Tncsb03l)
016800030206      * Scrivo Tncsb
016900030206     c                   Write     Tncsb
017000030206     c                   EndIf
017100030910
017200030910      * Leggo Fiar5
017300040531     c     kTassa        Setll     Fiar531c
017400030910     c                   Do        *Hival
017500040531     c     kTassa        Reade     Fiar531c
017600040531     c                   If        %Eof(Fiar531c)
017700030910     c                   Leave
017800030910     c                   EndIf
017900030910      * Scrivo Fiar5
018000030910     c                   Write     Fiar5
018100030910     c                   EndDo
018200050930
018300050930      * Leggo Fnevb
018400050930     c     kFnevb        Chain     Fnevb04l
018500050930     c                   If        %Found(Fnevb04l)
018600050930      * Scrivo Fnevb
018700050930     c                   Write     Fnevb
018800050930     c                   EndIf
018801171129
018802171129      * Leggo Fipnd
018803171129     c     kTassa        Chain     Fipnd01l
018804171129     c                   If        %Found(Fipnd01l)
018805171129      * Scrivo Fnevb
018806171129     c                   Write     Fipnd
018807171129     c                   EndIf
018900030206
019000030206      * Scrivo Titas
019100030206     c                   Write     Titas
019200030206
019300030206     c                   EndDo
019400030910
019500030910     c     Fine          Tag
019600030206     c                   Eval      *InLr = *On
