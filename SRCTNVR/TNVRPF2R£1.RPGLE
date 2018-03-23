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
002500030206
002600030206     fTita700f  uf a e           k Disk    Rename(Tita7000:Tita7)
002700030206     fTitaa00f  uf a e           k Disk    Rename(Titaa000:Titaa)
002800030206     fTita400f  uf a e           k Disk    Rename(Tita4000:Tita4)
002900030206     fTncsb00f  uf a e           k Disk    Rename(Tncsb000:Tncsb)
003000030910     fFiar500f  uf a e           k Disk    Rename(Fiar5000:Fiar5)
003100030206     fTitas00f  uf a e           k Disk    Rename(Titas000:Titas)
003200030206
003300030509     D KPJBA         E DS
003400030509      *
003500030509     d PARAM           DS
003600030509     d  PARKSC                        7  0
003700030509     d  PARDAS                        4  0
003800030509     d  PARDMS                        4  0
003900030509     d  PARAAS                        4  0
004000030509     d  PARAMS                        4  0
004100030509     d  PARLNP                        3  0
004200030509     d  PARLNA                        3  0
004300030509      *
004400030509     C     *ENTRY        PLIST
004500030509     C                   PARM                    KPJBA
004600030509     c                   movel     kpjbu         param
004700030509
004800030509     c     kTas38        Klist
004900030509     c                   Kfld                    Pardas
005000030509     c                   Kfld                    Pardms
005100030509
005200030509     c     kTas38K       Klist
005300030509     c                   Kfld                    Pardas
005400030509     c                   Kfld                    Pardms
005500030509     c                   Kfld                    Parksc
005600030206
005700030509     c     kTas31        Klist
005800030509     c                   Kfld                    Parksc
005900030509
006000030206     c     kTassa        Klist
006100030206     c                   Kfld                    TasAas
006200030206     c                   Kfld                    TasLnp
006300030206     c                   Kfld                    TasNrs
006400030206     c                   Kfld                    TasNsp
006500030206
006600030509      * Se richiesta codice cliente accendo il 51
006700030509      * altrimenti accendo il 52
006800030509
006900030509     c                   EVAL      *IN51 = (PARKSC > 0)
007000030509     c                   EVAL      *IN52 = (PARKSC = 0)
007100030910
007200030910      * se non sono state fatte delle selezioni vado a fine PGM
007300030206
007400030910     c                   if        pardas = 0 and paraas = 0 and parksc = 0  and
007500030910     c                             pardms = 0 and params = 0 and parlnp = 0  and
007600030910     c                             parlna = 0
007700030910     c                   goto      fine
007800030910     c                   endif
007900030910      * Leggo Titas
008000030509     C   51kTas31        Setll     Titas31c
008100030509     C   52kTas38        Setll     Titas38c
008200030910     c                   Do        *Hival
008300030509
008400030509     c   52              Read      Titas38c
008500030509     c   51KTAS31        Reade     Titas31c
008600030206
008700030509     c                   If        *IN52 AND %Eof(Titas38c)
008800030206     c                   Leave
008900030206     c                   EndIf
009000030509
009100030509     c                   If        *IN51 AND %Eof(Titas31c)
009200030509     c                   Leave
009300030509     c                   EndIf
009400030509      * verifico se record valido in base ai parametri che mi sono stati passati
009500030509      *
009600030509      * Anno di spedizione
009700030509     c                   if        pardas > 0 and tasaas < pardas
009800030509     c                   iter
009900030509     c                   endif
010000030509
010100030509     c                   if        paraas > 0 and tasaas > paraas
010200030509     c                   iter
010300030509     c                   endif
010400030509      * Mese giorno spedizione
010500030509     c                   if        pardms > 0 and tasmgs < pardms
010600030509     c                   iter
010700030509     c                   endif
010800030509
010900030509     c                   if        params > 0 and tasmgs > params
011000030509     c                   iter
011100030509     c                   endif
011200030509      * LNP e LNA
011300030509     c                   if        parlnp > 0 and taslnp <> parlnp
011400030509     c                   iter
011500030509     c                   endif
011600030509
011700030509     c                   if        parlna > 0 and taslna <> parlna
011800030509     c                   iter
011900030509     c                   endif
012000030206
012100030206      * Leggo Tita7
012200030206     c     kTassa        Setll     Tita730c
012300030206     c                   Do        *Hival
012400030206     c     kTassa        Reade     Tita730c
012500030206     c                   If        %Eof(Tita730c)
012600030206     c                   Leave
012700030206     c                   EndIf
012800030206      * Scrivo Tita7
012900030206     c                   Write     Tita7
013000030206     c                   EndDo
013100030206
013200030206      * Leggo Titaa
013300030206     c     kTassa        Setll     Titaa30c
013400030206     c                   Do        *Hival
013500030206     c     kTassa        Reade     Titaa30c
013600030206     c                   If        %Eof(Titaa30c)
013700030206     c                   Leave
013800030206     c                   EndIf
013900030206      * Scrivo Titaa
014000030206     c                   Write     Titaa
014100030206     c                   EndDo
014200030206
014300030206      * Leggo Tita4
014400030206     c     kTassa        Setll     Tita430c
014500030206     c                   Do        *Hival
014600030206     c     kTassa        Reade     Tita430c
014700030206     c                   If        %Eof(Tita430c)
014800030206     c                   Leave
014900030206     c                   EndIf
015000030206      * Scrivo Tita4
015100030206     c                   Write     Tita4
015200030206     c                   EndDo
015300030206
015400030206      * Leggo Tncsb
015500030206     c     kTassa        Chain     Tncsb03l
015600030206     c                   If        %Found(Tncsb03l)
015700030206      * Scrivo Tncsb
015800030206     c                   Write     Tncsb
015900030206     c                   EndIf
016000030910
016100030910      * Leggo Fiar5
016200040531     c     kTassa        Setll     Fiar531c
016300030910     c                   Do        *Hival
016400040531     c     kTassa        Reade     Fiar531c
016500040531     c                   If        %Eof(Fiar531c)
016600030910     c                   Leave
016700030910     c                   EndIf
016800030910      * Scrivo Fiar5
016900030910     c                   Write     Fiar5
017000030910     c                   EndDo
017100030206
017200030206      * Scrivo Titas
017300030206     c                   Write     Titas
017400030206
017500030206     c                   EndDo
017600030910
017700030910     c     Fine          Tag
017800030206     c                   Eval      *InLr = *On
