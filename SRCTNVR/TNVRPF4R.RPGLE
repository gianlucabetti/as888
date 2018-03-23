000100030206     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206
000300030206      *****************************************************************
000400030206      *                                                               *
000500050210      *       CREO FILE IN EDPPRVgia PER PROVE giacenze in uni        *
000600030206      *                                                               *
000700030206      *****************************************************************
000800050207     fTitas38c  if   e           k Disk    usropn
001100050207     fTitas31c  if   e           k Disk    usropn
001400030509     f                                     Rename(Titas000:Titas31)
001500030723     f                                     Rename(Titas010:Titas311)
001501050207     f                                     Rename(Titasp00:Titas31p)
001502050207     fTitas30c  if   e           k Disk    usropn
001503050207     f                                     Rename(Titas000:Titas30)
001504050207     f                                     Rename(Titas010:Titas301)
001505050207     f                                     Rename(Titasp00:Titas30p)
001600030206     fTita730c  if   e           k Disk
001800030206     fTitaa30c  if   e           k Disk
002000030206     fTita430c  if   e           k Disk
002200030206     fTncsb03l  if   e           k Disk
002300040531     fFiar531c  if   e           k Disk
002301050207     fFigcp01l  if   e           k Disk
002302050207     fFignp01l  if   e           k Disk
002303050207     fFigap01l  if   e           k Disk
002500030206
002600030206     fTita700f  uf a e           k Disk    Rename(Tita7000:Tita7)
002601050207     fTita7p0f  uf a e           k Disk    Rename(Tita7p00:Tita7p)
002700030206     fTitaa00f  uf a e           k Disk    Rename(Titaa000:Titaa)
002701050207     fTitaap0f  uf a e           k Disk    Rename(Titaap00:Titaap)
002800030206     fTita400f  uf a e           k Disk    Rename(Tita4000:Tita4)
002801050207     fTita4p0f  uf a e           k Disk    Rename(Tita4p00:Tita4p)
002900030206     fTncsb00f  uf a e           k Disk    Rename(Tncsb000:Tncsb)
003000030910     fFiar500f  uf a e           k Disk    Rename(Fiar5000:Fiar5)
003001050207     fFiar5p0f  uf a e           k Disk    Rename(Fiar5p00:Fiar5p)
003100030206     fTitas00f  uf a e           k Disk    Rename(Titas000:Titas)
003101050207     fTitas10f  uf a e           k Disk    Rename(Titas010:Titas10)
003102050207     fTitasp0f  uf a e           k Disk    Rename(Titasp00:Titasp)
003103050207     ffigcp00f  uf a e           k Disk    Rename(figcp000:figcp)
003104050207     ffignp00f  uf a e           k Disk    Rename(fignp000:fignp)
003105050207     ffigap00f  uf a e           k Disk    Rename(figap000:figap)
003200030206
003201050207     d savagc          s                   like(gcpagc)
003202050207     d savfgc          s                   like(gcpfgc)
003203050207     d savngc          s                   like(gcpngc)
003300030509     D KPJBA         E DS
003400030509      *
003500030509     d PARAM           DS
003600050207     d  PARKSC                        7s 0
003700050207     d  PARDAS                        4s 0
003800050207     d  PARDMS                        4s 0
003900050207     d  PARAAS                        4s 0
004000050207     d  PARAMS                        4s 0
004100050207     d  PARLNP                        3s 0
004200050207     d  PARLNA                        3s 0
004201050207     d  PARPUL                        2
004202050207     d  PARINV                        2
004203050207     d  PARaass                       4s 0
004204050207     d  PARlnps                       3s 0
004205050207     d  PARnrss                       2s 0
004206050207     d  PARnsps                       7s 0
004207050207     d  PARTPL                        1
004302050207      * Predisposizione indicatori per riconoscere i tipi record
004303050207      *   reperiti dal file TITAS30C/31C/38C
004304050207     iTITAS000      31
004305050207     iTITAS010      32
004306050207     iTITASP00      33
004307050207     iTITAS31       31
004308050207     iTITAS311      32
004309050207     iTITAS31p      33
004310050207     iTITAS30       31
004311050207     iTITAS301      32
004312050207     iTITAS30p      33
004313050207      *   reperiti dal file TITA730C
004314050207     iTITA7000      34
004315050207     iTITA7p00      35
004316050207      *   reperiti dal file TITAA30C
004317050207     iTITAa000      36
004318050207     iTITAap00      37
004319050207      *   reperiti dal file TITA430C
004320050207     iTITA4000      38
004321050207     iTITA4p00      39
004322050207      *   reperiti dal file fiar531c
004323050207     ifiar5000      40
004324050207     ifiar5p00      41
004325050207      *
004400030509     C     *ENTRY        PLIST
004500030509     C                   PARM                    KPJBA
004600030509     c                   movel     kpjbu         param
004601050210     c* chiudo tutto
004602050210     c                   if        partpl = 'C'
004603050210     c                   goto      fine
004604050210     c                   endif
004605050207      * se non sono state fatte delle selezioni vado a fine PGM
004606050207     c                   if        pardas = 0 and paraas = 0 and parksc = 0  and
004607050207     c                             pardms = 0 and params = 0 and parlnp = 0  and
004608050207     c                             parlna = 0 and paraass=0 and parlnps=0
004609050207     c                             and parnrss= 0 and parnsps=0
004610050207     c                   goto      fine
004611050207     c                   endif
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
006401050207
006402050207     c     kTas30        Klist
006403050207     c                   Kfld                    parAass
006404050207     c                   Kfld                    parLnps
006405050207     c                   Kfld                    parNrss
006406050207     c                   Kfld                    parNsps
006407050207
006408050207     c     kfigcp        Klist
006409050209     c                   Kfld                    GCPAGC
006410050209     c                   Kfld                    GCPFGC
006411050209     c                   Kfld                    GCPNGC
006500030206
006600050207      * Se richiesta spedizione accendo il 50
006601050207      * Se richiesta codice cliente accendo il 51
006700030509      * altrimenti accendo il 52
006800030509
006900050207     c                   EVAL      *IN50 = (PARaass > 0)
006901050207     c                   EVAL      *IN51 = (PARKSC > 0)
007000050207     c  n50              EVAL      *IN52 = (PARKSC = 0)
007001050207     c* apro titas
007002050207     c                   select
007003050207     c                   when      *in50 and not %open(titas30c)
007004050207     c                   open      titas30c
007005050207     c                   when      *in51 and not %open(titas31c)
007006050207     c                   open      titas31c
007007050207     c                   when      *in52 and not %open(titas38c)
007008050207     c                   open      titas38c
007009050207     c                   endsl
007010050207     c**********************************************************************
007900030910      * Leggo Titas
007901050207     c**********************************************************************
008000050207     C   50kTas30        Setll     Titas30c
008001050207     C   51kTas31        Setll     Titas31c
008100030509     C   52kTas38        Setll     Titas38c
008200030910     c                   Do        *Hival
008300030509
008301050207     c                   setoff                                       313233
008400030509     c   52              Read      Titas38c
008500030509     c   51KTAS31        Reade     Titas31c
008501050207     c   50KTAS30        Reade     Titas30c
008600030206
008700030509     c                   If        *IN52 AND %Eof(Titas38c)
008800030206     c                   Leave
008900030206     c                   EndIf
009000030509
009100030509     c                   If        *IN51 AND %Eof(Titas31c)
009200030509     c                   Leave
009300030509     c                   EndIf
009301050207
009302050207     c                   If        *IN50 AND %Eof(Titas30c)
009303050207     c                   Leave
009304050207     c                   EndIf
009305050210     c* escludo il tipo bolla RECUPERO perchè potrebbe duplicare gli archivi
009306050210     c* inutilmente visto che per tali bolle non si può aprire giacenza
009307050210     c                   if        tastbl = 'AP' or
009308050210     c                             tastbl = 'AR' or
009309050210     c                             tastbl = 'A3' or
009310050210     c                             tastbl = 'FC' or
009311050210     c                             tastbl = 'F3'
009312050210     c                   iter
009313050210     c                   end
009400030509      * verifico se record valido in base ai parametri che mi sono stati passati
009500050207      * eccetto il caso di scelta spedizione
009501050207     c                   if        paraass = 0
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
011901050207     c                   endif
012001050207     c**********************************************************************
012100030206      * Leggo Tita7
012101050207     c**********************************************************************
012200030206     c     kTassa        Setll     Tita730c
012300030206     c                   Do        *Hival
012301050207     c                   setoff                                       3435
012400030206     c     kTassa        Reade     Tita730c
012500030206     c                   If        %Eof(Tita730c)
012600030206     c                   Leave
012700030206     c                   EndIf
012800030206      * Scrivo Tita7
012900050207     c   34              Write     Tita7
012901050207     c   35              Write     Tita7p
013000030206     c                   EndDo
013101050207     c**********************************************************************
013200030206      * Leggo Titaa
013201050207     c**********************************************************************
013300030206     c     kTassa        Setll     Titaa30c
013400030206     c                   Do        *Hival
013401050207     c                   setoff                                       3637
013500030206     c     kTassa        Reade     Titaa30c
013600030206     c                   If        %Eof(Titaa30c)
013700030206     c                   Leave
013800030206     c                   EndIf
013900030206      * Scrivo Titaa
014000050207     c   36              Write     Titaa
014001050207     c   37              Write     Titaap
014100030206     c                   EndDo
014201050207     c**********************************************************************
014300030206      * Leggo Tita4
014301050207     c**********************************************************************
014400030206     c     kTassa        Setll     Tita430c
014500030206     c                   Do        *Hival
014501050207     c                   setoff                                       3839
014600030206     c     kTassa        Reade     Tita430c
014700030206     c                   If        %Eof(Tita430c)
014800030206     c                   Leave
014900030206     c                   EndIf
015000030206      * Scrivo Tita4
015100050207     c   38              Write     Tita4
015101050207     c   39              Write     Tita4p
015200030206     c                   EndDo
015301050207     c**********************************************************************
015400030206      * Leggo Tncsb
015401050207     c**********************************************************************
015500030206     c     kTassa        Chain     Tncsb03l
015600030206     c                   If        %Found(Tncsb03l)
015700030206      * Scrivo Tncsb
015800030206     c                   Write     Tncsb
015900030206     c                   EndIf
016001050207     c**********************************************************************
016100030910      * Leggo Fiar5
016101050207     c**********************************************************************
016200040531     c     kTassa        Setll     Fiar531c
016300030910     c                   Do        *Hival
016301050207     c                   setoff                                       4041
016400040531     c     kTassa        Reade     Fiar531c
016500040531     c                   If        %Eof(Fiar531c)
016600030910     c                   Leave
016700030910     c                   EndIf
016800030910      * Scrivo Fiar5
016900050207     c   40              Write     Fiar5
016901050207     c   41              Write     Fiar5p
017000030910     c                   EndDo
017100050207     c**********************************************************************
017101050207      * GIACENZE
017102050207     c**********************************************************************
017103050207     c     ktas30        Setll     Figcp01l
017104050207     c                   Do        *Hival
017105050207     c     ktas30        Reade     Figcp01l
017106050207     c                   If        %Eof(Figcp01l)
017107050207     c                   Leave
017108050207     c                   EndIf
017111050207      * a rottura di giacenza leggo Fignp01l
017112050207     c                   if        gcpagc <> savagc or gcpfgc <> savfgc or
017113050207     c                             gcpngc <> savngc
017114050207     c     kfigcp        Setll     Fignp01l
017115050207     c                   Do        *Hival
017116050207     c     kfigcp        Reade     Fignp01l
017117050207     c                   If        %Eof(Fignp01l)
017118050207     c                   Leave
017119050207     c                   EndIf
017120050207      * Scrivo Figcp
017121050207     c                   Write     Fignp
017122050207     c                   EndDo
017123050207     c                   eval      savagc = gcpagc
017124050207     c                   eval      savfgc = gcpfgc
017125050207     c                   eval      savngc = gcpngc
017126050207     c                   end
017133050207      * Scrivo Figcp
017134050207     c                   Write     Figcp
017135050207     c                   EndDo
017136050207      * Leggo Figap01l
017137050207     c     ktas30        Setll     Figap01l
017138050207     c                   Do        *Hival
017139050207     c     ktas30        Reade     Figap01l
017140050207     c                   If        %Eof(Figap01l)
017141050207     c                   Leave
017142050207     c                   EndIf
017143050207      * Scrivo Figap
017144050207     c                   Write     Figap
017145050207     c                   EndDo
017146050207     c**********************************************************************
017200030206      * Scrivo Titas
017300050207     c   31              Write     Titas
017301050207     c   32              Write     Titas10
017302050207     c   33              Write     Titasp
017400030206
017500030206     c                   EndDo
017600030910
017700030910     c     Fine          Tag
017701050207     C*
017702050207     C* SE IL TIPO LANCIO = R CHIUDO IN RETURN
017703050207     C*
017704050210     C                   IF        PARTPL<>'C'
017800050207     c                   RETURN
017801050207     C                   ELSE
017900050207     c                   Eval      *InLr = *On
018000050207     C                   END
