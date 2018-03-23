000100030206     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206
000300030206      *****************************************************************
000400030206      *                                                               *
000500050210      *       aggiungo archivi di prova giacenza su as888             *
000600030206      *                                                               *
000700030206      *****************************************************************
001502050210     fTitas30c  uf a e           k Disk    prefix(£)
001600050210     fTita730c  uf a e           k Disk    prefix(£)
001800050210     fTitaa30c  uf a e           k Disk    prefix(£)
002000050210     fTita430c  uf a e           k Disk    prefix(£)
002200050210     fTncsb03l  uf a e           k Disk    prefix(£)
002300050210     fFiar531c  uf a e           k Disk    prefix(£)
002301050210     fFigcp01l  uf a e           k Disk    prefix(£)
002302050210     fFignp01l  uf a e           k Disk    prefix(£)
002303050210     fFigap01l  uf a e           k Disk    prefix(£)
002500030206
002600050210     fTita700f  if   e           k Disk    Rename(Tita7000:Tita7)
002601050210     fTita7p0f  if   e           k Disk    Rename(Tita7p00:Tita7p)
002700050210     fTitaa00f  if   e           k Disk    Rename(Titaa000:Titaa)
002701050210     fTitaap0f  if   e           k Disk    Rename(Titaap00:Titaap)
002800050210     fTita400f  if   e           k Disk    Rename(Tita4000:Tita4)
002801050210     fTita4p0f  if   e           k Disk    Rename(Tita4p00:Tita4p)
002900050210     fTncsb00f  if   e           k Disk    Rename(Tncsb000:Tncsb)
003000050210     fFiar500f  if   e           k Disk    Rename(Fiar5000:Fiar5)
003001050210     fFiar5p0f  if   e           k Disk    Rename(Fiar5p00:Fiar5p)
003100050210     fTitas00f  if   e           k Disk    Rename(Titas000:Titas)
003101050210     fTitas10f  if   e           k Disk    Rename(Titas010:Titas10)
003102050210     fTitasp0f  if   e           k Disk    Rename(Titasp00:Titasp)
003103050210     ffigcp00f  if   e           k Disk    Rename(figcp000:figcp)
003104050210     ffignp00f  if   e           k Disk    Rename(fignp000:fignp)
003105050210     ffigap00f  if   e           k Disk    Rename(figap000:figap)
003200030206
003300030509     D KPJBA         E DS
003301050210     D dstaso        E DS                  extname(titas00f) prefix(£)
003302050210     D dstasi        E DS                  extname(titas00f)
003303050210     D dsta7o        E DS                  extname(tita700f) prefix(£)
003304050210     D dsta7i        E DS                  extname(tita700f)
003305050210     D dstaao        E DS                  extname(titaa00f) prefix(£)
003306050210     D dstaai        E DS                  extname(titaa00f)
003307050210     D dsta4o        E DS                  extname(tita400f) prefix(£)
003308050210     D dsta4i        E DS                  extname(tita400f)
003309050210     D dscsbo        E DS                  extname(tncsb00f) prefix(£)
003310050210     D dscsbi        E DS                  extname(tncsb00f)
003311050210     D dsar5o        E DS                  extname(fiar500f) prefix(£)
003312050210     D dsar5i        E DS                  extname(fiar500f)
003313050210     D dsgcpo        E DS                  extname(figcp00f) prefix(£)
003314050210     D dsgcpi        E DS                  extname(figcp00f)
003315050210     D dsgnpo        E DS                  extname(fignp00f) prefix(£)
003316050210     D dsgnpi        E DS                  extname(fignp00f)
003317050210     D dsgapo        E DS                  extname(figap00f) prefix(£)
003318050210     D dsgapi        E DS                  extname(figap00f)
004302050207      * Predisposizione indicatori per riconoscere i tipi record
004303050207      *   reperiti dal file TITAS30C/31C/38C
004304050207     iTITAS000      31
004305050207     iTITAS010      32
004306050207     iTITASP00      33
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
007010050207     c**********************************************************************
007900050210      * Leggo Titas00f
007901050207     c**********************************************************************
008200030910     c                   Do        *Hival
008300050210     c                   read      titas00f
008302050210     c                   If        %Eof(Titas00f)
008303050210     c                   Leave
008304050210     c                   EndIf
008305050210     c                   exsr      aggtas
008510050210     c                   enddo
008511050210     c**********************************************************************
008512050210      * Leggo Titas10f
008513050210     c**********************************************************************
008515050210     c                   Do        *Hival
008516050210     c                   read      titas10f
008518050210     c                   If        %Eof(Titas10f)
008519050210     c                   Leave
008520050210     c                   EndIf
008521050210     c                   exsr      aggtas
008532050210     c                   enddo
008533050210     c**********************************************************************
008534050210      * Leggo Titasp0f
008535050210     c**********************************************************************
008537050210     c                   Do        *Hival
008538050210     c                   read      titasp0f
008540050210     c                   If        %Eof(Titasp0f)
008541050210     c                   Leave
008542050210     c                   EndIf
008543050210     c                   exsr      aggtas
008554050210     c                   enddo
008555050210     c**********************************************************************
008556050210      * Leggo Tita700f
008557050210     c**********************************************************************
008559050210     c                   Do        *Hival
008560050210     c                   read      tita700f
008561050210     c                   If        %Eof(Tita700f)
008562050210     c                   Leave
008563050210     c                   EndIf
008564050210     c                   exsr      aggta7
008565050210     c                   enddo
008566050210     c**********************************************************************
008567050210      * Leggo Tita7p0f
008568050210     c**********************************************************************
008569050210     c                   Do        *Hival
008570050210     c                   read      tita7p0f
008571050210     c                   If        %Eof(Tita7p0f)
008572050210     c                   Leave
008573050210     c                   EndIf
008574050210     c                   exsr      aggta7
008575050210     c                   enddo
008576050210     c**********************************************************************
008577050210      * Leggo Titaa00f
008578050210     c**********************************************************************
008579050210     c                   Do        *Hival
008580050210     c                   read      titaa00f
008581050210     c                   If        %Eof(Titaa00f)
008582050210     c                   Leave
008583050210     c                   EndIf
008584050210     c                   exsr      aggtaa
008585050210     c                   enddo
008586050210     c**********************************************************************
008587050210      * Leggo Titaap0f
008588050210     c**********************************************************************
008589050210     c                   Do        *Hival
008590050210     c                   read      titaap0f
008591050210     c                   If        %Eof(Titaap0f)
008592050210     c                   Leave
008593050210     c                   EndIf
008594050210     c                   exsr      aggtaa
008595050210     c                   enddo
008596050210     c**********************************************************************
008597050210      * Leggo Tita400f
008598050210     c**********************************************************************
008599050210     c                   Do        *Hival
008600050210     c                   read      tita400f
008601050210     c                   If        %Eof(Tita400f)
008602050210     c                   Leave
008603050210     c                   EndIf
008604050210     c                   exsr      aggta4
008605050210     c                   enddo
008606050210     c**********************************************************************
008607050210      * Leggo Tita4p0f
008608050210     c**********************************************************************
008609050210     c                   Do        *Hival
008610050210     c                   read      tita4p0f
008611050210     c                   If        %Eof(Tita4p0f)
008612050210     c                   Leave
008613050210     c                   EndIf
008614050210     c                   exsr      aggta4
008615050210     c                   enddo
008616050210     c**********************************************************************
008617050210      * Leggo tncsb00f
008618050210     c**********************************************************************
008619050210     c                   Do        *Hival
008620050210     c                   read      tncsb00f
008621050210     c                   If        %Eof(Tncsb00f)
008622050210     c                   Leave
008623050210     c                   EndIf
008624050210     c                   exsr      aggcsb
008625050210     c                   enddo
008626050210     c**********************************************************************
008627050210      * Leggo fiar500f
008628050210     c**********************************************************************
008629050210     c                   Do        *Hival
008630050210     c                   read      fiar500f
008631050210     c                   If        %Eof(fiar500f)
008632050210     c                   Leave
008633050210     c                   EndIf
008634050210     c                   exsr      aggar5
008635050210     c                   enddo
008636050210     c**********************************************************************
008637050210      * Leggo fiar5p0f
008638050210     c**********************************************************************
008639050210     c                   Do        *Hival
008640050210     c                   read      fiar5p0f
008641050210     c                   If        %Eof(fiar5p0f)
008642050210     c                   Leave
008643050210     c                   EndIf
008644050210     c                   exsr      aggar5
008645050210     c                   enddo
008646050210     c**********************************************************************
008647050210      * Leggo figcp00f
008648050210     c**********************************************************************
008649050210     c                   Do        *Hival
008650050210     c                   read      figcp00f
008651050210     c                   If        %Eof(figcp00f)
008652050210     c                   Leave
008653050210     c                   EndIf
008654050210     c                   exsr      agggcp
008655050210     c                   enddo
008656050210     c**********************************************************************
008657050210      * Leggo fignp00f
008658050210     c**********************************************************************
008659050210     c                   Do        *Hival
008660050210     c                   read      fignp00f
008661050210     c                   If        %Eof(fignp00f)
008662050210     c                   Leave
008663050210     c                   EndIf
008664050210     c                   exsr      agggnp
008665050210     c                   enddo
008666050210     c**********************************************************************
008667050210      * Leggo figap00f
008668050210     c**********************************************************************
008669050210     c                   Do        *Hival
008670050210     c                   read      figap00f
008671050210     c                   If        %Eof(figap00f)
008672050210     c                   Leave
008673050210     c                   EndIf
008674050210     c                   exsr      agggap
008675050210     c                   enddo
008676030206
017900050207     c                   Eval      *InLr = *On
018000050210     c**********************************************************************
018100050210      * aggiorno titas
018200050210     c**********************************************************************
018300050210     c     aggtas        begsr
018301050210     c                   setoff                                       313233
018302050210     c     KTAS30        chain     Titas30c
018303050210     c                   eval      dstaso = dstasi
018304050210     c                   if        %found(titas30c)
018305050210     c   31              update    titas000
018306050210     c   32              update    titas010
018307050210     c   33              update    titasp00
018308050210     c                   else
018309050210     c                   write     titas000
018310050210     c                   endif
018400050210     c                   endsr
018500050210     c**********************************************************************
018600050210      * aggiorno tita7
018700050210     c**********************************************************************
018800050210     c     aggta7        begsr
018900050210     c                   setoff                                       3435
019000050210     c     KTA730        chain     Tita730c
019100050210     c                   eval      dsta7o = dsta7i
019200050210     c                   if        %found(tita730c)
019300050210     c   34              update    tita7000
019500050210     c   35              update    tita7p00
019600050210     c                   else
019700050210     c                   write     tita7000
019800050210     c                   endif
019900050210     c                   endsr
020000050210     c**********************************************************************
020100050210      * aggiorno titaa
020200050210     c**********************************************************************
020300050210     c     aggtaa        begsr
020400050210     c                   setoff                                       3637
020500050210     c     KTAa30        chain     Titaa30c
020600050210     c                   eval      dstaao = dstaai
020700050210     c                   if        %found(titaa30c)
020800050210     c   36              update    titaa000
020900050210     c   37              update    titaap00
021000050210     c                   else
021100050210     c                   write     titaa000
021200050210     c                   endif
021300050210     c                   endsr
021400050210     c**********************************************************************
021500050210      * aggiorno tita4
021600050210     c**********************************************************************
021700050210     c     aggta4        begsr
021800050210     c                   setoff                                       3839
021900050210     c     KTA430        chain     Tita430c
022000050210     c                   eval      dsta4o = dsta4i
022100050210     c                   if        %found(tita430c)
022200050210     c   38              update    tita4000
022300050210     c   39              update    tita4p00
022400050210     c                   else
022500050210     c                   write     tita4000
022600050210     c                   endif
022700050210     c                   endsr
022800050210     c**********************************************************************
022900050210      * aggiorno tncsb
023000050210     c**********************************************************************
023100050210     c     aggcsb        begsr
023300050210     c     Kcsb03        chain     tncsb03l
023400050210     c                   eval      dscsbo = dscsbi
023500050210     c                   if        %found(tncsb03l)
023700050210     c                   update    tncsb000
023800050210     c                   else
023900050210     c                   write     tncsb000
024000050210     c                   endif
024100050210     c                   endsr
024200050210     c**********************************************************************
024300050210      * aggiorno fiar5
024400050210     c**********************************************************************
024500050210     c     aggar5        begsr
024600050210     c                   setoff                                       4041
024700050210     c     Kar531        chain     fiar531c
024800050210     c                   eval      dsar5o = dsar5i
024900050210     c                   if        %found(fiar531c)
025000050210     c   40              update    fiar5000
025100050210     c   41              update    fiar5p00
025200050210     c                   else
025300050210     c                   write     fiar5000
025400050210     c                   endif
025500050210     c                   endsr
025600050210     c**********************************************************************
025700050210      * aggiorno figcp
025800050210     c**********************************************************************
025900050210     c     agggcp        begsr
026000050210     c     Kfigcp        chain     figcp01l
026100050210     c                   eval      dsgcpo = dsgcpi
026200050210     c                   if        %found(figcp01l)
026300050210     c                   update    figcp000
026400050210     c                   else
026500050210     c                   write     figcp000
026600050210     c                   endif
026700050210     c                   endsr
026800050210     c**********************************************************************
026900050210      * aggiorno fignp
027000050210     c**********************************************************************
027100050210     c     agggnp        begsr
027200050210     c     Kfignp        chain     fignp01l
027300050210     c                   eval      dsgnpo = dsgnpi
027400050210     c                   if        %found(fignp01l)
027500050210     c                   update    fignp000
027600050210     c                   else
027700050210     c                   write     fignp000
027800050210     c                   endif
027900050210     c                   endsr
028000050210     c**********************************************************************
028100050210      * aggiorno figap
028200050210     c**********************************************************************
028300050210     c     agggap        begsr
028400050210     c     Kfigap        chain     figap01l
028500050210     c                   eval      dsgapo = dsgapi
028600050210     c                   if        %found(figap01l)
028700050210     c                   update    figap000
028800050210     c                   else
028900050210     c                   write     figap000
029000050210     c                   endif
029100050210     c                   endsr
029101050210     c**********************************************************************
029102050210      * routine inizio
029103050210     c**********************************************************************
029104050210     c     *inzsr        begsr
029200050210      *
029300050210     C     *ENTRY        PLIST
029400050210     C                   PARM                    KPJBA
029500050210     c*
029600050210     c     kTas30        Klist
029700050210     c                   Kfld                    tasAas
029800050210     c                   Kfld                    tasLnp
029900050210     c                   Kfld                    tasNrs
030000050210     c                   Kfld                    tasNsp
030100050210     c                   Kfld                    tastbl
030102050210     c     kTa730        Klist
030103050210     c                   Kfld                    ta7Aas
030104050210     c                   Kfld                    ta7Lnp
030105050210     c                   Kfld                    ta7Nrs
030106050210     c                   Kfld                    ta7Nsp
030107050210     c                   Kfld                    ta7tbl
030108050210     c                   Kfld                    ta7svn
030109050210     c     kTaa30        Klist
030110050210     c                   Kfld                    taaAas
030111050210     c                   Kfld                    taaLnp
030112050210     c                   Kfld                    taaNrs
030113050210     c                   Kfld                    taaNsp
030114050210     c                   Kfld                    taatrc
030115050210     c     kTa430        Klist
030116050210     c                   Kfld                    ta4Aas
030117050210     c                   Kfld                    ta4Lnp
030118050210     c                   Kfld                    ta4Nrs
030119050210     c                   Kfld                    ta4Nsp
030120050210     c                   Kfld                    ta4trc
030121050210     c     kcsb03        Klist
030122050210     c                   Kfld                    csbAas
030123050210     c                   Kfld                    csbLnp
030124050210     c                   Kfld                    csbNrs
030125050210     c                   Kfld                    csbNsp
030126050210     c     kar531        Klist
030127050210     c                   Kfld                    ar5Aas
030128050210     c                   Kfld                    ar5Lnp
030129050210     c                   Kfld                    ar5Nrs
030130050210     c                   Kfld                    ar5Nsp
030131050210     c                   Kfld                    ar5trd
030132050210     c                   Kfld                    AR5PRG
030133050210     c                   Kfld                    AR5DAC
030134050210     c                   Kfld                    AR5ORC
030300050210     c     kfigcp        Klist
030400050210     c                   Kfld                    GCPAas
030500050210     c                   Kfld                    GCPlnp
030600050210     c                   Kfld                    GCPnrs
030601050210     c                   Kfld                    GCPnsp
030602050210     c                   Kfld                    GCPfrg
030603050210     c     kfignp        Klist
030604050210     c                   Kfld                    GNPAGC
030605050210     c                   Kfld                    GNPFGC
030606050210     c                   Kfld                    GNPNGC
030607050210     c                   Kfld                    GNPFRG
030609050210     c                   Kfld                    GNPFAS
030610050210     c                   Kfld                    GNPNPR
030611050210     c     kfigap        Klist
030612050210     c                   Kfld                    GaPAas
030613050210     c                   Kfld                    GaPlnp
030614050210     c                   Kfld                    GaPnrs
030615050210     c                   Kfld                    GaPnsp
030700050210     c                   endsr
