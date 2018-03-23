000100040917     H DECEDIT('0,') DATEDIT(*YMD.)
000200040917      *****************************************************************
000300040917      *       STAMPA TARIFFE GIACENZA FILTRO                          *
000400040917      *****************************************************************
000500040917     FAZORG01L  IF   E           K DISK
000600040917     FTABEL00F  IF   E           K DISK
000700040917     FTNTA23D   CF   E             WORKSTN
000800040917
000900041011     d codut           s              1  0 Inz(1)
001000041011     d wabi            s                   like(UteAut)
001100041011
001200041029     D MSG             S             78    DIM(06) CTDATA PERRCD(1)
001300041011
001400040917     D KPJBA         E DS
001500040917
001600040920     D TNTA23DS      E DS
001700041011
001800041011     d Azuteds       e ds                  extname(Azute00f)
001900041011     d dDatiute      e ds
002000041011     d tibs02ds      e ds
002100041011     d Tibs34ds      e ds
002200090701     d tntaa1ds      e ds
002300090701
002400040917     D                SDS
002500040917     D  V1CPGM                 1     10
002600040917
002700040917     D codtab          s                   like(tblcod)
002800040917     D keytab          s                   like(tblkey)
002900040917      *---------------------------------------------------------------*
003000040917      *                                                               *
003100040917      *    UTILIZZO DEGLI INDICATORI                                  *
003200040917      *                                                               *
003300040917      *---------------------------------------------------------------*
003400040917      *                                                               *
003500040917      *  28  - Visualizzo messaggio Errore
003600040917      *  30  - Comodo
003700040917      *  40  - ERRORE p.o. in gestione
003800040917      *                                                               *
003900040917      *---------------------------------------------------------------*
004000090701     c* Richiamo solo per avere codice di abilitazione
004100090701     c                   clear                   tntaa1ds
004200090701     c                   eval      itaa1caut='§utegtc'
004300090701     c                   clear                   kpjbu
004400090701     c                   movel     tntaa1ds      kpjbu
004500090703     c                   call      'TNTAA1C'
004600090701     c                   parm                    kpjba
004700090701     c                   movel     kpjbu         tntaa1ds
004800090701     c                   eval      wabi=otaa1cabi
004900090701      *
005000041011
005100041011     c     For01         Tag
005200040917     C                   EXFMT     TA23D01
005300041011
005400041011     C                   CLEAR                   V1CMSG
005500041011     C                   SETOFF                                         2840
005600041011     C                   SETOFF                                       010203
005700041029     c                   SETOFF                                       04
005800040917
005900040917      * F3=FINE
006000090701      * 08 on - utente non abilitato --> esco dal pgm
006100090701     C                   if        *inkc OR  *IN08
006200041011     C                   goto      fine
006300040917     C                   endif
006400040917
006500040917      * Controllo
006600041011     C                   EXSR      CTRD01
006700040917     C  NKF
006800041011     COR 28              goto      for01
006900040917
007000040917      * F6 = CONFERMA
007100040917     C                   CLEAR                   tnta23ds
007200041012     c                   eval      d23fil = v1cfil
007300041012     C                   Eval      d23dpo = v1dfil
007400041012     c                   if        d23fil = *Zeros
007500041012     c                   Eval      d23dpo = 'Tutti'
007600041012     c                   EndIf
007700090701     c                   eval      d23abi = wabi
007800090701     c                   eval      d23cdi = dutdis
007900090701     c                   eval      d23car = dutare
008000090701     c                   eval      d23cpo = dutpou
008100040917     C                   MOVEL     V1TCM         D23TCM
008200040920     c                   If        V1tcm = ' '
008300040920     c                   eval      D23DCM = 'Tutti'
008400040920     c                   else
008500040920     C                   MOVEL     V1DTCM        D23DCM
008600040920     c                   endif
008700040917     C                   MOVEL     V1TFG         D23TFG
008800040920     c                   If        V1tfg = ' '
008900040920     c                   eval      D23Dfg = 'Tutti'
009000040920     c                   else
009100040920     C                   MOVEL     V1DTFG        D23DFG
009200040920     c                   endif
009300041011     c                   Eval      d23kci = DutKci
009400041012     c                   Eval      d23rsu = Rsut
009500041012     c                   Eval      d23fel = simfel
009600041029     c                   Eval      d23dpc = vtdpc
009700040917     C                   MOVEL(P)  TNTA23DS      KPJBU
009800040921     C*****              CALL      'TNTA13R'
009900040921     C                   MOVEL     'TA13'        KCOAZ
010000040921     C                   CALL      'BCH10'
010100040917     C                   PARM                    KPJBA
010200041011
010300041011     c     Fine          Tag
010400090701     C*
010500090701     C                   clear                   tntaa1ds
010600090701     C                   movel     'C'           Itaa1Tla
010700090701     C                   movel(p)  tntaa1ds      kpjbu
010800090703     C                   CALL      'TNTAA1C'
010900090701     C                   PARM                    KPJBA
011000090701     c
011100040917     C                   eval      *inlr = *on
011200040917      *---------------------------------------------------------------*
011300040917      *    Controlli videata                                          *
011400040917      *---------------------------------------------------------------*
011500041011     C     CTRD01        BEGSR
011600041011
011700041011      *  P.o.
011800041011if  1c                   If        V1cfil > 0
011900041011     c     V1cfil        Chain     Azorg01L
012000041011     c                   If        Not%Found(Azorg01l)
012100041011     c                   Clear                   V1dfil
012200041011     c                   Eval      v1cmsg = msg(3)
012300041011     c                   Seton                                        4028
012400041011     c                   Goto      Endctrd01
012500041011     c                   EndIf
012600041011
012700041011     c                   Movel     OrgDES        V1dfil
012800041011      * controllo se la filiale è gestita dall'utente
012900090701     c                   clear                   tntaa1ds
013000090701     c                   eval      itaa1caut='§utegtc'
013100090701     c                   eval      itaa1fil=v1cfil
013200090701     c                   clear                   kpjbu
013300090701     c                   movel     tntaa1ds      kpjbu
013400090703     c                   call      'TNTAA1C'
013500090701     c                   parm                    kpjba
013600090701     c                   movel     kpjbu         tntaa1ds
013700090701     c
013800090701    1c                   if        otaa1fabi='N'
013900090701     c
014000090701     c**                 Movel     v1cfil        w003a             3
014100090701     c**   w003a         Lookup    fig                                    30
014200090701     c**                 If        not *In30
014300090701
014400041011     c                   eval      v1cmsg = msg(5)
014500041011     c                   Seton                                        4028
014600041011     c                   Goto      Endctrd01
014700041011     c                   EndIf
014800041011
014900041011   x1c                   Else
015000041011     c                   Clear                   v1dfil
015100041011e   1c                   EndIf
015200040917
015300040917     C                   CLEAR                   V1dtcm
015400040917     C                   CLEAR                   V1dtfg
015500040917
015600040917      * TIPO COMUNICAZIONE AL MITTENTE SE IMMESSA
015700040917     C                   if        v1tcm <> ' '
015800040917      * se ricerca
015900040917     c                   if        v1tcm = '?'
016000040917     c                   movel     '2F'          codtab
016100040917     c                   clear                   v1tcm
016200040917     c                   movel     *blanks       destab           25
016300040917     c                   movel     *blanks       keytab
016400040917     c                   call      'X§TABER'
016500040917     c                   parm                    codut
016600040917     c                   parm                    codtab
016700040917     c                   parm                    keytab
016800040917     c                   parm                    destab
016900040917     c                   movel     keytab        v1tcm
017000040917     c                   movel     destab        v1dtcm
017100041011     C                   goto      endctrd01
017200040917     c                   endif
017300040917
017400040917     c                   movel     '2F'          codtab
017500040917     c                   movel     v1tcm         keytab
017600040917     c     ktabel        chain     tabel00f
017700040917     c                   if        not %found(tabel00f)
017800040917     c                   seton                                        0228
017900040917     C                   movel     msg(1)        v1cmsg
018000041011     C                   goto      endctrd01
018100040921     c                   else
018200040921     c                   movel     tbluni        v1dtcm
018300040917     C                   endif
018400040917      *
018500040917     C                   endif
018600040917
018700040917      * TIPO COMUNICAZIONE FINE GIACENZA  SE IMMESSA
018800040917     C                   if        v1tfg <> ' '
018900040917      * se ricerca
019000040917     c                   if        v1tfg = '?'
019100040917     c                   movel     '2H'          codtab
019200040917     c                   clear                   v1tfg
019300040917     c                   movel     *blanks       destab           25
019400040917     c                   movel     *blanks       keytab
019500040917     c                   call      'X§TABER'
019600040917     c                   parm                    codut
019700040917     c                   parm                    codtab
019800040917     c                   parm                    keytab
019900040917     c                   parm                    destab
020000040917     c                   movel     keytab        v1tfg
020100040917     c                   movel     destab        v1dtfg
020200041011     C                   goto      endctrd01
020300040917     c                   endif
020400040917
020500040917     c                   movel     '2H'          codtab
020600040917     c                   movel     v1tfg         keytab
020700040917     c     ktabel        chain     tabel00f
020800040917     c                   if        not %found(tabel00f)
020900040917     c                   seton                                        0328
021000040917     C                   movel     msg(2)        v1cmsg
021100041011     C                   goto      endctrd01
021200040921     c                   else
021300040921     c                   movel     tbluni        v1dtfg
021400040917     C                   endif
021500040917      *
021600040917     C                   endif
021700041029      * Se richiesta lista delle atriffe diverse da PIano dei conti
021800041029      * non devon esistere le parzializzazioni
021900041029     c                   if        vtdpc = 'S' and (v1tfg <> ' '
022000041029     c                             or v1tcm <> ' ')
022100041029     c                   seton                                        0428
022200041029     C                   movel     msg(6)        v1cmsg
022300041029     C                   goto      endctrd01
022400041029     c                   endif
022500040917
022600041011     C     endctrd01     ENDSR
022700040917      *****************************************************************
022800040917      * ROUTINE INIZIALE
022900040917      *****************************************************************
023000040917     C     *INZSR        BEGSR
023100040917
023200040917     C     *ENTRY        PLIST
023300040917     C                   PARM                    KPJBA
023400040917
023500041011     c     *dtaara       define    §azute        azuteds
023600041011     c     *dtaara       define    §datiute      ddatiute
023700041011     c                   in(E)     *dtaara
023800041011     c                   If        %error  or RSUT = *blanks
023900041011     c                   Clear                   Tibs34ds
024000041011     c                   Call      'TIBS34R'
024100041011     c                   Parm                    Tibs34ds
024200041011     c                   In        *dtaara
024300041011     c                   EndIf
024400041011
024500041011     c                   Clear                   wabi
024600041011
024700041011     C                   MOVEL     rsut          V1CRSU
024800041012
024900041012     c                   If        DutLpo = '2' or DutLpo = *Blanks
025000041012     c                   Eval      V1cfil = Dutpou
025100041012     c                   EndIf
025200090701     C
025300090701     c* Richiamo solo per avere codice di abilitazione
025400090701     c                   clear                   tntaa1ds
025500090701     c                   eval      itaa1caut='§utegtc'
025600090701     c                   clear                   kpjbu
025700090701     c                   movel     tntaa1ds      kpjbu
025800090703     c                   call      'TNTAA1C'
025900090701     c                   parm                    kpjba
026000090701     c                   movel     kpjbu         tntaa1ds
026100090701     C
026200090701     c* utente non abilitato alla gestione tariffe
026300090701     c                   if        otaa1fabi='N'
026400090701     c                   Seton                                        442808
026500090701     C                   movel     msg(4)        v1cmsg
026600090701     c                   endif
026700090701      *
026800041012
026900040917
027000040917     c     KTABEL        Klist
027100040917     c                   kfld                    codut
027200040917     c                   kfld                    codtab
027300040917     c                   kfld                    keytab
027400040917
027500040917     C                   ENDSR
027600040917**
027700040917Immettere un tipo comunicazione al mittente valido                            1
027800040917Immettere un tipo comunicazione fine giacenza valido                          2
027900090701Filiale Errata
028000090701UTENTE NON ABILITATO ALLA GESTIONE DELLE TARIFFE                              4
028100090701Filiale non in gestione all'utente
028200041029Inserire parzializzazioni o lista delle tariffe diverse da Anag.Clienti       6
