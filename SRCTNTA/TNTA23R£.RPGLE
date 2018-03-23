000100040917     H DECEDIT('0,') DATEDIT(*YMD.)
000200040917
000300040917      *****************************************************************
000400040917      *                                                               *
000500040917      *       STAMPA TARIFFE GIACENZA FILTRO                          *
000600040917      *                                                               *
000700040917      *****************************************************************
000800040917      *
000900040917     FAZORG01L  IF   E           K DISK
001000040917     FTABEL00F  IF   E           K DISK
001100040917     FTNTA23D   CF   E             WORKSTN
001200040917
001300041011     d codut           s              1  0 Inz(1)
001400041011     d wabi            s                   like(UteAut)
001500041011
001600041029     D MSG             S             78    DIM(06) CTDATA PERRCD(1)
001700041011      * P.O. gestibili dall'utente
001800041011     d fig             s              3    dim(250) inz(*Zeros)
001900041011
002000040917     D KPJBA         E DS
002100040917
002200040920     D TNTA23DS      E DS
002300041011
002400041011     d Azuteds       e ds                  extname(Azute00f)
002500041011     d dDatiute      e ds
002600041011     d dLat          e ds
002700041011     d dute01        e ds
002800041011     d tibs02ds      e ds
002900041011     d Tibs34ds      e ds
003000041011     d Trul31ds      e ds
003100040917
003200040917     D                SDS
003300040917     D  V1CPGM                 1     10
003400040917
003500040917     D codtab          s                   like(tblcod)
003600040917     D keytab          s                   like(tblkey)
003700040917      *---------------------------------------------------------------*
003800040917      *                                                               *
003900040917      *    UTILIZZO DEGLI INDICATORI                                  *
004000040917      *                                                               *
004100040917      *---------------------------------------------------------------*
004200040917      *                                                               *
004300040917      *  28  - Visualizzo messaggio Errore
004400040917      *  30  - Comodo
004500040917      *  40  - ERRORE p.o. in gestione
004600040917      *                                                               *
004700040917      *---------------------------------------------------------------*
004800041011
004900041011      * Verifica errori e autorità profilo
005000041011s   1c                   Select
005100041011      * se ho errori nei dati utente esco dal pgm
005200041011w   1c                   When      DutErr = 'E'
005300041011     c                   GoTo      Fine
005400041011      * se non c'è l'abilitazione
005500041011      * --> se 1° livello, abilitazioni al terminal
005600041011      *     se 2° livello, abilitazioni al punto operativo
005700041011      *     se sede è impossibile ma se capita mando a fine il pgm
005800041011w   1c                   When      UteAut = *Blanks
005900041011if  2c                   If        DutLpo = '1'
006000041011     c                   Eval      wabi   = 'TP'
006100041011e   2c                   EndIf
006200041011if  2c                   If        DutLpo = '2'
006300041011     c                   Eval      wabi   = 'PO'
006400041011e   2c                   EndIf
006500041011if  2c                   If        DutLpo = 'S'
006600041011     c                   GoTo      Fine
006700041011e   2c                   EndIf
006800041011      * carica le abilitazioni del profilo
006900041011x   1c                   Other
007000041011     c                   Movel     UteFaf        Dute01
007100041011     c                   If        §UteGtc <> *Blanks
007200041011     c                   Eval      wabi = §UteGtc
007300041011     c                   Else
007400041011     c                   Eval      wabi = UteAut
007500041011     c                   EndIf
007600041011e   1c                   EndSl
007700041011
007800041011      * controllo se ok l'abilitazione dell'utente
007900041011     c                   Clear                   Tibs02ds
008000041011     c                   Eval      T02Mod = 'C'
008100041011     c                   Eval      T02Sif = knsif
008200041011     c                   Eval      T02Cod = 'LAT'
008300041011     c                   Movel(p)  wabi          T02Ke1
008400041011     c                   Call      'TIBS02R'
008500041011     c                   Parm                    kpjba
008600041011     c                   Parm                    Tibs02ds
008700041011if  1c                   If        T02Err = *Blanks
008800041011     c                   Eval      dLat = T02Uni
008900041011e   1c                   EndIf
009000041011      * errore o non abilitato
009100041011if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
009200041011     c                   Seton                                        402808
009300041011     C                   movel     msg(4)        v1cmsg
009400041011     c                   GoTo      For01
009500041011e   1c                   EndIf
009600041011      *
009700041011      * Reperimento dei P.O. gestibili dall'utente
009800041011     c                   clear                   TRUL31DS
009900041011     c                   eval      I31abi = wabi
010000041011     c                   eval      I31cdi = DUTdis
010100041011     c                   eval      I31car = DUTare
010200041011     c                   eval      I31cpo = DUTpou
010300041011     c                   call      'TRUL31R'
010400041011     c                   parm                    KPJBA
010500041011     c                   parm                    TRUL31DS
010600041011if  1c                   if        O31pog > *zeros
010700041011     c                   movea     O31pog        fig
010800041011x   1c                   else
010900041011     c                   Seton                                        402808
011000041011     c                   movel     MSG(4)        v1cmsg
011100041011     c                   GoTo      For01
011200041011e   1c                   endif
011300041011
011400041011     c     For01         Tag
011500040917     C                   EXFMT     TA23D01
011600041011
011700041011     C                   CLEAR                   V1CMSG
011800041011     C                   SETOFF                                         2840
011900041011     C                   SETOFF                                       010203
012000041029     c                   SETOFF                                       04
012100040917
012200040917      * F3=FINE
012300041011     C                   if        *inkc or *In08
012400041011     C                   goto      fine
012500040917     C                   endif
012600040917
012700040917      * Controllo
012800041011     C                   EXSR      CTRD01
012900040917     C  NKF
013000041011     COR 28              goto      for01
013100040917
013200040917      * F6 = CONFERMA
013300040917     C                   CLEAR                   tnta23ds
013400041012     c                   eval      d23fil = v1cfil
013500041012     C                   Eval      d23dpo = v1dfil
013600041012     c                   if        d23fil = *Zeros
013700041012     c                   Eval      d23dpo = 'Tutti'
013800041012     c                   EndIf
013900041012     c                   eval      d23abi = i31abi
014000041012     c                   eval      d23cdi = i31cdi
014100041012     c                   eval      d23car = i31car
014200041012     c                   eval      d23cpo = i31cpo
014300040917     C                   MOVEL     V1TCM         D23TCM
014400040920     c                   If        V1tcm = ' '
014500040920     c                   eval      D23DCM = 'Tutti'
014600040920     c                   else
014700040920     C                   MOVEL     V1DTCM        D23DCM
014800040920     c                   endif
014900040917     C                   MOVEL     V1TFG         D23TFG
015000040920     c                   If        V1tfg = ' '
015100040920     c                   eval      D23Dfg = 'Tutti'
015200040920     c                   else
015300040920     C                   MOVEL     V1DTFG        D23DFG
015400040920     c                   endif
015500041011     c                   Eval      d23kci = DutKci
015600041012     c                   Eval      d23rsu = Rsut
015700041012     c                   Eval      d23fel = simfel
015800041029     c                   Eval      d23dpc = vtdpc
015900040917     C                   MOVEL(P)  TNTA23DS      KPJBU
016000040921     C*****              CALL      'TNTA13R'
016100040921     C                   MOVEL     'TA13'        KCOAZ
016200040921     C                   CALL      'BCH10'
016300040917     C                   PARM                    KPJBA
016400041011
016500041011     c     Fine          Tag
016600040917
016700040917     C                   eval      *inlr = *on
016800040917
016900040917      *---------------------------------------------------------------*
017000040917      *    Controlli videata                                          *
017100040917      *---------------------------------------------------------------*
017200041011     C     CTRD01        BEGSR
017300041011
017400041011      *  P.o.
017500041011if  1c                   If        V1cfil > 0
017600041011     c     V1cfil        Chain     Azorg01L
017700041011     c                   If        Not%Found(Azorg01l)
017800041011     c                   Clear                   V1dfil
017900041011     c                   Eval      v1cmsg = msg(3)
018000041011     c                   Seton                                        4028
018100041011     c                   Goto      Endctrd01
018200041011     c                   EndIf
018300041011
018400041011     c                   Movel     OrgDES        V1dfil
018500041011      * controllo se la filiale è gestita dall'utente
018600041011     c                   Movel     v1cfil        w003a             3
018700041011     c     w003a         Lookup    fig                                    30
018800041011     c                   If        not *In30
018900041011     c                   eval      v1cmsg = msg(5)
019000041011     c                   Seton                                        4028
019100041011     c                   Goto      Endctrd01
019200041011     c                   EndIf
019300041011
019400041011   x1c                   Else
019500041011     c                   Clear                   v1dfil
019600041011e   1c                   EndIf
019700040917
019800040917     C                   CLEAR                   V1dtcm
019900040917     C                   CLEAR                   V1dtfg
020000040917
020100040917      * TIPO COMUNICAZIONE AL MITTENTE SE IMMESSA
020200040917     C                   if        v1tcm <> ' '
020300040917      * se ricerca
020400040917     c                   if        v1tcm = '?'
020500040917     c                   movel     '2F'          codtab
020600040917     c                   clear                   v1tcm
020700040917     c                   movel     *blanks       destab           25
020800040917     c                   movel     *blanks       keytab
020900040917     c                   call      'X§TABER'
021000040917     c                   parm                    codut
021100040917     c                   parm                    codtab
021200040917     c                   parm                    keytab
021300040917     c                   parm                    destab
021400040917     c                   movel     keytab        v1tcm
021500040917     c                   movel     destab        v1dtcm
021600041011     C                   goto      endctrd01
021700040917     c                   endif
021800040917
021900040917     c                   movel     '2F'          codtab
022000040917     c                   movel     v1tcm         keytab
022100040917     c     ktabel        chain     tabel00f
022200040917     c                   if        not %found(tabel00f)
022300040917     c                   seton                                        0228
022400040917     C                   movel     msg(1)        v1cmsg
022500041011     C                   goto      endctrd01
022600040921     c                   else
022700040921     c                   movel     tbluni        v1dtcm
022800040917     C                   endif
022900040917      *
023000040917     C                   endif
023100040917
023200040917      * TIPO COMUNICAZIONE FINE GIACENZA  SE IMMESSA
023300040917     C                   if        v1tfg <> ' '
023400040917      * se ricerca
023500040917     c                   if        v1tfg = '?'
023600040917     c                   movel     '2H'          codtab
023700040917     c                   clear                   v1tfg
023800040917     c                   movel     *blanks       destab           25
023900040917     c                   movel     *blanks       keytab
024000040917     c                   call      'X§TABER'
024100040917     c                   parm                    codut
024200040917     c                   parm                    codtab
024300040917     c                   parm                    keytab
024400040917     c                   parm                    destab
024500040917     c                   movel     keytab        v1tfg
024600040917     c                   movel     destab        v1dtfg
024700041011     C                   goto      endctrd01
024800040917     c                   endif
024900040917
025000040917     c                   movel     '2H'          codtab
025100040917     c                   movel     v1tfg         keytab
025200040917     c     ktabel        chain     tabel00f
025300040917     c                   if        not %found(tabel00f)
025400040917     c                   seton                                        0328
025500040917     C                   movel     msg(2)        v1cmsg
025600041011     C                   goto      endctrd01
025700040921     c                   else
025800040921     c                   movel     tbluni        v1dtfg
025900040917     C                   endif
026000040917      *
026100040917     C                   endif
026200041029      * Se richiesta lista delle atriffe diverse da PIano dei conti
026300041029      * non devon esistere le parzializzazioni
026400041029     c                   if        vtdpc = 'S' and (v1tfg <> ' '
026500041029     c                             or v1tcm <> ' ')
026600041029     c                   seton                                        0428
026700041029     C                   movel     msg(6)        v1cmsg
026800041029     C                   goto      endctrd01
026900041029     c                   endif
027000040917
027100041011     C     endctrd01     ENDSR
027200040917      *****************************************************************
027300040917      * ROUTINE INIZIALE
027400040917      *****************************************************************
027500040917     C     *INZSR        BEGSR
027600040917
027700040917     C     *ENTRY        PLIST
027800040917     C                   PARM                    KPJBA
027900040917
028000041011     c     *dtaara       define    §azute        azuteds
028100041011     c     *dtaara       define    §datiute      ddatiute
028200041011     c                   in(E)     *dtaara
028300041011     c                   If        %error  or RSUT = *blanks
028400041011     c                   Clear                   Tibs34ds
028500041011     c                   Call      'TIBS34R'
028600041011     c                   Parm                    Tibs34ds
028700041011     c                   In        *dtaara
028800041011     c                   EndIf
028900041011
029000041011     c                   Clear                   wabi
029100041011     c                   Clear                   dLat
029200041011
029300041011     C                   MOVEL     rsut          V1CRSU
029400041012
029500041012     c                   If        DutLpo = '2' or DutLpo = *Blanks
029600041012     c                   Eval      V1cfil = Dutpou
029700041012     c                   EndIf
029800041012
029900040917
030000040917     c     KTABEL        Klist
030100040917     c                   kfld                    codut
030200040917     c                   kfld                    codtab
030300040917     c                   kfld                    keytab
030400040917
030500040917     C                   ENDSR
030600040917**
030700040917Immettere un tipo comunicazione al mittente valido                            1
030800040917Immettere un tipo comunicazione fine giacenza valido                          2
030900040917P.O. gestione errato                                                          3
031000041011Utente non abilitato alla Gestione tariffe                                    4
031100041011Punto operativo in gestione ad altro p.o.  !!                                 5
031200041029Inserire parzializzazioni o lista delle tariffe diverse da Anag.Clienti       6
