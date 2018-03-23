000100091203     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200031105
000300031105      *****************************************************************
000400150312      *       Duplica tariffa DPD 199 del codice 1990005
000500031105      *****************************************************************
000600110224
000700150313     FTNTAM04L  UF   E           K DISK
000800150313     FTITAD04L  UF   E           K DISK
000900031105
001000150313     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001100031105      *------------------------------------------------------------------------*
001200150312     D Kctr            S                   LIKE(TAMctr) inz(199)
001300150313     D w_cliente       S              4  0
001400150313     d xx              s              3  0
001500150316     d kksc            s                   like(tadksc)
001600150316     d kprg            s                   like(tadprg)
001700150316     d kprgn           s                   like(tadprg)
001800150316     d wtad            s              5
001900150316     d w003a           s              3
002000150313     d oldlnp          s                   like(tadlnp)
002100150313     d oldcts          s                   like(tadcts)
002200031105
002300150313      *   s c h i e r e
002400150313     d Ksc_no          s              7  0 dim(7)  ctdata  perrcd( 1)
002500150313
002600150313     d sklnpo          s              3    Dim(50)
002700150313     d sklnpn          s              3    Dim(50)
002800150316     d sktad           s              5    Dim(500)
002900031105      *   D S   I N T E R N E / E S T E R N E
003000031105
003100031105     D WLBDAT          DS                  INZ
003200031105     D  G02DAT                 1      8  0
003300031105     D  G02INV                 9     16  0
003400031105     D  G02ERR                17     17
003500031105     D  G02TGI                18     22  0
003600031105
003700150312     d Tnta25ds      e ds
003800150312     d Tnta25tad     e ds                  inz
003900150312     d Tnta25tgc     e ds                  inz
004000150312     d Tnta25tpd     e ds                  inz
004100150312     d Tnta25tpt     e ds                  inz
004200150312
004300110228     D Dsta01        e ds
004400031105     D kpjba         e ds
004500031105
004600150313     d cnacoDS       e ds                  ExtName(cnaco00f)
004700081128      *------------------------------------------------------------------------*
004800110228     c
004900111111     c
005000111111     c/EXEC SQL
005100111111     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005200111111     c/END-EXEC
005300150313
005400150313     c                   exsr      Sr_Carsktad
005500150313
005600111111     c*
005700110228     c*
005800110228     C/EXEC SQL
005900110228     C+ declare C1 cursor for
006000150312     C+ SELECT acoKSC   FROM CNaco00F
006100150312     C+ where substr(digits(acoksc), 1, 3) in
006200150316     C+ (291, 292, 196, 193, 192, 195, 197, 191, 190, 194)
006300110228     c+ for read only
006400110228     C/END-EXEC
006500150312
006600110228     C/EXEC SQL
006700110228     C+ open C1
006800110228     C/END-EXEC
006900110228     C
007000110228     C/EXEC SQL
007100150312     C+ Fetch C1 into :acoKSC
007200110228     C/END-EXEC
007300110228     C*
007400111110     C
007500110228     C*
007600111110  do1C                   dow       sqlcod = *zeros
007700150316
007800150316     c                   move      acoksc        W_cliente
007900150316
008000150316     c                   select
008100150316      * Non faccio nulla se cliente tra quelli esclusi
008200150316     c                   when      %lookup(acoksc : ksc_no) > 0
008300150316      * Non faccio nulla se cliente è 8888 o 9999
008400150316     c                   when      W_Cliente = 8888 or W_Cliente = 9999
008500150316     c
008600150316     c                   OTHER
008700031105      *
008800150312      * Aggancio Tntam04l ultima tariffa 199 presente
008900110225     c     ktam4         setll     tntam04l
009000111110  do2c                   do        *hival
009100110225     c     ktam4         reade     tntam04l
009200031105      *
009300031105      * fine file
009400111110    1c                   if        %eof(tntam04l)
009500150312     c                   exsr      Sr_Duplica
009600031105     c                   leave
009700111110    1c                   endif
009800150312      * verifico la data scadenza dell'ultima tariffa
009900150312     c                   if        tamdst < 20150228
010000150312     c                   eval      tamdst = 20150228
010100150318     c                   eval      tamduv = *date
010200150318     c                   eval      tamdtr = *date
010300150318     c                   Clear                   tamftr
010400150313     c                   update    tntam000
010500150312     c                   exsr      Sr_Duplica
010600150312     c                   leave
010700150312     c                   endif
010800110228
010900150312      * verifico la data scadenza dell'ultima tariffa se = 20150228
011000150312     c                   if        tamdst = 20150228
011100150312     c                   exsr      Sr_Duplica
011200150312     c                   leave
011300150312     c                   endif
011400150312      * se maggiore stampo
011500150312     c                   If        tamdst > 20150228
011600110228     c                   except    tariffa
011700150312     c                   leave
011800150312     c                   endif
011900150312
012000111110  do2C                   enddo
012100150316
012200150316     c                   endsl
012300150316
012400110225     c
012500110228     C/EXEC SQL
012600150312     C+ Fetch C1 into :ACOKSC
012700110228     C/END-EXEC
012800111110  do1C                   enddo
012900081201     C
013000110228     C/EXEC SQL
013100110228     C+ open C1
013200110228     C/END-EXEC
013300081201
013400081201     c                   seton                                        lr
013500150313      *------------------------------------------------------------------------*
013600150313      * CARICO LE SCHIERE SUL DETTAGLIO TARIFFARIO
013700150313      *------------------------------------------------------------------------*
013800150313     c     Sr_Carsktad   BegSr
013900150313
014000150313     c                   Clear                   oldlnp
014100150313     c                   Clear                   oldcts
014200150313     c                   Movea     *Zeros        sklnpo
014300150313
014400150313      * Leggo dettaglio tariffa da cui copiare
014500150313     c                   eval      kksc = 1900005
014600150313     c                   eval      kctr = 199
014700150313     c                   eval      kprg = 1
014800150313     c     kTntam01      Setll     Titad04l
014900150313do  1c                   Do        *Hival
015000150313     c     kTntam01      Reade     Titad04l
015100150313     c                   If        %Eof
015200150313     c                   Leave
015300150313     c                   EndIf
015400150313     c                   If        tadatb <> *Blanks
015500150313     c                   Iter
015600150313     c                   EndIf
015700150313     c                   If        tadlnp = oldlnp and tadcts = oldcts
015800150313     c                   Iter
015900150313     c                   EndIf
016000150313     c                   Move      tadlnp        w003a
016100150313     c     w003a         Lookup    sklnpo                                 30
016200150313if  3c                   If        Not *In30
016300150313     c                   Eval      xx = 1
016400150313     c     *Zeros        Lookup    sklnpo(xx)                             30
016500150313if  4c                   If        *In30
016600150313     c                   Eval      sklnpo(xx) = w003a
016700150313    4c                   EndIf
016800150313    3c                   EndIf
016900150313      * poi memorizzo la sk delle lnp/cts
017000150313     c                   Movel     tadlnp        wtad
017100150313     c                   Move      tadcts        wtad
017200150313     c     wtad          Lookup    sktad                                  30
017300150313if  2c                   If        Not *In30
017400150313     c                   Eval      xx = 1
017500150313     c     *Blanks       Lookup    sktad(xx)                              30
017600150313     c                   If        *In30
017700150313     c                   Eval      sktad(xx) = wtad
017800150313     c                   EndIf
017900150313     c                   Eval      oldlnp = tadlnp
018000150313     c                   Eval      oldcts = tadcts
018100150313    2c                   EndIf
018200150313    1c                   EndDo
018300150313
018400150313     c                   EndSr
018500150313
018600150312      *****************************************************************
018700150312      * duplica tariffa 199 dpd
018800150312      *****************************************************************
018900150312     C     Sr_duplica    BEGSR
019000150312
019100150312      * Richiamo al nuovo pgm di copia tariffe
019200150312     c                   Clear                   Tnta25ds
019300150317     c                   Eval      ta25ta01  = '1'
019400150313     c                   Eval      ta25inglo = 'N'
019500150312     c                   Eval      ta25tipo = 'T'
019600150312     c                   Eval      ta25ksco = 1900005
019700150312     c                   Eval      ta25ctro = 199
019800150312     c                   Eval      ta25prgo = 1
019900150312     c                   Eval      ta25tipn = 'T'
020000150312     c                   Eval      ta25kscn = acoksc
020100150312     c                   Eval      ta25ctrn = 199
020200150312     c                   Eval      ta25tam = 'S'
020300150312     c                   Eval      ta25tad = 'T'
020400150312     c                   Eval      ta25tpt = 'T'
020500150312     c                   Eval      ta25tpd = 'N'
020600150312     c                   Eval      ta25tgc = 'T'
020700150313     c                   Eval      ta25cat = 'N'
020800150312     c                   Eval      ta25fie = 'I'
020900150312     c                   Call      'TNTA25R'
021000150312     c                   Parm                    kpjba
021100150312     c                   Parm                    Tnta25ds
021200150312     c                   Parm                    Tnta25tad
021300150312     c                   Parm                    Tnta25tgc
021400150312     c                   Parm                    Tnta25tpd
021500150312     c                   Parm                    Tnta25tpt
021600150312
021700150312      * Se errore da copia tariffa lo emetto a video
021800150312     c                   If        ta25err <> *Blanks
021900150312     c                   Except    Errore
022000150313     c                   else
022100150316     c                   move      ta25prg       kprgn
022200150316     c     ktam401       chain     tntam04l
022300150316     c                   if        %found(tntam04l)
022400150316     c                   eval      tamddt = 20150301
022500150317     c                   clear                   tampri
022600150317     c                   clear                   tamrct
022700150316     c                   update    tntam000
022800150316     c                   Except    Buona
022900150316     c                   else
023000150316     c                   except    buona01
023100150316     c                   endif
023200150312     c                   EndIf
023300150312
023400150312     c                   endsr
023500031105      *****************************************************************
023600031105      * ROUTINE INIZIALE
023700031105      *****************************************************************
023800031105     C     *INZSR        BEGSR
023900031105
024000031105     C     *ENTRY        PLIST
024100031105     C                   PARM                    KPJBA
024200110225     c                   movel     kpjbu         WAGGIO            1
024300150312
024400110225     c     ktam4         klist
024500150312     c                   kfld                    acoksc
024600150312     c                   kfld                    kctr
024700081201      *
024800150316     c     ktam401       klist
024900150316     c                   kfld                    acoksc
025000150316     c                   kfld                    kctr
025100150316     c                   kfld                    kprgn
025200150316      *
025300150313     c     kTntam01      klist
025400150313     c                   kfld                    kksc
025500150313     c                   kfld                    kctr
025600150313     c                   kfld                    kprg
025700150313
025800081201      *
025900031105
026000031105     C                   endsr
026100110224     OPRTF198   E            TESTA            02
026200150312     O                                            8 'TAVR58R '
026300150312     O                                           62 '* Scrittura Tariffa DPD  *'
026400110225     OPRTF198   E            tariffa     1
026500110225     o                       tamksc
026600110225     o                       tamctr           +   1
026700110225     o                       tamprg        z  +   1
026800110225     o                       tamfie           +   1
026900110225     o                       tamddt           +   1 '    /  /  '
027000110225     o                       tamdst           +   1 '    /  /  '
027100110225     o                                           49 ' '
027200150312     O                                        +   1 'Data scadenza > 20150228'
027300150312     OPRTF198   E            errore      1
027400150313     o                       acoksc
027500150312     o                                           49 ' '
027600150312     O                                        +   1 'Duplica errata '
027700150313     OPRTF198   E            buona       1
027800150313     o                       acoksc
027900150313     o                                        +   1 '199'
028000150313     o                       ta25prg          +   1
028100150313     o                                           49 ' '
028200150313     O                                        +   1 'Duplica effettuata'
028300150316     OPRTF198   E            buona01     1
028400150316     o                       acoksc
028500150316     o                                        +   1 '199'
028600150316     o                       ta25prg          +   1
028700150316     o                                           49 ' '
028800150316     O                                        +   1 'Duplica effett. no tamddt'
028900150313**
0290001503131900005
0291001503131900083
0292001503131910083
0293001503131940083
0294001503131950010
0295001503131960081
0296001503131970010
