000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400150826      *       controllo instradamenti bolle DPD
000500031017      *---------------------------------------------------------------*
000600150820     fFivab01l  if   e           k Disk
000700150825     fFivat01l  if   e           k Disk
000800150820     ftabel00f  if   e           k Disk
000900150820     fprtf198   o    f  198        Printer Oflind(*InOf)
001000150820     D TISI95DS      E DS
001100150820     D fnlv13ds      E DS
001200150820     D kpjba         E DS
001300150820     D dspr          E DS
001400150820     D W0140           S             14  0
001500150820     D WORA            S              6  0
001600150820     D XX              S              3  0 INZ
001700150820     D WDTGIO          S              8  0
001800150820     D DATEU           S              8  0
001900150820     D DATA_eur        S               D   DATFMT(*eur)
002000150820
002100150820     D reg             S             25    DIM(55)
002200150820     D LOC_I           S             35    DIM(5) CTDATA PERRCD(1)
002300150820     D LOC_E           S             35    DIM(5) CTDATA PERRCD(1)
002400150820
002500150820     D digits          c                   '0123456789 /-'
002600150820
002700150820      /COPY GAITRASRC/SRCUB,UBTL05D02
002800150820     D   pInNazione    S              3A
002900150820     D   pInProvincia  S              4A
003000150820     D   pInComune     S            128A
003100150820     D   pInLocalita   S            128A
003200150820     D   pInCAP        S              9A
003300150820     D   pInVia        S            128A
003400150820     D   pInCivico     S             10A
003500150820     D   pInScore      S             10A
003600150820     D   pOutEsito     S              1A
003700150820     D   pOutEsitoDes  S            128A
003800150820     D   pOutTotFound  S              4S 0
003900150820     D   pOutResults   DS                  dim(10) likeds(dsOutUBTL05R02)
004000150820
004100031017
004200150826     c     *entry        plist
004300150826     c                   parm                    kmgs              4
004400031017
004500150820     c     ktab          klist
004600150820     c                   kfld                    codut             1 0
004700150820     c                   kfld                    cod               2
004800150820     c                   kfld                    key               8
004900150820     c     ktab2         klist
005000150820     c                   kfld                    codut             1 0
005100150820     c                   kfld                    cod               2
005200150825     c     kvat          klist
005300150825     c                   kfld                    VAbFGS
005400150825     c                   kfld                    VAbCCM
005500150825     c                   kfld                    VAbAAS
005600150825     c                   kfld                    VAbLNP
005700150825     c                   kfld                    VAbNRS
005800150825     c                   kfld                    VAbNSP
005900150825     c                   kfld                    ktrc              1
006000150825     c                   eval      ktrc='E'
006100150820     c* carico le regioni
006200150820     c                   z-add     1             codut
006300150820     c                   movel     '14'          cod
006400150820     c                   z-add     1             xx
006500150820     c     ktab2         setll     tabel00f
006600150820     c     ktab2         reade     tabel00f
006700150820     c                   dow       not %eof(tabel00f)
006800150820     c                   add       1             xx
006900150820     c                   movel     tbluni        reg(xx)
007000150820     c     ktab2         reade     tabel00f
007100150820     c                   enddo
007200150820     c
007300150820     c                   movel     'PR'          cod
007400150820      * Recupero data e ora
007500150820     C                   TIME                    WORA
007600150820     C                   TIME                    W0140
007700150820      * UDATE IN GGMMAAAA
007800150820     C                   MOVE      W0140         WDTGIO
007900150820      * UDATE IN AAAAMMGG
008000150820     C     *eur          MOVEL     WDTGIO        DATA_eur
008100150820     C     *iso          MOVEL     DATA_eur      dateu
008200150820      *
008300031017
008400150820     c     1900083       Setll     Fivab01l
008500150820do  0c                   Do        *Hival
008600150820     c     1900083       Reade     Fivab01l
008700150820     c                   If        %Eof(Fivab01l)
008800031017     c                   Leave
008900031017     c                   EndIf
009000150826     c
009100150826     c                   if        kmgs<=*zeros or kmgs=%editc(vabmgs:'X')
009200150826     c
009300150820     C                   CLEAR                   newprv
009400150820     C                   CLEAR                   newcap
009500150820     C                   CLEAR                   newloc           35
009600150820     C                   CLEAR                   newind           35
009700150820     C                   CLEAR                   kloc             35
009800150820     C                   CLEAR                   esloc             1
009900150820     C                   CLEAR                   eslocnum          1
010000150826     C                   CLEAR                   wok               1
010100150826     c* di cui indirizzo vuoto
010200150826     c                   if        vabind=*blanks
010300150826     c                   add       1             noind             7 0
010400150826     c                   eval      wok='N'
010500150826     c                   else
010600150826     c* indirizzo più corto di 6 caratteri
010700150826     c                   if        %checkr(' ':vabind)<6
010800150826     c                   add       1             cortoind          7 0
010900150826     c                   eval      wok='N'
011000150826     c                   endif
011100150826     c                   endif
011200150826     c* di cui RSD       vuoto
011300150826     c                   if        vabrsd=*blanks
011400150826     c                   add       1             norsd             7 0
011500150826     c                   eval      wok='N'
011600150826     c                   endif
011700150826     c* di cui LOC       vuoto
011800150826     c                   if        vablod=*blanks
011900150826     c                   add       1             nolod             7 0
012000150826     c                   eval      wok='N'
012100150828     c                   else
012200150826
012300150826     c* di cui località è un numero conteggio
012400150826    2c                   if        %check(digits:vablod)=0
012500150826     c                   add       1             locnum            7 0
012600150826     c                   eval      wok='N'
012700150826     c                   endif
012800150828     c                   endif
012900150820
013000150820     C                   CLEAR                   TISI95DS
013100150820     C                   MOVEL     vabCAD        I95CAP
013200150820     C                   MOVEL     DATEU         I95DAT
013300150820     C                   MOVEL     '3'           I95TCN
013400150820     C                   CALL      'TISI95R'
013500150820     C                   PARM                    TISI95DS
013600150820      *     Errore
013700150820    1C     O95ERR        IFeq      *BLANKS
013800150820     C                   MOVEL     O95PRV        newprv            2
013900150826     c
014000150820     c*
014100150820     C* SE LA LOCALITÀ È STRANIERA --> "ITALIANIZZO"
014200150820     C                   Z-ADD     1             XX                3 0
014300150820     C                   EVAL      XX=%LOOKUP(VABLOD:LOC_E)
014400150820    2C                   IF        xx>0
014500150820     c                   movel     loc_i(XX)     newloc
014600150820     c                   add       1             loci              7 0
014700150820   x2c                   else
014800150826     c* se la localitÀ è la sigla di una provincia/italia o trattini o regione
014900150820     c                   eval      xx=1
015000150820     c                   eval      xx=%checkr(' ':vablod)
015100150820    3c                   if        xx=2 or %subst(vablod:1:4)='ITAL' or
015200150826     c                             %subst(vablod:1:2)='--'
015300150826     c                             or %lookup(vablod:reg)>0
015400150820     c*
015500150820     c                   add       1             loc_non           7 0
015600150826     c                   eval      wok='N'
015700150826    3c                   endif
015800150826    2c                   endif
015900150820     c
016000150820     c                   clear                   fnlv13ds
016100150820     C                   MOVEL     '7'           I95TCN
016200150820     c                   movel     vablod        i95loc
016300150820     c                   movel     newprv        i95prv
016400150820     c
016500150820     C                   MOVEL     'Y'           I13CNV
016600150820     c                   movel     vabind        i95ind
016700150820    c
016800150820     C                   CALL      'FNLV13R'
016900150820     C                   PARM                    KPJBA
017000150820     C                   PARM                    fnlv13ds
017100150820     C                   PARM                    tisi95ds
017200150820     C                   PARM      ' '           flgbac            1
017300150820    2c                   if        o13roc='S'
017400150820     c                   movel     o95cap        newcap            9
017500150820     c                   add       1             corretti          7 0
017600150820   x2c                   else
017700150820     c* se il livello  di affidabilità non è "3" uso tellus
017800150820    3c                   if        o95lia<'3'
017900150826     c                   eval      wok='N'
018000150820     c
018100150826   x3c                   else
018200150826     c                   add       1             indok             7 0
018300150820    3c                   endif
018400150820    2c                   endif
018500150820   x1c                   else
018600150826     c                   eval      wok='N'
018700150820     c                   add       1             noprv             7 0
018800150820    1c*
018900150820     c                   endif
019000150820
019100150826     c                   if        wok=' '
019200150826     c                   add       1             tuttook           7 0
019300150826     c                   endif
019400150826
019500150820     c                   add       1             totale            7 0
019600150825     c     kvat          chain     Fivat01l
019700150825     c                   movel     vatnot        parcel           14
019800150820     c                   except    riga
019900150826     c                   endif
020000150826
020100150820    0c                   enddo
020200150820      *
020300150820     c                   except    tot
020400031017
020500031017     c                   Eval      *InLr = *On
020600031017
020700150820     oprtf198   e            Riga           1
020800150820     o                                           +1 'DPD: '
020900150825     o                       parcel              +1
021000150825     o                       vabIND              +1
021100150820     o                       vablod              +1
021200150820     o                       vabprd              +1
021300150820     o                       vabcad              +1
021400150820     o                                           +2 'new'
021500150820     o                       newprv              +1
021600150820     o                       newcap              +1
021700150820     o                       newloc              +1
021800150820     o                       newind              +1
021900150820     o          e            tot            1
022000150828     o                                           +1 'DPD: totale bolle del '
022100150828     o                       kmgs                +0
022200150828     o                       totale        2     +1
022300150826     o                                           +1 'TUTTO OK'
022400150826     o                       tuttook       2     +0
022500150820     o                                           +1 'di cui PRV non trovata'
022600150820     o                       noprv         2     +0
022700150820     o                                           +1 'di cui loc in lingua'
022800150820     o                       loci          2     +0
022900150826     o                                           +1 'di cui lia>=3'
023000150825     o                       indok         2     +0
023100150825     o          e            tot            1
023200150820     o                                           +1 'di cui loc solo numeri'
023300150820     o                       locnum        2     +0
023400150826     o                                           +1 'di cui loc errate'
023500150820     o                       loc_non       2     +0
023600150826     o                                           +1 'di cui rag.soc   vuota'
023700150826     o                       norsd         2     +0
023800150826     o                                           +1 'di cui località  vuota'
023900150826     o                       nolod         2     +0
024000150826     oprtf198   e            tot            1
024100150826     o                                           +1 'di cui indirizzo vuoto'
024200150826     o                       noind         2     +0
024300150826     o                                           +1 'di cui indirizzo < 6 char'
024400150826     o                       cortoind      2     +0
024500150820     oprtf198   e            tot            1
024600150820     o                                           +1 'di cui corretto il cap'
024700150820     o                       corretti      2     +0
024800150820**
024900150820ROMA
025000150820VENEZIA
025100150820TORINO
025200150820MILANO
025300150820NAPOLI
025400150820**
025500150820ROME
025600150820VENICE
025700150820TURIN
025800150820MILAN
025900150820NAPLES
