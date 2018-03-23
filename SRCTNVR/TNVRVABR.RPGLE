000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400150820      *       correzione CAP sulle bolle
000500031017      *---------------------------------------------------------------*
000600150820     fFivab01l  if   e           k Disk
000700150825     fFivat01l  if   e           k Disk
000800150820     fazcpl01l  if   e           k Disk
000900150820     ftabel00f  if   e           k Disk
001000150820     fprtf198   o    f  198        Printer Oflind(*InOf)
001100150820     D TISI95DS      E DS
001200150820     D fnlv13ds      E DS
001300150820     D kpjba         E DS
001400150820     D dspr          E DS
001500150820     D W0140           S             14  0
001600150820     D WORA            S              6  0
001700150820     D XX              S              3  0 INZ
001800150820     D WDTGIO          S              8  0
001900150820     D DATEU           S              8  0
002000150820     D DATA_eur        S               D   DATFMT(*eur)
002100150820
002200150820     D reg             S             25    DIM(55)
002300150820     D LOC_I           S             35    DIM(5) CTDATA PERRCD(1)
002400150820     D LOC_E           S             35    DIM(5) CTDATA PERRCD(1)
002500150820
002600150820     D digits          c                   '0123456789 /-'
002700150820
002800150820      /COPY GAITRASRC/SRCUB,UBTL05D02
002900150820     D   pInNazione    S              3A
003000150820     D   pInProvincia  S              4A
003100150820     D   pInComune     S            128A
003200150820     D   pInLocalita   S            128A
003300150820     D   pInCAP        S              9A
003400150820     D   pInVia        S            128A
003500150820     D   pInCivico     S             10A
003600150820     D   pInScore      S             10A
003700150820     D   pOutEsito     S              1A
003800150820     D   pOutEsitoDes  S            128A
003900150820     D   pOutTotFound  S              4S 0
004000150820     D   pOutResults   DS                  dim(10) likeds(dsOutUBTL05R02)
004100150820
004200031017
004300150820     c     kcpl          klist
004400150820     c                   kfld                    p96ver
004500150820     c                   kfld                    vabnzd
004600150820     c                   kfld                    vabcad
004700031017
004800150820     c     ktab          klist
004900150820     c                   kfld                    codut             1 0
005000150820     c                   kfld                    cod               2
005100150820     c                   kfld                    key               8
005200150820     c     ktab2         klist
005300150820     c                   kfld                    codut             1 0
005400150820     c                   kfld                    cod               2
005500150825     c     kvat          klist
005600150825     c                   kfld                    VAbFGS
005700150825     c                   kfld                    VAbCCM
005800150825     c                   kfld                    VAbAAS
005900150825     c                   kfld                    VAbLNP
006000150825     c                   kfld                    VAbNRS
006100150825     c                   kfld                    VAbNSP
006200150825     c                   kfld                    ktrc              1
006300150825     c                   eval      ktrc='E'
006400150820     c* carico le regioni
006500150820     c                   z-add     1             codut
006600150820     c                   movel     '14'          cod
006700150820     c                   z-add     1             xx
006800150820     c     ktab2         setll     tabel00f
006900150820     c     ktab2         reade     tabel00f
007000150820     c                   dow       not %eof(tabel00f)
007100150820     c                   add       1             xx
007200150820     c                   movel     tbluni        reg(xx)
007300150820     c     ktab2         reade     tabel00f
007400150820     c                   enddo
007500150820     c
007600150820     c                   movel     'PR'          cod
007700150820      * Recupero data e ora
007800150820     C                   TIME                    WORA
007900150820     C                   TIME                    W0140
008000150820      * UDATE IN GGMMAAAA
008100150820     C                   MOVE      W0140         WDTGIO
008200150820      * UDATE IN AAAAMMGG
008300150820     C     *eur          MOVEL     WDTGIO        DATA_eur
008400150820     C     *iso          MOVEL     DATA_eur      dateu
008500150820      *
008600150820     c* recupera versione cappario
008700150820     C                   Z-ADD     dateu         P96DRI            8 0
008800150820     C                   Z-ADD     *ZEROS        P96VER            5 0
008900150820     C                   CALL      'TISI96R'
009000150820     C                   PARM                    P96DRI
009100150820     C                   PARM                    P96VER
009200031017
009300150820     c     1900083       Setll     Fivab01l
009400150820do  0c                   Do        *Hival
009500150820     c     1900083       Reade     Fivab01l
009600150820     c                   If        %Eof(Fivab01l)
009700031017     c                   Leave
009800031017     c                   EndIf
009900150820
010000150820     C                   CLEAR                   newprv
010100150820     C                   CLEAR                   newcap
010200150820     C                   CLEAR                   newloc           35
010300150820     C                   CLEAR                   newind           35
010400150820     C                   CLEAR                   kloc             35
010500150820     C                   CLEAR                   esloc             1
010600150820     C                   CLEAR                   eslocnum          1
010700150820
010800150820     C                   CLEAR                   TISI95DS
010900150820     C                   MOVEL     vabCAD        I95CAP
011000150820     C                   MOVEL     DATEU         I95DAT
011100150820     C                   MOVEL     '3'           I95TCN
011200150820     C                   CALL      'TISI95R'
011300150820     C                   PARM                    TISI95DS
011400150820      *     Errore
011500150820    1C     O95ERR        IFeq      *BLANKS
011600150820     C                   MOVEL     O95PRV        newprv            2
011700150825     c* di cui indirizzo vuoto
011800150825     c                   if        vabind=*blanks
011900150825     c                   add       1             noind             7 0
012000150825     c                   endif
012100150825
012200150820     c* Se la località è un numero la unisco all'indirizzo
012300150820    2c                   if        %check(digits:vablod)=0
012400150820     c                   eval      newind=%trim(vabind)+' ' + vablod
012500150820     c                   add       1             locnum            7 0
012600150820     c                   eval      eslocnum='S'
012700150820     c* se la località inizia per numero, per non perderlo lo unisco all'indirizzo
012800150820   x2c                   else
012900150820   2ac                   if        %subst(vabind:33:3)='   '
013000150820     c                   z-add     1             xx
013100150820    3c                   dow       %check(digits:%subst(vablod:xx:1))=0
013200150820    4c                   if        xx=1
013300150820     c                   eval      newind=%trim(vabind)+' ' +%subst(vablod:xx:1)
013400150820   x4c                   else
013500150820     c                   eval      newind=%trim(newind)+ %subst(vablod:xx:1)
013600150820    4c                   endif
013700150820     c
013800150820     c                   add       1             xx
013900150820    4c                   if        xx>35
014000150820     c                   leave
014100150820    4c                   endif
014200150820    3c                   enddo
014300150820     c
014400150820   2ac                   endif
014500150820    2c                   endif
014600150820     c*
014700150820     C* SE LA LOCALITÀ È STRANIERA --> "ITALIANIZZO"
014800150820     C                   Z-ADD     1             XX                3 0
014900150820     C                   EVAL      XX=%LOOKUP(VABLOD:LOC_E)
015000150820    2C                   IF        xx>0
015100150820     c                   movel     loc_i(XX)     newloc
015200150820     c                   add       1             loci              7 0
015300150820   x2c                   else
015400150820     c* se la localitÀ è la sigla di una provincia --> decodifico
015500150820     c*  oppure trattini, o italy, prendo del CAP la prima località valida
015600150820     c*  e vedo se riesco a trovare la località giusta con tellus
015700150820     c                   eval      xx=1
015800150820     c                   eval      xx=%checkr(' ':vablod)
015900150820    3c                   if        xx=2 or %subst(vablod:1:4)='ITAL' or
016000150820     c                             %subst(vablod:1:2)='--'  or vablod=*blanks
016100150820     c                             or eslocnum='S' or %lookup(vablod:reg)>0
016200150820     c*
016300150820     c                   add       1             loc_non           7 0
016400150820     c
016500150820     c     kcpl          chain     azcpl01l
016600150820    4c                   if        %found(azcpl01l)
016700150820     c                   movel     cplloc        kloc             35
016800150820     c                   eval      esloc='N'
016900150820    4c                   endif
017000150820     c
017100150820     c**                 movel     vablod        key
017200150820     c**   ktab          chain     tabel00f
017300150820     c**                 if        %found(tabel00f)
017400150820     c**                 movel     tbluni        dspr
017500150820     c**                 movel     §prdes        newloc
017600150820     c**                 add       1             locprv            9 0
017700150820     c**                 endif
017800150820     c
017900150820    3c                   endif
018000150820    2c                   endif
018100150820     c
018200150820     c                   clear                   fnlv13ds
018300150820     C                   MOVEL     '7'           I95TCN
018400150820     c                   movel     vablod        i95loc
018500150820     c                   movel     newprv        i95prv
018600150820    2c                   if        newloc<>*blanks
018700150820     c                   movel     newloc        i95loc
018800150820    2c                   endif
018900150820     c
019000150820     C                   MOVEL     'Y'           I13CNV
019100150820     c                   movel     vabind        i95ind
019200150820    2c                   if        newind<>*blanks
019300150820     c                   movel     newind        i95ind
019400150820    2c                   endif
019500150820    c
019600150820     C                   CALL      'FNLV13R'
019700150820     C                   PARM                    KPJBA
019800150820     C                   PARM                    fnlv13ds
019900150820     C                   PARM                    tisi95ds
020000150820     C                   PARM      ' '           flgbac            1
020100150820    2c                   if        o13roc='S'
020200150820     c                   movel     o95cap        newcap            9
020300150820     c                   add       1             corretti          7 0
020400150820   x2c                   else
020500150820     c* se il livello  di affidabilità non è "3" uso tellus
020600150820    3c                   if        o95lia<'3'
020700150820     c
020800150820     c* altrimenti provo a richiamare TELLUS
020900150820     C                   EVAL      pInNazione   = i95nar
021000150820     C                   EVAL      pInProvincia = i95prv
021100150820     C                   EVAL      pInComune    = i95loc
021200150820     c* se località "finta" provo ad impostare la prima località del cap
021300150820    4c                   if        esloc='N' and kloc<>*blanks
021400150820     C                   EVAL      pInComune    = kloc
021500150820    4c                   endif
021600150820    c
021700150820     C                   EVAL      pInLocalita  = *blanks
021800150820     C                   EVAL      pInCAP       = i95cap
021900150820     C                   EVAL      pInVia       = i95ind
022000150820     C                   EVAL      pInCivico    = *blanks
022100150820     C                   EVAL      pInScore     = *blanks
022200150820     C                   CALL      'UBTL05R02'
022300150820     C                   PARM                    pInNazione
022400150820     C                   PARM                    pInProvincia
022500150820     C                   PARM                    pInComune
022600150820     C                   PARM                    pInLocalita
022700150820     C                   PARM                    pInCAP
022800150820     C                   PARM                    pInVia
022900150820     C                   PARM                    pInCivico
023000150820     C                   PARM                    pInScore
023100150820     C                   PARM                    pOutEsito
023200150820     C                   PARM                    pOutEsitoDes
023300150820     C                   PARM                    pOutTotFound
023400150820     C                   PARM                    pOutResults
023500150820     c                   z-add     1             inx               3 0
023600150820    4c                   dow       inx<=pOutTotFound
023700150820    5c                   if        pOutResults(inx).esatto='0' and
023800150820     c                             pOutResults(inx).LivNorm='V'
023900150820     c                   eval      newcap  =pOutResults(inx).CAP
024000150820     c                   eval      newloc  =pOutResults(inx).comune
024100150820     c                   leave
024200150820    5c                   endif
024300150820     c                   add       1             inx
024400150820    4c                   enddo
024500150820
024600150820   3ac                   if        newcap<>*blanks
024700150820    4c                   if        newcap<>vabcad
024800150820    5c                   if        esloc='N'
024900150820     c                   add       1             tel_capN          7 0
025000150820   x5c                   else
025100150820     c                   add       1             tel_cap           7 0
025200150820    5c                   endif
025300150820     c
025400150820   x4c                   else
025500150820     c                   clear                   newcap
025600150820    5c                   if        newloc<>vablod
025700150820    6c                   if        esloc='N'
025800150820     c                   add       1             tel_locN          7 0
025900150820     c                   else
026000150820     c                   add       1             tel_loc           7 0
026100150820    6c                   endif
026200150820     c
026300150820   x5c                   else
026400150820     c                   clear                   newloc
026500150820    5c                   endif
026600150820    4c                   endif
026700150820   3ac                   endif
026800150820     c
026900150825   x3c                   else
027000150825     c                   add       1             indok             7 0
027100150820    3c                   endif
027200150820    2c                   endif
027300150820   x1c                   else
027400150820     c                   add       1             noprv             7 0
027500150820    1c*
027600150820     c                   endif
027700150820     c* Se la località nuova non c'è inutile cambiare l'indirizzo  ??
027800150820
027900150820     c                   add       1             totale            7 0
028000150825     c     kvat          chain     Fivat01l
028100150825     c                   movel     vatnot        parcel           14
028200150820     c                   except    riga
028300150820    0c                   enddo
028400150820      *
028500150820     c                   except    tot
028600031017
028700031017     c                   Eval      *InLr = *On
028800031017
028900150820     oprtf198   e            Riga           1
029000150820     o                                           +1 'DPD: '
029100150825     o                       parcel              +1
029200150825     o                       vabIND              +1
029300150820     o                       vablod              +1
029400150820     o                       vabprd              +1
029500150820     o                       vabcad              +1
029600150820     o                                           +2 'new'
029700150820     o                       newprv              +1
029800150820     o                       newcap              +1
029900150820     o                       newloc              +1
030000150820     o                       newind              +1
030100150820     o          e            tot            1
030200150820     o                                           +1 'DPD: totale bolle '
030300150820     o                       totale        2     +0
030400150820     o                                           +1 'di cui PRV non trovata'
030500150820     o                       noprv         2     +0
030600150820     o                                           +1 'di cui loc in lingua'
030700150820     o                       loci          2     +0
030800150825     o                                           +1 'di cui tutto ok(lia=3)'
030900150825     o                       indok         2     +0
031000150825     o          e            tot            1
031100150820     o                                           +1 'di cui loc solo numeri'
031200150825     o                                           +1 'aggiunto all''indirizzo'
031300150820     o                       locnum        2     +0
031400150820     o                                           +1 'di cui loc inesistenti'
031500150820     o                       loc_non       2     +0
031600150825     o                                           +1 'di cui indirizzo vuoto'
031700150825     o                       noind         2     +0
031800150820     oprtf198   e            tot            1
031900150820     o                                           +1 'di cui corretto il cap'
032000150820     o                       corretti      2     +0
032100150820     o                                           +1 'di cui LOC da Tellus'
032200150820     o                       tel_loc       2     +0
032300150820     o                                           +1 'di cui CAP/LOC da Tellus'
032400150820     o                       tel_cap       2     +0
032500150820     o                                           +1 'di cui con LOC INESISTENTE'
032600150820     o                                           +1 'LOC da Tellus'
032700150820     o                       tel_locn      2     +0
032800150820     o                                           +1 'CAP da Tellus'
032900150820     o                       tel_capn      2     +0
033000150820**
033100150820ROMA
033200150820VENEZIA
033300150820TORINO
033400150820MILANO
033500150820NAPOLI
033600150820**
033700150820ROME
033800150820VENICE
033900150820TURIN
034000150820MILAN
034100150820NAPLES
