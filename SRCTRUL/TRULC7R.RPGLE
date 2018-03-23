000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200011011      *
000300020207      *  Routine recupero tariffa di cartello
000400020207      * -------------------------------------
000500160216     ftntam00l  if   e           k disk
000600011019
000700021115     d msg             s             78    dim(10) ctdata perrcd(1)
000800020207
000900160215     d trulc7ds      e ds
001000020208     d dtca          e ds
001100021115     d ddfe          e ds
001200011029     d tibs02DS      e ds
001300011030     d kpjba         e ds
001400020211
001500020211     d x               s              2  0
001600020211     d xx              s              3  0
001700020208     d yy              s              3  0
001800021115     d Wctb            s              2  0
001900160224     d savksc          s                   like(tamksc)
002000160224     d savctr          s                   like(tamctr)
002100160224     d savddt          s                   like(tamddt)
002200160224     d savdst          s                   like(tamdst)
002300120920     D Volta1          s               n
002400011019
002500011019      *----------------------------------------------------------------
002600011019
002700011029     c     *entry        plist
002800160215     c                   parm                    trulc7ds
002900160216     c
003000160216     c     ktam          klist
003100160216     c                   kfld                    oc7kscc
003200160216     c                   kfld                    oc7ctrc
003300011019
003400020220     c                   eval      *in30 = *off
003500020220     c                   eval      *in31 = *off
003600160215     c                   clear                   oc7kscc
003700160215     c                   clear                   oc7ctrc
003800160215     c                   clear                   oc7prgc
003900160215     c                   clear                   oc7ddtc
004000160215     c                   clear                   oc7dstc
004100160215     c                   clear                   oc7err
004200160215     c                   clear                   oc7msg
004300011031
004400011031      * Controllo validità tipo lancio
004500160215     c                   if        ic7tla <> 'C' and ic7tla <> 'L' and
004600160215     c                             ic7tla <> *blanks
004700160215     c                   clear                   ic7tla
004800011031     c                   endif
004900011019
005000011019      * Se Tipo lancio 'C' --> solo chiusura file
005100160215     c                   if        ic7tla <> 'C'
005200160215
005300160303     c* se non è stato passato  network e fie ='X' --> errore
005400160303     c                   if        ic7tntw=*blanks and  ic7tfie='X'
005500160216     c                   eval      oc7err = 'E'
005600160216     c                   eval      oc7msg = msg(3)
005700160215     c                   else
005800011029     c                   exsr      elabora
005900160215     c                   endif
006000011029     c                   endif
006100011011
006200160215     c                   if        ic7tla <> *blanks
006300011031     c                   eval      *inlr = *on
006400011029     c                   else
006500011031     c                   eval      *inrt = *on
006600011029     c                   endif
006700011011
006800011011      *----------------------------------------------------
006900011019      * ELABORAZIONE
007000011011      *----------------------------------------------------
007100011029     c     elabora       begsr
007200020208
007300120920     C* eseguo questo reperimento solo la prima volta
007400160224    1C                   IF        Volta1 <> *on
007500120920     C                   EVAL      Volta1 =  *on
007600020208      * aggancio la tabella TCA
007700020208     c                   clear                   dtca
007800020208     c                   clear                   tibs02ds
007900020208     c                   movel     'C'           t02mod
008000020208     c                   movel     knsif         t02sif
008100020208     c                   movel     'TCA'         t02cod
008200020208     c                   movel(p)  '1'           t02ke1
008300020208     c                   call      'TIBS02R'
008400020208     c                   parm                    kpjba
008500020208     c                   parm                    tibs02ds
008600020208
008700160224    2c                   if        t02err = *blanks
008800020208     c                   movel     t02uni        dtca
008900160224    2c                   endif
009000020208
009100160224    2c                   if        t02err = 'E'
009200160216     c                   eval      oc7err = 'E'
009300160216     c                   eval      oc7msg = msg(1)
009400160216     c                   leavesr
009500160224     c
009600160224    2c                   endif
009700021115
009800021115      * aggancio la tabella DFE
009900021115     c                   clear                   ddfe
010000021115     c                   clear                   tibs02ds
010100150821     c                   movel     'L'           T02tla
010200021115     c                   movel     'C'           t02mod
010300021115     c                   movel     knsif         t02sif
010400021115     c                   movel     'DFE'         t02cod
010500021118     c                   movel(p)  'FED'         t02ke1
010600021115     c                   call      'TIBS02R'
010700021115     c                   parm                    kpjba
010800021115     c                   parm                    tibs02ds
010900021115
011000160224    2c                   if        t02err = *blanks
011100021115     c                   movel     t02uni        ddfe
011200160224    2c                   endif
011300120920     C*
011400160224     c* Pulisco i campi che mi servono per rileggere tntam
011500160224     c                   clear                   savctr
011600160224     c                   clear                   savksc
011700160224     c                   clear                   savddt
011800160224     c                   clear                   savdst
011900160224    1C                   ENDIF
012000020211
012100160215      * Il network vince sul FIE
012200160224    1c                   select
012300020211      * DPD
012400160215     c                   when      ic7tntw = 'DPD'  or
012500160215     c                             ic7tntw='   ' and ic7tfie = 'D'
012600160216     c                   eval      oc7kscc = §tcadc
012700160216     c                   eval      oc7ctrc = §tcadt
012800020211      * EuroExpress
012900160215     c                   when      ic7tntw = 'EEX' or
013000160215     c                             ic7tntw='   ' and ic7tfie = 'E'
013100160216     c                   eval      oc7kscc = §tcaec
013200160216     c                   eval      oc7ctrc = §tcaet
013300020211      * FedEx
013400160215     c                   when      ic7tntw = 'FED' or
013500160215     c                             ic7tntw='   ' and ic7tfie = 'F'  or
013600160215     c                             ic7tntw='   ' and ic7tfie = 'M'  or
013700160215     c                             ic7tntw='   ' and ic7tfie = 'N'
013800021115     c                   exsr      tarfedex
013900160215      * ITALIA
014000020211     c                   other
014100160216     c                   eval      oc7kscc= §tcabc
014200160216     c                   eval      oc7ctrc= §tcabt
014300160224    1c                   endsl
014400020211
014500020211      * Passata la data recupero progressivo
014600160224 b1 1c                   if        ic7dta <> *zeros
014700160224     c
014800160224     c* Se non ho modificato cod cliente/ctr e la data passata rientra nel DDT/DST
014900160224     c*  ripasso lo stesso progressivo, altrimenti rileggo
015000160224    2c                   if        savksc=oc7kscc and savctr=oc7ctrc and
015100160224     c                             ic7dta>=tamddt and ic7dta<=tamdst
015200160224     c                   movel     tamprg        oc7prgc
015300160224   x2c                   else
015400160224     c
015500020211      * Cerco progressivo
015600160216     c     ktam          setgt     tntam00l
015700160216     c     ktam          readpe    tntam00l
015800160224    3c                   dow       not %eof(tntam00l)
015900160216     c
016000160216     c                   if        ic7dta>=tamddt and ic7dta<=tamdst
016100160216     c                   movel     tamprg        oc7prgc
016200160216     c                   leave
016300160216     c                   endif
016400160216
016500160229     c                   if        ic7dta>tamdst
016600160216     c                   movel     tamprg        oc7prgc
016700160216     c                   eval      oc7msg = msg(5)
016800160216     c                   leave
016900160216     c                   endif
017000160216     c
017100160216     c     ktam          readpe    tntam00l
017200160224    3c                   enddo
017300160224    2c                   endif
017400160216     c
017500160216     c*
017600160224    2c                   if        oc7prgc=*blanks
017700160216     c                   eval      oc7err = 'E'
017800160216     c                   eval      oc7msg = msg(4)
017900160224     c                   else
018000160224     c                   eval      savksc=oc7kscc
018100160224     c                   eval      savctr=oc7ctrc
018200160224     c                   eval      savddt=tamddt
018300160224     c                   eval      savdst=tamdst
018400160224 e1 2c                   endif
018500160224     c
018600160224    1c                   endif
018700020211
018800020211     c                   endsr
018900021115      *----------------------------------------------------
019000021115      * Carico tariffa FedEx
019100021115      *----------------------------------------------------
019200021115     c     TarFedEx      BegSr
019300021115
019400021115     c                   Clear                   Wctb
019500160215     c
019600160215     c                   if        ic7tfie='M'
019700160216     c                   Eval      Oc7Kscc = §Tcafc
019800160216     c                   Eval      Oc7Ctrc = §Tcaft
019900160215     c                   Goto      EndTarFedEx
020000160215    1c                   EndIf
020100160215     c
020200160215     c                   if        ic7tfie='N'
020300160216     c                   Eval      Oc7Kscc = §Tcafcd
020400160216     c                   Eval      Oc7Ctrc = §Tcaftd
020500160215     c                   Goto      EndTarFedEx
020600160215    1c                   EndIf
020700021115
020800021115      * Se passato il codice tariffa devo stabilire in quale range
020900021115      * se merce o documenti
021000021115      * controllo solo gli ultimi due caratteri del codice tariffa passato
021100160215If  1c                   If        ic7Ctb <> *Blanks
021200160215     c                   Move      ic7Ctb        WCtb
021300021115If  2c                   If        WCtb >= §Dfedalm and WCtb <= §Dfealm
021400160216     c                   Eval      Oc7Kscc = §Tcafc
021500160216     c                   Eval      Oc7Ctrc = §Tcaft
021600021115    2c                   EndIf
021700021115If  2c                   If        WCtb >= §Dfedald and WCtb <= §Dfeald
021800160216     c                   Eval      Oc7Kscc = §Tcafcd
021900160216     c                   Eval      Oc7Ctrc = §Tcaftd
022000021115    2c                   EndIf
022100021115     c                   Goto      EndTarFedEx
022200021115    1c                   EndIf
022300021115
022400021115      * Se passato il tipo tariffa
022500021115      * per FedEx FM --> Merce
022600021115      *           FD --> Documenti
022700021115      * prendo la relativa cartello
022800160215If  1c                   If        ic7Tip = 'FM'
022900160216     c                   Eval      Oc7Kscc = §Tcafc
023000160216     c                   Eval      Oc7Ctrc = §Tcaft
023100021115     c                   Goto      EndTarFedEx
023200021115    1c                   EndIf
023300160215If  1c                   If        ic7Tip = 'FD'
023400160216     c                   Eval      Oc7Kscc = §Tcafcd
023500160216     c                   Eval      Oc7Ctrc = §Tcaftd
023600021115     c                   Goto      EndTarFedEx
023700021115    1c                   EndIf
023800021115
023900021115      * Se passato il peso
024000021115      * testo se superiore al limite previsto per la tariffa documenti
024100160215If  1c                   If        ic7Pkg > *Zeros
024200160215If  2c                   If        ic7Pkg > §DfePkgd
024300160216     c                   Eval      Oc7Kscc = §Tcafc
024400160216     c                   Eval      Oc7Ctrc = §Tcaft
024500021115   x2c                   Else
024600160216     c                   Eval      Oc7Kscc = §Tcafcd
024700160216     c                   Eval      Oc7Ctrc = §Tcaftd
024800021115    2c                   EndIf
024900021115     c                   Goto      EndTarFedEx
025000021115    1c                   EndIf
025100021115
025200021115      * Se non sono stati passati i campi necessari per reperire la cartello giusta
025300021115      * imposto lo stesso la cartello FEdEx merce, ma segnalo un errore
025400160216     c                   Eval      Oc7Kscc = §Tcafc
025500160216     c                   Eval      Oc7Ctrc = §Tcaft
025600021115      * Imposto anche un msg di errore
025700160216     c                   Eval      Oc7Err = 'E'
025800160216     c                   Eval      Oc7Msg = Msg(2)
025900021115
026000021115     c     EndTarFedEx   EndSr
026100020211
026200020207** MSG  Lungh. 78                                                            *
026300160215TRULC7R - Non trovata la tabella TCA, telefonare in SEDE                            1
026400160215TRULC7R - Mancano i dati per reperire la corretta Tariffa di Cartello FedEx
026500160215TRULC7R -Manca il network tariffa per reperire la corretta Tariffa di Cartello
026600160216TRULC7R -Non trovato progressivo
026700160216TRULC7R -Trovato progressivo scaduto
