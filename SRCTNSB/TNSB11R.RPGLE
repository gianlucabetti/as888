000100040216     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040216
000300040216      *------------------------------------------------------------------------*
000400040216      *           Elenco spedizioni senza immagine LDV
000500040216      *------------------------------------------------------------------------*
000600040216     fAzorg01l  if   e           k Disk
000700040216     fTnsb11d   cf   e             Workstn
000800040218
000900040218      *------------------------------------------------------------------------*
001000040218      *   C A M P I   D I   L A V O R O
001100040218      *------------------------------------------------------------------------*
001200040218     d wdcmd           s               d   datfmt(*iso)
001300040218     d wdcma           s               d   datfmt(*iso)
001400040218     d wgio            s              5  0
001500040310     d w003a           s              3
001600040216
001700040216      *------------------------------------------------------------------------*
001800040216      *   S C H I E R E
001900040216      *------------------------------------------------------------------------*
002000040216
002100120912     d Msg             s             78    Dim(20) Ctdata Perrcd(1)
002200040216
002300040216      *------------------------------------------------------------------------*
002400040216      *   D S   I N T E R N E / E S T E R N E
002500040216      *------------------------------------------------------------------------*
002600040216
002700040216     d wlbdat          ds                  Inz
002800040216     d  g02dat                 1      8  0
002900040216     d  g02inv                 9     16  0
003000040216     d  g02err                17     17
003100040216     d  g02tgi                18     22  0
003200040216
003300040216     d Param           ds
003400040216     d  PaLna                  1      3  0
003500040216     d  PaDcd                  4     11  0
003600040216     d  PaDca                 12     19  0
003700040310     d  PaEEX                 20     20
003800040630     d  PaDPD                 21     21
003900040715     d  PaFile                22     22
004000060814     d  PaFileD               23     23
004100120913     d  PaKsc                 24     30  0
004200120913     d  PaRag                 31     60
004300040216
004400040216     d AzuteDs       e ds                  ExtName(AzUte00F)
004500040216     d DDatiUte      e ds
004600040216     d Tibs34Ds      e ds                  Inz
004700120912
004800120912       // - Controllo Abilitazioni Utente
004900120913     d TNTAA1ds      e ds                  inz
005000170126
005100170126      // - Reperimento filiali gestite dall'utente
005200170126     d TRUL31DS      e ds
005300170126     d  POG                   10    759    dim(250)
005400040216
005500040216     d Og143         e ds
005600040219     d Og146         e ds
005700040216
005800040216     d Kpjba         e ds
005900120912
006000120912      //---------------------------------------------------------------
006100120912      //?Prototipi.
006200120912      //---------------------------------------------------------------
006300120912      /copy gaitrasrc/srcprotopi,tibs69r
006400120912      /copy gaitrasrc/srcprotopr,tibs69r
006500120912      /copy gaitrasrc/srcprotopr,tntaa1c
006600170126
006700170126       // - Caricamento Filiali in gestione
006800170126     d TRUL31R         pr                  extpgm('TRUL31R')
006900170126     d  kpjba                              likeds(kpjba)
007000170126     d  trul31ds                           likeds(trul31ds)
007100040216
007200040216      *------------------------------------------------------------------------*
007300040216      *   I N D I C A T O R I
007400040216      *------------------------------------------------------------------------*
007500040216      * 10 - Linea di arrivo protetta
007600040216      * 11 - Linea di arrivo posizionamento cursore
007700040216      * 12 - Data consegna merce dal posizionamento cursore
007800040216      * 13 - Data consegna merce  al posizionamento cursore
007900040630      * 14 - Utente di sede
008000060814      * 15 - Utente EDP (sede)
008100120912      * 16 - Cliente
008200040216      * 28 - Msg di errore
008300040216      *------------------------------------------------------------------------*
008400040216
008500040216      *------------------------------------------------------------------------*
008600040216
008700040216do  1c                   Do        *Hival
008800040216     c                   Exfmt     Sb11d01
008900040216     c                   Eval      *In28 = *Off
009000040216
009100040216      * F3 = Fine
009200040216     c   kc              Leave
009300040216
009400040216      * Controllo i dati immessi
009500040216     c                   ExSr      Sr_Ctrd01
009600040216
009700040216      * F6 = Elaborazione
009800040216if  2c                   If        *InKf and Not *In28
009900040216     c                   Movel(p)  Param         Kpjbu
010000040216     c                   Eval      Kcoaz = 'SB12'
010100120913     c                   IF        knmus = *all'1'
010200120913     c                   call      'TNSB12R'
010300120913     c                   Parm                    Kpjba
010400120913     c                   leave
010500120913     c                   ELSE
010600040216     c                   Call      'BCH10'
010700040216     c                   Parm                    Kpjba
010800040217     c                   Leave
010900120913     c                   ENDIF
011000040216    2c                   EndIf
011100040216
011200040216    1c                   EndDo
011300040216
011400040216     c                   Eval      *InLr = *On
011500040216
011600040216      *------------------------------------------------------------------------*
011700040216      * CONTROLLO I DATI IMMESSI
011800040216      *------------------------------------------------------------------------*
011900040216     c     Sr_Ctrd01     BegSr
012000040216
012100040216     c                   Setoff                                       111213
012200120912     c                   setoff                                       16
012300120912     c                   clear                   V1Dksc
012400120912
012500120912      /free
012600120912       //?Controllo cliente e lna, sono in alternativa
012700120912         IF  V1Cksc > 0 and V1Clna > 0;
012800120912           *in28 = *on;
012900120912           *in16 = *on;
013000120912           V1Cmsg = msg(10);
013100120912           leavesr;
013200120912         ENDIF;
013300120912
013400120912       //?Controllo cliente valido
013500120912         IF  V1Cksc > 0;
013600120912           clear TIBS69DS;
013700120912           I69kac = %dec(V1Cksc:7:0);
013800120912           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
013900120912           IF  O69err <> *blanks;
014000120912             *in28 = *on;
014100120912             *in16 = *on;
014200120912             V1Cmsg = msg(12);
014300120912             leavesr;
014400120912           ENDIF;
014500120912           V1Dksc = ACOrag;
014600120912       //?Controllo cliente utente abilitato
014700120912           reset TNTAA1DS;
014800120912           ITAA1caut = '§utegtc';
014900120912           ITAA1ksc  = V1Cksc;
015000120912           kpjbu     = TNTAA1DS;
015100120912           tntaa1c (kpjba);
015200120912           TNTAA1DS = kpjbu;
015300120912           IF  OTAA1fabi = 'N';
015400120912             *in28 = *on;
015500120912             *in16 = *on;
015600120912             V1Cmsg = msg(12);
015700120912             leavesr;
015800120912           ENDIF;
015900121003         ENDIF;
016000121003         PaKsc = V1Cksc;
016100121003         PaRag = V1Dksc;
016200120912
016300120912      /end-free
016400040216
016500040630      * Controllo se linea di arrivo valida
016600040630if  1c                   If        V1cLna > *Zeros
016700040216     c                   Clear                   Og143
016800040216     c                   Clear                   DesLna
016900040216     c     V1cLna        Chain     Azorg01l
017000040216if  2c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks or
017100040216     c                             (OrgFag <> 'F' and OrgFag <> 'A')
017200040216     c                   Eval      V1cMsg = Msg(01)
017300040216     c                   Eval      *In28 = *On
017400040216     c                   Eval      *In11 = *On
017500040216     c                   Leavesr
017600040216    2c                   EndIf
017700040216     c                   Movel     OrgDe3        Og143
017800040216if  2c                   If        §OgNtw = 'DPD' or §OgNtw = 'PPT'
017900040216     c                   Eval      V1cMsg = Msg(01)
018000040216     c                   Eval      %Subst(V1cMsg:24:7)  = 'Network'
018100040216     c                   Eval      %Subst(V1cMsg:32:3)  = §OgNtw
018200040216     c                   Eval      *In28 = *On
018300040216     c                   Eval      *In11 = *On
018400040216     c                   Leavesr
018500040216    2c                   EndIf
018600120912       //?Se utente di 2° liv la LNA deve essere = DUTpou
018700120912      /free
018800120912        IF  DUTlpo <> '1' and not *in14 and V1Clna <> DUTpou;
018900120912          *in28 = *on;
019000120912          *in11 = *on;
019100120912          V1Cmsg = msg(11);
019200120912          leavesr;
019300120912        ENDIF;
019400120912      /end-free
019500040310      * deve essere una linea gestita dal primo livello
019600040310if  2c                   If        DutLpo = '1'
019700170126     c                   move      V1Clna        w003a
019800170126     c     w003a         Lookup    pog                                    30
019900040310if  3c                   If        Not *In30
020000040310     c                   Move      DutPou        w003a
020100040310     c                   Eval      V1cMsg = Msg(06)
020200040310     c                   Eval      %Subst(V1cMsg:41:3) = w003a
020300040310     c                   Eval      *In28 = *On
020400040310     c                   Eval      *In11 = *On
020500040310     c                   Leavesr
020600040310    3c                   EndIf
020700040310    2c                   EndIf
020800040219     c                   Movel     OrgDe6        Og146
020900040219if  2c                   If        §OgDse = *Blanks or §OgDse = *Zeros
021000040219     c                   Eval      V1cMsg = Msg(01)
021100040219     c                   Eval      %Subst(V1cMsg:24:20) = 'Procedura non attiva'
021200040219     c                   Eval      *In28 = *On
021300040219     c                   Eval      *In11 = *On
021400040219     c                   Leavesr
021500040219    2c                   EndIf
021600040216     c                   Movel     OrgDes        DesLna
021700040630   x1c                   Else
021800120913       //?Se non è presente il codice cliente
021900120913     c                   IF        V1Cksc = 0
022000040630      * Obbligatoria se non è un utente di sede
022100040630if  2c                   If        DutPou <> 46
022200040630     c                   Eval      V1cMsg = Msg(01)
022300040630     c                   Eval      *In28 = *On
022400040630     c                   Eval      *In11 = *On
022500040630     c                   Leavesr
022600040630    2c                   EndIf
022700120913     c                   ENDIF
022800040216    1c                   EndIf
022900040216     c                   Movel     V1cLna        PaLna
023000040216
023100040216      * Data consegna merce dal
023200040216if  1c                   If        V1cDcd = *Zeros
023300040216     c                   Eval      V1cMsg = Msg(02)
023400040216     c                   Eval      *In28 = *On
023500040216     c                   Eval      *In12 = *On
023600040216     c                   Leavesr
023700040216    1c                   EndIf
023800040216
023900040216     c                   Clear                   wlbdat
024000040216     c                   Z-add     V1cDcd        g02dat
024100040216     c                   Call      'XSRDA8'
024200040216     c                   Parm                    wlbdat
024300040216if  1c                   If        g02err = '1'
024400040216     c                   Eval      V1cMsg = Msg(03)
024500040216     c                   Eval      *In28 = *On
024600040216     c                   Eval      *In12 = *On
024700040216     c                   Leavesr
024800040216    1c                   EndIf
024900040216     c                   Z-add     g02dat        V1cDcd
025000040216     c                   Z-add     g02inv        PaDcd
025100040216
025200040216      * Data consegna merce al
025300040216if  1c                   If        V1cDca = *Zeros
025400040216     c                   Eval      V1cDca = V1cDcd
025500040216     c                   Eval      PaDca = PaDcd
025600040216     c                   Leavesr
025700040216    1c                   EndIf
025800040216
025900040216     c                   Clear                   wlbdat
026000040216     c                   Z-add     V1cDca        g02dat
026100040216     c                   Call      'XSRDA8'
026200040216     c                   Parm                    wlbdat
026300040216if  1c                   If        g02err = '1'
026400040216     c                   Eval      V1cMsg = Msg(03)
026500040216     c                   Eval      *In28 = *On
026600040715     c                   Eval      *In13 = *On
026700040216     c                   Leavesr
026800040216    1c                   EndIf
026900040216     c                   Z-add     g02dat        V1cDca
027000040216     c                   Z-add     g02inv        PaDca
027100040216
027200040216      * Range
027300040216if  1c                   If        PaDcd > PaDca
027400040216     c                   Eval      V1cMsg = Msg(04)
027500040216     c                   Eval      *In28 = *On
027600040216     c                   Eval      *In12 = *On
027700040216     c                   Leavesr
027800040216    1c                   EndIf
027900040218
028000040218     c     *Iso          Movel     PaDcd         wdcmd
028100040218     c     *Iso          Movel     PaDca         wdcma
028200040218
028300040218      * Range massimo di 31 gg
028400040218     c     wdcma         Subdur    wdcmd         wgio:*d
028500040218     c                   Eval      wgio = wgio + 1
028600040218if  1c                   If        wgio > 31
028700040218     c                   Eval      V1cMsg = Msg(05)
028800040218     c                   Eval      *In28 = *On
028900040218     c                   Eval      *In12 = *On
029000040218     c                   Leavesr
029100040218    1c                   EndIf
029200040216
029300040311      * Flag solo partenza EEX impostabile solo x LNA = 0
029400040311if  3c                   If        V1CEEX = 'S' and V1CLNA <> *zeros
029500040311     c                   Eval      V1cMsg = Msg(07)
029600040311     c                   Eval      *In28 = *On
029700040311     c                   Eval      *In11 = *On
029800040311     c                   Leavesr
029900040311    3c                   EndIf
030000040310     C                   movel     V1Ceex        PAeex
030100040630
030200040630      * Flag solo partenza DPD impostabile solo x LNA = 0
030300040630if  3c                   If        V1CDPD = 'S' and V1CLNA <> *zeros
030400040630     c                   Eval      V1cMsg = Msg(08)
030500040630     c                   Eval      *In28 = *On
030600040630     c                   Eval      *In11 = *On
030700040630     c                   Leavesr
030800040630    3c                   EndIf
030900040630     c                   Movel     V1cDpd        PaDPD
031000040715
031100060814      * Flags creazione file impostabili solo x LNA = 0
031200060814if  3c                   If        V1CLNA <> *zeros  and
031300060814if  3c                             (V1CFILE = 'S'  or  V1CFILED = 'S')
031400040715     c                   Eval      V1cMsg = Msg(09)
031500040715     c                   Eval      *In28 = *On
031600040715     c                   Eval      *In11 = *On
031700040715     c                   Leavesr
031800040715    3c                   EndIf
031900040715     c                   Movel     V1cFile       PaFILE
032000060814     c                   Movel     V1cFileD      PaFILEd
032100060814
032200040216     c                   EndSr
032300040216
032400040216      *------------------------------------------------------------------------*
032500040216      * ROUTINE INIZIALE
032600040216      *------------------------------------------------------------------------*
032700040216     c     *Inzsr        BegSr
032800040216
032900040216     c     *entry        plist
033000040216     c                   parm                    Kpjba
033100040216
033200040216     c     *dtaara       define    §azute        azuteds
033300040216     c     *dtaara       define    §datiute      ddatiute
033400040216     c                   in(E)     *dtaara
033500040216     c                   If        %error  or RSUT = *blanks
033600040216     c                   CLEAR                   tibs34ds
033700040216     c                   CALL      'TIBS34R'
033800040216     c                   PARM                    tibs34ds
033900040216     c                   in        *dtaara
034000040216     c                   EndIf
034100040216
034200040216      * -> Imposto il p.o. utente
034300040216      * Utente di sede
034400060814if  1c                   IF        DutPou = 46
034500040216     c                   Clear                   V1cLna
034600040216     c                   Clear                   DesLna
034700040216     c                   Eval      *In10 = *Off
034800040630     c                   Eval      *In14 = *On
034900060814    2c                   If        %subst(dutute:1:3) = 'EDP'
035000060814     c                   Eval      *In15 = *On
035100060814   x2c                   Else
035200060814     c                   Eval      *In15 = *Off
035300060814    2c                   EndIf
035400060814      *
035500060814   x1c                   ELSE
035600040216      * Utente di Filiale proteggo il campo
035700040310      * se non è un primo livello
035800040216     c                   Eval      V1cLna = DutPou
035900040216     c                   Eval      DesLna = DutDpo
036000040310    2c                   If        DutLpo = '1'
036100040310     c                   Eval      *In10 = *Off
036200040310   x2c                   Else
036300040310     c                   Eval      *In10 = *On
036400040310    2c                   EndIf
036500040630     c                   Eval      *In14 = *Off
036600060814     c                   Eval      *In15 = *Off
036700060814    1c                   ENDIF
036800040310
036900040310      * Se sono primo livello carico £1
037000040310    2c                   If        DutLpo = '1'
037100170126      /free
037200170126       //?Carico le filiali abilitate all'utente
037300170126         clear TRUL31DS;
037400170126         I31abi = 'RA';
037500170126         I31cdi = DUTdis;
037600170126         I31car = DUTare;
037700170126         I31cpo = DUTpou;
037800170126         TRUL31R (kpjba:trul31ds);
037900170126      /end-free
038000040310    1c                   EndIf
038100120912
038200120912       //?Visto che aggiungiamo in alternativa alla LNA il codice cliente
038300120912       //?sproteggo sempre la LNA nei controlli poi verifico che se
038400120912       //?secondo livello la lna deve essere = a DUTpou
038500120912     c                   eval      *in10 = *off
038600040216
038700040216     c                   EndSr
038800040216** Msg  Lungh. 78                                                            *
038900040216Linea di Arrivo errata                                                        01
039000040216Immettere la data consegna merce                                              02
039100040216Data formalmente errata                                                       03
039200040216Data consegna merce "dal" maggiore di data consegna merce "al"                04
039300040218Non è possibile elaborare un periodo superiore a 31 giorni                    05
039400040310Linea di Arrivo non in gestione al p.o.                                       06
039500040630Se scelte solo LNP Euroexpress è obbligatorio scegliere tutte le LNA          07
039600040630Se scelte solo LNP DPD è obbligatorio scegliere tutte le LNA                  08
039700040715Se scelta la creazione del file è obbligatorio scegliere tutte le LNA         09
039800120913Codice Cliente e Linea di Arrivo sono da immettere in alternativa!!           10
039900120912Linea di Arrivo non in gestione all'utente                                    11
040000120912Codice Cliente errato o non in gestione all'utente                            12
