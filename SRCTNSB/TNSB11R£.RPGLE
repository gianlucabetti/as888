000100040216     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040216
000300040216      *------------------------------------------------------------------------*
000400040216      *           Elenco spedizioni senza immagine LDV
000500040216      *------------------------------------------------------------------------*
000600040216
000700040216      *------------------------------------------------------------------------*
000800040216      *   N O T E
000900040216      *------------------------------------------------------------------------*
001000040216      * Compilare con: TGTRLS *CURRENT
001100040216      *------------------------------------------------------------------------*
001200040216
001300040216     fAzorg01l  if   e           k Disk
001400040216     fTnsb11d   cf   e             Workstn
001500040218
001600040218      *------------------------------------------------------------------------*
001700040218      *   C A M P I   D I   L A V O R O
001800040218      *------------------------------------------------------------------------*
001900040218     d wdcmd           s               d   datfmt(*iso)
002000040218     d wdcma           s               d   datfmt(*iso)
002100040218     d wgio            s              5  0
002200040310     d w003a           s              3
002300040216
002400040216      *------------------------------------------------------------------------*
002500040216      *   S C H I E R E
002600040216      *------------------------------------------------------------------------*
002700040216
002800120912     d Msg             s             78    Dim(20) Ctdata Perrcd(1)
002900040216
003000040216      *------------------------------------------------------------------------*
003100040216      *   D S   I N T E R N E / E S T E R N E
003200040216      *------------------------------------------------------------------------*
003300040216
003400040216     d wlbdat          ds                  Inz
003500040216     d  g02dat                 1      8  0
003600040216     d  g02inv                 9     16  0
003700040216     d  g02err                17     17
003800040216     d  g02tgi                18     22  0
003900040216
004000040216     d Param           ds
004100040216     d  PaLna                  1      3  0
004200040216     d  PaDcd                  4     11  0
004300040216     d  PaDca                 12     19  0
004400040310     d  PaEEX                 20     20
004500040630     d  PaDPD                 21     21
004600040715     d  PaFile                22     22
004700060814     d  PaFileD               23     23
004800120913     d  PaKsc                 24     30  0
004900120913     d  PaRag                 31     60
005000040216
005100040310     d Trul06ds      e ds
005200040310     d  l1                     1     90  0 Dim(30)
005300040216     d AzuteDs       e ds                  ExtName(AzUte00F)
005400040216     d DDatiUte      e ds
005500040216     d Tibs34Ds      e ds                  Inz
005600120912
005700120912       // - Controllo Abilitazioni Utente
005800120913     d TNTAA1ds      e ds                  inz
005900040216
006000040216     d Og143         e ds
006100040219     d Og146         e ds
006200040216
006300040216     d Kpjba         e ds
006400120912
006500120912      //---------------------------------------------------------------
006600120912      //?Prototipi.
006700120912      //---------------------------------------------------------------
006800120912      /copy gaitrasrc/srcprotopi,tibs69r
006900120912      /copy gaitrasrc/srcprotopr,tibs69r
007000120912      /copy gaitrasrc/srcprotopr,tntaa1c
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
019700040310     c     V1cLna        Lookup    l1                                     30
019800040310if  3c                   If        Not *In30
019900040310     c                   Move      DutPou        w003a
020000040310     c                   Eval      V1cMsg = Msg(06)
020100040310     c                   Eval      %Subst(V1cMsg:41:3) = w003a
020200040310     c                   Eval      *In28 = *On
020300040310     c                   Eval      *In11 = *On
020400040310     c                   Leavesr
020500040310    3c                   EndIf
020600040310    2c                   EndIf
020700040219     c                   Movel     OrgDe6        Og146
020800040219if  2c                   If        §OgDse = *Blanks or §OgDse = *Zeros
020900040219     c                   Eval      V1cMsg = Msg(01)
021000040219     c                   Eval      %Subst(V1cMsg:24:20) = 'Procedura non attiva'
021100040219     c                   Eval      *In28 = *On
021200040219     c                   Eval      *In11 = *On
021300040219     c                   Leavesr
021400040219    2c                   EndIf
021500040216     c                   Movel     OrgDes        DesLna
021600040630   x1c                   Else
021700120913       //?Se non è presente il codice cliente
021800120913     c                   IF        V1Cksc = 0
021900040630      * Obbligatoria se non è un utente di sede
022000040630if  2c                   If        DutPou <> 46
022100040630     c                   Eval      V1cMsg = Msg(01)
022200040630     c                   Eval      *In28 = *On
022300040630     c                   Eval      *In11 = *On
022400040630     c                   Leavesr
022500040630    2c                   EndIf
022600120913     c                   ENDIF
022700040216    1c                   EndIf
022800040216     c                   Movel     V1cLna        PaLna
022900040216
023000040216      * Data consegna merce dal
023100040216if  1c                   If        V1cDcd = *Zeros
023200040216     c                   Eval      V1cMsg = Msg(02)
023300040216     c                   Eval      *In28 = *On
023400040216     c                   Eval      *In12 = *On
023500040216     c                   Leavesr
023600040216    1c                   EndIf
023700040216
023800040216     c                   Clear                   wlbdat
023900040216     c                   Z-add     V1cDcd        g02dat
024000040216     c                   Call      'XSRDA8'
024100040216     c                   Parm                    wlbdat
024200040216if  1c                   If        g02err = '1'
024300040216     c                   Eval      V1cMsg = Msg(03)
024400040216     c                   Eval      *In28 = *On
024500040216     c                   Eval      *In12 = *On
024600040216     c                   Leavesr
024700040216    1c                   EndIf
024800040216     c                   Z-add     g02dat        V1cDcd
024900040216     c                   Z-add     g02inv        PaDcd
025000040216
025100040216      * Data consegna merce al
025200040216if  1c                   If        V1cDca = *Zeros
025300040216     c                   Eval      V1cDca = V1cDcd
025400040216     c                   Eval      PaDca = PaDcd
025500040216     c                   Leavesr
025600040216    1c                   EndIf
025700040216
025800040216     c                   Clear                   wlbdat
025900040216     c                   Z-add     V1cDca        g02dat
026000040216     c                   Call      'XSRDA8'
026100040216     c                   Parm                    wlbdat
026200040216if  1c                   If        g02err = '1'
026300040216     c                   Eval      V1cMsg = Msg(03)
026400040216     c                   Eval      *In28 = *On
026500040715     c                   Eval      *In13 = *On
026600040216     c                   Leavesr
026700040216    1c                   EndIf
026800040216     c                   Z-add     g02dat        V1cDca
026900040216     c                   Z-add     g02inv        PaDca
027000040216
027100040216      * Range
027200040216if  1c                   If        PaDcd > PaDca
027300040216     c                   Eval      V1cMsg = Msg(04)
027400040216     c                   Eval      *In28 = *On
027500040216     c                   Eval      *In12 = *On
027600040216     c                   Leavesr
027700040216    1c                   EndIf
027800040218
027900040218     c     *Iso          Movel     PaDcd         wdcmd
028000040218     c     *Iso          Movel     PaDca         wdcma
028100040218
028200040218      * Range massimo di 31 gg
028300040218     c     wdcma         Subdur    wdcmd         wgio:*d
028400040218     c                   Eval      wgio = wgio + 1
028500040218if  1c                   If        wgio > 31
028600040218     c                   Eval      V1cMsg = Msg(05)
028700040218     c                   Eval      *In28 = *On
028800040218     c                   Eval      *In12 = *On
028900040218     c                   Leavesr
029000040218    1c                   EndIf
029100040216
029200040311      * Flag solo partenza EEX impostabile solo x LNA = 0
029300040311if  3c                   If        V1CEEX = 'S' and V1CLNA <> *zeros
029400040311     c                   Eval      V1cMsg = Msg(07)
029500040311     c                   Eval      *In28 = *On
029600040311     c                   Eval      *In11 = *On
029700040311     c                   Leavesr
029800040311    3c                   EndIf
029900040310     C                   movel     V1Ceex        PAeex
030000040630
030100040630      * Flag solo partenza DPD impostabile solo x LNA = 0
030200040630if  3c                   If        V1CDPD = 'S' and V1CLNA <> *zeros
030300040630     c                   Eval      V1cMsg = Msg(08)
030400040630     c                   Eval      *In28 = *On
030500040630     c                   Eval      *In11 = *On
030600040630     c                   Leavesr
030700040630    3c                   EndIf
030800040630     c                   Movel     V1cDpd        PaDPD
030900040715
031000060814      * Flags creazione file impostabili solo x LNA = 0
031100060814if  3c                   If        V1CLNA <> *zeros  and
031200060814if  3c                             (V1CFILE = 'S'  or  V1CFILED = 'S')
031300040715     c                   Eval      V1cMsg = Msg(09)
031400040715     c                   Eval      *In28 = *On
031500040715     c                   Eval      *In11 = *On
031600040715     c                   Leavesr
031700040715    3c                   EndIf
031800040715     c                   Movel     V1cFile       PaFILE
031900060814     c                   Movel     V1cFileD      PaFILEd
032000060814
032100040216     c                   EndSr
032200040216
032300040216      *------------------------------------------------------------------------*
032400040216      * ROUTINE INIZIALE
032500040216      *------------------------------------------------------------------------*
032600040216     c     *Inzsr        BegSr
032700040216
032800040216     c     *entry        plist
032900040216     c                   parm                    Kpjba
033000040216
033100040216     c     *dtaara       define    §azute        azuteds
033200040216     c     *dtaara       define    §datiute      ddatiute
033300040216     c                   in(E)     *dtaara
033400040216     c                   If        %error  or RSUT = *blanks
033500040216     c                   CLEAR                   tibs34ds
033600040216     c                   CALL      'TIBS34R'
033700040216     c                   PARM                    tibs34ds
033800040216     c                   in        *dtaara
033900040216     c                   EndIf
034000040216
034100040216      * -> Imposto il p.o. utente
034200040216      * Utente di sede
034300060814if  1c                   IF        DutPou = 46
034400040216     c                   Clear                   V1cLna
034500040216     c                   Clear                   DesLna
034600040216     c                   Eval      *In10 = *Off
034700040630     c                   Eval      *In14 = *On
034800060814    2c                   If        %subst(dutute:1:3) = 'EDP'
034900060814     c                   Eval      *In15 = *On
035000060814   x2c                   Else
035100060814     c                   Eval      *In15 = *Off
035200060814    2c                   EndIf
035300060814      *
035400060814   x1c                   ELSE
035500040216      * Utente di Filiale proteggo il campo
035600040310      * se non è un primo livello
035700040216     c                   Eval      V1cLna = DutPou
035800040216     c                   Eval      DesLna = DutDpo
035900040310    2c                   If        DutLpo = '1'
036000040310     c                   Eval      *In10 = *Off
036100040310   x2c                   Else
036200040310     c                   Eval      *In10 = *On
036300040310    2c                   EndIf
036400040630     c                   Eval      *In14 = *Off
036500060814     c                   Eval      *In15 = *Off
036600060814    1c                   ENDIF
036700040310
036800040310      * Se sono primo livello carico £1
036900040310    2c                   If        DutLpo = '1'
037000040310     c                   Clear                   Trul06ds
037100040310     c                   Eval      D06Cod = '£1'
037200040310     c                   Movel     V1cLna        D06Key
037300040310     c                   Movel(p)  Trul06ds      Kpjbu
037400040310     c                   Call      'TRUL06R'
037500040310     c                   Parm                    Kpjba
037600040310     c                   Movel(p)  Kpjbu         Trul06ds
037700040310    1c                   EndIf
037800120912
037900120912       //?Visto che aggiungiamo in alternativa alla LNA il codice cliente
038000120912       //?sproteggo sempre la LNA nei controlli poi verifico che se
038100120912       //?secondo livello la lna deve essere = a DUTpou
038200120912     c                   eval      *in10 = *off
038300040216
038400040216     c                   EndSr
038500040216** Msg  Lungh. 78                                                            *
038600040216Linea di Arrivo errata                                                        01
038700040216Immettere la data consegna merce                                              02
038800040216Data formalmente errata                                                       03
038900040216Data consegna merce "dal" maggiore di data consegna merce "al"                04
039000040218Non è possibile elaborare un periodo superiore a 31 giorni                    05
039100040310Linea di Arrivo non in gestione al p.o.                                       06
039200040630Se scelte solo LNP Euroexpress è obbligatorio scegliere tutte le LNA          07
039300040630Se scelte solo LNP DPD è obbligatorio scegliere tutte le LNA                  08
039400040715Se scelta la creazione del file è obbligatorio scegliere tutte le LNA         09
039500120913Codice Cliente e Linea di Arrivo sono da immettere in alternativa!!           10
039600120912Linea di Arrivo non in gestione all'utente                                    11
039700120912Codice Cliente errato o non in gestione all'utente                            12
