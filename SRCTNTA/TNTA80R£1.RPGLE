000100041116     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200041116
000300041116      *------------------------------------------------------------------------*
000400041116      *                                                                        *
000500041116      *                 GESTIONE LUOGHI DI DESTINAZIONE    ?                   *
000600041116      *                                                                        *
000700041116      *------------------------------------------------------------------------*
000800041116
000900041116      * ATTENZIONE
001000041116      * CARICO IL SUBFILE SOLO FINO A 9999
001100041116
001200041116      *------------------------------------------------------------------------*
001300041116
001400041116     fAzorg01l  if   e           k disk
001500041116     fCnaco00f  if   e           k disk
001600041117     fFnspe01l  if   e           k disk
001700041117     fFnsp201l  if   e           k disk
001800041116     fTabel00f  if   e           k disk
001900041116     fTnta80D   cf   e             workstn  sfile(ta8002S:nrr)
002000041116
002100041116      *------------------------------------------------------------------------*
002200041116      *  RIEPILOGO INDICATORI
002300041116      *------------------------------------------------------------------------*
002400041122      * 01 - Richiamato
002500041116      * 02 - Utente non abilitato - esce dal pgm
002600041122      * 03 - Abilita F10
002700041116      * 12 - Utente di sede
002800041116      * 20 - GESTIONE SUBFILE
002900041116      * 21 - GESTIONE SUBFILE
003000041116      * 28 - ERRORE GENERICO DSPF
003100041116      * 30 - Comodo
003200041116      * 40 - Errore codice luogo
003300041116      * 41 - Errore codice cliente
003400041116      * 42 - Errore punto operativo
003500041117      * 47 - Errore opzione
003600041117      * 90 - Riemissione videata
003700041116      *------------------------------------------------------------------------*
003800041116
003900041116      *   V A R I A B I L I
004000041116     d codut           s                   Like(TblKut) Inz(1)
004100041119     d dopz            s             20    Inz('235')
004200041122     d dopza           s             20    Inz('5')
004300041116     d kCod            s                   Like(TblCod)
004400041116     d kKey            s                   Like(TblKey)
004500041116     d kSpeFls         s                   Like(SpeFls) Inz('L')
004600041117     d kSp2Tpe         s                   Like(Sp2Tpe) Inz('EM')
004700041116     d nrr             s              4  0
004800041116     d wabi            s                   Like(UteAut)
004900041116     d wdes            s             25
005000041116     d w003a           s              3
005100041117     d w0030           s              3  0
005200041116     d w0040           s              4  0
005300090115     d $okcli          s               n
005400041116
005500041116      *   S C H I E R E
005600041117     d msg             s             78    Dim(10) ctdata perrcd(1)
005700041117     d opz             s              1    Dim(20)
005800041116     d skpog           s              3    Dim(250) inz(*Zeros)
005900041116
006000041116      *   D S   I N T E R N E / E S T E R N E
006100041122     d Param80         ds
006200041122     d  paropz                 1      1
006300041122     d  parcli                 2      8
006400060216     d  partnta60              9      9
006500060216     d  parimta60             10     10
006600041122
006700041116     d                sds
006800041116     d  Vtcpgm                 1     10
006900041116
007000041116     d Azuteds       e ds                  Extname(Azute00f)
007100041116     d dDatiute      e ds
007200041116     d dLat          e ds
007300041116     d ds4l          e ds
007400050415     d dute01        e ds
007500041116     d Kpjba         e ds
007600090115     d og143         e ds
007700041116     d Tibs02ds      e ds
007800041116     d Tibs34ds      e ds
007900041116     d Tnta81ds      e ds
008000041116     d Trul31ds      e ds
008100041116
008200041117      *   C O S T A N T I
008300041116      *  titolo videata (lunghezza massima 34)
008400041117     d VtcTit          C                   CONST('** G E S T I O N E  L U O G H-
008500041117     d                                      I **')
008600041116      *  Descrizione opzioni (lunghezza massima 78)
008700041118     d dopz1           c                   CONST('2=Manutenzione   3=Copia   5=-
008800041118     d                                     Visualizza     ')
008900041122     d dopz1a          c                   CONST('5=Visualizza                 -
009000041116     d                                                    ')
009100041116     d dopz2           c                   CONST(' ')
009200041116
009300041116      *------------------------------------------------------------------------*
009400041116
009500041116      * Emissione 1° Videata
009600041116     c     Inizio        Tag
009700041116
009800041116      * Se richiamato per ricerca vado direttamente al subfile
009900041116     c   01              Goto      Sfl
010000041116
010100041116      * Pulizia 1° Videata
010200041116     c                   If        Not *In28 and Not *In90
010300041116     c                   ExSr      Sr_Pul01d
010400041116     c                   EndIf
010500041116
010600041116     c                   Exfmt     Ta8001d
010700041116
010800041116     c                   Setoff                                       2890
010900041116
011000041116      * Se utente non abilitato esce dal programma
011100041118     c   02
011200041116      * F3=Fine
011300041118     cor kc              Goto      Fine
011400070319
011500070319      * Se richiesta la gestione stampa richiamo il pgm
011600070319     c                   if        *inkh
011700070319     c                   call      'TNTA84R'
011800070319     c                   parm                    kpjba
011900070319     c                   goto      inizio
012000070319     c                   endif
012100041116
012200041116      * Controlli 1° videata
012300041116     c                   ExSr      Sr_Ctr01d
012400041116
012500041116     c   28
012600041116     cor 90              Goto      Inizio
012700041116
012800041116      * Gestione subfile
012900041116     c     Sfl           Tag
013000041116
013100041116      * Imposto opzioni
013200041116     c                   ExSr      Sr_Caropz
013300041116
013400041116     c                   ExSr      Sr_Gessfl
013500041116     c                   Eval      *In28 = *Off
013600041116     c   kc              Goto      Fine
013700041118     c                   If        O81C03 = '1'
013800041118     c                   Goto      Fine
013900041118     c                   EndIf
014000041122     c                   If        Not *In01 and *InKl
014100041122     c                   Goto      Inizio
014200041122     c                   EndIf
014300041122     c                   If        *In01 and *InKl
014400041122     c                   Goto      Fine
014500041122     c                   EndIf
014600041116
014700041116     c     Fine          Tag
014800041116
014900041116     c                   Eval      *InLr = *On
015000041116
015100041116      *------------------------------------------------------------------------*
015200041116      * PULIZIA 1° VIDEATA
015300041116      *------------------------------------------------------------------------*
015400041116     c     Sr_Pul01d     BegSr
015500041116
015600041116     c                   Clear                   V1cCod
015700041116     c                   Clear                   V1dCod
015800041116     c                   Clear                   V1cCli
015900041116     c                   Clear                   V1dCli
016000070412     c                   Eval      V1cFil = 999
016100041116
016200041116     c                   EndSr
016300041116
016400041116      *------------------------------------------------------------------------*
016500041116      * CONTROLLI VIDEO 1
016600041116      *------------------------------------------------------------------------*
016700041116     c     Sr_Ctr01d     BegSr
016800041117
016900041116     c                   Setoff                                       404142
017000041116     c                   Clear                   V1dCod
017100041116     c                   Clear                   V1dCli
017200041116
017300041116      * Codice luogo o codice cliente obbligatori
017400041116     c                   If        V1cCod = *Blanks and V1cCli = *Zeros
017500041116     c                   Eval      *In28 = *On
017600041116     c                   Eval      *In40 = *On
017700041116     c                   Eval      V1cmsg = msg(02)
017800041116     c                   Goto      EndCtr01d
017900041116     c                   EndIf
018000041117
018100041116      * --> Codice Luogo
018200041116if  1c                   If        V1cCod <> *Blanks
018300041116      * ricerca
018400041116     c     '?'           Scan      V1cCod                                 30
018500041116if  2c                   If        *In30
018600041116     c                   Eval      kCod = '4L'
018700041116     c                   Clear                   kKey
018800041116     c                   Clear                   wDes
018900041116     c                   Call      'X§TABER'
019000041116     c                   Parm                    Codut
019100041116     c                   Parm                    kCod
019200041116     c                   Parm                    kKey
019300041116     c                   Parm                    wDes
019400041116     c                   Eval      *In40 = *On
019500041116     c                   Eval      *In90 = *On
019600041116     c                   Eval      V1cCod = kKey
019700041116     c                   Goto      EndCtr01d
019800041116e   2c                   EndIf
019900041116      * controllo
020000041116     c                   Eval      kCod = '4L'
020100041116     c                   Eval      kKey = V1cCod
020200041116     c     kTab          Chain     Tabel00f
020300041116if  2c                   If        Not %Found(Tabel00f) or
020400041116     c                             TblFlg <> *Blanks
020500041116     c                   Eval      *In40 = *On
020600041116     c                   Eval      *In28 = *On
020700041116     c                   Eval      V1cmsg = msg(03)
020800041116     c                   Goto      EndCtr01d
020900041116e   2c                   EndIf
021000041116     c                   Eval      ds4l = TblUni
021100041117if  2c                   If        Not *In12 and §4lufi <> 'S'
021200041116     c                   Eval      *In40 = *On
021300041116     c                   Eval      *In28 = *On
021400041116     c                   Eval      V1cmsg = msg(03)
021500041116     c                   Goto      EndCtr01d
021600041116e   2c                   EndIf
021700041116      * decodifico
021800041116     c                   Eval      V1dCod = §4ldes
021900041116e   1c                   EndIf
022000041117
022100041116      * --> Codice Cliente
022200041116      * no 8888 o 9999
022300041116if  1c                   If        V1cCli <> *Zeros
022400041116     c                   Move      V1cCli        w0040
022500041116if  2c                   If        w0040 = *all'9' or
022600041116     c                             w0040 = *all'8'
022700041116     c                   Eval      *In28 = *On
022800041116     c                   Eval      *In41 = *On
022900041116     c                   Eval      V1cmsg = msg(04)
023000041116     c                   Goto      EndCtr01d
023100041116e   2c                   EndIf
023200090115      * se utente di sede e luogo 101 e cliente logistica (ntw=LOG) tutto ok
023300090115     c                   eval      $okcli = *off
023400090115     c                   if        v1ccod = '101' and *in12
023500090115     c                   movel     v1ccli        w0030
023600090115     c                   clear                   og143
023700090115     c     w0030         chain     azorg01l
023800090115if  2c                   if        %found(azorg01l) and orgfva = *blanks
023900090115     c                   eval      og143 = orgde3
024000090115     c                   endif
024100090115     c                   if        §ogntw = 'LOG'
024200090115     c                   eval      $okcli = *on
024300090115     c                   endif
024400090115     c                   endif
024500041116      * gestibile dall'utente
024600090115     c                   if        not $okcli
024700041116     c                   Movel     V1cCli        w003a
024800041116     c     w003a         Lookup    skpog                                  30
024900041116if  2c                   If        Not *In30
025000041116     c                   Eval      *In41 = *On
025100041116     c                   Eval      *In28 = *On
025200041116     c                   Eval      V1cmsg = msg(05)
025300041116     c                   Goto      EndCtr01d
025400041116e   2c                   EndIf
025500090115     c                   endif
025600041116      * controllo
025700041116     c     kAco          Chain     Cnaco00f
025800041116if  2c                   If        Not %Found(Cnaco00f) or
025900041116     c                             AcoFlg <> *Blanks
026000041116     c                   Eval      *In41 = *On
026100041116     c                   Eval      *In28 = *On
026200041116     c                   Eval      V1cmsg = msg(05)
026300041116     c                   Goto      EndCtr01d
026400041116e   2c                   EndIf
026500041116      * decodifico
026600041116     c                   Eval      V1dCli = Acorag
026700041116e   1c                   EndIf
026800041116
026900041116      * --> Punto Operativo
027000070412     c                   if        V1cFil <> *all'9'
027100090115      * non controllo se cliente Logistica per luogo 101 e utente sede
027200090115     c                             and not $okcli
027300041116      * Controllo
027400041116     c     V1cFil        Chain     Azorg01l
027500041116if  2c                   If        Not %Found(Azorg01l) or
027600041116     c                             OrgFva <> *Blanks
027700041116     c                   Eval      *In42 = *On
027800041116     c                   Eval      *In28 = *On
027900041116     c                   Eval      V1cmsg = msg(06)
028000041116     c                   Goto      EndCtr01d
028100041116e   2c                   EndIf
028200070412     c                   move      v1cfil        w003a
028300070412     c     w003a         Lookup    skpog                                  30
028400070412     c                   if        not *in30
028500070412     c                   eval      *in42 = *on
028600070412     c                   eval      *in28 = *on
028700070412     c                   eval      v1cmsg = msg(09)
028800070412     c                   goto      endctr01d
028900070412     c                   endif
029000041116e   1c                   EndIf
029100041116
029200041116     c     Endctr01d     EndSr
029300041116
029400041116      *------------------------------------------------------------------------*
029500041116      * IMPOSTO OPZIONI
029600041116      *------------------------------------------------------------------------*
029700041116     c     Sr_Caropz     BegSr
029800041116
029900041116     c                   Clear                   opz
030000041116
030100041122      * Se richiamato
030200041116     c                   If        *In01
030300041122     c                   Select
030400041122      * interrogazione
030500041122     c                   When      paropz = '2'
030600041122     c                   Eval      v2opz1 = dopz1
030700041122     c                   Movea     dopz          opz
030800041122      * manutenzione
030900041122     c                   When      paropz = '5'
031000041122     c                   Eval      v2opz1 = dopz1a
031100041122     c                   Movea     dopza         opz
031200041122     c                   EndSl
031300041122
031400041122      * Se sono in gestione
031500041116     c                   Else
031600041116     c                   Eval      v2opz1 = dopz1
031700041116     c                   Movea     dopz          opz
031800041116     c                   EndIf
031900041116
032000041116     c                   Eval      v2opz2 = dopz2
032100041116
032200041116     c                   EndSr
032300041116
032400041116      *------------------------------------------------------------------------*
032500041116      * GESTIONE SUBFILE
032600041116      *------------------------------------------------------------------------*
032700041116     c     Sr_Gessfl     BegSr
032800041116
032900041116     c                   Eval      recsf = 1
033000041116do  1c                   Do        *Hival
033100041116     c                   Eval      *In47 = *Off
033200041116
033300041116      * Pulisco subfile
033400041116     c                   Clear                   nrr
033500041116     c                   Eval      *In21 = *Off
033600041116     c                   Write     Ta8002c
033700041116     c                   Eval      *In20 = *On
033800041116     c                   Eval      *In21 = *On
033900041117
034000041117      * Carico il subfile in base alle scelte fatte
034100041117      * solo 9999 record xchè il file video è definito così
034200041117
034300041116s   2c                   Select
034400041116w   2c                   When      V1cCod = *Blanks
034500041117     c     kSpe          Setll     Fnspe01l
034600041116w   2c                   When      V1cCli = *Zeros
034700041117     c     kSpeFls       Setll     Fnspe01l
034800041116   x2c                   Other
034900041116     c     kSpe01        Setll     Fnspe01l
035000041116e   2c                   EndSl
035100041116
035200041116do  2c                   Do        *Hival
035300041116
035400041116s   3c                   Select
035500041116w   2c                   When      V1cCod = *Blanks
035600041117     c     kSpe          Reade     Fnspe01l
035700041116w   2c                   When      V1cCli = *Zeros
035800041117     c     kSpeFls       Reade     Fnspe01l
035900041116   x2c                   Other
036000041116     c     kSpe01        Reade     Fnspe01l
036100041116e   3c                   Endsl
036200041116
036300041116if  3c                   If        %Eof
036400041116     c                   Leave
036500041116e   3c                   EndIf
036600041116
036700041117if  3c                   If        nrr >= 9998
036800041116     c                   Eval      *In28 = *On
036900041116     c                   Eval      V2cmsg = msg(07)
037000041116     c                   Leave
037100041117e   3c                   EndIf
037200041116
037300041116      * Se selezione solo per codice luogo devo leggere solo i luoghi uguali
037400041116     c                   If        V1cCli = *Zeros  and
037500041116     c                             SpeCod <> V1cCod
037600041116     c                   Iter
037700041116     c                   EndIf
037800041117
037900041117     c                   Movel     SpeCli        w003a
038000041117     c                   Movel     SpeCli        w0030
038100090116      * se utente di sede e luogo 101 e cliente logistica (ntw=LOG) tutto ok
038200090116     c                   eval      $okcli = *off
038300090116     c                   if        v1ccod = '101' and *in12
038400090116     c                   clear                   og143
038500090116     c     w0030         chain     azorg01l
038600090116if  2c                   if        %found(azorg01l) and orgfva = *blanks
038700090116     c                   eval      og143 = orgde3
038800090116     c                   endif
038900090116     c                   if        §ogntw = 'LOG'
039000090116     c                   eval      $okcli = *on
039100090116     c                   endif
039200090116     c                   endif
039300041117
039400041117      * Controllo se devo emettere solo i clienti di un p.o. o dell'area
039500041117s   3c                   Select
039600090116w   3c                   When      V1cFil = 999 and not $okcli
039700041117     c     w003a         Lookup    skpog                                  30
039800041117     c                   If        Not *In30
039900041117     c                   Iter
040000041117     c                   EndIf
040100070412     c                   when      w0030 <> V1cFil  and
040200041118     c                             V1cCod <> *Blanks
040300090116     c                             and not $okcli
040400041117     c                   Iter
040500041117e   3c                   EndSl
040600041117
040700041117     c                   ExSr      Sr_Carsfl
0408000411173   2c                   EndDo
040900041117
041000041117     c     emettosf      Tag
041100041117
041200041117      * Controllo se recsf è maggiore di nrr
041300041117     c                   If        recsf > nrr
041400041117     c                   Eval      recsf = 1
041500041117     c                   EndIf
041600041117
041700041117      * Se non ho caricato emetto una videata di segnalazione.
041800041118     c                   If        nrr = *Zeros
041900041118     c                   Exfmt     Ta8004d
042000041117     c                   EndIf
042100041117
042200041117      * Se ho caricato emetto il subfile
042300041117     c                   If        nrr > *Zeros
042400041117     c                   Write     Ta8003z
042500041117     c                   Exfmt     Ta8002c
042600041117     c                   EndIf
042700041117
042800041117      * F3=Fine
042900041117     c   kc              Leave
043000041117
043100041117      * F12=Ritorno
043200041117     c   kl              Leave
043300041117
043400041117      * F10=Immissione
043500041117     c                   If        *InKj
043600041117     c                   ExSr      Sr_Modifica
043700041118     c                   Eval      recsf = 1
043800041118     c                   If        O81C03 = '1'
043900041118     c                   Goto      Fine
044000041118     c                   EndIf
044100041118     c                   Iter
044200041117     c                   EndIf
044300041117
044400041117      * Leggo le opzioni immesse dall'utente ?
044500041117
044600041117if  2c                   If        nrr > *Zeros
044700041117
044800041117     c                   Eval      nrr = *Zeros
044900041117do  3c                   Do        *Hival
045000041117     c                   Eval      nrr = nrr +1
045100041117     c     nrr           Chain     Ta8002s                            30
045200041117     c                   If        *In30
045300041117     c                   Leave
045400041117     c                   EndIf
045500041117
045600041117     c                   Eval      *In28 = *Off
045700041117     c                   Eval      *In47 = *Off
045800041117
045900041117      * Controllo se scelta valida
046000041117     c     V2copz        Lookup    opz                                    30
046100041117if  4c                   If        Not *In30
046200041117     c                   Eval      *In28 = *On
046300041117     c                   Eval      *In47 = *On
046400041117     c                   Eval      V2cMsg = msg(08)
046500041117     c                   Eval      recsf = nrr
046600041117     c                   Update    Ta8002s
046700041117     c                   Goto      Emettosf
046800041117e   4c                   EndIf
046900041117
047000041117      * Visualizzazione
047100041117if  4C                   If        V2copz = '5'
047200041117     c                   Eval      recsf = nrr
047300041117     c                   ExSr      Sr_Modifica
047400041117e   4c                   EndIf
047500041117
047600041117      * Manutenzione
047700041117if  4c                   If        V2copz = '2'
047800041117     c                   Eval      recsf = nrr
047900050617      * controllo se posso manutenzionare il luogo
048000050617     c                   Eval      kCod = '4L'
048100050617     c                   Eval      kKey = v2ccod
048200050617     c     kTab          Chain     Tabel00f
048300050617if  2c                   If        Not %Found(Tabel00f) or
048400050617     c                             TblFlg <> *Blanks
048500050617     c                   Eval      *In47 = *On
048600050617     c                   Eval      *In28 = *On
048700050617     c                   Eval      V2cmsg = msg(03)
048800050617     c                   Update    Ta8002s
048900050617     c                   Goto      Emettosf
049000050617    2c                   EndIf
049100050617     c                   Eval      ds4l = TblUni
049200050617if  2c                   If        Not *In12 and §4lufi <> 'S'
049300050617     c                   Eval      *In47 = *On
049400050617     c                   Eval      *In28 = *On
049500050617     c                   Eval      V2cmsg = msg(03)
049600050617     c                   Update    Ta8002s
049700050617     c                   Goto      Emettosf
049800050617e   2c                   EndIf
049900070411      * controllo se posso manutenzionare il cliente
050000070411     c                   movel     v2ccli        w003a
050100070411     c     w003a         lookup    skpog                                  30
050200070411     c                   if        not *in30
050300070411     c                   eval      *in47 = *on
050400070411     c                   eval      *in28 = *on
050500070411     c                   eval      v2cmsg = msg(09)
050600070411     c                   update    ta8002s
050700070411     c                   goto      emettosf
050800070411     c                   endif
050900041117     c                   ExSr      Sr_Modifica
051000041117e   4c                   EndIf
051100041119
051200041119      * Copia
051300041119if  4c                   If        V2copz = '3'
051400041119     c                   Eval      recsf = nrr
051500050617      * controllo se posso manutenzionare il luogo
051600050617     c                   Eval      kCod = '4L'
051700050617     c                   Eval      kKey = v2ccod
051800050617     c     kTab          Chain     Tabel00f
051900050617if  2c                   If        Not %Found(Tabel00f) or
052000050617     c                             TblFlg <> *Blanks
052100050617     c                   Eval      *In47 = *On
052200050617     c                   Eval      *In28 = *On
052300050617     c                   Eval      V2cmsg = msg(03)
052400050617     c                   Update    Ta8002s
052500050617     c                   Goto      Emettosf
052600050617e   2c                   EndIf
052700050617     c                   Eval      ds4l = TblUni
052800050617if  2c                   If        Not *In12 and §4lufi <> 'S'
052900050617     c                   Eval      *In47 = *On
053000050617     c                   Eval      *In28 = *On
053100050617     c                   Eval      V2cmsg = msg(03)
053200050617     c                   Update    Ta8002s
053300050617     c                   Goto      Emettosf
053400050617e   2c                   EndIf
053500041119     c                   ExSr      Sr_Modifica
053600041119e   4c                   EndIf
053700041117
053800041117      * Ripulisco il campo opzione  (qui non devo segnalare errori)
053900041117      *   a meno che non sia stata una selezione
054000041117     c                   Clear                   V2copz
054100041117     c                   Update    Ta8002s
054200041117
054300041117e   3c                   EndDo
054400041117
054500041117e   2c                   EndIf
054600041117
054700041117e   1c                   EndDo
054800041117
054900041117     c                   EndSr
055000041117
055100041117      *------------------------------------------------------------------------*
055200041117      * CARICO SUBFILE
055300041117      *------------------------------------------------------------------------*
055400041117     c     Sr_Carsfl     BegSr
055500041117
055600041117     c                   Clear                   V2copz
055700041117     c                   Eval      V2cCod = SpeCod
055800041117     c                   Eval      V2cRag = SpeRag
055900041117     c                   Eval      V2cCli = SpeCli
056000041117     c                   Eval      V2cInd = SpeInd
056100041118     c                   Eval      V2cFax = SpeFax
056200041117     c                   Eval      V2cCap = SpeCap
056300041117     c                   Eval      V2cLoc = SpeLoc
056400041122     c                   If        SpeNaz = *Blanks
056500041117     c                   Eval      V2cPro = SpePro
056600041122     c                   Else
056700041122     c                   Eval      V2cPro = SpeNaz
056800041122     c                   EndIf
056900041117
057000041117      * Decodifico Luogo
057100041117     c                   Clear                   V2dCod
057200041117     c                   Eval      kCod = '4L'
057300041118     c                   Eval      kKey = V2cCod
057400041117     c     kTab          Chain     Tabel00f
057500041117if  2c                   If        %Found(Tabel00f)
057600041117     c                   Eval      ds4l = TblUni
057700041117e   2c                   EndIf
057800041117     c                   Eval      V2dCod = §4ldes
057900041117
058000041117      * Recupero il campo e-mail
058100041117     c                   Clear                   V2cMail
058200041117     c     kSp2          Chain     Fnsp201l
058300041117if  2c                   If        %Found(Fnsp201l)
058400041117     c                   Eval      V2cMail = Sp2Est
058500041117e   2c                   EndIf
058600041117
058700041117     c                   Add       1             nrr
058800041117     c                   Write     Ta8002s
058900041117
059000041117     c                   EndSr
059100041117
059200041117      *------------------------------------------------------------------------*
059300041117      * RICHIAMO IL PROGRAMMA DI MODIFICA
059400041117      *------------------------------------------------------------------------*
059500041117     c     Sr_Modifica   BegSr
059600041117
059700041117     c                   Clear                   Tnta81ds
059800041118     c                   If        V2cOpz <> *Blanks
059900041117     c                   Eval      I81Opz = V2copz
060000041118     c                   Eval      I81Cod = V2cCod
060100041117     c                   Eval      I81Cli = V2cCli
060200041118     c                   Else
060300041118     c                   If        V1cCli = *Zeros
060400041118     c                   Eval      I81Cod = V1cCod
060500041118     c                   EndIf
060600041118     c                   Eval      I81Cli = V1cCli
060700041118     c                   EndIf
060800060216     c                   eval      i81gcli = partnta60
060900060216     c                   eval      i81icli = parimta60
061000041117     c                   Call      'TNTA81R'
061100041117     c                   Parm                    Kpjba
061200041117     c                   Parm                    Tnta81ds
061300041221
061400041221     c                   If        O81C03 = '1'
061500041221     c                   Goto      Fine
061600041221     c                   EndIf
061700041117
061800041117     c                   EndSr
061900041117
062000041116      *------------------------------------------------------------------------*
062100041117      * ROUTINE INIZIALE
062200041116      *------------------------------------------------------------------------*
062300041116     c     *Inzsr        BegSr
062400041116
062500041116     c     *Entry        Plist
062600041116     c                   Parm                    Kpjba
062700041122     c                   Eval      Param80 = Kpjbu
062800041116
062900041122     c                   If        Paropz <> *Blanks
063000041116     c                   Eval      *In01 = *On
063100041116     c                   EndIf
063200041117
063300041116     c     *dtaara       define    §azute        azuteds
063400041116     c     *dtaara       define    §datiute      ddatiute
063500041116     c                   in(E)     *dtaara
063600041116     c                   If        %error  or RSUT = *blanks
063700041116     c                   Clear                   Tibs34ds
063800041116     c                   Call      'TIBS34R'
063900041116     c                   Parm                    Tibs34ds
064000041116     c                   In        *dtaara
064100041116     c                   EndIf
064200041116
064300041116     c                   Clear                   wabi
064400041116     c                   Clear                   dLat
064500041116
064600041116      * Verifica errori e autorità profilo
064700041116s   1c                   Select
064800041116      * se ho errori nei dati utente esco dal pgm
064900041116w   1c                   When      DutErr = 'E'
065000041116     c                   GoTo      Fine
065100041116      * se non c'è l'abilitazione
065200041116      * --> se 1° livello, abilitazioni al terminal
065300041116      *     se 2° livello, abilitazioni al punto operativo
065400041116      *     se sede è impossibile ma se capita mando a fine il pgm
065500041116w   1c                   When      UteAut = *Blanks
065600041116if  2c                   If        DutLpo = '1'
065700041116     c                   Eval      wabi   = 'TP'
065800041116e   2c                   EndIf
065900041116if  2c                   If        DutLpo = '2'
066000041116     c                   Eval      wabi   = 'PO'
066100041116e   2c                   EndIf
066200041116if  2c                   If        DutLpo = 'S'
066300041116     c                   GoTo      Fine
066400041116e   2c                   EndIf
066500041116      * carica le abilitazioni del profilo
066600041116x   1c                   Other
066700050415     c                   Movel     UteFaf        Dute01
066800050415     c                   If        §UteCli <> *Blanks
066900050415     c                   Eval      wabi = §UteCli
067000050415     c                   Else
067100041116     c                   Eval      wabi = UteAut
067200050415     c                   EndIf
067300041116e   1c                   EndSl
067400041116
067500041116      * controllo se ok l'abilitazione dell'utente
067600041116     c                   Clear                   Tibs02ds
067700041116     c                   Eval      T02Mod = 'C'
067800041116     c                   Eval      T02Sif = knsif
067900041116     c                   Eval      T02Cod = 'LAT'
068000041116     c                   Movel(p)  wabi          T02Ke1
068100041116     c                   Call      'TIBS02R'
068200041116     c                   Parm                    kpjba
068300041116     c                   Parm                    Tibs02ds
068400041116if  1c                   If        T02Err = *Blanks
068500041116     c                   Eval      dLat = T02Uni
068600041116e   1c                   EndIf
068700041116      * errore o non abilitato
068800041116if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
068900041116     c                   Eval      *In02 = *On
069000041116     c                   Eval      *In28 = *On
069100041116     c                   Eval      V1cmsg = msg(01)
069200041117     c                   GoTo      Inizio
069300041116e   1c                   EndIf
069400041116
069500041116      * Reperimento dei P.O. gestibili dall'utente
069600041116     c                   Clear                   Trul31ds
069700041116     c                   Eval      I31abi = wabi
069800041116     c                   Eval      I31cdi = DUTdis
069900041116     c                   Eval      I31car = DUTare
070000041116     c                   Eval      I31cpo = DUTpou
070100041116     c                   Call      'TRUL31R'
070200041116     c                   Parm                    KPJBA
070300041116     c                   Parm                    Trul31ds
070400041116if  1c                   If        O31pog > *zeros
070500041116     c                   Movea     O31pog        skpog
070600041116x   1c                   Else
070700041116     c                   Eval      *In02 = *On
070800041116     c                   Eval      *In28 = *On
070900041116     c                   Eval      V1cmsg = msg(01)
071000041117     c                   GoTo      Inizio
071100041116e   1c                   EndIf
071200041117
071300041116      * Controllo se sono in sede
071400041116     c                   If        Simfel = *Zeros
071500041116     c                   Eval      *In12 = *On
071600041116     c                   EndIf
071700041122
071800041122      * se richiamato imposto già i campi
071900041122     c                   If        *In01
072000041122     c                   Move      Parcli        V1cCli
072100070412     c                   Eval      V1cFil = 999
072200041122     c                   EndIf
072300041122
072400041122     c                   Eval      *In03 = *On
072500041122     c                   If        *In01 and paropz = '5'
072600041122     c                   Eval      *In03 = *Off
072700041122     c                   EndIf
072800041116
072900041116      * KLIST
073000041116     c     kTab          Klist
073100041116     c                   Kfld                    Codut
073200041116     c                   Kfld                    kCod
073300041116     c                   Kfld                    kKey
073400041116
073500041116     c     kAco          Klist
073600041116     c                   Kfld                    Codut
073700041116     c                   Kfld                    DutKci
073800041116     c                   Kfld                    V1cCli
073900041116
074000041116     c     kSpe          Klist
074100041116     c                   Kfld                    kSpeFls
074200041116     c                   Kfld                    V1cCli
074300041116
074400041116     c     kSpe01        Klist
074500041116     c                   Kfld                    kSpeFls
074600041116     c                   Kfld                    V1cCli
074700041116     c                   Kfld                    V1cCod
074800041117
074900041117     c     kSp2          Klist
075000041117     c                   Kfld                    SpeCli
075100041117     c                   Kfld                    SpeCod
075200041117     c                   Kfld                    kSp2Tpe
075300041117
075400041116     c                   EndSr
075500041116      *------------------------------------------------------------------------*
075600041116
075700041117** MSG  Lungh. 78                                                            *
075800041116Utente non abilitato alla Gestione Luoghi di destinazione                     01
075900041116Immettere il Luogo o il Cliente                                               02
076000041116Codice Luogo Errato o non utilizzabile                                        03
076100041116Cliente non gestibile con 9999 o 8888                                         04
076200041116Cliente Inesistente o non gestibile                                           05
076300070412Immettere una filiale valida                                                  06
076400041116Ampiezza massima subfile superata                                             07
076500041117Opzione immessa non valida                                                    08
076600070411Cliente non gestibile è possibile solo visualizzare                           09
076700070412Filiale non in gestione all'utente                                            09
