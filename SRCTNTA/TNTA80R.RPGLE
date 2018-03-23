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
004800041116     d wdes            s             25
004900041116     d w003a           s              3
005000041117     d w0030           s              3  0
005100041116     d w0040           s              4  0
005200090115     d $okcli          s               n
005300041116
005400041116      *   S C H I E R E
005500041117     d msg             s             78    Dim(10) ctdata perrcd(1)
005600041117     d opz             s              1    Dim(20)
005700041116
005800041116      *   D S   I N T E R N E / E S T E R N E
005900041122     d Param80         ds
006000041122     d  paropz                 1      1
006100041122     d  parcli                 2      8
006200060216     d  partnta60              9      9
006300060216     d  parimta60             10     10
006400041122
006500041116     d                sds
006600041116     d  Vtcpgm                 1     10
006700041116
006800041116     d Azuteds       e ds                  Extname(Azute00f)
006900041116     d dDatiute      e ds
007000041116     d ds4l          e ds
007100041116     d Kpjba         e ds
007200090115     d og143         e ds
007300041116     d Tibs02ds      e ds
007400041116     d Tibs34ds      e ds
007500041116     d Tnta81ds      e ds
007600090709     d tntaa1ds      e ds
007700041116
007800041117      *   C O S T A N T I
007900041116      *  titolo videata (lunghezza massima 34)
008000041117     d VtcTit          C                   CONST('** G E S T I O N E  L U O G H-
008100041117     d                                      I **')
008200041116      *  Descrizione opzioni (lunghezza massima 78)
008300041118     d dopz1           c                   CONST('2=Manutenzione   3=Copia   5=-
008400041118     d                                     Visualizza     ')
008500041122     d dopz1a          c                   CONST('5=Visualizza                 -
008600041116     d                                                    ')
008700041116     d dopz2           c                   CONST(' ')
008800041116
008900041116      *------------------------------------------------------------------------*
009000041116
009100041116      * Emissione 1° Videata
009200041116     c     Inizio        Tag
009300041116
009400041116      * Se richiamato per ricerca vado direttamente al subfile
009500041116     c   01              Goto      Sfl
009600041116
009700041116      * Pulizia 1° Videata
009800041116     c                   If        Not *In28 and Not *In90
009900041116     c                   ExSr      Sr_Pul01d
010000041116     c                   EndIf
010100041116
010200041116     c                   Exfmt     Ta8001d
010300041116
010400041116     c                   Setoff                                       2890
010500041116
010600041116      * Se utente non abilitato esce dal programma
010700041118     c   02
010800041116      * F3=Fine
010900041118     cor kc              Goto      Fine
011000070319
011100070319      * Se richiesta la gestione stampa richiamo il pgm
011200070319     c                   if        *inkh
011300070319     c                   call      'TNTA84R'
011400070319     c                   parm                    kpjba
011500070319     c                   goto      inizio
011600070319     c                   endif
011700041116
011800041116      * Controlli 1° videata
011900041116     c                   ExSr      Sr_Ctr01d
012000041116
012100041116     c   28
012200041116     cor 90              Goto      Inizio
012300041116
012400041116      * Gestione subfile
012500041116     c     Sfl           Tag
012600041116
012700041116      * Imposto opzioni
012800041116     c                   ExSr      Sr_Caropz
012900041116
013000041116     c                   ExSr      Sr_Gessfl
013100041116     c                   Eval      *In28 = *Off
013200041116     c   kc              Goto      Fine
013300041118     c                   If        O81C03 = '1'
013400041118     c                   Goto      Fine
013500041118     c                   EndIf
013600041122     c                   If        Not *In01 and *InKl
013700041122     c                   Goto      Inizio
013800041122     c                   EndIf
013900041122     c                   If        *In01 and *InKl
014000041122     c                   Goto      Fine
014100041122     c                   EndIf
014200041116
014300041116     c     Fine          Tag
014400090710     C*
014500090710     C                   clear                   tntaa1ds
014600090710     C                   movel     'C'           Itaa1Tla
014700090710     C                   movel(p)  tntaa1ds      kpjbu
014800090710     C                   CALL      'TNTAA1C'
014900090710     C                   PARM                    KPJBA
015000090710     c
015100041116     c                   Eval      *InLr = *On
015200041116
015300041116      *------------------------------------------------------------------------*
015400041116      * PULIZIA 1° VIDEATA
015500041116      *------------------------------------------------------------------------*
015600041116     c     Sr_Pul01d     BegSr
015700041116
015800041116     c                   Clear                   V1cCod
015900041116     c                   Clear                   V1dCod
016000041116     c                   Clear                   V1cCli
016100041116     c                   Clear                   V1dCli
016200070412     c                   Eval      V1cFil = 999
016300041116
016400041116     c                   EndSr
016500041116
016600041116      *------------------------------------------------------------------------*
016700041116      * CONTROLLI VIDEO 1
016800041116      *------------------------------------------------------------------------*
016900041116     c     Sr_Ctr01d     BegSr
017000041117
017100041116     c                   Setoff                                       404142
017200041116     c                   Clear                   V1dCod
017300041116     c                   Clear                   V1dCli
017400041116
017500041116      * Codice luogo o codice cliente obbligatori
017600041116     c                   If        V1cCod = *Blanks and V1cCli = *Zeros
017700041116     c                   Eval      *In28 = *On
017800041116     c                   Eval      *In40 = *On
017900041116     c                   Eval      V1cmsg = msg(02)
018000041116     c                   Goto      EndCtr01d
018100041116     c                   EndIf
018200041117
018300041116      * --> Codice Luogo
018400041116if  1c                   If        V1cCod <> *Blanks
018500041116      * ricerca
018600041116     c     '?'           Scan      V1cCod                                 30
018700041116if  2c                   If        *In30
018800041116     c                   Eval      kCod = '4L'
018900041116     c                   Clear                   kKey
019000041116     c                   Clear                   wDes
019100041116     c                   Call      'X§TABER'
019200041116     c                   Parm                    Codut
019300041116     c                   Parm                    kCod
019400041116     c                   Parm                    kKey
019500041116     c                   Parm                    wDes
019600041116     c                   Eval      *In40 = *On
019700041116     c                   Eval      *In90 = *On
019800041116     c                   Eval      V1cCod = kKey
019900041116     c                   Goto      EndCtr01d
020000041116e   2c                   EndIf
020100041116      * controllo
020200041116     c                   Eval      kCod = '4L'
020300041116     c                   Eval      kKey = V1cCod
020400041116     c     kTab          Chain     Tabel00f
020500041116if  2c                   If        Not %Found(Tabel00f) or
020600041116     c                             TblFlg <> *Blanks
020700041116     c                   Eval      *In40 = *On
020800041116     c                   Eval      *In28 = *On
020900041116     c                   Eval      V1cmsg = msg(03)
021000041116     c                   Goto      EndCtr01d
021100041116e   2c                   EndIf
021200041116     c                   Eval      ds4l = TblUni
021300041117if  2c                   If        Not *In12 and §4lufi <> 'S'
021400041116     c                   Eval      *In40 = *On
021500041116     c                   Eval      *In28 = *On
021600041116     c                   Eval      V1cmsg = msg(03)
021700041116     c                   Goto      EndCtr01d
021800041116e   2c                   EndIf
021900041116      * decodifico
022000041116     c                   Eval      V1dCod = §4ldes
022100041116e   1c                   EndIf
022200041117
022300041116      * --> Codice Cliente
022400041116      * no 8888 o 9999
022500041116if  1c                   If        V1cCli <> *Zeros
022600041116     c                   Move      V1cCli        w0040
022700041116if  2c                   If        w0040 = *all'9' or
022800041116     c                             w0040 = *all'8'
022900041116     c                   Eval      *In28 = *On
023000041116     c                   Eval      *In41 = *On
023100041116     c                   Eval      V1cmsg = msg(04)
023200041116     c                   Goto      EndCtr01d
023300041116e   2c                   EndIf
023400090115      * se utente di sede e luogo 101 e cliente logistica (ntw=LOG) tutto ok
023500090115     c                   eval      $okcli = *off
023600090115     c                   if        v1ccod = '101' and *in12
023700090115     c                   movel     v1ccli        w0030
023800090115     c                   clear                   og143
023900090115     c     w0030         chain     azorg01l
024000090115if  2c                   if        %found(azorg01l) and orgfva = *blanks
024100090115     c                   eval      og143 = orgde3
024200090115     c                   endif
024300090115     c                   if        §ogntw = 'LOG'
024400090115     c                   eval      $okcli = *on
024500090115     c                   endif
024600090115     c                   endif
024700041116      * gestibile dall'utente
024800090115     c                   if        not $okcli
024900090710     c                   clear                   tntaa1ds
025000090710     c                   eval      itaa1ksc =v1ccli
025100090710     c                   eval      itaa1caut='§UTEcli'
025200090710     c                   clear                   kpjbu
025300090710     c                   movel     tntaa1ds      kpjbu
025400090710     c                   call      'TNTAA1C'
025500090710     c                   parm                    kpjba
025600090710     c                   movel     kpjbu         tntaa1ds
025700090710     c
025800090710     c                   if        otaa1fabi='N'
025900090710     c
026000090710     c**                 Movel     V1cCli        w003a
026100090710     c**   w003a         Lookup    skpog                                  30
026200090710if  2c**                 If        Not *In30
026300090710
026400041116     c                   Eval      *In41 = *On
026500041116     c                   Eval      *In28 = *On
026600041116     c                   Eval      V1cmsg = msg(05)
026700041116     c                   Goto      EndCtr01d
026800041116e   2c                   EndIf
026900090115     c                   endif
027000041116      * controllo
027100041116     c     kAco          Chain     Cnaco00f
027200041116if  2c                   If        Not %Found(Cnaco00f) or
027300041116     c                             AcoFlg <> *Blanks
027400041116     c                   Eval      *In41 = *On
027500041116     c                   Eval      *In28 = *On
027600041116     c                   Eval      V1cmsg = msg(05)
027700041116     c                   Goto      EndCtr01d
027800041116e   2c                   EndIf
027900041116      * decodifico
028000041116     c                   Eval      V1dCli = Acorag
028100041116e   1c                   EndIf
028200041116
028300041116      * --> Punto Operativo
028400070412     c                   if        V1cFil <> *all'9'
028500090115      * non controllo se cliente Logistica per luogo 101 e utente sede
028600090115     c                             and not $okcli
028700041116      * Controllo
028800041116     c     V1cFil        Chain     Azorg01l
028900041116if  2c                   If        Not %Found(Azorg01l) or
029000041116     c                             OrgFva <> *Blanks
029100041116     c                   Eval      *In42 = *On
029200041116     c                   Eval      *In28 = *On
029300041116     c                   Eval      V1cmsg = msg(06)
029400041116     c                   Goto      EndCtr01d
029500041116e   2c                   EndIf
029600090710
029700090710     c* Filiale gestibile dall'utente
029800090710     c                   clear                   tntaa1ds
029900090710     c                   eval      itaa1fil =v1cfil
030000090710     c                   eval      itaa1caut='§UTEcli'
030100090710     c                   clear                   kpjbu
030200090710     c                   movel     tntaa1ds      kpjbu
030300090710     c                   call      'TNTAA1C'
030400090710     c                   parm                    kpjba
030500090710     c                   movel     kpjbu         tntaa1ds
030600090710     c
030700090710     c                   if        otaa1fabi='N'
030800090710     c
030900090710     c**                 move      v1cfil        w003a
031000090710     c**   w003a         Lookup    skpog                                  30
031100090710     c**                 if        not *in30
031200090710     c
031300070412     c                   eval      *in42 = *on
031400070412     c                   eval      *in28 = *on
031500070412     c                   eval      v1cmsg = msg(09)
031600070412     c                   goto      endctr01d
031700070412     c                   endif
031800090710e   1c                   EndIf
031900041116
032000041116     c     Endctr01d     EndSr
032100041116
032200041116      *------------------------------------------------------------------------*
032300041116      * IMPOSTO OPZIONI
032400041116      *------------------------------------------------------------------------*
032500041116     c     Sr_Caropz     BegSr
032600041116
032700041116     c                   Clear                   opz
032800041116
032900041122      * Se richiamato
033000041116     c                   If        *In01
033100041122     c                   Select
033200041122      * interrogazione
033300041122     c                   When      paropz = '2'
033400041122     c                   Eval      v2opz1 = dopz1
033500041122     c                   Movea     dopz          opz
033600041122      * manutenzione
033700041122     c                   When      paropz = '5'
033800041122     c                   Eval      v2opz1 = dopz1a
033900041122     c                   Movea     dopza         opz
034000041122     c                   EndSl
034100041122
034200041122      * Se sono in gestione
034300041116     c                   Else
034400041116     c                   Eval      v2opz1 = dopz1
034500041116     c                   Movea     dopz          opz
034600041116     c                   EndIf
034700041116
034800041116     c                   Eval      v2opz2 = dopz2
034900041116
035000041116     c                   EndSr
035100041116
035200041116      *------------------------------------------------------------------------*
035300041116      * GESTIONE SUBFILE
035400041116      *------------------------------------------------------------------------*
035500041116     c     Sr_Gessfl     BegSr
035600041116
035700041116     c                   Eval      recsf = 1
035800041116do  1c                   Do        *Hival
035900041116     c                   Eval      *In47 = *Off
036000041116
036100041116      * Pulisco subfile
036200041116     c                   Clear                   nrr
036300041116     c                   Eval      *In21 = *Off
036400041116     c                   Write     Ta8002c
036500041116     c                   Eval      *In20 = *On
036600041116     c                   Eval      *In21 = *On
036700041117
036800041117      * Carico il subfile in base alle scelte fatte
036900041117      * solo 9999 record xchè il file video è definito così
037000041117
037100041116s   2c                   Select
037200041116w   2c                   When      V1cCod = *Blanks
037300041117     c     kSpe          Setll     Fnspe01l
037400041116w   2c                   When      V1cCli = *Zeros
037500041117     c     kSpeFls       Setll     Fnspe01l
037600041116   x2c                   Other
037700041116     c     kSpe01        Setll     Fnspe01l
037800041116e   2c                   EndSl
037900041116
038000041116do  2c                   Do        *Hival
038100041116
038200041116s   3c                   Select
038300041116w   2c                   When      V1cCod = *Blanks
038400041117     c     kSpe          Reade     Fnspe01l
038500041116w   2c                   When      V1cCli = *Zeros
038600041117     c     kSpeFls       Reade     Fnspe01l
038700041116   x2c                   Other
038800041116     c     kSpe01        Reade     Fnspe01l
038900041116e   3c                   Endsl
039000041116
039100041116if  3c                   If        %Eof
039200041116     c                   Leave
039300041116e   3c                   EndIf
039400041116
039500041117if  3c                   If        nrr >= 9998
039600041116     c                   Eval      *In28 = *On
039700041116     c                   Eval      V2cmsg = msg(07)
039800041116     c                   Leave
039900041117e   3c                   EndIf
040000041116
040100041116      * Se selezione solo per codice luogo devo leggere solo i luoghi uguali
040200041116     c                   If        V1cCli = *Zeros  and
040300041116     c                             SpeCod <> V1cCod
040400041116     c                   Iter
040500041116     c                   EndIf
040600041117
040700041117     c                   Movel     SpeCli        w003a
040800041117     c                   Movel     SpeCli        w0030
040900090116      * se utente di sede e luogo 101 e cliente logistica (ntw=LOG) tutto ok
041000090116     c                   eval      $okcli = *off
041100090116     c                   if        v1ccod = '101' and *in12
041200090116     c                   clear                   og143
041300090116     c     w0030         chain     azorg01l
041400090116if  2c                   if        %found(azorg01l) and orgfva = *blanks
041500090116     c                   eval      og143 = orgde3
041600090116     c                   endif
041700090116     c                   if        §ogntw = 'LOG'
041800090116     c                   eval      $okcli = *on
041900090116     c                   endif
042000090116     c                   endif
042100041117
042200041117      * Controllo se devo emettere solo i clienti di un p.o. o dell'area
042300090710      *  se emetto tutto, non controllo agente unificante ma solofil del
042400090710      *  codice cliente
042500041117s   3c                   Select
042600090116w   3c                   When      V1cFil = 999 and not $okcli
042700090713     c                             and v1ccli=0
042800090710     c                   clear                   tntaa1ds
042900090710     c                   eval      itaa1fil =w0030
043000090710     c                   eval      itaa1caut='§UTEcli'
043100090710     c                   clear                   kpjbu
043200090710     c                   movel     tntaa1ds      kpjbu
043300090710     c                   call      'TNTAA1C'
043400090710     c                   parm                    kpjba
043500090710     c                   movel     kpjbu         tntaa1ds
043600090710     c
043700090710     c                   if        otaa1fabi='N'
043800090710     c
043900090710     c**   w003a         Lookup    skpog                                  30
044000090710     c**                 If        Not *In30
044100041117     c                   Iter
044200041117     c                   EndIf
044300070412     c                   when      w0030 <> V1cFil  and
044400090713     c                             V1cCod <> *Blanks  and v1cfil<>999
044500090116     c                             and not $okcli
044600041117     c                   Iter
044700041117e   3c                   EndSl
044800041117
044900041117     c                   ExSr      Sr_Carsfl
0450000411173   2c                   EndDo
045100041117
045200041117     c     emettosf      Tag
045300041117
045400041117      * Controllo se recsf è maggiore di nrr
045500041117     c                   If        recsf > nrr
045600041117     c                   Eval      recsf = 1
045700041117     c                   EndIf
045800041117
045900041117      * Se non ho caricato emetto una videata di segnalazione.
046000041118     c                   If        nrr = *Zeros
046100041118     c                   Exfmt     Ta8004d
046200041117     c                   EndIf
046300041117
046400041117      * Se ho caricato emetto il subfile
046500041117     c                   If        nrr > *Zeros
046600041117     c                   Write     Ta8003z
046700041117     c                   Exfmt     Ta8002c
046800041117     c                   EndIf
046900041117
047000041117      * F3=Fine
047100041117     c   kc              Leave
047200041117
047300041117      * F12=Ritorno
047400041117     c   kl              Leave
047500041117
047600041117      * F10=Immissione
047700041117     c                   If        *InKj
047800041117     c                   ExSr      Sr_Modifica
047900041118     c                   Eval      recsf = 1
048000041118     c                   If        O81C03 = '1'
048100041118     c                   Goto      Fine
048200041118     c                   EndIf
048300041118     c                   Iter
048400041117     c                   EndIf
048500041117
048600041117      * Leggo le opzioni immesse dall'utente ?
048700041117
048800041117if  2c                   If        nrr > *Zeros
048900041117
049000041117     c                   Eval      nrr = *Zeros
049100041117do  3c                   Do        *Hival
049200041117     c                   Eval      nrr = nrr +1
049300041117     c     nrr           Chain     Ta8002s                            30
049400041117     c                   If        *In30
049500041117     c                   Leave
049600041117     c                   EndIf
049700041117
049800041117     c                   Eval      *In28 = *Off
049900041117     c                   Eval      *In47 = *Off
050000041117
050100041117      * Controllo se scelta valida
050200041117     c     V2copz        Lookup    opz                                    30
050300041117if  4c                   If        Not *In30
050400041117     c                   Eval      *In28 = *On
050500041117     c                   Eval      *In47 = *On
050600041117     c                   Eval      V2cMsg = msg(08)
050700041117     c                   Eval      recsf = nrr
050800041117     c                   Update    Ta8002s
050900041117     c                   Goto      Emettosf
051000041117e   4c                   EndIf
051100041117
051200041117      * Visualizzazione
051300041117if  4C                   If        V2copz = '5'
051400041117     c                   Eval      recsf = nrr
051500041117     c                   ExSr      Sr_Modifica
051600041117e   4c                   EndIf
051700041117
051800041117      * Manutenzione
051900041117if  4c                   If        V2copz = '2'
052000041117     c                   Eval      recsf = nrr
052100050617      * controllo se posso manutenzionare il luogo
052200050617     c                   Eval      kCod = '4L'
052300050617     c                   Eval      kKey = v2ccod
052400050617     c     kTab          Chain     Tabel00f
052500050617if  2c                   If        Not %Found(Tabel00f) or
052600050617     c                             TblFlg <> *Blanks
052700050617     c                   Eval      *In47 = *On
052800050617     c                   Eval      *In28 = *On
052900050617     c                   Eval      V2cmsg = msg(03)
053000050617     c                   Update    Ta8002s
053100050617     c                   Goto      Emettosf
053200050617    2c                   EndIf
053300050617     c                   Eval      ds4l = TblUni
053400050617if  2c                   If        Not *In12 and §4lufi <> 'S'
053500050617     c                   Eval      *In47 = *On
053600050617     c                   Eval      *In28 = *On
053700050617     c                   Eval      V2cmsg = msg(03)
053800050617     c                   Update    Ta8002s
053900050617     c                   Goto      Emettosf
054000050617e   2c                   EndIf
054100070411      * controllo se posso manutenzionare il cliente
054200070411     c                   movel     v2ccli        w003a
054300100505
054400100505      * se utente di sede e luogo 101 e cliente logistica (ntw=LOG) tutto ok
054500100505     c                   eval      $okcli = *off
054600100505    5c                   if        v2ccod = '101' and *in12
054700100505     c                   clear                   og143
054800100505     c     w0030         chain     azorg01l
054900100505    6c                   if        %found(azorg01l) and orgfva = *blanks
055000100505     c                   eval      og143 = orgde3
055100100505    6c                   endif
055200100505    6c                   if        §ogntw = 'LOG'
055300100505     c                   eval      $okcli = *on
055400100505    6c                   endif
055500100505    5c                   endif
055600090710     C
055700100505    5c                   if        not $okcli
055800090710     c                   clear                   tntaa1ds
055900090710     c                   eval      itaa1ksc =v2ccli
056000090710     c                   eval      itaa1caut='§UTEcli'
056100090710     c                   clear                   kpjbu
056200090710     c                   movel     tntaa1ds      kpjbu
056300090710     c                   call      'TNTAA1C'
056400090710     c                   parm                    kpjba
056500090710     c                   movel     kpjbu         tntaa1ds
056600090710     c
056700100505    5c                   if        otaa1fabi='N'
056800070411     c                   eval      *in47 = *on
056900070411     c                   eval      *in28 = *on
057000070411     c                   eval      v2cmsg = msg(09)
057100070411     c                   update    ta8002s
057200070411     c                   goto      emettosf
057300100505    6c                   endif
057400100505    5c                   endif
057500041117     c                   ExSr      Sr_Modifica
057600100505e   4c                   EndIf
057700041119
057800041119      * Copia
057900041119if  4c                   If        V2copz = '3'
058000041119     c                   Eval      recsf = nrr
058100050617      * controllo se posso manutenzionare il luogo
058200050617     c                   Eval      kCod = '4L'
058300050617     c                   Eval      kKey = v2ccod
058400050617     c     kTab          Chain     Tabel00f
058500050617if  2c                   If        Not %Found(Tabel00f) or
058600050617     c                             TblFlg <> *Blanks
058700050617     c                   Eval      *In47 = *On
058800050617     c                   Eval      *In28 = *On
058900050617     c                   Eval      V2cmsg = msg(03)
059000050617     c                   Update    Ta8002s
059100050617     c                   Goto      Emettosf
059200050617e   2c                   EndIf
059300050617     c                   Eval      ds4l = TblUni
059400050617if  2c                   If        Not *In12 and §4lufi <> 'S'
059500050617     c                   Eval      *In47 = *On
059600050617     c                   Eval      *In28 = *On
059700050617     c                   Eval      V2cmsg = msg(03)
059800050617     c                   Update    Ta8002s
059900050617     c                   Goto      Emettosf
060000050617e   2c                   EndIf
060100041119     c                   ExSr      Sr_Modifica
060200041119e   4c                   EndIf
060300041117
060400041117      * Ripulisco il campo opzione  (qui non devo segnalare errori)
060500041117      *   a meno che non sia stata una selezione
060600041117     c                   Clear                   V2copz
060700041117     c                   Update    Ta8002s
060800041117
060900041117e   3c                   EndDo
061000041117
061100041117e   2c                   EndIf
061200041117
061300041117e   1c                   EndDo
061400041117
061500041117     c                   EndSr
061600041117
061700041117      *------------------------------------------------------------------------*
061800041117      * CARICO SUBFILE
061900041117      *------------------------------------------------------------------------*
062000041117     c     Sr_Carsfl     BegSr
062100041117
062200041117     c                   Clear                   V2copz
062300041117     c                   Eval      V2cCod = SpeCod
062400041117     c                   Eval      V2cRag = SpeRag
062500041117     c                   Eval      V2cCli = SpeCli
062600041117     c                   Eval      V2cInd = SpeInd
062700041118     c                   Eval      V2cFax = SpeFax
062800041117     c                   Eval      V2cCap = SpeCap
062900041117     c                   Eval      V2cLoc = SpeLoc
063000041122     c                   If        SpeNaz = *Blanks
063100041117     c                   Eval      V2cPro = SpePro
063200041122     c                   Else
063300041122     c                   Eval      V2cPro = SpeNaz
063400041122     c                   EndIf
063500041117
063600041117      * Decodifico Luogo
063700041117     c                   Clear                   V2dCod
063800041117     c                   Eval      kCod = '4L'
063900041118     c                   Eval      kKey = V2cCod
064000041117     c     kTab          Chain     Tabel00f
064100041117if  2c                   If        %Found(Tabel00f)
064200041117     c                   Eval      ds4l = TblUni
064300041117e   2c                   EndIf
064400041117     c                   Eval      V2dCod = §4ldes
064500041117
064600041117      * Recupero il campo e-mail
064700041117     c                   Clear                   V2cMail
064800041117     c     kSp2          Chain     Fnsp201l
064900041117if  2c                   If        %Found(Fnsp201l)
065000041117     c                   Eval      V2cMail = Sp2Est
065100041117e   2c                   EndIf
065200041117
065300041117     c                   Add       1             nrr
065400041117     c                   Write     Ta8002s
065500041117
065600041117     c                   EndSr
065700041117
065800041117      *------------------------------------------------------------------------*
065900041117      * RICHIAMO IL PROGRAMMA DI MODIFICA
066000041117      *------------------------------------------------------------------------*
066100041117     c     Sr_Modifica   BegSr
066200041117
066300041117     c                   Clear                   Tnta81ds
066400041118     c                   If        V2cOpz <> *Blanks
066500041117     c                   Eval      I81Opz = V2copz
066600041118     c                   Eval      I81Cod = V2cCod
066700041117     c                   Eval      I81Cli = V2cCli
066800041118     c                   Else
066900041118     c                   If        V1cCli = *Zeros
067000041118     c                   Eval      I81Cod = V1cCod
067100041118     c                   EndIf
067200041118     c                   Eval      I81Cli = V1cCli
067300041118     c                   EndIf
067400060216     c                   eval      i81gcli = partnta60
067500060216     c                   eval      i81icli = parimta60
067600041117     c                   Call      'TNTA81R'
067700041117     c                   Parm                    Kpjba
067800041117     c                   Parm                    Tnta81ds
067900041221
068000041221     c                   If        O81C03 = '1'
068100041221     c                   Goto      Fine
068200041221     c                   EndIf
068300041117
068400041117     c                   EndSr
068500041117
068600041116      *------------------------------------------------------------------------*
068700041117      * ROUTINE INIZIALE
068800041116      *------------------------------------------------------------------------*
068900041116     c     *Inzsr        BegSr
069000041116
069100041116     c     *Entry        Plist
069200041116     c                   Parm                    Kpjba
069300041122     c                   Eval      Param80 = Kpjbu
069400041116
069500041122     c                   If        Paropz <> *Blanks
069600041116     c                   Eval      *In01 = *On
069700041116     c                   EndIf
069800041117
069900041116     c     *dtaara       define    §azute        azuteds
070000041116     c     *dtaara       define    §datiute      ddatiute
070100041116     c                   in(E)     *dtaara
070200041116     c                   If        %error  or RSUT = *blanks
070300041116     c                   Clear                   Tibs34ds
070400041116     c                   Call      'TIBS34R'
070500041116     c                   Parm                    Tibs34ds
070600041116     c                   In        *dtaara
070700041116     c                   EndIf
070800090710
070900090710     c* Richiamo solo per avere codice di abilitazione
071000090710     c                   clear                   tntaa1ds
071100090710     c                   eval      itaa1caut='§utecli'
071200090710     c                   clear                   kpjbu
071300090710     c                   movel     tntaa1ds      kpjbu
071400090710     c                   call      'TNTAA1C'
071500090710     c                   parm                    kpjba
071600090710     c                   movel     kpjbu         tntaa1ds
071700090710     c
071800090710     C* UTENTE NON abilitato
071900090710     c                   if        otaa1fabi='N'
072000041116     c                   Eval      *In02 = *On
072100041116     c                   Eval      *In28 = *On
072200041116     c                   Eval      V1cmsg = msg(01)
072300041116e   1c                   EndIf
072400041117
072500041116      * Controllo se sono in sede
072600041116     c                   If        Simfel = *Zeros
072700041116     c                   Eval      *In12 = *On
072800041116     c                   EndIf
072900041122
073000041122      * se richiamato imposto già i campi
073100041122     c                   If        *In01
073200041122     c                   Move      Parcli        V1cCli
073300070412     c                   Eval      V1cFil = 999
073400041122     c                   EndIf
073500041122
073600041122     c                   Eval      *In03 = *On
073700041122     c                   If        *In01 and paropz = '5'
073800041122     c                   Eval      *In03 = *Off
073900041122     c                   EndIf
074000041116
074100041116      * KLIST
074200041116     c     kTab          Klist
074300041116     c                   Kfld                    Codut
074400041116     c                   Kfld                    kCod
074500041116     c                   Kfld                    kKey
074600041116
074700041116     c     kAco          Klist
074800041116     c                   Kfld                    Codut
074900041116     c                   Kfld                    DutKci
075000041116     c                   Kfld                    V1cCli
075100041116
075200041116     c     kSpe          Klist
075300041116     c                   Kfld                    kSpeFls
075400041116     c                   Kfld                    V1cCli
075500041116
075600041116     c     kSpe01        Klist
075700041116     c                   Kfld                    kSpeFls
075800041116     c                   Kfld                    V1cCli
075900041116     c                   Kfld                    V1cCod
076000041117
076100041117     c     kSp2          Klist
076200041117     c                   Kfld                    SpeCli
076300041117     c                   Kfld                    SpeCod
076400041117     c                   Kfld                    kSp2Tpe
076500041117
076600041116     c                   EndSr
076700041116      *------------------------------------------------------------------------*
076800041116
076900041117** MSG  Lungh. 78                                                            *
077000090710UTENTE NON ABILITATO ALLA GESITONE CLIENTI                                    01
077100041116Immettere il Luogo o il Cliente                                               02
077200041116Codice Luogo Errato o non utilizzabile                                        03
077300041116Cliente non gestibile con 9999 o 8888                                         04
077400041116Cliente Inesistente o non gestibile                                           05
077500070412Immettere una filiale valida                                                  06
077600041116Ampiezza massima subfile superata                                             07
077700041117Opzione immessa non valida                                                    08
077800070411Cliente non gestibile è possibile solo visualizzare                           09
077900070412Filiale non in gestione all'utente                                            09
