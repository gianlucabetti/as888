000100060803     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200041117
000300041117      *------------------------------------------------------------------------*
000400041117      *                                                                        *
000500041117      *                 GESTIONE LUOGHI DI DESTINAZIONE    ?                   *
000600041117      *                                                                        *
000700041117      *------------------------------------------------------------------------*
000800041117
000900041117     fCnaco00f  if   e           k disk
001000041117     fFncls01l  if   e           k disk
001100041117     fFnspe01l  uf a e           k disk
001200041117     fFnsp201l  uf a e           k disk
001300041118     fTabel00f  if   e           k disk
001400041217     fAzorg01l  if   e           k disk
001500041217     fTnta81D   cf   e             workstn sfile(ta8103s:nrr)
001600041117
001700041117      *------------------------------------------------------------------------*
001800041117      *  RIEPILOGO INDICATORI
001900041117      *------------------------------------------------------------------------*
002000041117      * 01 - IMMISSIONE
002100041117      * 02 - MANUTENZIONE
002200041119      * 03 - COPIA
002300041117      * 05 - INTERROGAZIONE
002400060216      * 06 - proteggo il codice luogo
002500060216      * 07 - proteggo il codice cliente
002600041221      * 10 - Utente abilitato alla gestione pers. p.o. partenza
002700041117      * 11 - Utente non abilitato - esce dal pgm
002800041117      * 12 - Utente di sede
002900041221      * 13 - Esistono person. p.o. partenza
003000041221      * 14 - Visualizzo F7
003100060202      * 15 - Protezione indirizzo mail per luogo 001
003200041217      * 20 - ON   SFLDSP
003300041217      * 21 - ON   SFLDSPCTL
003400041217      * 21 - OFF  SFLCLR
003500041117      * 28 - ERRORE GENERICO DSPF
003600041117      * 30 - Comodo
003700041117      * 40 - Errore codice luogo
003800041117      * 41 - Errore codice cliente
003900041117      * 42 - Errore ragione sociale
004000041117      * 43 - Errore indirizzo
004100041117      * 44 - Errore cap
004200041117      * 45 - Errore località
004300041117      * 46 - Errore provincia
004400041117      * 47 - Errore nazione
004500041117      * 48 - Errore Telefono
004600041117      * 49 - Errore Fax
004700041117      * 50 - Errore IVA
004800041118      * 51 - Errore e-mail
004900041217      * 52 - Errore P.o. partenza
005000041217      * 53 - Errore P.o. arrivo
005100060803      * 54 - Errore Zona arrivo
005200070316      * 55 - Errore giro
005300041117      * 90 - Riemissione videata
005400041117      *------------------------------------------------------------------------*
005500041117
005600041117      *   V A R I A B I L I
005700041117     d codut           s                   Like(TblKut) Inz(1)
005800041117     d kCod            s                   Like(TblCod)
005900041117     d kKey            s                   Like(TblKey)
006000041117     d kSpeFls         s                   Like(SpeFls) Inz('L')
006100041217     d kSp2Tpe         s                   Like(Sp2Tpe)
006200041217     d nrr             s              4  0
006300041119     d savcli          s                   Like(V1cCli)
006400041117     d xx              s              3  0
006500041221     d yy              s              3  0
006600041117     d wdes            s             25
006700041117     d werr            s                   Like(O13Err)
006800041117     d wfax            s              1    Inz('0')
006900041221     d wforza          s              1    Inz('0')
007000070411     d wforzaiva       s              1    inz(*Off)
007100041221     d wf7             s              1    Inz('0')
007200041117     d wmsg            s                   Like(O13Msg)
007300041217     d wpa             s             60
007400041221     d wpers           s              1    Inz('0')
007500070316     d wpg1            s             60
007600070316     d wpg2            s             60
007700070316     d wpg3            s             60
007800070316     d wpg4            s             60
007900041217     d wpp             s             60
008000060803     d wpz             s             40
008100060803     d wkey5           s              5
008200041117     d wsvcap          s                   Like(V1cCap)
008300041117     d wsvloc          s                   Like(V1cLoc)
008400041117     d wsvnaz          s                   Like(V1cNaz)
008500041117     d wsvpro          s                   Like(V1cPro)
008600041117     d wfzv            s                   Like(E13Fzv)
008700041117     d wfz1            s                   Like(E13Fz1)
008800041117     d wfz2            s                   Like(E13Fz2)
008900041117     d wfz3            s                   Like(E13Fz3)
009000070316     d ww              s              3  0
009100070411     d w002a           s              2
009200041117     d w003a           s              3
009300041117     d w0030           s              3  0
009400041117     d w0040           s              4  0
009500041119     d w034a           s             34
009600070316     d zz              s              3  0
009700090115     d $okcli          s               n
009800041117
009900041117      *   S C H I E R E
010000041117     d c2f             s              1    Dim(100)
010100041117     d c2h             s              1    Dim(100)
010200041117     d car             s              1    Dim(10) ctdata perrcd(10)
010300041119     d mod             s             15    Dim(4)  ctdata perrcd(1)
010400070316     d msg             s             78    Dim(27) ctdata perrcd(1)
010500041221     d skpa            s              3    Dim(20) inz(*Zeros)
010600070316     d skpg1           s             10    Dim(6)
010700070316     d skpg2           s             10    Dim(6)
010800070316     d skpg3           s             10    Dim(6)
010900070316     d skpg4           s             10    Dim(6)
011000041221     d skpp            s              3    Dim(20) inz(*Zeros)
011100060803     d skpz            s              2    Dim(20) inz(*Zeros)
011200041117     d t2f             s              3    Dim(100)
011300041117     d t2h             s              1    Dim(100)
011400041221     d wskpa           s              3    Dim(20) inz(*Zeros)
011500070316     d wskpg1          s             10    Dim(6)
011600070316     d wskpg2          s             10    Dim(6)
011700070316     d wskpg3          s             10    Dim(6)
011800070316     d wskpg4          s             10    Dim(6)
011900041221     d wskpp           s              3    Dim(20) inz(*Zeros)
012000060803     d wskpz           s              2    Dim(20) inz(*Zeros)
012100041117
012200041117      *   D S   I N T E R N E / E S T E R N E
012300041117     d                 ds
012400041117     d  wapert                 1      1
012500041117     d  wchius                10     10
012600041119     d  wClsFlo                1     10
012700041117
012800070411     d                 ds
012900070411     d v1ivaeu                 1      2
013000070411     d v1ivait                 3     16
013100070411     d v1civa                  1     16
013200041122
013300041122     d Param           ds
013400041122     d  parke1                 1     11
013500041122     d  parctr                12     14
013600041122     d  parapl                15     15
013700041122     d  pardes                16     55
013800060426     d  parint                56     56
013900090715     d  parcont               57     57
014000090715     d  pargcli               58     58
014100090715     d  paricli               59     59
014200090715     d  parcmm                60     66
014300041117
014400041117     d                sds
014500041117     d  Vtcpgm                 1     10
014600041117
014700041117     d Azuteds       e ds                  Extname(Azute00f)
014800041117     d dDatiute      e ds
014900041118     d dsemail       e ds
015000041117     d ds15          e ds
015100041117     d ds2f          e ds
015200041117     d ds2h          e ds
015300041117     d ds4l          e ds
015400041119     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
015500041119     d ds_cnind      e ds                  inz  extname(Cnind00f)
015600041119     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
015700041119     d ds_fncls      e ds                  inz  extname(Fncls00f)
015800070316     d fidg09ds      e ds
015900041117     d Fnlv13ds      e ds
016000041117     d Kpjba         e ds
016100090115     d og143         e ds
016200041117     d Tibs02ds      e ds
016300041117     d Tibs34ds      e ds
016400041118     d Tibs69ds      e ds
016500041117     d Tisi00ds      e ds
016600041117     d Tisi95ds      e ds
016700090715     d tnta12ds      e ds
016800041117     d Tnta81ds      e ds
016900090713     d tntaa1ds      e ds
017000041117     d Trul42ds      e ds
017100070411     d xcfivads      e ds
017200070411     d  ivaeu                  3      4
017300070411     d  ivait                  5     18
017400041117
017500041117      *   C O S T A N T I
017600041117      * titolo videata (lunghezza massima 34)
017700041117     d VtcTit          C                   CONST('** G E S T I O N E  L U O G H-
017800041117     d                                      I **')
017900041117
018000041117      *------------------------------------------------------------------------*
018100041117
018200041117      * Aggiornamento luoghi
018300041117if  1c                   If        *In02
018400041117     c     kSpe          Chain(e)  Fnspe01l
018500041117      * rcd allocato
018600041117     c                   If        %Error
018700041117     c                   ExSr      Sr_Alloca
018800041117     c                   Goto      Fine
018900041117     c                   EndIf
019000041117e   1c                   EndIf
019100041118
019200041119      * Visualizza/Copia luoghi
019300041119if  1c                   If        *In05 or *In03
019400041118     c     kSpe          Chain(n)  Fnspe01l
019500041118e   1c                   EndIf
019600041117
019700041119     c                   ExSr      Sr_Car01d
019800041221
019900041221     c                   Reset                   wpers
020000041221     c                   Reset                   wforza
020100041221     c                   Reset                   wf7
020200070411     c                   eval      wforzaiva = *off
020300041119
020400041221do  1c                   Do        *Hival
020500041117
020600041117     c                   Exfmt     Ta8101d
020700041117     c                   Eval      *In28 = *Off
020800041117     c                   Eval      *In90 = *Off
020900041117
021000041117      * Se utente non abilitato esce dal programma
021100041221     c   11              Leave
021200041117      * F3=Fine
021300041221     c                   If        *Inkc or
021400041117      * F12=Ritorno
021500041221     c                             *Inkl
021600041221      * se sono state fatte delle modifiche alle pers. p.o. devo avvisare che
021700041221      * le modifiche fatte andranno perse
021800041221     c                   If        wpers = *On and wforza = *Off and
021900041221     c                             wpp <> *Blanks and wpp <> *Zeros
022000041221     c                   Eval      V1cmsg = msg(24)
022100041221     c                   Eval      *In28 = *On
022200041221     c                   Eval      wforza = *On
022300041221     c                   Iter
022400041221     c                   EndIf
022500041221     c                   Leave
022600041221     c                   EndIf
022700041117
022800041117      * F16=Annulla
022900041221     c                   If        *Inkq
023000041221     c                   ExSr      Sr_Annulla
023100041221     c                   Leave
023200041221     c                   EndIf
023300090715
023400090715      * F2=Rubrica
023500090715     c                   If        *Inkb
023600090715     c                   ExSr      Sr_Rubrica
023700090715     c                   Iter
023800090715     c                   EndIf
023900041122
024000041122      * F18=Int.Note
024100041217     c                   If        *Inks
024200041217     c                   ExSr      Sr_Note
024300041221     c                   Iter
024400041217     c                   EndIf
024500041117
024600041221      * Controllo i dati se non è visualizzazione
024700041117     c  n05              ExSr      Sr_Ctr01d
024800041117
024900041221     c                   If        *In28 or *In90
025000041221     c                   Iter
025100041217     c                   EndIf
025200041217
025300041221      * gestisco la videata delle pers. p.o. partenza
025400041221     c                   If        *Inkg
025500041221     c                   ExSr      Sr_Ges02d
025600041221     c                   Iter
025700041221     c                   EndIf
025800041117
025900041117      * F6=Conferma
026000041221     c                   If        *Inkf
026100041221     c                   ExSr      Sr_Conferma
026200041221     c                   Leave
026300041221     c                   EndIf
026400041221
026500041221e   1c                   EndDo
026600041221
026700041221     c     Fine          Tag
026800041118
026900041118     c                   If        *InKc
027000041118     c                   Eval      O81C03 = '1'
027100041118     c                   EndIf
027200041118     c                   If        *InKl
027300041118     c                   Eval      O81C12 = '1'
027400041118     c                   EndIf
027500090713     C*
027600090713     C                   clear                   tntaa1ds
027700090713     C                   movel     'C'           Itaa1Tla
027800090713     C                   movel(p)  tntaa1ds      kpjbu
027900090713     C                   CALL      'TNTAA1C'
028000090713     C                   PARM                    KPJBA
028100090713     c
028200041117
028300041117     c                   Eval      *InLr = *On
028400041117
028500041117      *------------------------------------------------------------------------*
028600041118      * VIDEATA PER RCD ALLOCATO AD ALTRO UTENTE
028700041117      *------------------------------------------------------------------------*
028800041117     c     Sr_Alloca     BegSr
028900041117
029000041118     c                   Eval      V1cCod = I81Cod
029100041118     c                   Eval      V1cCli = I81Cli
029200041117     c                   ExSr      Sr_CtrCod
029300041117     c                   Exfmt     Ta8102w
029400041117
029500041117     c                   EndSr
029600041117
029700041117      *------------------------------------------------------------------------*
029800041117      * CARICA DATI NELLA VIDEATA DAL FILE
029900041117      *------------------------------------------------------------------------*
030000041117     c     Sr_Car01d     BegSr
030100041217
030200041217     c                   Eval      *In10 = *Off
030300041221     c                   Eval      *In13 = *Off
030400041221     c                   Eval      *In14 = *Off
030500041117
030600041119if  1c                   If        Not *In01
030700041117     c                   Eval      V1cRag = SpeRag
030800041117     c                   Eval      V1cInd = SpeInd
030900041117     c                   Eval      V1cCap = SpeCap
031000041117     c                   Eval      V1cLoc = SpeLoc
031100041117     c                   Eval      V1cPro = SpePro
031200041117     c                   Eval      V1cNaz = SpeNaz
031300041117     c                   Eval      V1cTel = SpeTel
031400041117     c                   Eval      V1cFax = SpeFax
031500041117     c                   Eval      V1cPer = SpePer
031600070411     c                   movel     spepiv        w002a
031700070411     c                   if        w002a >= '00'
031800070411     c                   movel     spepiv        v1ivait
031900070411     c                   else
032000070411     c                   movel     spepiv        v1civa
032100070411     c                   endif
032200041217     c                   Eval      kSp2Tpe = 'EM'
032300041117     c     kSp2          Chain(n)  Fnsp201l
032400041119     c                   If        %Found(Fnsp201l)
032500041118     c                   Eval      V1cMail = Sp2Est
032600041119     c                   EndIf
032700041119e   1c                   EndIf
032800041119
032900041118     c                   Eval      kCod = '4L'
033000041118     c                   Eval      kKey = V1cCod
033100041118     c     kTab          Chain     Tabel00f
033200041118if  1c                   If        %Found(Tabel00f)
033300041118     c                   Eval      ds4l = TblUni
033400041118     c                   Eval      V1dCod = §4ldes
033500041118e   1c                   EndIf
033600041217
033700041221      * flag di forzatura p.o. faccio vedere il tasto funzione F7
033800041221if  1c                   If        §4lPof = 'S'
033900041221     c                   Eval      *In14 = *On
034000041221      * Controllo se utente abilitato alla gestione dei p.o. partenza
034100070410     c**!!!              If        %subst(knmus:1:3) = 'EDP'
034200090713     c                   if        otaa1flna = 'S'
034300041217     c                   Eval      *In10 = *On
034400041221     c                   EndIf
034500041221      * se non è immissione e non è copia
034600041221      * controllo se ci sono dei p.o. personalizzati
034700041221if  2c                   If        Not *In01 and Not *In03
034800041221     c                   Eval      kSp2Tpe = 'PP'
034900041221     c     kSp2          Setll     Fnsp201l
035000041221     c                   If        %Equal(Fnsp201l)
035100041221     c                   Eval      *In13 = *On
035200070702      * se ho trovato dei p.o. personalizzati devo caricare il subfile
035300070702      * così facendo non si rischia di perdere dei dati nel caso che l'utente
035400070702      * non visualizzi la seconda videata
035500070702     c                   Eval      rec = 1
035600070702      * Pulisco Subfile
035700070702     c                   ExSr      Sr_Pulsfl
035800070702      * Carico il Subfile
035900070702     c                   ExSr      Sr_Carsfl
036000041221     c                   EndIf
036100041221e   2c                   EndIF
036200041221e   1c                   EndIf
036300041118
036400041118if  1c                   If        V1cCli > *Zeros
036500041119     c                   ExSr      Sr_Anagr
036600041118e   1c                   EndIf
036700041117
036800041117     c                   EndSr
036900041119
037000041119      *------------------------------------------------------------------------*
037100041119      * RECUPERO I DATI DALL'ANAGRAFICA CLIENTI
037200041119      *------------------------------------------------------------------------*
037300041119     c     Sr_Anagr      BegSr
037400041117
037500041119     c                   Eval      savcli = V1cCli
037600041119     c                   clear                   TIBS69DS
037700041119     c                   move      V1cCli        I69kac
037800041119     c                   move      V1cCli        I69kin
037900041119     c                   call      'TIBS69R'
038000041119     c                   parm                    TIBS69DS
038100041119     c                   parm                    ds_cnaco
038200041119     c                   parm                    ds_cnind
038300041119     c                   parm                    ds_cnclp
038400041119     c                   parm                    ds_fncls
038500041119if  1c                   If        O69err <> *on
038600041119     c                   movel     ACOrag        V1dCli
038700041119     c                   movel     IndVia        V1cInd1
038800041119     c                   movel     IndCap        V1cCap1
038900041119     c                   movel     IndPrv        V1cPro1
039000041119     c                   movel     IndCit        V1cLoc1
039100041119     c                   movel     IndSta        V1cNaz1
039200041119     c                   movel     IndTlf        V1cFax1
039300041119e   1c                   EndIf
039400041119
039500041119     c                   EndSr
039600041217
039700041117      *------------------------------------------------------------------------*
039800041117      * CONTROLLI
039900041117      *------------------------------------------------------------------------*
040000041117     c     Sr_Ctr01d     BegSr
040100041119
040200041119     c                   Setoff                                       404142
040300041119     c                   Setoff                                       434445
040400041119     c                   Setoff                                       464748
040500041119     c                   Setoff                                       495051
040600041117
040700041117      * --> Codice Luogo
040800041117     c                   ExSr      Sr_CtrCod
040900041119     c   28
041000041119     cor 90              Goto      Endctr01d
041100041117
041200041117      * --> Codice Cliente
041300041119if  1c                   If        *In01 or *In03
041400041117     c                   If        V1cCli = *Zeros
041500041117     c                   Eval      *In28 = *On
041600041117     c                   Eval      *In41 = *On
041700041117     c                   Eval      V1cmsg = msg(04)
041800041117     c                   Goto      Endctr01d
041900041117     c                   EndIf
042000041119      * codice valido
042100041117     c                   Move      V1cCli        w0040
042200041118if  2c                   If        w0040 = *all'9' or
042300041117     c                             w0040 = *all'8'
042400041117     c                   Eval      *In28 = *On
042500041117     c                   Eval      *In41 = *On
042600041117     c                   Eval      V1cmsg = msg(05)
042700041117     c                   Goto      EndCtr01d
042800041118e   2c                   EndIf
042900090115      * se utente di sede e luogo 101 e cliente logistica (ntw=LOG) tutto ok
043000090115     c                   eval      $okcli = *off
043100090115     c                   if        v1ccod = '101' and *in12
043200090115     c                   movel     v1ccli        w0030
043300090115     c                   clear                   og143
043400090115     c     w0030         chain     azorg01l
043500090115if  2c                   if        %found(azorg01l) and orgfva = *blanks
043600090115     c                   eval      og143 = orgde3
043700090115     c                   endif
043800090115     c                   if        §ogntw = 'LOG'
043900090115     c                   eval      $okcli = *on
044000090115     c                   endif
044100090115     c                   endif
044200041117      * gestibile dall'utente
044300090115     c                   if        not $okcli
044400090713     c                   clear                   tntaa1ds
044500090713     c                   eval      itaa1ksc =v1ccli
044600090713     c                   eval      itaa1caut='§UTEcli'
044700090713     c                   clear                   kpjbu
044800090713     c                   movel     tntaa1ds      kpjbu
044900090713     c                   call      'TNTAA1C'
045000090713     c                   parm                    kpjba
045100090713     c                   movel     kpjbu         tntaa1ds
045200090713     c
045300090713     c                   if        otaa1fabi='N'
045400090713     c
045500090713     c***                Movel     V1cCli        w003a
045600090713     c***  w003a         Lookup    skpog                                  30
045700090713if  2c***                If        Not *In30
045800090713     c
045900041117     c                   Eval      *In41 = *On
046000041117     c                   Eval      *In28 = *On
046100041117     c                   Eval      V1cmsg = msg(06)
046200041117     c                   Goto      EndCtr01d
046300041118e   2c                   EndIf
046400090115     c                   endif
046500041117      * controllo
046600060216      * se provengo da gestione anagrafica clienti e sono in immissione
046700060216      * dell'anagrafica non devo controllare l'esitenza del cliente
046800060216     c                   if        i81gcli <> '1' and i81icli <> '1'
046900041117     c     kAco          Chain     Cnaco00f
047000041118if  2c                   If        Not %Found(Cnaco00f) or
047100041117     c                             AcoFlg <> *Blanks
047200041117     c                   Eval      *In41 = *On
047300041117     c                   Eval      *In28 = *On
047400041117     c                   Eval      V1cmsg = msg(06)
047500041117     c                   Goto      EndCtr01d
047600041118e   2c                   EndIf
047700060216     c                   endif
047800041119      * modificato codice
047900041119     c                   If        V1cCli <> savcli
048000041119     c                   ExSr      Sr_Anagr
048100041119     c                   EndIf
048200041118e   1c                   EndIf
048300041118
048400041118      * se immissione controllo che il rcd non sia già stato inserito
048500041119     c                   If        *In01 or *In03
048600041118     c     kSpe          Chain(n)  Fnspe01l
048700041118     c                   If        %Found(Fnspe01l)
048800041118     c                   Eval      *In40 = *On
048900041118     c                   Eval      *In28 = *On
049000041118     c                   Eval      V1cmsg = msg(19)
049100041118     c                   Goto      EndCtr01d
049200041118     c                   EndIf
049300041118     c                   EndIf
049400041117
049500041117      * --> Ragione Sociale
049600041117     c                   If        V1cRag = *Blanks
049700041117     c                   Eval      *In42 = *On
049800041117     c                   Eval      *In28 = *On
049900041117     c                   Eval      V1cmsg = msg(07)
050000041117     c                   Goto      EndCtr01d
050100041117     c                   EndIf
050200041117      * caratteri validi
050300041117     c                   Eval      xx = 1
050400041117     c                   Dow       xx <= 10 and
050500041117     c                             car(xx) <> *Blanks
050600041117     c     car(xx)       Scan      V1cRag                                 30
050700041117     c                   If        *In30
050800041117     c                   Eval      *In42 = *On
050900041117     c                   Eval      *In28 = *On
051000041117     c                   Eval      V1cmsg = msg(08)
051100041117     c                   Goto      EndCtr01d
051200041117     c                   EndIf
051300041117     c                   Eval      xx = xx + 1
051400041117     c                   EndDo
051500041117
051600041117      * --> Indirizzo
051700041117     c                   If        V1cInd = *Blanks
051800041117     c                   Eval      *In43 = *On
051900041117     c                   Eval      *In28 = *On
052000041117     c                   Eval      V1cmsg = msg(09)
052100041117     c                   Goto      EndCtr01d
052200041117     c                   EndIf
052300041117      * caratteri validi
052400041117     c                   Eval      xx = 1
052500041117     c                   Dow       xx <= 10 and
052600041117     c                             car(xx) <> *Blanks
052700041117     c     car(xx)       Scan      V1cInd                                 30
052800041117     c                   If        *In30
052900041117     c                   Eval      *In43 = *On
053000041117     c                   Eval      *In28 = *On
053100041117     c                   Eval      V1cmsg = msg(08)
053200041117     c                   Goto      EndCtr01d
053300041117     c                   EndIf
053400041117     c                   Eval      xx = xx + 1
053500041117     c                   EndDo
053600041117
053700041117      * --> Località
053800041117      * obbligatoria
053900041117     c                   If        V1cLoc = *Blanks
054000041117     c                   Eval      *In45 = *On
054100041117     c                   Eval      *In28 = *On
054200041117     c                   Eval      V1cmsg = msg(10)
054300041117     c                   Goto      EndCtr01d
054400041117     c                   EndIf
054500041117      * caratteri validi
054600041117     c                   Eval      xx = 1
054700041117     c                   Dow       xx <= 10 and
054800041117     c                             car(xx) <> *Blanks
054900041117     c     car(xx)       Scan      V1cLoc                                 30
055000041117     c                   If        *In30
055100041117     c                   Eval      *In45 = *On
055200041117     c                   Eval      *In28 = *On
055300041117     c                   Eval      V1cmsg = msg(08)
055400041117     c                   Goto      EndCtr01d
055500041117     c                   EndIf
055600041117     c                   Eval      xx = xx + 1
055700041117     c                   EndDo
055800041117
055900041117      * --> Nazione
056000041119      * ricerca
056100041119     c     '?'           Scan      V1cNaz                                 30
056200041119if  1c                   If        *In30
056300041119     c                   Eval      kCod = '15'
056400041119     c                   Clear                   kKey
056500041119     c                   Clear                   wDes
056600041119     c                   Call      'X§TABER'
056700041119     c                   Parm                    Codut
056800041119     c                   Parm                    kCod
056900041119     c                   Parm                    kKey
057000041119     c                   Parm                    wDes
057100041119     c                   Eval      *In47 = *On
057200041119     c                   Eval      *In90 = *On
057300041119     c                   Eval      V1cNaz = kKey
057400041119     c                   Goto      EndCtr01d
057500041119e   1c                   EndIf
057600041119      * controllo
057700041117     c                   Eval      kCod = '15'
057800041117     c                   Eval      kKey = V1cNaz
057900041117     c     kTab          Chain     Tabel00f
058000041117if  1c                   If        Not %Found(Tabel00f) or
058100041117     c                             TblFlg <> *Blanks
058200041117     c                   Eval      *In47 = *On
058300041117     c                   Eval      *In28 = *On
058400041117     c                   Eval      V1cmsg = msg(11)
058500041117     c                   Goto      EndCtr01d
058600041117e   1c                   EndIf
058700041117     c                   Eval      ds15 = TblUni
058800041117
058900041117      * Ricerca località
059000041117if  1c                   If        §15cpp = *Blanks and
059100041117     c                             %subst(V1cLoc:1:1) = '?'
059200041119     c                   Move      V1cLoc        w034a
059300041117      * se immesso solo '?' inizio la ricerca da 'A'
059400041119     c                   If        w034a = *Blanks
059500041119     c                   Eval      w034a = 'A'
059600041117     c                   EndIf
059700041117     c                   Eval      *In90 = *On
059800041117     c                   Eval      *In45 = *On
059900041117      * ricerca
060000041117     c                   Clear                   Tisi00ds
060100041117     c                   Eval      D00Op0 = 'L01'
060200041119     c                   Eval      D00Loc = w034a
060300041117     c                   Z-add     *date         D00Dri
060400041117     c                   Call      'TISI30R'
060500041117     c                   Parm                    Kpjba
060600041117     c                   Parm                    Tisi00ds
060700041117      * se selezione
060800041117if  2c                   If        D00Cap <> *Blanks
060900041117     c                   Eval      V1cCap = D00Cap
061000041117     c                   Eval      V1cLoc = D00Loc
061100041117     c                   Eval      V1cNaz = D00Nar
061200041117     c                   Eval      V1cPro = D00Prv
061300041117   x2c                   Else
061400041117     c                   Clear                   V1cLoc
061500041117e   2c                   EndIf
061600041117     c                   Goto      Endctr01d
061700041117e   1c                   EndIf
061800041117
061900041117      * --> Cap
062000041117      * controllo solo se nazione con cappario
062100041117     c                   If        §15cpp = *Blanks
062200041117     c                   ExSr      Sr_CtrCap
062300041117     c   28              Goto      Endctr01d
062400041117     c                   EndIf
062500041117
062600041117      * --> Provincia
062700041117      * obbligatoria per nazione italia
062800041117     c                   If        V1cPro = *Blanks and
062900041117     c                             V1cNaz = *Blanks
063000041117     c                   Eval      *In46 = *On
063100041117     c                   Eval      *In28 = *On
063200041117     c                   Eval      V1cmsg = msg(16)
063300041117     c                   Goto      EndCtr01d
063400041117     c                   EndIf
063500041117      * controllo
063600041207     c                   If        V1cPro <> *Blanks
063700041117     c                   Eval      kCod = 'PR'
063800041117     c                   Eval      kKey = V1cPro
063900041117     c     kTab          Chain     Tabel00f
064000041117if  1c                   If        Not %Found(Tabel00f) or
064100041117     c                             TblFlg <> *Blanks
064200041117     c                   Eval      *In46 = *On
064300041117     c                   Eval      *In28 = *On
064400041117     c                   Eval      V1cmsg = msg(17)
064500041117     c                   Goto      EndCtr01d
064600041117e   1c                   EndIf
064700041207     c                   EndIf
064800041117      * caratteri validi
064900041117     c                   Eval      xx = 1
065000041117     c                   Dow       xx <= 10 and
065100041117     c                             car(xx) <> *Blanks
065200041117     c     car(xx)       Scan      V1cPro                                 30
065300041117     c                   If        *In30
065400041117     c                   Eval      *In46 = *On
065500041117     c                   Eval      *In28 = *On
065600041117     c                   Eval      V1cmsg = msg(08)
065700041117     c                   Goto      EndCtr01d
065800041117     c                   EndIf
065900041117     c                   Eval      xx = xx + 1
066000041117     c                   EndDo
066100041117
066200041117      * --> FAX
066300041117     c                   Eval      wfax  = *Off
066400041117      * se il luogo è x invio comunicazioni di giacenza controllo se
066500041117      * fax obbligatorio
066600041117if  1c                   If        §4lgia = 'S'
066700041117     c     V1cCli        Chain     Fncls01l
066800041117if  2c                   If        %Found(Fncls01l)
066900041119     c                   Eval      wclsflo = ClsFlo
067000041117     c                   Eval      xx = 1
067100041117     c     wapert        Lookup    c2f(xx)                                30
067200041117if  3c                   If        *In30 and t2f(xx) = 'F'
067300041117     c                   Eval      wfax = *On
067400041117   x3c                   Else
067500041117     c                   Eval      xx = 1
067600041117     c     wchius        Lookup    c2h(xx)                                30
067700041117if  4c                   If        *In30 and t2h(xx) = 'F'
067800041117     c                   Eval      wfax = *On
067900041117e   4c                   EndIf
068000041117e   3c                   EndIf
068100041117e   2c                   EndIf
068200041117e   1c                   EndIf
068300041117      * controllo
068400041117if  1c                   If        V1cFax <> *Blanks or
068500041117     c                             §4lfax = 'S' or wfax = *On
068600041117     c                   Clear                   Trul42ds
068700041117     c                   Eval      D42Fax = V1cFax
068800041117     c                   Call      'TRUL42R'
068900041117     c                   Parm                    Trul42ds
069000041117     c                   If        D42Err = '1'
069100041117     c                   Eval      *In49 = *On
069200041117     c                   Eval      *In28 = *On
069300041117     c                   Eval      V1cmsg = D42Msg
069400041117     c                   Goto      EndCtr01d
069500041117     c                   EndIf
069600041117e   1c                   EndIf
069700041118
069800041117      * --> Telefono
069900041117if  1c                   If        V1cTel <> *Blanks
070000041117     c                   Clear                   Trul42ds
070100041117     c                   Eval      D42Fax = V1cTel
070200041117     c                   Call      'TRUL42R'
070300041117     c                   Parm                    Trul42ds
070400041117     c                   If        D42Err = '1'
070500041117     c                   Eval      *In48 = *On
070600041117     c                   Eval      *In28 = *On
070700041117     c                   Eval      V1cmsg = D42Msg
070800041117     c                   Goto      EndCtr01d
070900041117     c                   EndIf
071000041117e   1c                   EndIf
071100041119
071200041119      * --> Partita IVA
071300070411     c                   if        v1civa <> *blanks
071400041119     c                   ExSr      Sr_CtrIva
071500041119     c   28              Goto      Endctr01d
071600041119     c                   EndIf
071700041118
071800041118      * --> e-mail
071900041118     c                   If        V1cMail <> *Blanks
072000041118     c                   Clear                   dsemail
072100041118     c                   Eval      §emlindi = V1cMail
072200041118     c                   Call      'XEMAIL'
072300041118     c                   Parm                    dsemail
072400041118     c                   If        §emlerro = '1'
072500041118     c                   Eval      *In28 = *On
072600041118     c                   Eval      *In51 = *On
072700041118     c                   Eval      V1cMsg = §emlmsgo
072800041118     c                   Goto      EndCtr01d
072900041118     c                   EndIf
073000041118     c                   Eval      V1cMail = §emlindo
073100041118     c                   EndIf
073200041117
073300041117     c     Endctr01d     EndSr
073400041117
073500041117      *------------------------------------------------------------------------*
073600041117      * CONTROLLO IL CODICE LUOGO
073700041117      *------------------------------------------------------------------------*
073800041117     c     Sr_CtrCod     BegSr
073900041117
074000041117      * Obbligatorio
074100041117     c                   If        V1cCod = *Blanks
074200041117     c                   Eval      *In28 = *On
074300041117     c                   Eval      *In40 = *On
074400041117     c                   Eval      V1cmsg = msg(02)
074500041117     c                   Goto      EndCtrcod
074600041117     c                   EndIf
074700041119
074800041119      * ricerca
074900041119     c     '?'           Scan      V1cCod                                 30
075000041119if  1c                   If        *In30
075100041119     c                   Eval      kCod = '4L'
075200041119     c                   Clear                   kKey
075300041119     c                   Clear                   wDes
075400041119     c                   Call      'X§TABER'
075500041119     c                   Parm                    Codut
075600041119     c                   Parm                    kCod
075700041119     c                   Parm                    kKey
075800041119     c                   Parm                    wDes
075900041119     c                   Eval      *In40 = *On
076000041119     c                   Eval      *In90 = *On
076100041119     c                   Eval      V1cCod = kKey
076200041119     c                   Goto      EndCtrCod
076300041119e   1c                   EndIf
076400041117
076500041117     c                   Eval      kCod = '4L'
076600041117     c                   Eval      kKey = V1cCod
076700041117     c     kTab          Chain     Tabel00f
076800041117if  1c                   If        Not %Found(Tabel00f) or
076900041117     c                             TblFlg <> *Blanks
077000041117     c                   Eval      *In40 = *On
077100041117     c                   Eval      *In28 = *On
077200041117     c                   Eval      V1cmsg = msg(03)
077300041117     c                   Goto      EndCtrcod
077400041117e   1c                   EndIf
077500041117     c                   Eval      ds4l = TblUni
077600071128if  1c                   If        Not *In12 and §4lufi <> 'S'
077700071128     c**!!!                        (*In01 or *In03)
077800041117     c                   Eval      *In40 = *On
077900041117     c                   Eval      *In28 = *On
078000041117     c                   Eval      V1cmsg = msg(03)
078100041117     c                   Goto      EndCtrcod
078200041117e   1c                   EndIf
078300041117      * decodifico
078400041117     c                   Eval      V1dCod = §4ldes
078500041117
078600041117     c     EndCtrcod     EndSr
078700041117
078800041117      *------------------------------------------------------------------------*
078900041117      * CONTROLLO ESISTENZA CAP
079000041117      *------------------------------------------------------------------------*
079100041117     c     Sr_CtrCap     BegSr
079200041117
079300041117      * controllo il cap
079400041117     c                   Clear                   Tisi95ds
079500041117     c                   Clear                   Fnlv13ds
079600041117     c                   Eval      I95Tcn = '7'
079700041117     c                   Movel     V1cCap        I95Cap
079800041117     c                   Movel     V1cNaz        I95Nar
079900041117     c                   Movel     V1cInd        I95Ind
080000041117     c                   Movel     V1cLoc        I95Loc
080100041117     c                   Movel     V1cPro        I95Prv
080200041117     c                   Movel     'S'           I13Af0
080300041117     c                   Movel     'S'           I13Af1
080400041117     c                   Movel     'S'           I13Sz2
080500041117     c                   Movel     'S'           I13La3
080600041117     c                   Movel     'S'           I13Sz3
080700041117     c                   Movel     'S'           I13Cnv
080800041117     c                   Movel     'S'           I13Szv
080900041117     c   12              Z-Add     46            I95Tfp
081000041117     c  n12              Z-Add     Simfel        I95Tfp
081100041117     c                   Movel     'S'           I95Fre
081200041117
081300041117      * Gestione errori forzabili: se cambiato CAP/Località
081400041117      * riazzero i valori di errori forzabili
081500041117if  1c                   If        V1cCap <> wsvcap
081600041117     c                   Clear                   wfzv
081700041117     c                   Clear                   wfz1
081800041117     c                   Clear                   wfz2
081900041117     c                   Clear                   wfz3
082000041117     c                   Eval      wsvcap = V1cCap
082100041117e   1c                   EndIf
082200041117      * Modifica località
082300041117if  1c                   If        V1cLoc <> wsvloc
082400041117     c                   Clear                   wfz1
082500041117     c                   Clear                   wfz2
082600041117     c                   Clear                   wfz3
082700041117     c                   Eval      wsvloc = V1cLoc
082800041117e   1c                   EndIf
082900041117      * Modifica provincia
083000041117if  1c                   If        V1cPro <> wsvpro
083100041117     c                   Clear                   wfz3
083200041117     c                   Eval      wsvpro = V1cPro
083300041117e   1c                   EndIf
083400041117
083500041117     c                   Movel     wfzv          E13Fzv
083600041117     c                   Movel     wfz1          E13Fz1
083700041117     c                   Movel     wfz2          E13Fz2
083800041117     c                   Movel     wfz3          E13Fz3
083900041117     c                   Call      'FNLV13R'
084000041117     c                   Parm                    Kpjba
084100041117     c                   Parm                    Fnlv13ds
084200041117     c                   Parm                    Tisi95ds
084300041117
084400041117      * Ricontrollo errori
084500041117     c                   Movel     O13Err        werr
084600041117     c                   Movel     O13Msg        wmsg
084700041117     c                   Movel     E13Fzv        wfzv
084800041117     c                   Movel     E13Fz1        wfz1
084900041117     c                   Movel     E13Fz2        wfz2
085000041117     c                   Movel     E13Fz3        wfz3
085100041117
085200041117      * Se il livello di affidabilità non è inferiore a 2 e
085300041117      * il livello di reperimento dati non è inferiore a 3
085400041117      * reimposto i dati da ds TISI95
085500041117if  1c                   If        O13Ron = 'S' or
085600041117     c                             O13Roc = 'S' or
085700041117     c                             O13Rop = 'S' or
085800041117     c                             O13Rol = 'S'
085900041117     c                   Movel     O95Loc        V1cLoc
086000041117     c                   Movel     O95Prv        V1cPro
086100041117     c                   Movel     O95Nar        V1cNaz
086200041117     c                   Movel     O95Cap        V1cCap
086300041117e   1c                   EndIf
086400041117
086500041117     c                   ExSr      Sr_CtrInd
086600041117      * Se non ci sono errori proseguo
086700041117     c   28              Goto      EndCtrcap
086800041117
086900041117      * Controllo se reimpostata località
087000041117     c                   If        O13Rol = 'S'
087100041117     c                   Eval      *In28 = *On
087200041117     c                   Eval      *In45 = *On
087300041117     c                   Eval      V1cMsg = msg(12)
087400041117     c                   Goto      EndCtrcap
087500041117     c                   EndIf
087600041117      * Controllo se reimpostata Provincia
087700041117     c                   If        O13Rop = 'S'
087800041117     c                   Eval      *In28 = *On
087900041117     c                   Eval      *In46 = *On
088000041117     c                   Eval      V1cMsg = msg(13)
088100041117     c                   Goto      EndCtrcap
088200041117     c                   EndIf
088300041117      * Controllo se reimpostato CAP
088400041117     c                   If        O13Roc = 'S'
088500041117     c                   Eval      *In28 = *On
088600041117     c                   Eval      *In44 = *On
088700041117     c                   Eval      V1cMsg = msg(14)
088800041117     c                   Goto      EndCtrcap
088900041117     c                   EndIf
089000041117      * Controllo se reimpostata Nazione
089100041117     c                   If        O13Ron = 'S'
089200041117     c                   Eval      *In28 = *On
089300041117     c                   Eval      *In47 = *On
089400041117     c                   Eval      V1cMsg = msg(15)
089500041117     c                   Goto      EndCtrcap
089600041117     c                   EndIf
089700041117
089800041117     c     EndCtrcap     EndSr
089900041117
090000041117      *------------------------------------------------------------------------*
090100041117      *  CONTROLLO INDIRIZZO
090200041117      *------------------------------------------------------------------------*
090300041117     c     Sr_CtrInd     BegSr
090400041117
090500041118      * Errore
090600041117if  1c                   If        werr <> *Blanks and
090700041117     c                             wmsg <> *Blanks
090800041117s   2c                   Select
090900041117      * Errore su località/città
091000041117w   2c                   When      werr = '3'
091100041117     c                   Eval      *In28 = *On
091200041117     c                   Eval      *In45 = *On
091300041117      * Errore su Cap
091400041117w   2c                   When      werr = '5'
091500041117     c                   Eval      *In28 = *On
091600041117     c                   Eval      *In44 = *On
091700041117      * Errore su provincia
091800041117w   2c                   When      werr ='4'
091900041117     c                   Eval      *In28 = *On
092000041117     c                   Eval      *In46 = *On
092100041117      * Nazione  errata
092200041117w   2c                   When      werr = '6'
092300041117     c                   Eval      *In28 = *On
092400041117     c                   Eval      *In47 = *On
092500041117e   2c                   EndSl
092600041117     c                   Eval      V1cmsg = wmsg
092700041117e   1c                   EndIf
092800041117
092900041117     c                   EndSr
093000041117
093100041117      *------------------------------------------------------------------------*
093200041117      * CONTROLLO PARTITA IVA
093300041117      *------------------------------------------------------------------------*
093400041117     c     Sr_CtrIva     BegSr
093500041117
093600070411     c                   clear                   xcfivads
093700070411     c                   eval      xcfivamod = 'P'
093800070411     c                   eval      xcfivapar = v1civa
093900070411     c                   eval      xcfivanar = v1cnaz
094000070411     c                   eval      xcfivaprv = v1cpro
094100070411     c                   call      'XCFIVAR'
094200070411     c                   parm                    xcfivads
094300070411      * --> errata forzabile
094400070411     c                   If        xcfivaflg = -9 and wforzaiva = *off
094500070411     c                   Eval      *In28 = *On
094600070411     c                   Eval      *In50 = *On
094700070411     c                   Eval      v1cmsg = xcfivamsg
094800070411     c                   Eval      v1cmsg = %trim(v1cmsg) + ' Enter x forzare'
094900070411     c                   eval      wforzaiva = *on
095000070411     c                   Leavesr
095100070411     c                   EndIf
095200070411      * --> errore
095300070411     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
095400070411     c                   Eval      *In28 = *On
095500070411     c                   Eval      *In50 = *On
095600070411     c                   eval      v1cmsg = xcfivamsg
095700070411     c                   Leavesr
095800070411     c                   EndIf
095900070411      * se non impostato devo riportare il codice iso della partita iva che
096000070411      * mi viene passato dal pgm di controllo
096100070411     c                   if        v1ivaeu <> ivaeu
096200070411     c                   eval      v1ivaeu = ivaeu
096300070411     c                   eval      *in39 = *on
096400070411     c                   eval      *in90 = *on
096500070411     c                   endif
096600041117
096700041117     c                   EndSr
096800041217
096900041217      *------------------------------------------------------------------------*
097000041221      * GESTICO LE PERSONALIZZAZIONI P.O. PARTENZA
097100041217      *------------------------------------------------------------------------*
097200041217     c     Sr_Ges02d     BegSr
097300041217
097400041217     c                   Eval      rec = 1
097500041221
097600041221      * Pulisco Subfile
097700041221     c                   ExSr      Sr_Pulsfl
097800041221      * Carico il Subfile
097900041221     c                   ExSr      Sr_Carsfl
098000041221
098100041221     c                   Eval      wf7 = *On
098200041221
098300041221do  1c                   Do        *Hival
098400041221
098500041221      * Emetto il Subfile
098600041221     c                   ExSr      Sr_Emisfl
098700041217
098800041221      * F12 - Torno alla videata precedente
098900041221     c                   If        *Inkl and Not *In28
099000041221     c                   Leave
099100041221     c                   EndIf
099200041217
099300041221      * Gestione Subfile
099400041221     c                   If        nrr > *Zeros  and Not *In05 and
099500041221     c                             *In10
099600041221     c                   ExSr      Sr_Gessfl
099700041221     c                   EndIf
099800041221
099900041221      * Sistemo le sk dei p.o.
100000041217     c                   do        20            nrr
100100041217     c     nrr           Chain     ta8103s                            30
100200041221     c                   If        Not *In30
100300041221     c                   Move      v3scpp        skpp(nrr)
100400041221     c                   Move      v3scpa        skpa(nrr)
100500060803     c                   Move      v3scpz        skpz(nrr)
100600070316      * i giri sono divisi su 4 sk
100700070316     c                   select
100800070316     c                   when      nrr <= 6
100900070316     c                   eval      skpg1(nrr) = v3scgi
101000070316     c                   when      nrr > 6 and nrr <= 12
101100070316     c                   eval      zz = nrr - 6
101200070316     c                   eval      skpg2(zz) = v3scgi
101300070316     c                   when      nrr > 12 and nrr <= 18
101400070316     c                   eval      zz = nrr - 12
101500070316     c                   eval      skpg3(zz) = v3scgi
101600070316     c                   other
101700070316     c                   eval      zz = nrr - 18
101800070316     c                   eval      skpg4(zz) = v3scgi
101900070316     c                   endsl
102000041217     c                   endif
102100041217     c                   enddo
102200041217
102300060405     c                   movea     *zeros        wskpp
102400060405     c                   movea     *zeros        wskpa
102500060803     c                   movea     *zeros        wskpz
102600070316     c                   clear                   wskpg1
102700070316     c                   clear                   wskpg2
102800070316     c                   clear                   wskpg3
102900070316     c                   clear                   wskpg4
103000041221     c                   Clear                   yy
103100041221     c                   do        20            xx
103200041221     c                   If        skpp(xx) > *Zeros
103300041221     c                   add       1             yy
103400041221     c                   eval      wskpp(yy) = skpp(xx)
103500041221     c                   eval      wskpa(yy) = skpa(xx)
103600060803     c                   eval      wskpz(yy) = skpz(xx)
103700070316      * i giri sono divisi su 4 sk
103800070316     c                   select
103900070316     c                   when      yy <= 6
104000070316     c                   eval      wskpg1(yy) = skpg1(xx)
104100070316     c                   when      yy > 6 and yy <= 12
104200070316     c                   eval      zz = yy - 6
104300070316     c                   eval      ww = xx - 6
104400070316     c                   eval      wskpg2(zz) = skpg2(ww)
104500070316     c                   when      yy > 12 and yy <= 18
104600070316     c                   eval      zz = yy - 12
104700070316     c                   eval      ww = xx - 12
104800070316     c                   eval      wskpg3(zz) = skpg3(ww)
104900070316     c                   other
105000070316     c                   eval      zz = yy - 18
105100070316     c                   eval      ww = xx - 18
105200070316     c                   eval      wskpg4(zz) = skpg4(ww)
105300070316     c                   endsl
105400041221     c                   EndIf
105500041221     c                   EndDo
105600041221
105700041221     c                   movea     wskpp         wpp
105800041221     c                   movea     wskpa         wpa
105900060803     c                   movea     wskpz         wpz
106000070316     c                   movea     wskpg1        wpg1
106100070316     c                   movea     wskpg2        wpg2
106200070316     c                   movea     wskpg3        wpg3
106300070316     c                   movea     wskpg4        wpg4
106400041217
106500041221e   1c                   EndDo
106600041217
106700041217     c                   EndSr
106800041221
106900041221      *------------------------------------------------------------------------*
107000041221      * PULISCO IL SUBFILE
107100041221      *------------------------------------------------------------------------*
107200041221     c     Sr_Pulsfl     BegSr
107300041221
107400041221     c                   Clear                   nrr
107500041221     c                   Eval      *In21 = *Off
107600041221     c                   Write     ta8103c
107700041221     c                   Eval      *In20 = *On
107800041221     c                   Eval      *In21 = *On
107900041221
108000041221     c                   EndSr
108100041221
108200041221      *------------------------------------------------------------------------*
108300041221      * CARICO IL SUBFILE
108400041221      *------------------------------------------------------------------------*
108500041221     c     Sr_Carsfl     BegSr
108600041221
108700041221      * Se non è immissione o copia e se non ho mai caricato i dati nel
108800041221      * subfile carico i p.o. dal file
108900041221if  1c                   If        Not *In01 and Not *In03 and wf7 = *Off
109000041221     c                   Eval      kSp2Tpe = 'PP'
109100041221     c     kSp2          Chain(n)  Fnsp201l
109200041221     c                   If        %Found(Fnsp201l)
109300041221     c                   Movel     Sp2Est        wpp
109400041221     c                   EndIf
109500041221     c                   Eval      kSp2Tpe = 'PA'
109600041221     c     kSp2          Chain(n)  Fnsp201l
109700041221     c                   If        %Found(Fnsp201l)
109800041221     c                   Movel     Sp2Est        wpa
109900041221     c                   EndIf
110000060803     c                   Eval      kSp2Tpe = 'PZ'
110100060803     c     kSp2          Chain(n)  Fnsp201l
110200060803     c                   If        %Found(Fnsp201l)
110300060803     c                   Movel     Sp2Est        wpz
110400060803     c                   EndIf
110500070316      * carico anche i giri divisi in 4 tipi record
110600070316     c                   eval      ksp2tpe = 'G1'
110700070316     c     ksp2          chain(n)  fnsp201l
110800070316     c                   if        %found(fnsp201l)
110900070316     c                   movel     sp2est        wpg1
111000070316     c                   endif
111100070316     c                   eval      ksp2tpe = 'G2'
111200070316     c     ksp2          chain(n)  fnsp201l
111300070316     c                   if        %found(fnsp201l)
111400070316     c                   movel     sp2est        wpg2
111500070316     c                   endif
111600070316     c                   eval      ksp2tpe = 'G3'
111700070316     c     ksp2          chain(n)  fnsp201l
111800070316     c                   if        %found(fnsp201l)
111900070316     c                   movel     sp2est        wpg3
112000070316     c                   endif
112100070316     c                   eval      ksp2tpe = 'G4'
112200070316     c     ksp2          chain(n)  fnsp201l
112300070316     c                   if        %found(fnsp201l)
112400070316     c                   movel     sp2est        wpg4
112500070316     c                   endif
112600041221
112700041221     c                   Movea     wpa           skpa
112800041221     c                   Movea     wpp           skpp
112900060803     c                   Movea     wpz           skpz
113000070316     c                   movea     wpg1          skpg1
113100070316     c                   movea     wpg2          skpg2
113200070316     c                   movea     wpg3          skpg3
113300070316     c                   movea     wpg4          skpg4
113400041221e   1c                   EndIf
113500041221
113600041221      * Se ho già caricato almeno una volta il sbfl devo riportare i dati
113700041221      * modificati in precedenza
113800041221if  1c                   If        wf7 = *On
113900041221     c                   Movea     wpa           skpa
114000041221     c                   Movea     wpp           skpp
114100060803     c                   Movea     wpz           skpz
114200070316     c                   movea     wpg1          skpg1
114300070316     c                   movea     wpg2          skpg2
114400070316     c                   movea     wpg3          skpg3
114500070316     c                   movea     wpg4          skpg4
114600041221e   1c                   EndIf
114700041221
114800041221do  1c                   Do        20            xx
114900041221     c                   Clear                   v3scpp
115000041221     c                   Clear                   v3sdpp
115100041221     c                   Clear                   v3scpa
115200041221     c                   Clear                   v3sdpa
115300060803     c                   Clear                   v3scpz
115400070316     c                   clear                   v3scgi
115500070316     c                   clear                   v3sdcgi
115600041221     c                   If        skpp(xx) > *Zeros
115700041221     c                   Move      skpp(xx)      v3scpp
115800041221     c     v3scpp        Chain     Azorg01l
115900041221     c                   If        %Found(Azorg01l)
116000041221     c                   Eval      v3sdpp = OrgDes
116100041221     c                   EndIf
116200041221     c                   If        v3scpp = 999
116300041221     c                   Eval      v3sdpp = 'Tutte le altre'
116400041221     c                   EndIf
116500041221     c                   EndIf
116600041221     c                   If        skpa(xx) > *Zeros
116700041221     c                   Move      skpa(xx)      v3scpa
116800060803     c                   Move      skpz(xx)      v3scpz
116900041221     c     v3scpa        Chain     Azorg01l
117000041221     c                   If        %Found(Azorg01l)
117100041221     c                   Eval      v3sdpa = OrgDes
117200041221     c                   EndIf
117300041221     c                   If        v3scpa = 999
117400041221     c                   Eval      v3sdpa = 'Tutte le altre'
117500041221     c                   EndIf
117600041221     c                   EndIf
117700070316      * giri divisi su 4 sk
117800070316     c                   select
117900070316     c                   when      xx <= 6
118000070316     c                   if        skpg1(xx) > *blanks
118100070316     c                   move      skpg1(xx)     v3scgi
118200070316     c                   exsr      sr_ctrgiro
118300070316     c                   eval      v3sdcgi = d09odes
118400070316     c                   endif
118500070316     c                   when      xx > 6 and xx <= 12
118600070316     c                   eval      zz = xx - 6
118700070316     c                   if        skpg2(zz) > *blanks
118800070316     c                   move      skpg2(zz)     v3scgi
118900070316     c                   exsr      sr_ctrgiro
119000070316     c                   eval      v3sdcgi = d09odes
119100070316     c                   endif
119200070316     c                   when      xx > 12 and xx <= 18
119300070316     c                   eval      zz = xx - 12
119400070316     c                   if        skpg3(zz) > *blanks
119500070316     c                   move      skpg3(zz)     v3scgi
119600070316     c                   exsr      sr_ctrgiro
119700070316     c                   eval      v3sdcgi = d09odes
119800070316     c                   endif
119900070316     c                   other
120000070316     c                   eval      zz = xx - 18
120100070316     c                   if        skpg4(zz) > *blanks
120200070316     c                   move      skpg4(zz)     v3scgi
120300070316     c                   exsr      sr_ctrgiro
120400070316     c                   eval      v3sdcgi = d09odes
120500070316     c                   endif
120600070316     c                   endsl
120700070316
120800041221     c                   Add       1             Nrr
120900041221     c                   Write     ta8103s
121000041221e   1c                   EndDo
121100041221
121200041221     c                   EndSr
121300041221
121400041221      *------------------------------------------------------------------------*
121500041221      * EMETTO IL SUBFILE
121600041221      *------------------------------------------------------------------------*
121700041221     c     Sr_Emisfl     BegSr
121800041221
121900041221     c                   If        nrr > rec
122000041221     c                   Eval      rec = 1
122100041221     c                   EndIf
122200041221
122300041221     c                   Write     ta8103z
122400041221     c                   Exfmt     ta8103c
122500041221
122600041221     c                   EndSr
122700041221
122800041221      *------------------------------------------------------------------------*
122900041221      * GESTIONE SUBFILE
123000041221      *------------------------------------------------------------------------*
123100041221     c     Sr_Gessfl     BegSr
123200041221
123300041221     c                   Clear                   nrr
123400041221
123500041221     c                   Reset                   wforza
123600041221     c                   Eval      wpers = *On
123700041221
123800041221do  1c                   Do        *Hival
123900041221     c                   Eval      nrr = nrr +1
124000041221     c     nrr           Chain     ta8103s                            30
124100041221     c                   If        *In30
124200041221     c                   Leave
124300041221     c                   EndIf
124400041221
124500041221     c                   Eval      *In28 = *Off
124600041221     c                   Eval      *In52 = *Off
124700041221     c                   Eval      *In53 = *Off
124800060803     c                   Eval      *In54 = *Off
124900070316     c                   eval      *in55 = *off
125000041221
125100041221     c                   Clear                   v3sdpp
125200041221      * Controllo p.o. partenza
125300041221if  2c                   If        v3scpp > *Zeros
125400041221if  3c                   If        v3scpp <> 999
125500041221     c     v3scpp        Chain     Azorg01l
125600041221     c                   If        Not %Found(Azorg01l) or
125700041221     c                             OrgFva <> *Blanks or
125800041221     c                             (OrgFag <> 'A' and OrgFag <> 'F')
125900041221     c                   Eval      *In28 = *On
126000041221     c                   Eval      *In52 = *On
126100041221     c                   Eval      v1cmsg = msg(20)
126200041221     c                   Z-add     nrr           rec
126300041221     c                   Update    ta8103s
126400041221     c                   Leave
126500041221     c                   EndIf
126600041221     c                   Eval      v3sdpp = OrgDes
126700041221   x3c                   Else
126800041221     c                   Eval      v3sdpp = 'Tutte le altre'
126900041221e   3c                   EndIf
127000041221    2c                   EndIf
127100041221
127200041221     c                   Clear                   v3sdpa
127300041221      * Controllo p.o. arrivo
127400041221if  2c                   If        v3scpa > *Zeros
127500041221if  3c                   If        v3scpa <> 999
127600041221     c     v3scpa        Chain     Azorg01l
127700041221     c                   If        Not %Found(Azorg01l) or
127800041221     c                             OrgFva <> *Blanks or
127900041221     c                             (OrgFag <> 'A' and OrgFag <> 'F')
128000041221     c                   Eval      *In28 = *On
128100041221     c                   Eval      *In53 = *On
128200041221     c                   Eval      v1cmsg = msg(21)
128300041221     c                   Z-add     nrr           rec
128400041221     c                   Update    ta8103s
128500041221     c                   Leave
128600041221     c                   EndIf
128700041221     c                   Eval      v3sdpa = OrgDes
128800041221   x3c                   Else
128900041221     c                   Eval      v3sdpa = 'Tutte le altre'
129000041221e   3c                   EndIf
129100041221    2c                   EndIf
129200041221
129300060803      * Controllo Zona arrivo
129400060803if  2c                   If        v3scpz > *Zeros
129500060803     c                   movel     v3scpa        wkey5
129600060803     c                   move      v3scpz        wkey5
129700060803     c                   movel(p)  wkey5         kkey
129800060803     c                   move      '03'          kcod
129900060803     c     ktab          Chain     tabel00f
130000060803     c                   If        Not %Found(tabel00f) or
130100060803     c                             TblFlg <> *Blanks
130200060803     c                   Eval      *In28 = *On
130300060803     c                   Eval      *In54 = *On
130400060803     c                   Eval      v1cmsg = msg(26)
130500060803     c                   Z-add     nrr           rec
130600060803     c                   Update    ta8103s
130700060803     c                   Leave
130800060803     c                   EndIf
130900060803    2c                   EndIf
131000070316
131100070316      * giri divisi su 4 sk
131200070316     c                   clear                   v3sdcgi
131300070316      * interrogazione controllo giro
131400070316if  2c                   if        v3scgi <> *blanks
131500070316      * interrogazione
131600070316if  3c                   if        %scan('?':v3scgi) > *zeros
131700070316     c                   clear                   fidg09ds
131800070316     c                   eval      d09iop0 = 'P01'
131900070316     c                   eval      d09ifgs = v3scpa
132000070316     c                   eval      d09idat = *date
132100070709     c                   eval      d09itug = 'C'
132200070316     c                   eval      kpjbu = fidg09ds
132300070316     c                   call      'FIDG09R'
132400070316     c                   parm                    kpjba
132500070316     c                   eval      fidg09ds = kpjbu
132600070316    4c                   if        d09oerr = '1'
132700070316     c                   eval      *in28 = *on
132800070316     c                   eval      *in55 = *on
132900070316     c                   eval      v1cmsg = msg(27)
133000070316     c                   eval      rec = nrr
133100070316     c                   update    ta8103s
133200070316     c                   leave
133300070316    4c                   endif
133400070316    4c                   if        d09ocgi <> ' '
133500070316     c                   eval      v3scgi = d09ocgi
133600070316     c                   eval      v3sdcgi = d09odes
133700070316    4c                   endif
133800070316     c                   eval      rec = nrr
133900070316     c                   update    ta8103s
134000070316     c                   leave
134100070316    3c                   endif
134200070316      * controllo
134300070316     c                   exsr      sr_ctrgiro
134400070316    3c                   if        d09oerr = '1'
134500070316     c                   eval      *in28 = *on
134600070316     c                   eval      *in55 = *on
134700070316     c                   eval      v1cmsg = msg(27)
134800070316     c                   eval      rec = nrr
134900070316     c                   update    ta8103s
135000070316     c                   leave
135100070316    3c                   endif
135200070316    3c                   if        d09ocgi <> ' '
135300070316     c                   eval      v3sdcgi = d09odes
135400070316    3c                   endif
135500070316    2c                   endif
135600060803
135700041221      * Se immesso p.o. partenza devo immettere il p.o. arrivo
135800041221if  2c                   If        v3scpp > *Zeros and v3scpa = *Zeros
135900041221     c                   Eval      *In28 = *On
136000041221     c                   Eval      *In53 = *On
136100041221     c                   Eval      v1cmsg = msg(22)
136200041221     c                   Z-add     nrr           rec
136300041221     c                   Update    ta8103s
136400041221     c                   Leave
136500041221    2c                   EndIf
136600041221
136700041221      * Se immesso p.o. arrivo devo immettere il p.o. partenza
136800041221if  2c                   If        v3scpa > *Zeros and v3scpp = *Zeros
136900041221     c                   Eval      *In28 = *On
137000041221     c                   Eval      *In52 = *On
137100041221     c                   Eval      v1cmsg = msg(23)
137200041221     c                   Z-add     nrr           rec
137300041221     c                   Update    ta8103s
137400041221     c                   Leave
137500041221    2c                   EndIf
137600041221
137700041221      * Se immesso p.o. partenza 999 il p.o. arrivo deve essere 999
137800041221if  2c                   If        v3scpp = 999 and v3scpa <> 999
137900041221     c                   Eval      *In28 = *On
138000041221     c                   Eval      *In53 = *On
138100041221     c                   Eval      v1cmsg = msg(25)
138200041221     c                   Z-add     nrr           rec
138300041221     c                   Update    ta8103s
138400041221     c                   Leave
138500041221    2c                   EndIf
138600041221
138700041221      * Se immesso p.o. arrivo 999 il p.o. partenza deve essere 999
138800041221if  2c                   If        v3scpa = 999 and v3scpp <> 999
138900041221     c                   Eval      *In28 = *On
139000041221     c                   Eval      *In52 = *On
139100041221     c                   Eval      v1cmsg = msg(25)
139200041221     c                   Z-add     nrr           rec
139300041221     c                   Update    ta8103s
139400041221     c                   Leave
139500041221    2c                   EndIf
139600041221
139700041221     c                   Z-add     nrr           rec
139800041221     c                   Update    ta8103s
139900041221
140000041221e   1c                   EndDo
140100041221
140200041221     c                   EndSr
140300041221
140400041118      *------------------------------------------------------------------------*
140500001011      * F6 - Conferma
140600041118      *------------------------------------------------------------------------*
140700041118     c     Sr_Conferma   BegSr
140800030611
140900041119     c   01
141000041119     cor 03              Clear                   Fnspe000
141100041118
141200041118     c                   Eval      SpeFls = kSpeFls
141300041118     c                   Eval      SpeCli = V1cCli
141400041118     c                   Eval      SpeCod = V1cCod
141500041118     c                   Eval      SpeRag = V1cRag
141600041118     c                   Eval      SpeInd = V1cInd
141700041118     c                   Eval      SpeCap = V1cCap
141800041118     c                   Eval      SpeLoc = V1cLoc
141900041118     c                   Eval      SpePro = V1cPro
142000041118     c                   Eval      SpeNaz = V1cNaz
142100070411     c**!!!              Eval      SpePiv = dsiva
142200070411     c                   if        v1ivaeu = *blanks
142300070411     c                   movel     v1ivait       spepiv
142400070411     c                   else
142500070411     c                   movel     v1civa        spepiv
142600070411     c                   endif
142700041118     c                   Eval      SpeTel = V1cTel
142800041118     c                   Eval      SpePer = V1cPer
142900041118     c                   Eval      SpeFax = V1cFax
143000041118     c                   Z-Add     *date         SpeDtr
143100010704
143200041118      * Scrivo
143300041119     c                   If        *In01 or *In03
143400041119     c                   If        V1cMail <> *Blanks
143500041118     c                   Clear                   Fnsp2000
143600041118     c                   Eval      Sp2Cod = SpeCod
143700041118     c                   Eval      Sp2Cli = SpeCli
143800041217     c                   Eval      Sp2Tpe = 'EM'
143900041118     c                   Eval      Sp2Est = V1cMail
144000041118     c                   Write     Fnsp2000
144100041119     c                   EndIf
144200041217
144300041217     c                   If        *In10
144400041217      * P.o. partenza
144500041221     c                   If        wpp <> *Blanks and wpp <> *Zeros
144600041217     c                   Clear                   Fnsp2000
144700041217     c                   Eval      Sp2Cod = SpeCod
144800041217     c                   Eval      Sp2Cli = SpeCli
144900041217     c                   Eval      Sp2Tpe = 'PP'
145000041217     c                   Eval      Sp2Est = wpp
145100041217     c                   Write     Fnsp2000
145200041217     c                   EndIf
145300041217      * P.o. arrivo
145400041221     c                   If        wpa <> *Blanks and wpa <> *Zeros
145500041217     c                   Clear                   Fnsp2000
145600041217     c                   Eval      Sp2Cod = SpeCod
145700041217     c                   Eval      Sp2Cli = SpeCli
145800041217     c                   Eval      Sp2Tpe = 'PA'
145900041217     c                   Eval      Sp2Est = wpa
146000041217     c                   Write     Fnsp2000
146100041217     c                   EndIf
146200060803      * Zona arrivo
146300060803     c                   If        wpz <> *Blanks and wpz <> *Zeros
146400060803     c                   Clear                   Fnsp2000
146500060803     c                   Eval      Sp2Cod = SpeCod
146600060803     c                   Eval      Sp2Cli = SpeCli
146700060803     c                   Eval      Sp2Tpe = 'PZ'
146800060803     c                   Eval      Sp2Est = wpz
146900060803     c                   Write     Fnsp2000
147000060803     c                   EndIf
147100070316      * giri divisi su 4 sk
147200070316     c                   if        wpg1 <> *blanks
147300070316     c                   clear                   fnsp2000
147400070316     c                   eval      sp2cod = specod
147500070316     c                   eval      sp2cli = specli
147600070316     c                   eval      sp2tpe = 'G1'
147700070316     c                   eval      sp2est = wpg1
147800070316     c                   write     fnsp2000
147900070316     c                   endif
148000070316     c                   if        wpg2 <> *blanks
148100070316     c                   clear                   fnsp2000
148200070316     c                   eval      sp2cod = specod
148300070316     c                   eval      sp2cli = specli
148400070316     c                   eval      sp2tpe = 'G2'
148500070316     c                   eval      sp2est = wpg2
148600070316     c                   write     fnsp2000
148700070316     c                   endif
148800070316     c                   if        wpg3 <> *blanks
148900070316     c                   clear                   fnsp2000
149000070316     c                   eval      sp2cod = specod
149100070316     c                   eval      sp2cli = specli
149200070316     c                   eval      sp2tpe = 'G3'
149300070316     c                   eval      sp2est = wpg3
149400070316     c                   write     fnsp2000
149500070316     c                   endif
149600070316     c                   if        wpg4 <> *blanks
149700070316     c                   clear                   fnsp2000
149800070316     c                   eval      sp2cod = specod
149900070316     c                   eval      sp2cli = specli
150000070316     c                   eval      sp2tpe = 'G4'
150100070316     c                   eval      sp2est = wpg4
150200070316     c                   write     fnsp2000
150300070316     c                   endif
150400041217     c                   EndIf
150500041217
150600041118     c                   Write     Fnspe000
150700041118     c                   EndIf
150800041118
150900041118      * Aggiorno
151000041118if  1c                   If        *In02
151100041217      * E-mail
151200041217     c                   Eval      kSp2Tpe = 'EM'
151300041118     c     kSp2          Chain     Fnsp201l
151400041118if  2c                   If        %Found(Fnsp201l)
151500041118if  3c                   If        V1cMail <> *Blanks
151600041118     c                   Eval      Sp2Est = V1cMail
151700041118     c                   Update    Fnsp2000
151800041118   x3c                   Else
151900041118     c                   Delete    Fnsp2000
152000041119e   3c                   EndIf
152100041119   x2c                   Else
152200041119if  3c                   If        V1cMail <> *Blanks
152300041119     c                   Clear                   Fnsp2000
152400041119     c                   Eval      Sp2Cod = SpeCod
152500041119     c                   Eval      Sp2Cli = SpeCli
152600041217     c                   Eval      Sp2Tpe = 'EM'
152700041119     c                   Eval      Sp2Est = V1cMail
152800041119     c                   Write     Fnsp2000
152900041119e   3c                   EndIf
153000041119e   2c                   EndIf
153100041217
153200041217     c                   If        *In10
153300041217      * P.o. partenza
153400041217     c                   Eval      kSp2Tpe = 'PP'
153500041217     c     kSp2          Chain     Fnsp201l
153600041217if  2c                   If        %Found(Fnsp201l)
153700041221if  3c                   If        wpp <> *Blanks and wpp <> *Zeros
153800041217     c                   Eval      Sp2Est = wpp
153900041217     c                   Update    Fnsp2000
154000041217   x3c                   Else
154100041217     c                   Delete    Fnsp2000
154200041217e   3c                   EndIf
154300041217   x2c                   Else
154400041221if  3c                   If        wpp <> *Blanks and wpp <> *Zeros
154500041217     c                   Clear                   Fnsp2000
154600041217     c                   Eval      Sp2Cod = SpeCod
154700041217     c                   Eval      Sp2Cli = SpeCli
154800041217     c                   Eval      Sp2Tpe = 'PP'
154900041217     c                   Eval      Sp2Est = wpp
155000041217     c                   Write     Fnsp2000
155100041217e   3c                   EndIf
155200041217e   2c                   EndIf
155300041217      * P.o. arrivo
155400041217     c                   Eval      kSp2Tpe = 'PA'
155500041217     c     kSp2          Chain     Fnsp201l
155600041217if  2c                   If        %Found(Fnsp201l)
155700041221if  3c                   If        wpa <> *Blanks and wpa <> *Zeros
155800041217     c                   Eval      Sp2Est = wpa
155900041217     c                   Update    Fnsp2000
156000041217   x3c                   Else
156100041217     c                   Delete    Fnsp2000
156200041217e   3c                   EndIf
156300041217   x2c                   Else
156400041221if  3c                   If        wpa <> *Blanks and wpa <> *Zeros
156500041217     c                   Clear                   Fnsp2000
156600041217     c                   Eval      Sp2Cod = SpeCod
156700041217     c                   Eval      Sp2Cli = SpeCli
156800041217     c                   Eval      Sp2Tpe = 'PA'
156900041217     c                   Eval      Sp2Est = wpa
157000041217     c                   Write     Fnsp2000
157100041217e   3c                   EndIf
157200041217e   2c                   EndIf
157300060803      * Zona arrivo
157400060803     c                   Eval      kSp2Tpe = 'PZ'
157500060803     c     kSp2          Chain     Fnsp201l
157600060803if  2c                   If        %Found(Fnsp201l)
157700060803if  3c                   If        wpz <> *Blanks and wpz <> *Zeros
157800060803     c                   Eval      Sp2Est = wpz
157900060803     c                   Update    Fnsp2000
158000060803   x3c                   Else
158100060803     c                   Delete    Fnsp2000
158200060803e   3c                   EndIf
158300060803   x2c                   Else
158400060803if  3c                   If        wpz <> *Blanks and wpz <> *Zeros
158500060803     c                   Clear                   Fnsp2000
158600060803     c                   Eval      Sp2Cod = SpeCod
158700060803     c                   Eval      Sp2Cli = SpeCli
158800060803     c                   Eval      Sp2Tpe = 'PZ'
158900060803     c                   Eval      Sp2Est = wpz
159000060803     c                   Write     Fnsp2000
159100060803e   3c                   EndIf
159200060803e   2c                   EndIf
159300070316      * giri divisi su 4 sk
159400070316     c                   eval      ksp2tpe = 'G1'
159500070316     c     ksp2          chain     fnsp201l
159600070316    2c                   if        %found(fnsp201l)
159700070316    3c                   if        wpg1 <> *blanks
159800070316     c                   eval      sp2est = wpg1
159900070316     c                   update    fnsp2000
160000070316   x3c                   else
160100070316     c                   delete    fnsp2000
160200070316    3c                   endif
160300070316   x2c                   else
160400070316    3c                   if        wpg1 <> *blanks
160500070316     c                   clear                   fnsp2000
160600070316     c                   eval      sp2cod = specod
160700070316     c                   eval      sp2cli = specli
160800070316     c                   eval      sp2tpe = 'G1'
160900070316     c                   eval      sp2est = wpg1
161000070316     c                   write     fnsp2000
161100070316    3c                   endif
161200070316    2c                   endif
161300070316     c                   eval      ksp2tpe = 'G2'
161400070316     c     ksp2          chain     fnsp201l
161500070316    2c                   if        %found(fnsp201l)
161600070316    3c                   if        wpg2 <> *blanks
161700070316     c                   eval      sp2est = wpg2
161800070316     c                   update    fnsp2000
161900070316   x3c                   else
162000070316     c                   delete    fnsp2000
162100070316    3c                   endif
162200070316   x2c                   else
162300070316    3c                   if        wpg2 <> *blanks
162400070316     c                   clear                   fnsp2000
162500070316     c                   eval      sp2cod = specod
162600070316     c                   eval      sp2cli = specli
162700070316     c                   eval      sp2tpe = 'G2'
162800070316     c                   eval      sp2est = wpg2
162900070316     c                   write     fnsp2000
163000070316    3c                   endif
163100070316    2c                   endif
163200070316     c                   eval      ksp2tpe = 'G3'
163300070316     c     ksp2          chain     fnsp201l
163400070316    2c                   if        %found(fnsp201l)
163500070316    3c                   if        wpg3 <> *blanks
163600070316     c                   eval      sp2est = wpg3
163700070316     c                   update    fnsp2000
163800070316   x3c                   else
163900070316     c                   delete    fnsp2000
164000070316    3c                   endif
164100070316   x2c                   else
164200070316    3c                   if        wpg3 <> *blanks
164300070316     c                   clear                   fnsp2000
164400070316     c                   eval      sp2cod = specod
164500070316     c                   eval      sp2cli = specli
164600070316     c                   eval      sp2tpe = 'G3'
164700070316     c                   eval      sp2est = wpg3
164800070316     c                   write     fnsp2000
164900070316    3c                   endif
165000070316    2c                   endif
165100070316     c                   eval      ksp2tpe = 'G4'
165200070316     c     ksp2          chain     fnsp201l
165300070316    2c                   if        %found(fnsp201l)
165400070316    3c                   if        wpg4 <> *blanks
165500070316     c                   eval      sp2est = wpg4
165600070316     c                   update    fnsp2000
165700070316   x3c                   else
165800070316     c                   delete    fnsp2000
165900070316    3c                   endif
166000070316   x2c                   else
166100070316    3c                   if        wpg4 <> *blanks
166200070316     c                   clear                   fnsp2000
166300070316     c                   eval      sp2cod = specod
166400070316     c                   eval      sp2cli = specli
166500070316     c                   eval      sp2tpe = 'G4'
166600070316     c                   eval      sp2est = wpg4
166700070316     c                   write     fnsp2000
166800070316    3c                   endif
166900070316    2c                   endif
167000041217     c                   EndIf
167100041217
167200041119     c                   Update    Fnspe000
167300041118e   1c                   EndIf
167400010704
167500041118     c                   EndSr
167600041118
167700041118      *------------------------------------------------------------------------*
167800010703      * F16 - Annullamento
167900041118      *------------------------------------------------------------------------*
168000041118     c     Sr_Annulla    BegSr
168100001011
168200041217     c     kSp21         Setll     Fnsp201l
168300041217     c                   Do        *Hival
168400041217     c     kSp21         Reade     Fnsp201l
168500041217     c                   If        %Eof(Fnsp201l)
168600041217     c                   Leave
168700041217     c                   EndIf
168800041118     c                   Delete    Fnsp2000
168900041217     c                   EndDo
169000001011
169100041118     c                   Delete    Fnspe000
169200041118
169300041118     c                   EndSr
169400090715
169500090715      *------------------------------------------------------------------------*
169600090715      * F18 - Note
169700090715      *------------------------------------------------------------------------*
169800090715     c     Sr_Note       BegSr
169900090715
170000090715     c                   clear                   param
170100090715     c                   Clear                   parctr
170200090715     c                   Eval      parapl = 'C'
170300090715     c                   Movel     DutKci        parke1
170400090715     c                   Move      V1cCli        parke1
170500090715     c                   Eval      pardes = V1dCli
170600090715     c   05              eval      parint = '1'
170700090715     c                   Eval      Kpjbu = Param
170800090715     c                   Call      'TNTA53R'
170900090715     c                   Parm                    Kpjba
171000090715
171100090715     c                   EndSr
171200041122
171300041122      *------------------------------------------------------------------------*
171400090715      * F2 - Rubrica
171500041122      *------------------------------------------------------------------------*
171600090715     c     Sr_Rubrica    BegSr
171700090715
171800090715     c                   clear                   tnta12ds
171900090715     c  n05              eval      ta12ric = 'M'
172000090715     c   05              eval      ta12ric = 'V'
172100090715     c                   Eval      ta12apl = 'C'
172200090715     c                   Eval      ta12ksc = v1ccli
172300090715     c                   Eval      ta12rag = v1dcli
172400090715     c                   Call      'TNTA12R'
172500090715     c                   Parm                    kpjba
172600090715     c                   Parm                    tnta12ds
172700041122
172800041122     c                   EndSr
172900070316
173000070316      *------------------------------------------------------------------------*
173100070316      * controlla giro
173200070316      *------------------------------------------------------------------------*
173300070316     c     sr_ctrgiro    begsr
173400070316
173500070316     c                   clear                   fidg09ds
173600070316     c                   eval      d09iop0 = '001'
173700070316     c                   eval      d09ifgs = v3scpa
173800070316     c                   eval      d09idat = *date
173900070316     c                   eval      d09icgi = v3scgi
174000070709     c                   eval      d09itug = 'C'
174100070316     c                   eval      kpjbu = fidg09ds
174200070316     c                   call      'FIDG09R'
174300070316     c                   parm                    kpjba
174400070316     c                   eval      fidg09ds = kpjbu
174500070316
174600070316     c                   endsr
174700041117
174800041117      *------------------------------------------------------------------------*
174900041117      * ROUTINE INIZIALE
175000041117      *------------------------------------------------------------------------*
175100041117     c     *Inzsr        BegSr
175200041117
175300041117     c     *Entry        Plist
175400041117     c                   Parm                    Kpjba
175500041117     c                   Parm                    Tnta81ds
175600041117
175700041117     c     *dtaara       define    §azute        azuteds
175800041117     c     *dtaara       define    §datiute      ddatiute
175900041117     c                   in(E)     *dtaara
176000041117     c                   If        %error  or RSUT = *blanks
176100041117     c                   Clear                   Tibs34ds
176200041117     c                   Call      'TIBS34R'
176300041117     c                   Parm                    Tibs34ds
176400041117     c                   In        *dtaara
176500041117     c                   EndIf
176600090713
176700090713
176800090713     c* Richiamo solo per avere codice di abilitazione
176900090713     c                   clear                   tntaa1ds
177000090713     c                   eval      itaa1caut='§utecli'
177100090713     c                   clear                   kpjbu
177200090713     c                   movel     tntaa1ds      kpjbu
177300090713     c                   call      'TNTAA1C'
177400090713     c                   parm                    kpjba
177500090713     c                   movel     kpjbu         tntaa1ds
177600090713     c
177700090713     C* UTENTE NON abilitato
177800090713     c                   if        otaa1fabi='N'
177900041117     c                   Eval      *In11 = *On
178000041117     c                   Eval      *In28 = *On
178100041117     c                   Eval      V1cmsg = msg(01)
178200041221e   2c                   EndIf
178300041117
178400041117      * Controllo se sono in sede
178500041117     c                   If        Simfel = *Zeros
178600041117     c                   Eval      *In12 = *On
178700041117     c                   EndIf
178800041117
178900041117     c                   Select
179000041117      * Immissione
179100041117     c                   When      I81opz = *Blanks
179200041117     c                   Eval      V1mod = mod(01)
179300041117     c                   Eval      *In01 = *On
179400041117      * Manutenzione
179500041117     c                   When      I81opz = '2'
179600041117     c                   Eval      V1mod = mod(02)
179700041117     c                   Eval      *In02 = *On
179800041119      * Copia
179900041119     c                   When      I81opz = '3'
180000041119     c                   Eval      V1mod = mod(03)
180100041119     c                   Eval      *In03 = *On
180200041117      * Interrogazione
180300041117     c                   When      I81opz = '5'
180400041119     c                   Eval      V1mod = mod(04)
180500041117     c                   Eval      *In05 = *On
180600041117     c                   EndSl
180700041117
180800041118     c                   Eval      V1cCod = I81Cod
180900041118     c                   Eval      V1cCli = I81Cli
181000041118      * posiziono il cursore per immissione
181100041119     c                   If        *In01 or *In03
181200041118     c                   Select
181300041118     c                   When      V1cCli <> *Zeros and V1cCod <> *Blanks
181400041118     c                   Eval      *In42 = *On
181500041118     c                   When      V1cCod = *Blanks
181600041118     c                   Eval      *In40 = *On
181700041118     c                   When      V1cCli <> *Zeros
181800041118     c                   Eval      *In41 = *On
181900041118     c                   EndSl
182000041118     c                   EndIf
182100060216      * proteggo i campi chiave se già passati
182200060216      * se non è copia
182300060216     c                   if        not *in03
182400060216     c                   eval      *in06 = (v1ccod <> *blanks)
182500060216     c                   eval      *in07 = (v1ccli <> *zeros)
182600060216     c                   endif
182700060216      * se provengo da anagrafica clienti proteggo sempre il cliente
182800060216     c                   if        i81gcli = '1'
182900060216     c                   eval      *in07 = *on
183000060216     c                   endif
183100041117
183200041117      * Carico Tabella 2F
183300041117     c                   Clear                   xx
183400041117     c                   Clear                   c2f
183500041117     c                   Clear                   t2f
183600041117     c                   Eval      kCod = '2F'
183700041117     c     kTab01        Setll     Tabel00f
183800041117     c                   Do        *Hival
183900041117     c     kTab01        Reade     Tabel00f
184000041117     c                   If        %Eof(Tabel00f)
184100041117     c                   Leave
184200041117     c                   EndIf
184300041117     c                   If        xx >= 100
184400041117     c                   Leave
184500041117     c                   EndIf
184600041117     c                   If        TblFlg <> *Blanks
184700041117     c                   Iter
184800041117     c                   EndIf
184900041117     c                   Eval      xx = xx + 1
185000041117     c                   Eval      ds2f = TblUni
185100041117     c                   Movel     TblKey        c2f(xx)
185200041117     c                   Movel     §2fFtp        t2f(xx)
185300041117     c                   EndDo
185400041117
185500041117      * Carico Tabella 2H
185600041117     c                   Clear                   xx
185700041117     c                   Clear                   c2h
185800041117     c                   Clear                   t2h
185900041117     c                   Eval      kCod = '2H'
186000041117     c     kTab01        Setll     Tabel00f
186100041117     c                   Do        *Hival
186200041117     c     kTab01        Reade     Tabel00f
186300041117     c                   If        %Eof(Tabel00f)
186400041117     c                   Leave
186500041117     c                   EndIf
186600041117     c                   If        xx >= 100
186700041117     c                   Leave
186800041117     c                   EndIf
186900041117     c                   If        TblFlg <> *Blanks
187000041117     c                   Iter
187100041117     c                   EndIf
187200041117     c                   Eval      xx = xx + 1
187300041117     c                   Eval      ds2h = TblUni
187400041117     c                   Movel     TblKey        c2h(xx)
187500041118     c                   Movel     §2hFax        t2h(xx)
187600041117     c                   EndDo
187700041117
187800041117      * KLIST
187900041117     c     kTab          Klist
188000041117     c                   Kfld                    Codut
188100041117     c                   Kfld                    kCod
188200041117     c                   Kfld                    kKey
188300041117
188400041117     c     kTab01        Klist
188500041117     c                   Kfld                    Codut
188600041117     c                   Kfld                    kCod
188700041117
188800041117     c     kAco          Klist
188900041117     c                   Kfld                    Codut
189000041117     c                   Kfld                    DutKci
189100041117     c                   Kfld                    V1cCli
189200041117
189300041117     c     kSpe          Klist
189400041117     c                   Kfld                    kSpeFls
189500041117     c                   Kfld                    V1cCli
189600041117     c                   Kfld                    V1cCod
189700041117
189800041117     c     kSp2          Klist
189900041117     c                   Kfld                    SpeCli
190000041117     c                   Kfld                    SpeCod
190100041117     c                   Kfld                    kSp2Tpe
190200041217
190300041217     c     kSp21         Klist
190400041217     c                   Kfld                    SpeCli
190500041217     c                   Kfld                    SpeCod
190600041117
190700041117     c                   EndSr
190800041117      *------------------------------------------------------------------------*
190900041117
191000041117** CAR
191100041117!:§
191200041117** MOD  Lungh. 15
191300041117  IMMISSIONE
191400041117 MANUTENZIONE
191500041119    COPIA
191600041117VISUALIZZAZIONE
191700001010** MSG  Lungh. 78                                                            *
191800041117Utente non abilitato alla Gestione Luoghi di destinazione                     01
191900041117Immettere il Luogo                                                            02
192000041117Codice Luogo Errato o non utilizzabile                                        03
192100041117Immettere il Cliente                                                          04
192200041117Cliente non gestibile con 9999 o 8888                                         05
192300041117Cliente Inesistente o non gestibile                                           06
192400041117Immettere la ragione sociale                                                  07
192500041117Nel campo sono stati inseriti caratteri non validi                            08
192600041117Immettere l'indirizzo                                                         09
192700041117Immettere la località                                                         10
192800041117Nazione errata                                                                11
192900041117Attenzione !! Modificata Località                                             12
193000041117Attenzione !! Modificata Provincia                                            13
193100041117Attenzione !! Modificato CAP                                                  14
193200041117Attenzione !! Modificata Nazione                                              15
193300041117Immettere la Provincia                                                        16
193400041117Provincia errata                                                              17
193500070411                                                                              18
193600041118Luogo già inserito per il cliente immesso                                     19
193700070316Filiale Partenza errata                                                       20
193800070316Filiare Arrivo errata                                                         21
193900070316Inserire la filiale arrivo se immessa la filiale partenza                     22
194000070316Inserire la filiale partenza se immessa la filiale arrivo                     23
194100070316Attenzione le modifiche fatte alle personalizzazioni fil. andranno perse!!!   24
194200070316Inserire la filiale "999"                                                     25
194300060803Zona Arrivo errata                                                            26
194400070316Immettere un giro valido                                                      27
