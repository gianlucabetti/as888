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
017100151021     d xcfiva1ds     e ds
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
023900041117
024000041221      * Controllo i dati se non è visualizzazione
024100041117     c  n05              ExSr      Sr_Ctr01d
024200041117
024300041221     c                   If        *In28 or *In90
024400041221     c                   Iter
024500041217     c                   EndIf
024600041217
024700041221      * gestisco la videata delle pers. p.o. partenza
024800041221     c                   If        *Inkg
024900041221     c                   ExSr      Sr_Ges02d
025000041221     c                   Iter
025100041221     c                   EndIf
025200041117
025300041117      * F6=Conferma
025400041221     c                   If        *Inkf
025500041221     c                   ExSr      Sr_Conferma
025600041221     c                   Leave
025700041221     c                   EndIf
025800041221
025900041221e   1c                   EndDo
026000041221
026100041221     c     Fine          Tag
026200041118
026300041118     c                   If        *InKc
026400041118     c                   Eval      O81C03 = '1'
026500041118     c                   EndIf
026600041118     c                   If        *InKl
026700041118     c                   Eval      O81C12 = '1'
026800041118     c                   EndIf
026900090713     C*
027000090713     C                   clear                   tntaa1ds
027100090713     C                   movel     'C'           Itaa1Tla
027200090713     C                   movel(p)  tntaa1ds      kpjbu
027300090713     C                   CALL      'TNTAA1C'
027400090713     C                   PARM                    KPJBA
027500090713     c
027600041117
027700041117     c                   Eval      *InLr = *On
027800041117
027900041117      *------------------------------------------------------------------------*
028000041118      * VIDEATA PER RCD ALLOCATO AD ALTRO UTENTE
028100041117      *------------------------------------------------------------------------*
028200041117     c     Sr_Alloca     BegSr
028300041117
028400041118     c                   Eval      V1cCod = I81Cod
028500041118     c                   Eval      V1cCli = I81Cli
028600041117     c                   ExSr      Sr_CtrCod
028700041117     c                   Exfmt     Ta8102w
028800041117
028900041117     c                   EndSr
029000041117
029100041117      *------------------------------------------------------------------------*
029200041117      * CARICA DATI NELLA VIDEATA DAL FILE
029300041117      *------------------------------------------------------------------------*
029400041117     c     Sr_Car01d     BegSr
029500041217
029600041217     c                   Eval      *In10 = *Off
029700041221     c                   Eval      *In13 = *Off
029800041221     c                   Eval      *In14 = *Off
029900041117
030000041119if  1c                   If        Not *In01
030100041117     c                   Eval      V1cRag = SpeRag
030200041117     c                   Eval      V1cInd = SpeInd
030300041117     c                   Eval      V1cCap = SpeCap
030400041117     c                   Eval      V1cLoc = SpeLoc
030500041117     c                   Eval      V1cPro = SpePro
030600041117     c                   Eval      V1cNaz = SpeNaz
030700041117     c                   Eval      V1cTel = SpeTel
030800041117     c                   Eval      V1cFax = SpeFax
030900041117     c                   Eval      V1cPer = SpePer
031000070411     c                   movel     spepiv        w002a
031100070411     c                   if        w002a >= '00'
031200070411     c                   movel     spepiv        v1ivait
031300070411     c                   else
031400070411     c                   movel     spepiv        v1civa
031500070411     c                   endif
031600041217     c                   Eval      kSp2Tpe = 'EM'
031700041117     c     kSp2          Chain(n)  Fnsp201l
031800041119     c                   If        %Found(Fnsp201l)
031900041118     c                   Eval      V1cMail = Sp2Est
032000041119     c                   EndIf
032100041119e   1c                   EndIf
032200041119
032300041118     c                   Eval      kCod = '4L'
032400041118     c                   Eval      kKey = V1cCod
032500041118     c     kTab          Chain     Tabel00f
032600041118if  1c                   If        %Found(Tabel00f)
032700041118     c                   Eval      ds4l = TblUni
032800041118     c                   Eval      V1dCod = §4ldes
032900041118e   1c                   EndIf
033000041217
033100041221      * flag di forzatura p.o. faccio vedere il tasto funzione F7
033200041221if  1c                   If        §4lPof = 'S'
033300041221     c                   Eval      *In14 = *On
033400041221      * Controllo se utente abilitato alla gestione dei p.o. partenza
033500070410     c**!!!              If        %subst(knmus:1:3) = 'EDP'
033600090713     c                   if        otaa1flna = 'S'
033700041217     c                   Eval      *In10 = *On
033800041221     c                   EndIf
033900041221      * se non è immissione e non è copia
034000041221      * controllo se ci sono dei p.o. personalizzati
034100041221if  2c                   If        Not *In01 and Not *In03
034200041221     c                   Eval      kSp2Tpe = 'PP'
034300041221     c     kSp2          Setll     Fnsp201l
034400041221     c                   If        %Equal(Fnsp201l)
034500041221     c                   Eval      *In13 = *On
034600070702      * se ho trovato dei p.o. personalizzati devo caricare il subfile
034700070702      * così facendo non si rischia di perdere dei dati nel caso che l'utente
034800070702      * non visualizzi la seconda videata
034900070702     c                   Eval      rec = 1
035000070702      * Pulisco Subfile
035100070702     c                   ExSr      Sr_Pulsfl
035200070702      * Carico il Subfile
035300070702     c                   ExSr      Sr_Carsfl
035400041221     c                   EndIf
035500041221e   2c                   EndIF
035600041221e   1c                   EndIf
035700041118
035800041118if  1c                   If        V1cCli > *Zeros
035900041119     c                   ExSr      Sr_Anagr
036000041118e   1c                   EndIf
036100041117
036200041117     c                   EndSr
036300041119
036400041119      *------------------------------------------------------------------------*
036500041119      * RECUPERO I DATI DALL'ANAGRAFICA CLIENTI
036600041119      *------------------------------------------------------------------------*
036700041119     c     Sr_Anagr      BegSr
036800041117
036900041119     c                   Eval      savcli = V1cCli
037000041119     c                   clear                   TIBS69DS
037100041119     c                   move      V1cCli        I69kac
037200041119     c                   move      V1cCli        I69kin
037300041119     c                   call      'TIBS69R'
037400041119     c                   parm                    TIBS69DS
037500041119     c                   parm                    ds_cnaco
037600041119     c                   parm                    ds_cnind
037700041119     c                   parm                    ds_cnclp
037800041119     c                   parm                    ds_fncls
037900041119if  1c                   If        O69err <> *on
038000041119     c                   movel     ACOrag        V1dCli
038100041119     c                   movel     IndVia        V1cInd1
038200041119     c                   movel     IndCap        V1cCap1
038300041119     c                   movel     IndPrv        V1cPro1
038400041119     c                   movel     IndCit        V1cLoc1
038500041119     c                   movel     IndSta        V1cNaz1
038600041119     c                   movel     IndTlf        V1cFax1
038700041119e   1c                   EndIf
038800041119
038900041119     c                   EndSr
039000041217
039100041117      *------------------------------------------------------------------------*
039200041117      * CONTROLLI
039300041117      *------------------------------------------------------------------------*
039400041117     c     Sr_Ctr01d     BegSr
039500041119
039600041119     c                   Setoff                                       404142
039700041119     c                   Setoff                                       434445
039800041119     c                   Setoff                                       464748
039900041119     c                   Setoff                                       495051
040000041117
040100041117      * --> Codice Luogo
040200041117     c                   ExSr      Sr_CtrCod
040300041119     c   28
040400041119     cor 90              Goto      Endctr01d
040500041117
040600041117      * --> Codice Cliente
040700041119if  1c                   If        *In01 or *In03
040800041117     c                   If        V1cCli = *Zeros
040900041117     c                   Eval      *In28 = *On
041000041117     c                   Eval      *In41 = *On
041100041117     c                   Eval      V1cmsg = msg(04)
041200041117     c                   Goto      Endctr01d
041300041117     c                   EndIf
041400041119      * codice valido
041500041117     c                   Move      V1cCli        w0040
041600041118if  2c                   If        w0040 = *all'9' or
041700041117     c                             w0040 = *all'8'
041800041117     c                   Eval      *In28 = *On
041900041117     c                   Eval      *In41 = *On
042000041117     c                   Eval      V1cmsg = msg(05)
042100041117     c                   Goto      EndCtr01d
042200041118e   2c                   EndIf
042300090115      * se utente di sede e luogo 101 e cliente logistica (ntw=LOG) tutto ok
042400090115     c                   eval      $okcli = *off
042500090115     c                   if        v1ccod = '101' and *in12
042600090115     c                   movel     v1ccli        w0030
042700090115     c                   clear                   og143
042800090115     c     w0030         chain     azorg01l
042900090115if  2c                   if        %found(azorg01l) and orgfva = *blanks
043000090115     c                   eval      og143 = orgde3
043100090115     c                   endif
043200090115     c                   if        §ogntw = 'LOG'
043300090115     c                   eval      $okcli = *on
043400090115     c                   endif
043500090115     c                   endif
043600041117      * gestibile dall'utente
043700090115     c                   if        not $okcli
043800090713     c                   clear                   tntaa1ds
043900090713     c                   eval      itaa1ksc =v1ccli
044000090713     c                   eval      itaa1caut='§UTEcli'
044100090713     c                   clear                   kpjbu
044200090713     c                   movel     tntaa1ds      kpjbu
044300090713     c                   call      'TNTAA1C'
044400090713     c                   parm                    kpjba
044500090713     c                   movel     kpjbu         tntaa1ds
044600090713     c
044700090713     c                   if        otaa1fabi='N'
044800090713     c
044900090713     c***                Movel     V1cCli        w003a
045000090713     c***  w003a         Lookup    skpog                                  30
045100090713if  2c***                If        Not *In30
045200090713     c
045300041117     c                   Eval      *In41 = *On
045400041117     c                   Eval      *In28 = *On
045500041117     c                   Eval      V1cmsg = msg(06)
045600041117     c                   Goto      EndCtr01d
045700041118e   2c                   EndIf
045800090115     c                   endif
045900041117      * controllo
046000060216      * se provengo da gestione anagrafica clienti e sono in immissione
046100060216      * dell'anagrafica non devo controllare l'esitenza del cliente
046200060216     c                   if        i81gcli <> '1' and i81icli <> '1'
046300041117     c     kAco          Chain     Cnaco00f
046400041118if  2c                   If        Not %Found(Cnaco00f) or
046500041117     c                             AcoFlg <> *Blanks
046600041117     c                   Eval      *In41 = *On
046700041117     c                   Eval      *In28 = *On
046800041117     c                   Eval      V1cmsg = msg(06)
046900041117     c                   Goto      EndCtr01d
047000041118e   2c                   EndIf
047100060216     c                   endif
047200041119      * modificato codice
047300041119     c                   If        V1cCli <> savcli
047400041119     c                   ExSr      Sr_Anagr
047500041119     c                   EndIf
047600041118e   1c                   EndIf
047700041118
047800041118      * se immissione controllo che il rcd non sia già stato inserito
047900041119     c                   If        *In01 or *In03
048000041118     c     kSpe          Chain(n)  Fnspe01l
048100041118     c                   If        %Found(Fnspe01l)
048200041118     c                   Eval      *In40 = *On
048300041118     c                   Eval      *In28 = *On
048400041118     c                   Eval      V1cmsg = msg(19)
048500041118     c                   Goto      EndCtr01d
048600041118     c                   EndIf
048700041118     c                   EndIf
048800041117
048900041117      * --> Ragione Sociale
049000041117     c                   If        V1cRag = *Blanks
049100041117     c                   Eval      *In42 = *On
049200041117     c                   Eval      *In28 = *On
049300041117     c                   Eval      V1cmsg = msg(07)
049400041117     c                   Goto      EndCtr01d
049500041117     c                   EndIf
049600041117      * caratteri validi
049700041117     c                   Eval      xx = 1
049800041117     c                   Dow       xx <= 10 and
049900041117     c                             car(xx) <> *Blanks
050000041117     c     car(xx)       Scan      V1cRag                                 30
050100041117     c                   If        *In30
050200041117     c                   Eval      *In42 = *On
050300041117     c                   Eval      *In28 = *On
050400041117     c                   Eval      V1cmsg = msg(08)
050500041117     c                   Goto      EndCtr01d
050600041117     c                   EndIf
050700041117     c                   Eval      xx = xx + 1
050800041117     c                   EndDo
050900041117
051000041117      * --> Indirizzo
051100041117     c                   If        V1cInd = *Blanks
051200041117     c                   Eval      *In43 = *On
051300041117     c                   Eval      *In28 = *On
051400041117     c                   Eval      V1cmsg = msg(09)
051500041117     c                   Goto      EndCtr01d
051600041117     c                   EndIf
051700041117      * caratteri validi
051800041117     c                   Eval      xx = 1
051900041117     c                   Dow       xx <= 10 and
052000041117     c                             car(xx) <> *Blanks
052100041117     c     car(xx)       Scan      V1cInd                                 30
052200041117     c                   If        *In30
052300041117     c                   Eval      *In43 = *On
052400041117     c                   Eval      *In28 = *On
052500041117     c                   Eval      V1cmsg = msg(08)
052600041117     c                   Goto      EndCtr01d
052700041117     c                   EndIf
052800041117     c                   Eval      xx = xx + 1
052900041117     c                   EndDo
053000041117
053100041117      * --> Località
053200041117      * obbligatoria
053300041117     c                   If        V1cLoc = *Blanks
053400041117     c                   Eval      *In45 = *On
053500041117     c                   Eval      *In28 = *On
053600041117     c                   Eval      V1cmsg = msg(10)
053700041117     c                   Goto      EndCtr01d
053800041117     c                   EndIf
053900041117      * caratteri validi
054000041117     c                   Eval      xx = 1
054100041117     c                   Dow       xx <= 10 and
054200041117     c                             car(xx) <> *Blanks
054300041117     c     car(xx)       Scan      V1cLoc                                 30
054400041117     c                   If        *In30
054500041117     c                   Eval      *In45 = *On
054600041117     c                   Eval      *In28 = *On
054700041117     c                   Eval      V1cmsg = msg(08)
054800041117     c                   Goto      EndCtr01d
054900041117     c                   EndIf
055000041117     c                   Eval      xx = xx + 1
055100041117     c                   EndDo
055200041117
055300041117      * --> Nazione
055400041119      * ricerca
055500041119     c     '?'           Scan      V1cNaz                                 30
055600041119if  1c                   If        *In30
055700041119     c                   Eval      kCod = '15'
055800041119     c                   Clear                   kKey
055900041119     c                   Clear                   wDes
056000041119     c                   Call      'X§TABER'
056100041119     c                   Parm                    Codut
056200041119     c                   Parm                    kCod
056300041119     c                   Parm                    kKey
056400041119     c                   Parm                    wDes
056500041119     c                   Eval      *In47 = *On
056600041119     c                   Eval      *In90 = *On
056700041119     c                   Eval      V1cNaz = kKey
056800041119     c                   Goto      EndCtr01d
056900041119e   1c                   EndIf
057000041119      * controllo
057100041117     c                   Eval      kCod = '15'
057200041117     c                   Eval      kKey = V1cNaz
057300041117     c     kTab          Chain     Tabel00f
057400041117if  1c                   If        Not %Found(Tabel00f) or
057500041117     c                             TblFlg <> *Blanks
057600041117     c                   Eval      *In47 = *On
057700041117     c                   Eval      *In28 = *On
057800041117     c                   Eval      V1cmsg = msg(11)
057900041117     c                   Goto      EndCtr01d
058000041117e   1c                   EndIf
058100041117     c                   Eval      ds15 = TblUni
058200041117
058300041117      * Ricerca località
058400041117if  1c                   If        §15cpp = *Blanks and
058500041117     c                             %subst(V1cLoc:1:1) = '?'
058600041119     c                   Move      V1cLoc        w034a
058700041117      * se immesso solo '?' inizio la ricerca da 'A'
058800041119     c                   If        w034a = *Blanks
058900041119     c                   Eval      w034a = 'A'
059000041117     c                   EndIf
059100041117     c                   Eval      *In90 = *On
059200041117     c                   Eval      *In45 = *On
059300041117      * ricerca
059400041117     c                   Clear                   Tisi00ds
059500041117     c                   Eval      D00Op0 = 'L01'
059600041119     c                   Eval      D00Loc = w034a
059700041117     c                   Z-add     *date         D00Dri
059800041117     c                   Call      'TISI30R'
059900041117     c                   Parm                    Kpjba
060000041117     c                   Parm                    Tisi00ds
060100041117      * se selezione
060200041117if  2c                   If        D00Cap <> *Blanks
060300041117     c                   Eval      V1cCap = D00Cap
060400041117     c                   Eval      V1cLoc = D00Loc
060500041117     c                   Eval      V1cNaz = D00Nar
060600041117     c                   Eval      V1cPro = D00Prv
060700041117   x2c                   Else
060800041117     c                   Clear                   V1cLoc
060900041117e   2c                   EndIf
061000041117     c                   Goto      Endctr01d
061100041117e   1c                   EndIf
061200041117
061300041117      * --> Cap
061400041117      * controllo solo se nazione con cappario
061500041117     c                   If        §15cpp = *Blanks
061600041117     c                   ExSr      Sr_CtrCap
061700041117     c   28              Goto      Endctr01d
061800041117     c                   EndIf
061900041117
062000041117      * --> Provincia
062100041117      * obbligatoria per nazione italia
062200041117     c                   If        V1cPro = *Blanks and
062300041117     c                             V1cNaz = *Blanks
062400041117     c                   Eval      *In46 = *On
062500041117     c                   Eval      *In28 = *On
062600041117     c                   Eval      V1cmsg = msg(16)
062700041117     c                   Goto      EndCtr01d
062800041117     c                   EndIf
062900041117      * controllo
063000041207     c                   If        V1cPro <> *Blanks
063100041117     c                   Eval      kCod = 'PR'
063200041117     c                   Eval      kKey = V1cPro
063300041117     c     kTab          Chain     Tabel00f
063400041117if  1c                   If        Not %Found(Tabel00f) or
063500041117     c                             TblFlg <> *Blanks
063600041117     c                   Eval      *In46 = *On
063700041117     c                   Eval      *In28 = *On
063800041117     c                   Eval      V1cmsg = msg(17)
063900041117     c                   Goto      EndCtr01d
064000041117e   1c                   EndIf
064100041207     c                   EndIf
064200041117      * caratteri validi
064300041117     c                   Eval      xx = 1
064400041117     c                   Dow       xx <= 10 and
064500041117     c                             car(xx) <> *Blanks
064600041117     c     car(xx)       Scan      V1cPro                                 30
064700041117     c                   If        *In30
064800041117     c                   Eval      *In46 = *On
064900041117     c                   Eval      *In28 = *On
065000041117     c                   Eval      V1cmsg = msg(08)
065100041117     c                   Goto      EndCtr01d
065200041117     c                   EndIf
065300041117     c                   Eval      xx = xx + 1
065400041117     c                   EndDo
065500041117
065600041117      * --> FAX
065700041117     c                   Eval      wfax  = *Off
065800041117      * se il luogo è x invio comunicazioni di giacenza controllo se
065900041117      * fax obbligatorio
066000041117if  1c                   If        §4lgia = 'S'
066100041117     c     V1cCli        Chain     Fncls01l
066200041117if  2c                   If        %Found(Fncls01l)
066300041119     c                   Eval      wclsflo = ClsFlo
066400041117     c                   Eval      xx = 1
066500041117     c     wapert        Lookup    c2f(xx)                                30
066600041117if  3c                   If        *In30 and t2f(xx) = 'F'
066700041117     c                   Eval      wfax = *On
066800041117   x3c                   Else
066900041117     c                   Eval      xx = 1
067000041117     c     wchius        Lookup    c2h(xx)                                30
067100041117if  4c                   If        *In30 and t2h(xx) = 'F'
067200041117     c                   Eval      wfax = *On
067300041117e   4c                   EndIf
067400041117e   3c                   EndIf
067500041117e   2c                   EndIf
067600041117e   1c                   EndIf
067700041117      * controllo
067800041117if  1c                   If        V1cFax <> *Blanks or
067900041117     c                             §4lfax = 'S' or wfax = *On
068000041117     c                   Clear                   Trul42ds
068100041117     c                   Eval      D42Fax = V1cFax
068200041117     c                   Call      'TRUL42R'
068300041117     c                   Parm                    Trul42ds
068400041117     c                   If        D42Err = '1'
068500041117     c                   Eval      *In49 = *On
068600041117     c                   Eval      *In28 = *On
068700041117     c                   Eval      V1cmsg = D42Msg
068800041117     c                   Goto      EndCtr01d
068900041117     c                   EndIf
069000041117e   1c                   EndIf
069100041118
069200041117      * --> Telefono
069300041117if  1c                   If        V1cTel <> *Blanks
069400041117     c                   Clear                   Trul42ds
069500041117     c                   Eval      D42Fax = V1cTel
069600041117     c                   Call      'TRUL42R'
069700041117     c                   Parm                    Trul42ds
069800041117     c                   If        D42Err = '1'
069900041117     c                   Eval      *In48 = *On
070000041117     c                   Eval      *In28 = *On
070100041117     c                   Eval      V1cmsg = D42Msg
070200041117     c                   Goto      EndCtr01d
070300041117     c                   EndIf
070400041117e   1c                   EndIf
070500041119
070600041119      * --> Partita IVA
070700070411     c                   if        v1civa <> *blanks
070800041119     c                   ExSr      Sr_CtrIva
070900041119     c   28              Goto      Endctr01d
071000041119     c                   EndIf
071100041118
071200041118      * --> e-mail
071300041118     c                   If        V1cMail <> *Blanks
071400041118     c                   Clear                   dsemail
071500041118     c                   Eval      §emlindi = V1cMail
071600041118     c                   Call      'XEMAIL'
071700041118     c                   Parm                    dsemail
071800041118     c                   If        §emlerro = '1'
071900041118     c                   Eval      *In28 = *On
072000041118     c                   Eval      *In51 = *On
072100041118     c                   Eval      V1cMsg = §emlmsgo
072200041118     c                   Goto      EndCtr01d
072300041118     c                   EndIf
072400041118     c                   Eval      V1cMail = §emlindo
072500041118     c                   EndIf
072600041117
072700041117     c     Endctr01d     EndSr
072800041117
072900041117      *------------------------------------------------------------------------*
073000041117      * CONTROLLO IL CODICE LUOGO
073100041117      *------------------------------------------------------------------------*
073200041117     c     Sr_CtrCod     BegSr
073300041117
073400041117      * Obbligatorio
073500041117     c                   If        V1cCod = *Blanks
073600041117     c                   Eval      *In28 = *On
073700041117     c                   Eval      *In40 = *On
073800041117     c                   Eval      V1cmsg = msg(02)
073900041117     c                   Goto      EndCtrcod
074000041117     c                   EndIf
074100041119
074200041119      * ricerca
074300041119     c     '?'           Scan      V1cCod                                 30
074400041119if  1c                   If        *In30
074500041119     c                   Eval      kCod = '4L'
074600041119     c                   Clear                   kKey
074700041119     c                   Clear                   wDes
074800041119     c                   Call      'X§TABER'
074900041119     c                   Parm                    Codut
075000041119     c                   Parm                    kCod
075100041119     c                   Parm                    kKey
075200041119     c                   Parm                    wDes
075300041119     c                   Eval      *In40 = *On
075400041119     c                   Eval      *In90 = *On
075500041119     c                   Eval      V1cCod = kKey
075600041119     c                   Goto      EndCtrCod
075700041119e   1c                   EndIf
075800041117
075900041117     c                   Eval      kCod = '4L'
076000041117     c                   Eval      kKey = V1cCod
076100041117     c     kTab          Chain     Tabel00f
076200041117if  1c                   If        Not %Found(Tabel00f) or
076300041117     c                             TblFlg <> *Blanks
076400041117     c                   Eval      *In40 = *On
076500041117     c                   Eval      *In28 = *On
076600041117     c                   Eval      V1cmsg = msg(03)
076700041117     c                   Goto      EndCtrcod
076800041117e   1c                   EndIf
076900041117     c                   Eval      ds4l = TblUni
077000071128if  1c                   If        Not *In12 and §4lufi <> 'S'
077100071128     c**!!!                        (*In01 or *In03)
077200041117     c                   Eval      *In40 = *On
077300041117     c                   Eval      *In28 = *On
077400041117     c                   Eval      V1cmsg = msg(03)
077500041117     c                   Goto      EndCtrcod
077600041117e   1c                   EndIf
077700041117      * decodifico
077800041117     c                   Eval      V1dCod = §4ldes
077900041117
078000041117     c     EndCtrcod     EndSr
078100041117
078200041117      *------------------------------------------------------------------------*
078300041117      * CONTROLLO ESISTENZA CAP
078400041117      *------------------------------------------------------------------------*
078500041117     c     Sr_CtrCap     BegSr
078600041117
078700041117      * controllo il cap
078800041117     c                   Clear                   Tisi95ds
078900041117     c                   Clear                   Fnlv13ds
079000041117     c                   Eval      I95Tcn = '7'
079100041117     c                   Movel     V1cCap        I95Cap
079200041117     c                   Movel     V1cNaz        I95Nar
079300041117     c                   Movel     V1cInd        I95Ind
079400041117     c                   Movel     V1cLoc        I95Loc
079500041117     c                   Movel     V1cPro        I95Prv
079600041117     c                   Movel     'S'           I13Af0
079700041117     c                   Movel     'S'           I13Af1
079800041117     c                   Movel     'S'           I13Sz2
079900041117     c                   Movel     'S'           I13La3
080000041117     c                   Movel     'S'           I13Sz3
080100041117     c                   Movel     'S'           I13Cnv
080200041117     c                   Movel     'S'           I13Szv
080300041117     c   12              Z-Add     46            I95Tfp
080400041117     c  n12              Z-Add     Simfel        I95Tfp
080500041117     c                   Movel     'S'           I95Fre
080600041117
080700041117      * Gestione errori forzabili: se cambiato CAP/Località
080800041117      * riazzero i valori di errori forzabili
080900041117if  1c                   If        V1cCap <> wsvcap
081000041117     c                   Clear                   wfzv
081100041117     c                   Clear                   wfz1
081200041117     c                   Clear                   wfz2
081300041117     c                   Clear                   wfz3
081400041117     c                   Eval      wsvcap = V1cCap
081500041117e   1c                   EndIf
081600041117      * Modifica località
081700041117if  1c                   If        V1cLoc <> wsvloc
081800041117     c                   Clear                   wfz1
081900041117     c                   Clear                   wfz2
082000041117     c                   Clear                   wfz3
082100041117     c                   Eval      wsvloc = V1cLoc
082200041117e   1c                   EndIf
082300041117      * Modifica provincia
082400041117if  1c                   If        V1cPro <> wsvpro
082500041117     c                   Clear                   wfz3
082600041117     c                   Eval      wsvpro = V1cPro
082700041117e   1c                   EndIf
082800041117
082900041117     c                   Movel     wfzv          E13Fzv
083000041117     c                   Movel     wfz1          E13Fz1
083100041117     c                   Movel     wfz2          E13Fz2
083200041117     c                   Movel     wfz3          E13Fz3
083300041117     c                   Call      'FNLV13R'
083400041117     c                   Parm                    Kpjba
083500041117     c                   Parm                    Fnlv13ds
083600041117     c                   Parm                    Tisi95ds
083700041117
083800041117      * Ricontrollo errori
083900041117     c                   Movel     O13Err        werr
084000041117     c                   Movel     O13Msg        wmsg
084100041117     c                   Movel     E13Fzv        wfzv
084200041117     c                   Movel     E13Fz1        wfz1
084300041117     c                   Movel     E13Fz2        wfz2
084400041117     c                   Movel     E13Fz3        wfz3
084500041117
084600041117      * Se il livello di affidabilità non è inferiore a 2 e
084700041117      * il livello di reperimento dati non è inferiore a 3
084800041117      * reimposto i dati da ds TISI95
084900041117if  1c                   If        O13Ron = 'S' or
085000041117     c                             O13Roc = 'S' or
085100041117     c                             O13Rop = 'S' or
085200041117     c                             O13Rol = 'S'
085300041117     c                   Movel     O95Loc        V1cLoc
085400041117     c                   Movel     O95Prv        V1cPro
085500041117     c                   Movel     O95Nar        V1cNaz
085600041117     c                   Movel     O95Cap        V1cCap
085700041117e   1c                   EndIf
085800041117
085900041117     c                   ExSr      Sr_CtrInd
086000041117      * Se non ci sono errori proseguo
086100041117     c   28              Goto      EndCtrcap
086200041117
086300041117      * Controllo se reimpostata località
086400041117     c                   If        O13Rol = 'S'
086500041117     c                   Eval      *In28 = *On
086600041117     c                   Eval      *In45 = *On
086700041117     c                   Eval      V1cMsg = msg(12)
086800041117     c                   Goto      EndCtrcap
086900041117     c                   EndIf
087000041117      * Controllo se reimpostata Provincia
087100041117     c                   If        O13Rop = 'S'
087200041117     c                   Eval      *In28 = *On
087300041117     c                   Eval      *In46 = *On
087400041117     c                   Eval      V1cMsg = msg(13)
087500041117     c                   Goto      EndCtrcap
087600041117     c                   EndIf
087700041117      * Controllo se reimpostato CAP
087800041117     c                   If        O13Roc = 'S'
087900041117     c                   Eval      *In28 = *On
088000041117     c                   Eval      *In44 = *On
088100041117     c                   Eval      V1cMsg = msg(14)
088200041117     c                   Goto      EndCtrcap
088300041117     c                   EndIf
088400041117      * Controllo se reimpostata Nazione
088500041117     c                   If        O13Ron = 'S'
088600041117     c                   Eval      *In28 = *On
088700041117     c                   Eval      *In47 = *On
088800041117     c                   Eval      V1cMsg = msg(15)
088900041117     c                   Goto      EndCtrcap
089000041117     c                   EndIf
089100041117
089200041117     c     EndCtrcap     EndSr
089300041117
089400041117      *------------------------------------------------------------------------*
089500041117      *  CONTROLLO INDIRIZZO
089600041117      *------------------------------------------------------------------------*
089700041117     c     Sr_CtrInd     BegSr
089800041117
089900041118      * Errore
090000041117if  1c                   If        werr <> *Blanks and
090100041117     c                             wmsg <> *Blanks
090200041117s   2c                   Select
090300041117      * Errore su località/città
090400041117w   2c                   When      werr = '3'
090500041117     c                   Eval      *In28 = *On
090600041117     c                   Eval      *In45 = *On
090700041117      * Errore su Cap
090800041117w   2c                   When      werr = '5'
090900041117     c                   Eval      *In28 = *On
091000041117     c                   Eval      *In44 = *On
091100041117      * Errore su provincia
091200041117w   2c                   When      werr ='4'
091300041117     c                   Eval      *In28 = *On
091400041117     c                   Eval      *In46 = *On
091500041117      * Nazione  errata
091600041117w   2c                   When      werr = '6'
091700041117     c                   Eval      *In28 = *On
091800041117     c                   Eval      *In47 = *On
091900041117e   2c                   EndSl
092000041117     c                   Eval      V1cmsg = wmsg
092100041117e   1c                   EndIf
092200041117
092300041117     c                   EndSr
092400041117
092500041117      *------------------------------------------------------------------------*
092600041117      * CONTROLLO PARTITA IVA
092700041117      *------------------------------------------------------------------------*
092800041117     c     Sr_CtrIva     BegSr
092900041117
093000151021     c                   clear                   xcfiva1ds
093100070411     c                   eval      xcfivamod = 'P'
093200070411     c                   eval      xcfivapar = v1civa
093300070411     c                   eval      xcfivanar = v1cnaz
093400070411     c                   eval      xcfivaprv = v1cpro
093500151021     c                   eval      xcfivaloc = v1cloc
093600151021     c                   eval      xcfivacap = v1ccap
093700151021     c                   call      'XCFIVAR1'
093800151021     c                   parm                    xcfiva1ds
093900070411      * --> errata forzabile
094000070411     c                   If        xcfivaflg = -9 and wforzaiva = *off
094100070411     c                   Eval      *In28 = *On
094200070411     c                   Eval      *In50 = *On
094300070411     c                   Eval      v1cmsg = xcfivamsg
094400070411     c                   Eval      v1cmsg = %trim(v1cmsg) + ' Enter x forzare'
094500070411     c                   eval      wforzaiva = *on
094600070411     c                   Leavesr
094700070411     c                   EndIf
094800070411      * --> errore
094900070411     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
095000070411     c                   Eval      *In28 = *On
095100070411     c                   Eval      *In50 = *On
095200070411     c                   eval      v1cmsg = xcfivamsg
095300070411     c                   Leavesr
095400070411     c                   EndIf
095500070411      * se non impostato devo riportare il codice iso della partita iva che
095600070411      * mi viene passato dal pgm di controllo
095700070411     c                   if        v1ivaeu <> ivaeu
095800070411     c                   eval      v1ivaeu = ivaeu
095900070411     c                   eval      *in39 = *on
096000070411     c                   eval      *in90 = *on
096100070411     c                   endif
096200041117
096300041117     c                   EndSr
096400041217
096500041217      *------------------------------------------------------------------------*
096600041221      * GESTICO LE PERSONALIZZAZIONI P.O. PARTENZA
096700041217      *------------------------------------------------------------------------*
096800041217     c     Sr_Ges02d     BegSr
096900041217
097000041217     c                   Eval      rec = 1
097100041221
097200041221      * Pulisco Subfile
097300041221     c                   ExSr      Sr_Pulsfl
097400041221      * Carico il Subfile
097500041221     c                   ExSr      Sr_Carsfl
097600041221
097700041221     c                   Eval      wf7 = *On
097800041221
097900041221do  1c                   Do        *Hival
098000041221
098100041221      * Emetto il Subfile
098200041221     c                   ExSr      Sr_Emisfl
098300041217
098400041221      * F12 - Torno alla videata precedente
098500041221     c                   If        *Inkl and Not *In28
098600041221     c                   Leave
098700041221     c                   EndIf
098800041217
098900041221      * Gestione Subfile
099000041221     c                   If        nrr > *Zeros  and Not *In05 and
099100041221     c                             *In10
099200041221     c                   ExSr      Sr_Gessfl
099300041221     c                   EndIf
099400041221
099500041221      * Sistemo le sk dei p.o.
099600041217     c                   do        20            nrr
099700041217     c     nrr           Chain     ta8103s                            30
099800041221     c                   If        Not *In30
099900041221     c                   Move      v3scpp        skpp(nrr)
100000041221     c                   Move      v3scpa        skpa(nrr)
100100060803     c                   Move      v3scpz        skpz(nrr)
100200070316      * i giri sono divisi su 4 sk
100300070316     c                   select
100400070316     c                   when      nrr <= 6
100500070316     c                   eval      skpg1(nrr) = v3scgi
100600070316     c                   when      nrr > 6 and nrr <= 12
100700070316     c                   eval      zz = nrr - 6
100800070316     c                   eval      skpg2(zz) = v3scgi
100900070316     c                   when      nrr > 12 and nrr <= 18
101000070316     c                   eval      zz = nrr - 12
101100070316     c                   eval      skpg3(zz) = v3scgi
101200070316     c                   other
101300070316     c                   eval      zz = nrr - 18
101400070316     c                   eval      skpg4(zz) = v3scgi
101500070316     c                   endsl
101600041217     c                   endif
101700041217     c                   enddo
101800041217
101900060405     c                   movea     *zeros        wskpp
102000060405     c                   movea     *zeros        wskpa
102100060803     c                   movea     *zeros        wskpz
102200070316     c                   clear                   wskpg1
102300070316     c                   clear                   wskpg2
102400070316     c                   clear                   wskpg3
102500070316     c                   clear                   wskpg4
102600041221     c                   Clear                   yy
102700041221     c                   do        20            xx
102800041221     c                   If        skpp(xx) > *Zeros
102900041221     c                   add       1             yy
103000041221     c                   eval      wskpp(yy) = skpp(xx)
103100041221     c                   eval      wskpa(yy) = skpa(xx)
103200060803     c                   eval      wskpz(yy) = skpz(xx)
103300070316      * i giri sono divisi su 4 sk
103400070316     c                   select
103500070316     c                   when      yy <= 6
103600070316     c                   eval      wskpg1(yy) = skpg1(xx)
103700070316     c                   when      yy > 6 and yy <= 12
103800070316     c                   eval      zz = yy - 6
103900070316     c                   eval      ww = xx - 6
104000070316     c                   eval      wskpg2(zz) = skpg2(ww)
104100070316     c                   when      yy > 12 and yy <= 18
104200070316     c                   eval      zz = yy - 12
104300070316     c                   eval      ww = xx - 12
104400070316     c                   eval      wskpg3(zz) = skpg3(ww)
104500070316     c                   other
104600070316     c                   eval      zz = yy - 18
104700070316     c                   eval      ww = xx - 18
104800070316     c                   eval      wskpg4(zz) = skpg4(ww)
104900070316     c                   endsl
105000041221     c                   EndIf
105100041221     c                   EndDo
105200041221
105300041221     c                   movea     wskpp         wpp
105400041221     c                   movea     wskpa         wpa
105500060803     c                   movea     wskpz         wpz
105600070316     c                   movea     wskpg1        wpg1
105700070316     c                   movea     wskpg2        wpg2
105800070316     c                   movea     wskpg3        wpg3
105900070316     c                   movea     wskpg4        wpg4
106000041217
106100041221e   1c                   EndDo
106200041217
106300041217     c                   EndSr
106400041221
106500041221      *------------------------------------------------------------------------*
106600041221      * PULISCO IL SUBFILE
106700041221      *------------------------------------------------------------------------*
106800041221     c     Sr_Pulsfl     BegSr
106900041221
107000041221     c                   Clear                   nrr
107100041221     c                   Eval      *In21 = *Off
107200041221     c                   Write     ta8103c
107300041221     c                   Eval      *In20 = *On
107400041221     c                   Eval      *In21 = *On
107500041221
107600041221     c                   EndSr
107700041221
107800041221      *------------------------------------------------------------------------*
107900041221      * CARICO IL SUBFILE
108000041221      *------------------------------------------------------------------------*
108100041221     c     Sr_Carsfl     BegSr
108200041221
108300041221      * Se non è immissione o copia e se non ho mai caricato i dati nel
108400041221      * subfile carico i p.o. dal file
108500041221if  1c                   If        Not *In01 and Not *In03 and wf7 = *Off
108600041221     c                   Eval      kSp2Tpe = 'PP'
108700041221     c     kSp2          Chain(n)  Fnsp201l
108800041221     c                   If        %Found(Fnsp201l)
108900041221     c                   Movel     Sp2Est        wpp
109000041221     c                   EndIf
109100041221     c                   Eval      kSp2Tpe = 'PA'
109200041221     c     kSp2          Chain(n)  Fnsp201l
109300041221     c                   If        %Found(Fnsp201l)
109400041221     c                   Movel     Sp2Est        wpa
109500041221     c                   EndIf
109600060803     c                   Eval      kSp2Tpe = 'PZ'
109700060803     c     kSp2          Chain(n)  Fnsp201l
109800060803     c                   If        %Found(Fnsp201l)
109900060803     c                   Movel     Sp2Est        wpz
110000060803     c                   EndIf
110100070316      * carico anche i giri divisi in 4 tipi record
110200070316     c                   eval      ksp2tpe = 'G1'
110300070316     c     ksp2          chain(n)  fnsp201l
110400070316     c                   if        %found(fnsp201l)
110500070316     c                   movel     sp2est        wpg1
110600070316     c                   endif
110700070316     c                   eval      ksp2tpe = 'G2'
110800070316     c     ksp2          chain(n)  fnsp201l
110900070316     c                   if        %found(fnsp201l)
111000070316     c                   movel     sp2est        wpg2
111100070316     c                   endif
111200070316     c                   eval      ksp2tpe = 'G3'
111300070316     c     ksp2          chain(n)  fnsp201l
111400070316     c                   if        %found(fnsp201l)
111500070316     c                   movel     sp2est        wpg3
111600070316     c                   endif
111700070316     c                   eval      ksp2tpe = 'G4'
111800070316     c     ksp2          chain(n)  fnsp201l
111900070316     c                   if        %found(fnsp201l)
112000070316     c                   movel     sp2est        wpg4
112100070316     c                   endif
112200041221
112300041221     c                   Movea     wpa           skpa
112400041221     c                   Movea     wpp           skpp
112500060803     c                   Movea     wpz           skpz
112600070316     c                   movea     wpg1          skpg1
112700070316     c                   movea     wpg2          skpg2
112800070316     c                   movea     wpg3          skpg3
112900070316     c                   movea     wpg4          skpg4
113000041221e   1c                   EndIf
113100041221
113200041221      * Se ho già caricato almeno una volta il sbfl devo riportare i dati
113300041221      * modificati in precedenza
113400041221if  1c                   If        wf7 = *On
113500041221     c                   Movea     wpa           skpa
113600041221     c                   Movea     wpp           skpp
113700060803     c                   Movea     wpz           skpz
113800070316     c                   movea     wpg1          skpg1
113900070316     c                   movea     wpg2          skpg2
114000070316     c                   movea     wpg3          skpg3
114100070316     c                   movea     wpg4          skpg4
114200041221e   1c                   EndIf
114300041221
114400041221do  1c                   Do        20            xx
114500041221     c                   Clear                   v3scpp
114600041221     c                   Clear                   v3sdpp
114700041221     c                   Clear                   v3scpa
114800041221     c                   Clear                   v3sdpa
114900060803     c                   Clear                   v3scpz
115000070316     c                   clear                   v3scgi
115100070316     c                   clear                   v3sdcgi
115200041221     c                   If        skpp(xx) > *Zeros
115300041221     c                   Move      skpp(xx)      v3scpp
115400041221     c     v3scpp        Chain     Azorg01l
115500041221     c                   If        %Found(Azorg01l)
115600041221     c                   Eval      v3sdpp = OrgDes
115700041221     c                   EndIf
115800041221     c                   If        v3scpp = 999
115900041221     c                   Eval      v3sdpp = 'Tutte le altre'
116000041221     c                   EndIf
116100041221     c                   EndIf
116200041221     c                   If        skpa(xx) > *Zeros
116300041221     c                   Move      skpa(xx)      v3scpa
116400060803     c                   Move      skpz(xx)      v3scpz
116500041221     c     v3scpa        Chain     Azorg01l
116600041221     c                   If        %Found(Azorg01l)
116700041221     c                   Eval      v3sdpa = OrgDes
116800041221     c                   EndIf
116900041221     c                   If        v3scpa = 999
117000041221     c                   Eval      v3sdpa = 'Tutte le altre'
117100041221     c                   EndIf
117200041221     c                   EndIf
117300070316      * giri divisi su 4 sk
117400070316     c                   select
117500070316     c                   when      xx <= 6
117600070316     c                   if        skpg1(xx) > *blanks
117700070316     c                   move      skpg1(xx)     v3scgi
117800070316     c                   exsr      sr_ctrgiro
117900070316     c                   eval      v3sdcgi = d09odes
118000070316     c                   endif
118100070316     c                   when      xx > 6 and xx <= 12
118200070316     c                   eval      zz = xx - 6
118300070316     c                   if        skpg2(zz) > *blanks
118400070316     c                   move      skpg2(zz)     v3scgi
118500070316     c                   exsr      sr_ctrgiro
118600070316     c                   eval      v3sdcgi = d09odes
118700070316     c                   endif
118800070316     c                   when      xx > 12 and xx <= 18
118900070316     c                   eval      zz = xx - 12
119000070316     c                   if        skpg3(zz) > *blanks
119100070316     c                   move      skpg3(zz)     v3scgi
119200070316     c                   exsr      sr_ctrgiro
119300070316     c                   eval      v3sdcgi = d09odes
119400070316     c                   endif
119500070316     c                   other
119600070316     c                   eval      zz = xx - 18
119700070316     c                   if        skpg4(zz) > *blanks
119800070316     c                   move      skpg4(zz)     v3scgi
119900070316     c                   exsr      sr_ctrgiro
120000070316     c                   eval      v3sdcgi = d09odes
120100070316     c                   endif
120200070316     c                   endsl
120300070316
120400041221     c                   Add       1             Nrr
120500041221     c                   Write     ta8103s
120600041221e   1c                   EndDo
120700041221
120800041221     c                   EndSr
120900041221
121000041221      *------------------------------------------------------------------------*
121100041221      * EMETTO IL SUBFILE
121200041221      *------------------------------------------------------------------------*
121300041221     c     Sr_Emisfl     BegSr
121400041221
121500041221     c                   If        nrr > rec
121600041221     c                   Eval      rec = 1
121700041221     c                   EndIf
121800041221
121900041221     c                   Write     ta8103z
122000041221     c                   Exfmt     ta8103c
122100041221
122200041221     c                   EndSr
122300041221
122400041221      *------------------------------------------------------------------------*
122500041221      * GESTIONE SUBFILE
122600041221      *------------------------------------------------------------------------*
122700041221     c     Sr_Gessfl     BegSr
122800041221
122900041221     c                   Clear                   nrr
123000041221
123100041221     c                   Reset                   wforza
123200041221     c                   Eval      wpers = *On
123300041221
123400041221do  1c                   Do        *Hival
123500041221     c                   Eval      nrr = nrr +1
123600041221     c     nrr           Chain     ta8103s                            30
123700041221     c                   If        *In30
123800041221     c                   Leave
123900041221     c                   EndIf
124000041221
124100041221     c                   Eval      *In28 = *Off
124200041221     c                   Eval      *In52 = *Off
124300041221     c                   Eval      *In53 = *Off
124400060803     c                   Eval      *In54 = *Off
124500070316     c                   eval      *in55 = *off
124600041221
124700041221     c                   Clear                   v3sdpp
124800041221      * Controllo p.o. partenza
124900041221if  2c                   If        v3scpp > *Zeros
125000041221if  3c                   If        v3scpp <> 999
125100041221     c     v3scpp        Chain     Azorg01l
125200041221     c                   If        Not %Found(Azorg01l) or
125300041221     c                             OrgFva <> *Blanks or
125400041221     c                             (OrgFag <> 'A' and OrgFag <> 'F')
125500041221     c                   Eval      *In28 = *On
125600041221     c                   Eval      *In52 = *On
125700041221     c                   Eval      v1cmsg = msg(20)
125800041221     c                   Z-add     nrr           rec
125900041221     c                   Update    ta8103s
126000041221     c                   Leave
126100041221     c                   EndIf
126200041221     c                   Eval      v3sdpp = OrgDes
126300041221   x3c                   Else
126400041221     c                   Eval      v3sdpp = 'Tutte le altre'
126500041221e   3c                   EndIf
126600041221    2c                   EndIf
126700041221
126800041221     c                   Clear                   v3sdpa
126900041221      * Controllo p.o. arrivo
127000041221if  2c                   If        v3scpa > *Zeros
127100041221if  3c                   If        v3scpa <> 999
127200041221     c     v3scpa        Chain     Azorg01l
127300041221     c                   If        Not %Found(Azorg01l) or
127400041221     c                             OrgFva <> *Blanks or
127500041221     c                             (OrgFag <> 'A' and OrgFag <> 'F')
127600041221     c                   Eval      *In28 = *On
127700041221     c                   Eval      *In53 = *On
127800041221     c                   Eval      v1cmsg = msg(21)
127900041221     c                   Z-add     nrr           rec
128000041221     c                   Update    ta8103s
128100041221     c                   Leave
128200041221     c                   EndIf
128300041221     c                   Eval      v3sdpa = OrgDes
128400041221   x3c                   Else
128500041221     c                   Eval      v3sdpa = 'Tutte le altre'
128600041221e   3c                   EndIf
128700041221    2c                   EndIf
128800041221
128900060803      * Controllo Zona arrivo
129000060803if  2c                   If        v3scpz > *Zeros
129100060803     c                   movel     v3scpa        wkey5
129200060803     c                   move      v3scpz        wkey5
129300060803     c                   movel(p)  wkey5         kkey
129400060803     c                   move      '03'          kcod
129500060803     c     ktab          Chain     tabel00f
129600060803     c                   If        Not %Found(tabel00f) or
129700060803     c                             TblFlg <> *Blanks
129800060803     c                   Eval      *In28 = *On
129900060803     c                   Eval      *In54 = *On
130000060803     c                   Eval      v1cmsg = msg(26)
130100060803     c                   Z-add     nrr           rec
130200060803     c                   Update    ta8103s
130300060803     c                   Leave
130400060803     c                   EndIf
130500060803    2c                   EndIf
130600070316
130700070316      * giri divisi su 4 sk
130800070316     c                   clear                   v3sdcgi
130900070316      * interrogazione controllo giro
131000070316if  2c                   if        v3scgi <> *blanks
131100070316      * interrogazione
131200070316if  3c                   if        %scan('?':v3scgi) > *zeros
131300070316     c                   clear                   fidg09ds
131400070316     c                   eval      d09iop0 = 'P01'
131500070316     c                   eval      d09ifgs = v3scpa
131600070316     c                   eval      d09idat = *date
131700070709     c                   eval      d09itug = 'C'
131800070316     c                   eval      kpjbu = fidg09ds
131900070316     c                   call      'FIDG09R'
132000070316     c                   parm                    kpjba
132100070316     c                   eval      fidg09ds = kpjbu
132200070316    4c                   if        d09oerr = '1'
132300070316     c                   eval      *in28 = *on
132400070316     c                   eval      *in55 = *on
132500070316     c                   eval      v1cmsg = msg(27)
132600070316     c                   eval      rec = nrr
132700070316     c                   update    ta8103s
132800070316     c                   leave
132900070316    4c                   endif
133000070316    4c                   if        d09ocgi <> ' '
133100070316     c                   eval      v3scgi = d09ocgi
133200070316     c                   eval      v3sdcgi = d09odes
133300070316    4c                   endif
133400070316     c                   eval      rec = nrr
133500070316     c                   update    ta8103s
133600070316     c                   leave
133700070316    3c                   endif
133800070316      * controllo
133900070316     c                   exsr      sr_ctrgiro
134000070316    3c                   if        d09oerr = '1'
134100070316     c                   eval      *in28 = *on
134200070316     c                   eval      *in55 = *on
134300070316     c                   eval      v1cmsg = msg(27)
134400070316     c                   eval      rec = nrr
134500070316     c                   update    ta8103s
134600070316     c                   leave
134700070316    3c                   endif
134800070316    3c                   if        d09ocgi <> ' '
134900070316     c                   eval      v3sdcgi = d09odes
135000070316    3c                   endif
135100070316    2c                   endif
135200060803
135300041221      * Se immesso p.o. partenza devo immettere il p.o. arrivo
135400041221if  2c                   If        v3scpp > *Zeros and v3scpa = *Zeros
135500041221     c                   Eval      *In28 = *On
135600041221     c                   Eval      *In53 = *On
135700041221     c                   Eval      v1cmsg = msg(22)
135800041221     c                   Z-add     nrr           rec
135900041221     c                   Update    ta8103s
136000041221     c                   Leave
136100041221    2c                   EndIf
136200041221
136300041221      * Se immesso p.o. arrivo devo immettere il p.o. partenza
136400041221if  2c                   If        v3scpa > *Zeros and v3scpp = *Zeros
136500041221     c                   Eval      *In28 = *On
136600041221     c                   Eval      *In52 = *On
136700041221     c                   Eval      v1cmsg = msg(23)
136800041221     c                   Z-add     nrr           rec
136900041221     c                   Update    ta8103s
137000041221     c                   Leave
137100041221    2c                   EndIf
137200041221
137300041221      * Se immesso p.o. partenza 999 il p.o. arrivo deve essere 999
137400041221if  2c                   If        v3scpp = 999 and v3scpa <> 999
137500041221     c                   Eval      *In28 = *On
137600041221     c                   Eval      *In53 = *On
137700041221     c                   Eval      v1cmsg = msg(25)
137800041221     c                   Z-add     nrr           rec
137900041221     c                   Update    ta8103s
138000041221     c                   Leave
138100041221    2c                   EndIf
138200041221
138300041221      * Se immesso p.o. arrivo 999 il p.o. partenza deve essere 999
138400041221if  2c                   If        v3scpa = 999 and v3scpp <> 999
138500041221     c                   Eval      *In28 = *On
138600041221     c                   Eval      *In52 = *On
138700041221     c                   Eval      v1cmsg = msg(25)
138800041221     c                   Z-add     nrr           rec
138900041221     c                   Update    ta8103s
139000041221     c                   Leave
139100041221    2c                   EndIf
139200041221
139300041221     c                   Z-add     nrr           rec
139400041221     c                   Update    ta8103s
139500041221
139600041221e   1c                   EndDo
139700041221
139800041221     c                   EndSr
139900041221
140000041118      *------------------------------------------------------------------------*
140100001011      * F6 - Conferma
140200041118      *------------------------------------------------------------------------*
140300041118     c     Sr_Conferma   BegSr
140400030611
140500041119     c   01
140600041119     cor 03              Clear                   Fnspe000
140700041118
140800041118     c                   Eval      SpeFls = kSpeFls
140900041118     c                   Eval      SpeCli = V1cCli
141000041118     c                   Eval      SpeCod = V1cCod
141100041118     c                   Eval      SpeRag = V1cRag
141200041118     c                   Eval      SpeInd = V1cInd
141300041118     c                   Eval      SpeCap = V1cCap
141400041118     c                   Eval      SpeLoc = V1cLoc
141500041118     c                   Eval      SpePro = V1cPro
141600041118     c                   Eval      SpeNaz = V1cNaz
141700070411     c**!!!              Eval      SpePiv = dsiva
141800070411     c                   if        v1ivaeu = *blanks
141900070411     c                   movel     v1ivait       spepiv
142000070411     c                   else
142100070411     c                   movel     v1civa        spepiv
142200070411     c                   endif
142300041118     c                   Eval      SpeTel = V1cTel
142400041118     c                   Eval      SpePer = V1cPer
142500041118     c                   Eval      SpeFax = V1cFax
142600041118     c                   Z-Add     *date         SpeDtr
142700010704
142800041118      * Scrivo
142900041119     c                   If        *In01 or *In03
143000041119     c                   If        V1cMail <> *Blanks
143100041118     c                   Clear                   Fnsp2000
143200041118     c                   Eval      Sp2Cod = SpeCod
143300041118     c                   Eval      Sp2Cli = SpeCli
143400041217     c                   Eval      Sp2Tpe = 'EM'
143500041118     c                   Eval      Sp2Est = V1cMail
143600041118     c                   Write     Fnsp2000
143700041119     c                   EndIf
143800041217
143900041217     c                   If        *In10
144000041217      * P.o. partenza
144100041221     c                   If        wpp <> *Blanks and wpp <> *Zeros
144200041217     c                   Clear                   Fnsp2000
144300041217     c                   Eval      Sp2Cod = SpeCod
144400041217     c                   Eval      Sp2Cli = SpeCli
144500041217     c                   Eval      Sp2Tpe = 'PP'
144600041217     c                   Eval      Sp2Est = wpp
144700041217     c                   Write     Fnsp2000
144800041217     c                   EndIf
144900041217      * P.o. arrivo
145000041221     c                   If        wpa <> *Blanks and wpa <> *Zeros
145100041217     c                   Clear                   Fnsp2000
145200041217     c                   Eval      Sp2Cod = SpeCod
145300041217     c                   Eval      Sp2Cli = SpeCli
145400041217     c                   Eval      Sp2Tpe = 'PA'
145500041217     c                   Eval      Sp2Est = wpa
145600041217     c                   Write     Fnsp2000
145700041217     c                   EndIf
145800060803      * Zona arrivo
145900060803     c                   If        wpz <> *Blanks and wpz <> *Zeros
146000060803     c                   Clear                   Fnsp2000
146100060803     c                   Eval      Sp2Cod = SpeCod
146200060803     c                   Eval      Sp2Cli = SpeCli
146300060803     c                   Eval      Sp2Tpe = 'PZ'
146400060803     c                   Eval      Sp2Est = wpz
146500060803     c                   Write     Fnsp2000
146600060803     c                   EndIf
146700070316      * giri divisi su 4 sk
146800070316     c                   if        wpg1 <> *blanks
146900070316     c                   clear                   fnsp2000
147000070316     c                   eval      sp2cod = specod
147100070316     c                   eval      sp2cli = specli
147200070316     c                   eval      sp2tpe = 'G1'
147300070316     c                   eval      sp2est = wpg1
147400070316     c                   write     fnsp2000
147500070316     c                   endif
147600070316     c                   if        wpg2 <> *blanks
147700070316     c                   clear                   fnsp2000
147800070316     c                   eval      sp2cod = specod
147900070316     c                   eval      sp2cli = specli
148000070316     c                   eval      sp2tpe = 'G2'
148100070316     c                   eval      sp2est = wpg2
148200070316     c                   write     fnsp2000
148300070316     c                   endif
148400070316     c                   if        wpg3 <> *blanks
148500070316     c                   clear                   fnsp2000
148600070316     c                   eval      sp2cod = specod
148700070316     c                   eval      sp2cli = specli
148800070316     c                   eval      sp2tpe = 'G3'
148900070316     c                   eval      sp2est = wpg3
149000070316     c                   write     fnsp2000
149100070316     c                   endif
149200070316     c                   if        wpg4 <> *blanks
149300070316     c                   clear                   fnsp2000
149400070316     c                   eval      sp2cod = specod
149500070316     c                   eval      sp2cli = specli
149600070316     c                   eval      sp2tpe = 'G4'
149700070316     c                   eval      sp2est = wpg4
149800070316     c                   write     fnsp2000
149900070316     c                   endif
150000041217     c                   EndIf
150100041217
150200041118     c                   Write     Fnspe000
150300041118     c                   EndIf
150400041118
150500041118      * Aggiorno
150600041118if  1c                   If        *In02
150700041217      * E-mail
150800041217     c                   Eval      kSp2Tpe = 'EM'
150900041118     c     kSp2          Chain     Fnsp201l
151000041118if  2c                   If        %Found(Fnsp201l)
151100041118if  3c                   If        V1cMail <> *Blanks
151200041118     c                   Eval      Sp2Est = V1cMail
151300041118     c                   Update    Fnsp2000
151400041118   x3c                   Else
151500041118     c                   Delete    Fnsp2000
151600041119e   3c                   EndIf
151700041119   x2c                   Else
151800041119if  3c                   If        V1cMail <> *Blanks
151900041119     c                   Clear                   Fnsp2000
152000041119     c                   Eval      Sp2Cod = SpeCod
152100041119     c                   Eval      Sp2Cli = SpeCli
152200041217     c                   Eval      Sp2Tpe = 'EM'
152300041119     c                   Eval      Sp2Est = V1cMail
152400041119     c                   Write     Fnsp2000
152500041119e   3c                   EndIf
152600041119e   2c                   EndIf
152700041217
152800041217     c                   If        *In10
152900041217      * P.o. partenza
153000041217     c                   Eval      kSp2Tpe = 'PP'
153100041217     c     kSp2          Chain     Fnsp201l
153200041217if  2c                   If        %Found(Fnsp201l)
153300041221if  3c                   If        wpp <> *Blanks and wpp <> *Zeros
153400041217     c                   Eval      Sp2Est = wpp
153500041217     c                   Update    Fnsp2000
153600041217   x3c                   Else
153700041217     c                   Delete    Fnsp2000
153800041217e   3c                   EndIf
153900041217   x2c                   Else
154000041221if  3c                   If        wpp <> *Blanks and wpp <> *Zeros
154100041217     c                   Clear                   Fnsp2000
154200041217     c                   Eval      Sp2Cod = SpeCod
154300041217     c                   Eval      Sp2Cli = SpeCli
154400041217     c                   Eval      Sp2Tpe = 'PP'
154500041217     c                   Eval      Sp2Est = wpp
154600041217     c                   Write     Fnsp2000
154700041217e   3c                   EndIf
154800041217e   2c                   EndIf
154900041217      * P.o. arrivo
155000041217     c                   Eval      kSp2Tpe = 'PA'
155100041217     c     kSp2          Chain     Fnsp201l
155200041217if  2c                   If        %Found(Fnsp201l)
155300041221if  3c                   If        wpa <> *Blanks and wpa <> *Zeros
155400041217     c                   Eval      Sp2Est = wpa
155500041217     c                   Update    Fnsp2000
155600041217   x3c                   Else
155700041217     c                   Delete    Fnsp2000
155800041217e   3c                   EndIf
155900041217   x2c                   Else
156000041221if  3c                   If        wpa <> *Blanks and wpa <> *Zeros
156100041217     c                   Clear                   Fnsp2000
156200041217     c                   Eval      Sp2Cod = SpeCod
156300041217     c                   Eval      Sp2Cli = SpeCli
156400041217     c                   Eval      Sp2Tpe = 'PA'
156500041217     c                   Eval      Sp2Est = wpa
156600041217     c                   Write     Fnsp2000
156700041217e   3c                   EndIf
156800041217e   2c                   EndIf
156900060803      * Zona arrivo
157000060803     c                   Eval      kSp2Tpe = 'PZ'
157100060803     c     kSp2          Chain     Fnsp201l
157200060803if  2c                   If        %Found(Fnsp201l)
157300060803if  3c                   If        wpz <> *Blanks and wpz <> *Zeros
157400060803     c                   Eval      Sp2Est = wpz
157500060803     c                   Update    Fnsp2000
157600060803   x3c                   Else
157700060803     c                   Delete    Fnsp2000
157800060803e   3c                   EndIf
157900060803   x2c                   Else
158000060803if  3c                   If        wpz <> *Blanks and wpz <> *Zeros
158100060803     c                   Clear                   Fnsp2000
158200060803     c                   Eval      Sp2Cod = SpeCod
158300060803     c                   Eval      Sp2Cli = SpeCli
158400060803     c                   Eval      Sp2Tpe = 'PZ'
158500060803     c                   Eval      Sp2Est = wpz
158600060803     c                   Write     Fnsp2000
158700060803e   3c                   EndIf
158800060803e   2c                   EndIf
158900070316      * giri divisi su 4 sk
159000070316     c                   eval      ksp2tpe = 'G1'
159100070316     c     ksp2          chain     fnsp201l
159200070316    2c                   if        %found(fnsp201l)
159300070316    3c                   if        wpg1 <> *blanks
159400070316     c                   eval      sp2est = wpg1
159500070316     c                   update    fnsp2000
159600070316   x3c                   else
159700070316     c                   delete    fnsp2000
159800070316    3c                   endif
159900070316   x2c                   else
160000070316    3c                   if        wpg1 <> *blanks
160100070316     c                   clear                   fnsp2000
160200070316     c                   eval      sp2cod = specod
160300070316     c                   eval      sp2cli = specli
160400070316     c                   eval      sp2tpe = 'G1'
160500070316     c                   eval      sp2est = wpg1
160600070316     c                   write     fnsp2000
160700070316    3c                   endif
160800070316    2c                   endif
160900070316     c                   eval      ksp2tpe = 'G2'
161000070316     c     ksp2          chain     fnsp201l
161100070316    2c                   if        %found(fnsp201l)
161200070316    3c                   if        wpg2 <> *blanks
161300070316     c                   eval      sp2est = wpg2
161400070316     c                   update    fnsp2000
161500070316   x3c                   else
161600070316     c                   delete    fnsp2000
161700070316    3c                   endif
161800070316   x2c                   else
161900070316    3c                   if        wpg2 <> *blanks
162000070316     c                   clear                   fnsp2000
162100070316     c                   eval      sp2cod = specod
162200070316     c                   eval      sp2cli = specli
162300070316     c                   eval      sp2tpe = 'G2'
162400070316     c                   eval      sp2est = wpg2
162500070316     c                   write     fnsp2000
162600070316    3c                   endif
162700070316    2c                   endif
162800070316     c                   eval      ksp2tpe = 'G3'
162900070316     c     ksp2          chain     fnsp201l
163000070316    2c                   if        %found(fnsp201l)
163100070316    3c                   if        wpg3 <> *blanks
163200070316     c                   eval      sp2est = wpg3
163300070316     c                   update    fnsp2000
163400070316   x3c                   else
163500070316     c                   delete    fnsp2000
163600070316    3c                   endif
163700070316   x2c                   else
163800070316    3c                   if        wpg3 <> *blanks
163900070316     c                   clear                   fnsp2000
164000070316     c                   eval      sp2cod = specod
164100070316     c                   eval      sp2cli = specli
164200070316     c                   eval      sp2tpe = 'G3'
164300070316     c                   eval      sp2est = wpg3
164400070316     c                   write     fnsp2000
164500070316    3c                   endif
164600070316    2c                   endif
164700070316     c                   eval      ksp2tpe = 'G4'
164800070316     c     ksp2          chain     fnsp201l
164900070316    2c                   if        %found(fnsp201l)
165000070316    3c                   if        wpg4 <> *blanks
165100070316     c                   eval      sp2est = wpg4
165200070316     c                   update    fnsp2000
165300070316   x3c                   else
165400070316     c                   delete    fnsp2000
165500070316    3c                   endif
165600070316   x2c                   else
165700070316    3c                   if        wpg4 <> *blanks
165800070316     c                   clear                   fnsp2000
165900070316     c                   eval      sp2cod = specod
166000070316     c                   eval      sp2cli = specli
166100070316     c                   eval      sp2tpe = 'G4'
166200070316     c                   eval      sp2est = wpg4
166300070316     c                   write     fnsp2000
166400070316    3c                   endif
166500070316    2c                   endif
166600041217     c                   EndIf
166700041217
166800041119     c                   Update    Fnspe000
166900041118e   1c                   EndIf
167000010704
167100041118     c                   EndSr
167200041118
167300041118      *------------------------------------------------------------------------*
167400010703      * F16 - Annullamento
167500041118      *------------------------------------------------------------------------*
167600041118     c     Sr_Annulla    BegSr
167700001011
167800041217     c     kSp21         Setll     Fnsp201l
167900041217     c                   Do        *Hival
168000041217     c     kSp21         Reade     Fnsp201l
168100041217     c                   If        %Eof(Fnsp201l)
168200041217     c                   Leave
168300041217     c                   EndIf
168400041118     c                   Delete    Fnsp2000
168500041217     c                   EndDo
168600001011
168700041118     c                   Delete    Fnspe000
168800041118
168900041118     c                   EndSr
169000090715
169100041122      *------------------------------------------------------------------------*
169200090715      * F2 - Rubrica
169300041122      *------------------------------------------------------------------------*
169400090715     c     Sr_Rubrica    BegSr
169500090715
169600090715     c                   clear                   tnta12ds
169700090715     c  n05              eval      ta12ric = 'M'
169800090715     c   05              eval      ta12ric = 'V'
169900090715     c                   Eval      ta12apl = 'C'
170000090715     c                   Eval      ta12ksc = v1ccli
170100090715     c                   Eval      ta12rag = v1dcli
170200090715     c                   Call      'TNTA12R'
170300090715     c                   Parm                    kpjba
170400090715     c                   Parm                    tnta12ds
170500041122
170600041122     c                   EndSr
170700070316
170800070316      *------------------------------------------------------------------------*
170900070316      * controlla giro
171000070316      *------------------------------------------------------------------------*
171100070316     c     sr_ctrgiro    begsr
171200070316
171300070316     c                   clear                   fidg09ds
171400070316     c                   eval      d09iop0 = '001'
171500070316     c                   eval      d09ifgs = v3scpa
171600070316     c                   eval      d09idat = *date
171700070316     c                   eval      d09icgi = v3scgi
171800070709     c                   eval      d09itug = 'C'
171900070316     c                   eval      kpjbu = fidg09ds
172000070316     c                   call      'FIDG09R'
172100070316     c                   parm                    kpjba
172200070316     c                   eval      fidg09ds = kpjbu
172300070316
172400070316     c                   endsr
172500041117
172600041117      *------------------------------------------------------------------------*
172700041117      * ROUTINE INIZIALE
172800041117      *------------------------------------------------------------------------*
172900041117     c     *Inzsr        BegSr
173000041117
173100041117     c     *Entry        Plist
173200041117     c                   Parm                    Kpjba
173300041117     c                   Parm                    Tnta81ds
173400041117
173500041117     c     *dtaara       define    §azute        azuteds
173600041117     c     *dtaara       define    §datiute      ddatiute
173700041117     c                   in(E)     *dtaara
173800041117     c                   If        %error  or RSUT = *blanks
173900041117     c                   Clear                   Tibs34ds
174000041117     c                   Call      'TIBS34R'
174100041117     c                   Parm                    Tibs34ds
174200041117     c                   In        *dtaara
174300041117     c                   EndIf
174400090713
174500090713
174600090713     c* Richiamo solo per avere codice di abilitazione
174700090713     c                   clear                   tntaa1ds
174800090713     c                   eval      itaa1caut='§utecli'
174900090713     c                   clear                   kpjbu
175000090713     c                   movel     tntaa1ds      kpjbu
175100090713     c                   call      'TNTAA1C'
175200090713     c                   parm                    kpjba
175300090713     c                   movel     kpjbu         tntaa1ds
175400090713     c
175500090713     C* UTENTE NON abilitato
175600090713     c                   if        otaa1fabi='N'
175700041117     c                   Eval      *In11 = *On
175800041117     c                   Eval      *In28 = *On
175900041117     c                   Eval      V1cmsg = msg(01)
176000041221e   2c                   EndIf
176100041117
176200041117      * Controllo se sono in sede
176300041117     c                   If        Simfel = *Zeros
176400041117     c                   Eval      *In12 = *On
176500041117     c                   EndIf
176600041117
176700041117     c                   Select
176800041117      * Immissione
176900041117     c                   When      I81opz = *Blanks
177000041117     c                   Eval      V1mod = mod(01)
177100041117     c                   Eval      *In01 = *On
177200041117      * Manutenzione
177300041117     c                   When      I81opz = '2'
177400041117     c                   Eval      V1mod = mod(02)
177500041117     c                   Eval      *In02 = *On
177600041119      * Copia
177700041119     c                   When      I81opz = '3'
177800041119     c                   Eval      V1mod = mod(03)
177900041119     c                   Eval      *In03 = *On
178000041117      * Interrogazione
178100041117     c                   When      I81opz = '5'
178200041119     c                   Eval      V1mod = mod(04)
178300041117     c                   Eval      *In05 = *On
178400041117     c                   EndSl
178500041117
178600041118     c                   Eval      V1cCod = I81Cod
178700041118     c                   Eval      V1cCli = I81Cli
178800041118      * posiziono il cursore per immissione
178900041119     c                   If        *In01 or *In03
179000041118     c                   Select
179100041118     c                   When      V1cCli <> *Zeros and V1cCod <> *Blanks
179200041118     c                   Eval      *In42 = *On
179300041118     c                   When      V1cCod = *Blanks
179400041118     c                   Eval      *In40 = *On
179500041118     c                   When      V1cCli <> *Zeros
179600041118     c                   Eval      *In41 = *On
179700041118     c                   EndSl
179800041118     c                   EndIf
179900060216      * proteggo i campi chiave se già passati
180000060216      * se non è copia
180100060216     c                   if        not *in03
180200060216     c                   eval      *in06 = (v1ccod <> *blanks)
180300060216     c                   eval      *in07 = (v1ccli <> *zeros)
180400060216     c                   endif
180500060216      * se provengo da anagrafica clienti proteggo sempre il cliente
180600060216     c                   if        i81gcli = '1'
180700060216     c                   eval      *in07 = *on
180800060216     c                   endif
180900041117
181000041117      * Carico Tabella 2F
181100041117     c                   Clear                   xx
181200041117     c                   Clear                   c2f
181300041117     c                   Clear                   t2f
181400041117     c                   Eval      kCod = '2F'
181500041117     c     kTab01        Setll     Tabel00f
181600041117     c                   Do        *Hival
181700041117     c     kTab01        Reade     Tabel00f
181800041117     c                   If        %Eof(Tabel00f)
181900041117     c                   Leave
182000041117     c                   EndIf
182100041117     c                   If        xx >= 100
182200041117     c                   Leave
182300041117     c                   EndIf
182400041117     c                   If        TblFlg <> *Blanks
182500041117     c                   Iter
182600041117     c                   EndIf
182700041117     c                   Eval      xx = xx + 1
182800041117     c                   Eval      ds2f = TblUni
182900041117     c                   Movel     TblKey        c2f(xx)
183000041117     c                   Movel     §2fFtp        t2f(xx)
183100041117     c                   EndDo
183200041117
183300041117      * Carico Tabella 2H
183400041117     c                   Clear                   xx
183500041117     c                   Clear                   c2h
183600041117     c                   Clear                   t2h
183700041117     c                   Eval      kCod = '2H'
183800041117     c     kTab01        Setll     Tabel00f
183900041117     c                   Do        *Hival
184000041117     c     kTab01        Reade     Tabel00f
184100041117     c                   If        %Eof(Tabel00f)
184200041117     c                   Leave
184300041117     c                   EndIf
184400041117     c                   If        xx >= 100
184500041117     c                   Leave
184600041117     c                   EndIf
184700041117     c                   If        TblFlg <> *Blanks
184800041117     c                   Iter
184900041117     c                   EndIf
185000041117     c                   Eval      xx = xx + 1
185100041117     c                   Eval      ds2h = TblUni
185200041117     c                   Movel     TblKey        c2h(xx)
185300041118     c                   Movel     §2hFax        t2h(xx)
185400041117     c                   EndDo
185500041117
185600041117      * KLIST
185700041117     c     kTab          Klist
185800041117     c                   Kfld                    Codut
185900041117     c                   Kfld                    kCod
186000041117     c                   Kfld                    kKey
186100041117
186200041117     c     kTab01        Klist
186300041117     c                   Kfld                    Codut
186400041117     c                   Kfld                    kCod
186500041117
186600041117     c     kAco          Klist
186700041117     c                   Kfld                    Codut
186800041117     c                   Kfld                    DutKci
186900041117     c                   Kfld                    V1cCli
187000041117
187100041117     c     kSpe          Klist
187200041117     c                   Kfld                    kSpeFls
187300041117     c                   Kfld                    V1cCli
187400041117     c                   Kfld                    V1cCod
187500041117
187600041117     c     kSp2          Klist
187700041117     c                   Kfld                    SpeCli
187800041117     c                   Kfld                    SpeCod
187900041117     c                   Kfld                    kSp2Tpe
188000041217
188100041217     c     kSp21         Klist
188200041217     c                   Kfld                    SpeCli
188300041217     c                   Kfld                    SpeCod
188400041117
188500041117     c                   EndSr
188600041117      *------------------------------------------------------------------------*
188700041117
188800041117** CAR
188900041117!:§
189000041117** MOD  Lungh. 15
189100041117  IMMISSIONE
189200041117 MANUTENZIONE
189300041119    COPIA
189400041117VISUALIZZAZIONE
189500001010** MSG  Lungh. 78                                                            *
189600041117Utente non abilitato alla Gestione Luoghi di destinazione                     01
189700041117Immettere il Luogo                                                            02
189800041117Codice Luogo Errato o non utilizzabile                                        03
189900041117Immettere il Cliente                                                          04
190000041117Cliente non gestibile con 9999 o 8888                                         05
190100041117Cliente Inesistente o non gestibile                                           06
190200041117Immettere la ragione sociale                                                  07
190300041117Nel campo sono stati inseriti caratteri non validi                            08
190400041117Immettere l'indirizzo                                                         09
190500041117Immettere la località                                                         10
190600041117Nazione errata                                                                11
190700041117Attenzione !! Modificata Località                                             12
190800041117Attenzione !! Modificata Provincia                                            13
190900041117Attenzione !! Modificato CAP                                                  14
191000041117Attenzione !! Modificata Nazione                                              15
191100041117Immettere la Provincia                                                        16
191200041117Provincia errata                                                              17
191300070411                                                                              18
191400041118Luogo già inserito per il cliente immesso                                     19
191500070316Filiale Partenza errata                                                       20
191600070316Filiare Arrivo errata                                                         21
191700070316Inserire la filiale arrivo se immessa la filiale partenza                     22
191800070316Inserire la filiale partenza se immessa la filiale arrivo                     23
191900070316Attenzione le modifiche fatte alle personalizzazioni fil. andranno perse!!!   24
192000070316Inserire la filiale "999"                                                     25
192100060803Zona Arrivo errata                                                            26
192200070316Immettere un giro valido                                                      27
