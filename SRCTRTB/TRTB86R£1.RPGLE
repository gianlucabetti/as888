000100020307     H DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
000200020206
000300020206      ***********************************************************
000400020206      *
000500020206      * GESTIONE TABELLA NAZIONI
000600020206      *
000700020206      ***********************************************************
000800020206
000900020206      ***********************************************************
001000020206      *  RIEPILOGO INDICATORI
001100020206      ***********************************************************
001200020206      * 01 - INSERIMENTO
001300020206      * 02 - VARIAZIONE
001400020206      * 03 - ANNULLATO
001500020206      * 28 - ERRORE GENERICO DSPF
001600020206      * 30 - RICERCA TABELLA
001700020206      * 40 - ERRORE DESCRIZIONE
001800020307      * 41 - ERRORE CODICE ISO
001900020307      * 42 - ERRORE DIVISA
002000020307      * 43 - ERRORE ISO PER PARTITA IVA
002100020307      * 44 - ERRORE CAP
002200020307      * 45 - ERRORE ATTRAVERSAMENTO DOGANA DPD
002300020307      * 46 - ERRORE ATTRAVERSAMENTO DOGANA FEDEX
002400020307      * 47 - ERRORE CODICE TASSAZIONE FEDEX
002500020307      * 48 - ERRORE FLAG INOLTRO FEDEX
002600020311      * 49 - ERRORE TIPO TEMPO CONSEGNA FEDEX
002700100122      * 50 - ERRORE iso numerico
002800100122      * 51 - ERRORE orm automatici
002900100122      * 52 - ERRORE nazione UE
003000020206      ***********************************************************
003100020206
003200020131     FTRTB86D   CF   E             WORKSTN
003300060707     f                                     sfile(tb86s1:nrr)
003400891030     FTABEL00F  UF A E           K DISK
003401131218     Ftntbe01l  iF   E           K DISK
003500060707     fazlin01l  if   e           k disk
003600020206
003700060707     d kazlin          s                   like(lintabel)
003800020206     D kcod            S                   LIKE(tblcod)
003900020206     D kkey            S                   LIKE(tblkey)
004000060707     d kkut            s                   like(tblkut)
004001131218     d ktcod           s                   like(tbecod) inz('NAS')
004002131218     d ktke1           s                   like(tbeke1)
004003131218     d ktke2           s                   like(tbeke2)
004100020206
004200020206     D ordina          S              1
004300020206     D comando         S              1
004400020301     d wf6             s              1    inz('0')
004500020301     d wf16            s              1    inz('0')
004600020301     d wf5             s              1    inz('0')
004700060707
004800060707     d savtblftt       s                   like(tblftt)
004900060707     d savtblflt       s                   like(tblflt)
005000060707     d savtbluni       s                   like(tbluni)
005100060707     d nrr             s              4  0
005200060707     d savrec          s                   like(nrr)
005300020206
005400131218     D MSG             S             78    DIM(19) CTDATA PERRCD(1)
005500020301
005600020301     D DataR           DS
005700020301     D  aaR                    1      2
005800020301     D  mmR                    3      4
005900020411     D  ggR                    5      6
006000020301
006100020301     D DataD           DS
006200020301     D  ggD                    1      2
006300020301     D  mmD                    3      4
006400020411     D  aaD                    5      6
006500020206
006600020206     D Video2        E DS                  extname(trtb86d:tb86d2)
006700020206
006800120620     d trtb09ds      e ds
006900020131     D DS15          E DS
007000100122     D DS4I          E DS
007100020312     d tisi00ds      e ds
007200080423     d tisiq0ds      e ds
007300020206     D UT§DSE0F      E DS
007400891030     D KPJBA         E DS
007500020206     D  rem                    1      3
007600100122
007700100122     d forzaiso        s              1
007800100122     d forzaUE         s              1
007900020206      *----------------------------------------------------------
008000020206
008100020206     c     *entry        plist
008200020206     c                   parm                    kpjba
008300020206
008400020206     c                   eval      codut = 1
008500020206     c                   call      'XPARUT'
008600020206     c                   parm                    ut§dse0f
008700020206     c                   movel     ragut         rsut
008800020206
008900020206     c     ktab          klist
009000060707     c                   kfld                    kkut
009100020206     c                   kfld                    kcod
009200020206     c                   kfld                    kkey
009201131218     c     ktbe          klist
009202131218     c                   kfld                    ktcod
009203131218     c                   kfld                    ktke1
009204131218     c                   kfld                    ktke2
009300020206
009400020206      * Prima videata: richiesta codice tabella
009500020206
009600020206     c     inizio        tag
009700020206
009800020206     c                   eval      *in01 = *off
009900020206     c                   eval      *in02 = *off
010000020206     c                   eval      *in03 = *off
010100020301     c                   eval      wf6 = *off
010200020301     c                   eval      wf16 = *off
010300020301     c                   eval      wf5 = *off
010400020206
010500020206     c                   if        v1cnaz = *blanks
010600100122     c                   eval      v1cnaz = '?  '
010700020206     c                   endif
010800020206
010900020206     c                   exfmt     tb86d1
011000020206
011100020206      * F3 - Fine
011200020206     c   kc              goto      fine
011300020206
011400020206      * Controllo codice tabella
011500020206     c                   exsr      ctrcod
011600020206     c   30              goto      inizio
011700020301
011800020206
011900020206      * Seconda videata: dati tabella
012000020206     c                   do        *hival
012100020206
012200060707     c     inz02         tag
012300020206     c                   exfmt     tb86d2
012400020206     c                   eval      *in28 = *off
012500130925     c                   clear                   V2Cmsg
012600020206
012700020206      * F3 - Fine
012800020206     c   kc              goto      fine
012900060707      * F9 - Traduzione
013000060707     c                   if        *inki
013100060707     c                   eval      *in05 = *on
013200060707     c                   exsr      sr_lingua
013300060707     c                   eval      *in05 = *off
013400060707     c                   iter
013500060707     c                   endif
013600020301
013700020301      * F6 - Conferma
013800020301     c                   if        *inkf
013900020301     c                   exsr      ctrd02
014000020301     c   28              iter
014100020301     c                   exsr      sr_f06
014200020301     c                   if        wf6 = *on
014300060707      * tabelle in lingua
014400060707     c                   eval      *in05 = *off
014500060707     c                   exsr      sr_lingua
014600020301     c                   clear                   v1cnaz
014700020301     c                   goto      inizio
014800020301     c                   else
014900020301     c                   iter
015000020301     c                   endif
015100020301     c                   endif
015200020301
015300020301      * F10 - Decodifica struttura cap
015400020301     c                   if        *inkj
015500020301     c                   exsr      sr_f10
015600020301     c                   endif
015700080423      * F15 - Cappario utilizzi particolari
015800080423     c                   if        *inkp
015900080423     c                   clear                   tisiq0ds
016000080423     c                   MOVEL     'C05'         dq0op0
016100080423     c                   clear                   dq0op1
016200080423     c                   MOVEL     '0'           dq0f03
016300080423     c                   MOVEL     '0'           dq0f12
016400080423     c                   MOVEL     '0'           dq0err
016500080423     c                   call      'TISIQ0R'
016600080423     c                   parm                    kpjba
016700080423     c                   parm                    tisiQ0ds
016800080423     c                   iter
016900080423     c                   endif
017000020206
017100020206      * F12 - Ritorno
017200020206     c                   if        *inkl
017300020206     c                   clear                   v1cnaz
017400020206     c                   goto      inizio
017500020206     c                   endif
017600020206
017700020206      * F16 - Annullamento
017800020206     c                   if        *inkq
017900020301     c                   eval      wf16 = *on
018000020301     c                   exsr      ctrd02
018100020301     c   28              iter
018200020301     c                   exsr      sr_f06
018300020301     c                   if        wf6 = *on
018400020301     c                   clear                   v1cnaz
018500020301     c                   goto      inizio
018600020301     c                   else
018700020301     c                   eval      wf16 = *off
018800020301     c                   iter
018900020301     c                   endif
019000020206     c                   endif
019100020206
019200020206      * F5 - Ripristino
019300020206     c                   if        *inke
019400020301     c                   eval      wf5 = *on
019500020301     c                   exsr      ctrd02
019600020301     c   28              iter
019700020301     c                   exsr      sr_f06
019800020301     c                   if        wf6 = *on
019900020301     c                   clear                   v1cnaz
020000020301     c                   goto      inizio
020100020301     c                   else
020200020301     c                   eval      wf5 = *off
020300020301     c                   iter
020400020301     c                   endif
020500020206     c                   endif
020600020206
020700020206      * Controlli
020800020206     c                   exsr      ctrd02
020900020206     c   28              iter
021000020206
021100020206     c                   enddo
021200020206
021300020206     c     fine          tag
021400020206     c                   eval      *inlr = *on
021500020206      *----------------------------------------------------------
021600020206      *    Controllo Codice
021700020206      *----------------------------------------------------------
021800020206     c     ctrcod        begsr
021900020307
022000060707     c                   eval      kkut = 1
022100020307     c                   movel     '15'          kcod
022200020206
022300020206      *   Ricerca tabella
022400020206     c     '?'           scan      v1cnaz                                 30
022500020206     c                   if        *in30
022600020206     c                   call      'TRUL19R'
022700020206     c                   parm                    kcod
022800020206     c                   parm                    ordina
022900020206     c                   parm                    kkey
023000020206     c                   parm                    comando
023100020206     c                   movel     kkey          v1cnaz
023200020206     c                   goto      endctrcod
023300020206     c                   endif
023400020206
023500020206     c                   movel     v1cnaz        kkey
023600020206
023700020206     c     ktab          chain     tabel00f
023800020206      * Immissione
023900020206     c                   if        not %found
024000020206     c                   eval      *in01 = *on
024100020206     c                   else
024200020206     c                   eval      ds15 = tbluni
024300020206     c                   if        tblflg <> *blanks
024400020206      * Annullato
024500020206     c                   eval      *in03 = *on
024600020206     c                   else
024700020206      * Aggiornamento
024800020206     c                   eval      *in02 = *on
024900020206     c                   endif
025000020206     c                   endif
025100020206
025200020206     c                   clear                   video2
025300020206      * Passo dati al video
025400020206     c                   if        not *in01
025500020206     c                   eval      v2des = §15des
025600020206     c                   eval      v2ecf = §15ecf
025700020206     c                   eval      v2pcf = §15pcf
025800020206     c                   eval      v2ucf = §15ucf
025900020206     c                   eval      v2vlt = §15vlt
026000020206     c                   eval      v2iso = §15iso
026100020206     c                   eval      v2cap = §15cap
026200020206     c                   eval      v2eft = §15eft
026300020206     c                   eval      v2ita = §15ita
026400020206     c                   eval      v2cpr = §15cpr
026500020206     c                   eval      v2cod = §15cod
026600020206     c                   eval      v2cie = §15cie
026700020206     c                   eval      v2cpp = §15cpp
026800020206     c                   eval      v2lad = §15lad
026900060919     c                   eval      v2red = §15red
027000020206     c                   eval      v2efd = §15efd
027100020206     c                   eval      v2cei = §15cei
027200020206     c                   eval      v2dog = §15dog
027300020206     c                   eval      v2laf = §15laf
027400020206     c                   eval      v2sto = §15sto
027500020206     c                   eval      v2eff = §15eff
027600020206     c                   eval      v2cts = §15cts
027700020206     c                   eval      v2inl = §15inl
027800020206     c                   eval      v2ffn = §15ffn
027900020206     c                   eval      v2eex = §15eex
028000020412     c                   eval      v2acn = §15acn
028100080604     c                   move      §15dpdPOE     V2dpdPOE
028200020311     c                   movel     §15ttc        v2ttc
028300060209     c                   eval      v2vas = §15vas
028400100115     c                   eval      v2ue  = §15ue
028500100122     c
028600100122     c                   clear                   forzaiso
028700100122     c                   clear                   forzaUE
028800100122     c
028900080423     c**                 eval      v2caeex = §15cex
029000080423     c**                 eval      v2cadpd = §15cdp
029100080423     c**                 eval      v2cafed = §15cfe
029200020206     c                   endif
029300020206
029400020206     c     endctrcod     endsr
029500020206
029600020206      *----------------------------------------------------------
029700020206      *    Controllo Seconda videata
029800020206      *----------------------------------------------------------
029900020206     c     ctrd02        begsr
030000020312
030100020312     c                   setoff                                       404142
030200020312     c                   setoff                                       434445
030300020312     c                   setoff                                       464748
030400020312     c                   setoff                                       49
030500020206
030600020206      * Descrizione
030700020206     c                   if        v2des = *blanks
030800020206     c                   movel     msg(1)        v2cmsg
030900020206     c                   eval      *in28 = *on
031000020206     c                   eval      *in40 = *on
031100020206     c                   goto      endctrd02
031200020206     c                   endif
031300020307
031400020307      * Codice Iso
031500020307     c                   if        v2cod = *blanks
031600131218     c                   movel     msg(2)        v2cmsg
031700020307     c                   eval      *in28 = *on
031800020307     c                   eval      *in41 = *on
031900020307     c                   goto      endctrd02
032000020307     c                   endif
032001131218     c                   eval      ktke1='ALPHA-2 '
032002131218     c                   eval      ktke2=v2cod
032003131218     c     ktbe          chain     tntbe01l
032004131218     c                   if        not %found(tntbe01l)
032005131218     c                   movel     msg(19)       v2cmsg
032006131218     c                   eval      *in28 = *on
032007131218     c                   eval      *in41 = *on
032008131218     c                   goto      endctrd02
032009131218     c                   endif
032010131218     c
032100060426
032200060426      * Codice Iso Numerico
032300060426     c                   if        v2cie = *zeros
032400060426     c                   movel     msg(2)        v2cmsg
032500060426     c                   eval      *in28 = *on
032600060426     c                   eval      *in50 = *on
032700060426     c                   goto      endctrd02
032800060426     c                   endif
032801131218     c                   eval      ktke1='NUMERICO'
032802131218     c                   movel(p)  v2cie         ktke2
032803131218     c     ktbe          chain     tntbe01l
032804131218     c                   if        not %found(tntbe01l)
032805131218     c                   movel     msg(19)       v2cmsg
032806131218     c                   eval      *in28 = *on
032807131218     c                   eval      *in50 = *on
032808131218     c                   goto      endctrd02
032809131218     c                   endif
032900020307
033000020307      * Divisa
033100020312 b1  c                   if        v2vlt = *blanks
033200020307     c                   movel     msg(3)        v2cmsg
033300020307     c                   eval      *in28 = *on
033400020307     c                   eval      *in42 = *on
033500020307     c                   goto      endctrd02
033600020312 x1  c                   else
033700020311      *   ricerca
033800060707     c                   eval      kkut = 1
033900020311     c                   eval      kcod = 'CV'
034000020311     c                   clear                   kkey
034100020311     c     '?'           scan      v2vlt                                  30
034200020312 b2  c                   if        *in30
034300020311     c                   call      'TRUL19R'
034400020311     c                   parm                    kcod
034500020311     c                   parm                    ordina
034600020311     c                   parm                    kkey
034700020311     c                   parm                    comando
034800020311     c                   movel     kkey          v2vlt
034900020311     c                   goto      endctrd02
035000020312 e2  c                   endif
035100020311      *   controllo
035200100122     c                   movel(P)  v2vlt         kkey
035300020311     c     ktab          chain(n)  tabel00f
035400020312 b2  c                   if        not %found
035500020311     c                   movel     msg(11)       v2cmsg
035600020311     c                   eval      *in28 = *on
035700020311     c                   eval      *in42 = *on
035800020311     c                   goto      endctrd02
035900020312 e2  c                   endif
036000020312 e1  c                   endif
036100020307
036200020307      * Codice Iso per partita iva
036300020307     c                   if        v2iso = *blanks
036400020307     c                   movel     msg(4)        v2cmsg
036500020307     c                   eval      *in28 = *on
036600020307     c                   eval      *in43 = *on
036700020307     c                   goto      endctrd02
036800020307     c                   endif
036900100122
037000100122      * Appartenenza a U E
037100100122    1c                   if        v2ue='S'
037200100122     c* il codice iso per partita iva non deve essere $$ --> errore forzabile
037300100122    2c                   if        v2iso='$$' and forzaiso=' '
037400100122     c                   movel     msg(15)       v2cmsg
037500100122     c                   eval      *in28 = *on
037600100122     c                   eval      *in52 = *on
037700100122     c                   eval      forzaiso='1'
037800100122     c                   goto      endctrd02
037900100122    2c                   endif
038000100122    1c                   endif
038100100122     c
038200100122     c* Verifico se presente flag UE anche nella 4I errore forzabile
038300100122    1c                   if        forzaue=' '
038400100122     c                   eval      kcod = '4I'
038500100122     c                   movel(P)  v2iso         kkey
038600100122     c     ktab          chain(n)  tabel00f
038700100122    2c                   if        not %found
038800100122     c                   movel     msg(16)       v2cmsg
038900100122     c                   else
039000100122     c                   movel     tbluni        ds4i
039100100122
039200100122    3c                   if        §4iue<>v2ue
039300100122    4c                   if        v2ue='S'
039400100122     c                   movel     msg(17)       v2cmsg
039500100122     c                   else
039600100122     c                   movel     msg(18)       v2cmsg
039700100122    4c                   endif
039800100122    3c                   endif
039900100122    2c                   endif
040000100122     c* Errore
040100100122    2c                   if        v2cmsg<>*blanks
040200100122     c                   eval      *in28 = *on
040300100122     c                   eval      *in52 = *on
040400100122     c                   eval      forzaue ='1'
040500100122     c                   goto      endctrd02
040600100122    2c                   endif
040700100122    1c                   endif
040800020206
040900020206      * Cap
041000020206     c                   if        v2cpp = 'N' and v2cpr = 'S'
041100020307     c                   movel     msg(5)        v2cmsg
041200020206     c                   eval      *in28 = *on
041300020307     c                   eval      *in44 = *on
041400020206     c                   goto      endctrd02
041500020206     c                   endif
041600020307
041700080605      * DPD P.O.Emissione
041800080605     c                   if        v2dpdPOE <> 0   and
041900080605     c                             v2dpdPOE <> 190 and
042000080605     c                             v2dpdPOE <> 195
042100080605     c                   movel     msg(14)       v2cmsg
042200080605     c                   eval      *in28 = *on
042300080605     c                   eval      *in51 = *on
042400080605     c                   goto      endctrd02
042500080605     c                   endif
042600020307
042700020307      * FedEx
042800020312 b1  c                   if        v2laf <> *zeros
042900020307      * codice tassazione
043000120620     c                   if        %scan('?':v2cts) > 0
043100120620     c                   clear                   TRTB09DS
043200120620     c                   eval      TB09fun = '1'
043300120713     c                   eval      TB09ord = '1'
043400120713     c                   eval      TB09utf = 'S'
043500120620     c                   eval      kpjbu = TRTB09DS
043600120620     c                   call      'TRTB09R'
043700120620     c                   parm                    kpjba
043800120620     c                   eval      TRTB09DS = kpjbu
043900120620     c                   IF        TB09cod <> *blanks
044000120620     c                   eval      v2cts = TB09cod
044100120620     c                   ENDIF
044200120620     c                   endif
044300020312 b2  c                   if        v2cts = *blanks
044400020307     c                   movel     msg(8)        v2cmsg
044500020307     c                   eval      *in28 = *on
044600020307     c                   eval      *in47 = *on
044700020307     c                   goto      endctrd02
044800020312 e2  c                   endif
044900020307      * flag inoltro
045000020312 b2  c                   if        v2inl = *blanks
045100020307     c                   movel     msg(9)        v2cmsg
045200020307     c                   eval      *in28 = *on
045300020307     c                   eval      *in48 = *on
045400020307     c                   goto      endctrd02
045500020312 x2  c                   else
045600020311      *   ricerca
045700060707     c                   eval      kkut = 1
045800020307     c                   eval      kcod = '9A'
045900020307     c                   clear                   kkey
046000020307     c     '?'           scan      v2inl                                  30
046100020312 b3  c                   if        *in30
046200020307     c                   call      'TRUL19R'
046300020307     c                   parm                    kcod
046400020307     c                   parm                    ordina
046500020307     c                   parm                    kkey
046600020307     c                   parm                    comando
046700020307     c                   movel     kkey          v2inl
046800020307     c                   goto      endctrd02
046900020312 e3  c                   endif
047000020311      *   controllo
047100100122     c                   movel(P)  v2inl         kkey
047200020307     c     ktab          chain(n)  tabel00f
047300020312 b3  c                   if        not %found
047400020307     c                   movel     msg(10)       v2cmsg
047500020307     c                   eval      *in28 = *on
047600020307     c                   eval      *in48 = *on
047700020307     c                   goto      endctrd02
047800020312 e3  c                   endif
047900020312 e2  c                   endif
048000020311      * tipo tempo consegna
048100020312 b2  c                   if        v2ttc = *blanks
048200020311     c                   movel     msg(12)       v2cmsg
048300020311     c                   eval      *in28 = *on
048400020311     c                   eval      *in49 = *on
048500020311     c                   goto      endctrd02
048600020312 x2  c                   else
048700020311      *   ricerca
048800020311     c     '?'           scan      v2ttc                                  30
048900020312 b3  c                   if        *in30
049000020312     c                   clear                   tisi00ds
049100020312     c                   eval      d00op0 = 'T01'
049200020312     c                   eval      d00f03 = *off
049300020312     c                   eval      d00f12 = *off
049400020312     c                   eval      d00err = *off
049500020311     c                   call      'TISI41R'
049600020311     c                   parm                    kpjba
049700020312     c                   parm                    tisi00ds
049800020312 b4  c                   if        d00f03 = *on or d00f12 = *on or
049900020312     c                             d00err = *on
050000020312     c                   goto      endctrd02
050100020312 e4  c                   endif
050200020312     c                   movel     d00ttc        v2ttc
050300020312     c                   goto      endctrd02
050400020312 e3  c                   endif
050500020312 e2  c                   endif
050600020312 e1  c                   endif
050700020206
050800020206     c     endctrd02     endsr
050900020206
051000020301      *----------------------------------------------------------
051100020301      *    Decodifica struttura cap
051200020301      *----------------------------------------------------------
051300020301     c     sr_f10        begsr
051400020301
051500020301     c                   do        *hival
051600020304     c                   exfmt     tb86w2
051700020301      * F12 - Ritorno
051800020301     c                   if        *inkl
051900020301     c                   leave
052000020301     c                   endif
052100020301
052200020301     c                   enddo
052300020304     c                   eval      *inkl = *off
052400020301
052500020301     c                   endsr
052600020301
052700020301      *----------------------------------------------------------
052800020301      *    Videata conferma + dati trasmissione
052900020301      *----------------------------------------------------------
053000020301     c     sr_f06        begsr
053100020301
053200060707     c                   eval      kkut = 1
053300020307     c                   eval      kcod = '15'
053400100122     c                   movel(P)  v1cnaz        kkey
053500020307     c     ktab          chain(n)  tabel00f
053600020307     c                   if        %found
053700020301     c                   eval      w1ftt = tblftt
053800020301     c                   eval      w1flt = tblflt
053900020301     c                   eval      w1ftr = tblftr
054000020301
054100020301     c                   move      tbldtr        dataR
054200020301     c                   eval      aad = aar
054300020301     c                   eval      mmd = mmr
054400020301     c                   eval      ggd = ggr
054500020301     c                   move      dataD         w1dtr
054600020307     c                   else
054700020307     c                   clear                   w1ftt
054800020307     c                   clear                   w1flt
054900020307     c                   clear                   w1ftr
055000020307     c                   clear                   w1dtr
055100020307     c                   endif
055200020301
055300020301     c                   do        *hival
055400020301     c                   exfmt     tb86w1
055500020301      * F12 - Ritorno
055600020301     c                   if        *inkl
055700020301     c                   leave
055800020301     c                   endif
055900020301
056000020301      * F6 - Conferma
056100020301     c                   if        *inkf
056200020301     c                   exsr      scrivi
056300020301     c                   eval      wf6 = *on
056400020301     c                   leave
056500020301     c                   endif
056600020301
056700020301     c                   enddo
056800020301
056900020301     c                   endsr
057000020301
057100020206      *----------------------------------------------------------
057200020206      *    Scrittura - Aggiornamento
057300020206      *----------------------------------------------------------
057400020206     c     scrivi        begsr
057500020206
057600020206      * Passo dati alla ds15
057700020206     c                   eval      §15des = v2des
057800020206     c                   eval      §15ecf = v2ecf
057900020206     c                   eval      §15pcf = v2pcf
058000020206     c                   eval      §15ucf = v2ucf
058100020206     c                   eval      §15vlt = v2vlt
058200020206     c                   eval      §15iso = v2iso
058300020206     c                   eval      §15cap = v2cap
058400020206     c                   eval      §15eft = v2eft
058500020206     c                   eval      §15ita = v2ita
058600020206     c                   eval      §15cpr = v2cpr
058700020206     c                   eval      §15cod = v2cod
058800020206     c                   eval      §15cie = v2cie
058900020206     c                   eval      §15cpp = v2cpp
059000020206     c                   eval      §15lad = v2lad
059100060919     c                   eval      §15red = v2red
059200020206     c                   eval      §15efd = v2efd
059300020206     c                   eval      §15cei = v2cei
059400020206     c                   eval      §15dog = v2dog
059500020206     c                   eval      §15laf = v2laf
059600020206     c                   eval      §15sto = v2sto
059700020206     c                   eval      §15eff = v2eff
059800020206     c                   eval      §15cts = v2cts
059900020206     c                   eval      §15inl = v2inl
060000020206     c                   eval      §15ffn = v2ffn
060100020206     c                   eval      §15eex = v2eex
060200020412     c                   eval      §15acn = v2acn
060300080604     c                   move      v2DPDpoe      §15dpdPOE
060400020311     c                   movel     v2ttc         §15ttc
060500060209     c                   eval      §15vas = v2vas
060600100115     c                   eval      §15ue  = v2ue
060700080423     c***                eval      §15cex = v2caeex
060800080423     c***                eval      §15cdp = v2cadpd
060900080423     c***                eval      §15cfe = v2cafed
061000020206
061100060707     c                   eval      kkut = 1
061200020307     c                   eval      kcod = '15'
061300100122     c                   movel(P)  v1cnaz        kkey
061400020206     c     ktab          chain     tabel00f
061500020206
061600020206     c                   movel     ds15          tbluni
061700020301
061800020301     c                   if        wf16 = *on
061900020301     c                   eval      tblflg = '*'
062000020301     c                   endif
062100020301
062200020301     c                   if        wf5 = *on
062300020301     c                   clear                   tblflg
062400020301     c                   endif
062500020206
062600020301     c                   eval      tblftt = w1ftt
062700020206     c                   clear                   tblftr
062800020206     c                   clear                   tbldtr
062900020206
063000020206      * scrivo
063100020206     c                   if        not %found
063200060707     c                   eval      tblkut = kkut
063300020206     c                   eval      tblcod = kcod
063400020206     c                   eval      tblkey = kkey
063500020206     c                   write     tabel
063600020206
063700020206      * aggiorno
063800020206     c                   else
063900020206     c                   update    tabel
064000020206     c                   endif
064100060707
064200060707      * se annullamento o ripristino devo fare la stessa cosa anche
064300060707      * sulle tabelle in lingua
064400060707     c                   if        wf16 = *on or wf5 = *on
064500060707      * aggancio le lingue presenti su AZLIN
064600060707     c                   move      kkut          kazlin
064700060707     c     kazlin        setgt     azlin01l
064800060707     c                   do        *hival
064900060707     c                   read      azlin01l
065000060707     c                   if        %eof(azlin01l)
065100060707     c                   leave
065200060707     c                   endif
065300060707      * per ogni lingua aggiorno
065400060707     c                   move      lintabel      kkut
065500060707     c                   eval      kcod = '15'
065600100122     c                   movel(P)  v1cnaz        kkey
065700060707     c     ktab          chain     tabel00f
065800060707     c                   if        %found(tabel00f)
065900060707     c                   if        wf16 = *on
066000060707     c                   eval      tblflg = '*'
066100060707     c                   endif
066200060707     c                   if        wf5 = *on
066300060707     c                   clear                   tblflg
066400060707     c                   endif
066500060707     c                   eval      tblftt = w1ftt
066600060707     c                   clear                   tblftr
066700060707     c                   clear                   tbldtr
066800060707     c                   update    tabel
066900060707     c                   endif
067000060707     c                   enddo
067100060707     c                   endif
067200020206
067300020206     c                   endsr
067400060707
067500060707      *---------------------------------------------------------------*
067600060707      * gestione tabella in lingua
067700060707      *---------------------------------------------------------------*
067800060707     c     sr_lingua     begsr
067900060707
068000060707      * pulisco il subfile
068100060707     c                   exsr      sr_pulsfl
068200060707      * carico il subfile con i dati che trovo o vuoto
068300060707     c                   exsr      sr_carsfl
068400060707      * emetto il subfile
068500060707     c     emisfl        tag
068600060707     c                   eval      rec = 1
068700060707     c                   if        savrec > 0
068800060707     c                   eval      rec = savrec
068900060707     c                   endif
069000060707     c                   write     tb86z1
069100060707     c                   exfmt     tb86c1
069200060707
069300060707      * F12 - Ritorno
069400060707     c                   if        *inkl
069500060707     c                   setoff                                       28
069600060707     c                   clear                   v1cmsg
069700060707     c                   goto      inz02
069800060707     c                   endif
069900060707
070000060707     c                   setoff                                       28
070100060707     c                   setoff                                       39
070200060707      * Controlli
070300060707     c  n05              exsr      sr_ctrsfl
070400060707     c   28
070500060707     cornkf              goto      emisfl
070600060707
070700060707      * F6 - Conferma
070800060707     c   kf              exsr      sr_confsfl
070900060707
071000060707     c                   endsr
071100060707
071200060707      *---------------------------------------------------------------*
071300060707      * pulisco il subfile
071400060707      *---------------------------------------------------------------*
071500060707     c     sr_pulsfl     begsr
071600060707
071700060707     c                   clear                   nrr
071800060707     c                   clear                   savrec
071900060707     c                   eval      *in20 = *off
072000060707     c                   eval      *in21 = *off
072100060707     c                   write     tb86c1
072200060707     c                   eval      *in20 = *on
072300060707     c                   eval      *in21 = *on
072400060707
072500060707     c                   endsr
072600060707
072700060707      *---------------------------------------------------------------*
072800060707      * carico il subfile
072900060707      *---------------------------------------------------------------*
073000060707     c     sr_carsfl     begsr
073100060707
073200060707     c                   setoff                                       30
073300060707      * carico un record per ogni lingua inserite in AZLIN
073400060707     c                   move      '1'           kazlin
073500060707     c     kazlin        setgt     azlin01l
073600060707     c                   do        *hival
073700060707     c                   read      azlin01l
073800060707      * fine file
073900060707     c                   if        %eof(azlin01l)
074000060707     c                   leave
074100060707     c                   endif
074200060707     c                   eval      s1dlin = lindesita
074300060707     c                   clear                   ds15
074400060707     c                   move      lintabel      kkut
074500060707     c                   eval      kcod = '15'
074600100122     c                   clear                   kkey
074700060707     c                   eval      kkey = v1cnaz
074800060707     c     ktab          chain(n)  tabel00f
074900060707     c                   if        not %found(tabel00f)
075000060707     c                   clear                   s1cdes
075100060707     c                   else
075200060707     c                   eval      ds15 = tbluni
075300060707     c                   eval      s1cdes = §15des
075400060707     c                   endif
075500060707     c                   eval      s1hcdlin = kkut
075600060707     c                   add       1             nrr
075700060707     c                   write     tb86s1
075800060707     c                   enddo
075900060707
076000060707     c                   seton                                        30
076100060707
076200060707     c                   endsr
076300060707
076400060707      *---------------------------------------------------------------*
076500060707      * controllo il subfile
076600060707      *---------------------------------------------------------------*
076700060707     c     sr_ctrsfl     begsr
076800060707
076900060707     c                   clear                   nrr
077000060707     c                   do        *hival
077100060707     c                   eval      nrr = nrr +1
077200060707     c     nrr           chain     tb86s1                             32
077300060707     c                   if        *in32
077400060707     c                   leave
077500060707     c                   endif
077600060707      * deve esserci la descrizione
077700060707     c                   if        s1cdes = *blanks
077800060707     c                   eval      v1cmsg = msg(01)
077900060707     c                   eval      *in28 = *on
078000060707     c                   eval      *in39 = *on
078100060707     c                   eval      savrec = nrr
078200060707     c                   update    tb86s1
078300060707     c                   leavesr
078400060707     c                   endif
078500060707     c                   update    tb86s1
078600060707     c                   enddo
078700060707
078800060707     c                   endsr
078900060707
079000060707      *---------------------------------------------------------------*
079100060707      * confermo i dati del subfile
079200060707      *---------------------------------------------------------------*
079300060707     c     sr_confsfl    begsr
079400060707
079500060707      * recupero i dati della tabella in italiano
079600060707     c                   clear                   savtbluni
079700060707     c                   eval      kkut = 1
079800060707     c                   eval      kcod = '15'
079900100122     c                   movel(P)  v1cnaz        kkey
080000060707     c     ktab          chain(n)  tabel00f
080100060707     c                   if        %found
080200060707     c                   eval      savtbluni = tbluni
080300060707     c                   endif
080400060707
080500060707      * leggo il subfile e aggiorno/scrivo i dati in lingua
080600060707     c                   clear                   nrr
080700060707     c                   do        *hival
080800060707     c                   eval      nrr = nrr +1
080900060707     c     nrr           chain     tb86s1                             32
081000060707     c                   if        *in32
081100060707     c                   leave
081200060707     c                   endif
081300060707     c                   eval      kkut = s1hcdlin
081400060707     c     ktab          chain     tabel00f
081500060707     c                   eval      ds15 = savtbluni
081600060707     c                   eval      §15des = s1cdes
081700060707     c                   eval      tbluni = ds15
081800060707     c                   eval      tblftt = w1ftt
081900060707     c                   clear                   tblftr
082000060707     c                   clear                   tbldtr
082100060707      * scrivo
082200060707     c                   if        not %found(tabel00f)
082300060707     c                   eval      tblkut = kkut
082400060707     c                   eval      tblcod = kcod
082500060707     c                   eval      tblkey = kkey
082600060707     c                   write     tabel
082700060707      * aggiorno
082800060707     c                   else
082900060707     c                   update    tabel
083000060707     c                   endif
083100060707
083200060707     c                   enddo
083300060707
083400060707     c                   endsr
083500020206      *-------------------------------------------------------------------------
083600020206** MSG  Lungh. 78                                                            *
083700020312Descrizione obbligatoria                                                      1
083800020312Codice Iso obbligatorio                                                       2
083900020312Divisa obbligatoria                                                           3
084000020312Codice iso per partita iva obbligatorio                                       4
084100020312Se Nazione senza cappario non richiedere il controllo cap/provincia           5
084200020312Linea arrivo DPD: inserire flag attraversamento dogana                        6
084300020312Linea arrivo FedEx: inserire flag attraversamento dogana                      7
084400020312Linea arrivo FedEx: inserire codice di tassazione                             8
084500020312Linea arrivo FedEx: inserire flag inoltro                                     9
084600020312Flag inoltro errato                                                           10
084700020312Divisa errata                                                                 11
084800020312Immettere il tipo tempo consegna FedEx                                        12
084900020312Tipo tempo consegna FedEx errato                                              13
085000080605I P.O.Emissione attualmente gestiti sono 190 e 195                            14
085100100122Se nazione UE il codice ISO per part.IVA non deve essere "$$". Enter forza    15
085200100122Non trovata la tabella del codice ISO. Inserire la "4I". enter forza          16
085300100122Aggionare anche tabella del codice ISO("4I") per appartenenza UE.Enter forza  17
085400100122Rendere congruente flag appartenenza UE con quello del codice ISO("4I").Forza 18
085500131218Codice Iso non valido                                                         19
