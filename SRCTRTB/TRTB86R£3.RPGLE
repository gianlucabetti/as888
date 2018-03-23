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
003000140606      * 53 - ERRORE obbligatorietà num.pref.telefonico internazionale
003100020206      ***********************************************************
003200020206
003300020131     FTRTB86D   CF   E             WORKSTN
003400060707     f                                     sfile(tb86s1:nrr)
003500891030     FTABEL00F  UF A E           K DISK
003600131218     Ftntbe01l  iF   E           K DISK
003700060707     fazlin01l  if   e           k disk
003800020206
003900060707     d kazlin          s                   like(lintabel)
004000020206     D kcod            S                   LIKE(tblcod)
004100020206     D kkey            S                   LIKE(tblkey)
004200060707     d kkut            s                   like(tblkut)
004300131218     d ktcod           s                   like(tbecod) inz('NAS')
004400131218     d ktke1           s                   like(tbeke1)
004500131218     d ktke2           s                   like(tbeke2)
004600020206
004700020206     D ordina          S              1
004800020206     D comando         S              1
004900020301     d wf6             s              1    inz('0')
005000020301     d wf16            s              1    inz('0')
005100020301     d wf5             s              1    inz('0')
005200060707
005300060707     d savtblftt       s                   like(tblftt)
005400060707     d savtblflt       s                   like(tblflt)
005500060707     d savtbluni       s                   like(tbluni)
005600060707     d nrr             s              4  0
005700060707     d savrec          s                   like(nrr)
005800020206
005900140606     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
006000020301
006100020301     D DataR           DS
006200020301     D  aaR                    1      2
006300020301     D  mmR                    3      4
006400020411     D  ggR                    5      6
006500020301
006600020301     D DataD           DS
006700020301     D  ggD                    1      2
006800020301     D  mmD                    3      4
006900020411     D  aaD                    5      6
007000020206
007100020206     D Video2        E DS                  extname(trtb86d:tb86d2)
007200020206
007300120620     d trtb09ds      e ds
007400020131     D DS15          E DS
007500100122     D DS4I          E DS
007600020312     d tisi00ds      e ds
007700080423     d tisiq0ds      e ds
007800020206     D UT§DSE0F      E DS
007900891030     D KPJBA         E DS
008000020206     D  rem                    1      3
008100100122
008200100122     d forzaiso        s              1
008300100122     d forzaUE         s              1
008400020206      *----------------------------------------------------------
008500020206
008600020206     c     *entry        plist
008700020206     c                   parm                    kpjba
008800020206
008900020206     c                   eval      codut = 1
009000020206     c                   call      'XPARUT'
009100020206     c                   parm                    ut§dse0f
009200020206     c                   movel     ragut         rsut
009300020206
009400020206     c     ktab          klist
009500060707     c                   kfld                    kkut
009600020206     c                   kfld                    kcod
009700020206     c                   kfld                    kkey
009800131218     c     ktbe          klist
009900131218     c                   kfld                    ktcod
010000131218     c                   kfld                    ktke1
010100131218     c                   kfld                    ktke2
010200020206
010300020206      * Prima videata: richiesta codice tabella
010400020206
010500020206     c     inizio        tag
010600020206
010700020206     c                   eval      *in01 = *off
010800020206     c                   eval      *in02 = *off
010900020206     c                   eval      *in03 = *off
011000020301     c                   eval      wf6 = *off
011100020301     c                   eval      wf16 = *off
011200020301     c                   eval      wf5 = *off
011300020206
011400020206     c                   if        v1cnaz = *blanks
011500100122     c                   eval      v1cnaz = '?  '
011600020206     c                   endif
011700020206
011800020206     c                   exfmt     tb86d1
011900020206
012000020206      * F3 - Fine
012100020206     c   kc              goto      fine
012200020206
012300020206      * Controllo codice tabella
012400020206     c                   exsr      ctrcod
012500020206     c   30              goto      inizio
012600020301
012700020206
012800020206      * Seconda videata: dati tabella
012900020206     c                   do        *hival
013000020206
013100060707     c     inz02         tag
013200020206     c                   exfmt     tb86d2
013300020206     c                   eval      *in28 = *off
013400130925     c                   clear                   V2Cmsg
013500020206
013600020206      * F3 - Fine
013700020206     c   kc              goto      fine
013800060707      * F9 - Traduzione
013900060707     c                   if        *inki
014000060707     c                   eval      *in05 = *on
014100060707     c                   exsr      sr_lingua
014200060707     c                   eval      *in05 = *off
014300060707     c                   iter
014400060707     c                   endif
014500020301
014600020301      * F6 - Conferma
014700020301     c                   if        *inkf
014800020301     c                   exsr      ctrd02
014900020301     c   28              iter
015000020301     c                   exsr      sr_f06
015100020301     c                   if        wf6 = *on
015200060707      * tabelle in lingua
015300060707     c                   eval      *in05 = *off
015400060707     c                   exsr      sr_lingua
015500020301     c                   clear                   v1cnaz
015600020301     c                   goto      inizio
015700020301     c                   else
015800020301     c                   iter
015900020301     c                   endif
016000020301     c                   endif
016100020301
016200020301      * F10 - Decodifica struttura cap
016300020301     c                   if        *inkj
016400020301     c                   exsr      sr_f10
016500020301     c                   endif
016600080423      * F15 - Cappario utilizzi particolari
016700080423     c                   if        *inkp
016800080423     c                   clear                   tisiq0ds
016900080423     c                   MOVEL     'C05'         dq0op0
017000080423     c                   clear                   dq0op1
017100080423     c                   MOVEL     '0'           dq0f03
017200080423     c                   MOVEL     '0'           dq0f12
017300080423     c                   MOVEL     '0'           dq0err
017400080423     c                   call      'TISIQ0R'
017500080423     c                   parm                    kpjba
017600080423     c                   parm                    tisiQ0ds
017700080423     c                   iter
017800080423     c                   endif
017900020206
018000020206      * F12 - Ritorno
018100020206     c                   if        *inkl
018200020206     c                   clear                   v1cnaz
018300020206     c                   goto      inizio
018400020206     c                   endif
018500020206
018600020206      * F16 - Annullamento
018700020206     c                   if        *inkq
018800020301     c                   eval      wf16 = *on
018900020301     c                   exsr      ctrd02
019000020301     c   28              iter
019100020301     c                   exsr      sr_f06
019200020301     c                   if        wf6 = *on
019300020301     c                   clear                   v1cnaz
019400020301     c                   goto      inizio
019500020301     c                   else
019600020301     c                   eval      wf16 = *off
019700020301     c                   iter
019800020301     c                   endif
019900020206     c                   endif
020000020206
020100020206      * F5 - Ripristino
020200020206     c                   if        *inke
020300020301     c                   eval      wf5 = *on
020400020301     c                   exsr      ctrd02
020500020301     c   28              iter
020600020301     c                   exsr      sr_f06
020700020301     c                   if        wf6 = *on
020800020301     c                   clear                   v1cnaz
020900020301     c                   goto      inizio
021000020301     c                   else
021100020301     c                   eval      wf5 = *off
021200020301     c                   iter
021300020301     c                   endif
021400020206     c                   endif
021500020206
021600020206      * Controlli
021700020206     c                   exsr      ctrd02
021800020206     c   28              iter
021900020206
022000020206     c                   enddo
022100020206
022200020206     c     fine          tag
022300020206     c                   eval      *inlr = *on
022400020206      *----------------------------------------------------------
022500020206      *    Controllo Codice
022600020206      *----------------------------------------------------------
022700020206     c     ctrcod        begsr
022800020307
022900060707     c                   eval      kkut = 1
023000020307     c                   movel     '15'          kcod
023100020206
023200020206      *   Ricerca tabella
023300020206     c     '?'           scan      v1cnaz                                 30
023400020206     c                   if        *in30
023500020206     c                   call      'TRUL19R'
023600020206     c                   parm                    kcod
023700020206     c                   parm                    ordina
023800020206     c                   parm                    kkey
023900020206     c                   parm                    comando
024000020206     c                   movel     kkey          v1cnaz
024100020206     c                   goto      endctrcod
024200020206     c                   endif
024300020206
024400020206     c                   movel     v1cnaz        kkey
024500020206
024600020206     c     ktab          chain     tabel00f
024700020206      * Immissione
024800020206     c                   if        not %found
024900020206     c                   eval      *in01 = *on
025000020206     c                   else
025100020206     c                   eval      ds15 = tbluni
025200020206     c                   if        tblflg <> *blanks
025300020206      * Annullato
025400020206     c                   eval      *in03 = *on
025500020206     c                   else
025600020206      * Aggiornamento
025700020206     c                   eval      *in02 = *on
025800020206     c                   endif
025900020206     c                   endif
026000020206
026100020206     c                   clear                   video2
026200020206      * Passo dati al video
026300020206     c                   if        not *in01
026400020206     c                   eval      v2des = §15des
026500020206     c                   eval      v2ecf = §15ecf
026600020206     c                   eval      v2pcf = §15pcf
026700020206     c                   eval      v2ucf = §15ucf
026800020206     c                   eval      v2vlt = §15vlt
026900020206     c                   eval      v2iso = §15iso
027000020206     c                   eval      v2cap = §15cap
027100020206     c                   eval      v2eft = §15eft
027200020206     c                   eval      v2ita = §15ita
027300020206     c                   eval      v2cpr = §15cpr
027400020206     c                   eval      v2cod = §15cod
027500020206     c                   eval      v2cie = §15cie
027600020206     c                   eval      v2cpp = §15cpp
027700020206     c                   eval      v2lad = §15lad
027800140606     c                   eval      v2pref= §15pref
027900060919     c                   eval      v2red = §15red
028000020206     c                   eval      v2efd = §15efd
028100020206     c                   eval      v2cei = §15cei
028200020206     c                   eval      v2dog = §15dog
028300020206     c                   eval      v2laf = §15laf
028400020206     c                   eval      v2sto = §15sto
028500020206     c                   eval      v2eff = §15eff
028600020206     c                   eval      v2cts = §15cts
028700020206     c                   eval      v2inl = §15inl
028800020206     c                   eval      v2ffn = §15ffn
028900020206     c                   eval      v2eex = §15eex
029000020412     c                   eval      v2acn = §15acn
029100080604     c                   move      §15dpdPOE     V2dpdPOE
029200020311     c                   movel     §15ttc        v2ttc
029300060209     c                   eval      v2vas = §15vas
029400100115     c                   eval      v2ue  = §15ue
029500100122     c
029600100122     c                   clear                   forzaiso
029700100122     c                   clear                   forzaUE
029800100122     c
029900080423     c**                 eval      v2caeex = §15cex
030000080423     c**                 eval      v2cadpd = §15cdp
030100080423     c**                 eval      v2cafed = §15cfe
030200020206     c                   endif
030300020206
030400020206     c     endctrcod     endsr
030500020206
030600020206      *----------------------------------------------------------
030700020206      *    Controllo Seconda videata
030800020206      *----------------------------------------------------------
030900020206     c     ctrd02        begsr
031000020312
031100020312     c                   setoff                                       404142
031200020312     c                   setoff                                       434445
031300020312     c                   setoff                                       464748
031400020312     c                   setoff                                       49
031500140606     c                   setoff                                       53
031600020206
031700020206      * Descrizione
031800020206     c                   if        v2des = *blanks
031900020206     c                   movel     msg(1)        v2cmsg
032000020206     c                   eval      *in28 = *on
032100020206     c                   eval      *in40 = *on
032200020206     c                   goto      endctrd02
032300020206     c                   endif
032400020307
032500140606      * Pref.Telef.Internazionale  obbligatorio x naz.con DPD
032600140606     c                   if         v2pref = *blanks
032700140606     c                   if          v2lad  > 0        or
032800140606     c                               v2red <> *blanks  or
032900140606     c                               v2dpdpoe > 0
033000140606     c                   movel     msg(20)       v2cmsg
033100140606     c                   eval      *in28 = *on
033200140606     c                   eval      *in53 = *on
033300140606     c                   goto      endctrd02
033400140606     c                   end
033500140606      *  se inserito qualcosa nel campo non deve iniziare x (0)
033600140606     c                   else
033700140606     c                   if        %subst(%TrimL(V2pref):1:1) = '0'
033800140606     c                                 or
033900140606     c                             %subst(%TrimL(V2pref):1:1) = '+'
034000140606     c                   movel     msg(20)       v2cmsg
034100140606     c                   eval      *in28 = *on
034200140606     c                   eval      *in53 = *on
034300140606     c                   goto      endctrd02
034400140606     c                   end
034500140606     c                   endif
034600140606
034700020307      * Codice Iso
034800020307     c                   if        v2cod = *blanks
034900131218     c                   movel     msg(2)        v2cmsg
035000020307     c                   eval      *in28 = *on
035100020307     c                   eval      *in41 = *on
035200020307     c                   goto      endctrd02
035300020307     c                   endif
035400131218     c                   eval      ktke1='ALPHA-2 '
035500131218     c                   eval      ktke2=v2cod
035600131218     c     ktbe          chain     tntbe01l
035700131218     c                   if        not %found(tntbe01l)
035800131218     c                   movel     msg(19)       v2cmsg
035900131218     c                   eval      *in28 = *on
036000131218     c                   eval      *in41 = *on
036100131218     c                   goto      endctrd02
036200131218     c                   endif
036300131218     c
036400060426
036500060426      * Codice Iso Numerico
036600060426     c                   if        v2cie = *zeros
036700060426     c                   movel     msg(2)        v2cmsg
036800060426     c                   eval      *in28 = *on
036900060426     c                   eval      *in50 = *on
037000060426     c                   goto      endctrd02
037100060426     c                   endif
037200131218     c                   eval      ktke1='NUMERICO'
037300131218     c                   movel(p)  v2cie         ktke2
037400131218     c     ktbe          chain     tntbe01l
037500131218     c                   if        not %found(tntbe01l)
037600131218     c                   movel     msg(19)       v2cmsg
037700131218     c                   eval      *in28 = *on
037800131218     c                   eval      *in50 = *on
037900131218     c                   goto      endctrd02
038000131218     c                   endif
038100020307
038200020307      * Divisa
038300020312 b1  c                   if        v2vlt = *blanks
038400020307     c                   movel     msg(3)        v2cmsg
038500020307     c                   eval      *in28 = *on
038600020307     c                   eval      *in42 = *on
038700020307     c                   goto      endctrd02
038800020312 x1  c                   else
038900020311      *   ricerca
039000060707     c                   eval      kkut = 1
039100020311     c                   eval      kcod = 'CV'
039200020311     c                   clear                   kkey
039300020311     c     '?'           scan      v2vlt                                  30
039400020312 b2  c                   if        *in30
039500020311     c                   call      'TRUL19R'
039600020311     c                   parm                    kcod
039700020311     c                   parm                    ordina
039800020311     c                   parm                    kkey
039900020311     c                   parm                    comando
040000020311     c                   movel     kkey          v2vlt
040100020311     c                   goto      endctrd02
040200020312 e2  c                   endif
040300020311      *   controllo
040400100122     c                   movel(P)  v2vlt         kkey
040500020311     c     ktab          chain(n)  tabel00f
040600020312 b2  c                   if        not %found
040700020311     c                   movel     msg(11)       v2cmsg
040800020311     c                   eval      *in28 = *on
040900020311     c                   eval      *in42 = *on
041000020311     c                   goto      endctrd02
041100020312 e2  c                   endif
041200020312 e1  c                   endif
041300020307
041400020307      * Codice Iso per partita iva
041500020307     c                   if        v2iso = *blanks
041600020307     c                   movel     msg(4)        v2cmsg
041700020307     c                   eval      *in28 = *on
041800020307     c                   eval      *in43 = *on
041900020307     c                   goto      endctrd02
042000020307     c                   endif
042100100122
042200100122      * Appartenenza a U E
042300100122    1c                   if        v2ue='S'
042400100122     c* il codice iso per partita iva non deve essere $$ --> errore forzabile
042500100122    2c                   if        v2iso='$$' and forzaiso=' '
042600100122     c                   movel     msg(15)       v2cmsg
042700100122     c                   eval      *in28 = *on
042800100122     c                   eval      *in52 = *on
042900100122     c                   eval      forzaiso='1'
043000100122     c                   goto      endctrd02
043100100122    2c                   endif
043200100122    1c                   endif
043300100122     c
043400100122     c* Verifico se presente flag UE anche nella 4I errore forzabile
043500100122    1c                   if        forzaue=' '
043600100122     c                   eval      kcod = '4I'
043700100122     c                   movel(P)  v2iso         kkey
043800100122     c     ktab          chain(n)  tabel00f
043900100122    2c                   if        not %found
044000100122     c                   movel     msg(16)       v2cmsg
044100100122     c                   else
044200100122     c                   movel     tbluni        ds4i
044300100122
044400100122    3c                   if        §4iue<>v2ue
044500100122    4c                   if        v2ue='S'
044600100122     c                   movel     msg(17)       v2cmsg
044700100122     c                   else
044800100122     c                   movel     msg(18)       v2cmsg
044900100122    4c                   endif
045000100122    3c                   endif
045100100122    2c                   endif
045200100122     c* Errore
045300100122    2c                   if        v2cmsg<>*blanks
045400100122     c                   eval      *in28 = *on
045500100122     c                   eval      *in52 = *on
045600100122     c                   eval      forzaue ='1'
045700100122     c                   goto      endctrd02
045800100122    2c                   endif
045900100122    1c                   endif
046000020206
046100020206      * Cap
046200020206     c                   if        v2cpp = 'N' and v2cpr = 'S'
046300020307     c                   movel     msg(5)        v2cmsg
046400020206     c                   eval      *in28 = *on
046500020307     c                   eval      *in44 = *on
046600020206     c                   goto      endctrd02
046700020206     c                   endif
046800020307
046900080605      * DPD P.O.Emissione
047000080605     c                   if        v2dpdPOE <> 0   and
047100080605     c                             v2dpdPOE <> 190 and
047200080605     c                             v2dpdPOE <> 195
047300080605     c                   movel     msg(14)       v2cmsg
047400080605     c                   eval      *in28 = *on
047500080605     c                   eval      *in51 = *on
047600080605     c                   goto      endctrd02
047700080605     c                   endif
047800020307
047900020307      * FedEx
048000020312 b1  c                   if        v2laf <> *zeros
048100020307      * codice tassazione
048200120620     c                   if        %scan('?':v2cts) > 0
048300120620     c                   clear                   TRTB09DS
048400120620     c                   eval      TB09fun = '1'
048500120713     c                   eval      TB09ord = '1'
048600120713     c                   eval      TB09utf = 'S'
048700120620     c                   eval      kpjbu = TRTB09DS
048800120620     c                   call      'TRTB09R'
048900120620     c                   parm                    kpjba
049000120620     c                   eval      TRTB09DS = kpjbu
049100120620     c                   IF        TB09cod <> *blanks
049200120620     c                   eval      v2cts = TB09cod
049300120620     c                   ENDIF
049400120620     c                   endif
049500020312 b2  c                   if        v2cts = *blanks
049600020307     c                   movel     msg(8)        v2cmsg
049700020307     c                   eval      *in28 = *on
049800020307     c                   eval      *in47 = *on
049900020307     c                   goto      endctrd02
050000020312 e2  c                   endif
050100020307      * flag inoltro
050200020312 b2  c                   if        v2inl = *blanks
050300020307     c                   movel     msg(9)        v2cmsg
050400020307     c                   eval      *in28 = *on
050500020307     c                   eval      *in48 = *on
050600020307     c                   goto      endctrd02
050700020312 x2  c                   else
050800020311      *   ricerca
050900060707     c                   eval      kkut = 1
051000020307     c                   eval      kcod = '9A'
051100020307     c                   clear                   kkey
051200020307     c     '?'           scan      v2inl                                  30
051300020312 b3  c                   if        *in30
051400020307     c                   call      'TRUL19R'
051500020307     c                   parm                    kcod
051600020307     c                   parm                    ordina
051700020307     c                   parm                    kkey
051800020307     c                   parm                    comando
051900020307     c                   movel     kkey          v2inl
052000020307     c                   goto      endctrd02
052100020312 e3  c                   endif
052200020311      *   controllo
052300100122     c                   movel(P)  v2inl         kkey
052400020307     c     ktab          chain(n)  tabel00f
052500020312 b3  c                   if        not %found
052600020307     c                   movel     msg(10)       v2cmsg
052700020307     c                   eval      *in28 = *on
052800020307     c                   eval      *in48 = *on
052900020307     c                   goto      endctrd02
053000020312 e3  c                   endif
053100020312 e2  c                   endif
053200020311      * tipo tempo consegna
053300020312 b2  c                   if        v2ttc = *blanks
053400020311     c                   movel     msg(12)       v2cmsg
053500020311     c                   eval      *in28 = *on
053600020311     c                   eval      *in49 = *on
053700020311     c                   goto      endctrd02
053800020312 x2  c                   else
053900020311      *   ricerca
054000020311     c     '?'           scan      v2ttc                                  30
054100020312 b3  c                   if        *in30
054200020312     c                   clear                   tisi00ds
054300020312     c                   eval      d00op0 = 'T01'
054400020312     c                   eval      d00f03 = *off
054500020312     c                   eval      d00f12 = *off
054600020312     c                   eval      d00err = *off
054700020311     c                   call      'TISI41R'
054800020311     c                   parm                    kpjba
054900020312     c                   parm                    tisi00ds
055000020312 b4  c                   if        d00f03 = *on or d00f12 = *on or
055100020312     c                             d00err = *on
055200020312     c                   goto      endctrd02
055300020312 e4  c                   endif
055400020312     c                   movel     d00ttc        v2ttc
055500020312     c                   goto      endctrd02
055600020312 e3  c                   endif
055700020312 e2  c                   endif
055800020312 e1  c                   endif
055900020206
056000020206     c     endctrd02     endsr
056100020206
056200020301      *----------------------------------------------------------
056300020301      *    Decodifica struttura cap
056400020301      *----------------------------------------------------------
056500020301     c     sr_f10        begsr
056600020301
056700020301     c                   do        *hival
056800020304     c                   exfmt     tb86w2
056900020301      * F12 - Ritorno
057000020301     c                   if        *inkl
057100020301     c                   leave
057200020301     c                   endif
057300020301
057400020301     c                   enddo
057500020304     c                   eval      *inkl = *off
057600020301
057700020301     c                   endsr
057800020301
057900020301      *----------------------------------------------------------
058000020301      *    Videata conferma + dati trasmissione
058100020301      *----------------------------------------------------------
058200020301     c     sr_f06        begsr
058300020301
058400060707     c                   eval      kkut = 1
058500020307     c                   eval      kcod = '15'
058600100122     c                   movel(P)  v1cnaz        kkey
058700020307     c     ktab          chain(n)  tabel00f
058800020307     c                   if        %found
058900020301     c                   eval      w1ftt = tblftt
059000020301     c                   eval      w1flt = tblflt
059100020301     c                   eval      w1ftr = tblftr
059200020301
059300020301     c                   move      tbldtr        dataR
059400020301     c                   eval      aad = aar
059500020301     c                   eval      mmd = mmr
059600020301     c                   eval      ggd = ggr
059700020301     c                   move      dataD         w1dtr
059800020307     c                   else
059900020307     c                   clear                   w1ftt
060000020307     c                   clear                   w1flt
060100020307     c                   clear                   w1ftr
060200020307     c                   clear                   w1dtr
060300020307     c                   endif
060400020301
060500020301     c                   do        *hival
060600020301     c                   exfmt     tb86w1
060700020301      * F12 - Ritorno
060800020301     c                   if        *inkl
060900020301     c                   leave
061000020301     c                   endif
061100020301
061200020301      * F6 - Conferma
061300020301     c                   if        *inkf
061400020301     c                   exsr      scrivi
061500020301     c                   eval      wf6 = *on
061600020301     c                   leave
061700020301     c                   endif
061800020301
061900020301     c                   enddo
062000020301
062100020301     c                   endsr
062200020301
062300020206      *----------------------------------------------------------
062400020206      *    Scrittura - Aggiornamento
062500020206      *----------------------------------------------------------
062600020206     c     scrivi        begsr
062700020206
062800020206      * Passo dati alla ds15
062900020206     c                   eval      §15des = v2des
063000020206     c                   eval      §15ecf = v2ecf
063100020206     c                   eval      §15pcf = v2pcf
063200020206     c                   eval      §15ucf = v2ucf
063300020206     c                   eval      §15vlt = v2vlt
063400020206     c                   eval      §15iso = v2iso
063500020206     c                   eval      §15cap = v2cap
063600020206     c                   eval      §15eft = v2eft
063700020206     c                   eval      §15ita = v2ita
063800020206     c                   eval      §15cpr = v2cpr
063900020206     c                   eval      §15cod = v2cod
064000020206     c                   eval      §15cie = v2cie
064100020206     c                   eval      §15cpp = v2cpp
064200020206     c                   eval      §15lad = v2lad
064300060919     c                   eval      §15red = v2red
064400020206     c                   eval      §15efd = v2efd
064500020206     c                   eval      §15cei = v2cei
064600020206     c                   eval      §15dog = v2dog
064700020206     c                   eval      §15laf = v2laf
064800020206     c                   eval      §15sto = v2sto
064900020206     c                   eval      §15eff = v2eff
065000020206     c                   eval      §15cts = v2cts
065100020206     c                   eval      §15inl = v2inl
065200020206     c                   eval      §15ffn = v2ffn
065300020206     c                   eval      §15eex = v2eex
065400020412     c                   eval      §15acn = v2acn
065500140606     c                   eval      §15pref= v2pref
065600080604     c                   move      v2DPDpoe      §15dpdPOE
065700020311     c                   movel     v2ttc         §15ttc
065800060209     c                   eval      §15vas = v2vas
065900100115     c                   eval      §15ue  = v2ue
066000080423     c***                eval      §15cex = v2caeex
066100080423     c***                eval      §15cdp = v2cadpd
066200080423     c***                eval      §15cfe = v2cafed
066300020206
066400060707     c                   eval      kkut = 1
066500020307     c                   eval      kcod = '15'
066600100122     c                   movel(P)  v1cnaz        kkey
066700020206     c     ktab          chain     tabel00f
066800020206
066900020206     c                   movel     ds15          tbluni
067000020301
067100020301     c                   if        wf16 = *on
067200020301     c                   eval      tblflg = '*'
067300020301     c                   endif
067400020301
067500020301     c                   if        wf5 = *on
067600020301     c                   clear                   tblflg
067700020301     c                   endif
067800020206
067900020301     c                   eval      tblftt = w1ftt
068000020206     c                   clear                   tblftr
068100020206     c                   clear                   tbldtr
068200020206
068300020206      * scrivo
068400020206     c                   if        not %found
068500060707     c                   eval      tblkut = kkut
068600020206     c                   eval      tblcod = kcod
068700020206     c                   eval      tblkey = kkey
068800020206     c                   write     tabel
068900020206
069000020206      * aggiorno
069100020206     c                   else
069200020206     c                   update    tabel
069300020206     c                   endif
069400060707
069500060707      * se annullamento o ripristino devo fare la stessa cosa anche
069600060707      * sulle tabelle in lingua
069700060707     c                   if        wf16 = *on or wf5 = *on
069800060707      * aggancio le lingue presenti su AZLIN
069900060707     c                   move      kkut          kazlin
070000060707     c     kazlin        setgt     azlin01l
070100060707     c                   do        *hival
070200060707     c                   read      azlin01l
070300060707     c                   if        %eof(azlin01l)
070400060707     c                   leave
070500060707     c                   endif
070600060707      * per ogni lingua aggiorno
070700060707     c                   move      lintabel      kkut
070800060707     c                   eval      kcod = '15'
070900100122     c                   movel(P)  v1cnaz        kkey
071000060707     c     ktab          chain     tabel00f
071100060707     c                   if        %found(tabel00f)
071200060707     c                   if        wf16 = *on
071300060707     c                   eval      tblflg = '*'
071400060707     c                   endif
071500060707     c                   if        wf5 = *on
071600060707     c                   clear                   tblflg
071700060707     c                   endif
071800060707     c                   eval      tblftt = w1ftt
071900060707     c                   clear                   tblftr
072000060707     c                   clear                   tbldtr
072100060707     c                   update    tabel
072200060707     c                   endif
072300060707     c                   enddo
072400060707     c                   endif
072500020206
072600020206     c                   endsr
072700060707
072800060707      *---------------------------------------------------------------*
072900060707      * gestione tabella in lingua
073000060707      *---------------------------------------------------------------*
073100060707     c     sr_lingua     begsr
073200060707
073300060707      * pulisco il subfile
073400060707     c                   exsr      sr_pulsfl
073500060707      * carico il subfile con i dati che trovo o vuoto
073600060707     c                   exsr      sr_carsfl
073700060707      * emetto il subfile
073800060707     c     emisfl        tag
073900060707     c                   eval      rec = 1
074000060707     c                   if        savrec > 0
074100060707     c                   eval      rec = savrec
074200060707     c                   endif
074300060707     c                   write     tb86z1
074400060707     c                   exfmt     tb86c1
074500060707
074600060707      * F12 - Ritorno
074700060707     c                   if        *inkl
074800060707     c                   setoff                                       28
074900060707     c                   clear                   v1cmsg
075000060707     c                   goto      inz02
075100060707     c                   endif
075200060707
075300060707     c                   setoff                                       28
075400060707     c                   setoff                                       39
075500060707      * Controlli
075600060707     c  n05              exsr      sr_ctrsfl
075700060707     c   28
075800060707     cornkf              goto      emisfl
075900060707
076000060707      * F6 - Conferma
076100060707     c   kf              exsr      sr_confsfl
076200060707
076300060707     c                   endsr
076400060707
076500060707      *---------------------------------------------------------------*
076600060707      * pulisco il subfile
076700060707      *---------------------------------------------------------------*
076800060707     c     sr_pulsfl     begsr
076900060707
077000060707     c                   clear                   nrr
077100060707     c                   clear                   savrec
077200060707     c                   eval      *in20 = *off
077300060707     c                   eval      *in21 = *off
077400060707     c                   write     tb86c1
077500060707     c                   eval      *in20 = *on
077600060707     c                   eval      *in21 = *on
077700060707
077800060707     c                   endsr
077900060707
078000060707      *---------------------------------------------------------------*
078100060707      * carico il subfile
078200060707      *---------------------------------------------------------------*
078300060707     c     sr_carsfl     begsr
078400060707
078500060707     c                   setoff                                       30
078600060707      * carico un record per ogni lingua inserite in AZLIN
078700060707     c                   move      '1'           kazlin
078800060707     c     kazlin        setgt     azlin01l
078900060707     c                   do        *hival
079000060707     c                   read      azlin01l
079100060707      * fine file
079200060707     c                   if        %eof(azlin01l)
079300060707     c                   leave
079400060707     c                   endif
079500060707     c                   eval      s1dlin = lindesita
079600060707     c                   clear                   ds15
079700060707     c                   move      lintabel      kkut
079800060707     c                   eval      kcod = '15'
079900100122     c                   clear                   kkey
080000060707     c                   eval      kkey = v1cnaz
080100060707     c     ktab          chain(n)  tabel00f
080200060707     c                   if        not %found(tabel00f)
080300060707     c                   clear                   s1cdes
080400060707     c                   else
080500060707     c                   eval      ds15 = tbluni
080600060707     c                   eval      s1cdes = §15des
080700060707     c                   endif
080800060707     c                   eval      s1hcdlin = kkut
080900060707     c                   add       1             nrr
081000060707     c                   write     tb86s1
081100060707     c                   enddo
081200060707
081300060707     c                   seton                                        30
081400060707
081500060707     c                   endsr
081600060707
081700060707      *---------------------------------------------------------------*
081800060707      * controllo il subfile
081900060707      *---------------------------------------------------------------*
082000060707     c     sr_ctrsfl     begsr
082100060707
082200060707     c                   clear                   nrr
082300060707     c                   do        *hival
082400060707     c                   eval      nrr = nrr +1
082500060707     c     nrr           chain     tb86s1                             32
082600060707     c                   if        *in32
082700060707     c                   leave
082800060707     c                   endif
082900060707      * deve esserci la descrizione
083000060707     c                   if        s1cdes = *blanks
083100060707     c                   eval      v1cmsg = msg(01)
083200060707     c                   eval      *in28 = *on
083300060707     c                   eval      *in39 = *on
083400060707     c                   eval      savrec = nrr
083500060707     c                   update    tb86s1
083600060707     c                   leavesr
083700060707     c                   endif
083800060707     c                   update    tb86s1
083900060707     c                   enddo
084000060707
084100060707     c                   endsr
084200060707
084300060707      *---------------------------------------------------------------*
084400060707      * confermo i dati del subfile
084500060707      *---------------------------------------------------------------*
084600060707     c     sr_confsfl    begsr
084700060707
084800060707      * recupero i dati della tabella in italiano
084900060707     c                   clear                   savtbluni
085000060707     c                   eval      kkut = 1
085100060707     c                   eval      kcod = '15'
085200100122     c                   movel(P)  v1cnaz        kkey
085300060707     c     ktab          chain(n)  tabel00f
085400060707     c                   if        %found
085500060707     c                   eval      savtbluni = tbluni
085600060707     c                   endif
085700060707
085800060707      * leggo il subfile e aggiorno/scrivo i dati in lingua
085900060707     c                   clear                   nrr
086000060707     c                   do        *hival
086100060707     c                   eval      nrr = nrr +1
086200060707     c     nrr           chain     tb86s1                             32
086300060707     c                   if        *in32
086400060707     c                   leave
086500060707     c                   endif
086600060707     c                   eval      kkut = s1hcdlin
086700060707     c     ktab          chain     tabel00f
086800060707     c                   eval      ds15 = savtbluni
086900060707     c                   eval      §15des = s1cdes
087000060707     c                   eval      tbluni = ds15
087100060707     c                   eval      tblftt = w1ftt
087200060707     c                   clear                   tblftr
087300060707     c                   clear                   tbldtr
087400060707      * scrivo
087500060707     c                   if        not %found(tabel00f)
087600060707     c                   eval      tblkut = kkut
087700060707     c                   eval      tblcod = kcod
087800060707     c                   eval      tblkey = kkey
087900060707     c                   write     tabel
088000060707      * aggiorno
088100060707     c                   else
088200060707     c                   update    tabel
088300060707     c                   endif
088400060707
088500060707     c                   enddo
088600060707
088700060707     c                   endsr
088800020206      *-------------------------------------------------------------------------
088900020206** MSG  Lungh. 78                                                            *
089000020312Descrizione obbligatoria                                                      1
089100020312Codice Iso obbligatorio                                                       2
089200020312Divisa obbligatoria                                                           3
089300020312Codice iso per partita iva obbligatorio                                       4
089400020312Se Nazione senza cappario non richiedere il controllo cap/provincia           5
089500020312Linea arrivo DPD: inserire flag attraversamento dogana                        6
089600020312Linea arrivo FedEx: inserire flag attraversamento dogana                      7
089700020312Linea arrivo FedEx: inserire codice di tassazione                             8
089800020312Linea arrivo FedEx: inserire flag inoltro                                     9
089900020312Flag inoltro errato                                                           10
090000020312Divisa errata                                                                 11
090100020312Immettere il tipo tempo consegna FedEx                                        12
090200020312Tipo tempo consegna FedEx errato                                              13
090300080605I P.O.Emissione attualmente gestiti sono 190 e 195                            14
090400100122Se nazione UE il codice ISO per part.IVA non deve essere "$$". Enter forza    15
090500100122Non trovata la tabella del codice ISO. Inserire la "4I". enter forza          16
090600100122Aggionare anche tabella del codice ISO("4I") per appartenenza UE.Enter forza  17
090700100122Rendere congruente flag appartenenza UE con quello del codice ISO("4I").Forza 18
090800131218Codice Iso non valido                                                         19
090900140606Pref.tel.Inter.obbligatorio. NON deve avere zeri o il (+) alla sx. del cod.   20
