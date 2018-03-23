000100020307     H DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
000200020206
000300020206      ***********************************************************
000400020206      *
000500020206      * GESTIONE TABELLA NAZIONI
000600020206      *
000700160307      * ATTENZIONE - La tabella 16 (continuazione della 15)
000800160307      * non viene al momento duplicata nelle varie lingue
000900160307      * in quanto l'unico campo in essa presente è utilizzato solo
001000160307      * dal pgm di bollettazione.
001100020206      ***********************************************************
001200020206
001300020206      ***********************************************************
001400020206      *  RIEPILOGO INDICATORI
001500020206      ***********************************************************
001600020206      * 01 - INSERIMENTO
001700020206      * 02 - VARIAZIONE
001800020206      * 03 - ANNULLATO
001900020206      * 28 - ERRORE GENERICO DSPF
002000020206      * 30 - RICERCA TABELLA
002100020206      * 40 - ERRORE DESCRIZIONE
002200020307      * 41 - ERRORE CODICE ISO
002300020307      * 42 - ERRORE DIVISA
002400020307      * 43 - ERRORE ISO PER PARTITA IVA
002500020307      * 44 - ERRORE CAP
002600020307      * 45 - ERRORE ATTRAVERSAMENTO DOGANA DPD
002700020307      * 46 - ERRORE ATTRAVERSAMENTO DOGANA FEDEX
002800020307      * 47 - ERRORE CODICE TASSAZIONE FEDEX
002900020307      * 48 - ERRORE FLAG INOLTRO FEDEX
003000020311      * 49 - ERRORE TIPO TEMPO CONSEGNA FEDEX
003100100122      * 50 - ERRORE iso numerico
003200100122      * 51 - ERRORE orm automatici
003300100122      * 52 - ERRORE nazione UE
003400140606      * 53 - ERRORE obbligatorietà num.pref.telefonico internazionale
003500160304      * 54 - ERRORE peso limite per instradamento forzato da eex a dpd
003600020206      ***********************************************************
003700020206
003800020131     FTRTB86D   CF   E             WORKSTN
003900060707     f                                     sfile(tb86s1:nrr)
004000891030     FTABEL00F  UF A E           K DISK
004100131218     Ftntbe01l  iF   E           K DISK
004200060707     fazlin01l  if   e           k disk
004300020206
004400060707     d kazlin          s                   like(lintabel)
004500020206     D kcod            S                   LIKE(tblcod)
004600020206     D kkey            S                   LIKE(tblkey)
004700060707     d kkut            s                   like(tblkut)
004800131218     d ktcod           s                   like(tbecod) inz('NAS')
004900131218     d ktke1           s                   like(tbeke1)
005000131218     d ktke2           s                   like(tbeke2)
005100020206
005200020206     D ordina          S              1
005300020206     D comando         S              1
005400020301     d wf6             s              1    inz('0')
005500020301     d wf16            s              1    inz('0')
005600020301     d wf5             s              1    inz('0')
005700060707
005800060707     d savtblftt       s                   like(tblftt)
005900060707     d savtblflt       s                   like(tblflt)
006000060707     d savtbluni       s                   like(tbluni)
006100060707     d nrr             s              4  0
006200060707     d savrec          s                   like(nrr)
006300020206
006400160304     D MSG             S             78    DIM(22) CTDATA PERRCD(1)
006500020301
006600020301     D DataR           DS
006700020301     D  aaR                    1      2
006800020301     D  mmR                    3      4
006900020411     D  ggR                    5      6
007000020301
007100020301     D DataD           DS
007200020301     D  ggD                    1      2
007300020301     D  mmD                    3      4
007400020411     D  aaD                    5      6
007500020206
007600020206     D Video2        E DS                  extname(trtb86d:tb86d2)
007700020206
007800120620     d trtb09ds      e ds
007900020131     D DS15          E DS
008000160304     D DS16          E DS
008100100122     D DS4I          E DS
008200160304     D DS3IDP        E DS
008300020312     d tisi00ds      e ds
008400080423     d tisiq0ds      e ds
008500020206     D UT§DSE0F      E DS
008600891030     D KPJBA         E DS
008700020206     D  rem                    1      3
008800100122
008900100122     d forzaiso        s              1
009000100122     d forzaUE         s              1
009100020206      *----------------------------------------------------------
009200020206
009300020206     c     *entry        plist
009400020206     c                   parm                    kpjba
009500020206
009600020206     c                   eval      codut = 1
009700020206     c                   call      'XPARUT'
009800020206     c                   parm                    ut§dse0f
009900020206     c                   movel     ragut         rsut
010000020206
010100020206     c     ktab          klist
010200060707     c                   kfld                    kkut
010300020206     c                   kfld                    kcod
010400020206     c                   kfld                    kkey
010500131218     c     ktbe          klist
010600131218     c                   kfld                    ktcod
010700131218     c                   kfld                    ktke1
010800131218     c                   kfld                    ktke2
010900160304     C***
011000160304     C* CARICO TABELLA DPD
011100160304     C***
011200160304     c                   eval      kkut = 1
011300160304     C                   MOVEL     '3I'          kCOD
011400160304     C                   MOVEL(P)  'DPD'         kKEY
011500160304     C                   CLEAR                   DS3IDP
011600160304     c     ktab          chain(n)  tabel00f
011700160304     c                   if        %found(tabel00f)
011800160304     C                   MOVEL     TBLUNI        DS3IDP
011900160304     c                   endif
012000020206
012100020206      * Prima videata: richiesta codice tabella
012200020206
012300020206     c     inizio        tag
012400020206
012500020206     c                   eval      *in01 = *off
012600020206     c                   eval      *in02 = *off
012700020206     c                   eval      *in03 = *off
012800020301     c                   eval      wf6 = *off
012900020301     c                   eval      wf16 = *off
013000020301     c                   eval      wf5 = *off
013100020206
013200020206     c                   if        v1cnaz = *blanks
013300100122     c                   eval      v1cnaz = '?  '
013400020206     c                   endif
013500020206
013600020206     c                   exfmt     tb86d1
013700020206
013800020206      * F3 - Fine
013900020206     c   kc              goto      fine
014000020206
014100020206      * Controllo codice tabella
014200020206     c                   exsr      ctrcod
014300020206     c   30              goto      inizio
014400020301
014500020206
014600020206      * Seconda videata: dati tabella
014700020206     c                   do        *hival
014800020206
014900060707     c     inz02         tag
015000020206     c                   exfmt     tb86d2
015100020206     c                   eval      *in28 = *off
015200130925     c                   clear                   V2Cmsg
015300020206
015400020206      * F3 - Fine
015500020206     c   kc              goto      fine
015600060707      * F9 - Traduzione
015700060707     c                   if        *inki
015800060707     c                   eval      *in05 = *on
015900060707     c                   exsr      sr_lingua
016000060707     c                   eval      *in05 = *off
016100060707     c                   iter
016200060707     c                   endif
016300020301
016400020301      * F6 - Conferma
016500020301     c                   if        *inkf
016600020301     c                   exsr      ctrd02
016700020301     c   28              iter
016800020301     c                   exsr      sr_f06
016900020301     c                   if        wf6 = *on
017000060707      * tabelle in lingua
017100060707     c                   eval      *in05 = *off
017200060707     c                   exsr      sr_lingua
017300020301     c                   clear                   v1cnaz
017400020301     c                   goto      inizio
017500020301     c                   else
017600020301     c                   iter
017700020301     c                   endif
017800020301     c                   endif
017900020301
018000020301      * F10 - Decodifica struttura cap
018100020301     c                   if        *inkj
018200020301     c                   exsr      sr_f10
018300020301     c                   endif
018400080423      * F15 - Cappario utilizzi particolari
018500080423     c                   if        *inkp
018600080423     c                   clear                   tisiq0ds
018700080423     c                   MOVEL     'C05'         dq0op0
018800080423     c                   clear                   dq0op1
018900080423     c                   MOVEL     '0'           dq0f03
019000080423     c                   MOVEL     '0'           dq0f12
019100080423     c                   MOVEL     '0'           dq0err
019200080423     c                   call      'TISIQ0R'
019300080423     c                   parm                    kpjba
019400080423     c                   parm                    tisiQ0ds
019500080423     c                   iter
019600080423     c                   endif
019700020206
019800020206      * F12 - Ritorno
019900020206     c                   if        *inkl
020000020206     c                   clear                   v1cnaz
020100020206     c                   goto      inizio
020200020206     c                   endif
020300020206
020400020206      * F16 - Annullamento
020500020206     c                   if        *inkq
020600020301     c                   eval      wf16 = *on
020700020301     c                   exsr      ctrd02
020800020301     c   28              iter
020900020301     c                   exsr      sr_f06
021000020301     c                   if        wf6 = *on
021100020301     c                   clear                   v1cnaz
021200020301     c                   goto      inizio
021300020301     c                   else
021400020301     c                   eval      wf16 = *off
021500020301     c                   iter
021600020301     c                   endif
021700020206     c                   endif
021800020206
021900020206      * F5 - Ripristino
022000020206     c                   if        *inke
022100020301     c                   eval      wf5 = *on
022200020301     c                   exsr      ctrd02
022300020301     c   28              iter
022400020301     c                   exsr      sr_f06
022500020301     c                   if        wf6 = *on
022600020301     c                   clear                   v1cnaz
022700020301     c                   goto      inizio
022800020301     c                   else
022900020301     c                   eval      wf5 = *off
023000020301     c                   iter
023100020301     c                   endif
023200020206     c                   endif
023300020206
023400020206      * Controlli
023500020206     c                   exsr      ctrd02
023600020206     c   28              iter
023700020206
023800020206     c                   enddo
023900020206
024000020206     c     fine          tag
024100020206     c                   eval      *inlr = *on
024200020206      *----------------------------------------------------------
024300020206      *    Controllo Codice
024400020206      *----------------------------------------------------------
024500020206     c     ctrcod        begsr
024600020307
024700060707     c                   eval      kkut = 1
024800020307     c                   movel     '15'          kcod
024900020206
025000020206      *   Ricerca tabella
025100020206     c     '?'           scan      v1cnaz                                 30
025200020206     c                   if        *in30
025300020206     c                   call      'TRUL19R'
025400020206     c                   parm                    kcod
025500020206     c                   parm                    ordina
025600020206     c                   parm                    kkey
025700020206     c                   parm                    comando
025800020206     c                   movel     kkey          v1cnaz
025900020206     c                   goto      endctrcod
026000020206     c                   endif
026100020206
026200020206     c                   movel     v1cnaz        kkey
026300020206
026400020206     c     ktab          chain     tabel00f
026500020206      * Immissione
026600020206     c                   if        not %found
026700020206     c                   eval      *in01 = *on
026800020206     c                   else
026900020206     c                   eval      ds15 = tbluni
027000020206     c                   if        tblflg <> *blanks
027100020206      * Annullato
027200020206     c                   eval      *in03 = *on
027300020206     c                   else
027400020206      * Aggiornamento
027500020206     c                   eval      *in02 = *on
027600020206     c                   endif
027700020206     c                   endif
027800020206
027900020206     c                   clear                   video2
028000020206      * Passo dati al video
028100020206     c                   if        not *in01
028200020206     c                   eval      v2des = §15des
028300020206     c                   eval      v2ecf = §15ecf
028400020206     c                   eval      v2pcf = §15pcf
028500020206     c                   eval      v2ucf = §15ucf
028600020206     c                   eval      v2vlt = §15vlt
028700020206     c                   eval      v2iso = §15iso
028800020206     c                   eval      v2cap = §15cap
028900150204     c                   eval      v2cfc = §15cfc
029000020206     c                   eval      v2eft = §15eft
029100020206     c                   eval      v2ita = §15ita
029200020206     c                   eval      v2cpr = §15cpr
029300020206     c                   eval      v2cod = §15cod
029400020206     c                   eval      v2cie = §15cie
029500020206     c                   eval      v2cpp = §15cpp
029600020206     c                   eval      v2lad = §15lad
029700140606     c                   eval      v2pref= §15pref
029800060919     c                   eval      v2red = §15red
029900020206     c                   eval      v2efd = §15efd
030000020206     c                   eval      v2cei = §15cei
030100020206     c                   eval      v2dog = §15dog
030200020206     c                   eval      v2laf = §15laf
030300020206     c                   eval      v2sto = §15sto
030400020206     c                   eval      v2eff = §15eff
030500020206     c                   eval      v2cts = §15cts
030600020206     c                   eval      v2inl = §15inl
030700020206     c                   eval      v2ffn = §15ffn
030800020206     c                   eval      v2eex = §15eex
030900020412     c                   eval      v2acn = §15acn
031000080604     c                   move      §15dpdPOE     V2dpdPOE
031100020311     c                   movel     §15ttc        v2ttc
031200160304     c                   eval      v2vas = §15vas
031300100115     c                   eval      v2ue  = §15ue
031400100122     c
031500100122     c                   clear                   forzaiso
031600100122     c                   clear                   forzaUE
031700160304     c* chaino estensione tabella
031800160304     c                   movel     '16'          kcod
031900160304     c                   movel     v1cnaz        kkey
032000160304     c     ktab          chain     tabel00f
032100160304     c                   if        %found(tabel00f)
032200160304     c                   eval      ds16 = tbluni
032300160304     c                   eval      v2pkd = §16pkd
032400160304     c                   endif
032500160304     c
032600100122     c
032700080423     c**                 eval      v2caeex = §15cex
032800080423     c**                 eval      v2cadpd = §15cdp
032900080423     c**                 eval      v2cafed = §15cfe
033000020206     c                   endif
033100020206
033200020206     c     endctrcod     endsr
033300020206
033400020206      *----------------------------------------------------------
033500020206      *    Controllo Seconda videata
033600020206      *----------------------------------------------------------
033700020206     c     ctrd02        begsr
033800020312
033900020312     c                   setoff                                       404142
034000020312     c                   setoff                                       434445
034100020312     c                   setoff                                       464748
034200020312     c                   setoff                                       49
034300160304     c                   setoff                                       5354
034400160307     c                   setoff                                       5152
034500020206
034600020206      * Descrizione
034700020206     c                   if        v2des = *blanks
034800020206     c                   movel     msg(1)        v2cmsg
034900020206     c                   eval      *in28 = *on
035000020206     c                   eval      *in40 = *on
035100020206     c                   goto      endctrd02
035200020206     c                   endif
035300020307
035400140606      * Pref.Telef.Internazionale  obbligatorio x naz.con DPD
035500140606     c                   if         v2pref = *blanks
035600140606     c                   if          v2lad  > 0        or
035700140606     c                               v2red <> *blanks  or
035800140606     c                               v2dpdpoe > 0
035900140606     c                   movel     msg(20)       v2cmsg
036000140606     c                   eval      *in28 = *on
036100140606     c                   eval      *in53 = *on
036200140606     c                   goto      endctrd02
036300140606     c                   end
036400140606      *  se inserito qualcosa nel campo non deve iniziare x (0)
036500140606     c                   else
036600140606     c                   if        %subst(%TrimL(V2pref):1:1) = '0'
036700140606     c                                 or
036800140606     c                             %subst(%TrimL(V2pref):1:1) = '+'
036900140606     c                   movel     msg(20)       v2cmsg
037000140606     c                   eval      *in28 = *on
037100140606     c                   eval      *in53 = *on
037200140606     c                   goto      endctrd02
037300140606     c                   end
037400140606     c                   endif
037500140606
037600020307      * Codice Iso
037700020307     c                   if        v2cod = *blanks
037800131218     c                   movel     msg(2)        v2cmsg
037900020307     c                   eval      *in28 = *on
038000020307     c                   eval      *in41 = *on
038100020307     c                   goto      endctrd02
038200020307     c                   endif
038300131218     c                   eval      ktke1='ALPHA-2 '
038400131218     c                   eval      ktke2=v2cod
038500131218     c     ktbe          chain     tntbe01l
038600131218     c                   if        not %found(tntbe01l)
038700131218     c                   movel     msg(19)       v2cmsg
038800131218     c                   eval      *in28 = *on
038900131218     c                   eval      *in41 = *on
039000131218     c                   goto      endctrd02
039100131218     c                   endif
039200131218     c
039300060426
039400060426      * Codice Iso Numerico
039500060426     c                   if        v2cie = *zeros
039600060426     c                   movel     msg(2)        v2cmsg
039700060426     c                   eval      *in28 = *on
039800060426     c                   eval      *in50 = *on
039900060426     c                   goto      endctrd02
040000060426     c                   endif
040100131218     c                   eval      ktke1='NUMERICO'
040200131218     c                   movel(p)  v2cie         ktke2
040300131218     c     ktbe          chain     tntbe01l
040400131218     c                   if        not %found(tntbe01l)
040500131218     c                   movel     msg(19)       v2cmsg
040600131218     c                   eval      *in28 = *on
040700131218     c                   eval      *in50 = *on
040800131218     c                   goto      endctrd02
040900131218     c                   endif
041000020307
041100020307      * Divisa
041200020312 b1  c                   if        v2vlt = *blanks
041300020307     c                   movel     msg(3)        v2cmsg
041400020307     c                   eval      *in28 = *on
041500020307     c                   eval      *in42 = *on
041600020307     c                   goto      endctrd02
041700020312 x1  c                   else
041800020311      *   ricerca
041900060707     c                   eval      kkut = 1
042000020311     c                   eval      kcod = 'CV'
042100020311     c                   clear                   kkey
042200020311     c     '?'           scan      v2vlt                                  30
042300020312 b2  c                   if        *in30
042400020311     c                   call      'TRUL19R'
042500020311     c                   parm                    kcod
042600020311     c                   parm                    ordina
042700020311     c                   parm                    kkey
042800020311     c                   parm                    comando
042900020311     c                   movel     kkey          v2vlt
043000020311     c                   goto      endctrd02
043100020312 e2  c                   endif
043200020311      *   controllo
043300100122     c                   movel(P)  v2vlt         kkey
043400020311     c     ktab          chain(n)  tabel00f
043500020312 b2  c                   if        not %found
043600020311     c                   movel     msg(11)       v2cmsg
043700020311     c                   eval      *in28 = *on
043800020311     c                   eval      *in42 = *on
043900020311     c                   goto      endctrd02
044000020312 e2  c                   endif
044100020312 e1  c                   endif
044200020307
044300020307      * Codice Iso per partita iva
044400020307     c                   if        v2iso = *blanks
044500020307     c                   movel     msg(4)        v2cmsg
044600020307     c                   eval      *in28 = *on
044700020307     c                   eval      *in43 = *on
044800020307     c                   goto      endctrd02
044900020307     c                   endif
045000100122
045100100122      * Appartenenza a U E
045200100122    1c                   if        v2ue='S'
045300100122     c* il codice iso per partita iva non deve essere $$ --> errore forzabile
045400100122    2c                   if        v2iso='$$' and forzaiso=' '
045500100122     c                   movel     msg(15)       v2cmsg
045600100122     c                   eval      *in28 = *on
045700100122     c                   eval      *in52 = *on
045800100122     c                   eval      forzaiso='1'
045900100122     c                   goto      endctrd02
046000100122    2c                   endif
046100100122    1c                   endif
046200100122     c
046300100122     c* Verifico se presente flag UE anche nella 4I errore forzabile
046400100122    1c                   if        forzaue=' '
046500100122     c                   eval      kcod = '4I'
046600100122     c                   movel(P)  v2iso         kkey
046700100122     c     ktab          chain(n)  tabel00f
046800100122    2c                   if        not %found
046900100122     c                   movel     msg(16)       v2cmsg
047000100122     c                   else
047100100122     c                   movel     tbluni        ds4i
047200100122
047300100122    3c                   if        §4iue<>v2ue
047400100122    4c                   if        v2ue='S'
047500100122     c                   movel     msg(17)       v2cmsg
047600100122     c                   else
047700100122     c                   movel     msg(18)       v2cmsg
047800100122    4c                   endif
047900100122    3c                   endif
048000100122    2c                   endif
048100100122     c* Errore
048200100122    2c                   if        v2cmsg<>*blanks
048300100122     c                   eval      *in28 = *on
048400100122     c                   eval      *in52 = *on
048500100122     c                   eval      forzaue ='1'
048600100122     c                   goto      endctrd02
048700100122    2c                   endif
048800100122    1c                   endif
048900020206
049000020206      * Cap
049100020206     c                   if        v2cpp = 'N' and v2cpr = 'S'
049200020307     c                   movel     msg(5)        v2cmsg
049300020206     c                   eval      *in28 = *on
049400020307     c                   eval      *in44 = *on
049500020206     c                   goto      endctrd02
049600020206     c                   endif
049700020307
049800080605      * DPD P.O.Emissione
049900080605     c                   if        v2dpdPOE <> 0   and
050000080605     c                             v2dpdPOE <> 190 and
050100141103     c                             v2dpdPOE <> 194 and
050200080605     c                             v2dpdPOE <> 195
050300080605     c                   movel     msg(14)       v2cmsg
050400080605     c                   eval      *in28 = *on
050500080605     c                   eval      *in51 = *on
050600080605     c                   goto      endctrd02
050700080605     c                   endif
050800160304
050900160304      * DPD Limite di peso per forzatura instradamento da EEX a DPD
051000160304     c                   if        v2pkd    > 0
051100160304     c* non inseribile se non previsto servizio DPD
051200160304     c                   if        v2lad    = 0
051300160304     c                   movel     msg(21)       v2cmsg
051400160304     c                   eval      *in28 = *on
051500160304     c                   eval      *in54 = *on
051600160304     c                   goto      endctrd02
051700160304     c                   endif
051800160304     c* non può superare il limite max gestito da DPD
051900160304     c                   if        v2pkd>§3ipkd
052000160304     c                   movel     msg(22)       v2cmsg
052100160304     c                   eval      v2cmsg=%trim(v2cmsg) + ' ' +
052200160307     c                             %trim(%editc(§3ipkd:'2')) + ' Kg'
052300160304     c                   eval      *in28 = *on
052400160304     c                   eval      *in54 = *on
052500160304     c                   goto      endctrd02
052600160304     c                   endif
052700160304     c                   endif
052800020307
052900020307      * FedEx
053000020312 b1  c                   if        v2laf <> *zeros
053100020307      * codice tassazione
053200120620     c                   if        %scan('?':v2cts) > 0
053300120620     c                   clear                   TRTB09DS
053400120620     c                   eval      TB09fun = '1'
053500120713     c                   eval      TB09ord = '1'
053600120713     c                   eval      TB09utf = 'S'
053700120620     c                   eval      kpjbu = TRTB09DS
053800120620     c                   call      'TRTB09R'
053900120620     c                   parm                    kpjba
054000120620     c                   eval      TRTB09DS = kpjbu
054100120620     c                   IF        TB09cod <> *blanks
054200120620     c                   eval      v2cts = TB09cod
054300120620     c                   ENDIF
054400120620     c                   endif
054500020312 b2  c                   if        v2cts = *blanks
054600020307     c                   movel     msg(8)        v2cmsg
054700020307     c                   eval      *in28 = *on
054800020307     c                   eval      *in47 = *on
054900020307     c                   goto      endctrd02
055000020312 e2  c                   endif
055100020307      * flag inoltro
055200020312 b2  c                   if        v2inl = *blanks
055300020307     c                   movel     msg(9)        v2cmsg
055400020307     c                   eval      *in28 = *on
055500020307     c                   eval      *in48 = *on
055600020307     c                   goto      endctrd02
055700020312 x2  c                   else
055800020311      *   ricerca
055900060707     c                   eval      kkut = 1
056000020307     c                   eval      kcod = '9A'
056100020307     c                   clear                   kkey
056200020307     c     '?'           scan      v2inl                                  30
056300020312 b3  c                   if        *in30
056400020307     c                   call      'TRUL19R'
056500020307     c                   parm                    kcod
056600020307     c                   parm                    ordina
056700020307     c                   parm                    kkey
056800020307     c                   parm                    comando
056900020307     c                   movel     kkey          v2inl
057000020307     c                   goto      endctrd02
057100020312 e3  c                   endif
057200020311      *   controllo
057300100122     c                   movel(P)  v2inl         kkey
057400020307     c     ktab          chain(n)  tabel00f
057500020312 b3  c                   if        not %found
057600020307     c                   movel     msg(10)       v2cmsg
057700020307     c                   eval      *in28 = *on
057800020307     c                   eval      *in48 = *on
057900020307     c                   goto      endctrd02
058000020312 e3  c                   endif
058100020312 e2  c                   endif
058200020311      * tipo tempo consegna
058300020312 b2  c                   if        v2ttc = *blanks
058400020311     c                   movel     msg(12)       v2cmsg
058500020311     c                   eval      *in28 = *on
058600020311     c                   eval      *in49 = *on
058700020311     c                   goto      endctrd02
058800020312 x2  c                   else
058900020311      *   ricerca
059000020311     c     '?'           scan      v2ttc                                  30
059100020312 b3  c                   if        *in30
059200020312     c                   clear                   tisi00ds
059300020312     c                   eval      d00op0 = 'T01'
059400020312     c                   eval      d00f03 = *off
059500020312     c                   eval      d00f12 = *off
059600020312     c                   eval      d00err = *off
059700020311     c                   call      'TISI41R'
059800020311     c                   parm                    kpjba
059900020312     c                   parm                    tisi00ds
060000020312 b4  c                   if        d00f03 = *on or d00f12 = *on or
060100020312     c                             d00err = *on
060200020312     c                   goto      endctrd02
060300020312 e4  c                   endif
060400020312     c                   movel     d00ttc        v2ttc
060500020312     c                   goto      endctrd02
060600020312 e3  c                   endif
060700020312 e2  c                   endif
060800020312 e1  c                   endif
060900020206
061000020206     c     endctrd02     endsr
061100020206
061200020301      *----------------------------------------------------------
061300020301      *    Decodifica struttura cap
061400020301      *----------------------------------------------------------
061500020301     c     sr_f10        begsr
061600020301
061700020301     c                   do        *hival
061800020304     c                   exfmt     tb86w2
061900020301      * F12 - Ritorno
062000020301     c                   if        *inkl
062100020301     c                   leave
062200020301     c                   endif
062300020301
062400020301     c                   enddo
062500020304     c                   eval      *inkl = *off
062600020301
062700020301     c                   endsr
062800020301
062900020301      *----------------------------------------------------------
063000020301      *    Videata conferma + dati trasmissione
063100020301      *----------------------------------------------------------
063200020301     c     sr_f06        begsr
063300020301
063400060707     c                   eval      kkut = 1
063500020307     c                   eval      kcod = '15'
063600100122     c                   movel(P)  v1cnaz        kkey
063700020307     c     ktab          chain(n)  tabel00f
063800020307     c                   if        %found
063900020301     c                   eval      w1ftt = tblftt
064000020301     c                   eval      w1flt = tblflt
064100020301     c                   eval      w1ftr = tblftr
064200020301
064300020301     c                   move      tbldtr        dataR
064400020301     c                   eval      aad = aar
064500020301     c                   eval      mmd = mmr
064600020301     c                   eval      ggd = ggr
064700020301     c                   move      dataD         w1dtr
064800020307     c                   else
064900020307     c                   clear                   w1ftt
065000020307     c                   clear                   w1flt
065100020307     c                   clear                   w1ftr
065200020307     c                   clear                   w1dtr
065300020307     c                   endif
065400020301
065500020301     c                   do        *hival
065600020301     c                   exfmt     tb86w1
065700020301      * F12 - Ritorno
065800020301     c                   if        *inkl
065900020301     c                   leave
066000020301     c                   endif
066100020301
066200020301      * F6 - Conferma
066300020301     c                   if        *inkf
066400020301     c                   exsr      scrivi
066500020301     c                   eval      wf6 = *on
066600020301     c                   leave
066700020301     c                   endif
066800020301
066900020301     c                   enddo
067000020301
067100020301     c                   endsr
067200020301
067300020206      *----------------------------------------------------------
067400020206      *    Scrittura - Aggiornamento
067500020206      *----------------------------------------------------------
067600020206     c     scrivi        begsr
067700020206
067800020206      * Passo dati alla ds15
067900020206     c                   eval      §15des = v2des
068000020206     c                   eval      §15ecf = v2ecf
068100020206     c                   eval      §15pcf = v2pcf
068200020206     c                   eval      §15ucf = v2ucf
068300020206     c                   eval      §15vlt = v2vlt
068400020206     c                   eval      §15iso = v2iso
068500020206     c                   eval      §15cap = v2cap
068600150204     c                   eval      §15cfc = v2cfc
068700020206     c                   eval      §15eft = v2eft
068800020206     c                   eval      §15ita = v2ita
068900020206     c                   eval      §15cpr = v2cpr
069000020206     c                   eval      §15cod = v2cod
069100020206     c                   eval      §15cie = v2cie
069200020206     c                   eval      §15cpp = v2cpp
069300020206     c                   eval      §15lad = v2lad
069400060919     c                   eval      §15red = v2red
069500020206     c                   eval      §15efd = v2efd
069600020206     c                   eval      §15cei = v2cei
069700020206     c                   eval      §15dog = v2dog
069800020206     c                   eval      §15laf = v2laf
069900020206     c                   eval      §15sto = v2sto
070000020206     c                   eval      §15eff = v2eff
070100020206     c                   eval      §15cts = v2cts
070200020206     c                   eval      §15inl = v2inl
070300020206     c                   eval      §15ffn = v2ffn
070400020206     c                   eval      §15eex = v2eex
070500020412     c                   eval      §15acn = v2acn
070600140606     c                   eval      §15pref= v2pref
070700080604     c                   move      v2DPDpoe      §15dpdPOE
070800020311     c                   movel     v2ttc         §15ttc
070900060209     c                   eval      §15vas = v2vas
071000100115     c                   eval      §15ue  = v2ue
071100080423     c***                eval      §15cex = v2caeex
071200080423     c***                eval      §15cdp = v2cadpd
071300080423     c***                eval      §15cfe = v2cafed
071400160304     c
071500160304     c                   eval      §16pkd = v2pkd
071600020206
071700060707     c                   eval      kkut = 1
071800020307     c                   eval      kcod = '15'
071900100122     c                   movel(P)  v1cnaz        kkey
072000020206     c     ktab          chain     tabel00f
072100020206
072200160307     c***                movel     ds15          tbluni
072300160307     c                   eval      tbluni=ds15
072400020301
072500020301     c                   if        wf16 = *on
072600020301     c                   eval      tblflg = '*'
072700020301     c                   endif
072800020301
072900020301     c                   if        wf5 = *on
073000020301     c                   clear                   tblflg
073100020301     c                   endif
073200020206
073300020301     c                   eval      tblftt = w1ftt
073400020206     c                   clear                   tblftr
073500020206     c                   clear                   tbldtr
073600020206
073700020206      * scrivo
073800020206     c                   if        not %found
073900060707     c                   eval      tblkut = kkut
074000020206     c                   eval      tblcod = kcod
074100020206     c                   eval      tblkey = kkey
074200020206     c                   write     tabel
074300160304     c* Scrivo anche estensione (tab 16) se dati tab 16 pieni
074400160307     c                   if        §16pkd>0
074500160307     c                   eval      kcod='16'
074600160307     c                   eval      tbluni=ds16
074700160307     c                   write     tabel
074800160307     c                   endif
074900020206
075000020206      * aggiorno
075100020206     c                   else
075200020206     c                   update    tabel
075300160304     c*
075400160304     c* agiorno estensione
075500160307     c                   eval      kkut = 1
075600160307     c                   eval      kcod = '16'
075700160307     c                   movel(P)  v1cnaz        kkey
075800160307     c     ktab          chain     tabel00f
075900160307     c                   eval      tbluni=ds16
076000160304
076100160307     c                   if        wf16 = *on or §16pkd=0
076200160307     c                   eval      tblflg = '*'
076300160307     c                   endif
076400160304
076500160307     c                   if        wf5 = *on or  (wf16= *off and §16pkd>0)
076600160307     c                   clear                   tblflg
076700160307     c                   endif
076800160304
076900160307     c                   eval      tblftt = w1ftt
077000160307     c                   clear                   tblftr
077100160307     c                   clear                   tbldtr
077200160307     c                   if        %found(tabel00f)
077300160307     c                   update    tabel
077400160307     c                   else
077500160307     c                   eval      tblkut = kkut
077600160307     c                   eval      tblcod = kcod
077700160307     c                   eval      tblkey = kkey
077800160307     c                   write     tabel
077900160307     c                   endif
078000160304     c
078100020206     c                   endif
078200060707
078300060707      * se annullamento o ripristino devo fare la stessa cosa anche
078400060707      * sulle tabelle in lingua
078500060707     c                   if        wf16 = *on or wf5 = *on
078600060707      * aggancio le lingue presenti su AZLIN
078700060707     c                   move      kkut          kazlin
078800060707     c     kazlin        setgt     azlin01l
078900060707     c                   do        *hival
079000060707     c                   read      azlin01l
079100060707     c                   if        %eof(azlin01l)
079200060707     c                   leave
079300060707     c                   endif
079400060707      * per ogni lingua aggiorno
079500060707     c                   move      lintabel      kkut
079600060707     c                   eval      kcod = '15'
079700100122     c                   movel(P)  v1cnaz        kkey
079800060707     c     ktab          chain     tabel00f
079900060707     c                   if        %found(tabel00f)
080000060707     c                   if        wf16 = *on
080100060707     c                   eval      tblflg = '*'
080200060707     c                   endif
080300060707     c                   if        wf5 = *on
080400060707     c                   clear                   tblflg
080500060707     c                   endif
080600060707     c                   eval      tblftt = w1ftt
080700060707     c                   clear                   tblftr
080800060707     c                   clear                   tbldtr
080900160304     c                   update    tabel
081000160304     c* agiorno estensione
081100160307     c***                eval      kcod = '16'
081200160307     c***                movel(P)  v1cnaz        kkey
081300160307     c***  ktab          chain     tabel00f
081400160307     c***                if        %found(tabel00f)
081500160307     c***                movel     ds16          tbluni
081600160304
081700160307     c***                if        wf16 = *on or §16pkd=0
081800160307     c***                eval      tblflg = '*'
081900160307     c***                endif
082000160304
082100160307     c***                if        wf5 = *on and §16pkd>0
082200160307     c***                clear                   tblflg
082300160307     c***                endif
082400160304
082500160307     c***                eval      tblftt = w1ftt
082600160307     c***                clear                   tblftr
082700160307     c***                clear                   tbldtr
082800160307     c***                update    tabel
082900160307     c                   endif
083000160307     c***                endif
083100060707     c                   enddo
083200060707     c                   endif
083300020206
083400020206     c                   endsr
083500060707
083600060707      *---------------------------------------------------------------*
083700060707      * gestione tabella in lingua
083800060707      *---------------------------------------------------------------*
083900060707     c     sr_lingua     begsr
084000060707
084100060707      * pulisco il subfile
084200060707     c                   exsr      sr_pulsfl
084300060707      * carico il subfile con i dati che trovo o vuoto
084400060707     c                   exsr      sr_carsfl
084500060707      * emetto il subfile
084600060707     c     emisfl        tag
084700060707     c                   eval      rec = 1
084800060707     c                   if        savrec > 0
084900060707     c                   eval      rec = savrec
085000060707     c                   endif
085100060707     c                   write     tb86z1
085200060707     c                   exfmt     tb86c1
085300060707
085400060707      * F12 - Ritorno
085500060707     c                   if        *inkl
085600060707     c                   setoff                                       28
085700060707     c                   clear                   v1cmsg
085800060707     c                   goto      inz02
085900060707     c                   endif
086000060707
086100060707     c                   setoff                                       28
086200060707     c                   setoff                                       39
086300060707      * Controlli
086400060707     c  n05              exsr      sr_ctrsfl
086500060707     c   28
086600060707     cornkf              goto      emisfl
086700060707
086800060707      * F6 - Conferma
086900060707     c   kf              exsr      sr_confsfl
087000060707
087100060707     c                   endsr
087200060707
087300060707      *---------------------------------------------------------------*
087400060707      * pulisco il subfile
087500060707      *---------------------------------------------------------------*
087600060707     c     sr_pulsfl     begsr
087700060707
087800060707     c                   clear                   nrr
087900060707     c                   clear                   savrec
088000060707     c                   eval      *in20 = *off
088100060707     c                   eval      *in21 = *off
088200060707     c                   write     tb86c1
088300060707     c                   eval      *in20 = *on
088400060707     c                   eval      *in21 = *on
088500060707
088600060707     c                   endsr
088700060707
088800060707      *---------------------------------------------------------------*
088900060707      * carico il subfile
089000060707      *---------------------------------------------------------------*
089100060707     c     sr_carsfl     begsr
089200060707
089300060707     c                   setoff                                       30
089400060707      * carico un record per ogni lingua inserite in AZLIN
089500060707     c                   move      '1'           kazlin
089600060707     c     kazlin        setgt     azlin01l
089700060707     c                   do        *hival
089800060707     c                   read      azlin01l
089900060707      * fine file
090000060707     c                   if        %eof(azlin01l)
090100060707     c                   leave
090200060707     c                   endif
090300060707     c                   eval      s1dlin = lindesita
090400060707     c                   clear                   ds15
090500060707     c                   move      lintabel      kkut
090600060707     c                   eval      kcod = '15'
090700100122     c                   clear                   kkey
090800060707     c                   eval      kkey = v1cnaz
090900060707     c     ktab          chain(n)  tabel00f
091000060707     c                   if        not %found(tabel00f)
091100060707     c                   clear                   s1cdes
091200060707     c                   else
091300060707     c                   eval      ds15 = tbluni
091400060707     c                   eval      s1cdes = §15des
091500060707     c                   endif
091600060707     c                   eval      s1hcdlin = kkut
091700060707     c                   add       1             nrr
091800060707     c                   write     tb86s1
091900060707     c                   enddo
092000060707
092100060707     c                   seton                                        30
092200060707
092300060707     c                   endsr
092400060707
092500060707      *---------------------------------------------------------------*
092600060707      * controllo il subfile
092700060707      *---------------------------------------------------------------*
092800060707     c     sr_ctrsfl     begsr
092900060707
093000060707     c                   clear                   nrr
093100060707     c                   do        *hival
093200060707     c                   eval      nrr = nrr +1
093300060707     c     nrr           chain     tb86s1                             32
093400060707     c                   if        *in32
093500060707     c                   leave
093600060707     c                   endif
093700060707      * deve esserci la descrizione
093800060707     c                   if        s1cdes = *blanks
093900060707     c                   eval      v1cmsg = msg(01)
094000060707     c                   eval      *in28 = *on
094100060707     c                   eval      *in39 = *on
094200060707     c                   eval      savrec = nrr
094300060707     c                   update    tb86s1
094400060707     c                   leavesr
094500060707     c                   endif
094600060707     c                   update    tb86s1
094700060707     c                   enddo
094800060707
094900060707     c                   endsr
095000060707
095100060707      *---------------------------------------------------------------*
095200060707      * confermo i dati del subfile
095300060707      *---------------------------------------------------------------*
095400060707     c     sr_confsfl    begsr
095500060707
095600060707      * recupero i dati della tabella in italiano
095700060707     c                   clear                   savtbluni
095800060707     c                   eval      kkut = 1
095900060707     c                   eval      kcod = '15'
096000100122     c                   movel(P)  v1cnaz        kkey
096100060707     c     ktab          chain(n)  tabel00f
096200060707     c                   if        %found
096300060707     c                   eval      savtbluni = tbluni
096400060707     c                   endif
096500060707
096600060707      * leggo il subfile e aggiorno/scrivo i dati in lingua
096700060707     c                   clear                   nrr
096800060707     c                   do        *hival
096900060707     c                   eval      nrr = nrr +1
097000060707     c     nrr           chain     tb86s1                             32
097100060707     c                   if        *in32
097200060707     c                   leave
097300060707     c                   endif
097400060707     c                   eval      kkut = s1hcdlin
097500060707     c     ktab          chain     tabel00f
097600060707     c                   eval      ds15 = savtbluni
097700060707     c                   eval      §15des = s1cdes
097800060707     c                   eval      tbluni = ds15
097900060707     c                   eval      tblftt = w1ftt
098000060707     c                   clear                   tblftr
098100060707     c                   clear                   tbldtr
098200060707      * scrivo
098300060707     c                   if        not %found(tabel00f)
098400060707     c                   eval      tblkut = kkut
098500060707     c                   eval      tblcod = kcod
098600060707     c                   eval      tblkey = kkey
098700060707     c                   write     tabel
098800060707      * aggiorno
098900060707     c                   else
099000060707     c                   update    tabel
099100060707     c                   endif
099200060707
099300060707     c                   enddo
099400060707
099500060707     c                   endsr
099600020206      *-------------------------------------------------------------------------
099700020206** MSG  Lungh. 78                                                            *
099800020312Descrizione obbligatoria                                                      1
099900020312Codice Iso obbligatorio                                                       2
100000020312Divisa obbligatoria                                                           3
100100020312Codice iso per partita iva obbligatorio                                       4
100200020312Se Nazione senza cappario non richiedere il controllo cap/provincia           5
100300020312Linea arrivo DPD: inserire flag attraversamento dogana                        6
100400020312Linea arrivo FedEx: inserire flag attraversamento dogana                      7
100500020312Linea arrivo FedEx: inserire codice di tassazione                             8
100600020312Linea arrivo FedEx: inserire flag inoltro                                     9
100700020312Flag inoltro errato                                                           10
100800020312Divisa errata                                                                 11
100900020312Immettere il tipo tempo consegna FedEx                                        12
101000020312Tipo tempo consegna FedEx errato                                              13
101100141103I P.O.Emissione attualmente gestiti sono 190, 195 e 194.                      14
101200100122Se nazione UE il codice ISO per part.IVA non deve essere "$$". Enter forza    15
101300100122Non trovata la tabella del codice ISO. Inserire la "4I". enter forza          16
101400100122Aggionare anche tabella del codice ISO("4I") per appartenenza UE.Enter forza  17
101500100122Rendere congruente flag appartenenza UE con quello del codice ISO("4I").Forza 18
101600131218Codice Iso non valido                                                         19
101700140606Pref.tel.Inter.obbligatorio. NON deve avere zeri o il (+) alla sx. del cod.   20
101800160304Limite di peso inseribile solo se la nazione prevede il servizio DPD          21
101900160304Il limite di peso non può superare i                                          22
