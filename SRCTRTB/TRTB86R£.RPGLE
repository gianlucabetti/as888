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
003600160406      * 55 - ERRORE vol. limite per instradamento forzato da eex a dpd
003700020206      ***********************************************************
003800020206
003900020131     FTRTB86D   CF   E             WORKSTN
004000060707     f                                     sfile(tb86s1:nrr)
004100891030     FTABEL00F  UF A E           K DISK
004200131218     Ftntbe01l  iF   E           K DISK
004300060707     fazlin01l  if   e           k disk
004400020206
004500060707     d kazlin          s                   like(lintabel)
004600020206     D kcod            S                   LIKE(tblcod)
004700020206     D kkey            S                   LIKE(tblkey)
004800060707     d kkut            s                   like(tblkut)
004900131218     d ktcod           s                   like(tbecod) inz('NAS')
005000131218     d ktke1           s                   like(tbeke1)
005100131218     d ktke2           s                   like(tbeke2)
005200020206
005300020206     D ordina          S              1
005400020206     D comando         S              1
005500020301     d wf6             s              1    inz('0')
005600020301     d wf16            s              1    inz('0')
005700020301     d wf5             s              1    inz('0')
005800060707
005900060707     d savtblftt       s                   like(tblftt)
006000060707     d savtblflt       s                   like(tblflt)
006100060707     d savtbluni       s                   like(tbluni)
006200060707     d nrr             s              4  0
006300060707     d savrec          s                   like(nrr)
006400020206
006500160406     D MSG             S             78    DIM(23) CTDATA PERRCD(1)
006600020301
006700020301     D DataR           DS
006800020301     D  aaR                    1      2
006900020301     D  mmR                    3      4
007000020411     D  ggR                    5      6
007100020301
007200020301     D DataD           DS
007300020301     D  ggD                    1      2
007400020301     D  mmD                    3      4
007500020411     D  aaD                    5      6
007600020206
007700020206     D Video2        E DS                  extname(trtb86d:tb86d2)
007800020206
007900120620     d trtb09ds      e ds
008000020131     D DS15          E DS
008100160304     D DS16          E DS
008200100122     D DS4I          E DS
008300160304     D DS3IDP        E DS
008400020312     d tisi00ds      e ds
008500080423     d tisiq0ds      e ds
008600020206     D UT§DSE0F      E DS
008700891030     D KPJBA         E DS
008800020206     D  rem                    1      3
008900100122
009000100122     d forzaiso        s              1
009100100122     d forzaUE         s              1
009200020206      *----------------------------------------------------------
009300020206
009400020206     c     *entry        plist
009500020206     c                   parm                    kpjba
009600020206
009700020206     c                   eval      codut = 1
009800020206     c                   call      'XPARUT'
009900020206     c                   parm                    ut§dse0f
010000020206     c                   movel     ragut         rsut
010100020206
010200020206     c     ktab          klist
010300060707     c                   kfld                    kkut
010400020206     c                   kfld                    kcod
010500020206     c                   kfld                    kkey
010600131218     c     ktbe          klist
010700131218     c                   kfld                    ktcod
010800131218     c                   kfld                    ktke1
010900131218     c                   kfld                    ktke2
011000160304     C***
011100160304     C* CARICO TABELLA DPD
011200160304     C***
011300160304     c                   eval      kkut = 1
011400160304     C                   MOVEL     '3I'          kCOD
011500160304     C                   MOVEL(P)  'DPD'         kKEY
011600160304     C                   CLEAR                   DS3IDP
011700160304     c     ktab          chain(n)  tabel00f
011800160304     c                   if        %found(tabel00f)
011900160304     C                   MOVEL     TBLUNI        DS3IDP
012000160304     c                   endif
012100020206
012200020206      * Prima videata: richiesta codice tabella
012300020206
012400020206     c     inizio        tag
012500020206
012600020206     c                   eval      *in01 = *off
012700020206     c                   eval      *in02 = *off
012800020206     c                   eval      *in03 = *off
012900020301     c                   eval      wf6 = *off
013000020301     c                   eval      wf16 = *off
013100020301     c                   eval      wf5 = *off
013200020206
013300020206     c                   if        v1cnaz = *blanks
013400100122     c                   eval      v1cnaz = '?  '
013500020206     c                   endif
013600020206
013700020206     c                   exfmt     tb86d1
013800020206
013900020206      * F3 - Fine
014000020206     c   kc              goto      fine
014100020206
014200020206      * Controllo codice tabella
014300020206     c                   exsr      ctrcod
014400020206     c   30              goto      inizio
014500020301
014600020206
014700020206      * Seconda videata: dati tabella
014800020206     c                   do        *hival
014900020206
015000060707     c     inz02         tag
015100020206     c                   exfmt     tb86d2
015200020206     c                   eval      *in28 = *off
015300130925     c                   clear                   V2Cmsg
015400020206
015500020206      * F3 - Fine
015600020206     c   kc              goto      fine
015700060707      * F9 - Traduzione
015800060707     c                   if        *inki
015900060707     c                   eval      *in05 = *on
016000060707     c                   exsr      sr_lingua
016100060707     c                   eval      *in05 = *off
016200060707     c                   iter
016300060707     c                   endif
016400020301
016500020301      * F6 - Conferma
016600020301     c                   if        *inkf
016700020301     c                   exsr      ctrd02
016800020301     c   28              iter
016900020301     c                   exsr      sr_f06
017000020301     c                   if        wf6 = *on
017100060707      * tabelle in lingua
017200060707     c                   eval      *in05 = *off
017300060707     c                   exsr      sr_lingua
017400020301     c                   clear                   v1cnaz
017500020301     c                   goto      inizio
017600020301     c                   else
017700020301     c                   iter
017800020301     c                   endif
017900020301     c                   endif
018000020301
018100020301      * F10 - Decodifica struttura cap
018200020301     c                   if        *inkj
018300020301     c                   exsr      sr_f10
018400020301     c                   endif
018500080423      * F15 - Cappario utilizzi particolari
018600080423     c                   if        *inkp
018700080423     c                   clear                   tisiq0ds
018800080423     c                   MOVEL     'C05'         dq0op0
018900080423     c                   clear                   dq0op1
019000080423     c                   MOVEL     '0'           dq0f03
019100080423     c                   MOVEL     '0'           dq0f12
019200080423     c                   MOVEL     '0'           dq0err
019300080423     c                   call      'TISIQ0R'
019400080423     c                   parm                    kpjba
019500080423     c                   parm                    tisiQ0ds
019600080423     c                   iter
019700080423     c                   endif
019800020206
019900020206      * F12 - Ritorno
020000020206     c                   if        *inkl
020100020206     c                   clear                   v1cnaz
020200020206     c                   goto      inizio
020300020206     c                   endif
020400020206
020500020206      * F16 - Annullamento
020600020206     c                   if        *inkq
020700020301     c                   eval      wf16 = *on
020800020301     c                   exsr      ctrd02
020900020301     c   28              iter
021000020301     c                   exsr      sr_f06
021100020301     c                   if        wf6 = *on
021200020301     c                   clear                   v1cnaz
021300020301     c                   goto      inizio
021400020301     c                   else
021500020301     c                   eval      wf16 = *off
021600020301     c                   iter
021700020301     c                   endif
021800020206     c                   endif
021900020206
022000020206      * F5 - Ripristino
022100020206     c                   if        *inke
022200020301     c                   eval      wf5 = *on
022300020301     c                   exsr      ctrd02
022400020301     c   28              iter
022500020301     c                   exsr      sr_f06
022600020301     c                   if        wf6 = *on
022700020301     c                   clear                   v1cnaz
022800020301     c                   goto      inizio
022900020301     c                   else
023000020301     c                   eval      wf5 = *off
023100020301     c                   iter
023200020301     c                   endif
023300020206     c                   endif
023400020206
023500020206      * Controlli
023600020206     c                   exsr      ctrd02
023700020206     c   28              iter
023800020206
023900020206     c                   enddo
024000020206
024100020206     c     fine          tag
024200020206     c                   eval      *inlr = *on
024300020206      *----------------------------------------------------------
024400020206      *    Controllo Codice
024500020206      *----------------------------------------------------------
024600020206     c     ctrcod        begsr
024700020307
024800060707     c                   eval      kkut = 1
024900020307     c                   movel     '15'          kcod
025000020206
025100020206      *   Ricerca tabella
025200020206     c     '?'           scan      v1cnaz                                 30
025300020206     c                   if        *in30
025400020206     c                   call      'TRUL19R'
025500020206     c                   parm                    kcod
025600020206     c                   parm                    ordina
025700020206     c                   parm                    kkey
025800020206     c                   parm                    comando
025900020206     c                   movel     kkey          v1cnaz
026000020206     c                   goto      endctrcod
026100020206     c                   endif
026200020206
026300020206     c                   movel     v1cnaz        kkey
026400020206
026500020206     c     ktab          chain     tabel00f
026600020206      * Immissione
026700020206     c                   if        not %found
026800020206     c                   eval      *in01 = *on
026900020206     c                   else
027000020206     c                   eval      ds15 = tbluni
027100020206     c                   if        tblflg <> *blanks
027200020206      * Annullato
027300020206     c                   eval      *in03 = *on
027400020206     c                   else
027500020206      * Aggiornamento
027600020206     c                   eval      *in02 = *on
027700020206     c                   endif
027800020206     c                   endif
027900020206
028000020206     c                   clear                   video2
028100020206      * Passo dati al video
028200020206     c                   if        not *in01
028300020206     c                   eval      v2des = §15des
028400020206     c                   eval      v2ecf = §15ecf
028500020206     c                   eval      v2pcf = §15pcf
028600020206     c                   eval      v2ucf = §15ucf
028700020206     c                   eval      v2vlt = §15vlt
028800020206     c                   eval      v2iso = §15iso
028900020206     c                   eval      v2cap = §15cap
029000150204     c                   eval      v2cfc = §15cfc
029100020206     c                   eval      v2eft = §15eft
029200020206     c                   eval      v2ita = §15ita
029300020206     c                   eval      v2cpr = §15cpr
029400020206     c                   eval      v2cod = §15cod
029500020206     c                   eval      v2cie = §15cie
029600020206     c                   eval      v2cpp = §15cpp
029700020206     c                   eval      v2lad = §15lad
029800140606     c                   eval      v2pref= §15pref
029900060919     c                   eval      v2red = §15red
030000170112     c                   eval      v2dpdiso2 = §15dpdiso2
030100020206     c                   eval      v2efd = §15efd
030200020206     c                   eval      v2cei = §15cei
030300020206     c                   eval      v2dog = §15dog
030400020206     c                   eval      v2laf = §15laf
030500020206     c                   eval      v2sto = §15sto
030600020206     c                   eval      v2eff = §15eff
030700020206     c                   eval      v2cts = §15cts
030800020206     c                   eval      v2inl = §15inl
030900020206     c                   eval      v2ffn = §15ffn
031000020206     c                   eval      v2eex = §15eex
031100020412     c                   eval      v2acn = §15acn
031200080604     c                   move      §15dpdPOE     V2dpdPOE
031300020311     c                   movel     §15ttc        v2ttc
031400160304     c                   eval      v2vas = §15vas
031500100115     c                   eval      v2ue  = §15ue
031600100122     c
031700100122     c                   clear                   forzaiso
031800100122     c                   clear                   forzaUE
031900160304     c* chaino estensione tabella
032000160304     c                   movel     '16'          kcod
032100160304     c                   movel     v1cnaz        kkey
032200160304     c     ktab          chain     tabel00f
032300160304     c                   if        %found(tabel00f)
032400160304     c                   eval      ds16 = tbluni
032500160304     c                   eval      v2pkd = §16pkd
032600160406     c                   eval      v2vld = §16vld
032700160304     c                   endif
032800160304     c
032900100122     c
033000080423     c**                 eval      v2caeex = §15cex
033100080423     c**                 eval      v2cadpd = §15cdp
033200080423     c**                 eval      v2cafed = §15cfe
033300020206     c                   endif
033400020206
033500020206     c     endctrcod     endsr
033600020206
033700020206      *----------------------------------------------------------
033800020206      *    Controllo Seconda videata
033900020206      *----------------------------------------------------------
034000020206     c     ctrd02        begsr
034100020312
034200020312     c                   setoff                                       404142
034300020312     c                   setoff                                       434445
034400020312     c                   setoff                                       464748
034500020312     c                   setoff                                       49
034600160406     c                   setoff                                       535455
034700160307     c                   setoff                                       5152
034800020206
034900020206      * Descrizione
035000020206     c                   if        v2des = *blanks
035100020206     c                   movel     msg(1)        v2cmsg
035200020206     c                   eval      *in28 = *on
035300020206     c                   eval      *in40 = *on
035400020206     c                   goto      endctrd02
035500020206     c                   endif
035600020307
035700140606      * Pref.Telef.Internazionale  obbligatorio x naz.con DPD
035800140606     c                   if         v2pref = *blanks
035900140606     c                   if          v2lad  > 0        or
036000140606     c                               v2red <> *blanks  or
036100140606     c                               v2dpdpoe > 0
036200140606     c                   movel     msg(20)       v2cmsg
036300140606     c                   eval      *in28 = *on
036400140606     c                   eval      *in53 = *on
036500140606     c                   goto      endctrd02
036600140606     c                   end
036700140606      *  se inserito qualcosa nel campo non deve iniziare x (0)
036800140606     c                   else
036900140606     c                   if        %subst(%TrimL(V2pref):1:1) = '0'
037000140606     c                                 or
037100140606     c                             %subst(%TrimL(V2pref):1:1) = '+'
037200140606     c                   movel     msg(20)       v2cmsg
037300140606     c                   eval      *in28 = *on
037400140606     c                   eval      *in53 = *on
037500140606     c                   goto      endctrd02
037600140606     c                   end
037700140606     c                   endif
037800140606
037900020307      * Codice Iso
038000020307     c                   if        v2cod = *blanks
038100131218     c                   movel     msg(2)        v2cmsg
038200020307     c                   eval      *in28 = *on
038300020307     c                   eval      *in41 = *on
038400020307     c                   goto      endctrd02
038500020307     c                   endif
038600131218     c                   eval      ktke1='ALPHA-2 '
038700131218     c                   eval      ktke2=v2cod
038800131218     c     ktbe          chain     tntbe01l
038900131218     c                   if        not %found(tntbe01l)
039000131218     c                   movel     msg(19)       v2cmsg
039100131218     c                   eval      *in28 = *on
039200131218     c                   eval      *in41 = *on
039300131218     c                   goto      endctrd02
039400131218     c                   endif
039500131218     c
039600060426
039700060426      * Codice Iso Numerico
039800060426     c                   if        v2cie = *zeros
039900060426     c                   movel     msg(2)        v2cmsg
040000060426     c                   eval      *in28 = *on
040100060426     c                   eval      *in50 = *on
040200060426     c                   goto      endctrd02
040300060426     c                   endif
040400131218     c                   eval      ktke1='NUMERICO'
040500131218     c                   movel(p)  v2cie         ktke2
040600131218     c     ktbe          chain     tntbe01l
040700131218     c                   if        not %found(tntbe01l)
040800131218     c                   movel     msg(19)       v2cmsg
040900131218     c                   eval      *in28 = *on
041000131218     c                   eval      *in50 = *on
041100131218     c                   goto      endctrd02
041200131218     c                   endif
041300020307
041400020307      * Divisa
041500020312 b1  c                   if        v2vlt = *blanks
041600020307     c                   movel     msg(3)        v2cmsg
041700020307     c                   eval      *in28 = *on
041800020307     c                   eval      *in42 = *on
041900020307     c                   goto      endctrd02
042000020312 x1  c                   else
042100020311      *   ricerca
042200060707     c                   eval      kkut = 1
042300020311     c                   eval      kcod = 'CV'
042400020311     c                   clear                   kkey
042500020311     c     '?'           scan      v2vlt                                  30
042600020312 b2  c                   if        *in30
042700020311     c                   call      'TRUL19R'
042800020311     c                   parm                    kcod
042900020311     c                   parm                    ordina
043000020311     c                   parm                    kkey
043100020311     c                   parm                    comando
043200020311     c                   movel     kkey          v2vlt
043300020311     c                   goto      endctrd02
043400020312 e2  c                   endif
043500020311      *   controllo
043600100122     c                   movel(P)  v2vlt         kkey
043700020311     c     ktab          chain(n)  tabel00f
043800020312 b2  c                   if        not %found
043900020311     c                   movel     msg(11)       v2cmsg
044000020311     c                   eval      *in28 = *on
044100020311     c                   eval      *in42 = *on
044200020311     c                   goto      endctrd02
044300020312 e2  c                   endif
044400020312 e1  c                   endif
044500020307
044600020307      * Codice Iso per partita iva
044700020307     c                   if        v2iso = *blanks
044800020307     c                   movel     msg(4)        v2cmsg
044900020307     c                   eval      *in28 = *on
045000020307     c                   eval      *in43 = *on
045100020307     c                   goto      endctrd02
045200020307     c                   endif
045300100122
045400100122      * Appartenenza a U E
045500100122    1c                   if        v2ue='S'
045600100122     c* il codice iso per partita iva non deve essere $$ --> errore forzabile
045700100122    2c                   if        v2iso='$$' and forzaiso=' '
045800100122     c                   movel     msg(15)       v2cmsg
045900100122     c                   eval      *in28 = *on
046000100122     c                   eval      *in52 = *on
046100100122     c                   eval      forzaiso='1'
046200100122     c                   goto      endctrd02
046300100122    2c                   endif
046400100122    1c                   endif
046500100122     c
046600100122     c* Verifico se presente flag UE anche nella 4I errore forzabile
046700100122    1c                   if        forzaue=' '
046800100122     c                   eval      kcod = '4I'
046900100122     c                   movel(P)  v2iso         kkey
047000100122     c     ktab          chain(n)  tabel00f
047100100122    2c                   if        not %found
047200100122     c                   movel     msg(16)       v2cmsg
047300100122     c                   else
047400100122     c                   movel     tbluni        ds4i
047500100122
047600100122    3c                   if        §4iue<>v2ue
047700100122    4c                   if        v2ue='S'
047800100122     c                   movel     msg(17)       v2cmsg
047900100122     c                   else
048000100122     c                   movel     msg(18)       v2cmsg
048100100122    4c                   endif
048200100122    3c                   endif
048300100122    2c                   endif
048400100122     c* Errore
048500100122    2c                   if        v2cmsg<>*blanks
048600100122     c                   eval      *in28 = *on
048700100122     c                   eval      *in52 = *on
048800100122     c                   eval      forzaue ='1'
048900100122     c                   goto      endctrd02
049000100122    2c                   endif
049100100122    1c                   endif
049200020206
049300020206      * Cap
049400020206     c                   if        v2cpp = 'N' and v2cpr = 'S'
049500020307     c                   movel     msg(5)        v2cmsg
049600020206     c                   eval      *in28 = *on
049700020307     c                   eval      *in44 = *on
049800020206     c                   goto      endctrd02
049900020206     c                   endif
050000020307
050100080605      * DPD P.O.Emissione
050200080605     c                   if        v2dpdPOE <> 0   and
050300080605     c                             v2dpdPOE <> 190 and
050400141103     c                             v2dpdPOE <> 194 and
050500080605     c                             v2dpdPOE <> 195
050600080605     c                   movel     msg(14)       v2cmsg
050700080605     c                   eval      *in28 = *on
050800080605     c                   eval      *in51 = *on
050900080605     c                   goto      endctrd02
051000080605     c                   endif
051100160304
051200160304      * DPD Limite di peso per forzatura instradamento da EEX a DPD
051300160304     c                   if        v2pkd    > 0
051400160304     c* non inseribile se non previsto servizio DPD
051500160304     c                   if        v2lad    = 0
051600160304     c                   movel     msg(21)       v2cmsg
051700160304     c                   eval      *in28 = *on
051800160304     c                   eval      *in54 = *on
051900160304     c                   goto      endctrd02
052000160304     c                   endif
052100160304     c* non può superare il limite max gestito da DPD
052200160304     c                   if        v2pkd>§3ipkd
052300160304     c                   movel     msg(22)       v2cmsg
052400160304     c                   eval      v2cmsg=%trim(v2cmsg) + ' ' +
052500160307     c                             %trim(%editc(§3ipkd:'2')) + ' Kg'
052600160304     c                   eval      *in28 = *on
052700160304     c                   eval      *in54 = *on
052800160304     c                   goto      endctrd02
052900160304     c                   endif
053000160304     c                   endif
053100160406      * DPD Limite di volume per forzatura instradamento da EEX a DPD
053200160406     c                   if        v2vld    > 0 and v2lad = 0
053300160406     c                   movel     msg(23)       v2cmsg
053400160406     c                   eval      *in28 = *on
053500160406     c                   eval      *in55 = *on
053600160406     c                   goto      endctrd02
053700160406     c                   endif
053800170112      * DPD Codice Iso 2
053900170112     c                   if        v2dpdiso2 <> *blanks
054000170112     c                   eval      ktke1='ALPHA-2 '
054100170112     c                   eval      ktke2=v2dpdiso2
054200170112     c     ktbe          chain     tntbe01l
054300170112     c                   if        not %found(tntbe01l)
054400170112     c                   movel     msg(19)       v2cmsg
054500170112     c                   eval      *in28 = *on
054600170112     c                   eval      *in56 = *on
054700170112     c                   goto      endctrd02
054800170112     c                   endif
054900170112     c                   endif
055000170112     c
055100020307
055200020307      * FedEx
055300020312 b1  c                   if        v2laf <> *zeros
055400020307      * codice tassazione
055500120620     c                   if        %scan('?':v2cts) > 0
055600120620     c                   clear                   TRTB09DS
055700120620     c                   eval      TB09fun = '1'
055800120713     c                   eval      TB09ord = '1'
055900120713     c                   eval      TB09utf = 'S'
056000120620     c                   eval      kpjbu = TRTB09DS
056100120620     c                   call      'TRTB09R'
056200120620     c                   parm                    kpjba
056300120620     c                   eval      TRTB09DS = kpjbu
056400120620     c                   IF        TB09cod <> *blanks
056500120620     c                   eval      v2cts = TB09cod
056600120620     c                   ENDIF
056700120620     c                   endif
056800020312 b2  c                   if        v2cts = *blanks
056900020307     c                   movel     msg(8)        v2cmsg
057000020307     c                   eval      *in28 = *on
057100020307     c                   eval      *in47 = *on
057200020307     c                   goto      endctrd02
057300020312 e2  c                   endif
057400020307      * flag inoltro
057500020312 b2  c                   if        v2inl = *blanks
057600020307     c                   movel     msg(9)        v2cmsg
057700020307     c                   eval      *in28 = *on
057800020307     c                   eval      *in48 = *on
057900020307     c                   goto      endctrd02
058000020312 x2  c                   else
058100020311      *   ricerca
058200060707     c                   eval      kkut = 1
058300020307     c                   eval      kcod = '9A'
058400020307     c                   clear                   kkey
058500020307     c     '?'           scan      v2inl                                  30
058600020312 b3  c                   if        *in30
058700020307     c                   call      'TRUL19R'
058800020307     c                   parm                    kcod
058900020307     c                   parm                    ordina
059000020307     c                   parm                    kkey
059100020307     c                   parm                    comando
059200020307     c                   movel     kkey          v2inl
059300020307     c                   goto      endctrd02
059400020312 e3  c                   endif
059500020311      *   controllo
059600100122     c                   movel(P)  v2inl         kkey
059700020307     c     ktab          chain(n)  tabel00f
059800020312 b3  c                   if        not %found
059900020307     c                   movel     msg(10)       v2cmsg
060000020307     c                   eval      *in28 = *on
060100020307     c                   eval      *in48 = *on
060200020307     c                   goto      endctrd02
060300020312 e3  c                   endif
060400020312 e2  c                   endif
060500020311      * tipo tempo consegna
060600020312 b2  c                   if        v2ttc = *blanks
060700020311     c                   movel     msg(12)       v2cmsg
060800020311     c                   eval      *in28 = *on
060900020311     c                   eval      *in49 = *on
061000020311     c                   goto      endctrd02
061100020312 x2  c                   else
061200020311      *   ricerca
061300020311     c     '?'           scan      v2ttc                                  30
061400020312 b3  c                   if        *in30
061500020312     c                   clear                   tisi00ds
061600020312     c                   eval      d00op0 = 'T01'
061700020312     c                   eval      d00f03 = *off
061800020312     c                   eval      d00f12 = *off
061900020312     c                   eval      d00err = *off
062000020311     c                   call      'TISI41R'
062100020311     c                   parm                    kpjba
062200020312     c                   parm                    tisi00ds
062300020312 b4  c                   if        d00f03 = *on or d00f12 = *on or
062400020312     c                             d00err = *on
062500020312     c                   goto      endctrd02
062600020312 e4  c                   endif
062700020312     c                   movel     d00ttc        v2ttc
062800020312     c                   goto      endctrd02
062900020312 e3  c                   endif
063000020312 e2  c                   endif
063100020312 e1  c                   endif
063200020206
063300020206     c     endctrd02     endsr
063400020206
063500020301      *----------------------------------------------------------
063600020301      *    Decodifica struttura cap
063700020301      *----------------------------------------------------------
063800020301     c     sr_f10        begsr
063900020301
064000020301     c                   do        *hival
064100020304     c                   exfmt     tb86w2
064200020301      * F12 - Ritorno
064300020301     c                   if        *inkl
064400020301     c                   leave
064500020301     c                   endif
064600020301
064700020301     c                   enddo
064800020304     c                   eval      *inkl = *off
064900020301
065000020301     c                   endsr
065100020301
065200020301      *----------------------------------------------------------
065300020301      *    Videata conferma + dati trasmissione
065400020301      *----------------------------------------------------------
065500020301     c     sr_f06        begsr
065600020301
065700060707     c                   eval      kkut = 1
065800020307     c                   eval      kcod = '15'
065900100122     c                   movel(P)  v1cnaz        kkey
066000020307     c     ktab          chain(n)  tabel00f
066100020307     c                   if        %found
066200020301     c                   eval      w1ftt = tblftt
066300020301     c                   eval      w1flt = tblflt
066400020301     c                   eval      w1ftr = tblftr
066500020301
066600020301     c                   move      tbldtr        dataR
066700020301     c                   eval      aad = aar
066800020301     c                   eval      mmd = mmr
066900020301     c                   eval      ggd = ggr
067000020301     c                   move      dataD         w1dtr
067100020307     c                   else
067200020307     c                   clear                   w1ftt
067300020307     c                   clear                   w1flt
067400020307     c                   clear                   w1ftr
067500020307     c                   clear                   w1dtr
067600020307     c                   endif
067700020301
067800020301     c                   do        *hival
067900020301     c                   exfmt     tb86w1
068000020301      * F12 - Ritorno
068100020301     c                   if        *inkl
068200020301     c                   leave
068300020301     c                   endif
068400020301
068500020301      * F6 - Conferma
068600020301     c                   if        *inkf
068700020301     c                   exsr      scrivi
068800020301     c                   eval      wf6 = *on
068900020301     c                   leave
069000020301     c                   endif
069100020301
069200020301     c                   enddo
069300020301
069400020301     c                   endsr
069500020301
069600020206      *----------------------------------------------------------
069700020206      *    Scrittura - Aggiornamento
069800020206      *----------------------------------------------------------
069900020206     c     scrivi        begsr
070000020206
070100020206      * Passo dati alla ds15
070200020206     c                   eval      §15des = v2des
070300020206     c                   eval      §15ecf = v2ecf
070400020206     c                   eval      §15pcf = v2pcf
070500020206     c                   eval      §15ucf = v2ucf
070600020206     c                   eval      §15vlt = v2vlt
070700020206     c                   eval      §15iso = v2iso
070800020206     c                   eval      §15cap = v2cap
070900150204     c                   eval      §15cfc = v2cfc
071000020206     c                   eval      §15eft = v2eft
071100020206     c                   eval      §15ita = v2ita
071200020206     c                   eval      §15cpr = v2cpr
071300020206     c                   eval      §15cod = v2cod
071400020206     c                   eval      §15cie = v2cie
071500020206     c                   eval      §15cpp = v2cpp
071600020206     c                   eval      §15lad = v2lad
071700060919     c                   eval      §15red = v2red
071800170112     c                   eval      §15dpdiso2 = v2dpdiso2
071900020206     c                   eval      §15efd = v2efd
072000020206     c                   eval      §15cei = v2cei
072100020206     c                   eval      §15dog = v2dog
072200020206     c                   eval      §15laf = v2laf
072300020206     c                   eval      §15sto = v2sto
072400020206     c                   eval      §15eff = v2eff
072500020206     c                   eval      §15cts = v2cts
072600020206     c                   eval      §15inl = v2inl
072700020206     c                   eval      §15ffn = v2ffn
072800020206     c                   eval      §15eex = v2eex
072900020412     c                   eval      §15acn = v2acn
073000140606     c                   eval      §15pref= v2pref
073100080604     c                   move      v2DPDpoe      §15dpdPOE
073200020311     c                   movel     v2ttc         §15ttc
073300060209     c                   eval      §15vas = v2vas
073400100115     c                   eval      §15ue  = v2ue
073500080423     c***                eval      §15cex = v2caeex
073600080423     c***                eval      §15cdp = v2cadpd
073700080423     c***                eval      §15cfe = v2cafed
073800160304     c
073900160304     c                   eval      §16pkd = v2pkd
074000160406     c                   eval      §16vld = v2vld
074100020206
074200060707     c                   eval      kkut = 1
074300020307     c                   eval      kcod = '15'
074400100122     c                   movel(P)  v1cnaz        kkey
074500020206     c     ktab          chain     tabel00f
074600020206
074700160307     c***                movel     ds15          tbluni
074800160307     c                   eval      tbluni=ds15
074900020301
075000020301     c                   if        wf16 = *on
075100020301     c                   eval      tblflg = '*'
075200020301     c                   endif
075300020301
075400020301     c                   if        wf5 = *on
075500020301     c                   clear                   tblflg
075600020301     c                   endif
075700020206
075800020301     c                   eval      tblftt = w1ftt
075900020206     c                   clear                   tblftr
076000020206     c                   clear                   tbldtr
076100020206
076200020206      * scrivo
076300020206     c                   if        not %found
076400060707     c                   eval      tblkut = kkut
076500020206     c                   eval      tblcod = kcod
076600020206     c                   eval      tblkey = kkey
076700020206     c                   write     tabel
076800160304     c* Scrivo anche estensione (tab 16) se dati tab 16 pieni
076900160406     c                   if        §16pkd>0 or §16vld>0
077000160307     c                   eval      kcod='16'
077100160307     c                   eval      tbluni=ds16
077200160307     c                   write     tabel
077300160307     c                   endif
077400020206
077500020206      * aggiorno
077600020206     c                   else
077700020206     c                   update    tabel
077800160304     c*
077900160304     c* agiorno estensione
078000160307     c                   eval      kkut = 1
078100160307     c                   eval      kcod = '16'
078200160307     c                   movel(P)  v1cnaz        kkey
078300160307     c     ktab          chain     tabel00f
078400160307     c                   eval      tbluni=ds16
078500160304
078600160406     c                   if        wf16 = *on or (§16pkd=0 and §16vld=0)
078700160307     c                   eval      tblflg = '*'
078800160307     c                   endif
078900160304
079000160406     c                   if        wf5 = *on or
079100160406     c                             (wf16= *off and (§16pkd>0 or §16vld>0))
079200160307     c                   clear                   tblflg
079300160307     c                   endif
079400160304
079500160307     c                   eval      tblftt = w1ftt
079600160307     c                   clear                   tblftr
079700160307     c                   clear                   tbldtr
079800160307     c                   if        %found(tabel00f)
079900160307     c                   update    tabel
080000160307     c                   else
080100160307     c                   eval      tblkut = kkut
080200160307     c                   eval      tblcod = kcod
080300160307     c                   eval      tblkey = kkey
080400160307     c                   write     tabel
080500160307     c                   endif
080600160304     c
080700020206     c                   endif
080800060707
080900060707      * se annullamento o ripristino devo fare la stessa cosa anche
081000060707      * sulle tabelle in lingua
081100060707     c                   if        wf16 = *on or wf5 = *on
081200060707      * aggancio le lingue presenti su AZLIN
081300060707     c                   move      kkut          kazlin
081400060707     c     kazlin        setgt     azlin01l
081500060707     c                   do        *hival
081600060707     c                   read      azlin01l
081700060707     c                   if        %eof(azlin01l)
081800060707     c                   leave
081900060707     c                   endif
082000060707      * per ogni lingua aggiorno
082100060707     c                   move      lintabel      kkut
082200060707     c                   eval      kcod = '15'
082300100122     c                   movel(P)  v1cnaz        kkey
082400060707     c     ktab          chain     tabel00f
082500060707     c                   if        %found(tabel00f)
082600060707     c                   if        wf16 = *on
082700060707     c                   eval      tblflg = '*'
082800060707     c                   endif
082900060707     c                   if        wf5 = *on
083000060707     c                   clear                   tblflg
083100060707     c                   endif
083200060707     c                   eval      tblftt = w1ftt
083300060707     c                   clear                   tblftr
083400060707     c                   clear                   tbldtr
083500160304     c                   update    tabel
083600160304     c* agiorno estensione
083700160307     c***                eval      kcod = '16'
083800160307     c***                movel(P)  v1cnaz        kkey
083900160307     c***  ktab          chain     tabel00f
084000160307     c***                if        %found(tabel00f)
084100160307     c***                movel     ds16          tbluni
084200160304
084300160307     c***                if        wf16 = *on or §16pkd=0
084400160307     c***                eval      tblflg = '*'
084500160307     c***                endif
084600160304
084700160307     c***                if        wf5 = *on and §16pkd>0
084800160307     c***                clear                   tblflg
084900160307     c***                endif
085000160304
085100160307     c***                eval      tblftt = w1ftt
085200160307     c***                clear                   tblftr
085300160307     c***                clear                   tbldtr
085400160307     c***                update    tabel
085500160307     c                   endif
085600160307     c***                endif
085700060707     c                   enddo
085800060707     c                   endif
085900020206
086000020206     c                   endsr
086100060707
086200060707      *---------------------------------------------------------------*
086300060707      * gestione tabella in lingua
086400060707      *---------------------------------------------------------------*
086500060707     c     sr_lingua     begsr
086600060707
086700060707      * pulisco il subfile
086800060707     c                   exsr      sr_pulsfl
086900060707      * carico il subfile con i dati che trovo o vuoto
087000060707     c                   exsr      sr_carsfl
087100060707      * emetto il subfile
087200060707     c     emisfl        tag
087300060707     c                   eval      rec = 1
087400060707     c                   if        savrec > 0
087500060707     c                   eval      rec = savrec
087600060707     c                   endif
087700060707     c                   write     tb86z1
087800060707     c                   exfmt     tb86c1
087900060707
088000060707      * F12 - Ritorno
088100060707     c                   if        *inkl
088200060707     c                   setoff                                       28
088300060707     c                   clear                   v1cmsg
088400060707     c                   goto      inz02
088500060707     c                   endif
088600060707
088700060707     c                   setoff                                       28
088800060707     c                   setoff                                       39
088900060707      * Controlli
089000060707     c  n05              exsr      sr_ctrsfl
089100060707     c   28
089200060707     cornkf              goto      emisfl
089300060707
089400060707      * F6 - Conferma
089500060707     c   kf              exsr      sr_confsfl
089600060707
089700060707     c                   endsr
089800060707
089900060707      *---------------------------------------------------------------*
090000060707      * pulisco il subfile
090100060707      *---------------------------------------------------------------*
090200060707     c     sr_pulsfl     begsr
090300060707
090400060707     c                   clear                   nrr
090500060707     c                   clear                   savrec
090600060707     c                   eval      *in20 = *off
090700060707     c                   eval      *in21 = *off
090800060707     c                   write     tb86c1
090900060707     c                   eval      *in20 = *on
091000060707     c                   eval      *in21 = *on
091100060707
091200060707     c                   endsr
091300060707
091400060707      *---------------------------------------------------------------*
091500060707      * carico il subfile
091600060707      *---------------------------------------------------------------*
091700060707     c     sr_carsfl     begsr
091800060707
091900060707     c                   setoff                                       30
092000060707      * carico un record per ogni lingua inserite in AZLIN
092100060707     c                   move      '1'           kazlin
092200060707     c     kazlin        setgt     azlin01l
092300060707     c                   do        *hival
092400060707     c                   read      azlin01l
092500060707      * fine file
092600060707     c                   if        %eof(azlin01l)
092700060707     c                   leave
092800060707     c                   endif
092900060707     c                   eval      s1dlin = lindesita
093000060707     c                   clear                   ds15
093100060707     c                   move      lintabel      kkut
093200060707     c                   eval      kcod = '15'
093300100122     c                   clear                   kkey
093400060707     c                   eval      kkey = v1cnaz
093500060707     c     ktab          chain(n)  tabel00f
093600060707     c                   if        not %found(tabel00f)
093700060707     c                   clear                   s1cdes
093800060707     c                   else
093900060707     c                   eval      ds15 = tbluni
094000060707     c                   eval      s1cdes = §15des
094100060707     c                   endif
094200060707     c                   eval      s1hcdlin = kkut
094300060707     c                   add       1             nrr
094400060707     c                   write     tb86s1
094500060707     c                   enddo
094600060707
094700060707     c                   seton                                        30
094800060707
094900060707     c                   endsr
095000060707
095100060707      *---------------------------------------------------------------*
095200060707      * controllo il subfile
095300060707      *---------------------------------------------------------------*
095400060707     c     sr_ctrsfl     begsr
095500060707
095600060707     c                   clear                   nrr
095700060707     c                   do        *hival
095800060707     c                   eval      nrr = nrr +1
095900060707     c     nrr           chain     tb86s1                             32
096000060707     c                   if        *in32
096100060707     c                   leave
096200060707     c                   endif
096300060707      * deve esserci la descrizione
096400060707     c                   if        s1cdes = *blanks
096500060707     c                   eval      v1cmsg = msg(01)
096600060707     c                   eval      *in28 = *on
096700060707     c                   eval      *in39 = *on
096800060707     c                   eval      savrec = nrr
096900060707     c                   update    tb86s1
097000060707     c                   leavesr
097100060707     c                   endif
097200060707     c                   update    tb86s1
097300060707     c                   enddo
097400060707
097500060707     c                   endsr
097600060707
097700060707      *---------------------------------------------------------------*
097800060707      * confermo i dati del subfile
097900060707      *---------------------------------------------------------------*
098000060707     c     sr_confsfl    begsr
098100060707
098200060707      * recupero i dati della tabella in italiano
098300060707     c                   clear                   savtbluni
098400060707     c                   eval      kkut = 1
098500060707     c                   eval      kcod = '15'
098600100122     c                   movel(P)  v1cnaz        kkey
098700060707     c     ktab          chain(n)  tabel00f
098800060707     c                   if        %found
098900060707     c                   eval      savtbluni = tbluni
099000060707     c                   endif
099100060707
099200060707      * leggo il subfile e aggiorno/scrivo i dati in lingua
099300060707     c                   clear                   nrr
099400060707     c                   do        *hival
099500060707     c                   eval      nrr = nrr +1
099600060707     c     nrr           chain     tb86s1                             32
099700060707     c                   if        *in32
099800060707     c                   leave
099900060707     c                   endif
100000060707     c                   eval      kkut = s1hcdlin
100100060707     c     ktab          chain     tabel00f
100200060707     c                   eval      ds15 = savtbluni
100300060707     c                   eval      §15des = s1cdes
100400060707     c                   eval      tbluni = ds15
100500060707     c                   eval      tblftt = w1ftt
100600060707     c                   clear                   tblftr
100700060707     c                   clear                   tbldtr
100800060707      * scrivo
100900060707     c                   if        not %found(tabel00f)
101000060707     c                   eval      tblkut = kkut
101100060707     c                   eval      tblcod = kcod
101200060707     c                   eval      tblkey = kkey
101300060707     c                   write     tabel
101400060707      * aggiorno
101500060707     c                   else
101600060707     c                   update    tabel
101700060707     c                   endif
101800060707
101900060707     c                   enddo
102000060707
102100060707     c                   endsr
102200020206      *-------------------------------------------------------------------------
102300020206** MSG  Lungh. 78                                                            *
102400020312Descrizione obbligatoria                                                      1
102500020312Codice Iso obbligatorio                                                       2
102600020312Divisa obbligatoria                                                           3
102700020312Codice iso per partita iva obbligatorio                                       4
102800020312Se Nazione senza cappario non richiedere il controllo cap/provincia           5
102900020312Linea arrivo DPD: inserire flag attraversamento dogana                        6
103000020312Linea arrivo FedEx: inserire flag attraversamento dogana                      7
103100020312Linea arrivo FedEx: inserire codice di tassazione                             8
103200020312Linea arrivo FedEx: inserire flag inoltro                                     9
103300020312Flag inoltro errato                                                           10
103400020312Divisa errata                                                                 11
103500020312Immettere il tipo tempo consegna FedEx                                        12
103600020312Tipo tempo consegna FedEx errato                                              13
103700141103I P.O.Emissione attualmente gestiti sono 190, 195 e 194.                      14
103800100122Se nazione UE il codice ISO per part.IVA non deve essere "$$". Enter forza    15
103900100122Non trovata la tabella del codice ISO. Inserire la "4I". enter forza          16
104000100122Aggionare anche tabella del codice ISO("4I") per appartenenza UE.Enter forza  17
104100100122Rendere congruente flag appartenenza UE con quello del codice ISO("4I").Forza 18
104200131218Codice Iso non valido                                                         19
104300140606Pref.tel.Inter.obbligatorio. NON deve avere zeri o il (+) alla sx. del cod.   20
104400160304Limite di peso inseribile solo se la nazione prevede il servizio DPD          21
104500160304Il limite di peso non può superare i                                          22
104600160406Limite di volume inseribile solo se la nazione prevede il servizio DPD        23
