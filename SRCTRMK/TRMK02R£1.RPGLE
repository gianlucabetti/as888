000100080222     H DATEDIT(*YMD) option(*nodebugio) decedit('0,')
000200091104     h dftactgrp(*no) actgrp(*caller)
000300091028     F* TRMK02R *----------------------------------------------------*
000400920415     F*           GESTIONE ANAGRAFICA CLIENTI POTENZIALI             *
000500981012     F* -------------------------------------------------------------*
000600081003     FTNCPO01L  UF A E           K DISK
000700080207     FTNCPO05L  IF   E           K DISK    rename(tncpo000:tncpo005)
000800080207     F                                     prefix(c_)
000900080207     FTNCPO06L  IF   E           K DISK    rename(tncpo000:tncpo006)
001000080207     F                                     prefix(c_)
001100140714     FTNCPO11L  UF A E           K DISK
001200081003     FTICPI01L  iF   E           K DISK
001300091028     Ftiatc04l  if   e           k disk
001400130808     fAZCMM01L  if   e           k disk
001500920417     FAZORG01L  IF   E           K DISK
001600140910     FTFNTC01L  UF a E           K DISK
001700091028     fTicpn01l  if   e           k disk
001800060714     FTABEL00F  iF   E           K DISK
001900120703     FTRMK02D   CF   E             WORKSTN
002000041207
002100041207     d codut           s              1  0 Inz(1)
002200050518     d ktnt            s                   like(ntctnt)
002300050518     d knk2            s                   like(ntcnk2) inz
002400050518     d conta           s              1  0 Inz(1)
002500060714     d savprv          s                   like(cpoprv)
002600080206     d savnaz          s                   like(vidnaz)
002700080207     d savcdf          s                   like(vidcdf) inz
002800080207     d cod_forzcdf     s                   like(cpocdf) inz
002900080207     d cod_forziva     s                   like(cpopiv) inz
003000080626     d savrag          s             15
003100080626     d savfsf          s              1
003200080207     d saviva          s                   like(vidiva) inz
003300080207     d codice          s                   like(vidiva) inz
003400080206     d wforzacdf       s              1    inz(*Off)
003500080206     d wforzaiva       s              1    inz(*Off)
003600080206     d wforzaiva1      s              1    inz(*Off)
003700131126     d $logistica      s              1    inz(*Off)
003800090518     d ktpf            s                   like(cpitpf)
003900100120     d Immissione      s              1n   inz(*off)
004000100409     d W_daatti        s              1
004100100810     d $inke           s               n
004200041207
004300920415      * TABELLE------------------------------------------------------*
004400951009     D*
004500981123     D TES             S             38    DIM(3) CTDATA PERRCD(1)
004600080205     D MSG             S             78    DIM(51) CTDATA PERRCD(1)
004700891116     D*--------------------------------------------------------------*
004800920417      *
004900090716     d ds1t          e ds
005000080207     D DS1L          E DS
005100110203     d dcpo          e ds
005200920417      *
005300900328     D KPJBA         E DS
005400920417      *
005500981109     D* PASSAGGIO PARAMETRI TRA PGM  DI POTENZIALI
005600091028     D trmk01ds      E DS
005700080925     D DSMK50        E DS                  EXTNAME(TRMK50DS)
005800091028     D trmk17ds      E DS
005900100409     D trmk19ds      E DS
006000100617     d sav_trmk19    e ds                  extname(TRMK19DS)
006100100617     d                                     prefix(w_)
006200091028     d trmk20ds      e ds
006300091028     d TRMK21ds      e ds                  inz
006400130808     d TRMK43ds      e ds                  inz
006500100809     d TNTA88DS      e ds
006600920417     D*
006700941125     D WLBDA8          DS
006800941125     D  G08DAT                 1      8  0
006900941125     D  G08INV                 9     16  0
007000941125     D  G08ERR                17     17
007100941125     D  G08TGI                18     22  0
007200941125     D*
007300970711     D* DS PER FNLV13R - CONTROLLO DI UN CAP
007400970711     D DSLV13        E DS                  EXTNAME(FNLV13DS)
007500970711     D* DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
007600970711     D DSLV14        E DS                  EXTNAME(FNLV14DS)
007700970711     D* DS PER TISI95R - CONTROLLO DI CAP
007800970711     D DSSI95        E DS                  EXTNAME(TISI95DS)
007900981014     D* Ds esterna x richiamo pgm controllo fax
008000981014     D TRUL42        E DS                  EXTNAME(TRUL42DS)
008100020708     D* DS PER TRUL33R - REPERIMENTO AGGIORNAMENTO NUMERATORI in GRU
008200020708     D TRUL33DS      E DS
008300021017
008400090626     d tntaa1ds      e ds
008500041207
008600041207     d Azuteds       e ds                  extname(Azute00f)
008700041207     d dDatiute      e ds
008800100922     d dute01        e ds
008900041207     d Tibs34ds      e ds
009000060713     d dspr          e ds
009100081003     d dcpo01        e ds
009200140910     d dcpo02        e ds
009300090716     d tnta12ds      e ds
009400091030     d tibs02ds      e ds
009500091125     d tnta69ds      e ds
009600081003     d
009700080205      * Partita I.V.A.
009800080205     D                 DS
009900080205     D  Vidiva                 1     16
010000080205     D  Vidivaeu               1      2
010100080205     D  Vidivait               3     16
010200080205
010300080205     d xcfivads      e ds
010400080205     d  ivaeu                  3      4
010500080205     d  ivait                  5     18
010600130808
010700130808     d c_Digits        c                   const('0123456789')
010800130808      *
010900080222     D* CAMPO PER DSPATR "HI"       DEI CAMPI A VIDEO
011000080208     D hi              C                   CONST(X'22')
011100080222     D* CAMPO PER DSPATR "normal" (per togliere hi)
011200080222     D nm              C                   CONST(X'20')
011300020919
011400020919      * USO DEGLI INDICATORI-----------------------------------------*
011500981012      * 02    : INDICA L'INTERROGAZIONE
011600100202      * 03    : richiamato da menù
011700120120      * 05    : Abilita F5=Attività
011800120120      * 06    : Abilita F22=Richiesta Contatto
011900981014      * 10    : Record non trovato su TNCPO00F- IMMISSIONE
012000091028      * 11    : F14 Pink - Info commerciali presenti
012100110203      * 12    : categoria potenziale <> da 'M'
012200140716      * 13    : Record non trovato su TNCPO10F- IMMISSIONE
012300981105      * 28    : EMISSIONE MESSAGGIO DI ERRORE
012400120703      * 40-56 : POSIZIONAMENTO CURSORE
012500981105      * 59    : PGM RICHIAMATO DA ANNULLAMENTO VISITA: INIBISCO SCELTA
012600981105      *          1-VISITE / F16-ANNULLA / F12-RITORNO
012700981111      * 60-64 : ERRORE ERRMSG IN VID5
012800120703      * 66    : Posizionamento cursore
012900120703      * 67    : Posizionamento cursore
013000100506      * 77    : F05 Pink - Attività
013100091028      * 78    : F02 Pink - Rubrica presente
013200091028      * 79    : F18 Pink - Note presenti
013300981012      * 89    : ON --> P.O. - OFF --> SEDE
013400981012      * 90    : ERRORE GENERICO
013500981012      *
013600920415     C*----------------------------------------------------------------*
013700941111     C* MAIN LINE
013800941111     C*----------------------------------------------------------------*
013900920415      *
014000981009     C**
014100981009     C                   MOVEL     VIDCP1        CPOCPO
014200981009     C                   MOVE      VIDCP2        CPOCPO
014300050629     C* CONTROLLI sul potenziale
014400050629     C                   EXSR      CTRcpo
014500130808      *
014600981009     C**
014700981009     C* SE IL POTENZIALE ESISTE CARICO I DATI
014800981009     C  N10              EXSR      CARICA
014900981014     C* PER IMMISSIONE NON CI POSSONO ESSERE STATI NELLO STORICO
015000091104     C   10              SETON                                        09
015100920612      *
015200091028      * Verifico sul file note se già ci sono note per il potenziale
015300091028      * Se ci Pink F18
015400091028     C                   setoff                                       79
015500091028     c     cpocpo        chain     ticpn01l
015600091028     c                   if        %found(ticpn01l)
015700091028     c                   eval      *in79 = *on
015800091028     c                   endif
015900091028      * Verifico sul TFNTC01L se già ci sono contatti per il potenziale
016000091028      * Se ci sono Pink tasto F2-Rubrica
016100920612     C                   MOVE      'P'           FLAG02
016200920612     C                   MOVEL     VIDCP1        CLIPOT
016300920612     C                   MOVE      VIDCP2        CLIPOT
016400091028     C                   setoff                                       78
016500010130     C     knote         setll     tfntc01l
016600010130     C                   do        *hival
016700091029     C     knote         reade     tfntc01l
016800010130     C                   if        %eof(tfntc01l)
016900010130     C                   leave
017000010130     C                   endif
017100090716     c                   if        ntcflt = 'A'
017200090716     c                   iter
017300090716     c                   endif
017400090716      * controllo se Nota o Contatto
017500090716     c                   clear                   ds1t
017600090716     c                   eval      cod = '1T'
017700090716     c                   eval      key = ntctnt
017800091029     c     ktab          chain     tabel00f
017900090716     c                   if        %found(tabel00f) and tblflg = ' '
018000090716     c                   eval      ds1t = tbluni
018100090716     c                   endif
018200090716      * Nota
018300090716     c                   if        §1trich = 'N'
018400091028     c                   iter
018500090716     c                   endif
018600090716      * Contatto
018700090716     c                   if        §1trich = 'C'
018800090716     c  n78              eval      *in78 = *on
018900091028      * esco dal ciclo, ne basta uno per Pink tasto F2=Rubrica
019000091028     C                   leave
019100090716     c                   endif
019200010130     C                   enddo
019300981105      *
019400981014     C* IN INSERIMENTO DATA ACQUISIZIONE = DATA GIORNO
019500981014     C   10              Z-ADD     GMADAT        VIDDAC
019600981123     C* SE RICHIAMATO E PASSATTO MESSAGGIO DI ERRORE LO VISUALIZZO
019700091028     C     mk01K10       IFNE      ' '
019800091028     C     mk01K10       ANDNE     'I'
019900091028     C     mk01MSG       ANDNE     *BLANKS
020000091028     C                   MOVEL     mk01MSG       VIDMSG
020100981123     C                   SETON                                        28
020200981123     C                   ENDIF
020300940318     C*
020400981012     C     FOR02         TAG
020500100810     c                   eval      $inke = *off
020600050518      * carico il responsabile trasporti dalla nota '05'
020700050518     c                   Eval      conta = 1
020800050518     c                   Eval      flag02 = 'P'
020900050518     c                   Movel     vidcp1        clipot
021000050518     c                   Move      vidcp2        clipot
021100050518     c                   Eval      ktnt = '05'
021200050518     c     knote01       Setll     Tfntc01l
021300050518     c                   Do        *Hival
021400091029     c     knote01       Reade     Tfntc01l
021500050518     c                   If        %Eof(Tfntc01l)
021600050518     c                   Leave
021700050518     c                   EndIf
021800050518     c                   If        conta = 1
021900050518     c                   Eval      vidrsp1 = ntcrnt
022000050518     c                   Else
022100080213     c                   Eval      vidrsp1 = %trim(vidrsp1) + ', ' +
022200080205     c                             ntcrnt
022300050518     c                   EndIf
022400050518     c                   add       1             conta
022500050518     c                   EndDo
022600100528      *
022700120120      * - Abilitazione F5=Attività sempre se nel sistema informativo di filiale
022800120120      *                            solo in interrogazione se S.I. di sede
022900120120     c                   IF        %subst(KNSIF : 1 : 3) = 'FIL'
023000120120     c                   eval      *in05 = *on
023100120120     c                   ELSE
023200120120     c                   eval      *in05 = *in02
023300120120     c                   ENDIF
023400130808      *
023500120120      * - Abilitazione F22=Richiesta contatto se nel S.I. di filale
023600120120      * - e solo se cliente in categoria <> da codificato
023700120120     c                   IF        %subst(KNSIF : 1 : 3) = 'FIL' and
023800120120     c                             CPOfls <> 'C'
023900120120     c                   eval      *in06 = *on
024000110209     c                   ENDIF
024100131126      *
024200131126      * - Disattivo il tasto F22= Rischiesta contatto e tasto F5= Attivita
024300131126      * - Solo in interrogazione se richiamato dalla logistica
024400131126     c                   If        $logistica = *on
024500131126     c                   eval      *in05 = *in02
024600131126     c                   eval      *in06 = *off
024700131126     c                   Endif
024800130808      *
024900100617      * Controllo se ci sono attività
025000100506      * Pink F05-Attività
025100091028     c     cpocpo        chain     tiatc04l
025200091028     c                   if        %found(tiatc04l)
025300091028     c                   eval      *in77 = *on
025400091028     c                   else
025500091028     c                   eval      *in77 = *off
025600091028     c                   endif
025700920612      *
025800120703     C                   EXFMT     MK02D01
025900941125     C*
026000941125     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN90)
026100941125     C                   CLEAR                   VIDMSG
026200941125     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
026300021127     C                   SETOFF                                       65
026400981012     C                   SETOFF                                       404142
026500981012     C                   SETOFF                                       434445
026600981012     C                   SETOFF                                       464748
026700981012     C                   SETOFF                                       495051
026800981012     C                   SETOFF                                       525354
026900981014     C                   SETOFF                                       902855
027000080205     C                   SETOFF                                       5666
027100981014     C* F3  - FINE
027200981109     C     *INKC         IFEQ      *ON
027300091028     C                   MOVEL     '1'           mk01F03
027400981109     C                   GOTO      FINE
027500981109     C                   ENDIF
027600981014     C* F12 - RITORNO
027700050629     C                   IF        *inkl
027800091028     C                   MOVEL     '1'           mk01F12
027900110601     c                   IF        w_DaAtti = 'S' and CPOfls = 'C'
028000110601     c                   clear                   mk01f06
028100110601     c                   ENDIF
028200981014     C                   GOTO      FINE
028300981014     C                   ENDIF
028400100122      * attivo in questo punto il controllo del commerciale
028500100122      * se sono in manutenzione e premo qualsiasi tasto (che non sia l'enter)
028600100122      * devo dare errore se manca il commerciale
028700101202      * o se commerciale con particolarità impostata sulla tabella
028800100122     c                   IF        not *in02 and not *in10 and
028900100122     c                             (*inkb or *inks or *inku or
029000100524     c                              *inkw or *inkn or *inke) and
029100101202     c                             (vidcmm = *zeros or vidcmm = *blanks
029200130808     c                              or CMMpar <> *blanks)
029300100122     C                   SETON                                        502890
029400100319     C                   eval      vidmsg = msg(04)
029500100122     C                   GOTO      FOR02
029600100122    2C                   ENDIF
029700940318     C*
029800130808      *
029900091125      * F20 - Interrogazione codici collegati (anagrafica clienti)
030000091125     c                   if        *inku
030100091125     c                   clear                   tnta69ds
030200091125     c  n02              eval      Ita69Fle = '2'
030300091125     c   02              eval      Ita69Fle = '5'
030400091125     c                   eval      Ita69Cpo = cpocpo
030500091125     c                   call      'TNTA69R'
030600091125     c                   parm                    kpjba
030700091125     c                   parm                    tnta69ds
030800091125     c                   goto      for02
030900091125     c                   endif
031000981012     C**
031100981012     C** CONTROLLO FORMATO2
031200981124     C                   EXSR      CTRD02
031300981012     C   90              GOTO      FOR02
031400100524      * Se F22 = Richiesta contatto
031500100120      * prima controllo se commerciale già inserito
031600100120      * altrimenti errore
031700100524     c                   IF        *inkw and
031800100120     c                             (vidcmm = *zeros or vidcmm = *blanks)
031900100120     C                   SETON                                        502890
032000100319     C                   eval      vidmsg = msg(04)
032100100120     C                   GOTO      FOR02
032200100120    2C                   ENDIF
032300940321     C*
032400981110     C* IN GESTIONE CON ENTER, RIEMETTO VIDEATA,
032500091030     C*  IN INTERROGAZIONE ESCO se no tasti funzione
032600981110     C     *IN02         IFEQ      *OFF
032700981110     C     *INKF         ANDEQ     *OFF
032800020708     C     *INKN         ANDEQ     *OFF
032900100524     C     *INKW         ANDEQ     *OFF
033000100506     C     *INKe         ANDEQ     *OFF
033100140918     c     *inkb         andeq     *off
033200140918     c     *inks         andeq     *off
033300981110     C                   GOTO      FOR02
033400981110     C                   ENDIF
033500940318     C*
033600981027     C* F6 O F8 - CONFERMA
033700091028     C     mk01K10       IFNE      'N'
033800940321     C                   EXSR      AGGIOR
033900981102     C                   ENDIF
034000140918      * f2 - Inserimento o Variazione Rubrica
034100140918     c                   if        *inkb
034200140918     c                   clear                   tnta12ds
034300140918     c  n02              Eval      ta12ric = 'M'
034400140918     c   02              Eval      ta12ric = 'V'
034500140918     c                   Eval      ta12apl = 'P'
034600140918     c                   Eval      ta12pot = vidcp1 * 100000000 + vidcp2
034700140918     c                   Eval      ta12rag = vidrag
034800140918     c                   Call      'TNTA12R'
034900140918     c                   Parm                    kpjba
035000140918     c                   Parm                    tnta12ds
035100141027      * riaggancio l'anagrafico
035200141027     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
035300141027     c   02cpocpo        chain(n)  tncpo01l                           10
035400141027     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
035500141027     c   02cpocpo        chain(n)  tncpo11l                           13
035600141027     c                   if        %error
035700141027     c                   eval      mk01m10 = msg(3)
035800141027     c                   eval      mk01err = 'E'
035900141027     c                   goto      fine
036000141027     c                   endif
036100140918     c                   goto      for02
036200140918     c                   endif
036300140918      *
036400140918      * CMD18 - Inserimento o Variazione NOTE
036500140918     C     *INKS         IFEQ      '1'
036600140918     c                   clear                   trmk20ds
036700140918     c   02              eval      IMK20tla = 'L'
036800140918     c   02              eval      imk20flm = 'I'
036900140918     C                   eval      imk20cpo = cpocpo
037000140918     C                   eval      imk20rsc = vidrag
037100140918     c                   call      'TRMK20R'
037200140918     c                   parm                    kpjba
037300140918     c                   parm                    trmk20ds
037400141027      * riaggancio l'anagrafico
037500141027     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
037600141027     c   02cpocpo        chain(n)  tncpo01l                           10
037700141027     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
037800141027     c   02cpocpo        chain(n)  tncpo11l                           13
037900141027     c                   if        %error
038000141027     c                   eval      mk01m10 = msg(3)
038100141027     c                   eval      mk01err = 'E'
038200141027     c                   goto      fine
038300141027     c                   endif
038400140918     C                   GOTO      FOR02
038500140918     C                   END
038600981102     C**
038700081015     C* PER F14 - INFORMAZIONI COMMERCIALI
038800080925     C                   CLEAR                   DSMK50
038900981103     C     *INKN         IFEQ      *ON
039000080925     C                   MOVEL     CPOCPO        I50cpo
039100091029     C     mk01K10       IFEQ      'N'
039200080925     C                   MOVEL     'I'           I50MOD
039300981102     C                   ELSE
039400080925     C                   MOVEL     'G'           I50MOD
039500081015     C                   MOVEL     ' '           I50OBL
039600981102     C                   ENDIF
039700080925     C                   MOVEL     CPORAG        I50rag
039800081003     c                   movel     §cpoifotot    i50ifotot
039900981102     C**
040000080925     C                   CALL      'TRMK50R'
040100981102     C                   PARM                    KPJBA
040200080925     C                   PARM                    DSMK50
040300981103     C                   ENDIF
040400920415      *
040500020722     C* GESTIONE Ritorno/Fine INFO COMMERC. NEI DUE CASI
040600020722     C* DI PGM RICHIAMATO E NON RICHIAMATO
040700080925     C     O50F12        IFEQ      'S'
040800020723      * riaggancio l'anagrafico
040900070523     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
041000070523     c   02cpocpo        chain(n)  tncpo01l                           10
041100140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
041200140716     c   02cpocpo        chain(n)  tncpo11l                           13
041300070523     c                   if        %error
041400091029     c                   eval      mk01m10 = msg(3)
041500091029     c                   eval      mk01err = 'E'
041600070523     c                   goto      fine
041700070523     c                   endif
041800020722     C                   GOTO      FOR02
041900020722     C                   ENDIF
042000080925     C     O50F03        IFEQ      'S'
042100020722     C                   GOTO      FINE
042200020722     C                   ENDIF
042300091028      *
042400100524      * F22 = Richiesta contatto
042500100524     c                   if        *inkw
042600091028     c                   clear                   TRMK17ds
042700100114     c                   eval      imk17patt = 'S'
042800100114     c                   eval      imk17att = 'T'
042900100114     c                   eval      imk17pcau = 'S'
043000100114     c                   eval      imk17cau = '19'
043100091028     c                   eval      imk17cpo = CPOcpo
043200091028     c                   if        vidcmm <> *blanks
043300091028     c                   eval      imk17cmm= %dec(vidcmm:7:0)
043400091028     c                   endif
043500091028     c                   call      'TRMK17R'
043600091028     c                   parm                    kpjba
043700091028     c                   parm                    trmk17ds
043800091028      * riaggancio l'anagrafico
043900091028     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
044000091028     c   02cpocpo        chain(n)  tncpo01l                           10
044100140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
044200140716     c   02cpocpo        chain(n)  tncpo11l                           13
044300091028     c                   if        %error
044400091028     c                   eval      mk01m10 = msg(3)
044500091028     c                   eval      mk01err = 'E'
044600091028     c                   goto      fine
044700091028     c                   endif
044800091028     C                   GOTO      FOR02
044900091028     c                   endif
045000130808      *
045100100506      * F05 = Interrogazione attività
045200100810     c                   if        *inKe
045300100617     c                   clear                   TRMK21ds
045400091028     c                   eval      I21cpo = CPOcpo
045500091028     c                   eval      I21rsc = VIDrag
045600100810      * se richiamato dalla gestione attività/trattative deve andare in gestione
045700100810      * con visualizzazione sempre delle attività e senza dare il msg di altre attività
045800100810      * in window
045900100810     c                   IF        w_DaAtti = 'S'
046000100810     c                   eval      $inke = *on
046100100810     c                   clear                   omk19cpo
046200100810     c                   clear                   omk19ksc
046300100810     c                   clear                   omk19err
046400100810     c                   clear                   omk19msg
046500100810     c                   eval      I21mod = 'V'
046600100810     c                   eval      sav_trmk19 = trmk19ds
046700100810     c                   eval      KPJBU  = TRMK21ds
046800100810     c                   eval      IMK19cpo = CPOcpo
046900100810     c                   call      'TRMK21R'
047000100810     c                   parm                    kpjba
047100100810     c                   parm                    trmk19ds
047200100810      * se richiamato da interrogazione o da menù deve andare in interrogazione
047300100810     c                   ELSE
047400100810     c                   eval      I21mod = 'I'
047500091028     c                   eval      KPJBU  = TRMK21ds
047600091028     c                   call      'TRMK21R'
047700091028     c                   parm                    kpjba
047800100810     c                   ENDIF
047900091028     c                   eval      TRMK21ds = KPJBU
048000091028      * riaggancio l'anagrafico
048100091028     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
048200091028     c   02cpocpo        chain(n)  tncpo01l                           10
048300140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
048400140716     c   02cpocpo        chain(n)  tncpo11l                           13
048500091028     c                   if        %error
048600091028     c                   eval      mk01m10 = msg(3)
048700091028     c                   eval      mk01err = 'E'
048800091028     c                   goto      fine
048900091028     c                   endif
049000100810      * da attività
049100100810     c                   IF        w_DaAtti = 'S'
049200100810      * rimetto la trmk19ds come prima
049300100810     c                   eval      trmk19ds = sav_trmk19
049400100810      * se F12 riemetto la videata del potenziale
049500100810     c                   IF        O21fxx = '12'
049600100810     c                   clear                   mk01f06
049700100810     c                   goto      for02
049800100810     c                   ENDIF
049900100810      * se ho gestito l'attività dal TRMK21R vado a fine
050000100810     c                   IF        O21fxx = '99'
050100100810     c                   clear                   mk01f06
050200100810     c                   goto      fine
050300100810     c                   ENDIF
050400100810      * se ho dato F10 vdevo andare avanti con il TRMK19R o TNTA88R
050500100810     c                   IF        O21fxx = '10'
050600100810     c                   clear                   mk01f06
050700100810     c                   ENDIF
050800100810      * se non ho fatto niente spengo $inke e vado avanti
050900100810     c                   IF        O21fxx = *blanks
051000100810     c                   eval      $inke = *off
051100100810     c                   ENDIF
051200100810     c                   ELSE
051300091028     C                   GOTO      FOR02
051400100810     c                   ENDIF
051500091028     c                   endif
051600020722      *
051700091028     C                   MOVEL     VIDCP1        mk01CPO
051800091028     C                   MOVE      VIDCP2        mk01CPO
051900091028     C                   MOVEL     VIDRAG        mk01RAG
052000091028     C                   MOVEL     '1'           mk01F06
052100130808      *
052200100409      * se sono stato richiamato dalla gestione attività richiamo il programma trmk19
052300100810      * se non sono già passata per Inke
052400100810     c                   If        W_Daatti = 'S'
052500100617     c                   clear                   omk19cpo
052600100617     c                   clear                   omk19ksc
052700100617     c                   clear                   omk19err
052800100617     c                   clear                   omk19msg
052900110518      * Se potenziale codificato e non sono passata da F5-Attività non devo dare la
053000110518      * possibilità di fare una nuova attività o aprire trattativa.
053100110518     c                   IF        CPOfls = 'C'
053200110518     c                   eval      *in28 = *on
053300110518     c                   eval      *in90 = *on
053400110518     c                   eval      vidmsg =
053500110518     c                             'Nuova Attività/Trattativa non possibile-
053600110518     c                              per Potenziale CODIFICATO.'
053700110518     c                   goto      for02
053800110518     c                   ENDIF
053900100809      * Prima di passare al trmk19r o TNTA88R
054000100809      * richiamo il trmk21r per dare la possibilità di gestire
054100100617      * eventuali attività ancora da eseguire
054200100617      * *in77 = F5-Attività in pink (attività presenti sul potenziale)
054300100810     c                   IF        *in77 and not $inke
054400100617      * salvo la trmk19ds così come mi è arrivata
054500100617     c                   eval      sav_trmk19 = trmk19ds
054600100617     c                   clear                   TRMK21ds
054700100617     c                   eval      I21mod = 'G'
054800100617     c                   eval      I21cpo = CPOcpo
054900100617     c                   eval      I21rsc = VIDrag
055000100617     c                   eval      KPJBU  = TRMK21ds
055100100617     c                   eval      IMK19cpo = CPOcpo
055200100617     c                   call      'TRMK21R'
055300100617     c                   parm                    kpjba
055400100617     c                   parm                    trmk19ds
055500100617     c                   eval      TRMK21ds = KPJBU
055600100706      * rimetto la trmk19ds come prima
055700100706     c                   eval      trmk19ds = sav_trmk19
055800100706      * se F12 riemetto la videata del potenziale
055900100706     c                   IF        O21fxx = '12'
056000100706     c                   clear                   mk01f06
056100100706     c                   goto      for02
056200100706     c                   ENDIF
056300100706      * se ho gestito l'attività dal TRMK21R vado a fine
056400100617     c                   IF        O21fxx = '99'
056500100617     c                   clear                   mk01f06
056600100617     c                   goto      fine
056700100617     c                   ENDIF
056800100617     c                   ENDIF
056900100617      * richiamo la gestione attività
057000100809      * o gestione trattative, dipende da dove arrivo
057100100430     c                   clear                   omk19cpo
057200100430     c                   clear                   omk19ksc
057300100430     c                   clear                   omk19err
057400100430     c                   clear                   omk19msg
057500100910     c                   IF        IMK19tco = 'T'
057600100409     c                   call      'TRMK19R'
057700100409     c                   Parm                    kpjba
057800100409     c                   Parm                    trmk19ds
057900100409     c                   parm                    trmk01ds
058000100910     c                   endif
058100100910     c                   IF        IMK19tco = 'O'
058200100910     c                   clear                   tnta88ds
058300101111     c                   eval      ita88cmmi = imk19com
058400100910     c                   eval      ita88fle = imk19fle
058500100910     c                   eval      ita88cpo = mk01cpo
058600100910     c                   call      'TNTA88R'
058700100910     c                   Parm                    kpjba
058800100910     c                   Parm                    tnta88ds
058900100910     c                   clear                   mk01f06
059000100910     c                   ENDIF
059100100409     c                   endif
059200130808     c*
059300080925     C                   GOTO      FINE
059400920415      *
059500920415     C     FINE          TAG
059600970711     C*
059700970711     C                   MOVEL     'C'           I13TLA
059800970711     C                   CALL      'FNLV13R'
059900970711     C                   PARM                    KPJBA
060000970711     C                   PARM                    DSLV13
060100970711     C                   PARM                    DSSI95
060200970711     C*
060300970711     C                   MOVEL     'C'           I14TLA
060400970711     C                   CALL      'FNLV14R'
060500970711     C                   PARM                    KPJBA
060600970711     C                   PARM                    DSLV14
060700090626     C**
060800080925     C                   CLEAR                   DSMK50
060900080925     C                   MOVEL     'C'           I50TLA
061000080925     C                   CALL      'TRMK50R'
061100981102     C                   PARM                    KPJBA
061200080925     C                   PARM                    DSMK50
061300090626     C*
061400090626     C                   clear                   tntaa1ds
061500090626     C                   movel     'C'           Itaa1Tla
061600090626     C                   movel(p)  tntaa1ds      kpjbu
061700090703     C                   CALL      'TNTAA1C'
061800090626     C                   PARM                    KPJBA
061900971203     C**
062000920415     C                   SETON                                        LR
062100920416      *-------------------------------------------------------------------------
062200920416      *SUBROUTINE RICERCA ELEMENTI DI TABELLA
062300940317      *-------------------------------------------------------------------------
062400981012     C     CERTAB        BEGSR
062500920416     C                   MOVEL     CODUT         §KUT
062600981009     C                   MOVEL     COD           §COD
062700981103     C                   CLEAR                   §KEY
062800920416     C                   MOVE      *BLANKS       VIDELE
062900920416     C                   CALL      'X§TABER'
063000920416     C                   PARM                    §KUT
063100920416     C                   PARM                    §COD
063200920416     C                   PARM                    §KEY
063300920416     C                   PARM                    §DES             30
063400920416     C                   MOVEL     §KEY          VIDELE           10
063500920416     C                   MOVEL     §DES          DESELE           30
063600920416     C                   ENDSR
063700981009     C*---------------------------------------------------------------
063800050629     C* CONTROLLO potenziale
063900981009     C*---------------------------------------------------------------
064000050629     C     CTRcpo        BEGSR
064100081003     c                   clear                   dcpo01
064200140910     c                   clear                   dcpo02
064300140930     c                   clear                   cpotel
064400981009      *
064500981009     C* CHAIN
064600981009     C                   MOVEL     VIDCP1        CPOCPO
064700981009     C                   MOVE      VIDCP2        CPOCPO
064800981009     C**
064900981014    0C     CPOCPO        IFGT      0
065000091028     C     mk01K10       IFNE      'N'
065100981014     C     CPOCPO        CHAIN     TNCPO01L                           1094
065200140716     C     CPOCPO        CHAIN     TNCPO11L                           13
065300981023     C                   ELSE
065400981023     C     CPOCPO        CHAIN(N)  TNCPO01L                           10
065500140716     C     CPOCPO        CHAIN(n)  TNCPO11L                           13
065600981023     C                   ENDIF
065700981023     C***
065800981009     C* 94 ON  - RECORD ALLOCATO
065900981009    1C     *IN94         IFEQ      *ON
066000091028     C                   Eval      mk01m10 = msg(3)
066100091028     c                   Eval      mk01err = 'E'
066200050629     C                   GOTO      fine
066300981009    1C                   ENDIF
066400981014     C* SE NON IMMESSO POTENZIALE ACCENDO INDICATORE 10
066500981014     C                   ELSE
066600981014     C                   SETON                                        10
066700981014    0C                   ENDIF
066800021023      *
066900981009     C*
067000100120     C* - I M M I S S I O N E
067100100120    1C     Immissione    IFEQ      *ON
067200981009     C*
067300981009     C* CODICE GIA' ESISTENTE: ERRORE
067400981009    2C     *IN10         IFEQ      *OFF
067500091028     C                   Eval      mk01m10 = msg(17)
067600091028     c                   Eval      mk01err = 'E'
067700050629     C                   GOTO      fine
067800981009   X2C                   ELSE
067900020708      *
068000981009     C* SE NON IMMESSO NUMERO LO PRENDO DAL NUMERATORE (DS1S)
068100020708    3C*  10VIDCP2        IFEQ      *ZEROS
068200041026      * LO PRENDO SEMPRE DA SIMFEL+NUMERATORE
068300981009     C                   Z-ADD     SIMFEL        VIDCP1
068400020709      *
068500020709     C                   setoff                                       94
068600020708     C                   clear                   Trul33ds
068700020712     C                   Z-ADD     vidcp1        I33PO1
068800020709     C                   Z-ADD     0             I33OPE
068900020712     C                   Z-ADD     23            I33CNU
069000020709     C                   Z-ADD     1             I33NUM
069100020712     C     *in94         doweq     *off
069200020708     C                   movel(p)  trul33ds      kpjbu
069300020708     C                   CALL      'TRUL33R'
069400020708     C                   PARM                    KPJBA
069500020708     C                   eval      trul33ds = kpjbu
069600020708      *
069700020709     C                   if        O33ERR = *ZEROS
069800020709      *
069900020708      * Controllo che il numero non esista gia' nel file potenziali
070000020709     C                   eval      cpocpo = (Vidcp1*100000000) + O33NRI
070100981023     C     CPOCPO        CHAIN(N)  TNCPO01L                           94
070200020723      * Trul33 in errore
070300020723     C                   ELSE
070400091028     C                   Eval      mk01m10 = msg(46)
070500091028     c                   Eval      mk01err = 'E'
070600050629     C                   GOTO      fine
070700020712     C                   ENDIF
070800981009    4C                   ENDDO
070900020709     C                   MOVE      CPOCPO        vidcp2
071000040930     c                   Clear                   Cpoatb
071100020708    2C                   ENDIF
071200981009     C*
071300981009   X1C                   ELSE
071400981009     C* CODICE INESISTENTE
071500981009    2C     *IN10         IFEQ      *ON
071600091028     C                   Eval      mk01m10 = msg(19)
071700091028     c                   Eval      mk01err = 'E'
071800050629     C                   GOTO      fine
071900981014   X2C                   ELSE
072000981014     C**
072100090626     C* CONTROLLO abilitazioni utente
072200091028     C     mk01K10       Ifne      'N'
072300091028     C     mk01K10       ANDNE     'C'
072400091028     C     mk01K10       ANDNE     'B'
072500090626     c*
072600090626     c                   clear                   tntaa1ds
072700090626     c                   eval      itaa1cpo =cpocpo
072800090626     c                   eval      itaa1caut='§UTEPOT'
072900090626     c                   clear                   kpjbu
073000090626     c                   movel     tntaa1ds      kpjbu
073100090703     c                   call      'TNTAA1C'
073200090626     c                   parm                    kpjba
073300090626     c                   movel     kpjbu         tntaa1ds
073400130808     c*
073500090626     c* Errore o non abilitato
073600090626    3c                   if        otaa1fabi='N'
073700091028     C                   Eval      mk01m10 = msg(15)
073800091028     c                   Eval      mk01err = 'E'
073900050629     C                   GOTO      Fine
074000981103    4C                   ENDIF
074100981103    3C                   ENDIF
074200981014     C**
074300981009    2C                   END
074400981009    1C                   END
074500050629     C                   ENDSR
074600981012     C*---------------------------------------------------------------
074700981012     C* CARICO DATI DEL POTENZIALE
074800981012     C*---------------------------------------------------------------
074900981012     C     CARICA        BEGSR
075000080206     c                   eval      wforzacdf = *off
075100080206     c                   eval      wforzaiva = *off
075200080206     c                   eval      wforzaiva1 = *off
075300981012      * ANNULLATO
075400080206     C     CPOATB        IFEQ      'A'
075500100202     C                   SETON                                        02
075600981012     C                   END
075700981012      *
075800981012     C                   MOVE      CPORAG        VIDRAG
075900080206     c* SEDE/Filiale (per il momento in sospeso)
076000080623     c                   clear                   vidfsf
076100080623     c                   if        cpofsf='S'
076200080624     c                   eval      vidfsf='SEDE'
076300080623     c                   else
076400080624     c                   eval      vidfsf='FIL '
076500080623     c                   endif
076600130808      *
076700981012     C                   MOVE      CPOVIA        VIDVIA
076800981012     C                   MOVE      CPOCIT        VIDCIT
076900981014     C                   MOVEL     CPOCAP        VIDCAP
077000981012     C                   MOVE      CPOPRV        VIDPRV
077100060714     c                   eval      savprv = cpoprv
077200981012     C                   MOVE      CPONAZ        VIDNAZ
077300080206     C                   MOVE      CPONAZ        savnaz
077400140714     C                   MOVE      CPO1TEL       VIDTEL
077500140714     C                   MOVE      CPO1FAX       VIDFAX
077600080205     c* Codice fiscale
077700080205     c                   eval      vidcdf=cpocdf
077800080207     c                   eval      savcdf=cpocdf
077900080205     c* Partita IVA
078000080205     c                   IF        %subst(cpopiv:1:2) < *zeros
078100080205     C                   eval      Vidivaeu = %subst(cpopiv:1:2)
078200080205     C                   eval      Vidivait = %subst(cpopiv:3)
078300080205     c                   ELSE
078400080205     c                   movel     cpopiv        vidivait
078500080205     c                   ENDIF
078600080207     c                   eval      saviva=vidiva
078700080205      * CATEGORIA merceologica
078800981012     C                   MOVEL     CPOSCT        VIDSCT
078900981012     C                   MOVEL(P)  CPOSCT        KEY
079000981012     C                   EXSR      CTRSCT
079100981012     C                   MOVEL     WDES          DESSCT
079200981012      *
079300981012      * codice commerciale
079400981012     C     CPOCMM        IFNE      *ZEROS
079500981012     C                   MOVEL     CPOCMM        VIDCMM
079600981012     C                   EXSR      CTRCMM
079700981012     C                   ENDIF
079800981012      *
079900080222      * FATTURATO
080000080222     c                   clear                   vidfatd
080100091217     c                   eval      vidfatd='Fatt.to Azienda/1000'
080200080222     c                   if        cpoftaz>0
080300091217     c                   eval      vidfatd=%trim(vidfatd)+ ' ' + hi
080400080222     c                             + %trim(%editc(cpoftaz:'2'))
080500080222     c* Anno fatturato oppure stimato/dichiarato
080600080222     c                   select
080700080222     c                   when      cpoafaz>0
080800080222     c                   eval      vidfatd =%trim(vidfatd) + nm
080900080222     c                             +'Anno' + hi + %editc(cpoafaz:'Z')
081000080222     c                   when      cpoffaz='S'
081100080222     c                   eval      vidfatd=%trim(vidfatd) + nm + 'STIMATO'
081200080222     c                   when      cpoffaz='D'
081300080222     c                   eval      vidfatd=%trim(vidfatd) + nm +'DICHIARATO'
081400080222     c                   endsl
081500080222     c                   if        cpopexp>0
081600080226     c                   eval      vidfatd=%trim(vidfatd) + nm +'di cui export'
081700080226     c                             + hi + %editc(cpopexp:'Z') + '%'
081800080222     c                   endif
081900080222     c                   endif
082000130808     c*
082100090518     c* Ds del campo CPORST
082200090518     c                   eval      dcpo01=cporst
082300140910
082400090518     c* Spesa trasporti reale/presunta
082500090520     C                   CLEAR                   VIDDSPT
082600090518     c                   eval      ktpf='SPT'
082700090518     c     kcpi          chain     ticpi01l
082800090521     c                   select
082900090521     c                   when      %found(ticpi01l)
083000091217     c                   eval      viddspt='Spesa Trasporti      ' +
083100090520     c                                      hi + %trim(%editc(cpipft:'2')) +
083200090520     c                                     nm +'REALE   '
083300090521     c                   when      §cposptp>*zeros
083400091217     c                   eval      viddspt='Spesa Trasporti      ' +
083500090520     c                                      hi + %trim(%editc(§cposptp:'2')) +
083600090520     c                                     nm +'PRESUNTO'
083700090521     C                   ENDsl
083800140910
083900140910      /free
084000140910       //?Ds campo CPOTEL
084100140910         dcpo02 = CPOtel;
084200140910
084300140910       //?Anno Inizio Attività
084400140910         IF  §CPOannoia <> *blanks and §CPOannoia <> *zeros;
084500140910           vidannoia = §CPOannoia;
084600140910         ELSE;
084700140910           clear vidannoia;
084800140910         ENDIF;
084900140910      /end-free
085000130808      *
085100100319      * codice importanza cliente
085200100319     c                   eval      vidcic = cpoftr
085300090518     c
085400120703      * Data primo contatto da CPORST
085500120703     c                   IF        §CPOdtpcon > *zeros
085600120703     c                   eval      g08inv = %dec(§CPOdtpcon:8:0)
085700120703     c                   clear                   g08dat
085800120703     c                   eval      g08err = '3'
085900120703     c                   call      'XSRDA8'
086000120703     c                   parm                    wlbda8
086100120703     c                   eval      VIDdpc = g08dat
086200120703     c                   ELSE
086300120703     c                   clear                   VIDdpc
086400120703     c                   ENDIF
086500981012      *
086600981012      * DATA ACQUISIZIONE
086700981022     C                   Z-ADD     CPODAQ        G08INV
086800981022     C                   MOVE      *ZEROS        G08DAT
086900981022     C                   MOVEL     3             G08ERR
087000981022     C                   CALL      'XSRDA8'
087100981022     C                   PARM                    WLBDA8
087200981022     C                   Z-ADD     G08DAT        VIDDAC
087300981012     C**
087400981012     C                   MOVE      CPOFLT        VIDFLT
087500981014     C     CPOFLT        CHAIN     AZORG01L                           30
087600981014     C  N30              MOVEL     ORGDES        DESFLT
087700981127     C                   MOVEL     CPOFLT        OLDFLT
087800981120     C**
087900981120     C* VEDO SE CI SONO INFO COMMERCIALI
088000091029     C     cpocpo        CHAIN     TICPI01L                           31
088100981120    3C     *IN31         DOWEQ     *OFF
088200981120     C     CPIATB        ANDNE     ' '
088300091029     C     cpocpo        READE     TICPI01L                               31
088400981120    3C                   ENDDO
088500981120     C* 31 ON - NON CI SONO
088600981120     C     *IN31         IFEQ      *ON
088700091028     C                   SETOff                                       11
088800981120     C                   ELSE
088900091028     C                   SETOn                                        11
089000981120     C                   ENDIF
089100110203      * Decodifico la categoria del potenziale
089200110203     c                   clear                   dcpo
089300110203     c                   clear                   tibs02ds
089400110203     c                   eval      t02mod = 'C'
089500110203     c                   eval      t02cod = 'CPO'
089600110203     c                   eval      t02ke1 = CPOfls
089700110203     c                   eval      t02sif = knsif
089800110203     c                   call      'TIBS02R'
089900110203     c                   parm                    kpjba
090000110203     c                   parm                    tibs02ds
090100110203     c                   if        t02err = *blanks
090200110203     c                   eval      dcpo = t02uni
090300110203     c                   endif
090400110203      * imposto la descrizione
090500110503     c                   eval      viddfls = §cpodesc
090600110203     c                   eval      *in12 = (CPOfls <> 'M')
090700140910
090800140910      /free
090900140910       //?Carico la nota DP dettaglio prodotti
091000140910         flag02 = 'P';
091100140910         clipot = %editc(CPOcpo:'X');
091200140910         ktnt = 'DP';
091300140910         chain (flag02:clipot:knk2:ktnt) TFNTC01L;
091400140910         IF  %found(TFNTC01L);
091500140910           viddetp = ntcrnt;
091600140910         ELSE;
091700140910           clear viddetp;
091800140910         ENDIF;
091900140910      /end-free
092000140910
092100981216     C**
092200981012     C                   ENDSR
092300941111     C*------------------------------------------------------------------------*
092400941125     C*  CHKIND - CONTROLLO INDIRIZZO COMPLETO
092500941111     C*------------------------------------------------------------------------*
092600941125     C     CHKIND        BEGSR
092700941111     C*
092800941111     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
092900970711     C                   MOVEL     VIDRAG        I14RSC
093000970711     C                   MOVE      *BLANKS       I14RS2
093100970711     C                   MOVEL     VIDVIA        I14IND
093200970711     C                   MOVEL     VIDCIT        E14LOC
093300970711     C                   CLEAR                   E14PIP
093400970711     C                   MOVEL     VIDPRV        E14PRV
093500981012     C                   MOVEL     VIDNAZ        E14NAR
093600970711     C                   MOVEL     VIDCAP        E14CAP
093700970711     C*
093800970711     C                   CLEAR                   DSLV13
093900970711     C                   CLEAR                   DSSI95
094000970711     C                   CLEAR                   I14ISO
094100970711     C                   MOVEL     DATEU         I14DRI
094200970711     C*
094300970711     C                   CALL      'FNLV14R'
094400970711     C                   PARM                    KPJBA
094500970711     C                   PARM                    DSLV14
094600970711     C*
094700970711     C                   MOVEL     O14ERR        WERR              1
094800970711     C                   MOVEL     O14MSG        VIDMSG
094900970711     C                   EXSR      CTRERR
095000981012     C** ERRORE
095100981012     C     *IN90         IFEQ      *ON
095200970711     C                   GOTO      ENDC00
095300970711     C                   ENDIF
095400970711     C*************
095500970711     C* CONTROLLO CAP
095600970711     C                   MOVEL     '7'           I95TCN
095700970711     C                   MOVEL     'S'           I13LA3
095800970711     C                   MOVEL     'S'           I13SZ3
095900970711     C                   MOVEL     E14CAP        I95CAP
096000970711     C                   MOVEL     E14LOC        I95LOC
096100970711     C                   MOVEL     E14PRV        I95PRV
096200970711     C                   MOVEL     E14NAR        I95NAR
096300970711     C                   MOVEL     DATEU         I95DAT
096400970711     C                   MOVEL     'S'           I13AF0
096500970711     C                   MOVEL     'S'           I13CNV
096600970711     C                   MOVEL     'S'           I13AF1
096700970711     C**  SE CAMBIATI DATI CONTROLLO SE CONGRUENTI: ERRORE FORZABILE
096800970711    2C     E14CAP        IFNE      VARCAP
096900970711     C     E14LOC        ORNE      VARLOC
097000970711     C     E14PRV        ORNE      VARPRV
097100970711     C     E14NAR        ORNE      VARNAR
097200970711     C                   CLEAR                   WZ3
097300970711     C                   MOVEL     E14CAP        VARCAP
097400970711     C                   MOVEL     E14LOC        VARLOC
097500970711     C                   MOVEL     E14PRV        VARPRV
097600970711     C                   MOVEL     E14NAR        VARNAR
097700970711    2C                   ENDIF
097800970711     C                   MOVEL     WZ3           E13FZ3
097900970711     C**
098000970711     C** CALL
098100970711     C                   CALL      'FNLV13R'
098200970711     C                   PARM                    KPJBA
098300970711     C                   PARM                    DSLV13
098400970711     C                   PARM                    DSSI95
098500970711     C*
098600970711     C                   MOVEL     O13ERR        WERR              1
098700970711     C                   MOVEL     O13MSG        VIDMSG
098800970711     C                   EXSR      CTRERR
098900970711     C**
099000970711     C                   MOVEL     E13FZ3        WZ3
099100020826      **
099200981012     C** ERRORE
099300981012     C     *IN90         IFEQ      *ON
099400970711     C                   GOTO      ENDC00
099500970711     C                   ENDIF
099600970711     C*
099700020906      **
099800041207     C     dutPOU        CHAIN     AZORG01L                           30
099900020906     C   30              CLEAR                   ORGDIT
100000970711     C* SE LA FIL.APP.  E' VUOTA LA PROPONGO E RIEMETTO IL VIDEO
100100970711     C     VIDFLT        IFEQ      *BLANKS
100200970711     C     VIDFLT        OREQ      *ZEROS
100300090626     C* SE SONO una messaggerie, propongo la seconda linea distribuzione
100400090626     C     dutntw        IFEQ      'MES'
100500981103     C     SIMFEL        ANDNE     46
100600981103     C     O95CLO        ANDGT     0
100700981103     C                   MOVEL     O95CLO        VIDFLT
100800981103     C                   ELSE
100900981103     C                   MOVEL     O95LNA        VIDFLT
101000981103     C                   ENDIF
101100981117     C                   MOVEL     VIDFLT        NUMFLT
101200981103     C**
101300981117     C     NUMFLT        CHAIN     AZORG01L                           30
101400981014     C  N30              MOVEL     ORGDES        DESFLT
101500981012     C                   SETON                                        90
101600970711     C                   ENDIF
101700941125     C*
101800970711     C     ENDC00        TAG
101900970711     C* IMPOSTO I CAMPI DI UNA EVENTUALE RICERCA
102000970711     C                   MOVEL     E14LOC        VIDCIT
102100970711     C                   MOVEL     E14CAP        VIDCAP
102200970711     C                   MOVEL     E14PRV        VIDPRV
102300970711     C                   ENDSR
102400970711     C**************************************************************************
102500981012     C* CONTROLLO SE C'E' ERRORE
102600970711     C**************************************************************************
102700970711     C     CTRERR        BEGSR
102800970711     C* IMPOSTO CAMPI PASSATI
102900970711    1C     O13RON        IFEQ      'S'
103000970711     C                   MOVEL     O95NAR        E14NAR
103100970711    1C                   ENDIF
103200970711    1C     O13ROC        IFEQ      'S'
103300970711     C                   MOVEL     O95CAP        E14CAP
103400970711    1C                   ENDIF
103500970711    1C     O13ROP        IFEQ      'S'
103600970711     C                   MOVEL     O95PRV        E14PRV
103700970711    1C                   ENDIF
103800970711    1C     O13ROL        IFEQ      'S'
103900970711     C                   MOVEL     O95LOC        E14LOC
104000970711    1C                   ENDIF
104100970711     C* SE RITORNA CODICE DI ERRORE LO EMETTO A VIDEO
104200970711    1C     WERR          IFNE      ' '
104300970711    2C     VIDMSG        IFNE      *BLANKS
104400981012     C                   SETON                                        28
104500970711    2C                   END
104600970711     C* POSIZIONAMENTO CURSORE
104700970711    2C                   SELECT
104800970711     C     WERR          WHENEQ    '1'
104900981012     C                   SETON                                        40
105000970711     C     WERR          WHENEQ    '2'
105100981012     C                   SETON                                        41
105200970711     C     WERR          WHENEQ    '3'
105300981012     C                   SETON                                        42
105400970711     C     WERR          WHENEQ    '4'
105500981012     C                   SETON                                        43
105600970711     C     WERR          WHENEQ    '5'
105700981012     C                   SETON                                        44
105800981012     C     WERR          WHENEQ    '6'
105900981012     C                   SETON                                        45
106000970711    2C                   ENDSL
106100970711     C*
106200981012     C                   SETON                                        90
106300970711    1C                   ENDIF
106400970711     C                   ENDSR
106500981012     C*---------------------------------------------------------------
106600981012     C* CONTROLLO E DECODIFICO CODICE COMMERICIALE
106700981012     C*---------------------------------------------------------------
106800981012     C     CTRCMM        BEGSR
106900981012     C* RICERCA
107000101202     c                   if        %scan('?':vidcmm) > 0
107100010620     C                   MOVE      *BLANKS       VIDELE
107200130808     c                   clear                   TRMK43ds
107300130808     C                   CALL      'TRMK43R'
107400010620     C                   PARM                    KPJBA
107500130808     C                   PARM                    TRMK43ds
107600130808     c                   if        oMK43err = *off  and  oMK43fxx = *blank
107700130808     C                   MOVEL     oMK43cmm      VIDELE           10
107800130808     C                   MOVEL     oMK43des      DESELE           30
107900130808     c                   endif
108000101202     c                   eval      vidcmm = VIDELE
108100101202     c                   eval      descmm = DESELE
108200101202     c                   endif
108300981012     C* CONTROLLI
108400130808     c                   If        %check(c_Digits:VIDcmm) > *zeros
108500130808     c                   seton                                        289050
108600130808     c                   movel     Msg(20)       VIDmsg
108700130808     c                   leavesr
108800130808     c                   EndIf
108900101202     c                   if        vidcmm > *zeros
109000130808     c                   move      vidcmm        CMMcod
109100130808     C     CMMcod        CHAIN     AZCMM000
109200130808     c                   IF        %found(AZCMM01L)
109300130808     c                   eval      descmm = CMMdes
109400981012     C                   ELSE
109500981012     C                   SETON                                        289050
109600981012     C                   MOVEL     MSG(20)       VIDMSG
109700981012     C                   ENDIF
109800981012     C                   ENDIF
109900130808      *
110000981012     C                   ENDSR
110100981012      *-------------------------------------------------------------------------
110200080205      *CONTROLLO CATEGORIA merceologica
110300981012      *-------------------------------------------------------------------------
110400981012     C     CTRSCT        BEGSR
110500981012     C*
110600120703     C                   CLEAR                   WDES             40
110700981012     C                   MOVE      '1L'          COD
110800981012     C     '?'           SCAN      KEY                                    90
110900981012     C* RICERCA
111000981012     C     *IN90         IFEQ      '1'
111100981012     C                   EXSR      CERTAB
111200981012     C                   MOVEL(P)  VIDELE        KEY
111300981012     C                   MOVEL     DESELE        WDES
111400981014     C                   SETON                                        48
111500981012     C                   ELSE
111600981012     C* CONTROLLI
111700080207     c                   clear                   ds1l
111800091029     C     KTAB          CHAIN     TABEL                              30
111900981014     C     *IN30         IFEQ      *OFF
112000981026     C     TBLFLG        ANDEQ     ' '
112100080207     c                   movel     tbluni        ds1l
112200080207     C                   MOVEL     §1ldes        WDES
112300080207     c                   if        §1luti='N'
112400080207     C                   SETON                                        289048
112500080207     C                   MOVEL     MSG(25)       VIDMSG
112600080207     c                   endif
112700981012     C                   ELSE
112800981012     C                   SETON                                        289048
112900981012     C                   MOVEL     MSG(25)       VIDMSG
113000981012     C                   ENDIF
113100981012     C                   ENDIF
113200981012     C                   ENDSR
113300920416      *-------------------------------------------------------------------------
113400920416      *PULIZIA CAMPI FILE VIDEO
113500940317      *-------------------------------------------------------------------------
113600920416     C     AZZERO        BEGSR
113700970711     C                   MOVE      0             WZ3               1 0
113800920416     C                   Z-ADD     *ZEROS        VIDCP1
113900920416     C                   Z-ADD     *ZEROS        VIDCP2
114000920416     C                   MOVE      *BLANKS       VIDRAG
114100920416     C                   MOVE      *BLANKS       VIDVIA
114200920416     C                   MOVE      *BLANKS       VIDCIT
114300981014     C                   MOVEL     *BLANKS       VIDCAP
114400920416     C                   MOVE      *BLANKS       VIDPRV
114500981009     C                   MOVE      *BLANKS       VIDNAZ
114600920416     C                   MOVE      *BLANKS       VIDTEL
114700981009     C                   MOVE      *BLANKS       VIDFAX
114800920416     C                   MOVE      *BLANKS       VIDSCT
114900920416     C                   MOVE      *BLANKS       DESSCT
115000920416     C                   MOVE      *BLANKS       VIDCMM
115100920416     C                   MOVE      *BLANKS       DESCMM
115200920427     C                   MOVE      *BLANKS       VIDFLT
115300920421     C                   MOVE      *BLANKS       DESFLT
115400940321     C                   CLEAR                   UNO
115500940321     C                   CLEAR                   SAVFLT
115600981117     C                   CLEAR                   WDIVAS
115700050518     c                   Clear                   vidrsp1
115800081010     c                   eval      vidfsf='SEDE'
115900140910     c                   clear                   viddetp
116000140910     c                   clear                   vidannoia
116100140910
116200080205     c                   ENDSR
116300981012     C*---------------------------------------------------------------
116400981012     C* CONTROLLO 2 VIDEATA
116500981012     C*---------------------------------------------------------------
116600981012     C     CTRD02        BEGSR
116700981124     C** CONTROLLO SOLO IN MANUTENZIONE
116800981124    0C     *IN02         IFEQ      *OFF
116900981012     C**
117000981012     C** INDIRIZZO
117100981012     C**
117200981012     C* CONTROLLO INDIRIZZO COMPLETO
117300981012     C                   EXSR      CHKIND
117400981012     C   90              GOTO      ENDCT2
117500981012     C* TELEFONO
117600100524      * non controllo se F22=Richiesta contatto
117700981012     C     VIDTEL        IFEQ      *BLANKS
117800100524     c     *inkw         andeq     '0'
117900981012     C                   SETON                                        289046
118000981012     C                   MOVEL     MSG(24)       VIDMSG
118100981012     C                   GOTO      ENDCT2
118200981012     C                   ENDIF
118300981012     C**
118400100524     c                   if        not *inkw
118500981012     C                   CLEAR                   TRUL42
118600981014     C                   MOVEL     VIDTEL        D42FAX
118700981012     C                   CALL      'TRUL42R'
118800981012     C                   PARM                    TRUL42
118900981012     C     D42ERR        IFEQ      '1'
119000981012     C                   SETON                                        289046
119100981012     C                   MOVEL     D42MSG        VIDMSG
119200981012     C                   GOTO      ENDCT2
119300981012     C                   END
119400100407     c                   endif
119500981012     C** FAX SOLO SE C'E'
119600981012     C     VIDFAX        IFNE      *BLANKS
119700981012     C                   CLEAR                   TRUL42
119800981012     C                   MOVEL     VIDFAX        D42FAX
119900981012     C                   CALL      'TRUL42R'
120000981012     C                   PARM                    TRUL42
120100981012     C     D42ERR        IFEQ      '1'
120200981012     C                   SETON                                        289047
120300981012     C                   MOVEL     D42MSG        VIDMSG
120400981012     C                   GOTO      ENDCT2
120500981012     C                   END
120600981012     C                   END
120700080623     c**
120800100922     c** Falg sede filiale
120900080623     c**
121000080623     c* controllo solo se cambiato
121100080623     c                   if        %subst(vidfsf:1:1)<>cpofsf
121200080623     c* Se presente codice duns o dati azienda, non modificabile
121300081105     c                   if        cpoduns<>*blanks or cpoftaz>0
121400100922     c* l'utente deve essere autorizzato alla modifica
121500100922     c**                 if        simfel<>46 or (knmus<>'SEG019 ' and
121600100922     c**                           knmus<>'SEG013 ' and %subst(knmus:1:3)<>
121700100922     c**                           'EDP')
121800100922     c*
121900100922     c                   if        §utepotmsf<>'S' and  %subst(knmus:1:3)<>'EDP'
122000080623     C                   SETON                                        672890
122100080623     c                   if        cpofsf='S'
122200080623     C                   MOVEL     MSG(38)       VIDMSG
122300080623     c                   else
122400080623     C                   MOVEL     MSG(39)       VIDMSG
122500080623     c                   endif
122600080623     C                   GOTO      ENDCT2
122700080623    3C                   ENDIF
122800080623     c                   endif
122900081105     c                   endif
123000130808     c*
123100080205     C** CODICE FISCALE
123200080206     c* reimposto campi per forzatura e partita aiva in base allo stato
123300080206     c                   if        vidnaz<>savnaz
123400080206     c                   eval      wforzacdf = *off
123500080206     c                   eval      wforzaiva1 = *off
123600080206     c                   eval      savnaz=vidnaz
123700080206     c                   endif
123800080205     c                   exsr      sr_ctrcdf
123900080205     C   28              goto      endct2
124000981012     C**
124100080205     C** Partita Iva
124200080205     c                   exsr      sr_Ctriva
124300080205     C   28              goto      endct2
124400130808      *
124500981117     C** P.O. DI APPARTENENZA
124600981012     C**
124700981012      *RICERCA
124800981014     C     '?'           SCAN      VIDFLT                                 90
124900981014    1C     *IN90         IFEQ      '1'
125000981012     C                   EXSR      SELFIL
125100981014     C                   SETON                                            49
125200981012     C                   GOTO      ENDCT2
125300981012    1C                   ENDIF
125400981012     C* CONTROLLO
125500981012     C                   MOVE      VIDFLT        NUMFLT
125600981014     C     NUMFLT        CHAIN     AZORG01L                           30
125700981012     C* ERRATA
125800981014    1C     *IN30         IFEQ      *ON
125900981012     C     ORGFVA        ORNE      ' '
126000981012     C                   MOVEL     MSG(26)       VIDMSG
126100981012     C                   SETON                                        492890
126200981012     C                   GOTO      ENDCT2
126300981012    1C                   ENDIF
126400981014     C                   MOVEL     ORGDES        DESFLT
126500981012     C**
126600981027     C* P.O. ELABORATORE FIL TRASMISSIONE
126700981117    1C     SAVFLT        IFNE      NUMFLT
126800981117     C                   MOVEL     ' '           UNO               1
126900981117     C                   CLEAR                   WDIVAS
127000981117     C                   MOVEL     NUMFLT        SAVFLT
127100981117    1C                   ENDIF
127200981012     C*
127300090626      *Controllo validità  in base alle abilitazioni utente
127400090626     c                   clear                   tntaa1ds
127500090626     c                   eval      itaa1fil =numflt
127600090626     c                   eval      itaa1caut='§UTEPOT'
127700090626     c                   clear                   kpjbu
127800090626     c                   movel     tntaa1ds      kpjbu
127900090703     c                   call      'TNTAA1C'
128000090626     c                   parm                    kpjba
128100090626     c                   movel     kpjbu         tntaa1ds
128200090626     c
128300090626     c* Se non abilitato errore forzabile
128400090626     c                   if        otaa1fabi='N'
128500090626     c                             and  wdivas=' '
128600090626     C                   MOVEL     MSG(27)       VIDMSG
128700090626     C                   SETON                                        492890
128800090626     C                   MOVEL     'S'           WDIVAS            1
128900090626     C                   GOTO      ENDCT2
129000090626    1C                   ENDIF
129100090626     c
129200090626     c
129300981103     C* INCONGRUENZA COL CAP--> LNA STD O LNA OLTRE
129400981012    1C     NUMFLT        IFNE      O95LNA
129500981103    1C     NUMFLT        ANDNE     O95CLO
129600981012     C     UNO           ANDNE     '1'
129700981012     C                   SETON                                        492890
129800981012     C                   MOVEL     '1'           UNO
129900981012     C                   MOVEL     MSG(28)       VIDMSG
130000981012     C                   GOTO      ENDCT2
130100981012    1C                   ENDIF
130200981012     C*
130300981014     C                   MOVEL     ORGDES        DESFLT
130400981012     C**
130500080205     C** CATEGORIA merceologica
130600981012     C**
130700981012     C                   MOVEL(P)  VIDSCT        KEY
130800981012     C                   EXSR      CTRSCT
130900981012     C                   MOVEL     KEY           VIDSCT
131000981012     C                   MOVEL     WDES          DESSCT
131100981012     C   90              GOTO      ENDCT2
131200981012     C**
131300981012     C** CODICE COMMERCIALE
131400981012     C**
131500981026     C** OBBLIGATORIO PER STATO > 0 MA NON CALCOLO PIU' IN AUTOMATICO
131600981012     C** LEGGO L'ULTIMO STATO IMPOSTATO: SE 0 NON OBBLIGATORIO
131700981026     C**  SE IMMESSO A VIDEO L'ULTIMO STATO E' QUESTO
131800981012    1C     VIDCMM        IFEQ      *ZEROS
131900981012     C     VIDCMM        OREQ      *BLANKS
132000981027     C**
132100981027     C* SE NEL FILE C'ERA--> NON E' PIU' POSSIBILE CANCELLARLO
132200981104     C  N10CPOCMM        IFNE      *ZEROS
132300981027     C                   SETON                                        502890
132400981027     C                   MOVEL     MSG(29)       VIDMSG
132500981027     C                   GOTO      ENDCT2
132600981027    2C                   ENDIF
132700981111     C**
132800981111   X1C                   ELSE
132900981012     C**
133000981012     C                   EXSR      CTRCMM
133100981012     C   90              GOTO      ENDCT2
133200981027     C**
133300981027     C                   MOVEL     VIDCMM        W0030             3 0
133400981027     C     W0030         CHAIN     AZORG01L                           30
133500021029     C   30              MOVEL     W0030         ORGFEL
133600981027     C   30              CLEAR                   ORGDIT
133700981012     C**
133800130808      * NON può avere il flag di particolarità inserito in tab 01
133900130808     c                   if        CMMpar <> *blanks
134000981012     C                   SETON                                        502890
134100981012     C                   MOVEL     MSG(30)       VIDMSG
134200981012     C                   GOTO      ENDCT2
134300981027    2C                   ENDIF
134400111123
134500111123      /free
134600111123       //?Commerciale "valido" data inizio e fine attività
134700130808         IF  CMMdtIni > DateU or CMMdtFin < DateU;
134800111123           *in28 = *on;
134900111123           *in50 = *on;
135000111123           *in90 = *on;
135100111123           Vidmsg = msg(30);
135200111123           leavesr;
135300111123         ENDIF;
135400111123      /end-free
135500981112     C**
135600090626     c* se Diversi e l'utente è abilitato alla filiale  appartenenza
135700090626     c*              l'utente deve essere abilitato anche alla fil.mmerciale
135800130808     c*
135900981117    2C     W0030         IFNE      NUMFLT
136000130808     c*
136100981117    3C     WDIVAS        IFEQ      ' '
136200090626     c*
136300090626     c* Visto che qui non devo controllare l'unificante, passo la filiale
136400090626     c*  del commerciale per il controllo delle abilitazioni
136500090626     c                   clear                   tntaa1ds
136600090626     c                   eval      itaa1fil =w0030
136700090626     c                   eval      itaa1caut='§UTEPOT'
136800090626     c                   clear                   kpjbu
136900090626     c                   movel     tntaa1ds      kpjbu
137000090703     c                   call      'TNTAA1C'
137100090626     c                   parm                    kpjba
137200090626     c                   movel     kpjbu         tntaa1ds
137300130808     c*
137400090626    4c                   if        otaa1fabi='N'
137500981012     C                   SETON                                        502890
137600981012     C                   MOVEL     MSG(31)       VIDMSG
137700981012     C                   GOTO      ENDCT2
137800090626    4C                   ENDIF
137900090626    3C                   ENDIF
138000981117     C*
138100090626     C* SE l'utente non è abilitato alla filiale, devono essere per forza uguali
138200981117    3C     WDIVAS        IFEQ      'S'
138300981117     C                   SETON                                        502890
138400981117     C                   MOVEL     MSG(14)       VIDMSG
138500981117     C                   GOTO      ENDCT2
138600981117    3C                   ENDIF
138700981117    2C                   ENDIF
138800130808     c*
138900981012    1C                   ENDIF
139000091028    0C                   ENDIF
139100981012     C*
139200981012     C     ENDCT2        ENDSR
139300060330
139400920427     C***------------------------------------------------------------
139500920427     C** CARICAMENTO SCHIERA FIL
139600920427     C***------------------------------------------------------------
139700920427     C     CARTAB        BEGSR
139800940318     C*
139900970711     C                   TIME                    W0140            14 0
140000970711     C* UDATE IN GGMMAAAA
140100970711     C                   MOVE      W0140         WDTGIO            8 0
140200970711     C*
140300970711     C* UDATE IN AAAAMMGG
140400970714     C                   Z-ADD     WDTGIO        G08DAT
140500970714     C                   MOVEL     *BLANK        G08ERR
140600970711     C                   CALL      'XSRDA8'
140700970714     C                   PARM                    WLBDA8
140800970711     C                   MOVEL     G08INV        DATEU             8 0
140900050518     c                   Move      g08inv        data6             6 0
141000971203     C**
141100981103     C                   MOVEL     G08DAT        GMADAT            8 0
141200981012     C                   SETOFF                                       212223
141300981014     C                   SETOFF                                       242526
141400981012     C                   SETOFF                                       27
141500920430     C*
141600940321     C                   ENDSR
141700920427      *________________________________________________________________________
141800920427      *SELEZIONE FILIALE DI APPARTENENZA TRAMITE '?'
141900940317      *________________________________________________________________________
142000920427     C     SELFIL        BEGSR
142100941125     C                   CALL      'TNSD19R'
142200920427     C                   PARM                    §CODF             3
142300920427     C                   PARM                    §TIP              1
142400920427     C                   PARM                    §§DES            25
142500920427     C                   MOVEL     §CODF         VIDFLT
142600920427     C                   MOVEL     §§DES         DESFLT
142700920427     C                   ENDSR
142800940321      *-------------------------------------------------------------------------
142900960605      *AGGIORNO FILES
143000940321      *-------------------------------------------------------------------------
143100981014     C     AGGIOR        BEGSR
143200940321      *
143300940321     C                   MOVEL     VIDCP1        CPOCPO
143400940321     C                   MOVE      VIDCP2        CPOCPO
143500140714     c                   eval      CPO1cpo = CPOcpo
143600940321     C                   MOVE      VIDRAG        CPORAG
143700940321     C                   MOVE      VIDVIA        CPOVIA
143800940321     C                   MOVE      VIDCIT        CPOCIT
143900981014     C                   MOVEL     VIDCAP        CPOCAP
144000940321     C                   MOVE      VIDPRV        CPOPRV
144100981014     C                   MOVE      VIDNAZ        CPONAZ
144200140714     C                   MOVE      VIDTEL        CPO1TEL
144300140714     C                   MOVE      VIDFAX        CPO1FAX
144400080205     c                   eval      cpocdf=vidcdf
144500080205     c                   if        vidiva<>*blanks
144600080205     c                   if        vidivaeu=*blanks
144700080206     c                   movel     vidivait      cpopiv
144800080205     c                   else
144900080206     c                   movel     vidiva        cpopiv
145000080205     c                   endif
145100080205     c                   else
145200080206     c                   clear                   cpopiv
145300080205     c                   endif
145400080623     C                   MOVEL     VIDfsf        CPOfsf
145500080623     C                   MOVEL     VIDSCT        CPOSCT
145600940321     C                   MOVEL     VIDCMM        CPOCMM
145700100319     c                   eval      cpoftr = vidcic
145800981014     C* IMMISSIONE PRENDO  UDATE=DATA ACQUISIZIONE CLIENTE
145900981014££££ C   10              Z-ADD     DATEU         CPODAQ
146000940321     C                   MOVE      VIDFLT        CPOFLT
146100040805     c                   Eval      CpoDtr = dateu
146200010504      *
146300060714      * se non sono in annullamento devo reperire la nuova provincia dalla tabella PR e
146400060714      * memorizzarla sul file
146500080414     c***                if        not *inkq and savprv <> cpoprv
146600080414     c                   if        savprv <> cpoprv
146700060714     c                   clear                   dspr
146800060714     c                   eval      cod = 'PR'
146900060714     c                   eval      key = cpoprv
147000060714     c     ktab          chain     tabel00f
147100060714     c                   if        %found(tabel00f) and tblflg = *blanks
147200060714     c                   eval      dspr = tbluni
147300060714     c                   endif
147400060714     c                   if        §prnew <> *blanks
147500060714     c                   eval      cpoprv = §prnew
147600060714     c                   endif
147700060714     c                   endif
147800010301      * Aggiorno archivio potenziali
147900981127    1C     *IN10         IFEQ      *ON
148000080206     c                   clear                   CPOFTAZ
148100080206     c                   clear                   CPOAFAZ
148200080206     c                   clear                   CPOFFAZ
148300080206     c                   clear                   CPOPEXP
148400140930     c                   clear                   dcpo01
148500140930     c                   clear                   dcpo02
148600140930     c                   eval      cporst = dcpo01
148700110316     c                   eval      cpofls = 'M'
148800140930     c                   eval      §CPOannoia = *all'0'
148900140930     c                   eval      CPOtel = dcpo02
149000981014     C                   WRITE     TNCPO000
149100140714     C                   WRITE     TNCPO100
149200090522     c*
149300140929     c******             movel     cpocpo        parcpo
149400140929     c******             call      'TRMK08R'
149500140929     c******             parm                    kpjba
149600140929     c******             parm                    parcpo           11
149700090522     c
149800940321   X1C                   ELSE
149900981014     C                   UPDATE    TNCPO000
150000140716     C  n13              UPDATE    TNCPO100
150100140716     C   13              write     TNCPO100
150200981127    1C                   ENDIF
150300140910
150400140910      /free
150500140910       //?Scrivo/aggiorno nota "DP"
150600140910           flag02 = 'P';
150700140910           clipot = %editc(CPOcpo:'X');
150800140910           ktnt = 'DP';
150900140910           chain (flag02:clipot:knk2:ktnt) TFNTC01L;
151000140910           IF  viddetp <> *blanks;
151100140910             IF  %found(TFNTC01L);
151200140910               NTCrnt = viddetp;
151300140910               NTCntr = data6;
151400140910               update TFNTC;
151500140910             ENDIF;
151600140910             IF  not %found(TFNTC01L);
151700140910               clear TFNTC;
151800140910               NTCapl = flag02;
151900140910               NTCnk1 = clipot;
152000140910               NTCtnt = ktnt;
152100140910               NTCrnt = viddetp;
152200140910               NTCsns = 'S';
152300140910               NTCntr = data6;
152400140910               write TFNTC;
152500140910             ENDIF;
152600140910           ELSE;
152700140910             IF  %found(TFNTC01L);
152800140910               delete TFNTC;
152900140910             ENDIF;
153000140910           ENDIF;
153100140910      /end-free
153200140910
153300130808      *
153400940321     C                   ENDSR
153500080205      *------------------------------------------------------------------------*
153600080205      * CONTROLLO CODICE FISCALE
153700080207      *------------------------------------------------------------------------*
153800080205     c     Sr_Ctrcdf     BegSr
153900080626     c                   movel     vidfsf        vid1fsf           1
154000130808      *
154100080205      * devo richiamare il nuovo driver fatto per controllare il codice
154200080205      * fiscale e la partita iva
154300080205     c                   clear                   xcfivads
154400080205     c                   eval      xcfivapar = vidcdf
154500080205     c                   eval      xcfivanar = vidnaz
154600080205     c                   eval      xcfivaprv = vidprv
154700080205     c                   call      'XCFIVAR'
154800080205     c                   parm                    xcfivads
154900080422     c                   parm      ' '           forzaita          1
155000080422     c                   parm                    vidiva
155100080422     c
155200080207    2c                   if        xcfivaflg < *zeros
155300080205     c                   seton                                        562890
155400080422     c                   Eval      vidmsg = xcfivamsg
155500080205     c                   Leavesr
155600080207    2c                   EndIf
155700080422     c
155800080422      * Non inserito: Errore forzabile per clienti Italia se non previsto
155900080422      *     inserimento dai controlli xcfivar (xcfivaflg=9)
156000080422    1c                   if        vidcdf=*blanks and vidnaz=*blanks
156100080422     c                             and wforzacdf= *off  and xcfivaflg<>9
156200080422     c                   seton                                        562890
156300080422     c                   eval      vidmsg = msg(50)
156400080422     c                   eval      wforzacdf = *on
156500080422     c                   leavesr
156600080422    1c                   endif
156700080626     c
156800080206     c* lo stesso codice fiscale non deve essere già presente su altro
156900080206     c* potenziale sia come codice fiscale che come partita iva
157000080422    1c                   If        vidcdf <> *Blanks
157100130808      *
157200080626    2c                   if        (*in10 or vidcdf<>savcdf or
157300080626     c                              (vid1fsf<>cpofsf and vid1fsf='S'))
157400080207     c                             and vidcdf<>cod_forzcdf
157500080207     c                   eval      codice=vidcdf
157600080207     c                   exsr      sr_CdfAltroP
157700080207     c                   exsr      Sr_msgcdf
157800080207     c   90              leavesr
157900080626     c
158000080207    3c                   if        (vidivaeu=*blanks and vidcdf<>vidivait)
158100080207     c                             or (vidivaeu<>*blanks and vidcdf<>vidiva)
158200080207     c                   exsr      sr_PivAltroP
158300080207     c                   exsr      Sr_msgcdf
158400080207     c   90              Leavesr
158500080207    3c                   endif
158600080207    2c                   endif
158700080207    1c                   EndIf
158800130808      *
158900080205     c                   EndSr
159000130808      *
159100080205      *------------------------------------------------------------------------*
159200080205      * CONTROLLO PARTITA IVA E STATO
159300080205      *------------------------------------------------------------------------*
159400080205     c     Sr_Ctriva     BegSr
159500080626     c                   movel     vidfsf        vid1fsf           1
159600080205      * PARTITA IVA
159700080205      * Non inserita: Errore forzabile
159800080207    1c                   If        vidiva = *Blanks and wforzaiva=*off
159900080205     c                   seton                                        662890
160000080205     c                   eval      vidmsg = msg(51)
160100080205     c                   eval      wforzaiva = *on
160200080205     c                   leavesr
160300080207    1c                   EndIf
160400080205      * --> controllo
160500080207    1c                   If        vidiva <> *Blanks
160600080205     c                   clear                   xcfivads
160700080205     c                   eval      xcfivamod = 'P'
160800080205     c                   eval      xcfivapar = vidiva
160900080205     c                   eval      xcfivanar = vidnaz
161000080205     c                   eval      xcfivaprv = vidprv
161100080205     c                   call      'XCFIVAR'
161200080205     c                   parm                    xcfivads
161300080205      * --> errata forzabile
161400080207    2c                   If        xcfivaflg = -9 and wforzaiva1 = *off
161500080205     c                   seton                                        662890
161600080205     c                   Eval      vidmsg = xcfivamsg
161700080205     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
161800080205     c                   eval      wforzaiva1 = *on
161900080205     c                   Leavesr
162000080207    2c                   EndIf
162100080205      * --> errore
162200080207    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
162300080205     c                   seton                                        662890
162400080205     c                   eval      vidmsg = xcfivamsg
162500080205     c                   Leavesr
162600080207    2c                   EndIf
162700080205      * se non impostato devo riportare il codice iso della partita iva che
162800080205      * mi viene passato dal pgm di controllo
162900080207    2c                   if        vidivaeu <> ivaeu
163000080205     c                   eval      vidivaeu = ivaeu
163100080205     c                   eval      *in66 = *on
163200080205     c                   eval      *in90 = *on
163300080207    2c                   endif
163400080626     c
163500080207     c* la stessa partita Iva  non deve essere già presente su altro
163600080207     c* potenziale sia come partita iva che come codice fiscale
163700080626    2c                   if        (*in10 or vidiva<>saviva    or
163800080626     c                              (vid1fsf<>cpofsf and vid1fsf='S'))
163900080307     c                             and vidiva<>cod_forziva and vidivaeu<>'$$'
164000080208     c                   if        vidivaeu<>*blanks
164100080207     c                   eval      codice=vidiva
164200080208     c                   else
164300080208     c                   eval      codice=vidivait
164400080208     c                   endif
164500080207     c                   exsr      sr_PivAltroP
164600080207     c                   exsr      sr_msgiva
164700080207     c   90              leavesr
164800130808     c*
164900080207    3c                   if        (vidivaeu=*blanks and vidcdf<>vidivait)
165000080207     c                             or (vidivaeu<>*blanks and vidcdf<>vidiva)
165100080207     c                   exsr      sr_CdfAltroP
165200080207     c                   exsr      sr_msgiva
165300080207     c   90              leavesr
165400080207    3c                   endif
165500080207    2c                   endif
165600080207    1c                   EndIf
165700130808      *
165800080205     c                   EndSr
165900080207      *------------------------------------------------------------------------*
166000080207      * Controllo presenza codice su altro potenziale come cod.fiscale
166100080207      *------------------------------------------------------------------------*
166200080207     c     Sr_CdfAltroP  BegSr
166300080626     c                   clear                   savrag
166400080626     c                   clear                   savfsf
166500080207     c     codice        setll     tncpo05l
166600080207     c     codice        reade     tncpo05l
166700130808     c*
166800080229    3c                   dow       not %eof(tncpo05l)
166900080626     c*                                                                enz.
167000080626     c                   if        c_cpocpo<>cpocpo
167100080626     c                   if        savfsf<>'S'
167200080626     c                   eval      savfsf=c_cpofsf
167300080626     c                   eval      savrag=c_cporag
167400080626     c                   endif
167500080626     c                   endif
167600130808     c*
167700080207     c     codice        reade     tncpo05l
167800080207    3c                   enddo
167900080207     c                   endsr
168000080207      *------------------------------------------------------------------------*
168100080207      * Controllo presenza codice su altro potenziale come piva
168200080207      *------------------------------------------------------------------------*
168300080207     c     Sr_PivAltroP  BegSr
168400080207     c     codice        setll     tncpo06l
168500080207     c     codice        reade     tncpo06l
168600080229    3c                   dow       not %eof(tncpo06l)
168700080626     c*                                                                enz.
168800080626     c                   if        c_cpocpo<>cpocpo
168900080626     c                   if        savfsf<>'S'
169000080626     c                   eval      savfsf=c_cpofsf
169100080626     c                   eval      savrag=c_cporag
169200080626     c                   endif
169300080626     c                   endif
169400130808      *
169500080207     c     codice        reade     tncpo06l
169600080207    3c                   enddo
169700080207     c                   endsr
169800080207      *------------------------------------------------------------------------*
169900080207      * Messaggio di errore codice fiscal già presente per altro potenz
170000080207      *------------------------------------------------------------------------*
170100080207     c     Sr_msgcdf     BegSr
170200080626     c* Errore se trovato un altro potenziale con lo stesso cod.fiscale
170300080626    1c                   if        savrag<>*blanks
170400080626     c*  Se ne ho trovato uno di filiale o il pot aggiornato è di filiale
170500080626     c*                            --> msg forzabile
170600080626    2c                   if        savfsf='F' or vid1fsf='F'
170700080207     c                   eval      cod_forzcdf=codice
170800080207     c                   seton                                        562890
170900080207     c                   Eval      vidmsg = msg(1)
171000080626   x2c                   else
171100080626     c* se ne ho trovato uno di sede per pot di sede
171200080626     c*                            --> msg non forzabile
171300080626     c                   seton                                        562890
171400080626     c                   Eval      vidmsg = msg(16)
171500080626     c                   eval      %subst(vidmsg:19:15)=savrag
171600080626    2c                   endif
171700080626    1c                   endif
171800080207     c                   endsr
171900080207      *------------------------------------------------------------------------*
172000080207      * Messaggio di errore partita iva   già presente per altro potenz
172100080207      *------------------------------------------------------------------------*
172200080207     c     Sr_msgiva     BegSr
172300080626     c* Errore se trovato un altro potenziale con la stessa partita iva
172400080626    1c                   if        savrag<>*blanks
172500080626     c*  Se ne ho trovato uno di filiale o il pot aggiornato è di filiale
172600080626     c*                            --> msg forzabile
172700080626    2c                   if        savfsf='F' or vid1fsf='F'
172800080626     c                   eval      cod_forziva=vidiva
172900080626     c                   seton                                        662890
173000080626     c                   Eval      vidmsg = msg(2)
173100080626   x2c                   else
173200080626     c* se ne ho trovato uno di sede per pot di sede
173300080626     c*                            --> msg non forzabile
173400080626     c                   seton                                        662890
173500080626     c                   Eval      vidmsg = msg(42)
173600080626     c                   eval      %subst(vidmsg:19:15)=savrag
173700080626    2c                   endif
173800080626    1c                   endif
173900080207     c                   endsr
174000050629      *****************************************************************
174100050629      * ROUTINE INIZIALE
174200050629      *****************************************************************
174300050629     C     *INZSR        BEGSR
174400050629      *
174500050629     C     *ENTRY        PLIST
174600050629     C                   PARM                    KPJBA
174700091028     C                   PARM                    trmk01ds
174800100409     C                   PARM                    trmk19ds
174900130808      *
175000100409      * verifico se passati + parametri
175100100409     c                   If        %parms> 2
175200100409     c                   eval      W_daatti = 'S'
175300100409     c                   endif
175400130808      *
175500091028     C                   CLEAR                   mk01F12
175600091028     C                   CLEAR                   mk01F03
175700091028     C                   CLEAR                   mk01F06
175800091028     C                   CLEAR                   mk01ERR
175900091028     C                   CLEAR                   mk01M10
176000050629      *
176100050629     c     *dtaara       define    §azute        azuteds
176200050629     c     *dtaara       define    §datiute      ddatiute
176300050629     c                   in(E)     *dtaara
176400050629     c                   If        %error  or RSUT = *blanks
176500050629     c                   Clear                   Tibs34ds
176600050629     c                   Call      'TIBS34R'
176700050629     c                   Parm                    Tibs34ds
176800050629     c                   In        *dtaara
176900050629     c                   EndIf
177000100922     c
177100100922     c                   Movel     UteFaf        Dute01
177200130808      *
177300050629     C                   SETOFF                                       585957
177400050629      *
177500050629     C     SIMFEL        COMP      *ZERO                              8989
177600050629     C     SIMFEL        IFEQ      0
177700050629     C                   Z-ADD     046           SIMFEL
177800091029     C                   Endif
177900050629     C**
178000050629     C* CARICO TABELLE Ed ESEGUO OPERAZIONI INIZIALI
178100050629     C                   EXSR      CARTAB
178200050629     C                   MOVE      TES(1)        VIDTES
178300130808      *
178400050629     C                   EXSR      AZZERO
178500100202     C                   SETOFF                                       100302
178600140716     c                   setoff                                       13
178700130808      *
178800100202      * se primo campo kpjbu = 'S' è da menù
178900100202     c                   if        %subst(kpjbu:1:1) = 'S'
179000100202     c                   eval      *in03 = *on
179100100202     c                   endif
179200050629     C*
179300131126      * se secondo mpo kpjbu = 'S' è da menù
179400131126     c                   if        %subst(kpjbu:2:3) = 'LOG'
179500131126     c                   eval      $logistica = *on
179600131126     c                   endif
179700131126     C*
179800050629     C* SE C'E' IMPOSTO POTENZIALE PASSATO
179900091028     C     mk01K10       IFNE      ' '
180000091028     C     mk01K10       ANDNE     'I'
180100050629     C*
180200050629     C* 59 ON  - INIBISCO TASTI F7-VISITE / F16-ANNULLA / F12-RITORNO
180300091028     C     mk01K10       IFEQ      'A'
180400050629     C                   SETON                                        59
180500050629     C                   ENDIF
180600050629     C* IN INTERROGAZIONE SEMPRE CAMPI PROTETTI
180700091028     C     mk01K10       IFEQ      'N'
180800050629     C                   MOVE      TES(2)        VIDTES
180900091028     C                   SETON                                        02
181000050629     C                   ENDIF
181100050629     C*
181200091028     C                   MOVEL     mk01CPO       VIDCP1
181300091028     C                   MOVE      mk01CPO       VIDCP2
181400050629     C                   ENDIF
181500050629      *
181600050629     C* PARFLG  = "I" --> PROPONGO L'IMMISSIONE DEL POTENZIALE
181700091028     C     mk01K10       IFEQ      'I'
181800100120     C                   MOVEL     *ON           Immissione
181900050629     C                   ENDIF
182000130808      *
182100050629     C* DEFINIZIONE CHIAVI E CAMPI-------------------------------------*
182200050629     C     KTAB          KLIST
182300050629     C                   KFLD                    CODUT
182400050629     C                   KFLD                    COD               2
182500050629     C                   KFLD                    KEY               8
182600050629      *
182700050629     C     KTAB2         KLIST
182800050629     C                   KFLD                    CODUT
182900050629     C                   KFLD                    COD
183000050629      *
183100050629     C     KNOTE         KLIST
183200091028     C                   KFLD                    FLAG02            1
183300091028     C                   KFLD                    CLIPOT           11
183400130808      *
183500050629     c     knote01       klist
183600050629     c                   kfld                    flag02
183700050629     c                   kfld                    clipot
183800050629     c                   kfld                    knk2
183900050629     c                   kfld                    ktnt
184000130808      *
184100090518     C     Kcpi          klist
184200090518     C                   kfld                    CPocpo
184300090518     C                   kfld                    ktpf
184400130808      *
184500050629     C**
184600050629     C     *LIKE         DEFINE    TBLKUT        §KUT
184700050629     C     *LIKE         DEFINE    TBLCOD        §COD
184800050629     C     *LIKE         DEFINE    TBLKEY        §KEY
184900050629     C     *LIKE         DEFINE    CPOFLT        NUMFLT
185000050629     C     *LIKE         DEFINE    CPOFLT        SAVFLT
185100050629     C     *LIKE         DEFINE    VIDFLT        OLDFLT
185200050629     C     *LIKE         DEFINE    E14CAP        VARCAP
185300050629     C     *LIKE         DEFINE    E14LOC        VARLOC
185400050629     C     *LIKE         DEFINE    E14PRV        VARPRV
185500050629     C     *LIKE         DEFINE    E14NAR        VARNAR
185600130808      *
185700050629     C                   ENDSR
185800050629
185900920506      *------------------------------------------------------------------------
186000981130** TES
186100981123 *** GESTIONE  CLIENTI  POTENZIALI ***
186200981123 * INTERROGAZIONE CLIENTI POTENZIALI *
186300981123** ERR
186400080305Cod.fiscale già presente su altri potenziali. Enter per forzare               1
186500080305Partita Iva già presente su altri potenziali. Enter per forzare               2
186600960610Codice Potenziale non manutenzionabile perchè già in uso: riprovare più tardi!3
186700100319Immettere codice commerciale.                                                 4
186800100319                                                                              5
186900100319                                                                              6
187000100319                                                                              7
187100100319                                                                              8
187200100319                                                                              9
187300100319                                                                              10
187400100319                                                                              11
187500100319                                                                              12
187600100319                                                                              13
187700090626Se potenziale"ceduto"ad altra filiale,il COMMERCIALE deve appartenere alla fil
187800041207Potenziale non gestibile dall'utente                                          15
187900080626Presente pot.Sede"xxxxxxxxxxxxxxx"con stesso CodFiscale:modificare Cdf o Sd/Fi
188000981112Impossibile inserire un codice potenziale gia' esistente                      17
188100100319                                                                              18
188200981009Codice potenziale inesistente                                                 19
188300981012Codice commerciale inesistente                                                20
188400100319                                                                              21
188500100319                                                                              22
188600100319                                                                              23
188700981012Numero di telefono obbligatorio                                               24
188800080207Categoria merceologica inesistente o non utilizzabile                         25
188900090626Fil. di appartenenza errata                                                   26
189000090626Fil. di appartenenza immessa non in gestione all'utente. ENTER PER FORZARE    27
189100090626Fil. di appartenenza incongruente col CAP: ENTER per forzare                  28
189200100319Codice commerciale obbligatorio se gia'presente.                              29
189300981012Codice commerciale non utilizzabile                                           30
189400100319Codice commerciale non in gestione all'utente.                                31
189500100319                                                                              32
189600100319                                                                              33
189700100319                                                                              34
189800100319                                                                              35
189900100319                                                                              36
190000100319                                                                              37
190100080623Potenziale "S E D E" non modificabile                                         38
190200080623Potenziale " F I L I A L E" non modificabile                                  39
190300100319                                                                              40
190400100319                                                                              41
190500080626Presente pot.Sede"xxxxxxxxxxxxxxx"con stessa Par.IVA:modificare P.IVA o Sd/Fi 42
190600100319                                                                              43
190700100319                                                                              44
190800100319                                                                              45
190900050630Numeratore mancante : contattare EDP di sede!!!!                              46
191000100319                                                                              47
191100100319                                                                              48
191200100422Modifica non ammessa: la filiale xxx NON ha NUOVA GESTIONE TRATTATIVE attiva  49
191300100422Immettere Codice Fiscale . Enter per forzare                                  50
191400080205Immettere la Partita Iva. Enter per forzare                                   51
