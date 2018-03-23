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
001900160706     fTNCPVT1L  if   e           k disk
002000120703     FTRMK02D   CF   E             WORKSTN
002100041207
002200041207     d codut           s              1  0 Inz(1)
002300050518     d ktnt            s                   like(ntctnt)
002400050518     d knk2            s                   like(ntcnk2) inz
002500050518     d conta           s              1  0 Inz(1)
002600060714     d savprv          s                   like(cpoprv)
002700080206     d savnaz          s                   like(vidnaz)
002800080207     d savcdf          s                   like(vidcdf) inz
002900080207     d cod_forzcdf     s                   like(cpocdf) inz
003000080207     d cod_forziva     s                   like(cpopiv) inz
003100080626     d savrag          s             15
003200080626     d savfsf          s              1
003300080207     d saviva          s                   like(vidiva) inz
003400080207     d codice          s                   like(vidiva) inz
003500080206     d wforzacdf       s              1    inz(*Off)
003600080206     d wforzaiva       s              1    inz(*Off)
003700080206     d wforzaiva1      s              1    inz(*Off)
003800131126     d $logistica      s              1    inz(*Off)
003900090518     d ktpf            s                   like(cpitpf)
004000100120     d Immissione      s              1n   inz(*off)
004100100409     d W_daatti        s              1
004200100810     d $inke           s               n
004300041207
004400920415      * TABELLE------------------------------------------------------*
004500951009     D*
004600981123     D TES             S             38    DIM(3) CTDATA PERRCD(1)
004700080205     D MSG             S             78    DIM(51) CTDATA PERRCD(1)
004800891116     D*--------------------------------------------------------------*
004900920417      *
005000090716     d ds1t          e ds
005100080207     D DS1L          E DS
005200110203     d dcpo          e ds
005300920417      *
005400900328     D KPJBA         E DS
005500920417      *
005600981109     D* PASSAGGIO PARAMETRI TRA PGM  DI POTENZIALI
005700091028     D trmk01ds      E DS
005800080925     D DSMK50        E DS                  EXTNAME(TRMK50DS)
005900091028     D trmk17ds      E DS
006000100409     D trmk19ds      E DS
006100100617     d sav_trmk19    e ds                  extname(TRMK19DS)
006200100617     d                                     prefix(w_)
006300091028     d trmk20ds      e ds
006400091028     d TRMK21ds      e ds                  inz
006500130808     d TRMK43ds      e ds                  inz
006600100809     d TNTA88DS      e ds
006700920417     D*
006800941125     D WLBDA8          DS
006900941125     D  G08DAT                 1      8  0
007000941125     D  G08INV                 9     16  0
007100941125     D  G08ERR                17     17
007200941125     D  G08TGI                18     22  0
007300941125     D*
007400970711     D* DS PER FNLV13R - CONTROLLO DI UN CAP
007500970711     D DSLV13        E DS                  EXTNAME(FNLV13DS)
007600970711     D* DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
007700970711     D DSLV14        E DS                  EXTNAME(FNLV14DS)
007800970711     D* DS PER TISI95R - CONTROLLO DI CAP
007900970711     D DSSI95        E DS                  EXTNAME(TISI95DS)
008000981014     D* Ds esterna x richiamo pgm controllo fax
008100981014     D TRUL42        E DS                  EXTNAME(TRUL42DS)
008200020708     D* DS PER TRUL33R - REPERIMENTO AGGIORNAMENTO NUMERATORI in GRU
008300020708     D TRUL33DS      E DS
008400021017
008500090626     d tntaa1ds      e ds
008600041207
008700041207     d Azuteds       e ds                  extname(Azute00f)
008800041207     d dDatiute      e ds
008900100922     d dute01        e ds
009000041207     d Tibs34ds      e ds
009100060713     d dspr          e ds
009200081003     d dcpo01        e ds
009300140910     d dcpo02        e ds
009400090716     d tnta12ds      e ds
009500091030     d tibs02ds      e ds
009600091125     d tnta69ds      e ds
009700081003     d
009800080205      * Partita I.V.A.
009900080205     D                 DS
010000080205     D  Vidiva                 1     16
010100080205     D  Vidivaeu               1      2
010200080205     D  Vidivait               3     16
010300080205
010400151021     d xcfiva1ds     e ds
010500080205     d  ivaeu                  3      4
010600080205     d  ivait                  5     18
010700160705
010800160705     d TRMK30DS      e ds
010900160705     d TNCPO_30      e ds                  extname(TNCPO00F) inz
011000160705     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
011100160705     d TICPI_30      e ds                  extname(TICPI00F) inz
011200160706     d TRMK31DS      e ds
011300130808
011400130808     d c_Digits        c                   const('0123456789')
011500130808      *
011600080222     D* CAMPO PER DSPATR "HI"       DEI CAMPI A VIDEO
011700080208     D hi              C                   CONST(X'22')
011800080222     D* CAMPO PER DSPATR "normal" (per togliere hi)
011900080222     D nm              C                   CONST(X'20')
012000160705
012100160705      //---------------------------------------------------------------
012200160705      //?Definizione Procedure richiamate.
012300160705      //---------------------------------------------------------------
012400160705     d TRMK30R         pr                  extpgm('TRMK30R')
012500160705     d  trmk30ds                           likeds(trmk30ds)
012600160705     d  tncpo_30                           likeds(tncpo_30)
012700160705     d  tncpo1_30                          likeds(tncpo1_30)
012800160705     d  ticpi_30                           likeds(ticpi_30)
012900020919
013000020919      * USO DEGLI INDICATORI-----------------------------------------*
013100981012      * 02    : INDICA L'INTERROGAZIONE
013200100202      * 03    : richiamato da menù
013300120120      * 05    : Abilita F5=Attività
013400120120      * 06    : Abilita F22=Richiesta Contatto
013500981014      * 10    : Record non trovato su TNCPO00F- IMMISSIONE
013600091028      * 11    : F14 Pink - Info commerciali presenti
013700110203      * 12    : categoria potenziale <> da 'M'
013800140716      * 13    : Record non trovato su TNCPO10F- IMMISSIONE
013900981105      * 28    : EMISSIONE MESSAGGIO DI ERRORE
014000120703      * 40-56 : POSIZIONAMENTO CURSORE
014100981105      * 59    : PGM RICHIAMATO DA ANNULLAMENTO VISITA: INIBISCO SCELTA
014200981105      *          1-VISITE / F16-ANNULLA / F12-RITORNO
014300981111      * 60-64 : ERRORE ERRMSG IN VID5
014400120703      * 66    : Posizionamento cursore
014500120703      * 67    : Posizionamento cursore
014600100506      * 77    : F05 Pink - Attività
014700091028      * 78    : F02 Pink - Rubrica presente
014800091028      * 79    : F18 Pink - Note presenti
014900160706      * 80    : F19 Pink - Variazioni presenti
015000981012      * 89    : ON --> P.O. - OFF --> SEDE
015100981012      * 90    : ERRORE GENERICO
015200981012      *
015300920415     C*----------------------------------------------------------------*
015400941111     C* MAIN LINE
015500941111     C*----------------------------------------------------------------*
015600920415      *
015700981009     C**
015800981009     C                   MOVEL     VIDCP1        CPOCPO
015900981009     C                   MOVE      VIDCP2        CPOCPO
016000050629     C* CONTROLLI sul potenziale
016100050629     C                   EXSR      CTRcpo
016200130808      *
016300981009     C**
016400981009     C* SE IL POTENZIALE ESISTE CARICO I DATI
016500981009     C  N10              EXSR      CARICA
016600981014     C* PER IMMISSIONE NON CI POSSONO ESSERE STATI NELLO STORICO
016700091104     C   10              SETON                                        09
016800920612      *
016900091028      * Verifico sul file note se già ci sono note per il potenziale
017000091028      * Se ci Pink F18
017100091028     C                   setoff                                       79
017200091028     c     cpocpo        chain     ticpn01l
017300091028     c                   if        %found(ticpn01l)
017400091028     c                   eval      *in79 = *on
017500091028     c                   endif
017600091028      * Verifico sul TFNTC01L se già ci sono contatti per il potenziale
017700091028      * Se ci sono Pink tasto F2-Rubrica
017800920612     C                   MOVE      'P'           FLAG02
017900920612     C                   MOVEL     VIDCP1        CLIPOT
018000920612     C                   MOVE      VIDCP2        CLIPOT
018100091028     C                   setoff                                       78
018200010130     C     knote         setll     tfntc01l
018300010130     C                   do        *hival
018400091029     C     knote         reade     tfntc01l
018500010130     C                   if        %eof(tfntc01l)
018600010130     C                   leave
018700010130     C                   endif
018800090716     c                   if        ntcflt = 'A'
018900090716     c                   iter
019000090716     c                   endif
019100090716      * controllo se Nota o Contatto
019200090716     c                   clear                   ds1t
019300090716     c                   eval      cod = '1T'
019400090716     c                   eval      key = ntctnt
019500091029     c     ktab          chain     tabel00f
019600090716     c                   if        %found(tabel00f) and tblflg = ' '
019700090716     c                   eval      ds1t = tbluni
019800090716     c                   endif
019900090716      * Nota
020000090716     c                   if        §1trich = 'N'
020100091028     c                   iter
020200090716     c                   endif
020300090716      * Contatto
020400090716     c                   if        §1trich = 'C'
020500090716     c  n78              eval      *in78 = *on
020600091028      * esco dal ciclo, ne basta uno per Pink tasto F2=Rubrica
020700091028     C                   leave
020800090716     c                   endif
020900010130     C                   enddo
021000160706      *
021100160706      * Verifico se ci sono variazioni per il potenziale
021200160706      * Se sì Pink F19
021300160706     c                   eval      *in80 = *off
021400160706     c     CPOcpo        chain     TNCPVT1L
021500160706     c                   IF        %found(TNCPVT1L)
021600160706     c                   eval      *in80 = *on
021700160706     c                   ENDIF
021800981105      *
021900981014     C* IN INSERIMENTO DATA ACQUISIZIONE = DATA GIORNO
022000981014     C   10              Z-ADD     GMADAT        VIDDAC
022100981123     C* SE RICHIAMATO E PASSATTO MESSAGGIO DI ERRORE LO VISUALIZZO
022200091028     C     mk01K10       IFNE      ' '
022300091028     C     mk01K10       ANDNE     'I'
022400091028     C     mk01MSG       ANDNE     *BLANKS
022500091028     C                   MOVEL     mk01MSG       VIDMSG
022600981123     C                   SETON                                        28
022700981123     C                   ENDIF
022800940318     C*
022900981012     C     FOR02         TAG
023000100810     c                   eval      $inke = *off
023100050518      * carico il responsabile trasporti dalla nota '05'
023200050518     c                   Eval      conta = 1
023300050518     c                   Eval      flag02 = 'P'
023400050518     c                   Movel     vidcp1        clipot
023500050518     c                   Move      vidcp2        clipot
023600050518     c                   Eval      ktnt = '05'
023700050518     c     knote01       Setll     Tfntc01l
023800050518     c                   Do        *Hival
023900091029     c     knote01       Reade     Tfntc01l
024000050518     c                   If        %Eof(Tfntc01l)
024100050518     c                   Leave
024200050518     c                   EndIf
024300050518     c                   If        conta = 1
024400050518     c                   Eval      vidrsp1 = ntcrnt
024500050518     c                   Else
024600080213     c                   Eval      vidrsp1 = %trim(vidrsp1) + ', ' +
024700080205     c                             ntcrnt
024800050518     c                   EndIf
024900050518     c                   add       1             conta
025000050518     c                   EndDo
025100100528      *
025200120120      * - Abilitazione F5=Attività sempre se nel sistema informativo di filiale
025300120120      *                            solo in interrogazione se S.I. di sede
025400120120     c                   IF        %subst(KNSIF : 1 : 3) = 'FIL'
025500120120     c                   eval      *in05 = *on
025600120120     c                   ELSE
025700120120     c                   eval      *in05 = *in02
025800120120     c                   ENDIF
025900130808      *
026000120120      * - Abilitazione F22=Richiesta contatto se nel S.I. di filale
026100120120      * - e solo se cliente in categoria <> da codificato
026200120120     c                   IF        %subst(KNSIF : 1 : 3) = 'FIL' and
026300120120     c                             CPOfls <> 'C'
026400120120     c                   eval      *in06 = *on
026500110209     c                   ENDIF
026600131126      *
026700131126      * - Disattivo il tasto F22= Rischiesta contatto e tasto F5= Attivita
026800131126      * - Solo in interrogazione se richiamato dalla logistica
026900131126     c                   If        $logistica = *on
027000131126     c                   eval      *in05 = *in02
027100131126     c                   eval      *in06 = *off
027200131126     c                   Endif
027300130808      *
027400100617      * Controllo se ci sono attività
027500100506      * Pink F05-Attività
027600091028     c     cpocpo        chain     tiatc04l
027700091028     c                   if        %found(tiatc04l)
027800091028     c                   eval      *in77 = *on
027900091028     c                   else
028000091028     c                   eval      *in77 = *off
028100091028     c                   endif
028200920612      *
028300120703     C                   EXFMT     MK02D01
028400941125     C*
028500941125     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN90)
028600941125     C                   CLEAR                   VIDMSG
028700941125     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
028800021127     C                   SETOFF                                       65
028900981012     C                   SETOFF                                       404142
029000981012     C                   SETOFF                                       434445
029100981012     C                   SETOFF                                       464748
029200981012     C                   SETOFF                                       495051
029300981012     C                   SETOFF                                       525354
029400981014     C                   SETOFF                                       902855
029500080205     C                   SETOFF                                       5666
029600981014     C* F3  - FINE
029700981109     C     *INKC         IFEQ      *ON
029800091028     C                   MOVEL     '1'           mk01F03
029900981109     C                   GOTO      FINE
030000981109     C                   ENDIF
030100981014     C* F12 - RITORNO
030200050629     C                   IF        *inkl
030300091028     C                   MOVEL     '1'           mk01F12
030400110601     c                   IF        w_DaAtti = 'S' and CPOfls = 'C'
030500110601     c                   clear                   mk01f06
030600110601     c                   ENDIF
030700981014     C                   GOTO      FINE
030800981014     C                   ENDIF
030900100122      * attivo in questo punto il controllo del commerciale
031000100122      * se sono in manutenzione e premo qualsiasi tasto (che non sia l'enter)
031100100122      * devo dare errore se manca il commerciale
031200101202      * o se commerciale con particolarità impostata sulla tabella
031300100122     c                   IF        not *in02 and not *in10 and
031400100122     c                             (*inkb or *inks or *inku or
031500160706     c                              *inkt or
031600100524     c                              *inkw or *inkn or *inke) and
031700101202     c                             (vidcmm = *zeros or vidcmm = *blanks
031800130808     c                              or CMMpar <> *blanks)
031900100122     C                   SETON                                        502890
032000100319     C                   eval      vidmsg = msg(04)
032100100122     C                   GOTO      FOR02
032200100122    2C                   ENDIF
032300940318     C*
032400130808      *
032500091125      * F20 - Interrogazione codici collegati (anagrafica clienti)
032600091125     c                   if        *inku
032700091125     c                   clear                   tnta69ds
032800091125     c  n02              eval      Ita69Fle = '2'
032900091125     c   02              eval      Ita69Fle = '5'
033000091125     c                   eval      Ita69Cpo = cpocpo
033100091125     c                   call      'TNTA69R'
033200091125     c                   parm                    kpjba
033300091125     c                   parm                    tnta69ds
033400091125     c                   goto      for02
033500091125     c                   endif
033600160707
033700160707      * F19 - Interrogazione Variazioni Potenziale
033800160707     c                   IF        *inKT
033900160707     c                   clear                   TRMK31DS
034000160707     c                   eval      IMK31cpo = CPOcpo
034100160707     c                   eval      IMK31rag = CPOrag
034200160707     c                   eval      kpjbu = TRMK31DS
034300160707     c                   call      'TRMK31R'
034400160707     c                   parm                    kpjba
034500160707     c                   goto      for02
034600160707     c                   ENDIF
034700981012     C**
034800981012     C** CONTROLLO FORMATO2
034900981124     C                   EXSR      CTRD02
035000981012     C   90              GOTO      FOR02
035100100524      * Se F22 = Richiesta contatto
035200100120      * prima controllo se commerciale già inserito
035300100120      * altrimenti errore
035400100524     c                   IF        *inkw and
035500100120     c                             (vidcmm = *zeros or vidcmm = *blanks)
035600100120     C                   SETON                                        502890
035700100319     C                   eval      vidmsg = msg(04)
035800100120     C                   GOTO      FOR02
035900100120    2C                   ENDIF
036000940321     C*
036100981110     C* IN GESTIONE CON ENTER, RIEMETTO VIDEATA,
036200091030     C*  IN INTERROGAZIONE ESCO se no tasti funzione
036300981110     C     *IN02         IFEQ      *OFF
036400981110     C     *INKF         ANDEQ     *OFF
036500020708     C     *INKN         ANDEQ     *OFF
036600100524     C     *INKW         ANDEQ     *OFF
036700100506     C     *INKe         ANDEQ     *OFF
036800140918     c     *inkb         andeq     *off
036900140918     c     *inks         andeq     *off
037000160706     c     *inkt         andeq     *off
037100981110     C                   GOTO      FOR02
037200981110     C                   ENDIF
037300940318     C*
037400981027     C* F6 O F8 - CONFERMA
037500091028     C     mk01K10       IFNE      'N'
037600160705
037700160705      /free
037800160705       //?Salvo l'immagine Precedente
037900160705         exsr ImmaginePrima;
038000160705      /end-free
038100940321     C                   EXSR      AGGIOR
038200981102     C                   ENDIF
038300140918      * f2 - Inserimento o Variazione Rubrica
038400140918     c                   if        *inkb
038500140918     c                   clear                   tnta12ds
038600140918     c  n02              Eval      ta12ric = 'M'
038700140918     c   02              Eval      ta12ric = 'V'
038800140918     c                   Eval      ta12apl = 'P'
038900140918     c                   Eval      ta12pot = vidcp1 * 100000000 + vidcp2
039000140918     c                   Eval      ta12rag = vidrag
039100140918     c                   Call      'TNTA12R'
039200140918     c                   Parm                    kpjba
039300140918     c                   Parm                    tnta12ds
039400141027      * riaggancio l'anagrafico
039500141027     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
039600141027     c   02cpocpo        chain(n)  tncpo01l                           10
039700141027     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
039800141027     c   02cpocpo        chain(n)  tncpo11l                           13
039900141027     c                   if        %error
040000141027     c                   eval      mk01m10 = msg(3)
040100141027     c                   eval      mk01err = 'E'
040200141027     c                   goto      fine
040300141027     c                   endif
040400140918     c                   goto      for02
040500140918     c                   endif
040600140918      *
040700140918      * CMD18 - Inserimento o Variazione NOTE
040800140918     C     *INKS         IFEQ      '1'
040900140918     c                   clear                   trmk20ds
041000140918     c   02              eval      IMK20tla = 'L'
041100140918     c   02              eval      imk20flm = 'I'
041200140918     C                   eval      imk20cpo = cpocpo
041300140918     C                   eval      imk20rsc = vidrag
041400140918     c                   call      'TRMK20R'
041500140918     c                   parm                    kpjba
041600140918     c                   parm                    trmk20ds
041700141027      * riaggancio l'anagrafico
041800141027     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
041900141027     c   02cpocpo        chain(n)  tncpo01l                           10
042000141027     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
042100141027     c   02cpocpo        chain(n)  tncpo11l                           13
042200141027     c                   if        %error
042300141027     c                   eval      mk01m10 = msg(3)
042400141027     c                   eval      mk01err = 'E'
042500141027     c                   goto      fine
042600141027     c                   endif
042700140918     C                   GOTO      FOR02
042800140918     C                   END
042900981102     C**
043000081015     C* PER F14 - INFORMAZIONI COMMERCIALI
043100080925     C                   CLEAR                   DSMK50
043200981103     C     *INKN         IFEQ      *ON
043300080925     C                   MOVEL     CPOCPO        I50cpo
043400091029     C     mk01K10       IFEQ      'N'
043500080925     C                   MOVEL     'I'           I50MOD
043600981102     C                   ELSE
043700080925     C                   MOVEL     'G'           I50MOD
043800081015     C                   MOVEL     ' '           I50OBL
043900981102     C                   ENDIF
044000080925     C                   MOVEL     CPORAG        I50rag
044100081003     c                   movel     §cpoifotot    i50ifotot
044200981102     C**
044300080925     C                   CALL      'TRMK50R'
044400981102     C                   PARM                    KPJBA
044500080925     C                   PARM                    DSMK50
044600981103     C                   ENDIF
044700920415      *
044800020722     C* GESTIONE Ritorno/Fine INFO COMMERC. NEI DUE CASI
044900020722     C* DI PGM RICHIAMATO E NON RICHIAMATO
045000080925     C     O50F12        IFEQ      'S'
045100020723      * riaggancio l'anagrafico
045200070523     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
045300070523     c   02cpocpo        chain(n)  tncpo01l                           10
045400140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
045500140716     c   02cpocpo        chain(n)  tncpo11l                           13
045600070523     c                   if        %error
045700091029     c                   eval      mk01m10 = msg(3)
045800091029     c                   eval      mk01err = 'E'
045900070523     c                   goto      fine
046000070523     c                   endif
046100020722     C                   GOTO      FOR02
046200020722     C                   ENDIF
046300080925     C     O50F03        IFEQ      'S'
046400020722     C                   GOTO      FINE
046500020722     C                   ENDIF
046600091028      *
046700100524      * F22 = Richiesta contatto
046800100524     c                   if        *inkw
046900091028     c                   clear                   TRMK17ds
047000100114     c                   eval      imk17patt = 'S'
047100100114     c                   eval      imk17att = 'T'
047200100114     c                   eval      imk17pcau = 'S'
047300100114     c                   eval      imk17cau = '19'
047400091028     c                   eval      imk17cpo = CPOcpo
047500091028     c                   if        vidcmm <> *blanks
047600091028     c                   eval      imk17cmm= %dec(vidcmm:7:0)
047700091028     c                   endif
047800091028     c                   call      'TRMK17R'
047900091028     c                   parm                    kpjba
048000091028     c                   parm                    trmk17ds
048100091028      * riaggancio l'anagrafico
048200091028     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
048300091028     c   02cpocpo        chain(n)  tncpo01l                           10
048400140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
048500140716     c   02cpocpo        chain(n)  tncpo11l                           13
048600091028     c                   if        %error
048700091028     c                   eval      mk01m10 = msg(3)
048800091028     c                   eval      mk01err = 'E'
048900091028     c                   goto      fine
049000091028     c                   endif
049100091028     C                   GOTO      FOR02
049200091028     c                   endif
049300130808      *
049400100506      * F05 = Interrogazione attività
049500100810     c                   if        *inKe
049600100617     c                   clear                   TRMK21ds
049700091028     c                   eval      I21cpo = CPOcpo
049800091028     c                   eval      I21rsc = VIDrag
049900100810      * se richiamato dalla gestione attività/trattative deve andare in gestione
050000100810      * con visualizzazione sempre delle attività e senza dare il msg di altre attività
050100100810      * in window
050200100810     c                   IF        w_DaAtti = 'S'
050300100810     c                   eval      $inke = *on
050400100810     c                   clear                   omk19cpo
050500100810     c                   clear                   omk19ksc
050600100810     c                   clear                   omk19err
050700100810     c                   clear                   omk19msg
050800100810     c                   eval      I21mod = 'V'
050900100810     c                   eval      sav_trmk19 = trmk19ds
051000100810     c                   eval      KPJBU  = TRMK21ds
051100100810     c                   eval      IMK19cpo = CPOcpo
051200100810     c                   call      'TRMK21R'
051300100810     c                   parm                    kpjba
051400100810     c                   parm                    trmk19ds
051500100810      * se richiamato da interrogazione o da menù deve andare in interrogazione
051600100810     c                   ELSE
051700100810     c                   eval      I21mod = 'I'
051800091028     c                   eval      KPJBU  = TRMK21ds
051900091028     c                   call      'TRMK21R'
052000091028     c                   parm                    kpjba
052100100810     c                   ENDIF
052200091028     c                   eval      TRMK21ds = KPJBU
052300091028      * riaggancio l'anagrafico
052400091028     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
052500091028     c   02cpocpo        chain(n)  tncpo01l                           10
052600140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
052700140716     c   02cpocpo        chain(n)  tncpo11l                           13
052800091028     c                   if        %error
052900091028     c                   eval      mk01m10 = msg(3)
053000091028     c                   eval      mk01err = 'E'
053100091028     c                   goto      fine
053200091028     c                   endif
053300100810      * da attività
053400100810     c                   IF        w_DaAtti = 'S'
053500100810      * rimetto la trmk19ds come prima
053600100810     c                   eval      trmk19ds = sav_trmk19
053700100810      * se F12 riemetto la videata del potenziale
053800100810     c                   IF        O21fxx = '12'
053900100810     c                   clear                   mk01f06
054000100810     c                   goto      for02
054100100810     c                   ENDIF
054200100810      * se ho gestito l'attività dal TRMK21R vado a fine
054300100810     c                   IF        O21fxx = '99'
054400100810     c                   clear                   mk01f06
054500100810     c                   goto      fine
054600100810     c                   ENDIF
054700100810      * se ho dato F10 vdevo andare avanti con il TRMK19R o TNTA88R
054800100810     c                   IF        O21fxx = '10'
054900100810     c                   clear                   mk01f06
055000100810     c                   ENDIF
055100100810      * se non ho fatto niente spengo $inke e vado avanti
055200100810     c                   IF        O21fxx = *blanks
055300100810     c                   eval      $inke = *off
055400100810     c                   ENDIF
055500100810     c                   ELSE
055600091028     C                   GOTO      FOR02
055700100810     c                   ENDIF
055800091028     c                   endif
055900020722      *
056000091028     C                   MOVEL     VIDCP1        mk01CPO
056100091028     C                   MOVE      VIDCP2        mk01CPO
056200091028     C                   MOVEL     VIDRAG        mk01RAG
056300091028     C                   MOVEL     '1'           mk01F06
056400130808      *
056500100409      * se sono stato richiamato dalla gestione attività richiamo il programma trmk19
056600100810      * se non sono già passata per Inke
056700100810     c                   If        W_Daatti = 'S'
056800100617     c                   clear                   omk19cpo
056900100617     c                   clear                   omk19ksc
057000100617     c                   clear                   omk19err
057100100617     c                   clear                   omk19msg
057200110518      * Se potenziale codificato e non sono passata da F5-Attività non devo dare la
057300110518      * possibilità di fare una nuova attività o aprire trattativa.
057400110518     c                   IF        CPOfls = 'C'
057500110518     c                   eval      *in28 = *on
057600110518     c                   eval      *in90 = *on
057700110518     c                   eval      vidmsg =
057800110518     c                             'Nuova Attività/Trattativa non possibile-
057900110518     c                              per Potenziale CODIFICATO.'
058000110518     c                   goto      for02
058100110518     c                   ENDIF
058200100809      * Prima di passare al trmk19r o TNTA88R
058300100809      * richiamo il trmk21r per dare la possibilità di gestire
058400100617      * eventuali attività ancora da eseguire
058500100617      * *in77 = F5-Attività in pink (attività presenti sul potenziale)
058600100810     c                   IF        *in77 and not $inke
058700100617      * salvo la trmk19ds così come mi è arrivata
058800100617     c                   eval      sav_trmk19 = trmk19ds
058900100617     c                   clear                   TRMK21ds
059000100617     c                   eval      I21mod = 'G'
059100100617     c                   eval      I21cpo = CPOcpo
059200100617     c                   eval      I21rsc = VIDrag
059300100617     c                   eval      KPJBU  = TRMK21ds
059400100617     c                   eval      IMK19cpo = CPOcpo
059500100617     c                   call      'TRMK21R'
059600100617     c                   parm                    kpjba
059700100617     c                   parm                    trmk19ds
059800100617     c                   eval      TRMK21ds = KPJBU
059900100706      * rimetto la trmk19ds come prima
060000100706     c                   eval      trmk19ds = sav_trmk19
060100100706      * se F12 riemetto la videata del potenziale
060200100706     c                   IF        O21fxx = '12'
060300100706     c                   clear                   mk01f06
060400100706     c                   goto      for02
060500100706     c                   ENDIF
060600100706      * se ho gestito l'attività dal TRMK21R vado a fine
060700100617     c                   IF        O21fxx = '99'
060800100617     c                   clear                   mk01f06
060900100617     c                   goto      fine
061000100617     c                   ENDIF
061100100617     c                   ENDIF
061200100617      * richiamo la gestione attività
061300100809      * o gestione trattative, dipende da dove arrivo
061400100430     c                   clear                   omk19cpo
061500100430     c                   clear                   omk19ksc
061600100430     c                   clear                   omk19err
061700100430     c                   clear                   omk19msg
061800100910     c                   IF        IMK19tco = 'T'
061900170215     c                   eval      IMK19tat = IMK19tco
062000100409     c                   call      'TRMK19R'
062100100409     c                   Parm                    kpjba
062200100409     c                   Parm                    trmk19ds
062300100409     c                   parm                    trmk01ds
062400100910     c                   endif
062500100910     c                   IF        IMK19tco = 'O'
062600100910     c                   clear                   tnta88ds
062700101111     c                   eval      ita88cmmi = imk19com
062800100910     c                   eval      ita88fle = imk19fle
062900100910     c                   eval      ita88cpo = mk01cpo
063000100910     c                   call      'TNTA88R'
063100100910     c                   Parm                    kpjba
063200100910     c                   Parm                    tnta88ds
063300100910     c                   clear                   mk01f06
063400100910     c                   ENDIF
063500100409     c                   endif
063600130808     c*
063700080925     C                   GOTO      FINE
063800920415      *
063900920415     C     FINE          TAG
064000970711     C*
064100970711     C                   MOVEL     'C'           I13TLA
064200970711     C                   CALL      'FNLV13R'
064300970711     C                   PARM                    KPJBA
064400970711     C                   PARM                    DSLV13
064500970711     C                   PARM                    DSSI95
064600970711     C*
064700970711     C                   MOVEL     'C'           I14TLA
064800970711     C                   CALL      'FNLV14R'
064900970711     C                   PARM                    KPJBA
065000970711     C                   PARM                    DSLV14
065100090626     C**
065200080925     C                   CLEAR                   DSMK50
065300080925     C                   MOVEL     'C'           I50TLA
065400080925     C                   CALL      'TRMK50R'
065500981102     C                   PARM                    KPJBA
065600080925     C                   PARM                    DSMK50
065700090626     C*
065800090626     C                   clear                   tntaa1ds
065900090626     C                   movel     'C'           Itaa1Tla
066000090626     C                   movel(p)  tntaa1ds      kpjbu
066100090703     C                   CALL      'TNTAA1C'
066200090626     C                   PARM                    KPJBA
066300160705
066400160705      /free
066500160705         clear TRMK30DS;
066600160705         clear tncpo_30;
066700160705         clear tncpo1_30;
066800160705         clear ticpi_30;
066900160705         IMK30tla = 'C';
067000160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
067100160705      /end-free
067200160705
067300971203     C**
067400920415     C                   SETON                                        LR
067500920416      *-------------------------------------------------------------------------
067600920416      *SUBROUTINE RICERCA ELEMENTI DI TABELLA
067700940317      *-------------------------------------------------------------------------
067800981012     C     CERTAB        BEGSR
067900920416     C                   MOVEL     CODUT         §KUT
068000981009     C                   MOVEL     COD           §COD
068100981103     C                   CLEAR                   §KEY
068200920416     C                   MOVE      *BLANKS       VIDELE
068300920416     C                   CALL      'X§TABER'
068400920416     C                   PARM                    §KUT
068500920416     C                   PARM                    §COD
068600920416     C                   PARM                    §KEY
068700920416     C                   PARM                    §DES             30
068800920416     C                   MOVEL     §KEY          VIDELE           10
068900920416     C                   MOVEL     §DES          DESELE           30
069000920416     C                   ENDSR
069100981009     C*---------------------------------------------------------------
069200050629     C* CONTROLLO potenziale
069300981009     C*---------------------------------------------------------------
069400050629     C     CTRcpo        BEGSR
069500081003     c                   clear                   dcpo01
069600140910     c                   clear                   dcpo02
069700140930     c                   clear                   cpotel
069800981009      *
069900981009     C* CHAIN
070000981009     C                   MOVEL     VIDCP1        CPOCPO
070100981009     C                   MOVE      VIDCP2        CPOCPO
070200981009     C**
070300981014    0C     CPOCPO        IFGT      0
070400091028     C     mk01K10       IFNE      'N'
070500981014     C     CPOCPO        CHAIN     TNCPO01L                           1094
070600140716     C     CPOCPO        CHAIN     TNCPO11L                           13
070700981023     C                   ELSE
070800981023     C     CPOCPO        CHAIN(N)  TNCPO01L                           10
070900140716     C     CPOCPO        CHAIN(n)  TNCPO11L                           13
071000981023     C                   ENDIF
071100981023     C***
071200981009     C* 94 ON  - RECORD ALLOCATO
071300981009    1C     *IN94         IFEQ      *ON
071400091028     C                   Eval      mk01m10 = msg(3)
071500091028     c                   Eval      mk01err = 'E'
071600050629     C                   GOTO      fine
071700981009    1C                   ENDIF
071800981014     C* SE NON IMMESSO POTENZIALE ACCENDO INDICATORE 10
071900981014     C                   ELSE
072000981014     C                   SETON                                        10
072100981014    0C                   ENDIF
072200021023      *
072300981009     C*
072400100120     C* - I M M I S S I O N E
072500100120    1C     Immissione    IFEQ      *ON
072600981009     C*
072700981009     C* CODICE GIA' ESISTENTE: ERRORE
072800981009    2C     *IN10         IFEQ      *OFF
072900091028     C                   Eval      mk01m10 = msg(17)
073000091028     c                   Eval      mk01err = 'E'
073100050629     C                   GOTO      fine
073200981009   X2C                   ELSE
073300020708      *
073400981009     C* SE NON IMMESSO NUMERO LO PRENDO DAL NUMERATORE (DS1S)
073500020708    3C*  10VIDCP2        IFEQ      *ZEROS
073600041026      * LO PRENDO SEMPRE DA SIMFEL+NUMERATORE
073700981009     C                   Z-ADD     SIMFEL        VIDCP1
073800020709      *
073900020709     C                   setoff                                       94
074000020708     C                   clear                   Trul33ds
074100020712     C                   Z-ADD     vidcp1        I33PO1
074200020709     C                   Z-ADD     0             I33OPE
074300020712     C                   Z-ADD     23            I33CNU
074400020709     C                   Z-ADD     1             I33NUM
074500020712     C     *in94         doweq     *off
074600020708     C                   movel(p)  trul33ds      kpjbu
074700020708     C                   CALL      'TRUL33R'
074800020708     C                   PARM                    KPJBA
074900020708     C                   eval      trul33ds = kpjbu
075000020708      *
075100020709     C                   if        O33ERR = *ZEROS
075200020709      *
075300020708      * Controllo che il numero non esista gia' nel file potenziali
075400020709     C                   eval      cpocpo = (Vidcp1*100000000) + O33NRI
075500981023     C     CPOCPO        CHAIN(N)  TNCPO01L                           94
075600020723      * Trul33 in errore
075700020723     C                   ELSE
075800091028     C                   Eval      mk01m10 = msg(46)
075900091028     c                   Eval      mk01err = 'E'
076000050629     C                   GOTO      fine
076100020712     C                   ENDIF
076200981009    4C                   ENDDO
076300020709     C                   MOVE      CPOCPO        vidcp2
076400040930     c                   Clear                   Cpoatb
076500020708    2C                   ENDIF
076600981009     C*
076700981009   X1C                   ELSE
076800981009     C* CODICE INESISTENTE
076900981009    2C     *IN10         IFEQ      *ON
077000091028     C                   Eval      mk01m10 = msg(19)
077100091028     c                   Eval      mk01err = 'E'
077200050629     C                   GOTO      fine
077300981014   X2C                   ELSE
077400981014     C**
077500090626     C* CONTROLLO abilitazioni utente
077600091028     C     mk01K10       Ifne      'N'
077700091028     C     mk01K10       ANDNE     'C'
077800091028     C     mk01K10       ANDNE     'B'
077900090626     c*
078000090626     c                   clear                   tntaa1ds
078100090626     c                   eval      itaa1cpo =cpocpo
078200090626     c                   eval      itaa1caut='§UTEPOT'
078300090626     c                   clear                   kpjbu
078400090626     c                   movel     tntaa1ds      kpjbu
078500090703     c                   call      'TNTAA1C'
078600090626     c                   parm                    kpjba
078700090626     c                   movel     kpjbu         tntaa1ds
078800130808     c*
078900090626     c* Errore o non abilitato
079000090626    3c                   if        otaa1fabi='N'
079100091028     C                   Eval      mk01m10 = msg(15)
079200091028     c                   Eval      mk01err = 'E'
079300050629     C                   GOTO      Fine
079400981103    4C                   ENDIF
079500981103    3C                   ENDIF
079600981014     C**
079700981009    2C                   END
079800981009    1C                   END
079900050629     C                   ENDSR
080000981012     C*---------------------------------------------------------------
080100981012     C* CARICO DATI DEL POTENZIALE
080200981012     C*---------------------------------------------------------------
080300981012     C     CARICA        BEGSR
080400080206     c                   eval      wforzacdf = *off
080500080206     c                   eval      wforzaiva = *off
080600080206     c                   eval      wforzaiva1 = *off
080700981012      * ANNULLATO
080800080206     C     CPOATB        IFEQ      'A'
080900100202     C                   SETON                                        02
081000981012     C                   END
081100981012      *
081200981012     C                   MOVE      CPORAG        VIDRAG
081300080206     c* SEDE/Filiale (per il momento in sospeso)
081400080623     c                   clear                   vidfsf
081500080623     c                   if        cpofsf='S'
081600080624     c                   eval      vidfsf='SEDE'
081700080623     c                   else
081800080624     c                   eval      vidfsf='FIL '
081900080623     c                   endif
082000130808      *
082100981012     C                   MOVE      CPOVIA        VIDVIA
082200981012     C                   MOVE      CPOCIT        VIDCIT
082300981014     C                   MOVEL     CPOCAP        VIDCAP
082400981012     C                   MOVE      CPOPRV        VIDPRV
082500060714     c                   eval      savprv = cpoprv
082600981012     C                   MOVE      CPONAZ        VIDNAZ
082700080206     C                   MOVE      CPONAZ        savnaz
082800140714     C                   MOVE      CPO1TEL       VIDTEL
082900140714     C                   MOVE      CPO1FAX       VIDFAX
083000080205     c* Codice fiscale
083100080205     c                   eval      vidcdf=cpocdf
083200080207     c                   eval      savcdf=cpocdf
083300080205     c* Partita IVA
083400080205     c                   IF        %subst(cpopiv:1:2) < *zeros
083500080205     C                   eval      Vidivaeu = %subst(cpopiv:1:2)
083600080205     C                   eval      Vidivait = %subst(cpopiv:3)
083700080205     c                   ELSE
083800080205     c                   movel     cpopiv        vidivait
083900080205     c                   ENDIF
084000080207     c                   eval      saviva=vidiva
084100080205      * CATEGORIA merceologica
084200981012     C                   MOVEL     CPOSCT        VIDSCT
084300981012     C                   MOVEL(P)  CPOSCT        KEY
084400981012     C                   EXSR      CTRSCT
084500981012     C                   MOVEL     WDES          DESSCT
084600981012      *
084700981012      * codice commerciale
084800981012     C     CPOCMM        IFNE      *ZEROS
084900981012     C                   MOVEL     CPOCMM        VIDCMM
085000981012     C                   EXSR      CTRCMM
085100981012     C                   ENDIF
085200981012      *
085300080222      * FATTURATO
085400080222     c                   clear                   vidfatd
085500091217     c                   eval      vidfatd='Fatt.to Azienda/1000'
085600080222     c                   if        cpoftaz>0
085700091217     c                   eval      vidfatd=%trim(vidfatd)+ ' ' + hi
085800080222     c                             + %trim(%editc(cpoftaz:'2'))
085900080222     c* Anno fatturato oppure stimato/dichiarato
086000080222     c                   select
086100080222     c                   when      cpoafaz>0
086200080222     c                   eval      vidfatd =%trim(vidfatd) + nm
086300080222     c                             +'Anno' + hi + %editc(cpoafaz:'Z')
086400080222     c                   when      cpoffaz='S'
086500080222     c                   eval      vidfatd=%trim(vidfatd) + nm + 'STIMATO'
086600080222     c                   when      cpoffaz='D'
086700080222     c                   eval      vidfatd=%trim(vidfatd) + nm +'DICHIARATO'
086800080222     c                   endsl
086900080222     c                   if        cpopexp>0
087000080226     c                   eval      vidfatd=%trim(vidfatd) + nm +'di cui export'
087100080226     c                             + hi + %editc(cpopexp:'Z') + '%'
087200080222     c                   endif
087300080222     c                   endif
087400130808     c*
087500090518     c* Ds del campo CPORST
087600090518     c                   eval      dcpo01=cporst
087700140910
087800090518     c* Spesa trasporti reale/presunta
087900090520     C                   CLEAR                   VIDDSPT
088000090518     c                   eval      ktpf='SPT'
088100090518     c     kcpi          chain     ticpi01l
088200090521     c                   select
088300090521     c                   when      %found(ticpi01l)
088400091217     c                   eval      viddspt='Spesa Trasporti      ' +
088500090520     c                                      hi + %trim(%editc(cpipft:'2')) +
088600090520     c                                     nm +'REALE   '
088700090521     c                   when      §cposptp>*zeros
088800091217     c                   eval      viddspt='Spesa Trasporti      ' +
088900090520     c                                      hi + %trim(%editc(§cposptp:'2')) +
089000090520     c                                     nm +'PRESUNTO'
089100090521     C                   ENDsl
089200140910
089300140910      /free
089400140910       //?Ds campo CPOTEL
089500140910         dcpo02 = CPOtel;
089600140910
089700140910       //?Anno Inizio Attività
089800140910         IF  §CPOannoia <> *blanks and §CPOannoia <> *zeros;
089900140910           vidannoia = §CPOannoia;
090000140910         ELSE;
090100140910           clear vidannoia;
090200140910         ENDIF;
090300140910      /end-free
090400130808      *
090500100319      * codice importanza cliente
090600100319     c                   eval      vidcic = cpoftr
090700090518     c
090800120703      * Data primo contatto da CPORST
090900120703     c                   IF        §CPOdtpcon > *zeros
091000120703     c                   eval      g08inv = %dec(§CPOdtpcon:8:0)
091100120703     c                   clear                   g08dat
091200120703     c                   eval      g08err = '3'
091300120703     c                   call      'XSRDA8'
091400120703     c                   parm                    wlbda8
091500120703     c                   eval      VIDdpc = g08dat
091600120703     c                   ELSE
091700120703     c                   clear                   VIDdpc
091800120703     c                   ENDIF
091900981012      *
092000981012      * DATA ACQUISIZIONE
092100981022     C                   Z-ADD     CPODAQ        G08INV
092200981022     C                   MOVE      *ZEROS        G08DAT
092300981022     C                   MOVEL     3             G08ERR
092400981022     C                   CALL      'XSRDA8'
092500981022     C                   PARM                    WLBDA8
092600981022     C                   Z-ADD     G08DAT        VIDDAC
092700981012     C**
092800981012     C                   MOVE      CPOFLT        VIDFLT
092900981014     C     CPOFLT        CHAIN     AZORG01L                           30
093000981014     C  N30              MOVEL     ORGDES        DESFLT
093100981127     C                   MOVEL     CPOFLT        OLDFLT
093200981120     C**
093300981120     C* VEDO SE CI SONO INFO COMMERCIALI
093400091029     C     cpocpo        CHAIN     TICPI01L                           31
093500981120    3C     *IN31         DOWEQ     *OFF
093600981120     C     CPIATB        ANDNE     ' '
093700091029     C     cpocpo        READE     TICPI01L                               31
093800981120    3C                   ENDDO
093900981120     C* 31 ON - NON CI SONO
094000981120     C     *IN31         IFEQ      *ON
094100091028     C                   SETOff                                       11
094200981120     C                   ELSE
094300091028     C                   SETOn                                        11
094400981120     C                   ENDIF
094500110203      * Decodifico la categoria del potenziale
094600110203     c                   clear                   dcpo
094700110203     c                   clear                   tibs02ds
094800110203     c                   eval      t02mod = 'C'
094900110203     c                   eval      t02cod = 'CPO'
095000110203     c                   eval      t02ke1 = CPOfls
095100110203     c                   eval      t02sif = knsif
095200110203     c                   call      'TIBS02R'
095300110203     c                   parm                    kpjba
095400110203     c                   parm                    tibs02ds
095500110203     c                   if        t02err = *blanks
095600110203     c                   eval      dcpo = t02uni
095700110203     c                   endif
095800110203      * imposto la descrizione
095900110503     c                   eval      viddfls = §cpodesc
096000110203     c                   eval      *in12 = (CPOfls <> 'M')
096100140910
096200140910      /free
096300140910       //?Carico la nota DP dettaglio prodotti
096400140910         flag02 = 'P';
096500140910         clipot = %editc(CPOcpo:'X');
096600140910         ktnt = 'DP';
096700140910         chain (flag02:clipot:knk2:ktnt) TFNTC01L;
096800140910         IF  %found(TFNTC01L);
096900140910           viddetp = ntcrnt;
097000140910         ELSE;
097100140910           clear viddetp;
097200140910         ENDIF;
097300140910      /end-free
097400140910
097500981216     C**
097600981012     C                   ENDSR
097700941111     C*------------------------------------------------------------------------*
097800941125     C*  CHKIND - CONTROLLO INDIRIZZO COMPLETO
097900941111     C*------------------------------------------------------------------------*
098000941125     C     CHKIND        BEGSR
098100941111     C*
098200941111     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
098300970711     C                   MOVEL     VIDRAG        I14RSC
098400970711     C                   MOVE      *BLANKS       I14RS2
098500970711     C                   MOVEL     VIDVIA        I14IND
098600970711     C                   MOVEL     VIDCIT        E14LOC
098700970711     C                   CLEAR                   E14PIP
098800970711     C                   MOVEL     VIDPRV        E14PRV
098900981012     C                   MOVEL     VIDNAZ        E14NAR
099000970711     C                   MOVEL     VIDCAP        E14CAP
099100970711     C*
099200970711     C                   CLEAR                   DSLV13
099300970711     C                   CLEAR                   DSSI95
099400970711     C                   CLEAR                   I14ISO
099500970711     C                   MOVEL     DATEU         I14DRI
099600970711     C*
099700970711     C                   CALL      'FNLV14R'
099800970711     C                   PARM                    KPJBA
099900970711     C                   PARM                    DSLV14
100000970711     C*
100100970711     C                   MOVEL     O14ERR        WERR              1
100200970711     C                   MOVEL     O14MSG        VIDMSG
100300970711     C                   EXSR      CTRERR
100400981012     C** ERRORE
100500981012     C     *IN90         IFEQ      *ON
100600970711     C                   GOTO      ENDC00
100700970711     C                   ENDIF
100800970711     C*************
100900970711     C* CONTROLLO CAP
101000970711     C                   MOVEL     '7'           I95TCN
101100970711     C                   MOVEL     'S'           I13LA3
101200970711     C                   MOVEL     'S'           I13SZ3
101300970711     C                   MOVEL     E14CAP        I95CAP
101400970711     C                   MOVEL     E14LOC        I95LOC
101500970711     C                   MOVEL     E14PRV        I95PRV
101600970711     C                   MOVEL     E14NAR        I95NAR
101700970711     C                   MOVEL     DATEU         I95DAT
101800970711     C                   MOVEL     'S'           I13AF0
101900970711     C                   MOVEL     'S'           I13CNV
102000970711     C                   MOVEL     'S'           I13AF1
102100970711     C**  SE CAMBIATI DATI CONTROLLO SE CONGRUENTI: ERRORE FORZABILE
102200970711    2C     E14CAP        IFNE      VARCAP
102300970711     C     E14LOC        ORNE      VARLOC
102400970711     C     E14PRV        ORNE      VARPRV
102500970711     C     E14NAR        ORNE      VARNAR
102600970711     C                   CLEAR                   WZ3
102700970711     C                   MOVEL     E14CAP        VARCAP
102800970711     C                   MOVEL     E14LOC        VARLOC
102900970711     C                   MOVEL     E14PRV        VARPRV
103000970711     C                   MOVEL     E14NAR        VARNAR
103100970711    2C                   ENDIF
103200970711     C                   MOVEL     WZ3           E13FZ3
103300970711     C**
103400970711     C** CALL
103500970711     C                   CALL      'FNLV13R'
103600970711     C                   PARM                    KPJBA
103700970711     C                   PARM                    DSLV13
103800970711     C                   PARM                    DSSI95
103900970711     C*
104000970711     C                   MOVEL     O13ERR        WERR              1
104100970711     C                   MOVEL     O13MSG        VIDMSG
104200970711     C                   EXSR      CTRERR
104300970711     C**
104400970711     C                   MOVEL     E13FZ3        WZ3
104500020826      **
104600981012     C** ERRORE
104700981012     C     *IN90         IFEQ      *ON
104800970711     C                   GOTO      ENDC00
104900970711     C                   ENDIF
105000970711     C*
105100020906      **
105200041207     C     dutPOU        CHAIN     AZORG01L                           30
105300020906     C   30              CLEAR                   ORGDIT
105400970711     C* SE LA FIL.APP.  E' VUOTA LA PROPONGO E RIEMETTO IL VIDEO
105500970711     C     VIDFLT        IFEQ      *BLANKS
105600970711     C     VIDFLT        OREQ      *ZEROS
105700090626     C* SE SONO una messaggerie, propongo la seconda linea distribuzione
105800090626     C     dutntw        IFEQ      'MES'
105900981103     C     SIMFEL        ANDNE     46
106000981103     C     O95CLO        ANDGT     0
106100981103     C                   MOVEL     O95CLO        VIDFLT
106200981103     C                   ELSE
106300981103     C                   MOVEL     O95LNA        VIDFLT
106400981103     C                   ENDIF
106500981117     C                   MOVEL     VIDFLT        NUMFLT
106600981103     C**
106700981117     C     NUMFLT        CHAIN     AZORG01L                           30
106800981014     C  N30              MOVEL     ORGDES        DESFLT
106900981012     C                   SETON                                        90
107000970711     C                   ENDIF
107100941125     C*
107200970711     C     ENDC00        TAG
107300970711     C* IMPOSTO I CAMPI DI UNA EVENTUALE RICERCA
107400970711     C                   MOVEL     E14LOC        VIDCIT
107500970711     C                   MOVEL     E14CAP        VIDCAP
107600970711     C                   MOVEL     E14PRV        VIDPRV
107700970711     C                   ENDSR
107800970711     C**************************************************************************
107900981012     C* CONTROLLO SE C'E' ERRORE
108000970711     C**************************************************************************
108100970711     C     CTRERR        BEGSR
108200970711     C* IMPOSTO CAMPI PASSATI
108300970711    1C     O13RON        IFEQ      'S'
108400970711     C                   MOVEL     O95NAR        E14NAR
108500970711    1C                   ENDIF
108600970711    1C     O13ROC        IFEQ      'S'
108700970711     C                   MOVEL     O95CAP        E14CAP
108800970711    1C                   ENDIF
108900970711    1C     O13ROP        IFEQ      'S'
109000970711     C                   MOVEL     O95PRV        E14PRV
109100970711    1C                   ENDIF
109200970711    1C     O13ROL        IFEQ      'S'
109300970711     C                   MOVEL     O95LOC        E14LOC
109400970711    1C                   ENDIF
109500970711     C* SE RITORNA CODICE DI ERRORE LO EMETTO A VIDEO
109600970711    1C     WERR          IFNE      ' '
109700970711    2C     VIDMSG        IFNE      *BLANKS
109800981012     C                   SETON                                        28
109900970711    2C                   END
110000970711     C* POSIZIONAMENTO CURSORE
110100970711    2C                   SELECT
110200970711     C     WERR          WHENEQ    '1'
110300981012     C                   SETON                                        40
110400970711     C     WERR          WHENEQ    '2'
110500981012     C                   SETON                                        41
110600970711     C     WERR          WHENEQ    '3'
110700981012     C                   SETON                                        42
110800970711     C     WERR          WHENEQ    '4'
110900981012     C                   SETON                                        43
111000970711     C     WERR          WHENEQ    '5'
111100981012     C                   SETON                                        44
111200981012     C     WERR          WHENEQ    '6'
111300981012     C                   SETON                                        45
111400970711    2C                   ENDSL
111500970711     C*
111600981012     C                   SETON                                        90
111700970711    1C                   ENDIF
111800970711     C                   ENDSR
111900981012     C*---------------------------------------------------------------
112000981012     C* CONTROLLO E DECODIFICO CODICE COMMERICIALE
112100981012     C*---------------------------------------------------------------
112200981012     C     CTRCMM        BEGSR
112300981012     C* RICERCA
112400101202     c                   if        %scan('?':vidcmm) > 0
112500010620     C                   MOVE      *BLANKS       VIDELE
112600130808     c                   clear                   TRMK43ds
112700130808     C                   CALL      'TRMK43R'
112800010620     C                   PARM                    KPJBA
112900130808     C                   PARM                    TRMK43ds
113000130808     c                   if        oMK43err = *off  and  oMK43fxx = *blank
113100130808     C                   MOVEL     oMK43cmm      VIDELE           10
113200130808     C                   MOVEL     oMK43des      DESELE           30
113300130808     c                   endif
113400101202     c                   eval      vidcmm = VIDELE
113500101202     c                   eval      descmm = DESELE
113600101202     c                   endif
113700981012     C* CONTROLLI
113800130808     c                   If        %check(c_Digits:VIDcmm) > *zeros
113900130808     c                   seton                                        289050
114000130808     c                   movel     Msg(20)       VIDmsg
114100130808     c                   leavesr
114200130808     c                   EndIf
114300101202     c                   if        vidcmm > *zeros
114400130808     c                   move      vidcmm        CMMcod
114500130808     C     CMMcod        CHAIN     AZCMM000
114600130808     c                   IF        %found(AZCMM01L)
114700130808     c                   eval      descmm = CMMdes
114800981012     C                   ELSE
114900981012     C                   SETON                                        289050
115000981012     C                   MOVEL     MSG(20)       VIDMSG
115100981012     C                   ENDIF
115200981012     C                   ENDIF
115300130808      *
115400981012     C                   ENDSR
115500981012      *-------------------------------------------------------------------------
115600080205      *CONTROLLO CATEGORIA merceologica
115700981012      *-------------------------------------------------------------------------
115800981012     C     CTRSCT        BEGSR
115900981012     C*
116000120703     C                   CLEAR                   WDES             40
116100981012     C                   MOVE      '1L'          COD
116200981012     C     '?'           SCAN      KEY                                    90
116300981012     C* RICERCA
116400981012     C     *IN90         IFEQ      '1'
116500981012     C                   EXSR      CERTAB
116600981012     C                   MOVEL(P)  VIDELE        KEY
116700981012     C                   MOVEL     DESELE        WDES
116800981014     C                   SETON                                        48
116900981012     C                   ELSE
117000981012     C* CONTROLLI
117100080207     c                   clear                   ds1l
117200091029     C     KTAB          CHAIN     TABEL                              30
117300981014     C     *IN30         IFEQ      *OFF
117400981026     C     TBLFLG        ANDEQ     ' '
117500080207     c                   movel     tbluni        ds1l
117600080207     C                   MOVEL     §1ldes        WDES
117700080207     c                   if        §1luti='N'
117800080207     C                   SETON                                        289048
117900080207     C                   MOVEL     MSG(25)       VIDMSG
118000080207     c                   endif
118100981012     C                   ELSE
118200981012     C                   SETON                                        289048
118300981012     C                   MOVEL     MSG(25)       VIDMSG
118400981012     C                   ENDIF
118500981012     C                   ENDIF
118600981012     C                   ENDSR
118700920416      *-------------------------------------------------------------------------
118800920416      *PULIZIA CAMPI FILE VIDEO
118900940317      *-------------------------------------------------------------------------
119000920416     C     AZZERO        BEGSR
119100970711     C                   MOVE      0             WZ3               1 0
119200920416     C                   Z-ADD     *ZEROS        VIDCP1
119300920416     C                   Z-ADD     *ZEROS        VIDCP2
119400920416     C                   MOVE      *BLANKS       VIDRAG
119500920416     C                   MOVE      *BLANKS       VIDVIA
119600920416     C                   MOVE      *BLANKS       VIDCIT
119700981014     C                   MOVEL     *BLANKS       VIDCAP
119800920416     C                   MOVE      *BLANKS       VIDPRV
119900981009     C                   MOVE      *BLANKS       VIDNAZ
120000920416     C                   MOVE      *BLANKS       VIDTEL
120100981009     C                   MOVE      *BLANKS       VIDFAX
120200920416     C                   MOVE      *BLANKS       VIDSCT
120300920416     C                   MOVE      *BLANKS       DESSCT
120400920416     C                   MOVE      *BLANKS       VIDCMM
120500920416     C                   MOVE      *BLANKS       DESCMM
120600920427     C                   MOVE      *BLANKS       VIDFLT
120700920421     C                   MOVE      *BLANKS       DESFLT
120800940321     C                   CLEAR                   UNO
120900940321     C                   CLEAR                   SAVFLT
121000981117     C                   CLEAR                   WDIVAS
121100050518     c                   Clear                   vidrsp1
121200081010     c                   eval      vidfsf='SEDE'
121300140910     c                   clear                   viddetp
121400140910     c                   clear                   vidannoia
121500140910
121600080205     c                   ENDSR
121700981012     C*---------------------------------------------------------------
121800981012     C* CONTROLLO 2 VIDEATA
121900981012     C*---------------------------------------------------------------
122000981012     C     CTRD02        BEGSR
122100981124     C** CONTROLLO SOLO IN MANUTENZIONE
122200981124    0C     *IN02         IFEQ      *OFF
122300981012     C**
122400981012     C** INDIRIZZO
122500981012     C**
122600981012     C* CONTROLLO INDIRIZZO COMPLETO
122700981012     C                   EXSR      CHKIND
122800981012     C   90              GOTO      ENDCT2
122900981012     C* TELEFONO
123000100524      * non controllo se F22=Richiesta contatto
123100981012     C     VIDTEL        IFEQ      *BLANKS
123200100524     c     *inkw         andeq     '0'
123300981012     C                   SETON                                        289046
123400981012     C                   MOVEL     MSG(24)       VIDMSG
123500981012     C                   GOTO      ENDCT2
123600981012     C                   ENDIF
123700981012     C**
123800100524     c                   if        not *inkw
123900981012     C                   CLEAR                   TRUL42
124000981014     C                   MOVEL     VIDTEL        D42FAX
124100981012     C                   CALL      'TRUL42R'
124200981012     C                   PARM                    TRUL42
124300981012     C     D42ERR        IFEQ      '1'
124400981012     C                   SETON                                        289046
124500981012     C                   MOVEL     D42MSG        VIDMSG
124600981012     C                   GOTO      ENDCT2
124700981012     C                   END
124800100407     c                   endif
124900981012     C** FAX SOLO SE C'E'
125000981012     C     VIDFAX        IFNE      *BLANKS
125100981012     C                   CLEAR                   TRUL42
125200981012     C                   MOVEL     VIDFAX        D42FAX
125300981012     C                   CALL      'TRUL42R'
125400981012     C                   PARM                    TRUL42
125500981012     C     D42ERR        IFEQ      '1'
125600981012     C                   SETON                                        289047
125700981012     C                   MOVEL     D42MSG        VIDMSG
125800981012     C                   GOTO      ENDCT2
125900981012     C                   END
126000981012     C                   END
126100080623     c**
126200100922     c** Falg sede filiale
126300080623     c**
126400080623     c* controllo solo se cambiato
126500080623     c                   if        %subst(vidfsf:1:1)<>cpofsf
126600080623     c* Se presente codice duns o dati azienda, non modificabile
126700081105     c                   if        cpoduns<>*blanks or cpoftaz>0
126800100922     c* l'utente deve essere autorizzato alla modifica
126900100922     c**                 if        simfel<>46 or (knmus<>'SEG019 ' and
127000100922     c**                           knmus<>'SEG013 ' and %subst(knmus:1:3)<>
127100100922     c**                           'EDP')
127200100922     c*
127300100922     c                   if        §utepotmsf<>'S' and  %subst(knmus:1:3)<>'EDP'
127400080623     C                   SETON                                        672890
127500080623     c                   if        cpofsf='S'
127600080623     C                   MOVEL     MSG(38)       VIDMSG
127700080623     c                   else
127800080623     C                   MOVEL     MSG(39)       VIDMSG
127900080623     c                   endif
128000080623     C                   GOTO      ENDCT2
128100080623    3C                   ENDIF
128200080623     c                   endif
128300081105     c                   endif
128400130808     c*
128500080205     C** CODICE FISCALE
128600080206     c* reimposto campi per forzatura e partita aiva in base allo stato
128700080206     c                   if        vidnaz<>savnaz
128800080206     c                   eval      wforzacdf = *off
128900080206     c                   eval      wforzaiva1 = *off
129000080206     c                   eval      savnaz=vidnaz
129100080206     c                   endif
129200080205     c                   exsr      sr_ctrcdf
129300080205     C   28              goto      endct2
129400981012     C**
129500080205     C** Partita Iva
129600080205     c                   exsr      sr_Ctriva
129700080205     C   28              goto      endct2
129800130808      *
129900981117     C** P.O. DI APPARTENENZA
130000981012     C**
130100981012      *RICERCA
130200981014     C     '?'           SCAN      VIDFLT                                 90
130300981014    1C     *IN90         IFEQ      '1'
130400981012     C                   EXSR      SELFIL
130500981014     C                   SETON                                            49
130600981012     C                   GOTO      ENDCT2
130700981012    1C                   ENDIF
130800981012     C* CONTROLLO
130900981012     C                   MOVE      VIDFLT        NUMFLT
131000981014     C     NUMFLT        CHAIN     AZORG01L                           30
131100981012     C* ERRATA
131200981014    1C     *IN30         IFEQ      *ON
131300981012     C     ORGFVA        ORNE      ' '
131400981012     C                   MOVEL     MSG(26)       VIDMSG
131500981012     C                   SETON                                        492890
131600981012     C                   GOTO      ENDCT2
131700981012    1C                   ENDIF
131800981014     C                   MOVEL     ORGDES        DESFLT
131900981012     C**
132000981027     C* P.O. ELABORATORE FIL TRASMISSIONE
132100981117    1C     SAVFLT        IFNE      NUMFLT
132200981117     C                   MOVEL     ' '           UNO               1
132300981117     C                   CLEAR                   WDIVAS
132400981117     C                   MOVEL     NUMFLT        SAVFLT
132500981117    1C                   ENDIF
132600981012     C*
132700090626      *Controllo validità  in base alle abilitazioni utente
132800090626     c                   clear                   tntaa1ds
132900090626     c                   eval      itaa1fil =numflt
133000090626     c                   eval      itaa1caut='§UTEPOT'
133100090626     c                   clear                   kpjbu
133200090626     c                   movel     tntaa1ds      kpjbu
133300090703     c                   call      'TNTAA1C'
133400090626     c                   parm                    kpjba
133500090626     c                   movel     kpjbu         tntaa1ds
133600090626     c
133700090626     c* Se non abilitato errore forzabile
133800090626     c                   if        otaa1fabi='N'
133900090626     c                             and  wdivas=' '
134000090626     C                   MOVEL     MSG(27)       VIDMSG
134100090626     C                   SETON                                        492890
134200090626     C                   MOVEL     'S'           WDIVAS            1
134300090626     C                   GOTO      ENDCT2
134400090626    1C                   ENDIF
134500090626     c
134600090626     c
134700981103     C* INCONGRUENZA COL CAP--> LNA STD O LNA OLTRE
134800981012    1C     NUMFLT        IFNE      O95LNA
134900981103    1C     NUMFLT        ANDNE     O95CLO
135000981012     C     UNO           ANDNE     '1'
135100981012     C                   SETON                                        492890
135200981012     C                   MOVEL     '1'           UNO
135300981012     C                   MOVEL     MSG(28)       VIDMSG
135400981012     C                   GOTO      ENDCT2
135500981012    1C                   ENDIF
135600981012     C*
135700981014     C                   MOVEL     ORGDES        DESFLT
135800981012     C**
135900080205     C** CATEGORIA merceologica
136000981012     C**
136100981012     C                   MOVEL(P)  VIDSCT        KEY
136200981012     C                   EXSR      CTRSCT
136300981012     C                   MOVEL     KEY           VIDSCT
136400981012     C                   MOVEL     WDES          DESSCT
136500981012     C   90              GOTO      ENDCT2
136600981012     C**
136700981012     C** CODICE COMMERCIALE
136800981012     C**
136900981026     C** OBBLIGATORIO PER STATO > 0 MA NON CALCOLO PIU' IN AUTOMATICO
137000981012     C** LEGGO L'ULTIMO STATO IMPOSTATO: SE 0 NON OBBLIGATORIO
137100981026     C**  SE IMMESSO A VIDEO L'ULTIMO STATO E' QUESTO
137200981012    1C     VIDCMM        IFEQ      *ZEROS
137300981012     C     VIDCMM        OREQ      *BLANKS
137400981027     C**
137500981027     C* SE NEL FILE C'ERA--> NON E' PIU' POSSIBILE CANCELLARLO
137600981104     C  N10CPOCMM        IFNE      *ZEROS
137700981027     C                   SETON                                        502890
137800981027     C                   MOVEL     MSG(29)       VIDMSG
137900981027     C                   GOTO      ENDCT2
138000981027    2C                   ENDIF
138100981111     C**
138200981111   X1C                   ELSE
138300981012     C**
138400981012     C                   EXSR      CTRCMM
138500981012     C   90              GOTO      ENDCT2
138600981027     C**
138700981027     C                   MOVEL     VIDCMM        W0030             3 0
138800981027     C     W0030         CHAIN     AZORG01L                           30
138900021029     C   30              MOVEL     W0030         ORGFEL
139000981027     C   30              CLEAR                   ORGDIT
139100981012     C**
139200130808      * NON può avere il flag di particolarità inserito in tab 01
139300130808     c                   if        CMMpar <> *blanks
139400981012     C                   SETON                                        502890
139500981012     C                   MOVEL     MSG(30)       VIDMSG
139600981012     C                   GOTO      ENDCT2
139700981027    2C                   ENDIF
139800111123
139900111123      /free
140000111123       //?Commerciale "valido" data inizio e fine attività
140100130808         IF  CMMdtIni > DateU or CMMdtFin < DateU;
140200111123           *in28 = *on;
140300111123           *in50 = *on;
140400111123           *in90 = *on;
140500111123           Vidmsg = msg(30);
140600111123           leavesr;
140700111123         ENDIF;
140800111123      /end-free
140900981112     C**
141000090626     c* se Diversi e l'utente è abilitato alla filiale  appartenenza
141100090626     c*              l'utente deve essere abilitato anche alla fil.mmerciale
141200130808     c*
141300981117    2C     W0030         IFNE      NUMFLT
141400130808     c*
141500981117    3C     WDIVAS        IFEQ      ' '
141600090626     c*
141700090626     c* Visto che qui non devo controllare l'unificante, passo la filiale
141800090626     c*  del commerciale per il controllo delle abilitazioni
141900090626     c                   clear                   tntaa1ds
142000090626     c                   eval      itaa1fil =w0030
142100090626     c                   eval      itaa1caut='§UTEPOT'
142200090626     c                   clear                   kpjbu
142300090626     c                   movel     tntaa1ds      kpjbu
142400090703     c                   call      'TNTAA1C'
142500090626     c                   parm                    kpjba
142600090626     c                   movel     kpjbu         tntaa1ds
142700130808     c*
142800090626    4c                   if        otaa1fabi='N'
142900981012     C                   SETON                                        502890
143000981012     C                   MOVEL     MSG(31)       VIDMSG
143100981012     C                   GOTO      ENDCT2
143200090626    4C                   ENDIF
143300090626    3C                   ENDIF
143400981117     C*
143500090626     C* SE l'utente non è abilitato alla filiale, devono essere per forza uguali
143600981117    3C     WDIVAS        IFEQ      'S'
143700981117     C                   SETON                                        502890
143800981117     C                   MOVEL     MSG(14)       VIDMSG
143900981117     C                   GOTO      ENDCT2
144000981117    3C                   ENDIF
144100981117    2C                   ENDIF
144200130808     c*
144300981012    1C                   ENDIF
144400091028    0C                   ENDIF
144500981012     C*
144600981012     C     ENDCT2        ENDSR
144700060330
144800920427     C***------------------------------------------------------------
144900920427     C** CARICAMENTO SCHIERA FIL
145000920427     C***------------------------------------------------------------
145100920427     C     CARTAB        BEGSR
145200940318     C*
145300970711     C                   TIME                    W0140            14 0
145400970711     C* UDATE IN GGMMAAAA
145500970711     C                   MOVE      W0140         WDTGIO            8 0
145600970711     C*
145700970711     C* UDATE IN AAAAMMGG
145800970714     C                   Z-ADD     WDTGIO        G08DAT
145900970714     C                   MOVEL     *BLANK        G08ERR
146000970711     C                   CALL      'XSRDA8'
146100970714     C                   PARM                    WLBDA8
146200970711     C                   MOVEL     G08INV        DATEU             8 0
146300050518     c                   Move      g08inv        data6             6 0
146400971203     C**
146500981103     C                   MOVEL     G08DAT        GMADAT            8 0
146600981012     C                   SETOFF                                       212223
146700981014     C                   SETOFF                                       242526
146800981012     C                   SETOFF                                       27
146900920430     C*
147000940321     C                   ENDSR
147100920427      *________________________________________________________________________
147200920427      *SELEZIONE FILIALE DI APPARTENENZA TRAMITE '?'
147300940317      *________________________________________________________________________
147400920427     C     SELFIL        BEGSR
147500941125     C                   CALL      'TNSD19R'
147600920427     C                   PARM                    §CODF             3
147700920427     C                   PARM                    §TIP              1
147800920427     C                   PARM                    §§DES            25
147900920427     C                   MOVEL     §CODF         VIDFLT
148000920427     C                   MOVEL     §§DES         DESFLT
148100920427     C                   ENDSR
148200940321      *-------------------------------------------------------------------------
148300960605      *AGGIORNO FILES
148400940321      *-------------------------------------------------------------------------
148500981014     C     AGGIOR        BEGSR
148600940321      *
148700940321     C                   MOVEL     VIDCP1        CPOCPO
148800940321     C                   MOVE      VIDCP2        CPOCPO
148900140714     c                   eval      CPO1cpo = CPOcpo
149000940321     C                   MOVE      VIDRAG        CPORAG
149100940321     C                   MOVE      VIDVIA        CPOVIA
149200940321     C                   MOVE      VIDCIT        CPOCIT
149300981014     C                   MOVEL     VIDCAP        CPOCAP
149400940321     C                   MOVE      VIDPRV        CPOPRV
149500981014     C                   MOVE      VIDNAZ        CPONAZ
149600140714     C                   MOVE      VIDTEL        CPO1TEL
149700140714     C                   MOVE      VIDFAX        CPO1FAX
149800080205     c                   eval      cpocdf=vidcdf
149900080205     c                   if        vidiva<>*blanks
150000080205     c                   if        vidivaeu=*blanks
150100080206     c                   movel     vidivait      cpopiv
150200080205     c                   else
150300080206     c                   movel     vidiva        cpopiv
150400080205     c                   endif
150500080205     c                   else
150600080206     c                   clear                   cpopiv
150700080205     c                   endif
150800080623     C                   MOVEL     VIDfsf        CPOfsf
150900080623     C                   MOVEL     VIDSCT        CPOSCT
151000940321     C                   MOVEL     VIDCMM        CPOCMM
151100100319     c                   eval      cpoftr = vidcic
151200981014     C* IMMISSIONE PRENDO  UDATE=DATA ACQUISIZIONE CLIENTE
151300981014££££ C   10              Z-ADD     DATEU         CPODAQ
151400940321     C                   MOVE      VIDFLT        CPOFLT
151500040805     c                   Eval      CpoDtr = dateu
151600010504      *
151700060714      * se non sono in annullamento devo reperire la nuova provincia dalla tabella PR e
151800060714      * memorizzarla sul file
151900080414     c***                if        not *inkq and savprv <> cpoprv
152000080414     c                   if        savprv <> cpoprv
152100060714     c                   clear                   dspr
152200060714     c                   eval      cod = 'PR'
152300060714     c                   eval      key = cpoprv
152400060714     c     ktab          chain     tabel00f
152500060714     c                   if        %found(tabel00f) and tblflg = *blanks
152600060714     c                   eval      dspr = tbluni
152700060714     c                   endif
152800060714     c                   if        §prnew <> *blanks
152900060714     c                   eval      cpoprv = §prnew
153000060714     c                   endif
153100060714     c                   endif
153200010301      * Aggiorno archivio potenziali
153300981127    1C     *IN10         IFEQ      *ON
153400080206     c                   clear                   CPOFTAZ
153500080206     c                   clear                   CPOAFAZ
153600080206     c                   clear                   CPOFFAZ
153700080206     c                   clear                   CPOPEXP
153800140930     c                   clear                   dcpo01
153900140930     c                   clear                   dcpo02
154000140930     c                   eval      cporst = dcpo01
154100110316     c                   eval      cpofls = 'M'
154200140930     c                   eval      §CPOannoia = *all'0'
154300140930     c                   eval      CPOtel = dcpo02
154400981014     C                   WRITE     TNCPO000
154500140714     C                   WRITE     TNCPO100
154600160705
154700160705      /free
154800160705       //?Scrivo Storico Variazioni
154900160705         exsr ScriviVar;
155000160705      /end-free
155100160705
155200090522     c*
155300140929     c******             movel     cpocpo        parcpo
155400140929     c******             call      'TRMK08R'
155500140929     c******             parm                    kpjba
155600140929     c******             parm                    parcpo           11
155700090522     c
155800940321   X1C                   ELSE
155900981014     C                   UPDATE    TNCPO000
156000140716     C  n13              UPDATE    TNCPO100
156100140716     C   13              write     TNCPO100
156200160705
156300160705      /free
156400160705       //?Scrivo Storico Variazioni
156500160705         exsr ScriviVar;
156600160705      /end-free
156700981127    1C                   ENDIF
156800140910
156900140910      /free
157000140910       //?Scrivo/aggiorno nota "DP"
157100140910           flag02 = 'P';
157200140910           clipot = %editc(CPOcpo:'X');
157300140910           ktnt = 'DP';
157400140910           chain (flag02:clipot:knk2:ktnt) TFNTC01L;
157500140910           IF  viddetp <> *blanks;
157600140910             IF  %found(TFNTC01L);
157700140910               NTCrnt = viddetp;
157800140910               NTCntr = data6;
157900140910               update TFNTC;
158000140910             ENDIF;
158100140910             IF  not %found(TFNTC01L);
158200140910               clear TFNTC;
158300140910               NTCapl = flag02;
158400140910               NTCnk1 = clipot;
158500140910               NTCtnt = ktnt;
158600140910               NTCrnt = viddetp;
158700140910               NTCsns = 'S';
158800140910               NTCntr = data6;
158900140910               write TFNTC;
159000140910             ENDIF;
159100140910           ELSE;
159200140910             IF  %found(TFNTC01L);
159300140910               delete TFNTC;
159400140910             ENDIF;
159500140910           ENDIF;
159600140910      /end-free
159700140910
159800130808      *
159900940321     C                   ENDSR
160000080205      *------------------------------------------------------------------------*
160100080205      * CONTROLLO CODICE FISCALE
160200080207      *------------------------------------------------------------------------*
160300080205     c     Sr_Ctrcdf     BegSr
160400080626     c                   movel     vidfsf        vid1fsf           1
160500130808      *
160600080205      * devo richiamare il nuovo driver fatto per controllare il codice
160700080205      * fiscale e la partita iva
160800151021     c                   clear                   xcfiva1ds
160900080205     c                   eval      xcfivapar = vidcdf
161000080205     c                   eval      xcfivanar = vidnaz
161100080205     c                   eval      xcfivaprv = vidprv
161200151021     c                   eval      xcfivaloc = vidcit
161300151021     c                   eval      xcfivacap = vidcap
161400151021     c                   eval      xcfivapiva = vidiva
161500151021     c                   call      'XCFIVAR1'
161600151021     c                   parm                    xcfiva1ds
161700080422     c
161800080207    2c                   if        xcfivaflg < *zeros
161900080205     c                   seton                                        562890
162000080422     c                   Eval      vidmsg = xcfivamsg
162100080205     c                   Leavesr
162200080207    2c                   EndIf
162300080422     c
162400080422      * Non inserito: Errore forzabile per clienti Italia se non previsto
162500080422      *     inserimento dai controlli xcfivar (xcfivaflg=9)
162600080422    1c                   if        vidcdf=*blanks and vidnaz=*blanks
162700080422     c                             and wforzacdf= *off  and xcfivaflg<>9
162800080422     c                   seton                                        562890
162900080422     c                   eval      vidmsg = msg(50)
163000080422     c                   eval      wforzacdf = *on
163100080422     c                   leavesr
163200080422    1c                   endif
163300080626     c
163400080206     c* lo stesso codice fiscale non deve essere già presente su altro
163500080206     c* potenziale sia come codice fiscale che come partita iva
163600080422    1c                   If        vidcdf <> *Blanks
163700130808      *
163800080626    2c                   if        (*in10 or vidcdf<>savcdf or
163900080626     c                              (vid1fsf<>cpofsf and vid1fsf='S'))
164000080207     c                             and vidcdf<>cod_forzcdf
164100080207     c                   eval      codice=vidcdf
164200080207     c                   exsr      sr_CdfAltroP
164300080207     c                   exsr      Sr_msgcdf
164400080207     c   90              leavesr
164500080626     c
164600080207    3c                   if        (vidivaeu=*blanks and vidcdf<>vidivait)
164700080207     c                             or (vidivaeu<>*blanks and vidcdf<>vidiva)
164800080207     c                   exsr      sr_PivAltroP
164900080207     c                   exsr      Sr_msgcdf
165000080207     c   90              Leavesr
165100080207    3c                   endif
165200080207    2c                   endif
165300080207    1c                   EndIf
165400130808      *
165500080205     c                   EndSr
165600130808      *
165700080205      *------------------------------------------------------------------------*
165800080205      * CONTROLLO PARTITA IVA E STATO
165900080205      *------------------------------------------------------------------------*
166000080205     c     Sr_Ctriva     BegSr
166100080626     c                   movel     vidfsf        vid1fsf           1
166200080205      * PARTITA IVA
166300080205      * Non inserita: Errore forzabile
166400080207    1c                   If        vidiva = *Blanks and wforzaiva=*off
166500080205     c                   seton                                        662890
166600080205     c                   eval      vidmsg = msg(51)
166700080205     c                   eval      wforzaiva = *on
166800080205     c                   leavesr
166900080207    1c                   EndIf
167000080205      * --> controllo
167100080207    1c                   If        vidiva <> *Blanks
167200151021     c                   clear                   xcfiva1ds
167300080205     c                   eval      xcfivamod = 'P'
167400080205     c                   eval      xcfivapar = vidiva
167500080205     c                   eval      xcfivanar = vidnaz
167600080205     c                   eval      xcfivaprv = vidprv
167700151021     c                   eval      xcfivaloc = vidcit
167800151021     c                   eval      xcfivacap = vidcap
167900151021     c                   eval      xcfivapiva = vidiva
168000151021     c                   call      'XCFIVAR1'
168100151021     c                   parm                    xcfiva1ds
168200080205      * --> errata forzabile
168300080207    2c                   If        xcfivaflg = -9 and wforzaiva1 = *off
168400080205     c                   seton                                        662890
168500080205     c                   Eval      vidmsg = xcfivamsg
168600080205     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
168700080205     c                   eval      wforzaiva1 = *on
168800080205     c                   Leavesr
168900080207    2c                   EndIf
169000080205      * --> errore
169100080207    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
169200080205     c                   seton                                        662890
169300080205     c                   eval      vidmsg = xcfivamsg
169400080205     c                   Leavesr
169500080207    2c                   EndIf
169600080205      * se non impostato devo riportare il codice iso della partita iva che
169700080205      * mi viene passato dal pgm di controllo
169800080207    2c                   if        vidivaeu <> ivaeu
169900080205     c                   eval      vidivaeu = ivaeu
170000080205     c                   eval      *in66 = *on
170100080205     c                   eval      *in90 = *on
170200080207    2c                   endif
170300080626     c
170400080207     c* la stessa partita Iva  non deve essere già presente su altro
170500080207     c* potenziale sia come partita iva che come codice fiscale
170600080626    2c                   if        (*in10 or vidiva<>saviva    or
170700080626     c                              (vid1fsf<>cpofsf and vid1fsf='S'))
170800080307     c                             and vidiva<>cod_forziva and vidivaeu<>'$$'
170900080208     c                   if        vidivaeu<>*blanks
171000080207     c                   eval      codice=vidiva
171100080208     c                   else
171200080208     c                   eval      codice=vidivait
171300080208     c                   endif
171400080207     c                   exsr      sr_PivAltroP
171500080207     c                   exsr      sr_msgiva
171600080207     c   90              leavesr
171700130808     c*
171800080207    3c                   if        (vidivaeu=*blanks and vidcdf<>vidivait)
171900080207     c                             or (vidivaeu<>*blanks and vidcdf<>vidiva)
172000080207     c                   exsr      sr_CdfAltroP
172100080207     c                   exsr      sr_msgiva
172200080207     c   90              leavesr
172300080207    3c                   endif
172400080207    2c                   endif
172500080207    1c                   EndIf
172600130808      *
172700080205     c                   EndSr
172800080207      *------------------------------------------------------------------------*
172900080207      * Controllo presenza codice su altro potenziale come cod.fiscale
173000080207      *------------------------------------------------------------------------*
173100080207     c     Sr_CdfAltroP  BegSr
173200080626     c                   clear                   savrag
173300080626     c                   clear                   savfsf
173400080207     c     codice        setll     tncpo05l
173500080207     c     codice        reade     tncpo05l
173600130808     c*
173700080229    3c                   dow       not %eof(tncpo05l)
173800080626     c*                                                                enz.
173900080626     c                   if        c_cpocpo<>cpocpo
174000080626     c                   if        savfsf<>'S'
174100080626     c                   eval      savfsf=c_cpofsf
174200080626     c                   eval      savrag=c_cporag
174300080626     c                   endif
174400080626     c                   endif
174500130808     c*
174600080207     c     codice        reade     tncpo05l
174700080207    3c                   enddo
174800080207     c                   endsr
174900080207      *------------------------------------------------------------------------*
175000080207      * Controllo presenza codice su altro potenziale come piva
175100080207      *------------------------------------------------------------------------*
175200080207     c     Sr_PivAltroP  BegSr
175300080207     c     codice        setll     tncpo06l
175400080207     c     codice        reade     tncpo06l
175500080229    3c                   dow       not %eof(tncpo06l)
175600080626     c*                                                                enz.
175700080626     c                   if        c_cpocpo<>cpocpo
175800080626     c                   if        savfsf<>'S'
175900080626     c                   eval      savfsf=c_cpofsf
176000080626     c                   eval      savrag=c_cporag
176100080626     c                   endif
176200080626     c                   endif
176300130808      *
176400080207     c     codice        reade     tncpo06l
176500080207    3c                   enddo
176600080207     c                   endsr
176700080207      *------------------------------------------------------------------------*
176800080207      * Messaggio di errore codice fiscal già presente per altro potenz
176900080207      *------------------------------------------------------------------------*
177000080207     c     Sr_msgcdf     BegSr
177100080626     c* Errore se trovato un altro potenziale con lo stesso cod.fiscale
177200080626    1c                   if        savrag<>*blanks
177300080626     c*  Se ne ho trovato uno di filiale o il pot aggiornato è di filiale
177400080626     c*                            --> msg forzabile
177500080626    2c                   if        savfsf='F' or vid1fsf='F'
177600080207     c                   eval      cod_forzcdf=codice
177700080207     c                   seton                                        562890
177800080207     c                   Eval      vidmsg = msg(1)
177900080626   x2c                   else
178000080626     c* se ne ho trovato uno di sede per pot di sede
178100080626     c*                            --> msg non forzabile
178200080626     c                   seton                                        562890
178300080626     c                   Eval      vidmsg = msg(16)
178400080626     c                   eval      %subst(vidmsg:19:15)=savrag
178500080626    2c                   endif
178600080626    1c                   endif
178700080207     c                   endsr
178800080207      *------------------------------------------------------------------------*
178900080207      * Messaggio di errore partita iva   già presente per altro potenz
179000080207      *------------------------------------------------------------------------*
179100080207     c     Sr_msgiva     BegSr
179200080626     c* Errore se trovato un altro potenziale con la stessa partita iva
179300080626    1c                   if        savrag<>*blanks
179400080626     c*  Se ne ho trovato uno di filiale o il pot aggiornato è di filiale
179500080626     c*                            --> msg forzabile
179600080626    2c                   if        savfsf='F' or vid1fsf='F'
179700080626     c                   eval      cod_forziva=vidiva
179800080626     c                   seton                                        662890
179900080626     c                   Eval      vidmsg = msg(2)
180000080626   x2c                   else
180100080626     c* se ne ho trovato uno di sede per pot di sede
180200080626     c*                            --> msg non forzabile
180300080626     c                   seton                                        662890
180400080626     c                   Eval      vidmsg = msg(42)
180500080626     c                   eval      %subst(vidmsg:19:15)=savrag
180600080626    2c                   endif
180700080626    1c                   endif
180800080207     c                   endsr
180900160705
181000160705      /free
181100160705       //--------------------------------------------------------------
181200160705       //?Scrivo l'immagine Precedente.
181300160705       //--------------------------------------------------------------
181400160705       BEGSR ImmaginePrima;
181500160705
181600160705         clear TRMK30DS;
181700160705         IMK30immag = 'P';
181800160831         IF  *in10;
181900160831           CPOrst = dCPO01;
182000160831         ENDIF;
182100160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
182200160705
182300160705       ENDSR;
182400160705       //--------------------------------------------------------------
182500160705       //?Scrivo Storico Variazioni.
182600160705       //--------------------------------------------------------------
182700160705       BEGSR ScriviVar;
182800160705
182900160705         clear TRMK30DS;
183000160705         IMK30pru = knmus;
183100160705         IMK30noj = knmeb;
183200160705         IMK30pgm = 'TRMK02R';
183300160705         IMK30immag = 'D';
183400160705
183500160705       //?Immissione
183600160705         IF  *in10;
183700160705           IMK30cta = 'I';
183800160705         ENDIF;
183900160705
184000160705       //?Manutenzione
184100160705         IF  not *in10;
184200160705           IMK30cta = 'M';
184300160705         ENDIF;
184400160705
184500160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
184600160705
184700160705       ENDSR;
184800160705      /end-free
184900160705
185000050629      *****************************************************************
185100050629      * ROUTINE INIZIALE
185200050629      *****************************************************************
185300050629     C     *INZSR        BEGSR
185400050629      *
185500050629     C     *ENTRY        PLIST
185600050629     C                   PARM                    KPJBA
185700091028     C                   PARM                    trmk01ds
185800100409     C                   PARM                    trmk19ds
185900130808      *
186000100409      * verifico se passati + parametri
186100100409     c                   If        %parms> 2
186200100409     c                   eval      W_daatti = 'S'
186300100409     c                   endif
186400130808      *
186500091028     C                   CLEAR                   mk01F12
186600091028     C                   CLEAR                   mk01F03
186700091028     C                   CLEAR                   mk01F06
186800091028     C                   CLEAR                   mk01ERR
186900091028     C                   CLEAR                   mk01M10
187000050629      *
187100050629     c     *dtaara       define    §azute        azuteds
187200050629     c     *dtaara       define    §datiute      ddatiute
187300050629     c                   in(E)     *dtaara
187400050629     c                   If        %error  or RSUT = *blanks
187500050629     c                   Clear                   Tibs34ds
187600050629     c                   Call      'TIBS34R'
187700050629     c                   Parm                    Tibs34ds
187800050629     c                   In        *dtaara
187900050629     c                   EndIf
188000100922     c
188100100922     c                   Movel     UteFaf        Dute01
188200130808      *
188300050629     C                   SETOFF                                       585957
188400050629      *
188500050629     C     SIMFEL        COMP      *ZERO                              8989
188600050629     C     SIMFEL        IFEQ      0
188700050629     C                   Z-ADD     046           SIMFEL
188800091029     C                   Endif
188900050629     C**
189000050629     C* CARICO TABELLE Ed ESEGUO OPERAZIONI INIZIALI
189100050629     C                   EXSR      CARTAB
189200050629     C                   MOVE      TES(1)        VIDTES
189300130808      *
189400050629     C                   EXSR      AZZERO
189500100202     C                   SETOFF                                       100302
189600140716     c                   setoff                                       13
189700130808      *
189800100202      * se primo campo kpjbu = 'S' è da menù
189900100202     c                   if        %subst(kpjbu:1:1) = 'S'
190000100202     c                   eval      *in03 = *on
190100100202     c                   endif
190200050629     C*
190300131126      * se secondo mpo kpjbu = 'S' è da menù
190400131126     c                   if        %subst(kpjbu:2:3) = 'LOG'
190500131126     c                   eval      $logistica = *on
190600131126     c                   endif
190700131126     C*
190800050629     C* SE C'E' IMPOSTO POTENZIALE PASSATO
190900091028     C     mk01K10       IFNE      ' '
191000091028     C     mk01K10       ANDNE     'I'
191100050629     C*
191200050629     C* 59 ON  - INIBISCO TASTI F7-VISITE / F16-ANNULLA / F12-RITORNO
191300091028     C     mk01K10       IFEQ      'A'
191400050629     C                   SETON                                        59
191500050629     C                   ENDIF
191600050629     C* IN INTERROGAZIONE SEMPRE CAMPI PROTETTI
191700091028     C     mk01K10       IFEQ      'N'
191800050629     C                   MOVE      TES(2)        VIDTES
191900091028     C                   SETON                                        02
192000050629     C                   ENDIF
192100050629     C*
192200091028     C                   MOVEL     mk01CPO       VIDCP1
192300091028     C                   MOVE      mk01CPO       VIDCP2
192400050629     C                   ENDIF
192500050629      *
192600050629     C* PARFLG  = "I" --> PROPONGO L'IMMISSIONE DEL POTENZIALE
192700091028     C     mk01K10       IFEQ      'I'
192800100120     C                   MOVEL     *ON           Immissione
192900050629     C                   ENDIF
193000130808      *
193100050629     C* DEFINIZIONE CHIAVI E CAMPI-------------------------------------*
193200050629     C     KTAB          KLIST
193300050629     C                   KFLD                    CODUT
193400050629     C                   KFLD                    COD               2
193500050629     C                   KFLD                    KEY               8
193600050629      *
193700050629     C     KTAB2         KLIST
193800050629     C                   KFLD                    CODUT
193900050629     C                   KFLD                    COD
194000050629      *
194100050629     C     KNOTE         KLIST
194200091028     C                   KFLD                    FLAG02            1
194300091028     C                   KFLD                    CLIPOT           11
194400130808      *
194500050629     c     knote01       klist
194600050629     c                   kfld                    flag02
194700050629     c                   kfld                    clipot
194800050629     c                   kfld                    knk2
194900050629     c                   kfld                    ktnt
195000130808      *
195100090518     C     Kcpi          klist
195200090518     C                   kfld                    CPocpo
195300090518     C                   kfld                    ktpf
195400130808      *
195500050629     C**
195600050629     C     *LIKE         DEFINE    TBLKUT        §KUT
195700050629     C     *LIKE         DEFINE    TBLCOD        §COD
195800050629     C     *LIKE         DEFINE    TBLKEY        §KEY
195900050629     C     *LIKE         DEFINE    CPOFLT        NUMFLT
196000050629     C     *LIKE         DEFINE    CPOFLT        SAVFLT
196100050629     C     *LIKE         DEFINE    VIDFLT        OLDFLT
196200050629     C     *LIKE         DEFINE    E14CAP        VARCAP
196300050629     C     *LIKE         DEFINE    E14LOC        VARLOC
196400050629     C     *LIKE         DEFINE    E14PRV        VARPRV
196500050629     C     *LIKE         DEFINE    E14NAR        VARNAR
196600130808      *
196700050629     C                   ENDSR
196800050629
196900920506      *------------------------------------------------------------------------
197000981130** TES
197100981123 *** GESTIONE  CLIENTI  POTENZIALI ***
197200981123 * INTERROGAZIONE CLIENTI POTENZIALI *
197300981123** ERR
197400080305Cod.fiscale già presente su altri potenziali. Enter per forzare               1
197500080305Partita Iva già presente su altri potenziali. Enter per forzare               2
197600960610Codice Potenziale non manutenzionabile perchè già in uso: riprovare più tardi!3
197700100319Immettere codice commerciale.                                                 4
197800100319                                                                              5
197900100319                                                                              6
198000100319                                                                              7
198100100319                                                                              8
198200100319                                                                              9
198300100319                                                                              10
198400100319                                                                              11
198500100319                                                                              12
198600100319                                                                              13
198700090626Se potenziale"ceduto"ad altra filiale,il COMMERCIALE deve appartenere alla fil
198800041207Potenziale non gestibile dall'utente                                          15
198900080626Presente pot.Sede"xxxxxxxxxxxxxxx"con stesso CodFiscale:modificare Cdf o Sd/Fi
199000981112Impossibile inserire un codice potenziale gia' esistente                      17
199100100319                                                                              18
199200981009Codice potenziale inesistente                                                 19
199300981012Codice commerciale inesistente                                                20
199400100319                                                                              21
199500100319                                                                              22
199600100319                                                                              23
199700981012Numero di telefono obbligatorio                                               24
199800080207Categoria merceologica inesistente o non utilizzabile                         25
199900090626Fil. di appartenenza errata                                                   26
200000090626Fil. di appartenenza immessa non in gestione all'utente. ENTER PER FORZARE    27
200100090626Fil. di appartenenza incongruente col CAP: ENTER per forzare                  28
200200100319Codice commerciale obbligatorio se gia'presente.                              29
200300981012Codice commerciale non utilizzabile                                           30
200400100319Codice commerciale non in gestione all'utente.                                31
200500100319                                                                              32
200600100319                                                                              33
200700100319                                                                              34
200800100319                                                                              35
200900100319                                                                              36
201000100319                                                                              37
201100080623Potenziale "S E D E" non modificabile                                         38
201200080623Potenziale " F I L I A L E" non modificabile                                  39
201300100319                                                                              40
201400100319                                                                              41
201500080626Presente pot.Sede"xxxxxxxxxxxxxxx"con stessa Par.IVA:modificare P.IVA o Sd/Fi 42
201600100319                                                                              43
201700100319                                                                              44
201800100319                                                                              45
201900050630Numeratore mancante : contattare EDP di sede!!!!                              46
202000100319                                                                              47
202100100319                                                                              48
202200100422Modifica non ammessa: la filiale xxx NON ha NUOVA GESTIONE TRATTATIVE attiva  49
202300100422Immettere Codice Fiscale . Enter per forzare                                  50
202400080205Immettere la Partita Iva. Enter per forzare                                   51
