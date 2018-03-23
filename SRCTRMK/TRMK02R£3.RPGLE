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
013900160708      * 19    : Abilito F19-Variazioni
014000981105      * 28    : EMISSIONE MESSAGGIO DI ERRORE
014100120703      * 40-56 : POSIZIONAMENTO CURSORE
014200981105      * 59    : PGM RICHIAMATO DA ANNULLAMENTO VISITA: INIBISCO SCELTA
014300981105      *          1-VISITE / F16-ANNULLA / F12-RITORNO
014400981111      * 60-64 : ERRORE ERRMSG IN VID5
014500120703      * 66    : Posizionamento cursore
014600120703      * 67    : Posizionamento cursore
014700100506      * 77    : F05 Pink - Attività
014800091028      * 78    : F02 Pink - Rubrica presente
014900091028      * 79    : F18 Pink - Note presenti
015000160706      * 80    : F19 Pink - Variazioni presenti
015100981012      * 89    : ON --> P.O. - OFF --> SEDE
015200981012      * 90    : ERRORE GENERICO
015300981012      *
015400920415     C*----------------------------------------------------------------*
015500941111     C* MAIN LINE
015600941111     C*----------------------------------------------------------------*
015700920415      *
015800981009     C**
015900981009     C                   MOVEL     VIDCP1        CPOCPO
016000981009     C                   MOVE      VIDCP2        CPOCPO
016100050629     C* CONTROLLI sul potenziale
016200050629     C                   EXSR      CTRcpo
016300130808      *
016400981009     C**
016500981009     C* SE IL POTENZIALE ESISTE CARICO I DATI
016600981009     C  N10              EXSR      CARICA
016700981014     C* PER IMMISSIONE NON CI POSSONO ESSERE STATI NELLO STORICO
016800091104     C   10              SETON                                        09
016900920612      *
017000091028      * Verifico sul file note se già ci sono note per il potenziale
017100091028      * Se ci Pink F18
017200091028     C                   setoff                                       79
017300091028     c     cpocpo        chain     ticpn01l
017400091028     c                   if        %found(ticpn01l)
017500091028     c                   eval      *in79 = *on
017600091028     c                   endif
017700091028      * Verifico sul TFNTC01L se già ci sono contatti per il potenziale
017800091028      * Se ci sono Pink tasto F2-Rubrica
017900920612     C                   MOVE      'P'           FLAG02
018000920612     C                   MOVEL     VIDCP1        CLIPOT
018100920612     C                   MOVE      VIDCP2        CLIPOT
018200091028     C                   setoff                                       78
018300010130     C     knote         setll     tfntc01l
018400010130     C                   do        *hival
018500091029     C     knote         reade     tfntc01l
018600010130     C                   if        %eof(tfntc01l)
018700010130     C                   leave
018800010130     C                   endif
018900090716     c                   if        ntcflt = 'A'
019000090716     c                   iter
019100090716     c                   endif
019200090716      * controllo se Nota o Contatto
019300090716     c                   clear                   ds1t
019400090716     c                   eval      cod = '1T'
019500090716     c                   eval      key = ntctnt
019600091029     c     ktab          chain     tabel00f
019700090716     c                   if        %found(tabel00f) and tblflg = ' '
019800090716     c                   eval      ds1t = tbluni
019900090716     c                   endif
020000090716      * Nota
020100090716     c                   if        §1trich = 'N'
020200091028     c                   iter
020300090716     c                   endif
020400090716      * Contatto
020500090716     c                   if        §1trich = 'C'
020600090716     c  n78              eval      *in78 = *on
020700091028      * esco dal ciclo, ne basta uno per Pink tasto F2=Rubrica
020800091028     C                   leave
020900090716     c                   endif
021000010130     C                   enddo
021100160706      *
021200160706      * Verifico se ci sono variazioni per il potenziale
021300160706      * Se sì Pink F19
021400160706     c                   eval      *in80 = *off
021500160706     c     CPOcpo        chain     TNCPVT1L
021600160706     c                   IF        %found(TNCPVT1L)
021700160706     c                   eval      *in80 = *on
021800160706     c                   ENDIF
021900981105      *
022000981014     C* IN INSERIMENTO DATA ACQUISIZIONE = DATA GIORNO
022100981014     C   10              Z-ADD     GMADAT        VIDDAC
022200981123     C* SE RICHIAMATO E PASSATTO MESSAGGIO DI ERRORE LO VISUALIZZO
022300091028     C     mk01K10       IFNE      ' '
022400091028     C     mk01K10       ANDNE     'I'
022500091028     C     mk01MSG       ANDNE     *BLANKS
022600091028     C                   MOVEL     mk01MSG       VIDMSG
022700981123     C                   SETON                                        28
022800981123     C                   ENDIF
022900940318     C*
023000981012     C     FOR02         TAG
023100100810     c                   eval      $inke = *off
023200050518      * carico il responsabile trasporti dalla nota '05'
023300050518     c                   Eval      conta = 1
023400050518     c                   Eval      flag02 = 'P'
023500050518     c                   Movel     vidcp1        clipot
023600050518     c                   Move      vidcp2        clipot
023700050518     c                   Eval      ktnt = '05'
023800050518     c     knote01       Setll     Tfntc01l
023900050518     c                   Do        *Hival
024000091029     c     knote01       Reade     Tfntc01l
024100050518     c                   If        %Eof(Tfntc01l)
024200050518     c                   Leave
024300050518     c                   EndIf
024400050518     c                   If        conta = 1
024500050518     c                   Eval      vidrsp1 = ntcrnt
024600050518     c                   Else
024700080213     c                   Eval      vidrsp1 = %trim(vidrsp1) + ', ' +
024800080205     c                             ntcrnt
024900050518     c                   EndIf
025000050518     c                   add       1             conta
025100050518     c                   EndDo
025200100528      *
025300120120      * - Abilitazione F5=Attività sempre se nel sistema informativo di filiale
025400120120      *                            solo in interrogazione se S.I. di sede
025500120120     c                   IF        %subst(KNSIF : 1 : 3) = 'FIL'
025600120120     c                   eval      *in05 = *on
025700120120     c                   ELSE
025800120120     c                   eval      *in05 = *in02
025900120120     c                   ENDIF
026000130808      *
026100120120      * - Abilitazione F22=Richiesta contatto se nel S.I. di filale
026200120120      * - e solo se cliente in categoria <> da codificato
026300120120     c                   IF        %subst(KNSIF : 1 : 3) = 'FIL' and
026400120120     c                             CPOfls <> 'C'
026500120120     c                   eval      *in06 = *on
026600110209     c                   ENDIF
026700131126      *
026800131126      * - Disattivo il tasto F22= Rischiesta contatto e tasto F5= Attivita
026900131126      * - Solo in interrogazione se richiamato dalla logistica
027000131126     c                   If        $logistica = *on
027100131126     c                   eval      *in05 = *in02
027200131126     c                   eval      *in06 = *off
027300131126     c                   Endif
027400130808      *
027500100617      * Controllo se ci sono attività
027600100506      * Pink F05-Attività
027700091028     c     cpocpo        chain     tiatc04l
027800091028     c                   if        %found(tiatc04l)
027900091028     c                   eval      *in77 = *on
028000091028     c                   else
028100091028     c                   eval      *in77 = *off
028200091028     c                   endif
028300160708      *
028400160708      * Abilito F19 solo per EDPMB - EDPGA - EDPND
028500160708     c                   eval      *in19 = *off
028600160708     c                   IF        knmus = 'EDPMB' or
028700160708     c                             knmus = 'EDPGA' or
028800160708     c                             knmus = 'EDPND'
028900160708     c                   eval      *in19 = *on
029000160708     c                   ENDIF
029100920612      *
029200120703     C                   EXFMT     MK02D01
029300941125     C*
029400941125     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN90)
029500941125     C                   CLEAR                   VIDMSG
029600941125     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
029700021127     C                   SETOFF                                       65
029800981012     C                   SETOFF                                       404142
029900981012     C                   SETOFF                                       434445
030000981012     C                   SETOFF                                       464748
030100981012     C                   SETOFF                                       495051
030200981012     C                   SETOFF                                       525354
030300981014     C                   SETOFF                                       902855
030400080205     C                   SETOFF                                       5666
030500981014     C* F3  - FINE
030600981109     C     *INKC         IFEQ      *ON
030700091028     C                   MOVEL     '1'           mk01F03
030800981109     C                   GOTO      FINE
030900981109     C                   ENDIF
031000981014     C* F12 - RITORNO
031100050629     C                   IF        *inkl
031200091028     C                   MOVEL     '1'           mk01F12
031300110601     c                   IF        w_DaAtti = 'S' and CPOfls = 'C'
031400110601     c                   clear                   mk01f06
031500110601     c                   ENDIF
031600981014     C                   GOTO      FINE
031700981014     C                   ENDIF
031800100122      * attivo in questo punto il controllo del commerciale
031900100122      * se sono in manutenzione e premo qualsiasi tasto (che non sia l'enter)
032000100122      * devo dare errore se manca il commerciale
032100101202      * o se commerciale con particolarità impostata sulla tabella
032200100122     c                   IF        not *in02 and not *in10 and
032300100122     c                             (*inkb or *inks or *inku or
032400160706     c                              *inkt or
032500100524     c                              *inkw or *inkn or *inke) and
032600101202     c                             (vidcmm = *zeros or vidcmm = *blanks
032700130808     c                              or CMMpar <> *blanks)
032800100122     C                   SETON                                        502890
032900100319     C                   eval      vidmsg = msg(04)
033000100122     C                   GOTO      FOR02
033100100122    2C                   ENDIF
033200940318     C*
033300130808      *
033400091125      * F20 - Interrogazione codici collegati (anagrafica clienti)
033500091125     c                   if        *inku
033600091125     c                   clear                   tnta69ds
033700091125     c  n02              eval      Ita69Fle = '2'
033800091125     c   02              eval      Ita69Fle = '5'
033900091125     c                   eval      Ita69Cpo = cpocpo
034000091125     c                   call      'TNTA69R'
034100091125     c                   parm                    kpjba
034200091125     c                   parm                    tnta69ds
034300091125     c                   goto      for02
034400091125     c                   endif
034500160707
034600160707      * F19 - Interrogazione Variazioni Potenziale
034700160707     c                   IF        *inKT
034800160707     c                   clear                   TRMK31DS
034900160707     c                   eval      IMK31cpo = CPOcpo
035000160707     c                   eval      IMK31rag = CPOrag
035100160707     c                   eval      kpjbu = TRMK31DS
035200160707     c                   call      'TRMK31R'
035300160707     c                   parm                    kpjba
035400160707     c                   goto      for02
035500160707     c                   ENDIF
035600981012     C**
035700981012     C** CONTROLLO FORMATO2
035800981124     C                   EXSR      CTRD02
035900981012     C   90              GOTO      FOR02
036000100524      * Se F22 = Richiesta contatto
036100100120      * prima controllo se commerciale già inserito
036200100120      * altrimenti errore
036300100524     c                   IF        *inkw and
036400100120     c                             (vidcmm = *zeros or vidcmm = *blanks)
036500100120     C                   SETON                                        502890
036600100319     C                   eval      vidmsg = msg(04)
036700100120     C                   GOTO      FOR02
036800100120    2C                   ENDIF
036900940321     C*
037000981110     C* IN GESTIONE CON ENTER, RIEMETTO VIDEATA,
037100091030     C*  IN INTERROGAZIONE ESCO se no tasti funzione
037200981110     C     *IN02         IFEQ      *OFF
037300981110     C     *INKF         ANDEQ     *OFF
037400020708     C     *INKN         ANDEQ     *OFF
037500100524     C     *INKW         ANDEQ     *OFF
037600100506     C     *INKe         ANDEQ     *OFF
037700140918     c     *inkb         andeq     *off
037800140918     c     *inks         andeq     *off
037900160706     c     *inkt         andeq     *off
038000981110     C                   GOTO      FOR02
038100981110     C                   ENDIF
038200940318     C*
038300981027     C* F6 O F8 - CONFERMA
038400091028     C     mk01K10       IFNE      'N'
038500160705
038600160705      /free
038700160705       //?Salvo l'immagine Precedente
038800160705         exsr ImmaginePrima;
038900160705      /end-free
039000940321     C                   EXSR      AGGIOR
039100981102     C                   ENDIF
039200140918      * f2 - Inserimento o Variazione Rubrica
039300140918     c                   if        *inkb
039400140918     c                   clear                   tnta12ds
039500140918     c  n02              Eval      ta12ric = 'M'
039600140918     c   02              Eval      ta12ric = 'V'
039700140918     c                   Eval      ta12apl = 'P'
039800140918     c                   Eval      ta12pot = vidcp1 * 100000000 + vidcp2
039900140918     c                   Eval      ta12rag = vidrag
040000140918     c                   Call      'TNTA12R'
040100140918     c                   Parm                    kpjba
040200140918     c                   Parm                    tnta12ds
040300141027      * riaggancio l'anagrafico
040400141027     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
040500141027     c   02cpocpo        chain(n)  tncpo01l                           10
040600141027     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
040700141027     c   02cpocpo        chain(n)  tncpo11l                           13
040800141027     c                   if        %error
040900141027     c                   eval      mk01m10 = msg(3)
041000141027     c                   eval      mk01err = 'E'
041100141027     c                   goto      fine
041200141027     c                   endif
041300140918     c                   goto      for02
041400140918     c                   endif
041500140918      *
041600140918      * CMD18 - Inserimento o Variazione NOTE
041700140918     C     *INKS         IFEQ      '1'
041800140918     c                   clear                   trmk20ds
041900140918     c   02              eval      IMK20tla = 'L'
042000140918     c   02              eval      imk20flm = 'I'
042100140918     C                   eval      imk20cpo = cpocpo
042200140918     C                   eval      imk20rsc = vidrag
042300140918     c                   call      'TRMK20R'
042400140918     c                   parm                    kpjba
042500140918     c                   parm                    trmk20ds
042600141027      * riaggancio l'anagrafico
042700141027     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
042800141027     c   02cpocpo        chain(n)  tncpo01l                           10
042900141027     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
043000141027     c   02cpocpo        chain(n)  tncpo11l                           13
043100141027     c                   if        %error
043200141027     c                   eval      mk01m10 = msg(3)
043300141027     c                   eval      mk01err = 'E'
043400141027     c                   goto      fine
043500141027     c                   endif
043600140918     C                   GOTO      FOR02
043700140918     C                   END
043800981102     C**
043900081015     C* PER F14 - INFORMAZIONI COMMERCIALI
044000080925     C                   CLEAR                   DSMK50
044100981103     C     *INKN         IFEQ      *ON
044200080925     C                   MOVEL     CPOCPO        I50cpo
044300091029     C     mk01K10       IFEQ      'N'
044400080925     C                   MOVEL     'I'           I50MOD
044500981102     C                   ELSE
044600080925     C                   MOVEL     'G'           I50MOD
044700081015     C                   MOVEL     ' '           I50OBL
044800981102     C                   ENDIF
044900080925     C                   MOVEL     CPORAG        I50rag
045000081003     c                   movel     §cpoifotot    i50ifotot
045100981102     C**
045200080925     C                   CALL      'TRMK50R'
045300981102     C                   PARM                    KPJBA
045400080925     C                   PARM                    DSMK50
045500981103     C                   ENDIF
045600920415      *
045700020722     C* GESTIONE Ritorno/Fine INFO COMMERC. NEI DUE CASI
045800020722     C* DI PGM RICHIAMATO E NON RICHIAMATO
045900080925     C     O50F12        IFEQ      'S'
046000020723      * riaggancio l'anagrafico
046100070523     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
046200070523     c   02cpocpo        chain(n)  tncpo01l                           10
046300140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
046400140716     c   02cpocpo        chain(n)  tncpo11l                           13
046500070523     c                   if        %error
046600091029     c                   eval      mk01m10 = msg(3)
046700091029     c                   eval      mk01err = 'E'
046800070523     c                   goto      fine
046900070523     c                   endif
047000020722     C                   GOTO      FOR02
047100020722     C                   ENDIF
047200080925     C     O50F03        IFEQ      'S'
047300020722     C                   GOTO      FINE
047400020722     C                   ENDIF
047500091028      *
047600100524      * F22 = Richiesta contatto
047700100524     c                   if        *inkw
047800091028     c                   clear                   TRMK17ds
047900100114     c                   eval      imk17patt = 'S'
048000100114     c                   eval      imk17att = 'T'
048100100114     c                   eval      imk17pcau = 'S'
048200100114     c                   eval      imk17cau = '19'
048300091028     c                   eval      imk17cpo = CPOcpo
048400091028     c                   if        vidcmm <> *blanks
048500091028     c                   eval      imk17cmm= %dec(vidcmm:7:0)
048600091028     c                   endif
048700091028     c                   call      'TRMK17R'
048800091028     c                   parm                    kpjba
048900091028     c                   parm                    trmk17ds
049000091028      * riaggancio l'anagrafico
049100091028     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
049200091028     c   02cpocpo        chain(n)  tncpo01l                           10
049300140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
049400140716     c   02cpocpo        chain(n)  tncpo11l                           13
049500091028     c                   if        %error
049600091028     c                   eval      mk01m10 = msg(3)
049700091028     c                   eval      mk01err = 'E'
049800091028     c                   goto      fine
049900091028     c                   endif
050000091028     C                   GOTO      FOR02
050100091028     c                   endif
050200130808      *
050300100506      * F05 = Interrogazione attività
050400100810     c                   if        *inKe
050500100617     c                   clear                   TRMK21ds
050600091028     c                   eval      I21cpo = CPOcpo
050700091028     c                   eval      I21rsc = VIDrag
050800100810      * se richiamato dalla gestione attività/trattative deve andare in gestione
050900100810      * con visualizzazione sempre delle attività e senza dare il msg di altre attività
051000100810      * in window
051100100810     c                   IF        w_DaAtti = 'S'
051200100810     c                   eval      $inke = *on
051300100810     c                   clear                   omk19cpo
051400100810     c                   clear                   omk19ksc
051500100810     c                   clear                   omk19err
051600100810     c                   clear                   omk19msg
051700100810     c                   eval      I21mod = 'V'
051800100810     c                   eval      sav_trmk19 = trmk19ds
051900100810     c                   eval      KPJBU  = TRMK21ds
052000100810     c                   eval      IMK19cpo = CPOcpo
052100100810     c                   call      'TRMK21R'
052200100810     c                   parm                    kpjba
052300100810     c                   parm                    trmk19ds
052400100810      * se richiamato da interrogazione o da menù deve andare in interrogazione
052500100810     c                   ELSE
052600100810     c                   eval      I21mod = 'I'
052700091028     c                   eval      KPJBU  = TRMK21ds
052800091028     c                   call      'TRMK21R'
052900091028     c                   parm                    kpjba
053000100810     c                   ENDIF
053100091028     c                   eval      TRMK21ds = KPJBU
053200091028      * riaggancio l'anagrafico
053300091028     C  n02CPOCPO        CHAIN(e)  TNCPO01L                           10
053400091028     c   02cpocpo        chain(n)  tncpo01l                           10
053500140716     C  n02CPOCPO        CHAIN(e)  TNCPO11L                           13
053600140716     c   02cpocpo        chain(n)  tncpo11l                           13
053700091028     c                   if        %error
053800091028     c                   eval      mk01m10 = msg(3)
053900091028     c                   eval      mk01err = 'E'
054000091028     c                   goto      fine
054100091028     c                   endif
054200100810      * da attività
054300100810     c                   IF        w_DaAtti = 'S'
054400100810      * rimetto la trmk19ds come prima
054500100810     c                   eval      trmk19ds = sav_trmk19
054600100810      * se F12 riemetto la videata del potenziale
054700100810     c                   IF        O21fxx = '12'
054800100810     c                   clear                   mk01f06
054900100810     c                   goto      for02
055000100810     c                   ENDIF
055100100810      * se ho gestito l'attività dal TRMK21R vado a fine
055200100810     c                   IF        O21fxx = '99'
055300100810     c                   clear                   mk01f06
055400100810     c                   goto      fine
055500100810     c                   ENDIF
055600100810      * se ho dato F10 vdevo andare avanti con il TRMK19R o TNTA88R
055700100810     c                   IF        O21fxx = '10'
055800100810     c                   clear                   mk01f06
055900100810     c                   ENDIF
056000100810      * se non ho fatto niente spengo $inke e vado avanti
056100100810     c                   IF        O21fxx = *blanks
056200100810     c                   eval      $inke = *off
056300100810     c                   ENDIF
056400100810     c                   ELSE
056500091028     C                   GOTO      FOR02
056600100810     c                   ENDIF
056700091028     c                   endif
056800020722      *
056900091028     C                   MOVEL     VIDCP1        mk01CPO
057000091028     C                   MOVE      VIDCP2        mk01CPO
057100091028     C                   MOVEL     VIDRAG        mk01RAG
057200091028     C                   MOVEL     '1'           mk01F06
057300130808      *
057400100409      * se sono stato richiamato dalla gestione attività richiamo il programma trmk19
057500100810      * se non sono già passata per Inke
057600100810     c                   If        W_Daatti = 'S'
057700100617     c                   clear                   omk19cpo
057800100617     c                   clear                   omk19ksc
057900100617     c                   clear                   omk19err
058000100617     c                   clear                   omk19msg
058100110518      * Se potenziale codificato e non sono passata da F5-Attività non devo dare la
058200110518      * possibilità di fare una nuova attività o aprire trattativa.
058300110518     c                   IF        CPOfls = 'C'
058400110518     c                   eval      *in28 = *on
058500110518     c                   eval      *in90 = *on
058600110518     c                   eval      vidmsg =
058700110518     c                             'Nuova Attività/Trattativa non possibile-
058800110518     c                              per Potenziale CODIFICATO.'
058900110518     c                   goto      for02
059000110518     c                   ENDIF
059100100809      * Prima di passare al trmk19r o TNTA88R
059200100809      * richiamo il trmk21r per dare la possibilità di gestire
059300100617      * eventuali attività ancora da eseguire
059400100617      * *in77 = F5-Attività in pink (attività presenti sul potenziale)
059500100810     c                   IF        *in77 and not $inke
059600100617      * salvo la trmk19ds così come mi è arrivata
059700100617     c                   eval      sav_trmk19 = trmk19ds
059800100617     c                   clear                   TRMK21ds
059900100617     c                   eval      I21mod = 'G'
060000100617     c                   eval      I21cpo = CPOcpo
060100100617     c                   eval      I21rsc = VIDrag
060200100617     c                   eval      KPJBU  = TRMK21ds
060300100617     c                   eval      IMK19cpo = CPOcpo
060400100617     c                   call      'TRMK21R'
060500100617     c                   parm                    kpjba
060600100617     c                   parm                    trmk19ds
060700100617     c                   eval      TRMK21ds = KPJBU
060800100706      * rimetto la trmk19ds come prima
060900100706     c                   eval      trmk19ds = sav_trmk19
061000100706      * se F12 riemetto la videata del potenziale
061100100706     c                   IF        O21fxx = '12'
061200100706     c                   clear                   mk01f06
061300100706     c                   goto      for02
061400100706     c                   ENDIF
061500100706      * se ho gestito l'attività dal TRMK21R vado a fine
061600100617     c                   IF        O21fxx = '99'
061700100617     c                   clear                   mk01f06
061800100617     c                   goto      fine
061900100617     c                   ENDIF
062000100617     c                   ENDIF
062100100617      * richiamo la gestione attività
062200100809      * o gestione trattative, dipende da dove arrivo
062300100430     c                   clear                   omk19cpo
062400100430     c                   clear                   omk19ksc
062500100430     c                   clear                   omk19err
062600100430     c                   clear                   omk19msg
062700100910     c                   IF        IMK19tco = 'T'
062800100409     c                   call      'TRMK19R'
062900100409     c                   Parm                    kpjba
063000100409     c                   Parm                    trmk19ds
063100100409     c                   parm                    trmk01ds
063200100910     c                   endif
063300100910     c                   IF        IMK19tco = 'O'
063400100910     c                   clear                   tnta88ds
063500101111     c                   eval      ita88cmmi = imk19com
063600100910     c                   eval      ita88fle = imk19fle
063700100910     c                   eval      ita88cpo = mk01cpo
063800100910     c                   call      'TNTA88R'
063900100910     c                   Parm                    kpjba
064000100910     c                   Parm                    tnta88ds
064100100910     c                   clear                   mk01f06
064200100910     c                   ENDIF
064300100409     c                   endif
064400130808     c*
064500080925     C                   GOTO      FINE
064600920415      *
064700920415     C     FINE          TAG
064800970711     C*
064900970711     C                   MOVEL     'C'           I13TLA
065000970711     C                   CALL      'FNLV13R'
065100970711     C                   PARM                    KPJBA
065200970711     C                   PARM                    DSLV13
065300970711     C                   PARM                    DSSI95
065400970711     C*
065500970711     C                   MOVEL     'C'           I14TLA
065600970711     C                   CALL      'FNLV14R'
065700970711     C                   PARM                    KPJBA
065800970711     C                   PARM                    DSLV14
065900090626     C**
066000080925     C                   CLEAR                   DSMK50
066100080925     C                   MOVEL     'C'           I50TLA
066200080925     C                   CALL      'TRMK50R'
066300981102     C                   PARM                    KPJBA
066400080925     C                   PARM                    DSMK50
066500090626     C*
066600090626     C                   clear                   tntaa1ds
066700090626     C                   movel     'C'           Itaa1Tla
066800090626     C                   movel(p)  tntaa1ds      kpjbu
066900090703     C                   CALL      'TNTAA1C'
067000090626     C                   PARM                    KPJBA
067100160705
067200160705      /free
067300160705         clear TRMK30DS;
067400160705         clear tncpo_30;
067500160705         clear tncpo1_30;
067600160705         clear ticpi_30;
067700160705         IMK30tla = 'C';
067800160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
067900160705      /end-free
068000160705
068100971203     C**
068200920415     C                   SETON                                        LR
068300920416      *-------------------------------------------------------------------------
068400920416      *SUBROUTINE RICERCA ELEMENTI DI TABELLA
068500940317      *-------------------------------------------------------------------------
068600981012     C     CERTAB        BEGSR
068700920416     C                   MOVEL     CODUT         §KUT
068800981009     C                   MOVEL     COD           §COD
068900981103     C                   CLEAR                   §KEY
069000920416     C                   MOVE      *BLANKS       VIDELE
069100920416     C                   CALL      'X§TABER'
069200920416     C                   PARM                    §KUT
069300920416     C                   PARM                    §COD
069400920416     C                   PARM                    §KEY
069500920416     C                   PARM                    §DES             30
069600920416     C                   MOVEL     §KEY          VIDELE           10
069700920416     C                   MOVEL     §DES          DESELE           30
069800920416     C                   ENDSR
069900981009     C*---------------------------------------------------------------
070000050629     C* CONTROLLO potenziale
070100981009     C*---------------------------------------------------------------
070200050629     C     CTRcpo        BEGSR
070300081003     c                   clear                   dcpo01
070400140910     c                   clear                   dcpo02
070500140930     c                   clear                   cpotel
070600981009      *
070700981009     C* CHAIN
070800981009     C                   MOVEL     VIDCP1        CPOCPO
070900981009     C                   MOVE      VIDCP2        CPOCPO
071000981009     C**
071100981014    0C     CPOCPO        IFGT      0
071200091028     C     mk01K10       IFNE      'N'
071300981014     C     CPOCPO        CHAIN     TNCPO01L                           1094
071400140716     C     CPOCPO        CHAIN     TNCPO11L                           13
071500981023     C                   ELSE
071600981023     C     CPOCPO        CHAIN(N)  TNCPO01L                           10
071700140716     C     CPOCPO        CHAIN(n)  TNCPO11L                           13
071800981023     C                   ENDIF
071900981023     C***
072000981009     C* 94 ON  - RECORD ALLOCATO
072100981009    1C     *IN94         IFEQ      *ON
072200091028     C                   Eval      mk01m10 = msg(3)
072300091028     c                   Eval      mk01err = 'E'
072400050629     C                   GOTO      fine
072500981009    1C                   ENDIF
072600981014     C* SE NON IMMESSO POTENZIALE ACCENDO INDICATORE 10
072700981014     C                   ELSE
072800981014     C                   SETON                                        10
072900981014    0C                   ENDIF
073000021023      *
073100981009     C*
073200100120     C* - I M M I S S I O N E
073300100120    1C     Immissione    IFEQ      *ON
073400981009     C*
073500981009     C* CODICE GIA' ESISTENTE: ERRORE
073600981009    2C     *IN10         IFEQ      *OFF
073700091028     C                   Eval      mk01m10 = msg(17)
073800091028     c                   Eval      mk01err = 'E'
073900050629     C                   GOTO      fine
074000981009   X2C                   ELSE
074100020708      *
074200981009     C* SE NON IMMESSO NUMERO LO PRENDO DAL NUMERATORE (DS1S)
074300020708    3C*  10VIDCP2        IFEQ      *ZEROS
074400041026      * LO PRENDO SEMPRE DA SIMFEL+NUMERATORE
074500981009     C                   Z-ADD     SIMFEL        VIDCP1
074600020709      *
074700020709     C                   setoff                                       94
074800020708     C                   clear                   Trul33ds
074900020712     C                   Z-ADD     vidcp1        I33PO1
075000020709     C                   Z-ADD     0             I33OPE
075100020712     C                   Z-ADD     23            I33CNU
075200020709     C                   Z-ADD     1             I33NUM
075300020712     C     *in94         doweq     *off
075400020708     C                   movel(p)  trul33ds      kpjbu
075500020708     C                   CALL      'TRUL33R'
075600020708     C                   PARM                    KPJBA
075700020708     C                   eval      trul33ds = kpjbu
075800020708      *
075900020709     C                   if        O33ERR = *ZEROS
076000020709      *
076100020708      * Controllo che il numero non esista gia' nel file potenziali
076200020709     C                   eval      cpocpo = (Vidcp1*100000000) + O33NRI
076300981023     C     CPOCPO        CHAIN(N)  TNCPO01L                           94
076400020723      * Trul33 in errore
076500020723     C                   ELSE
076600091028     C                   Eval      mk01m10 = msg(46)
076700091028     c                   Eval      mk01err = 'E'
076800050629     C                   GOTO      fine
076900020712     C                   ENDIF
077000981009    4C                   ENDDO
077100020709     C                   MOVE      CPOCPO        vidcp2
077200040930     c                   Clear                   Cpoatb
077300020708    2C                   ENDIF
077400981009     C*
077500981009   X1C                   ELSE
077600981009     C* CODICE INESISTENTE
077700981009    2C     *IN10         IFEQ      *ON
077800091028     C                   Eval      mk01m10 = msg(19)
077900091028     c                   Eval      mk01err = 'E'
078000050629     C                   GOTO      fine
078100981014   X2C                   ELSE
078200981014     C**
078300090626     C* CONTROLLO abilitazioni utente
078400091028     C     mk01K10       Ifne      'N'
078500091028     C     mk01K10       ANDNE     'C'
078600091028     C     mk01K10       ANDNE     'B'
078700090626     c*
078800090626     c                   clear                   tntaa1ds
078900090626     c                   eval      itaa1cpo =cpocpo
079000090626     c                   eval      itaa1caut='§UTEPOT'
079100090626     c                   clear                   kpjbu
079200090626     c                   movel     tntaa1ds      kpjbu
079300090703     c                   call      'TNTAA1C'
079400090626     c                   parm                    kpjba
079500090626     c                   movel     kpjbu         tntaa1ds
079600130808     c*
079700090626     c* Errore o non abilitato
079800090626    3c                   if        otaa1fabi='N'
079900091028     C                   Eval      mk01m10 = msg(15)
080000091028     c                   Eval      mk01err = 'E'
080100050629     C                   GOTO      Fine
080200981103    4C                   ENDIF
080300981103    3C                   ENDIF
080400981014     C**
080500981009    2C                   END
080600981009    1C                   END
080700050629     C                   ENDSR
080800981012     C*---------------------------------------------------------------
080900981012     C* CARICO DATI DEL POTENZIALE
081000981012     C*---------------------------------------------------------------
081100981012     C     CARICA        BEGSR
081200080206     c                   eval      wforzacdf = *off
081300080206     c                   eval      wforzaiva = *off
081400080206     c                   eval      wforzaiva1 = *off
081500981012      * ANNULLATO
081600080206     C     CPOATB        IFEQ      'A'
081700100202     C                   SETON                                        02
081800981012     C                   END
081900981012      *
082000981012     C                   MOVE      CPORAG        VIDRAG
082100080206     c* SEDE/Filiale (per il momento in sospeso)
082200080623     c                   clear                   vidfsf
082300080623     c                   if        cpofsf='S'
082400080624     c                   eval      vidfsf='SEDE'
082500080623     c                   else
082600080624     c                   eval      vidfsf='FIL '
082700080623     c                   endif
082800130808      *
082900981012     C                   MOVE      CPOVIA        VIDVIA
083000981012     C                   MOVE      CPOCIT        VIDCIT
083100981014     C                   MOVEL     CPOCAP        VIDCAP
083200981012     C                   MOVE      CPOPRV        VIDPRV
083300060714     c                   eval      savprv = cpoprv
083400981012     C                   MOVE      CPONAZ        VIDNAZ
083500080206     C                   MOVE      CPONAZ        savnaz
083600140714     C                   MOVE      CPO1TEL       VIDTEL
083700140714     C                   MOVE      CPO1FAX       VIDFAX
083800080205     c* Codice fiscale
083900080205     c                   eval      vidcdf=cpocdf
084000080207     c                   eval      savcdf=cpocdf
084100080205     c* Partita IVA
084200080205     c                   IF        %subst(cpopiv:1:2) < *zeros
084300080205     C                   eval      Vidivaeu = %subst(cpopiv:1:2)
084400080205     C                   eval      Vidivait = %subst(cpopiv:3)
084500080205     c                   ELSE
084600080205     c                   movel     cpopiv        vidivait
084700080205     c                   ENDIF
084800080207     c                   eval      saviva=vidiva
084900080205      * CATEGORIA merceologica
085000981012     C                   MOVEL     CPOSCT        VIDSCT
085100981012     C                   MOVEL(P)  CPOSCT        KEY
085200981012     C                   EXSR      CTRSCT
085300981012     C                   MOVEL     WDES          DESSCT
085400981012      *
085500981012      * codice commerciale
085600981012     C     CPOCMM        IFNE      *ZEROS
085700981012     C                   MOVEL     CPOCMM        VIDCMM
085800981012     C                   EXSR      CTRCMM
085900981012     C                   ENDIF
086000981012      *
086100080222      * FATTURATO
086200080222     c                   clear                   vidfatd
086300091217     c                   eval      vidfatd='Fatt.to Azienda/1000'
086400080222     c                   if        cpoftaz>0
086500091217     c                   eval      vidfatd=%trim(vidfatd)+ ' ' + hi
086600080222     c                             + %trim(%editc(cpoftaz:'2'))
086700080222     c* Anno fatturato oppure stimato/dichiarato
086800080222     c                   select
086900080222     c                   when      cpoafaz>0
087000080222     c                   eval      vidfatd =%trim(vidfatd) + nm
087100080222     c                             +'Anno' + hi + %editc(cpoafaz:'Z')
087200080222     c                   when      cpoffaz='S'
087300080222     c                   eval      vidfatd=%trim(vidfatd) + nm + 'STIMATO'
087400080222     c                   when      cpoffaz='D'
087500080222     c                   eval      vidfatd=%trim(vidfatd) + nm +'DICHIARATO'
087600080222     c                   endsl
087700080222     c                   if        cpopexp>0
087800080226     c                   eval      vidfatd=%trim(vidfatd) + nm +'di cui export'
087900080226     c                             + hi + %editc(cpopexp:'Z') + '%'
088000080222     c                   endif
088100080222     c                   endif
088200130808     c*
088300090518     c* Ds del campo CPORST
088400090518     c                   eval      dcpo01=cporst
088500140910
088600090518     c* Spesa trasporti reale/presunta
088700090520     C                   CLEAR                   VIDDSPT
088800090518     c                   eval      ktpf='SPT'
088900090518     c     kcpi          chain     ticpi01l
089000090521     c                   select
089100090521     c                   when      %found(ticpi01l)
089200091217     c                   eval      viddspt='Spesa Trasporti      ' +
089300090520     c                                      hi + %trim(%editc(cpipft:'2')) +
089400090520     c                                     nm +'REALE   '
089500090521     c                   when      §cposptp>*zeros
089600091217     c                   eval      viddspt='Spesa Trasporti      ' +
089700090520     c                                      hi + %trim(%editc(§cposptp:'2')) +
089800090520     c                                     nm +'PRESUNTO'
089900090521     C                   ENDsl
090000140910
090100140910      /free
090200140910       //?Ds campo CPOTEL
090300140910         dcpo02 = CPOtel;
090400140910
090500140910       //?Anno Inizio Attività
090600140910         IF  §CPOannoia <> *blanks and §CPOannoia <> *zeros;
090700140910           vidannoia = §CPOannoia;
090800140910         ELSE;
090900140910           clear vidannoia;
091000140910         ENDIF;
091100140910      /end-free
091200130808      *
091300100319      * codice importanza cliente
091400100319     c                   eval      vidcic = cpoftr
091500090518     c
091600120703      * Data primo contatto da CPORST
091700120703     c                   IF        §CPOdtpcon > *zeros
091800120703     c                   eval      g08inv = %dec(§CPOdtpcon:8:0)
091900120703     c                   clear                   g08dat
092000120703     c                   eval      g08err = '3'
092100120703     c                   call      'XSRDA8'
092200120703     c                   parm                    wlbda8
092300120703     c                   eval      VIDdpc = g08dat
092400120703     c                   ELSE
092500120703     c                   clear                   VIDdpc
092600120703     c                   ENDIF
092700981012      *
092800981012      * DATA ACQUISIZIONE
092900981022     C                   Z-ADD     CPODAQ        G08INV
093000981022     C                   MOVE      *ZEROS        G08DAT
093100981022     C                   MOVEL     3             G08ERR
093200981022     C                   CALL      'XSRDA8'
093300981022     C                   PARM                    WLBDA8
093400981022     C                   Z-ADD     G08DAT        VIDDAC
093500981012     C**
093600981012     C                   MOVE      CPOFLT        VIDFLT
093700981014     C     CPOFLT        CHAIN     AZORG01L                           30
093800981014     C  N30              MOVEL     ORGDES        DESFLT
093900981127     C                   MOVEL     CPOFLT        OLDFLT
094000981120     C**
094100981120     C* VEDO SE CI SONO INFO COMMERCIALI
094200091029     C     cpocpo        CHAIN     TICPI01L                           31
094300981120    3C     *IN31         DOWEQ     *OFF
094400981120     C     CPIATB        ANDNE     ' '
094500091029     C     cpocpo        READE     TICPI01L                               31
094600981120    3C                   ENDDO
094700981120     C* 31 ON - NON CI SONO
094800981120     C     *IN31         IFEQ      *ON
094900091028     C                   SETOff                                       11
095000981120     C                   ELSE
095100091028     C                   SETOn                                        11
095200981120     C                   ENDIF
095300110203      * Decodifico la categoria del potenziale
095400110203     c                   clear                   dcpo
095500110203     c                   clear                   tibs02ds
095600110203     c                   eval      t02mod = 'C'
095700110203     c                   eval      t02cod = 'CPO'
095800110203     c                   eval      t02ke1 = CPOfls
095900110203     c                   eval      t02sif = knsif
096000110203     c                   call      'TIBS02R'
096100110203     c                   parm                    kpjba
096200110203     c                   parm                    tibs02ds
096300110203     c                   if        t02err = *blanks
096400110203     c                   eval      dcpo = t02uni
096500110203     c                   endif
096600110203      * imposto la descrizione
096700110503     c                   eval      viddfls = §cpodesc
096800110203     c                   eval      *in12 = (CPOfls <> 'M')
096900140910
097000140910      /free
097100140910       //?Carico la nota DP dettaglio prodotti
097200140910         flag02 = 'P';
097300140910         clipot = %editc(CPOcpo:'X');
097400140910         ktnt = 'DP';
097500140910         chain (flag02:clipot:knk2:ktnt) TFNTC01L;
097600140910         IF  %found(TFNTC01L);
097700140910           viddetp = ntcrnt;
097800140910         ELSE;
097900140910           clear viddetp;
098000140910         ENDIF;
098100140910      /end-free
098200140910
098300981216     C**
098400981012     C                   ENDSR
098500941111     C*------------------------------------------------------------------------*
098600941125     C*  CHKIND - CONTROLLO INDIRIZZO COMPLETO
098700941111     C*------------------------------------------------------------------------*
098800941125     C     CHKIND        BEGSR
098900941111     C*
099000941111     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
099100970711     C                   MOVEL     VIDRAG        I14RSC
099200970711     C                   MOVE      *BLANKS       I14RS2
099300970711     C                   MOVEL     VIDVIA        I14IND
099400970711     C                   MOVEL     VIDCIT        E14LOC
099500970711     C                   CLEAR                   E14PIP
099600970711     C                   MOVEL     VIDPRV        E14PRV
099700981012     C                   MOVEL     VIDNAZ        E14NAR
099800970711     C                   MOVEL     VIDCAP        E14CAP
099900970711     C*
100000970711     C                   CLEAR                   DSLV13
100100970711     C                   CLEAR                   DSSI95
100200970711     C                   CLEAR                   I14ISO
100300970711     C                   MOVEL     DATEU         I14DRI
100400970711     C*
100500970711     C                   CALL      'FNLV14R'
100600970711     C                   PARM                    KPJBA
100700970711     C                   PARM                    DSLV14
100800970711     C*
100900970711     C                   MOVEL     O14ERR        WERR              1
101000970711     C                   MOVEL     O14MSG        VIDMSG
101100970711     C                   EXSR      CTRERR
101200981012     C** ERRORE
101300981012     C     *IN90         IFEQ      *ON
101400970711     C                   GOTO      ENDC00
101500970711     C                   ENDIF
101600970711     C*************
101700970711     C* CONTROLLO CAP
101800970711     C                   MOVEL     '7'           I95TCN
101900970711     C                   MOVEL     'S'           I13LA3
102000970711     C                   MOVEL     'S'           I13SZ3
102100970711     C                   MOVEL     E14CAP        I95CAP
102200970711     C                   MOVEL     E14LOC        I95LOC
102300970711     C                   MOVEL     E14PRV        I95PRV
102400970711     C                   MOVEL     E14NAR        I95NAR
102500970711     C                   MOVEL     DATEU         I95DAT
102600970711     C                   MOVEL     'S'           I13AF0
102700970711     C                   MOVEL     'S'           I13CNV
102800970711     C                   MOVEL     'S'           I13AF1
102900970711     C**  SE CAMBIATI DATI CONTROLLO SE CONGRUENTI: ERRORE FORZABILE
103000970711    2C     E14CAP        IFNE      VARCAP
103100970711     C     E14LOC        ORNE      VARLOC
103200970711     C     E14PRV        ORNE      VARPRV
103300970711     C     E14NAR        ORNE      VARNAR
103400970711     C                   CLEAR                   WZ3
103500970711     C                   MOVEL     E14CAP        VARCAP
103600970711     C                   MOVEL     E14LOC        VARLOC
103700970711     C                   MOVEL     E14PRV        VARPRV
103800970711     C                   MOVEL     E14NAR        VARNAR
103900970711    2C                   ENDIF
104000970711     C                   MOVEL     WZ3           E13FZ3
104100970711     C**
104200970711     C** CALL
104300970711     C                   CALL      'FNLV13R'
104400970711     C                   PARM                    KPJBA
104500970711     C                   PARM                    DSLV13
104600970711     C                   PARM                    DSSI95
104700970711     C*
104800970711     C                   MOVEL     O13ERR        WERR              1
104900970711     C                   MOVEL     O13MSG        VIDMSG
105000970711     C                   EXSR      CTRERR
105100970711     C**
105200970711     C                   MOVEL     E13FZ3        WZ3
105300020826      **
105400981012     C** ERRORE
105500981012     C     *IN90         IFEQ      *ON
105600970711     C                   GOTO      ENDC00
105700970711     C                   ENDIF
105800970711     C*
105900020906      **
106000041207     C     dutPOU        CHAIN     AZORG01L                           30
106100020906     C   30              CLEAR                   ORGDIT
106200970711     C* SE LA FIL.APP.  E' VUOTA LA PROPONGO E RIEMETTO IL VIDEO
106300970711     C     VIDFLT        IFEQ      *BLANKS
106400970711     C     VIDFLT        OREQ      *ZEROS
106500090626     C* SE SONO una messaggerie, propongo la seconda linea distribuzione
106600090626     C     dutntw        IFEQ      'MES'
106700981103     C     SIMFEL        ANDNE     46
106800981103     C     O95CLO        ANDGT     0
106900981103     C                   MOVEL     O95CLO        VIDFLT
107000981103     C                   ELSE
107100981103     C                   MOVEL     O95LNA        VIDFLT
107200981103     C                   ENDIF
107300981117     C                   MOVEL     VIDFLT        NUMFLT
107400981103     C**
107500981117     C     NUMFLT        CHAIN     AZORG01L                           30
107600981014     C  N30              MOVEL     ORGDES        DESFLT
107700981012     C                   SETON                                        90
107800970711     C                   ENDIF
107900941125     C*
108000970711     C     ENDC00        TAG
108100970711     C* IMPOSTO I CAMPI DI UNA EVENTUALE RICERCA
108200970711     C                   MOVEL     E14LOC        VIDCIT
108300970711     C                   MOVEL     E14CAP        VIDCAP
108400970711     C                   MOVEL     E14PRV        VIDPRV
108500970711     C                   ENDSR
108600970711     C**************************************************************************
108700981012     C* CONTROLLO SE C'E' ERRORE
108800970711     C**************************************************************************
108900970711     C     CTRERR        BEGSR
109000970711     C* IMPOSTO CAMPI PASSATI
109100970711    1C     O13RON        IFEQ      'S'
109200970711     C                   MOVEL     O95NAR        E14NAR
109300970711    1C                   ENDIF
109400970711    1C     O13ROC        IFEQ      'S'
109500970711     C                   MOVEL     O95CAP        E14CAP
109600970711    1C                   ENDIF
109700970711    1C     O13ROP        IFEQ      'S'
109800970711     C                   MOVEL     O95PRV        E14PRV
109900970711    1C                   ENDIF
110000970711    1C     O13ROL        IFEQ      'S'
110100970711     C                   MOVEL     O95LOC        E14LOC
110200970711    1C                   ENDIF
110300970711     C* SE RITORNA CODICE DI ERRORE LO EMETTO A VIDEO
110400970711    1C     WERR          IFNE      ' '
110500970711    2C     VIDMSG        IFNE      *BLANKS
110600981012     C                   SETON                                        28
110700970711    2C                   END
110800970711     C* POSIZIONAMENTO CURSORE
110900970711    2C                   SELECT
111000970711     C     WERR          WHENEQ    '1'
111100981012     C                   SETON                                        40
111200970711     C     WERR          WHENEQ    '2'
111300981012     C                   SETON                                        41
111400970711     C     WERR          WHENEQ    '3'
111500981012     C                   SETON                                        42
111600970711     C     WERR          WHENEQ    '4'
111700981012     C                   SETON                                        43
111800970711     C     WERR          WHENEQ    '5'
111900981012     C                   SETON                                        44
112000981012     C     WERR          WHENEQ    '6'
112100981012     C                   SETON                                        45
112200970711    2C                   ENDSL
112300970711     C*
112400981012     C                   SETON                                        90
112500970711    1C                   ENDIF
112600970711     C                   ENDSR
112700981012     C*---------------------------------------------------------------
112800981012     C* CONTROLLO E DECODIFICO CODICE COMMERICIALE
112900981012     C*---------------------------------------------------------------
113000981012     C     CTRCMM        BEGSR
113100981012     C* RICERCA
113200101202     c                   if        %scan('?':vidcmm) > 0
113300010620     C                   MOVE      *BLANKS       VIDELE
113400130808     c                   clear                   TRMK43ds
113500130808     C                   CALL      'TRMK43R'
113600010620     C                   PARM                    KPJBA
113700130808     C                   PARM                    TRMK43ds
113800130808     c                   if        oMK43err = *off  and  oMK43fxx = *blank
113900130808     C                   MOVEL     oMK43cmm      VIDELE           10
114000130808     C                   MOVEL     oMK43des      DESELE           30
114100130808     c                   endif
114200101202     c                   eval      vidcmm = VIDELE
114300101202     c                   eval      descmm = DESELE
114400101202     c                   endif
114500981012     C* CONTROLLI
114600130808     c                   If        %check(c_Digits:VIDcmm) > *zeros
114700130808     c                   seton                                        289050
114800130808     c                   movel     Msg(20)       VIDmsg
114900130808     c                   leavesr
115000130808     c                   EndIf
115100101202     c                   if        vidcmm > *zeros
115200130808     c                   move      vidcmm        CMMcod
115300130808     C     CMMcod        CHAIN     AZCMM000
115400130808     c                   IF        %found(AZCMM01L)
115500130808     c                   eval      descmm = CMMdes
115600981012     C                   ELSE
115700981012     C                   SETON                                        289050
115800981012     C                   MOVEL     MSG(20)       VIDMSG
115900981012     C                   ENDIF
116000981012     C                   ENDIF
116100130808      *
116200981012     C                   ENDSR
116300981012      *-------------------------------------------------------------------------
116400080205      *CONTROLLO CATEGORIA merceologica
116500981012      *-------------------------------------------------------------------------
116600981012     C     CTRSCT        BEGSR
116700981012     C*
116800120703     C                   CLEAR                   WDES             40
116900981012     C                   MOVE      '1L'          COD
117000981012     C     '?'           SCAN      KEY                                    90
117100981012     C* RICERCA
117200981012     C     *IN90         IFEQ      '1'
117300981012     C                   EXSR      CERTAB
117400981012     C                   MOVEL(P)  VIDELE        KEY
117500981012     C                   MOVEL     DESELE        WDES
117600981014     C                   SETON                                        48
117700981012     C                   ELSE
117800981012     C* CONTROLLI
117900080207     c                   clear                   ds1l
118000091029     C     KTAB          CHAIN     TABEL                              30
118100981014     C     *IN30         IFEQ      *OFF
118200981026     C     TBLFLG        ANDEQ     ' '
118300080207     c                   movel     tbluni        ds1l
118400080207     C                   MOVEL     §1ldes        WDES
118500080207     c                   if        §1luti='N'
118600080207     C                   SETON                                        289048
118700080207     C                   MOVEL     MSG(25)       VIDMSG
118800080207     c                   endif
118900981012     C                   ELSE
119000981012     C                   SETON                                        289048
119100981012     C                   MOVEL     MSG(25)       VIDMSG
119200981012     C                   ENDIF
119300981012     C                   ENDIF
119400981012     C                   ENDSR
119500920416      *-------------------------------------------------------------------------
119600920416      *PULIZIA CAMPI FILE VIDEO
119700940317      *-------------------------------------------------------------------------
119800920416     C     AZZERO        BEGSR
119900970711     C                   MOVE      0             WZ3               1 0
120000920416     C                   Z-ADD     *ZEROS        VIDCP1
120100920416     C                   Z-ADD     *ZEROS        VIDCP2
120200920416     C                   MOVE      *BLANKS       VIDRAG
120300920416     C                   MOVE      *BLANKS       VIDVIA
120400920416     C                   MOVE      *BLANKS       VIDCIT
120500981014     C                   MOVEL     *BLANKS       VIDCAP
120600920416     C                   MOVE      *BLANKS       VIDPRV
120700981009     C                   MOVE      *BLANKS       VIDNAZ
120800920416     C                   MOVE      *BLANKS       VIDTEL
120900981009     C                   MOVE      *BLANKS       VIDFAX
121000920416     C                   MOVE      *BLANKS       VIDSCT
121100920416     C                   MOVE      *BLANKS       DESSCT
121200920416     C                   MOVE      *BLANKS       VIDCMM
121300920416     C                   MOVE      *BLANKS       DESCMM
121400920427     C                   MOVE      *BLANKS       VIDFLT
121500920421     C                   MOVE      *BLANKS       DESFLT
121600940321     C                   CLEAR                   UNO
121700940321     C                   CLEAR                   SAVFLT
121800981117     C                   CLEAR                   WDIVAS
121900050518     c                   Clear                   vidrsp1
122000081010     c                   eval      vidfsf='SEDE'
122100140910     c                   clear                   viddetp
122200140910     c                   clear                   vidannoia
122300140910
122400080205     c                   ENDSR
122500981012     C*---------------------------------------------------------------
122600981012     C* CONTROLLO 2 VIDEATA
122700981012     C*---------------------------------------------------------------
122800981012     C     CTRD02        BEGSR
122900981124     C** CONTROLLO SOLO IN MANUTENZIONE
123000981124    0C     *IN02         IFEQ      *OFF
123100981012     C**
123200981012     C** INDIRIZZO
123300981012     C**
123400981012     C* CONTROLLO INDIRIZZO COMPLETO
123500981012     C                   EXSR      CHKIND
123600981012     C   90              GOTO      ENDCT2
123700981012     C* TELEFONO
123800100524      * non controllo se F22=Richiesta contatto
123900981012     C     VIDTEL        IFEQ      *BLANKS
124000100524     c     *inkw         andeq     '0'
124100981012     C                   SETON                                        289046
124200981012     C                   MOVEL     MSG(24)       VIDMSG
124300981012     C                   GOTO      ENDCT2
124400981012     C                   ENDIF
124500981012     C**
124600100524     c                   if        not *inkw
124700981012     C                   CLEAR                   TRUL42
124800981014     C                   MOVEL     VIDTEL        D42FAX
124900981012     C                   CALL      'TRUL42R'
125000981012     C                   PARM                    TRUL42
125100981012     C     D42ERR        IFEQ      '1'
125200981012     C                   SETON                                        289046
125300981012     C                   MOVEL     D42MSG        VIDMSG
125400981012     C                   GOTO      ENDCT2
125500981012     C                   END
125600100407     c                   endif
125700981012     C** FAX SOLO SE C'E'
125800981012     C     VIDFAX        IFNE      *BLANKS
125900981012     C                   CLEAR                   TRUL42
126000981012     C                   MOVEL     VIDFAX        D42FAX
126100981012     C                   CALL      'TRUL42R'
126200981012     C                   PARM                    TRUL42
126300981012     C     D42ERR        IFEQ      '1'
126400981012     C                   SETON                                        289047
126500981012     C                   MOVEL     D42MSG        VIDMSG
126600981012     C                   GOTO      ENDCT2
126700981012     C                   END
126800981012     C                   END
126900080623     c**
127000100922     c** Falg sede filiale
127100080623     c**
127200080623     c* controllo solo se cambiato
127300080623     c                   if        %subst(vidfsf:1:1)<>cpofsf
127400080623     c* Se presente codice duns o dati azienda, non modificabile
127500081105     c                   if        cpoduns<>*blanks or cpoftaz>0
127600100922     c* l'utente deve essere autorizzato alla modifica
127700100922     c**                 if        simfel<>46 or (knmus<>'SEG019 ' and
127800100922     c**                           knmus<>'SEG013 ' and %subst(knmus:1:3)<>
127900100922     c**                           'EDP')
128000100922     c*
128100100922     c                   if        §utepotmsf<>'S' and  %subst(knmus:1:3)<>'EDP'
128200080623     C                   SETON                                        672890
128300080623     c                   if        cpofsf='S'
128400080623     C                   MOVEL     MSG(38)       VIDMSG
128500080623     c                   else
128600080623     C                   MOVEL     MSG(39)       VIDMSG
128700080623     c                   endif
128800080623     C                   GOTO      ENDCT2
128900080623    3C                   ENDIF
129000080623     c                   endif
129100081105     c                   endif
129200130808     c*
129300080205     C** CODICE FISCALE
129400080206     c* reimposto campi per forzatura e partita aiva in base allo stato
129500080206     c                   if        vidnaz<>savnaz
129600080206     c                   eval      wforzacdf = *off
129700080206     c                   eval      wforzaiva1 = *off
129800080206     c                   eval      savnaz=vidnaz
129900080206     c                   endif
130000080205     c                   exsr      sr_ctrcdf
130100080205     C   28              goto      endct2
130200981012     C**
130300080205     C** Partita Iva
130400080205     c                   exsr      sr_Ctriva
130500080205     C   28              goto      endct2
130600130808      *
130700981117     C** P.O. DI APPARTENENZA
130800981012     C**
130900981012      *RICERCA
131000981014     C     '?'           SCAN      VIDFLT                                 90
131100981014    1C     *IN90         IFEQ      '1'
131200981012     C                   EXSR      SELFIL
131300981014     C                   SETON                                            49
131400981012     C                   GOTO      ENDCT2
131500981012    1C                   ENDIF
131600981012     C* CONTROLLO
131700981012     C                   MOVE      VIDFLT        NUMFLT
131800981014     C     NUMFLT        CHAIN     AZORG01L                           30
131900981012     C* ERRATA
132000981014    1C     *IN30         IFEQ      *ON
132100981012     C     ORGFVA        ORNE      ' '
132200981012     C                   MOVEL     MSG(26)       VIDMSG
132300981012     C                   SETON                                        492890
132400981012     C                   GOTO      ENDCT2
132500981012    1C                   ENDIF
132600981014     C                   MOVEL     ORGDES        DESFLT
132700981012     C**
132800981027     C* P.O. ELABORATORE FIL TRASMISSIONE
132900981117    1C     SAVFLT        IFNE      NUMFLT
133000981117     C                   MOVEL     ' '           UNO               1
133100981117     C                   CLEAR                   WDIVAS
133200981117     C                   MOVEL     NUMFLT        SAVFLT
133300981117    1C                   ENDIF
133400981012     C*
133500090626      *Controllo validità  in base alle abilitazioni utente
133600090626     c                   clear                   tntaa1ds
133700090626     c                   eval      itaa1fil =numflt
133800090626     c                   eval      itaa1caut='§UTEPOT'
133900090626     c                   clear                   kpjbu
134000090626     c                   movel     tntaa1ds      kpjbu
134100090703     c                   call      'TNTAA1C'
134200090626     c                   parm                    kpjba
134300090626     c                   movel     kpjbu         tntaa1ds
134400090626     c
134500090626     c* Se non abilitato errore forzabile
134600090626     c                   if        otaa1fabi='N'
134700090626     c                             and  wdivas=' '
134800090626     C                   MOVEL     MSG(27)       VIDMSG
134900090626     C                   SETON                                        492890
135000090626     C                   MOVEL     'S'           WDIVAS            1
135100090626     C                   GOTO      ENDCT2
135200090626    1C                   ENDIF
135300090626     c
135400090626     c
135500981103     C* INCONGRUENZA COL CAP--> LNA STD O LNA OLTRE
135600981012    1C     NUMFLT        IFNE      O95LNA
135700981103    1C     NUMFLT        ANDNE     O95CLO
135800981012     C     UNO           ANDNE     '1'
135900981012     C                   SETON                                        492890
136000981012     C                   MOVEL     '1'           UNO
136100981012     C                   MOVEL     MSG(28)       VIDMSG
136200981012     C                   GOTO      ENDCT2
136300981012    1C                   ENDIF
136400981012     C*
136500981014     C                   MOVEL     ORGDES        DESFLT
136600981012     C**
136700080205     C** CATEGORIA merceologica
136800981012     C**
136900981012     C                   MOVEL(P)  VIDSCT        KEY
137000981012     C                   EXSR      CTRSCT
137100981012     C                   MOVEL     KEY           VIDSCT
137200981012     C                   MOVEL     WDES          DESSCT
137300981012     C   90              GOTO      ENDCT2
137400981012     C**
137500981012     C** CODICE COMMERCIALE
137600981012     C**
137700981026     C** OBBLIGATORIO PER STATO > 0 MA NON CALCOLO PIU' IN AUTOMATICO
137800981012     C** LEGGO L'ULTIMO STATO IMPOSTATO: SE 0 NON OBBLIGATORIO
137900981026     C**  SE IMMESSO A VIDEO L'ULTIMO STATO E' QUESTO
138000981012    1C     VIDCMM        IFEQ      *ZEROS
138100981012     C     VIDCMM        OREQ      *BLANKS
138200981027     C**
138300981027     C* SE NEL FILE C'ERA--> NON E' PIU' POSSIBILE CANCELLARLO
138400981104     C  N10CPOCMM        IFNE      *ZEROS
138500981027     C                   SETON                                        502890
138600981027     C                   MOVEL     MSG(29)       VIDMSG
138700981027     C                   GOTO      ENDCT2
138800981027    2C                   ENDIF
138900981111     C**
139000981111   X1C                   ELSE
139100981012     C**
139200981012     C                   EXSR      CTRCMM
139300981012     C   90              GOTO      ENDCT2
139400981027     C**
139500981027     C                   MOVEL     VIDCMM        W0030             3 0
139600981027     C     W0030         CHAIN     AZORG01L                           30
139700021029     C   30              MOVEL     W0030         ORGFEL
139800981027     C   30              CLEAR                   ORGDIT
139900981012     C**
140000130808      * NON può avere il flag di particolarità inserito in tab 01
140100130808     c                   if        CMMpar <> *blanks
140200981012     C                   SETON                                        502890
140300981012     C                   MOVEL     MSG(30)       VIDMSG
140400981012     C                   GOTO      ENDCT2
140500981027    2C                   ENDIF
140600111123
140700111123      /free
140800111123       //?Commerciale "valido" data inizio e fine attività
140900130808         IF  CMMdtIni > DateU or CMMdtFin < DateU;
141000111123           *in28 = *on;
141100111123           *in50 = *on;
141200111123           *in90 = *on;
141300111123           Vidmsg = msg(30);
141400111123           leavesr;
141500111123         ENDIF;
141600111123      /end-free
141700981112     C**
141800090626     c* se Diversi e l'utente è abilitato alla filiale  appartenenza
141900090626     c*              l'utente deve essere abilitato anche alla fil.mmerciale
142000130808     c*
142100981117    2C     W0030         IFNE      NUMFLT
142200130808     c*
142300981117    3C     WDIVAS        IFEQ      ' '
142400090626     c*
142500090626     c* Visto che qui non devo controllare l'unificante, passo la filiale
142600090626     c*  del commerciale per il controllo delle abilitazioni
142700090626     c                   clear                   tntaa1ds
142800090626     c                   eval      itaa1fil =w0030
142900090626     c                   eval      itaa1caut='§UTEPOT'
143000090626     c                   clear                   kpjbu
143100090626     c                   movel     tntaa1ds      kpjbu
143200090703     c                   call      'TNTAA1C'
143300090626     c                   parm                    kpjba
143400090626     c                   movel     kpjbu         tntaa1ds
143500130808     c*
143600090626    4c                   if        otaa1fabi='N'
143700981012     C                   SETON                                        502890
143800981012     C                   MOVEL     MSG(31)       VIDMSG
143900981012     C                   GOTO      ENDCT2
144000090626    4C                   ENDIF
144100090626    3C                   ENDIF
144200981117     C*
144300090626     C* SE l'utente non è abilitato alla filiale, devono essere per forza uguali
144400981117    3C     WDIVAS        IFEQ      'S'
144500981117     C                   SETON                                        502890
144600981117     C                   MOVEL     MSG(14)       VIDMSG
144700981117     C                   GOTO      ENDCT2
144800981117    3C                   ENDIF
144900981117    2C                   ENDIF
145000130808     c*
145100981012    1C                   ENDIF
145200091028    0C                   ENDIF
145300981012     C*
145400981012     C     ENDCT2        ENDSR
145500060330
145600920427     C***------------------------------------------------------------
145700920427     C** CARICAMENTO SCHIERA FIL
145800920427     C***------------------------------------------------------------
145900920427     C     CARTAB        BEGSR
146000940318     C*
146100970711     C                   TIME                    W0140            14 0
146200970711     C* UDATE IN GGMMAAAA
146300970711     C                   MOVE      W0140         WDTGIO            8 0
146400970711     C*
146500970711     C* UDATE IN AAAAMMGG
146600970714     C                   Z-ADD     WDTGIO        G08DAT
146700970714     C                   MOVEL     *BLANK        G08ERR
146800970711     C                   CALL      'XSRDA8'
146900970714     C                   PARM                    WLBDA8
147000970711     C                   MOVEL     G08INV        DATEU             8 0
147100050518     c                   Move      g08inv        data6             6 0
147200971203     C**
147300981103     C                   MOVEL     G08DAT        GMADAT            8 0
147400981012     C                   SETOFF                                       212223
147500981014     C                   SETOFF                                       242526
147600981012     C                   SETOFF                                       27
147700920430     C*
147800940321     C                   ENDSR
147900920427      *________________________________________________________________________
148000920427      *SELEZIONE FILIALE DI APPARTENENZA TRAMITE '?'
148100940317      *________________________________________________________________________
148200920427     C     SELFIL        BEGSR
148300941125     C                   CALL      'TNSD19R'
148400920427     C                   PARM                    §CODF             3
148500920427     C                   PARM                    §TIP              1
148600920427     C                   PARM                    §§DES            25
148700920427     C                   MOVEL     §CODF         VIDFLT
148800920427     C                   MOVEL     §§DES         DESFLT
148900920427     C                   ENDSR
149000940321      *-------------------------------------------------------------------------
149100960605      *AGGIORNO FILES
149200940321      *-------------------------------------------------------------------------
149300981014     C     AGGIOR        BEGSR
149400940321      *
149500940321     C                   MOVEL     VIDCP1        CPOCPO
149600940321     C                   MOVE      VIDCP2        CPOCPO
149700140714     c                   eval      CPO1cpo = CPOcpo
149800940321     C                   MOVE      VIDRAG        CPORAG
149900940321     C                   MOVE      VIDVIA        CPOVIA
150000940321     C                   MOVE      VIDCIT        CPOCIT
150100981014     C                   MOVEL     VIDCAP        CPOCAP
150200940321     C                   MOVE      VIDPRV        CPOPRV
150300981014     C                   MOVE      VIDNAZ        CPONAZ
150400140714     C                   MOVE      VIDTEL        CPO1TEL
150500140714     C                   MOVE      VIDFAX        CPO1FAX
150600080205     c                   eval      cpocdf=vidcdf
150700080205     c                   if        vidiva<>*blanks
150800080205     c                   if        vidivaeu=*blanks
150900080206     c                   movel     vidivait      cpopiv
151000080205     c                   else
151100080206     c                   movel     vidiva        cpopiv
151200080205     c                   endif
151300080205     c                   else
151400080206     c                   clear                   cpopiv
151500080205     c                   endif
151600080623     C                   MOVEL     VIDfsf        CPOfsf
151700080623     C                   MOVEL     VIDSCT        CPOSCT
151800940321     C                   MOVEL     VIDCMM        CPOCMM
151900100319     c                   eval      cpoftr = vidcic
152000981014     C* IMMISSIONE PRENDO  UDATE=DATA ACQUISIZIONE CLIENTE
152100981014££££ C   10              Z-ADD     DATEU         CPODAQ
152200940321     C                   MOVE      VIDFLT        CPOFLT
152300040805     c                   Eval      CpoDtr = dateu
152400010504      *
152500060714      * se non sono in annullamento devo reperire la nuova provincia dalla tabella PR e
152600060714      * memorizzarla sul file
152700080414     c***                if        not *inkq and savprv <> cpoprv
152800080414     c                   if        savprv <> cpoprv
152900060714     c                   clear                   dspr
153000060714     c                   eval      cod = 'PR'
153100060714     c                   eval      key = cpoprv
153200060714     c     ktab          chain     tabel00f
153300060714     c                   if        %found(tabel00f) and tblflg = *blanks
153400060714     c                   eval      dspr = tbluni
153500060714     c                   endif
153600060714     c                   if        §prnew <> *blanks
153700060714     c                   eval      cpoprv = §prnew
153800060714     c                   endif
153900060714     c                   endif
154000010301      * Aggiorno archivio potenziali
154100981127    1C     *IN10         IFEQ      *ON
154200080206     c                   clear                   CPOFTAZ
154300080206     c                   clear                   CPOAFAZ
154400080206     c                   clear                   CPOFFAZ
154500080206     c                   clear                   CPOPEXP
154600140930     c                   clear                   dcpo01
154700140930     c                   clear                   dcpo02
154800140930     c                   eval      cporst = dcpo01
154900110316     c                   eval      cpofls = 'M'
155000140930     c                   eval      §CPOannoia = *all'0'
155100140930     c                   eval      CPOtel = dcpo02
155200981014     C                   WRITE     TNCPO000
155300140714     C                   WRITE     TNCPO100
155400160705
155500160705      /free
155600160705       //?Scrivo Storico Variazioni
155700160705         exsr ScriviVar;
155800160705      /end-free
155900160705
156000090522     c*
156100140929     c******             movel     cpocpo        parcpo
156200140929     c******             call      'TRMK08R'
156300140929     c******             parm                    kpjba
156400140929     c******             parm                    parcpo           11
156500090522     c
156600940321   X1C                   ELSE
156700981014     C                   UPDATE    TNCPO000
156800140716     C  n13              UPDATE    TNCPO100
156900140716     C   13              write     TNCPO100
157000160705
157100160705      /free
157200160705       //?Scrivo Storico Variazioni
157300160705         exsr ScriviVar;
157400160705      /end-free
157500981127    1C                   ENDIF
157600140910
157700140910      /free
157800140910       //?Scrivo/aggiorno nota "DP"
157900140910           flag02 = 'P';
158000140910           clipot = %editc(CPOcpo:'X');
158100140910           ktnt = 'DP';
158200140910           chain (flag02:clipot:knk2:ktnt) TFNTC01L;
158300140910           IF  viddetp <> *blanks;
158400140910             IF  %found(TFNTC01L);
158500140910               NTCrnt = viddetp;
158600140910               NTCntr = data6;
158700140910               update TFNTC;
158800140910             ENDIF;
158900140910             IF  not %found(TFNTC01L);
159000140910               clear TFNTC;
159100140910               NTCapl = flag02;
159200140910               NTCnk1 = clipot;
159300140910               NTCtnt = ktnt;
159400140910               NTCrnt = viddetp;
159500140910               NTCsns = 'S';
159600140910               NTCntr = data6;
159700140910               write TFNTC;
159800140910             ENDIF;
159900140910           ELSE;
160000140910             IF  %found(TFNTC01L);
160100140910               delete TFNTC;
160200140910             ENDIF;
160300140910           ENDIF;
160400140910      /end-free
160500140910
160600130808      *
160700940321     C                   ENDSR
160800080205      *------------------------------------------------------------------------*
160900080205      * CONTROLLO CODICE FISCALE
161000080207      *------------------------------------------------------------------------*
161100080205     c     Sr_Ctrcdf     BegSr
161200080626     c                   movel     vidfsf        vid1fsf           1
161300130808      *
161400080205      * devo richiamare il nuovo driver fatto per controllare il codice
161500080205      * fiscale e la partita iva
161600151021     c                   clear                   xcfiva1ds
161700080205     c                   eval      xcfivapar = vidcdf
161800080205     c                   eval      xcfivanar = vidnaz
161900080205     c                   eval      xcfivaprv = vidprv
162000151021     c                   eval      xcfivaloc = vidcit
162100151021     c                   eval      xcfivacap = vidcap
162200151021     c                   eval      xcfivapiva = vidiva
162300151021     c                   call      'XCFIVAR1'
162400151021     c                   parm                    xcfiva1ds
162500080422     c
162600080207    2c                   if        xcfivaflg < *zeros
162700080205     c                   seton                                        562890
162800080422     c                   Eval      vidmsg = xcfivamsg
162900080205     c                   Leavesr
163000080207    2c                   EndIf
163100080422     c
163200080422      * Non inserito: Errore forzabile per clienti Italia se non previsto
163300080422      *     inserimento dai controlli xcfivar (xcfivaflg=9)
163400080422    1c                   if        vidcdf=*blanks and vidnaz=*blanks
163500080422     c                             and wforzacdf= *off  and xcfivaflg<>9
163600080422     c                   seton                                        562890
163700080422     c                   eval      vidmsg = msg(50)
163800080422     c                   eval      wforzacdf = *on
163900080422     c                   leavesr
164000080422    1c                   endif
164100080626     c
164200080206     c* lo stesso codice fiscale non deve essere già presente su altro
164300080206     c* potenziale sia come codice fiscale che come partita iva
164400080422    1c                   If        vidcdf <> *Blanks
164500130808      *
164600080626    2c                   if        (*in10 or vidcdf<>savcdf or
164700080626     c                              (vid1fsf<>cpofsf and vid1fsf='S'))
164800080207     c                             and vidcdf<>cod_forzcdf
164900080207     c                   eval      codice=vidcdf
165000080207     c                   exsr      sr_CdfAltroP
165100080207     c                   exsr      Sr_msgcdf
165200080207     c   90              leavesr
165300080626     c
165400080207    3c                   if        (vidivaeu=*blanks and vidcdf<>vidivait)
165500080207     c                             or (vidivaeu<>*blanks and vidcdf<>vidiva)
165600080207     c                   exsr      sr_PivAltroP
165700080207     c                   exsr      Sr_msgcdf
165800080207     c   90              Leavesr
165900080207    3c                   endif
166000080207    2c                   endif
166100080207    1c                   EndIf
166200130808      *
166300080205     c                   EndSr
166400130808      *
166500080205      *------------------------------------------------------------------------*
166600080205      * CONTROLLO PARTITA IVA E STATO
166700080205      *------------------------------------------------------------------------*
166800080205     c     Sr_Ctriva     BegSr
166900080626     c                   movel     vidfsf        vid1fsf           1
167000080205      * PARTITA IVA
167100080205      * Non inserita: Errore forzabile
167200080207    1c                   If        vidiva = *Blanks and wforzaiva=*off
167300080205     c                   seton                                        662890
167400080205     c                   eval      vidmsg = msg(51)
167500080205     c                   eval      wforzaiva = *on
167600080205     c                   leavesr
167700080207    1c                   EndIf
167800080205      * --> controllo
167900080207    1c                   If        vidiva <> *Blanks
168000151021     c                   clear                   xcfiva1ds
168100080205     c                   eval      xcfivamod = 'P'
168200080205     c                   eval      xcfivapar = vidiva
168300080205     c                   eval      xcfivanar = vidnaz
168400080205     c                   eval      xcfivaprv = vidprv
168500151021     c                   eval      xcfivaloc = vidcit
168600151021     c                   eval      xcfivacap = vidcap
168700151021     c                   eval      xcfivapiva = vidiva
168800151021     c                   call      'XCFIVAR1'
168900151021     c                   parm                    xcfiva1ds
169000080205      * --> errata forzabile
169100080207    2c                   If        xcfivaflg = -9 and wforzaiva1 = *off
169200080205     c                   seton                                        662890
169300080205     c                   Eval      vidmsg = xcfivamsg
169400080205     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
169500080205     c                   eval      wforzaiva1 = *on
169600080205     c                   Leavesr
169700080207    2c                   EndIf
169800080205      * --> errore
169900080207    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
170000080205     c                   seton                                        662890
170100080205     c                   eval      vidmsg = xcfivamsg
170200080205     c                   Leavesr
170300080207    2c                   EndIf
170400080205      * se non impostato devo riportare il codice iso della partita iva che
170500080205      * mi viene passato dal pgm di controllo
170600080207    2c                   if        vidivaeu <> ivaeu
170700080205     c                   eval      vidivaeu = ivaeu
170800080205     c                   eval      *in66 = *on
170900080205     c                   eval      *in90 = *on
171000080207    2c                   endif
171100080626     c
171200080207     c* la stessa partita Iva  non deve essere già presente su altro
171300080207     c* potenziale sia come partita iva che come codice fiscale
171400080626    2c                   if        (*in10 or vidiva<>saviva    or
171500080626     c                              (vid1fsf<>cpofsf and vid1fsf='S'))
171600080307     c                             and vidiva<>cod_forziva and vidivaeu<>'$$'
171700080208     c                   if        vidivaeu<>*blanks
171800080207     c                   eval      codice=vidiva
171900080208     c                   else
172000080208     c                   eval      codice=vidivait
172100080208     c                   endif
172200080207     c                   exsr      sr_PivAltroP
172300080207     c                   exsr      sr_msgiva
172400080207     c   90              leavesr
172500130808     c*
172600080207    3c                   if        (vidivaeu=*blanks and vidcdf<>vidivait)
172700080207     c                             or (vidivaeu<>*blanks and vidcdf<>vidiva)
172800080207     c                   exsr      sr_CdfAltroP
172900080207     c                   exsr      sr_msgiva
173000080207     c   90              leavesr
173100080207    3c                   endif
173200080207    2c                   endif
173300080207    1c                   EndIf
173400130808      *
173500080205     c                   EndSr
173600080207      *------------------------------------------------------------------------*
173700080207      * Controllo presenza codice su altro potenziale come cod.fiscale
173800080207      *------------------------------------------------------------------------*
173900080207     c     Sr_CdfAltroP  BegSr
174000080626     c                   clear                   savrag
174100080626     c                   clear                   savfsf
174200080207     c     codice        setll     tncpo05l
174300080207     c     codice        reade     tncpo05l
174400130808     c*
174500080229    3c                   dow       not %eof(tncpo05l)
174600080626     c*                                                                enz.
174700080626     c                   if        c_cpocpo<>cpocpo
174800080626     c                   if        savfsf<>'S'
174900080626     c                   eval      savfsf=c_cpofsf
175000080626     c                   eval      savrag=c_cporag
175100080626     c                   endif
175200080626     c                   endif
175300130808     c*
175400080207     c     codice        reade     tncpo05l
175500080207    3c                   enddo
175600080207     c                   endsr
175700080207      *------------------------------------------------------------------------*
175800080207      * Controllo presenza codice su altro potenziale come piva
175900080207      *------------------------------------------------------------------------*
176000080207     c     Sr_PivAltroP  BegSr
176100080207     c     codice        setll     tncpo06l
176200080207     c     codice        reade     tncpo06l
176300080229    3c                   dow       not %eof(tncpo06l)
176400080626     c*                                                                enz.
176500080626     c                   if        c_cpocpo<>cpocpo
176600080626     c                   if        savfsf<>'S'
176700080626     c                   eval      savfsf=c_cpofsf
176800080626     c                   eval      savrag=c_cporag
176900080626     c                   endif
177000080626     c                   endif
177100130808      *
177200080207     c     codice        reade     tncpo06l
177300080207    3c                   enddo
177400080207     c                   endsr
177500080207      *------------------------------------------------------------------------*
177600080207      * Messaggio di errore codice fiscal già presente per altro potenz
177700080207      *------------------------------------------------------------------------*
177800080207     c     Sr_msgcdf     BegSr
177900080626     c* Errore se trovato un altro potenziale con lo stesso cod.fiscale
178000080626    1c                   if        savrag<>*blanks
178100080626     c*  Se ne ho trovato uno di filiale o il pot aggiornato è di filiale
178200080626     c*                            --> msg forzabile
178300080626    2c                   if        savfsf='F' or vid1fsf='F'
178400080207     c                   eval      cod_forzcdf=codice
178500080207     c                   seton                                        562890
178600080207     c                   Eval      vidmsg = msg(1)
178700080626   x2c                   else
178800080626     c* se ne ho trovato uno di sede per pot di sede
178900080626     c*                            --> msg non forzabile
179000080626     c                   seton                                        562890
179100080626     c                   Eval      vidmsg = msg(16)
179200080626     c                   eval      %subst(vidmsg:19:15)=savrag
179300080626    2c                   endif
179400080626    1c                   endif
179500080207     c                   endsr
179600080207      *------------------------------------------------------------------------*
179700080207      * Messaggio di errore partita iva   già presente per altro potenz
179800080207      *------------------------------------------------------------------------*
179900080207     c     Sr_msgiva     BegSr
180000080626     c* Errore se trovato un altro potenziale con la stessa partita iva
180100080626    1c                   if        savrag<>*blanks
180200080626     c*  Se ne ho trovato uno di filiale o il pot aggiornato è di filiale
180300080626     c*                            --> msg forzabile
180400080626    2c                   if        savfsf='F' or vid1fsf='F'
180500080626     c                   eval      cod_forziva=vidiva
180600080626     c                   seton                                        662890
180700080626     c                   Eval      vidmsg = msg(2)
180800080626   x2c                   else
180900080626     c* se ne ho trovato uno di sede per pot di sede
181000080626     c*                            --> msg non forzabile
181100080626     c                   seton                                        662890
181200080626     c                   Eval      vidmsg = msg(42)
181300080626     c                   eval      %subst(vidmsg:19:15)=savrag
181400080626    2c                   endif
181500080626    1c                   endif
181600080207     c                   endsr
181700160705
181800160705      /free
181900160705       //--------------------------------------------------------------
182000160705       //?Scrivo l'immagine Precedente.
182100160705       //--------------------------------------------------------------
182200160705       BEGSR ImmaginePrima;
182300160705
182400160705         clear TRMK30DS;
182500160705         IMK30immag = 'P';
182600160831         IF  *in10;
182700160831           CPOrst = dCPO01;
182800160831         ENDIF;
182900160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
183000160705
183100160705       ENDSR;
183200160705       //--------------------------------------------------------------
183300160705       //?Scrivo Storico Variazioni.
183400160705       //--------------------------------------------------------------
183500160705       BEGSR ScriviVar;
183600160705
183700160705         clear TRMK30DS;
183800160705         IMK30pru = knmus;
183900160705         IMK30noj = knmeb;
184000160705         IMK30pgm = 'TRMK02R';
184100160705         IMK30immag = 'D';
184200160705
184300160705       //?Immissione
184400160705         IF  *in10;
184500160705           IMK30cta = 'I';
184600160705         ENDIF;
184700160705
184800160705       //?Manutenzione
184900160705         IF  not *in10;
185000160705           IMK30cta = 'M';
185100160705         ENDIF;
185200160705
185300160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
185400160705
185500160705       ENDSR;
185600160705      /end-free
185700160705
185800050629      *****************************************************************
185900050629      * ROUTINE INIZIALE
186000050629      *****************************************************************
186100050629     C     *INZSR        BEGSR
186200050629      *
186300050629     C     *ENTRY        PLIST
186400050629     C                   PARM                    KPJBA
186500091028     C                   PARM                    trmk01ds
186600100409     C                   PARM                    trmk19ds
186700130808      *
186800100409      * verifico se passati + parametri
186900100409     c                   If        %parms> 2
187000100409     c                   eval      W_daatti = 'S'
187100100409     c                   endif
187200130808      *
187300091028     C                   CLEAR                   mk01F12
187400091028     C                   CLEAR                   mk01F03
187500091028     C                   CLEAR                   mk01F06
187600091028     C                   CLEAR                   mk01ERR
187700091028     C                   CLEAR                   mk01M10
187800050629      *
187900050629     c     *dtaara       define    §azute        azuteds
188000050629     c     *dtaara       define    §datiute      ddatiute
188100050629     c                   in(E)     *dtaara
188200050629     c                   If        %error  or RSUT = *blanks
188300050629     c                   Clear                   Tibs34ds
188400050629     c                   Call      'TIBS34R'
188500050629     c                   Parm                    Tibs34ds
188600050629     c                   In        *dtaara
188700050629     c                   EndIf
188800100922     c
188900100922     c                   Movel     UteFaf        Dute01
189000130808      *
189100050629     C                   SETOFF                                       585957
189200050629      *
189300050629     C     SIMFEL        COMP      *ZERO                              8989
189400050629     C     SIMFEL        IFEQ      0
189500050629     C                   Z-ADD     046           SIMFEL
189600091029     C                   Endif
189700050629     C**
189800050629     C* CARICO TABELLE Ed ESEGUO OPERAZIONI INIZIALI
189900050629     C                   EXSR      CARTAB
190000050629     C                   MOVE      TES(1)        VIDTES
190100130808      *
190200050629     C                   EXSR      AZZERO
190300100202     C                   SETOFF                                       100302
190400140716     c                   setoff                                       13
190500130808      *
190600100202      * se primo campo kpjbu = 'S' è da menù
190700100202     c                   if        %subst(kpjbu:1:1) = 'S'
190800100202     c                   eval      *in03 = *on
190900100202     c                   endif
191000050629     C*
191100131126      * se secondo mpo kpjbu = 'S' è da menù
191200131126     c                   if        %subst(kpjbu:2:3) = 'LOG'
191300131126     c                   eval      $logistica = *on
191400131126     c                   endif
191500131126     C*
191600050629     C* SE C'E' IMPOSTO POTENZIALE PASSATO
191700091028     C     mk01K10       IFNE      ' '
191800091028     C     mk01K10       ANDNE     'I'
191900050629     C*
192000050629     C* 59 ON  - INIBISCO TASTI F7-VISITE / F16-ANNULLA / F12-RITORNO
192100091028     C     mk01K10       IFEQ      'A'
192200050629     C                   SETON                                        59
192300050629     C                   ENDIF
192400050629     C* IN INTERROGAZIONE SEMPRE CAMPI PROTETTI
192500091028     C     mk01K10       IFEQ      'N'
192600050629     C                   MOVE      TES(2)        VIDTES
192700091028     C                   SETON                                        02
192800050629     C                   ENDIF
192900050629     C*
193000091028     C                   MOVEL     mk01CPO       VIDCP1
193100091028     C                   MOVE      mk01CPO       VIDCP2
193200050629     C                   ENDIF
193300050629      *
193400050629     C* PARFLG  = "I" --> PROPONGO L'IMMISSIONE DEL POTENZIALE
193500091028     C     mk01K10       IFEQ      'I'
193600100120     C                   MOVEL     *ON           Immissione
193700050629     C                   ENDIF
193800130808      *
193900050629     C* DEFINIZIONE CHIAVI E CAMPI-------------------------------------*
194000050629     C     KTAB          KLIST
194100050629     C                   KFLD                    CODUT
194200050629     C                   KFLD                    COD               2
194300050629     C                   KFLD                    KEY               8
194400050629      *
194500050629     C     KTAB2         KLIST
194600050629     C                   KFLD                    CODUT
194700050629     C                   KFLD                    COD
194800050629      *
194900050629     C     KNOTE         KLIST
195000091028     C                   KFLD                    FLAG02            1
195100091028     C                   KFLD                    CLIPOT           11
195200130808      *
195300050629     c     knote01       klist
195400050629     c                   kfld                    flag02
195500050629     c                   kfld                    clipot
195600050629     c                   kfld                    knk2
195700050629     c                   kfld                    ktnt
195800130808      *
195900090518     C     Kcpi          klist
196000090518     C                   kfld                    CPocpo
196100090518     C                   kfld                    ktpf
196200130808      *
196300050629     C**
196400050629     C     *LIKE         DEFINE    TBLKUT        §KUT
196500050629     C     *LIKE         DEFINE    TBLCOD        §COD
196600050629     C     *LIKE         DEFINE    TBLKEY        §KEY
196700050629     C     *LIKE         DEFINE    CPOFLT        NUMFLT
196800050629     C     *LIKE         DEFINE    CPOFLT        SAVFLT
196900050629     C     *LIKE         DEFINE    VIDFLT        OLDFLT
197000050629     C     *LIKE         DEFINE    E14CAP        VARCAP
197100050629     C     *LIKE         DEFINE    E14LOC        VARLOC
197200050629     C     *LIKE         DEFINE    E14PRV        VARPRV
197300050629     C     *LIKE         DEFINE    E14NAR        VARNAR
197400130808      *
197500050629     C                   ENDSR
197600050629
197700920506      *------------------------------------------------------------------------
197800981130** TES
197900981123 *** GESTIONE  CLIENTI  POTENZIALI ***
198000981123 * INTERROGAZIONE CLIENTI POTENZIALI *
198100981123** ERR
198200080305Cod.fiscale già presente su altri potenziali. Enter per forzare               1
198300080305Partita Iva già presente su altri potenziali. Enter per forzare               2
198400960610Codice Potenziale non manutenzionabile perchè già in uso: riprovare più tardi!3
198500100319Immettere codice commerciale.                                                 4
198600100319                                                                              5
198700100319                                                                              6
198800100319                                                                              7
198900100319                                                                              8
199000100319                                                                              9
199100100319                                                                              10
199200100319                                                                              11
199300100319                                                                              12
199400100319                                                                              13
199500090626Se potenziale"ceduto"ad altra filiale,il COMMERCIALE deve appartenere alla fil
199600041207Potenziale non gestibile dall'utente                                          15
199700080626Presente pot.Sede"xxxxxxxxxxxxxxx"con stesso CodFiscale:modificare Cdf o Sd/Fi
199800981112Impossibile inserire un codice potenziale gia' esistente                      17
199900100319                                                                              18
200000981009Codice potenziale inesistente                                                 19
200100981012Codice commerciale inesistente                                                20
200200100319                                                                              21
200300100319                                                                              22
200400100319                                                                              23
200500981012Numero di telefono obbligatorio                                               24
200600080207Categoria merceologica inesistente o non utilizzabile                         25
200700090626Fil. di appartenenza errata                                                   26
200800090626Fil. di appartenenza immessa non in gestione all'utente. ENTER PER FORZARE    27
200900090626Fil. di appartenenza incongruente col CAP: ENTER per forzare                  28
201000100319Codice commerciale obbligatorio se gia'presente.                              29
201100981012Codice commerciale non utilizzabile                                           30
201200100319Codice commerciale non in gestione all'utente.                                31
201300100319                                                                              32
201400100319                                                                              33
201500100319                                                                              34
201600100319                                                                              35
201700100319                                                                              36
201800100319                                                                              37
201900080623Potenziale "S E D E" non modificabile                                         38
202000080623Potenziale " F I L I A L E" non modificabile                                  39
202100100319                                                                              40
202200100319                                                                              41
202300080626Presente pot.Sede"xxxxxxxxxxxxxxx"con stessa Par.IVA:modificare P.IVA o Sd/Fi 42
202400100319                                                                              43
202500100319                                                                              44
202600100319                                                                              45
202700050630Numeratore mancante : contattare EDP di sede!!!!                              46
202800100319                                                                              47
202900100319                                                                              48
203000100422Modifica non ammessa: la filiale xxx NON ha NUOVA GESTIONE TRATTATIVE attiva  49
203100100422Immettere Codice Fiscale . Enter per forzare                                  50
203200080205Immettere la Partita Iva. Enter per forzare                                   51
