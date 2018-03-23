000100020708     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200940517     H* TNTA01R *----------------------------------------------------*
000300940429     H*             MANUTENZIONE TARIFFE CLIENTI                     *
000400000000     H*--------------------------------------------------------------*
000500940517     FTNTA01D   CF   E             WORKSTN
000600940520     F                                     SFILE(TA01S03:NRR)
000700941012     FTNTAM01L  IF   E           K DISK
000800120229     ftntam00l  if   e           k disk    rename(tntam000:tntaml)
000900990913     FTITPT01L  IF   E           K DISK
001000990914     FTITGC01L  IF   E           K DISK
001100000000     FCNACO00F  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300100407     fTFNTC01L  if   e           k disk
001400100426     fazorg01l  if   e           k disk
001500040923
001600950519     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
001700020708     D COM             S              3    DIM(30)
001800950421     D*
001900940429     D TES             S             35    DIM(1) CTDATA PERRCD(1)
002000040923     D MSG             S             79    DIM(20) CTDATA PERRCD(1)
002100050530     d
002200050530     D TParEli         S              2    DIM(20)
002300050530     D TParDat         S              8  0 DIM(20)
002400050530     D ForzaTPar       S              2    DIM(20)
002500040924
002600040924      * P.O. gestibili dall'utente
002700040924     d fig             s              3    dim(250) inz(*Zeros)
002800020708
002900020708     d paxdut          s             30
003000020708     d paxdmt          s                   LIKE(ACOrag)
003100020708     d paxccc          s                   LIKE(ACOkcc)
003200020708     d paxsta          s              1  0
003300020708     d paxflr          s             90
003400020708     d paxdit          s              3
003500020708     d paxnum          s              2  0
003600020708     d paxkcm          s             80
003700020708     d paxksm          s            140
003800020708     d paxkdm          s             60
003900040923
004000040923     d codut           s              1  0 Inz(1)
004100040923     d kkut            s                   like(TblKut) Inz(1)
004200050530     d kctr            s                   like(tamctr)
004300040924     d w0050           s              5  0
004400050530     d
004500050530     d wdatadmy        s               d   datfmt(*dmy)
004600020708
004700940429     D*
004800940517     D* RICHIAMO PGM DI GESTIONE TARIFFE CLIENTI      - TNTA01R2 -
004900000000     D PARAM           DS
005000941011     D* CODICE CLIENTE
005100940429     D  PARCLI                 2      8  0
005200941011     D* PROGRESSIVO TARIFFA
005300940429     D  PARPRG                 9     11  0
005400941011     D* CAPOCONTO CLIENTI
005500940429     D  PARKCI                12     15  0
005600941011     D* RAGIONE SOCIALE CLIENTE
005700940520     D  V2DKSC                16     63
005800941011     D* CODICE TARIFFA
005900940429     D  PARCTR                64     66
006000940523     D* CODICE CLIENTE NELLA GESTIONE VISITE/OFFERTE
006100940429     D  PARKSC                67     73    INZ
006200941011     D* PARFLG = '5' ---> PROVENGO DA "CMD9-AGGIUNTA TARIFFA"
006300940429     D  PARFLG                74     74
006400941011     D* PARALL = 'A' ---> RECORD ALLOCATO: TARIFFA NON MANUTENZIONABILE
006500941011     D  PARALL                95     95
006600990913     D* FLAG DI RITORNO  DAI PROGRAMMI CHIAMATI
006700990913     D* 'C' FINE PGM
006800990913     D* 'R' RITORNO AL SUBFILE
006900990913     D  FLGRTN               162    162
007000031118     D* PARIST = 'E' ---> SCATTO ISTAT INESISTENTE ERRORE
007100031118     D  PARIST               178    178
007200940429     D*
007300030703     D WLBDAT          DS
007400030703     D  G02DAT                 1      8  0
007500030703     D  G02INV                 9     16  0
007600030703     D  G02ERR                17     17
007700030703     D  G02TGI                18     22  0
007800040923
007900940729     D                 DS
008000940729     D  AA                     1      4  0
008100100407     d  aa2                    3      4  0
008200940729     D  MM                     5      6  0
008300940729     D  GG                     7      8  0
008400940729     D  DATA                   1      8  0
008500040923
008600000000     D KPJBA         E DS
008700991022     D DSTA01        E DS
008800100426     d OG141         e ds
008900030703
009000030703     d tibs02ds      e ds
009100030703     d DMTC          e ds
009200040923
009300100407     d Azuteds       e ds                  extname(Azute00f)
009400040923     d dDatiute      e ds
009500050530     d DS1P          e ds
009600040923     d Tibs34ds      e ds
009700040923     d Trul31ds      e ds
009800050818     d Tnta25ds      e ds
009900050818     d Tnta25tad     e ds                  inz
010000050818     d Tnta25tgc     e ds                  inz
010100050818     d Tnta25tpd     e ds                  inz
010200050818     d Tnta25tpt     e ds                  inz
010300060207     d Tnta60ds      e ds
010400100427     d TNTA28DS      e ds
010500090629     d
010600090629     d* Controllo abilitazioni utente
010700090629     d tntaa1ds      e ds
010800130124     d dUTE01        e ds
010900030703
011000030703     d Dataiso         s               d   datfmt(*iso)
011100030703     d Dataoggi        s              8  0
011200030703     d Datascad        s              8  0
011300030703     d wa              s              2  0
011400030703     d wgiorni         s              3  0
011500050818     d wfie            s                   like(tamfie)
011600120223
011700120314       // -?ds dati passati da TNSB48R - Manca Tariffa
011800120314     d TNSB48ds      e ds
011900120315     d Parm48          s                   like(tnsb48ds)
012000120223
012100120305       // -?Flag booleani
012200120305     d wFine           s               n   inz(*off)
012300100427
012400100427      //---------------------------------------------------------------
012500100427      //?prototipi
012600100427      //---------------------------------------------------------------
012700120314      /copy gaitrasrc/srcprotopr,tnta01r2
012800100427      /copy gaitrasrc/srcprotopr,tnta28r
012900030703
013000940429     C*****************************************************************
013100940429     C* RIEPILOGO INDICATORI
013200940429     C*****************************************************************
013300940520     C* 05    - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
013400950414     C* 08    - QUALSIASI TASTO VENGA PREMUTO --> ESCO DAL PROGRAMMA
013500940520     C* 10    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
013600950118     C* 18    - USATO PER RIEMETTERE LA VIDEATA DEL SFL DI SCELTA
013700950118     C*         TARIFFA NEL CASO IN CUI IL RECORD SIA ALLOCATO
013800940520     C* 20    - PULIZIA SUBFILE (SFLCLR)
013900940520     C* 24    - DELETE  SUBFILE (SFLDLT)
014000940523     C* 27    - RICERCA ALFABETICA
014100940520     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
014200940520     C* 29    - LETTURA SUBFILE (READC)
014300940520     C* 30    - DI COMODO
014400940520     C* 31    - LETTURA PER CARICAMENTO SUBFILE
014500941013     C* 35    - USATO PER LOKUP E TESTN
014600941005     C* 36    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD: SE
014700941005     C*         IL RECORD E' ALLOCATO (CIOE' *IN36 ACCESO), EMETTO UN
014800941005     C*         MESSAGGIO PER AVVERTIRE CHE LA TARIFFA E' GIA' IN USO
014900940520     C* 37    - USATO PER REPERIRE DTAARA PER PASSWORD
015000940520     C* 40/41 - ERRORI
015100940520     C* 42    - USATO PER EMETTERE MESSAGGIO: "PER INSERIRE UN NUOVO
015200940520     C*         PROGRESSIVO BISOGNA DIGITARE IL CODICE TARIFFA"
015300120305      * 60    - Pgm richiamato (no F09 sì F01)
015400030703     c* 66    - Gestione password
015500940429     C*****************************************************************
015600921104     C*
015700940429     C     INIZIO        TAG
015800120229
015900120229       // -?Se richiamato
016000120314     c                   IF        *in60
016100120229       // -?imposto i dati del cliente
016200120314     c                   eval      v2cksc = ISB48ksc
016300120314     c                   eval      v2dksc = ISB48rag
016400120229       // -?controllo se esiste la tariffa
016500120229     c     V2Cksc        setll     TNTAM00L
016600120229    1c                   IF        not %equal(TNTAM00L)
016700120314     c                   eval      OSB48err = '1'
016800120314     c                   eval      OSB48msg = 'Non trovata tariffa'
016900120229     c                   goto      fine
017000120229     c                   ENDIF
017100120229       // -?mi posiziono sul file e vado subito in seconda videata
017200120229     c     V2Cksc        setll     TNTAM01L                               30
017300120229     c                   goto      for02
017400120229     c                   ENDIF
017500120224
017600950118     C                   SETOFF                                       18
017700940520     C                   CLEAR                   V2CKSC
017800940520     C                   CLEAR                   V2CCTR
017900940520     C                   CLEAR                   V2DKSC
018000940502     C                   CLEAR                   PARFLG
018100050530     C                   CLEAR                   forzatpar
018200050530     c* indice dedicato per la skiera forzatpar: non usare per altro
018300050530     C                   CLEAR                   yy                4 0
018400921104     C*
018500940429     C     FOR01         TAG
018600940520     C                   WRITE     TA01T01
018700940520     C                   EXFMT     TA01D02
018800940429     C*
018900940523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
019000940523     C                   CLEAR                   V2CMSG
019100940523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
019200940523     C                   SETOFF                                       4041
019300940523     C                   SETOFF                                       4228
019400030703     c                   SETOFF                                       66
019500940429     C*
019600940429     C** CMD3  - FINE LAVORO
019700950414     C   KC
019800950414     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
019900090701     c**          perchè utente non abilitato
020000950414     COR 08              GOTO      FINE
020100950414     C*
020200940429     C** CMD7  - AGGIORNAMENTO ANAGRAFICO PIANO DEI CONTI
020300940429     C     *INKG         IFEQ      *ON
020400060207     c                   Clear                   tnta60ds
020500060207     c                   Call      'TNTA60R'
020600060207     c                   Parm                    kpjba
020700060207     c                   Parm                    Tnta60ds
020800940429     C                   GOTO      FOR01
020900940429     C                   ENDIF
021000921104     C*
021100940429     C** CONTROLLI FORMATO
021200940520     C                   EXSR      CTRD02
021300940523     C*
021400940523     C   27
021500940523     COR 28              GOTO      FOR01
021600940429     C*
021700941005     C     FOR02         TAG
021800921104     C*
021900940429     C                   Z-ADD     0             PARPRG
022000940429     C                   MOVEL     *BLANKS       PARCTR
022100940429     C*
022200940429     C* 30 OFF - IMMISSIONE
022300940429     C* 30 ON  - AGGIORNAMENTO
022400940429     C*
022500940429    1C     *IN30         IFEQ      *OFF
022600950118     C     *IN18         ANDEQ     *OFF
022700940520     C                   MOVEL     V2CCTR        PARCTR
022800940429     C                   Z-ADD     0             PARPRG
022900921104     C*
023000940429   X1C                   ELSE
023100940429     C*
023200940429     C* CARICAMENTO SUBFILE
023300950118     C  N18              EXSR      RIES03
023400100427
023500921104     C*
023600940429     C* NRR = 0  --->  NESSUN PROGRESSIVO VALIDO CARICATO A VIDEO:
023700940429     C*                VADO IN IMMISSIONE CON L'ULTIMO LETTO + 1
023800940429    2C     NRR           IFEQ      0
023900940429     C*
024000941013    3C     *IN05         IFEQ      *OFF
024100941013     C                   MOVEL     MSG(5)        V2CMSG
024200941013     C                   SETON                                        4028
024300941013     C                   GOTO      FOR01
024400941013    3C                   ENDIF
024500941013     C*
024600940520     C                   MOVEL     V2CCTR        PARCTR
024700940523     C     WPRGLE        ADD       1             PARPRG
024800921104     C*
024900940429   X2C                   ELSE
025000940429     C* NRR <> 0
025100990913     C*
025200990913     C     FOR03         TAG
025300940429     C*
025400940429     C* EMETTO SUBFILE
025500940520     C                   WRITE     TA01T01
025600940520     C                   WRITE     TA01Z04
025700940520     C                   EXFMT     TA01C03
025800941012     C*
025900941012     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
026000941012     C                   CLEAR                   VCCMSG
026100941012     C                   SETOFF                                       28
026200941110     C*
026300941110     C** CMD 3 - FINE
026400941110     C   KC              GOTO      FINE
026500120224
026600120224       //?Se richiamato con F12 esco dal pgm
026700120314     c                   IF        *in60 and *inkl
026800120224     c                   goto      fine
026900120224     c                   ENDIF
027000940429     C*
027100940429     C** CMD12 - RITORNO
027200940429     C   KL              GOTO      INIZIO
027300100427
027400120305      /free
027500120305       //?F1 - dati manca tariffa
027600120305        IF  *inka;
027700120305          exsr gesw01;
027800120305      /end-free
027900120305     c  N05V2CKSC        SETLL     TNTAM000                               30
028000120305     c   05KTAM          SETLL     TNTAM000                               30
028100120305     c                   exsr      ries03
028200120305     c                   GOTO      for03
028300120305     c                   ENDIF
028400100427      /free
028500100427       //?F8 - Note tariffa
028600110105        IF  *inkh;
028700100427          clear TNTA28ds;
028800100427          ITA28ela = 'M';
028900100427          ITA28tip = 'C';
029000100427          ITA28cod = V2cksc;
029100100427          ITA28rsc = V2dksc;
029200100427          kpjbu    = TNTA28DS;
029300100427          tnta28r (kpjba);
029400100427      /end-free
029500100427     c  N05V2CKSC        SETLL     TNTAM000                               30
029600100427     c   05KTAM          SETLL     TNTAM000                               30
029700100427     c                   exsr      ries03
029800100427     c                   GOTO      for03
029900100427     c                   ENDIF
030000100427
030100940429     C*
030200050530     C** F9  - AGGIUNTA TARIFFA
030300940517    3C     *INKI         IFEQ      '1'
030400130125       //?se utente non abilitato alla modifica tariffe non do la
030500130125       //?possibilità di aggiungere un nuovo progressivo
030600130125       //?non controllo in caso di tariffa di cartello perchè se
030700130125       //?l'utente arriva in questo punto e sta modificando una tariffa
030800130125       //?di cartello vuol dire che è abilitato a fare tutto
030900130125       //?il controllo sulla possibilità di gestire la cartello viene
031000130125       //?fatto prima
031100130125     c                   Movel     v2cksc        w0050
031200130528     c                   IF        w0050 <> 88888 and
031300130125     c                             §UTEmodtar = *blanks
031400130125     c                   eval      VCCmsg = msg(01)
031500130125     c                   eval      *in28 = *on
031600130125     c                   goto      for03
031700130125     c                   ENDIF
031800940429     C*
031900940429     C* SE NON IMMESSO IL CODICE TARIFFA IN PRIMA VIDEATA ---> ERRORE
032000940520    4C     *IN05         IFEQ      *OFF
032100940520     C                   SETON                                        42
032200940523     C                   GOTO      FOR01
032300940429    4C                   ENDIF
032400940429     C*
032500940429     C**  D U P L I C A   T A R I F F A  **
032600050530     c* prima di duplicare, avviso se ci sono delle tariffe partic in scaden
032700050530     c*  za da eliminare che non verranno copiate
032800050530     c                   z-add     1             xx
032900050530     c                   movel     v2cctr        kctr
033000050530    1c                   dow       tpareli(XX) <>'  '
033100050530     c     ktpt2         chain     titpt01l
033200050530    2c                   if        %found(titpt01l) and tptatb=' '
033300050530   2ac                   if        vshdst>=tpardat(XX)
033400050530     c
033500050530     c* Errore  forzabile se non già dato
033600050530     c     tpareli(XX)   lookup    forzatpar                              31
033700050530    3c                   if        not *in31
033800050530     c                   eval      vccmsg=msg(17)
033900050530     c                   eval      %subst(vccmsg:26:2)=tpareli(XX)
034000050530     c     *iso          move      tpardat(XX)   wdatadmy
034100050530     c                   move      wdatadmy      wdata             6 0
034200050530     c                   eval      %subst(vccmsg:49:8) =(%editw(wdata:'  /  / -
034300050530     c                              '))
034400050530     c
034500050530     c                   add       1             yy
034600050530     c                   movel     tpareli(xx)   forzatpar(YY)
034700050530     C                   SETON                                        28
034800050530     c                   goto      for03
034900050530    3c                   endif
035000050530   2ac                   endif
035100050530    2c                   endif
035200050530     c                   add       1             xx
035300050530    1c                   enddo
035400050530     c
035500050818      * Richiamo al nuovo pgm di copia tariffe
035600050818     c                   Clear                   Tnta25ds
035700050818     c                   Eval      ta25ta01 = '1'
035800050818     c                   Eval      ta25inglo = 'N'
035900050818     c                   Eval      ta25tipo = 'T'
036000050818     c                   Eval      ta25ksco = v2cksc
036100050818     c                   Move      v2cctr        ta25ctro
036200050818     c                   Eval      ta25prgo = wprgul
036300050818     c                   Eval      ta25tipn = 'T'
036400050818     c                   Eval      ta25kscn = v2cksc
036500050818     c                   Move      v2cctr        ta25ctrn
036600050818     c                   Eval      ta25tam = 'S'
036700050818     c                   Eval      ta25tad = 'T'
036800050818     c                   Eval      ta25tpt = 'T'
036900050818     c                   Eval      ta25tpd = 'N'
037000050818     c                   Eval      ta25tgc = 'T'
037100050818     c                   Eval      ta25cat = 'S'
037200050818     c                   Eval      ta25fie = tamfie
037300050818     c                   Call      'TNTA25R'
037400050818     c                   Parm                    kpjba
037500050818     c                   Parm                    Tnta25ds
037600050818     c                   Parm                    Tnta25tad
037700050818     c                   Parm                    Tnta25tgc
037800050818     c                   Parm                    Tnta25tpd
037900050818     c                   Parm                    Tnta25tpt
038000050818
038100050818      * Riporto da tnta25r l'eventuale errore se manca lo scaglione ISTAT
038200050818     c                   Move      kpjbu         pa2err            3
038300031119     C     PA2ERR        IFEQ      'IST'
038400031119     C                   MOVEL     MSG(16)       V2CMSG
038500031119     C                   SETON                                        4028
038600031119     C                   GOTO      FOR01
038700031119    3C                   ENDIF
038800050818      * Se errore da copia tariffa lo emetto a video
038900050818     c                   If        ta25err <> *Blanks
039000050818     c                   Eval      v2cmsg = ta25msg
039100050818     c                   Eval      *In28 = *On
039200050818     c                   Eval      *In40 = *On
039300050818     c                   Goto      for01
039400050818     c                   EndIf
039500050818      * nessun errore imposto i campi per il richiamo al pgm di manutenzione
039600050818     c                   Move      ta25ctrn      parctr
039700050818     c                   Move      ta25prg       parprg
039800050818
039900940517     C* PARFLG = '5' ---> PROVENGO DA "CMD9-AGGIUNTA TARIFFA" QUINDI
040000940517     C*                   NEL PGM TNTA01R2 NON ABILITO IL CMD12 NELLA
040100940517     C*                   VIDEATA DELLE CONDIZIONI GENERALI TARIFFARIE
040200940429     C                   MOVEL     '5'           PARFLG
040300900320     C*
040400940429   X3C                   ELSE
040500940429     C* *INKI OFF - CONTROLLO SE E' STATA SELEZIONATA UNA TARIFFA
040600940517     C*
040700940520     C                   READC     TA01S03                                29
040800940429     C*
040900940520    4C     *IN29         IFEQ      *ON
041000940429     C* CON ENTER VIENE PROPOSTA L'ULTIMA TARIFFA ELENCATA NEL SUBFILE
041100940520     C                   MOVEL     WCTRUL        PARCTR
041200940520     C                   Z-ADD     WPRGUL        PARPRG
041300940429   X4C                   ELSE
041400941005     C* CON "1" O BARRA SPAZ. VIENE PROPOSTA LA TARIFFA SELEZIONATA
041500940520     C                   MOVEL     VSCCTR        PARCTR
041600940520     C                   Z-ADD     VSCPRG        PARPRG
041700940429    4C                   ENDIF
041800940429     C*
041900940429    3C                   ENDIF
042000940429    2C                   ENDIF
042100940429    1C                   ENDIF
042200941005     C*
042300940429     C******
042400940517     C*** T N T A 0 1 R 2
042500940429     C******
042600940520     C                   MOVEL     V2CKSC        PARCLI
042700000000     C                   MOVEL     PARAM         KPJBU
042800120314       //?se non richiamato faccio richiamo semplice al pgm gestione dettaglio
042900120314     c                   IF        not *in60
043000940517     C                   CALL      'TNTA01R2'
043100000000     C                   PARM                    KPJBA
043200120314     C                   ELSE
043300120314       //?se richiamato devo passare la DS del TNSB48R al TNTA01R2
043400120314      /free
043500120314         tnta01r2 (kpjba:TNSB48DS);
043600120314      /end-free
043700120314
043800120314     C                   ENDIF
043900941011     C                   MOVEL     KPJBU         PARAM
044000940429     C*
044100950118     C* SE RECORD ALLOCATO EMETTO MESSAGGIO
044200941011     C     PARALL        IFNE      ' '
044300941012     C                   MOVEL     MSG(4)        VCCMSG
044400950118     C                   SETON                                        2818
044500941011     C                   GOTO      FOR02
044600941011     C                   ENDIF
044700031118     C* SE MANCA SCATTO ISTAT ERRORE
044800031118     C     PARIST        IFNE      ' '
044900031118     C                   MOVEL     MSG(16)       V2CMSG
045000031118     C                   SETON                                        2840
045100120224       //?Se richiamato torno al subfile
045200120314     c                   IF        *in60
045300120224     c                   goto      for02
045400120224     c                   ENDIF
045500031118     C                   GOTO      FOR01
045600031118     C                   ENDIF
045700011206     C* PULISCO IL FLAG DI AGGIUNTA PROGRESSIVO
045800011206     C                   CLEAR                   PARFLG
045900941011     C*
046000990913     C* CONTROLLO IL FLAG DI RITORNO
046100990913     C* SE E' UGUALE A "R" RITORNO AL SUBFILE ALTRIMENTI VADO ALL'INIZIO
046200990913     C     FLGRTN        IFEQ      'R'
046300990914     C  N05V2CKSC        SETLL     TNTAM000                               30
046400990914     C   05KTAM          SETLL     TNTAM000                               30
046500990913     C                   EXSR      RIES03
046600990913     C                   GOTO      FOR03
046700990913     C                   ELSE
046800940429     C                   GOTO      INIZIO
046900990913     C                   ENDIF
047000990913     C*
047100000000     C     FINE          TAG
047200120315     c                   IF        *in60
047300120315     c                   eval      parm48 = tnsb48ds
047400120315     c                   ENDIF
047500000000     C                   SETON                                        LR
047600090629     c
047700940429     C*
047800940520     C* DELETE SUBFILE
047900940520     C                   SETON                                        24
048000940520     C                   WRITE     TA01C03
048100940520     C                   SETOFF                                       24
048200921102     C*
048300940429     C*--- CONTROLLI FORMATO -----------------------------------------*
048400940520     C     CTRD02        BEGSR
048500040924
048600940523     C                   SETOFF                                       2705
048700040923     C                   Z-ADD     dutkci        PARKCI
048800940429     C*
048900940429     C****  RICERCA ALFABETICA  ****
049000940429     C* SE CODICE CLIENTE A ZERO RICERCA ALFABETICA
049100940520    1C     V2CKSC        IFEQ      0
049200941128     C*
049300941128     C* SE ANCHE IL CAMPO DELLA RICERCA E' VUOTO ---> ERRORE
049400941128    2C     V2DKSC        IFEQ      *BLANKS
049500941128     C                   MOVEL     MSG(6)        V2CMSG
049600941128     C                   SETON                                        4028
049700941128     C                   GOTO      ENDCTR
049800941128    2C                   ENDIF
049900941128     C*
050000040927     c                   Z-add     1             xx
050100040924     c                   Clear                   com
050200040924     c     *zeros        Lookup    fig(xx)                                30
050300041008     c                   If        *In30 and xx <= 30
050400050530     C                   DO        30            XX
050500020708     C                   MOVE      FIG(XX)       COM(XX)
050600020708     C                   ENDDO
050700040924     c                   EndIf
050800020708     c                   movea     com           paxflr
050900020708
051000040924     c                   movel     rsut          paxdut
051100020708     c                   z-add     9             paxsta
051200040923     c                   z-add     dutkci        paxccc
051300020708     c                   movel(P)  v2dksc        paxdmt
051400020708     c                   z-add     1             paxnum
051500020708
051600020708     c                   call      'XALFA3BR'
051700020708     c                   parm                    paxdut
051800020708     c                   parm                    codut
051900020708     c                   parm                    paxdmt
052000020708     c                   parm                    paxccc
052100020708     c                   parm                    paxsta
052200020708     c                   parm                    paxflr
052300020708     c                   parm      *blanks       paxdit
052400020708     c                   parm                    paxnum
052500020708     c                   parm                    paxkcm
052600020708     c                   parm                    paxksm
052700020708     c                   parm                    paxkdm
052800020708
052900020708      * Non trovato o premuto F12
053000020708    2c                   if        paxsta > -1
053100020708     c                   movel     paxksm        v2cksc
053200020708    2c                   endif
053300020708
053400020708     c                   SETON                                        27
053500020708     c                   GOTO      ENDCTR
053600020708    1c                   ENDIF
053700040506     c*
053800040506     C****  CODICE CLIENTE  ****
053900040506     C                   Z-ADD     V2CKSC        WKSC
054000040506     C     KACO          CHAIN     CNACO000                           30
054100040506     C  N30ACOFLG        COMP      '*'                                    30
054200040506     C*
054300040506     C     *IN30         IFEQ      *ON
054400040506     C                   MOVEL     MSG(2)        V2CMSG
054500040506     C                   SETON                                        4028
054600040506     C                   GOTO      ENDCTR
054700040506     C                   ENDIF
054800040506     C*
054900040506     C* DECODIFICA CODICE CLIENTE
055000040506     C                   MOVEL     ACORAG        V2DKSC
055100940429     C*
055200940429     C****  CONTROLLO SE CONTO GESTIBILE  ****
055300090629     c                   clear                   tntaa1ds
055400120229     c                   eval      ITAA1tla = 'L'
055500090629     c                   eval      itaa1caut='§utegtc'
055600090629     c                   eval      itaa1ksc=v2cksc
055700090629     c                   clear                   kpjbu
055800090629     c                   movel     tntaa1ds      kpjbu
055900090703     c                   call      'TNTAA1C'
056000090629     c                   parm                    kpjba
056100090629     c                   movel     kpjbu         tntaa1ds
056200090629     c
056300090629    1c                   if        otaa1fabi='N'
056400090629      * per tariffa di cartello controllo se l'utente è autorizzato
056500090629     c                   Movel     v2cksc        w0050
056600090629    2c                   If        w0050 = 88888
056700090629     c                   Eval      v2cmsg = msg(7)
056800090629     c                   Eval      %subst(v2cmsg:44:11) = 'di cartello'
056900090629     c                   else
057000090629     C                   MOVEL     MSG(8)        V2CMSG
057100090629    2c                   endif
057200090629     c
057300090629     c                   Seton                                        4028
057400090629     c                   goto      endctr
057500090629    1c                   endif
057600940429     C*
057700940429     C****  CODICE TARIFFA  ****
057800940429     C* CONTROLLO TIPO TARIFFA SE IMMESSO
057900940520    1C     V2CCTR        IFNE      *BLANKS
058000941013     C*
058100941013     C                   TESTN                   V2CCTR               35
058200941013     C*
058300941013     C* CONTROLLO CHE L'ULTIMO CARATTERE DEL CODICE TARIFFA NON SIA
058400941013     C*   UNA LETTERA
058500941013     C   35              MOVE      V2CCTR        W001              1
058600941013    2C   35W001          IFLT      '0'
058700941013     C                   SETOFF                                       35
058800941013    2C                   ENDIF
058900941013     C*
059000941013    2C     *IN35         IFEQ      *OFF
059100941013     C                   MOVEL     MSG(3)        V2CMSG
059200941013     C                   SETON                                        4128
059300941013     C                   GOTO      ENDCTR
059400941013    2C                   ENDIF
059500941013     C*
059600940520     C                   MOVEL     V2CCTR        W0010             1 0
059700940523     C                   MOVEL     'TR'          KCOD
059800940523     C                   MOVEL     *BLANKS       KKEY
059900940523     C                   MOVEL     W0010         KKEY
060000940502     C     KTAB          CHAIN     TABEL                              30
060100941013    2C     *IN30         IFEQ      *ON
060200940520     C                   MOVEL     MSG(3)        V2CMSG
060300940520     C                   SETON                                        4128
060400940520     C                   GOTO      ENDCTR
060500941013    2C                   ENDIF
060600940429     C*
060700940520     C* 05 ON  - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
060800940520     C                   SETON                                        05
060900940520     C                   MOVEL     V2CCTR        WCTR
061000940429    1C                   ENDIF
061100940429     C*
061200941013     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
061300941013    1C     *IN05         IFEQ      *OFF
061400941013     C     V2CKSC        SETLL     TNTAM000                               30
061500941013    2C     *IN30         IFEQ      *OFF
061600941013     C                   MOVEL     MSG(5)        V2CMSG
061700941013     C                   SETON                                        4028
061800941013     C                   GOTO      ENDCTR
061900941013    2C                   ENDIF
062000941013     C*
062100941013   X1C                   ELSE
062200941013     C* 05 ON  - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
062300941013     C     KTAM          SETLL     TNTAM000                               30
062400941013    1C                   ENDIF
062500030703      *
062600030703      * controllo password
062700030703      *
062800030703     c                   If        V2cpwd = *Blanks
062900030703     c                   Seton                                        6628
063000030703     C                   movel     msg(14)       V2CMSG
063100030703     c                   GoTo      EndCtr
063200030703     c                   EndIf
063300030703      * stessa password
063400030703     c                   If        V2cpwd <> §MTCPwd
063500030703     c                   Seton                                        6628
063600030703     C                   movel     msg(12)       V2CMSG
063700030703     c                   GoTo      EndCtr
063800030703     c                   EndIf
063900030703      * data immissione/variazione password
064000030703     c                   Clear                   wlbdat
064100030703     c                   Z-Add     §MtcDta       G02Dat
064200030703     c                   Call      'XSRDA8'
064300030703     c                   Parm                    Wlbdat
064400030703     c                   Movel     G02Inv        Dataiso
064500030703      * Scadenza password
064600030703     c                   Clear                   Tibs02ds
064700030703     c                   Eval      T02Mod = 'C'
064800030703     c                   Eval      T02Sif = knsif
064900030703     c                   Eval      T02Cod = 'MTC'
065000030703     c                   Movel(p)  'PSW'         T02Ke1
065100030703     c                   Call      'TIBS02R'
065200030703     c                   Parm                    Kpjba
065300030703     c                   Parm                    Tibs02ds
065400030703If  2c                   If        T02Err <> *Blanks
065500030703     c                   Seton                                        6628
065600030703     C                   movel     msg(15)       V2CMSG
065700030703     c                   GoTo      EndCtr
065800030703   x2c                   Else
065900030703     c                   Movel     T02Uni        wgiorni
066000030703     c                   Adddur    wgiorni:*d    Dataiso
066100030703     c                   Move      Dataiso       Datascad
066200030703     c                   Z-Add     *Date         Dataoggi
066300030703     c                   If        Dataoggi > DataScad
066400030703     c                   Seton                                        6628
066500030703     C                   movel     msg(13)       V2CMSG
066600030703     c                   GoTo      EndCtr
066700030703     c                   EndIf
066800030703    2c                   EndIf
066900941013     C*
067000940520     C     ENDCTR        ENDSR
067100940429     C*
067200940429     C*--- CARICAMENTO SUBFILE ---------------------------------------*
067300940520     C     RIES03        BEGSR
067400940520     C*
067500940520     C* RIEMPO CAMPI DEL SFLCONTROL
067600940520     C                   MOVEL     V2CKSC        VCCKSC
067700940520     C                   MOVEL     V2DKSC        VCDKSC
067800940520     C*
067900940429     C* PULIZIA SUBFILE
068000940520     C                   SETON                                        20
068100940520     C                   WRITE     TA01C03
068200940520     C                   SETOFF                                       20
068300940429     C*
068400940429     C                   Z-ADD     0             NRR               4 0
068500940429     C*
068600940615     C   05KTAM          READE     TNTAM000                               31
068700940615     C  N05V2CKSC        READE     TNTAM000                               31
068800940429     C*
068900940520    1C     *IN31         DOWEQ     *OFF
069000940429     C*
069100940429     C* ULTIMO PROGRESSIVO LETTO (COMPRESI QUELLI ANNULLATI)
069200940523     C                   Z-ADD     TAMPRG        WPRGLE
069300940429     C*
069400940429     C* ESCLUDO RECORD ANNULLATI
069500940429    2C     TAMATB        IFEQ      ' '
069600940429     C* ULTIMO PROGRESSIVO CARICATO A VIDEO
069700940520     C                   Z-ADD     TAMCTR        WCTRUL
069800940520     C                   Z-ADD     TAMPRG        WPRGUL
069900050818      * salvo il tamfie dell'ultimo progressivo letto
070000050818     c                   Eval      wfie = tamfie
070100940429     C*
070200940429     C* CAMPO SELEZIONE
070300940520     C                   MOVEL     *BLANKS       VSCSCE
070400940429     C*
070500991022     C* CODICE E PROGRESSIVO TARIFFA E DIVISA
070600940520     C                   MOVEL     TAMCTR        VSCCTR
070700940520     C                   MOVEL     TAMPRG        VSCPRG
070800991022     C                   MOVEL     TAMFLO        DSTA01
070900991022     C                   MOVEL     §TADIV        VSCDIV
071000940429     C*
071100940429     C* GIRO DATA DECORRENZA TARIFFA
071200940729     C                   Z-ADD     TAMDDT        DATA
071300940729     C                   Z-ADD     MM            GGMM              4 0
071400940729     C                   MOVEL     GG            GGMM
071500100407     C                   Z-ADD     AA2           VSCDDT
071600940729     C                   MOVEL     GGMM          VSCDDT
071700940429     C* GIRO DATA SCADENZA   TARIFFA
071800940729     C                   Z-ADD     TAMDST        DATA
071900940729     C                   Z-ADD     MM            GGMM
072000940729     C                   MOVEL     GG            GGMM
072100100407     C                   Z-ADD     AA2           VSCDST
072200940729     C                   MOVEL     GGMM          VSCDST
072300050530     C                   Z-ADD     TAMDST        vshdst
072400990913     C* CERCO LA DATA ULTIMA VARIAZIONE PIU' ALTA DI TUTTI
072500990913     C                   EXSR      CERDUV
072600940429     C* GIRO DATA ULTIMA VARIAZIONE
072700940729     C                   Z-ADD     TAMDUV        DATA
072800940729     C                   Z-ADD     MM            GGMM
072900940729     C                   MOVEL     GG            GGMM
073000100407     C                   Z-ADD     AA2           VSCDUV
073100940729     C                   MOVEL     GGMM          VSCDUV
073200100407      /free
073300100407       //?Controllo se ci sono note tariffa (10)
073400100407         clear vscnote;
073500100407         NTCapl = 'C' ;
073600100407         NTCnk1 = %editc(dutkci: 'X') + %editc(v2cksc:'X');
073700100407         NTCnk2 = %editc(vscctr: 'X');
073800100407         NTCtnt = '10';
073900100407         setll (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
074000100407         IF  %equal(TFNTC01L);
074100100407           vscnote = 'SI';
074200100407         ELSE;
074300100407       //?provo con chiave generica senza codice tariffa
074400100407           clear NTCnk2;
074500100407           setll (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
074600100407           IF  %equal(TFNTC01L);
074700100407             vscnote = 'SI';
074800100407           ENDIF;
074900100407    2    ENDIF;
075000100407      /end-free
075100100407
075200940429     C*
075300940429     C* DESCRIZIONE TARIFFA
075400940520     C                   MOVEL     TAMDCV        VSCDCV
075500941110     C* ITALIA/ESTERO E TIPO SERVIZIO
075600941110     C                   MOVEL     TAMTSP        VSCTSP
075700020321     C* DPD/FEDEX/ITALIA/ESTERO
075800020321     C                   SELECT
075900020321  1A C     §TADPD        WHENEQ    'S'
076000020321     C                   MOVE      'D'           VSCFIE
076100020321     C*
076200020321  1A C     §TAFED        WHENEQ    'S'
076300020321     C                   MOVE      'F'           VSCFIE
076400020321     C*
076500020321     C                   OTHER
076600941110     C                   MOVEL     TAMFIE        VSCFIE
076700020321     C                   ENDSL
076800940429     C*
076900940429     C                   ADD       1             NRR
077000940520     C                   WRITE     TA01S03
077100940429    2C                   ENDIF
077200940429     C*
077300940615     C   05KTAM          READE     TNTAM000                               31
077400940615     C  N05V2CKSC        READE     TNTAM000                               31
077500940429    1C                   ENDDO
077600940429     C*
077700940429     C                   ENDSR
077800990913     C*
077900990913     C*--- RICERCA ULTIMA DATA VARIAZIONE TARIFFA --------------------*
078000990913     C     CERDUV        BEGSR
078100990913     C*
078200990913     C* LEGGO TUTTE LE TESTATE TARIFFE PARTICOLARI  CERCANDO LA DATA
078300990913     C* MAGGIORE DI ULTIMA MODIFICA
078400990913     C     KTPT          SETLL     TITPT01L
078500990913     C     KTPT          READE     TITPT01L                               33
078600990913     C*
078700990913     C     *IN33         DOWEQ     *OFF
078800990913     C* VERIFICO SE LA DATA ULTIMA VARIAZIONE E' MAGGIORE DI QUELLA
078900990913     C* DELLA TESTATA
079000990913     C     TPTDUV        IFGT      TAMDUV
079100990913     C     TPTATB        ANDEQ     ' '
079200990913     C                   Z-ADD     TPTDUV        TAMDUV
079300990913     C                   ENDIF
079400990913     C*
079500990913     C     KTPT          READE     TITPT01L                               33
079600990913     C*
079700990913     C                   ENDDO
079800990914     C* VERIFICO ANCHE L'ULTIMA DATA VARIAZIONE TARIFFA CHE C'E' IN
079900990914     C* TITGC TARIFFE GIACENZE  (SE ESISTE)
080000990914     C     KTPT          CHAIN     TITGC01L                           33
080100990914     C     *IN33         IFEQ      *OFF
080200990914     C     TGCATB        ANDEQ     ' '
080300990914     C     TGCDUV        ANDGT     TAMDUV
080400990914     C                   Z-ADD     TGCDUV        TAMDUV
080500990914     C                   ENDIF
080600990913     C*
080700990913     C                   ENDSR
080800120305
080900120305      /free
081000120305       //--------------------------------------------------------------
081100120305       //?Visualizza Dati manca tariffa-
081200120305       //--------------------------------------------------------------
081300120305       BEGSR GesW01;
081400120305
081500120321         wFine  = *off;
081600120314         W01dsp = ISB48dsp;
081700120314         W01tbl = ISB48tbl;
081800120314         W01lnp = ISB48lnp;
081900120314         W01lna = ISB48lna;
082000120314         W01pro = ISB48pro;
082100120314         W01cts = ISB48cts;
082200120314         W01ctr = ISB48ctr;
082300120314         W01ncl = ISB48ncl;
082400120314         W01pkf = ISB48pkf;
082500120314         W01vlf = ISB48vlf;
082600120314         W01err = ISB48err;
082700120305
082800120305         DOW wFine = *off;
082900120305           exfmt  TA01W01;
083000120305           //?- F12 = Ritorno
083100120305           IF  *inkl;
083200120305             wFine = *on;
083300120305           ENDIF;
083400120305         ENDDO;
083500120305
083600120305       ENDSR;
083700120305      /end-free
083800040924      *------------------------------------------------------------------------*
083900040924      * - ROUTINE INIZIALE
084000040924      *------------------------------------------------------------------------*
084100040924     c     *Inzsr        BegSr
084200040924
084300040924     c     *Entry        Plist
084400040924     c                   Parm                    Kpjba
084500120315     c                   parm                    Parm48
084600120314
084700120314      /free
084800120314       //?controllo se passati tutti i parametri
084900120314         IF  %parms = 1;
085000120314           clear tnsb48ds;
085100120314           *in60 = *off;
085200120314         ELSE;
085300120315           clear tnsb48ds;
085400120315           tnsb48ds = parm48;
085500120314           *in60 = *on;
085600120314         ENDIF;
085700120223
085800120314      /end-free
085900040924
086000040924     c     *dtaara       define    §azute        azuteds
086100040924     c     *dtaara       define    §datiute      ddatiute
086200040924     c                   in(E)     *dtaara
086300040924     c                   If        %error  or RSUT = *blanks
086400040924     c                   Clear                   Tibs34ds
086500040924     c                   Call      'TIBS34R'
086600040924     c                   Parm                    Tibs34ds
086700040924     c                   In        *dtaara
086800040924     c                   EndIf
086900130124
087000130124       //?Recupero il flag di 'Modifica tariffe'
087100130124     c                   eval      dUTE01 = UTEfaf
087200120223
087300120223       // -?Il prossimo controllo lo faccio solo se non richiamato
087400120314     c                   IF        not *in60
087500040924
087600040924      * Verifico se il P.O. utente può manutenzionare le tariffe
087700040924      * deve esistere la tabella MTC per il p.o. utente
087800040924     c                   Clear                   Tibs02ds
087900040924     c                   Clear                   DMTC
088000040924     c                   Eval      T02Mod = 'C'
088100040924     c                   Eval      T02Sif = knsif
088200040924     c                   Eval      T02Cod = 'MTC'
088300040924     c                   Movel(p)  DutPou        T02Ke1
088400040924     c                   Call      'TIBS02R'
088500040924     c                   Parm                    Kpjba
088600040924     c                   Parm                    Tibs02ds
088700040924If  2c                   If        T02Err <> *Blanks
088800040924     c                   Seton                                        402808
088900040924     C                   movel     msg(11)       v2cmsg
089000040924     c                   GoTo      For01
089100040924   x2c                   Else
089200040924     c                   Movel     T02Uni        Dmtc
089300040924    2c                   EndIf
089400040924
089500090629     c* Richiamo solo per avere codice di abilitazione
089600090629     c                   clear                   tntaa1ds
089700120229     c                   eval      ITAA1tla = 'L'
089800090629     c                   eval      itaa1caut='§utegtc'
089900090629     c                   clear                   kpjbu
090000090629     c                   movel     tntaa1ds      kpjbu
090100090703     c                   call      'TNTAA1C'
090200090629     c                   parm                    kpjba
090300090629     c                   movel     kpjbu         tntaa1ds
090400090701     c
090500090701     c* Utente non abilitato alla gestione tariffe
090600090701     c                   if        otaa1fabi='N'
090700090701     c                   Seton                                        402808
090800090701     c                   movel     MSG(7)        v2cmsg
090900090701e   2c                   else
091000040924      *
091100040924      * Reperimento dei P.O. gestibili dall'utente
091200040924     c                   clear                   TRUL31DS
091300090629     c                   eval      I31abi = otaa1cabi
091400040924     c                   eval      I31cdi = DUTdis
091500040924     c                   eval      I31car = DUTare
091600040924     c                   eval      I31cpo = DUTpou
091700040924     c                   call      'TRUL31R'
091800040924     c                   parm                    KPJBA
091900090629     c                   parm                    TRUL31DS
092000040924if  2c                   if        O31pog > *zeros
092100040924     c                   movea     O31pog        fig
092200040924x   2c                   else
092300040924     c                   Seton                                        402808
092400040924     c                   movel     MSG(7)        v2cmsg
092500040924e   2c                   endif
092600090701e   2c                   endif
092700040924      *
092800090629      * Se l'utente ha autorizzazione 'AZ' è di sede
092900040924     C* SE SONO IN SEDE NON DEVO ABILITARE IL TASTO FUNZIONALE F7
093000040924     C* PIANO DEI CONTI
093100090629if  1c                   if        otaa1cabi <> 'AZ'
093200040924     C                   SETON                                        01
093300040924e   1c                   endif
093400120223
093500120223     c                   ENDIF
093600120223
093700090701     c
093800050530     c* Carico tabella tariffe particolari in scadenza, da eliminare
093900050530     C                   MOVE      '1P'          kCOD
094000050530     c                   clear                   xx                4 0
094100050530     c     ktab2         setll     tabel00f
094200050530     c     ktab2         reade     tabel00f
094300050530     c                   dow       not %eof(tabel00f)
094400050530     c                   if        tblflg=' '
094500050530     c                   movel     tbluni        ds1p
094600050530     c                   if        §1peli>*zeros
094700050530     c                   add       1             xx
094800050530     c                   movel     tblkey        tpareli(xx)
094900050530     c                   movel     §1peli        tpardat(xx)
095000050530     c                   endif
095100050530     c                   endif
095200050530     c
095300050530     c     ktab2         reade     tabel00f
095400050530     c                   enddo
095500040924
095600040924     C* ACCESSO TABEL00F
095700040924     C     KTAB          KLIST
095800040924     C                   KFLD                    KKUT
095900040924     C                   KFLD                    KCOD
096000040924     C                   KFLD                    KKEY
096100050530     C     KTAb2         KLIST
096200050530     C                   KFLD                    KKUT
096300050530     C                   KFLD                    KCOD
096400040924     C* ACCESSO CNACO00F
096500040924     C     KACO          KLIST
096600040924     C                   KFLD                    KKUT
096700040924     C                   KFLD                    dutKCI
096800040924     C                   KFLD                    WKSC
096900040924     C* ACCESSO TNTAM01L
097000040924     C     KTAM          KLIST
097100040924     C                   KFLD                    V2CKSC
097200040924     C                   KFLD                    WCTR
097300040924     C* ACCESSO TITPT01L
097400040924     C     KTPT          KLIST
097500040924     C                   KFLD                    TAMKSC
097600040924     C                   KFLD                    TAMCTR
097700040924     C                   KFLD                    TAMPRG
097800050530     C     Ktpt2         KLIST
097900050530     C                   KFLD                    v2cksc
098000050530     C                   KFLD                    kctr
098100050530     C                   KFLD                    wprgul
098200050530     C                   KFLD                    tpareli(xx)
098300040924     C*
098400040924     C* DEFINIZIONE DI ALCUNI CAMPI
098500040924     C     *LIKE         DEFINE    TBLCOD        KCOD
098600040924     C     *LIKE         DEFINE    TBLKEY        KKEY
098700040924     C     *LIKE         DEFINE    ACOKSC        WKSC
098800040924     C     *LIKE         DEFINE    TAMCTR        WCTR
098900040924     C     *LIKE         DEFINE    TAMCTR        WCTRUL
099000040924     C     *LIKE         DEFINE    TAMPRG        WPRGUL
099100040924     C     *LIKE         DEFINE    TAMPRG        WPRGLE
099200040924
099300040924     C* TESTATA PGM
099400040924     C                   MOVE      TES(1)        VTCTES
099500040924
099600040924     c                   Movel     rsut          VTCRSU
099700040924
099800040924     c                   EndSr
099900950522     C*
100000940429     C*---------------------------------------------------------------*
100100001020**  TES
100200900409 ***  GESTIONE TARIFFE CLIENTI  ***
100300940622**  MSG
100400130124Utente non autorizzato al tasto funzione F9-Aggiunta Tariffa.                  1
100500940622Cliente non Codificato o Annullato                                             2
100600940622Codice Tariffa Inesistente                                                     3
100700941005Tariffa non manutenzionabile perche' gia' in uso !!                            4
100800941013Non esistono tariffe: per inserirle digitare il Codice Tariffa                 5
100900941128Effettuare almeno una selezione !!                                             6
101000090701UTENTE NON ABILITATO ALLA GESTIONE TARIFFE                                     7
101100090629Tariffa non in gestione all'utente                                             8
101200050818Libero.........                                                                9
101300050818Libero...........                                                              0
101400030703P.O. utente non abilitato alla manutenzione delle Tariffe Clienti              1
101500030703Password errata                                                                2
101600030703Password scaduta                                                               3
101700030703Immettere la password                                                          4
101800030703Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!               5
101900130926MANCA SCAGLIONE ISTAT  AVVISARE PRESIDIO VENDITE !!!!!!                        6
102000050530Presente TariffaPartic. "XX" da eliminare entro xx/xx/xx: non verrà duplicata  7
