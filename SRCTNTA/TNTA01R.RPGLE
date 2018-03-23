000100020708     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200940517     H* TNTA01R *----------------------------------------------------*
000300940429     H*             MANUTENZIONE TARIFFE CLIENTI                     *
000400000000     H*--------------------------------------------------------------*
000500940517     FTNTA01D   CF   E             WORKSTN
000600940520     F                                     SFILE(TA01S03:NRR)
000700941012     FTNTAM01L  IF   E           K DISK
000800150210     ftntam00l  uf   e           k disk    rename(tntam000:tntaml)
000900990913     FTITPT01L  IF   E           K DISK
001000990914     FTITGC01L  IF   E           K DISK
001100000000     FCNACO00F  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300100407     fTFNTC01L  if   e           k disk
001400100426     fazorg01l  if   e           k disk
001500150210     fTITAS41C  if   e           k disk    usropn extfile(wFLib)
001600040923
001700950519     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
001800020708     D COM             S              3    DIM(30)
001900950421     D*
002000940429     D TES             S             35    DIM(1) CTDATA PERRCD(1)
002100040923     D MSG             S             79    DIM(20) CTDATA PERRCD(1)
002200050530     d
002300050530     D TParEli         S              2    DIM(20)
002400050530     D TParDat         S              8  0 DIM(20)
002500050530     D ForzaTPar       S              2    DIM(20)
002600040924
002700040924      * P.O. gestibili dall'utente
002800040924     d fig             s              3    dim(250) inz(*Zeros)
002900020708
003000020708     d paxdut          s             30
003100020708     d paxdmt          s                   LIKE(ACOrag)
003200020708     d paxccc          s                   LIKE(ACOkcc)
003300020708     d paxsta          s              1  0
003400020708     d paxflr          s             90
003500020708     d paxdit          s              3
003600020708     d paxnum          s              2  0
003700020708     d paxkcm          s             80
003800020708     d paxksm          s            140
003900020708     d paxkdm          s             60
004000040923
004100040923     d codut           s              1  0 Inz(1)
004200040923     d kkut            s                   like(TblKut) Inz(1)
004300050530     d kctr            s                   like(tamctr)
004400040924     d w0050           s              5  0
004500150210
004600150210     d sav_TAMksc      s                   like(TAMksc) inz
004700150210     d sav_TAMctr      s                   like(TAMctr) inz
004800150210     d sav_TAMprg      s                   like(TAMprg) inz
004900150210     d sav_TAMddt      s                   like(TAMddt) inz
005000150210     d sav_TAMdst      s                   like(TAMdst) inz
005100150210     d wFineW02        s               n   inz
005200150210     d wInzW02         s               n   inz
005300150210     d wokAGGdst       s               n   inz
005400050530     d
005500150210     d datadmy         s               d   datfmt(*eur)
005600050530     d wdatadmy        s               d   datfmt(*dmy)
005700150210     d wDataScadNw     s                   like(TAMdst)
005800150210     d wDataSP         s                   like(TAMddt)
005900150210
006000150210       // -?Campi libreria e file per TITAS41C
006100150210     d wFLib           s             21
006200150210     d wLib            s             10
006300020708
006400940429     D*
006500940517     D* RICHIAMO PGM DI GESTIONE TARIFFE CLIENTI      - TNTA01R2 -
006600000000     D PARAM           DS
006700941011     D* CODICE CLIENTE
006800940429     D  PARCLI                 2      8  0
006900941011     D* PROGRESSIVO TARIFFA
007000940429     D  PARPRG                 9     11  0
007100941011     D* CAPOCONTO CLIENTI
007200940429     D  PARKCI                12     15  0
007300941011     D* RAGIONE SOCIALE CLIENTE
007400940520     D  V2DKSC                16     63
007500941011     D* CODICE TARIFFA
007600940429     D  PARCTR                64     66
007700940523     D* CODICE CLIENTE NELLA GESTIONE VISITE/OFFERTE
007800940429     D  PARKSC                67     73    INZ
007900941011     D* PARFLG = '5' ---> PROVENGO DA "CMD9-AGGIUNTA TARIFFA"
008000940429     D  PARFLG                74     74
008100941011     D* PARALL = 'A' ---> RECORD ALLOCATO: TARIFFA NON MANUTENZIONABILE
008200941011     D  PARALL                95     95
008300990913     D* FLAG DI RITORNO  DAI PROGRAMMI CHIAMATI
008400990913     D* 'C' FINE PGM
008500990913     D* 'R' RITORNO AL SUBFILE
008600990913     D  FLGRTN               162    162
008700031118     D* PARIST = 'E' ---> SCATTO ISTAT INESISTENTE ERRORE
008800031118     D  PARIST               178    178
008900940429     D*
009000030703     D WLBDAT          DS
009100030703     D  G02DAT                 1      8  0
009200030703     D  G02INV                 9     16  0
009300030703     D  G02ERR                17     17
009400030703     D  G02TGI                18     22  0
009500040923
009600940729     D                 DS
009700940729     D  AA                     1      4  0
009800100407     d  aa2                    3      4  0
009900940729     D  MM                     5      6  0
010000940729     D  GG                     7      8  0
010100940729     D  DATA                   1      8  0
010200040923
010300000000     D KPJBA         E DS
010400991022     D DSTA01        E DS
010500100426     d OG141         e ds
010600030703
010700030703     d tibs02ds      e ds
010800030703     d DMTC          e ds
010900040923
011000100407     d Azuteds       e ds                  extname(Azute00f)
011100040923     d dDatiute      e ds
011200050530     d DS1P          e ds
011300040923     d Tibs34ds      e ds
011400040923     d Trul31ds      e ds
011500050818     d Tnta25ds      e ds
011600050818     d Tnta25tad     e ds                  inz
011700050818     d Tnta25tgc     e ds                  inz
011800050818     d Tnta25tpd     e ds                  inz
011900050818     d Tnta25tpt     e ds                  inz
012000060207     d Tnta60ds      e ds
012100100427     d TNTA28DS      e ds
012200090629     d
012300090629     d* Controllo abilitazioni utente
012400090629     d tntaa1ds      e ds
012500130124     d dUTE01        e ds
012600030703
012700030703     d Dataiso         s               d   datfmt(*iso)
012800030703     d Dataoggi        s              8  0
012900030703     d Datascad        s              8  0
013000030703     d wa              s              2  0
013100030703     d wgiorni         s              3  0
013200050818     d wfie            s                   like(tamfie)
013300120223
013400120314       // -?ds dati passati da TNSB48R - Manca Tariffa
013500120314     d TNSB48ds      e ds
013600120315     d Parm48          s                   like(tnsb48ds)
013700120223
013800120305       // -?Flag booleani
013900120305     d wFine           s               n   inz(*off)
014000100427
014100100427      //---------------------------------------------------------------
014200100427      //?prototipi
014300100427      //---------------------------------------------------------------
014400120314      /copy gaitrasrc/srcprotopr,tnta01r2
014500100427      /copy gaitrasrc/srcprotopr,tnta28r
014600150210      /copy gaitrasrc/srcprotopr,xsrda8
014700030703
014800940429     C*****************************************************************
014900940429     C* RIEPILOGO INDICATORI
015000940429     C*****************************************************************
015100940520     C* 05    - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
015200950414     C* 08    - QUALSIASI TASTO VENGA PREMUTO --> ESCO DAL PROGRAMMA
015300940520     C* 10    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
015400950118     C* 18    - USATO PER RIEMETTERE LA VIDEATA DEL SFL DI SCELTA
015500950118     C*         TARIFFA NEL CASO IN CUI IL RECORD SIA ALLOCATO
015600940520     C* 20    - PULIZIA SUBFILE (SFLCLR)
015700940520     C* 24    - DELETE  SUBFILE (SFLDLT)
015800940523     C* 27    - RICERCA ALFABETICA
015900940520     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
016000940520     C* 29    - LETTURA SUBFILE (READC)
016100940520     C* 30    - DI COMODO
016200940520     C* 31    - LETTURA PER CARICAMENTO SUBFILE
016300941013     C* 35    - USATO PER LOKUP E TESTN
016400941005     C* 36    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD: SE
016500941005     C*         IL RECORD E' ALLOCATO (CIOE' *IN36 ACCESO), EMETTO UN
016600941005     C*         MESSAGGIO PER AVVERTIRE CHE LA TARIFFA E' GIA' IN USO
016700940520     C* 37    - USATO PER REPERIRE DTAARA PER PASSWORD
016800940520     C* 40/41 - ERRORI
016900940520     C* 42    - USATO PER EMETTERE MESSAGGIO: "PER INSERIRE UN NUOVO
017000940520     C*         PROGRESSIVO BISOGNA DIGITARE IL CODICE TARIFFA"
017100120305      * 60    - Pgm richiamato (no F09 sì F01)
017200030703     c* 66    - Gestione password
017300150210      * 90    - riemette il subfile
017400940429     C*****************************************************************
017500921104     C*
017600940429     C     INIZIO        TAG
017700120229
017800120229       // -?Se richiamato
017900120314     c                   IF        *in60
018000120229       // -?imposto i dati del cliente
018100120314     c                   eval      v2cksc = ISB48ksc
018200120314     c                   eval      v2dksc = ISB48rag
018300120229       // -?controllo se esiste la tariffa
018400120229     c     V2Cksc        setll     TNTAM00L
018500120229    1c                   IF        not %equal(TNTAM00L)
018600120314     c                   eval      OSB48err = '1'
018700120314     c                   eval      OSB48msg = 'Non trovata tariffa'
018800120229     c                   goto      fine
018900120229     c                   ENDIF
019000120229       // -?mi posiziono sul file e vado subito in seconda videata
019100120229     c     V2Cksc        setll     TNTAM01L                               30
019200120229     c                   goto      for02
019300120229     c                   ENDIF
019400120224
019500950118     C                   SETOFF                                       18
019600940520     C                   CLEAR                   V2CKSC
019700940520     C                   CLEAR                   V2CCTR
019800940520     C                   CLEAR                   V2DKSC
019900940502     C                   CLEAR                   PARFLG
020000050530     C                   CLEAR                   forzatpar
020100050530     c* indice dedicato per la skiera forzatpar: non usare per altro
020200050530     C                   CLEAR                   yy                4 0
020300921104     C*
020400940429     C     FOR01         TAG
020500940520     C                   WRITE     TA01T01
020600940520     C                   EXFMT     TA01D02
020700940429     C*
020800940523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
020900940523     C                   CLEAR                   V2CMSG
021000940523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
021100940523     C                   SETOFF                                       4041
021200940523     C                   SETOFF                                       4228
021300030703     c                   SETOFF                                       66
021400940429     C*
021500940429     C** CMD3  - FINE LAVORO
021600950414     C   KC
021700950414     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
021800090701     c**          perchè utente non abilitato
021900950414     COR 08              GOTO      FINE
022000950414     C*
022100940429     C** CMD7  - AGGIORNAMENTO ANAGRAFICO PIANO DEI CONTI
022200940429     C     *INKG         IFEQ      *ON
022300060207     c                   Clear                   tnta60ds
022400060207     c                   Call      'TNTA60R'
022500060207     c                   Parm                    kpjba
022600060207     c                   Parm                    Tnta60ds
022700940429     C                   GOTO      FOR01
022800940429     C                   ENDIF
022900921104     C*
023000940429     C** CONTROLLI FORMATO
023100940520     C                   EXSR      CTRD02
023200940523     C*
023300940523     C   27
023400940523     COR 28              GOTO      FOR01
023500940429     C*
023600941005     C     FOR02         TAG
023700921104     C*
023800940429     C                   Z-ADD     0             PARPRG
023900940429     C                   MOVEL     *BLANKS       PARCTR
024000940429     C*
024100940429     C* 30 OFF - IMMISSIONE
024200940429     C* 30 ON  - AGGIORNAMENTO
024300940429     C*
024400940429    1C     *IN30         IFEQ      *OFF
024500950118     C     *IN18         ANDEQ     *OFF
024600940520     C                   MOVEL     V2CCTR        PARCTR
024700940429     C                   Z-ADD     0             PARPRG
024800921104     C*
024900940429   X1C                   ELSE
025000940429     C*
025100940429     C* CARICAMENTO SUBFILE
025200950118     C  N18              EXSR      RIES03
025300100427
025400921104     C*
025500940429     C* NRR = 0  --->  NESSUN PROGRESSIVO VALIDO CARICATO A VIDEO:
025600940429     C*                VADO IN IMMISSIONE CON L'ULTIMO LETTO + 1
025700940429    2C     NRR           IFEQ      0
025800940429     C*
025900941013    3C     *IN05         IFEQ      *OFF
026000941013     C                   MOVEL     MSG(5)        V2CMSG
026100941013     C                   SETON                                        4028
026200941013     C                   GOTO      FOR01
026300941013    3C                   ENDIF
026400941013     C*
026500940520     C                   MOVEL     V2CCTR        PARCTR
026600940523     C     WPRGLE        ADD       1             PARPRG
026700921104     C*
026800940429   X2C                   ELSE
026900940429     C* NRR <> 0
027000990913     C*
027100990913     C     FOR03         TAG
027200940429     C*
027300940429     C* EMETTO SUBFILE
027400940520     C                   WRITE     TA01T01
027500940520     C                   WRITE     TA01Z04
027600940520     C                   EXFMT     TA01C03
027700941012     C*
027800941012     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
027900941012     C                   CLEAR                   VCCMSG
028000941012     C                   SETOFF                                       28
028100150217     c                   setoff                                       90
028200941110     C*
028300941110     C** CMD 3 - FINE
028400941110     C   KC              GOTO      FINE
028500120224
028600120224       //?Se richiamato con F12 esco dal pgm
028700120314     c                   IF        *in60 and *inkl
028800120224     c                   goto      fine
028900120224     c                   ENDIF
029000940429     C*
029100940429     C** CMD12 - RITORNO
029200940429     C   KL              GOTO      INIZIO
029300100427
029400120305      /free
029500120305       //?F1 - dati manca tariffa
029600120305        IF  *inka;
029700120305          exsr gesw01;
029800120305      /end-free
029900120305     c  N05V2CKSC        SETLL     TNTAM000                               30
030000120305     c   05KTAM          SETLL     TNTAM000                               30
030100120305     c                   exsr      ries03
030200120305     c                   GOTO      for03
030300120305     c                   ENDIF
030400100427      /free
030500100427       //?F8 - Note tariffa
030600110105        IF  *inkh;
030700100427          clear TNTA28ds;
030800100427          ITA28ela = 'M';
030900100427          ITA28tip = 'C';
031000100427          ITA28cod = V2cksc;
031100100427          ITA28rsc = V2dksc;
031200100427          kpjbu    = TNTA28DS;
031300100427          tnta28r (kpjba);
031400100427      /end-free
031500100427     c  N05V2CKSC        SETLL     TNTAM000                               30
031600100427     c   05KTAM          SETLL     TNTAM000                               30
031700100427     c                   exsr      ries03
031800100427     c                   GOTO      for03
031900100427     c                   ENDIF
032000100427
032100940429     C*
032200050530     C** F9  - AGGIUNTA TARIFFA
032300940517    3C     *INKI         IFEQ      '1'
032400130125       //?se utente non abilitato alla modifica tariffe non do la
032500130125       //?possibilità di aggiungere un nuovo progressivo
032600130125       //?non controllo in caso di tariffa di cartello perchè se
032700130125       //?l'utente arriva in questo punto e sta modificando una tariffa
032800130125       //?di cartello vuol dire che è abilitato a fare tutto
032900130125       //?il controllo sulla possibilità di gestire la cartello viene
033000130125       //?fatto prima
033100130125     c                   Movel     v2cksc        w0050
033200130528     c                   IF        w0050 <> 88888 and
033300130125     c                             §UTEmodtar = *blanks
033400130125     c                   eval      VCCmsg = msg(01)
033500130125     c                   eval      *in28 = *on
033600130125     c                   goto      for03
033700130125     c                   ENDIF
033800940429     C*
033900940429     C* SE NON IMMESSO IL CODICE TARIFFA IN PRIMA VIDEATA ---> ERRORE
034000940520    4C     *IN05         IFEQ      *OFF
034100940520     C                   SETON                                        42
034200940523     C                   GOTO      FOR01
034300940429    4C                   ENDIF
034400940429     C*
034500940429     C**  D U P L I C A   T A R I F F A  **
034600050530     c* prima di duplicare, avviso se ci sono delle tariffe partic in scaden
034700050530     c*  za da eliminare che non verranno copiate
034800050530     c                   z-add     1             xx
034900050530     c                   movel     v2cctr        kctr
035000050530    1c                   dow       tpareli(XX) <>'  '
035100050530     c     ktpt2         chain     titpt01l
035200050530    2c                   if        %found(titpt01l) and tptatb=' '
035300050530   2ac                   if        vshdst>=tpardat(XX)
035400050530     c
035500050530     c* Errore  forzabile se non già dato
035600050530     c     tpareli(XX)   lookup    forzatpar                              31
035700050530    3c                   if        not *in31
035800050530     c                   eval      vccmsg=msg(17)
035900050530     c                   eval      %subst(vccmsg:26:2)=tpareli(XX)
036000050530     c     *iso          move      tpardat(XX)   wdatadmy
036100050530     c                   move      wdatadmy      wdata             6 0
036200050530     c                   eval      %subst(vccmsg:49:8) =(%editw(wdata:'  /  / -
036300050530     c                              '))
036400050530     c
036500050530     c                   add       1             yy
036600050530     c                   movel     tpareli(xx)   forzatpar(YY)
036700050530     C                   SETON                                        28
036800050530     c                   goto      for03
036900050530    3c                   endif
037000050530   2ac                   endif
037100050530    2c                   endif
037200050530     c                   add       1             xx
037300050530    1c                   enddo
037400150210
037500150210     c                   eval      wokAGGdst = *off
037600150210      //?devo verificare la data scadenza
037700150210      //?dell'ultimo progressivo della nuova tariffa
037800150210      //?con data ultima spedizione fatturata
037900150210     c                   exsr      CercaDate
038000150210       //?Se la data scadenza della tariffa è inferiore alla data
038100150210       //?ultima spedizione fatturata devo emettere la win x poter
038200150210       //?modificare la data scadenza
038300150210     c                   IF        sav_TAMdst < wDataSP
038400150210     c                   eval      wInzW02 = *on
038500150210     c                   exsr      GesW02
038600150210     c                   IF        *In90
038700150210     c                   Goto      For03
038800150210     c                   ENDIF
038900150210     c                   IF        wokAGGdst
039000150210     c                   exsr      AggDataDst
039100150210     c                   ENDIF
039200150210     c                   ENDIF
039300050530     c
039400050818      * Richiamo al nuovo pgm di copia tariffe
039500050818     c                   Clear                   Tnta25ds
039600050818     c                   Eval      ta25ta01 = '1'
039700050818     c                   Eval      ta25inglo = 'N'
039800050818     c                   Eval      ta25tipo = 'T'
039900050818     c                   Eval      ta25ksco = v2cksc
040000050818     c                   Move      v2cctr        ta25ctro
040100050818     c                   Eval      ta25prgo = wprgul
040200050818     c                   Eval      ta25tipn = 'T'
040300050818     c                   Eval      ta25kscn = v2cksc
040400050818     c                   Move      v2cctr        ta25ctrn
040500050818     c                   Eval      ta25tam = 'S'
040600050818     c                   Eval      ta25tad = 'T'
040700050818     c                   Eval      ta25tpt = 'T'
040800050818     c                   Eval      ta25tpd = 'N'
040900050818     c                   Eval      ta25tgc = 'T'
041000050818     c                   Eval      ta25cat = 'S'
041100050818     c                   Eval      ta25fie = tamfie
041200050818     c                   Call      'TNTA25R'
041300050818     c                   Parm                    kpjba
041400050818     c                   Parm                    Tnta25ds
041500050818     c                   Parm                    Tnta25tad
041600050818     c                   Parm                    Tnta25tgc
041700050818     c                   Parm                    Tnta25tpd
041800050818     c                   Parm                    Tnta25tpt
041900050818
042000050818      * Riporto da tnta25r l'eventuale errore se manca lo scaglione ISTAT
042100050818     c                   Move      kpjbu         pa2err            3
042200031119     C     PA2ERR        IFEQ      'IST'
042300031119     C                   MOVEL     MSG(16)       V2CMSG
042400031119     C                   SETON                                        4028
042500031119     C                   GOTO      FOR01
042600031119    3C                   ENDIF
042700050818      * Se errore da copia tariffa lo emetto a video
042800050818     c                   If        ta25err <> *Blanks
042900050818     c                   Eval      v2cmsg = ta25msg
043000050818     c                   Eval      *In28 = *On
043100050818     c                   Eval      *In40 = *On
043200050818     c                   Goto      for01
043300050818     c                   EndIf
043400050818      * nessun errore imposto i campi per il richiamo al pgm di manutenzione
043500050818     c                   Move      ta25ctrn      parctr
043600050818     c                   Move      ta25prg       parprg
043700050818
043800940517     C* PARFLG = '5' ---> PROVENGO DA "CMD9-AGGIUNTA TARIFFA" QUINDI
043900940517     C*                   NEL PGM TNTA01R2 NON ABILITO IL CMD12 NELLA
044000940517     C*                   VIDEATA DELLE CONDIZIONI GENERALI TARIFFARIE
044100940429     C                   MOVEL     '5'           PARFLG
044200900320     C*
044300940429   X3C                   ELSE
044400940429     C* *INKI OFF - CONTROLLO SE E' STATA SELEZIONATA UNA TARIFFA
044500940517     C*
044600940520     C                   READC     TA01S03                                29
044700940429     C*
044800940520    4C     *IN29         IFEQ      *ON
044900940429     C* CON ENTER VIENE PROPOSTA L'ULTIMA TARIFFA ELENCATA NEL SUBFILE
045000940520     C                   MOVEL     WCTRUL        PARCTR
045100940520     C                   Z-ADD     WPRGUL        PARPRG
045200940429   X4C                   ELSE
045300941005     C* CON "1" O BARRA SPAZ. VIENE PROPOSTA LA TARIFFA SELEZIONATA
045400940520     C                   MOVEL     VSCCTR        PARCTR
045500940520     C                   Z-ADD     VSCPRG        PARPRG
045600940429    4C                   ENDIF
045700940429     C*
045800940429    3C                   ENDIF
045900940429    2C                   ENDIF
046000940429    1C                   ENDIF
046100941005     C*
046200940429     C******
046300940517     C*** T N T A 0 1 R 2
046400940429     C******
046500940520     C                   MOVEL     V2CKSC        PARCLI
046600000000     C                   MOVEL     PARAM         KPJBU
046700120314       //?se non richiamato faccio richiamo semplice al pgm gestione dettaglio
046800120314     c                   IF        not *in60
046900940517     C                   CALL      'TNTA01R2'
047000000000     C                   PARM                    KPJBA
047100120314     C                   ELSE
047200120314       //?se richiamato devo passare la DS del TNSB48R al TNTA01R2
047300120314      /free
047400120314         tnta01r2 (kpjba:TNSB48DS);
047500120314      /end-free
047600120314
047700120314     C                   ENDIF
047800941011     C                   MOVEL     KPJBU         PARAM
047900940429     C*
048000950118     C* SE RECORD ALLOCATO EMETTO MESSAGGIO
048100941011     C     PARALL        IFNE      ' '
048200941012     C                   MOVEL     MSG(4)        VCCMSG
048300950118     C                   SETON                                        2818
048400941011     C                   GOTO      FOR02
048500941011     C                   ENDIF
048600031118     C* SE MANCA SCATTO ISTAT ERRORE
048700031118     C     PARIST        IFNE      ' '
048800031118     C                   MOVEL     MSG(16)       V2CMSG
048900031118     C                   SETON                                        2840
049000120224       //?Se richiamato torno al subfile
049100120314     c                   IF        *in60
049200120224     c                   goto      for02
049300120224     c                   ENDIF
049400031118     C                   GOTO      FOR01
049500031118     C                   ENDIF
049600011206     C* PULISCO IL FLAG DI AGGIUNTA PROGRESSIVO
049700011206     C                   CLEAR                   PARFLG
049800941011     C*
049900990913     C* CONTROLLO IL FLAG DI RITORNO
050000990913     C* SE E' UGUALE A "R" RITORNO AL SUBFILE ALTRIMENTI VADO ALL'INIZIO
050100990913     C     FLGRTN        IFEQ      'R'
050200990914     C  N05V2CKSC        SETLL     TNTAM000                               30
050300990914     C   05KTAM          SETLL     TNTAM000                               30
050400990913     C                   EXSR      RIES03
050500990913     C                   GOTO      FOR03
050600990913     C                   ELSE
050700940429     C                   GOTO      INIZIO
050800990913     C                   ENDIF
050900990913     C*
051000000000     C     FINE          TAG
051100120315     c                   IF        *in60
051200120315     c                   eval      parm48 = tnsb48ds
051300120315     c                   ENDIF
051400000000     C                   SETON                                        LR
051500090629     c
051600940429     C*
051700940520     C* DELETE SUBFILE
051800940520     C                   SETON                                        24
051900940520     C                   WRITE     TA01C03
052000940520     C                   SETOFF                                       24
052100921102     C*
052200940429     C*--- CONTROLLI FORMATO -----------------------------------------*
052300940520     C     CTRD02        BEGSR
052400040924
052500940523     C                   SETOFF                                       2705
052600040923     C                   Z-ADD     dutkci        PARKCI
052700940429     C*
052800940429     C****  RICERCA ALFABETICA  ****
052900940429     C* SE CODICE CLIENTE A ZERO RICERCA ALFABETICA
053000940520    1C     V2CKSC        IFEQ      0
053100941128     C*
053200941128     C* SE ANCHE IL CAMPO DELLA RICERCA E' VUOTO ---> ERRORE
053300941128    2C     V2DKSC        IFEQ      *BLANKS
053400941128     C                   MOVEL     MSG(6)        V2CMSG
053500941128     C                   SETON                                        4028
053600941128     C                   GOTO      ENDCTR
053700941128    2C                   ENDIF
053800941128     C*
053900040927     c                   Z-add     1             xx
054000040924     c                   Clear                   com
054100040924     c     *zeros        Lookup    fig(xx)                                30
054200041008     c                   If        *In30 and xx <= 30
054300050530     C                   DO        30            XX
054400020708     C                   MOVE      FIG(XX)       COM(XX)
054500020708     C                   ENDDO
054600040924     c                   EndIf
054700020708     c                   movea     com           paxflr
054800020708
054900040924     c                   movel     rsut          paxdut
055000020708     c                   z-add     9             paxsta
055100040923     c                   z-add     dutkci        paxccc
055200020708     c                   movel(P)  v2dksc        paxdmt
055300020708     c                   z-add     1             paxnum
055400020708
055500020708     c                   call      'XALFA3BR'
055600020708     c                   parm                    paxdut
055700020708     c                   parm                    codut
055800020708     c                   parm                    paxdmt
055900020708     c                   parm                    paxccc
056000020708     c                   parm                    paxsta
056100020708     c                   parm                    paxflr
056200020708     c                   parm      *blanks       paxdit
056300020708     c                   parm                    paxnum
056400020708     c                   parm                    paxkcm
056500020708     c                   parm                    paxksm
056600020708     c                   parm                    paxkdm
056700020708
056800020708      * Non trovato o premuto F12
056900020708    2c                   if        paxsta > -1
057000020708     c                   movel     paxksm        v2cksc
057100020708    2c                   endif
057200020708
057300020708     c                   SETON                                        27
057400020708     c                   GOTO      ENDCTR
057500020708    1c                   ENDIF
057600040506     c*
057700040506     C****  CODICE CLIENTE  ****
057800040506     C                   Z-ADD     V2CKSC        WKSC
057900040506     C     KACO          CHAIN     CNACO000                           30
058000040506     C  N30ACOFLG        COMP      '*'                                    30
058100040506     C*
058200040506     C     *IN30         IFEQ      *ON
058300040506     C                   MOVEL     MSG(2)        V2CMSG
058400040506     C                   SETON                                        4028
058500040506     C                   GOTO      ENDCTR
058600040506     C                   ENDIF
058700040506     C*
058800040506     C* DECODIFICA CODICE CLIENTE
058900040506     C                   MOVEL     ACORAG        V2DKSC
059000940429     C*
059100940429     C****  CONTROLLO SE CONTO GESTIBILE  ****
059200090629     c                   clear                   tntaa1ds
059300120229     c                   eval      ITAA1tla = 'L'
059400090629     c                   eval      itaa1caut='§utegtc'
059500090629     c                   eval      itaa1ksc=v2cksc
059600090629     c                   clear                   kpjbu
059700090629     c                   movel     tntaa1ds      kpjbu
059800090703     c                   call      'TNTAA1C'
059900090629     c                   parm                    kpjba
060000090629     c                   movel     kpjbu         tntaa1ds
060100090629     c
060200090629    1c                   if        otaa1fabi='N'
060300090629      * per tariffa di cartello controllo se l'utente è autorizzato
060400090629     c                   Movel     v2cksc        w0050
060500090629    2c                   If        w0050 = 88888
060600090629     c                   Eval      v2cmsg = msg(7)
060700090629     c                   Eval      %subst(v2cmsg:44:11) = 'di cartello'
060800090629     c                   else
060900090629     C                   MOVEL     MSG(8)        V2CMSG
061000090629    2c                   endif
061100090629     c
061200090629     c                   Seton                                        4028
061300090629     c                   goto      endctr
061400090629    1c                   endif
061500940429     C*
061600940429     C****  CODICE TARIFFA  ****
061700940429     C* CONTROLLO TIPO TARIFFA SE IMMESSO
061800940520    1C     V2CCTR        IFNE      *BLANKS
061900941013     C*
062000941013     C                   TESTN                   V2CCTR               35
062100941013     C*
062200941013     C* CONTROLLO CHE L'ULTIMO CARATTERE DEL CODICE TARIFFA NON SIA
062300941013     C*   UNA LETTERA
062400941013     C   35              MOVE      V2CCTR        W001              1
062500941013    2C   35W001          IFLT      '0'
062600941013     C                   SETOFF                                       35
062700941013    2C                   ENDIF
062800941013     C*
062900941013    2C     *IN35         IFEQ      *OFF
063000941013     C                   MOVEL     MSG(3)        V2CMSG
063100941013     C                   SETON                                        4128
063200941013     C                   GOTO      ENDCTR
063300941013    2C                   ENDIF
063400941013     C*
063500940520     C                   MOVEL     V2CCTR        W0010             1 0
063600940523     C                   MOVEL     'TR'          KCOD
063700940523     C                   MOVEL     *BLANKS       KKEY
063800940523     C                   MOVEL     W0010         KKEY
063900940502     C     KTAB          CHAIN     TABEL                              30
064000941013    2C     *IN30         IFEQ      *ON
064100940520     C                   MOVEL     MSG(3)        V2CMSG
064200940520     C                   SETON                                        4128
064300940520     C                   GOTO      ENDCTR
064400941013    2C                   ENDIF
064500940429     C*
064600940520     C* 05 ON  - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
064700940520     C                   SETON                                        05
064800940520     C                   MOVEL     V2CCTR        WCTR
064900940429    1C                   ENDIF
065000940429     C*
065100941013     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
065200941013    1C     *IN05         IFEQ      *OFF
065300941013     C     V2CKSC        SETLL     TNTAM000                               30
065400941013    2C     *IN30         IFEQ      *OFF
065500941013     C                   MOVEL     MSG(5)        V2CMSG
065600941013     C                   SETON                                        4028
065700941013     C                   GOTO      ENDCTR
065800941013    2C                   ENDIF
065900941013     C*
066000941013   X1C                   ELSE
066100941013     C* 05 ON  - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
066200941013     C     KTAM          SETLL     TNTAM000                               30
066300941013    1C                   ENDIF
066400030703      *
066500030703      * controllo password
066600030703      *
066700030703     c                   If        V2cpwd = *Blanks
066800030703     c                   Seton                                        6628
066900030703     C                   movel     msg(14)       V2CMSG
067000030703     c                   GoTo      EndCtr
067100030703     c                   EndIf
067200030703      * stessa password
067300030703     c                   If        V2cpwd <> §MTCPwd
067400030703     c                   Seton                                        6628
067500030703     C                   movel     msg(12)       V2CMSG
067600030703     c                   GoTo      EndCtr
067700030703     c                   EndIf
067800030703      * data immissione/variazione password
067900030703     c                   Clear                   wlbdat
068000030703     c                   Z-Add     §MtcDta       G02Dat
068100030703     c                   Call      'XSRDA8'
068200030703     c                   Parm                    Wlbdat
068300030703     c                   Movel     G02Inv        Dataiso
068400030703      * Scadenza password
068500030703     c                   Clear                   Tibs02ds
068600030703     c                   Eval      T02Mod = 'C'
068700030703     c                   Eval      T02Sif = knsif
068800030703     c                   Eval      T02Cod = 'MTC'
068900030703     c                   Movel(p)  'PSW'         T02Ke1
069000030703     c                   Call      'TIBS02R'
069100030703     c                   Parm                    Kpjba
069200030703     c                   Parm                    Tibs02ds
069300030703If  2c                   If        T02Err <> *Blanks
069400030703     c                   Seton                                        6628
069500030703     C                   movel     msg(15)       V2CMSG
069600030703     c                   GoTo      EndCtr
069700030703   x2c                   Else
069800030703     c                   Movel     T02Uni        wgiorni
069900030703     c                   Adddur    wgiorni:*d    Dataiso
070000030703     c                   Move      Dataiso       Datascad
070100030703     c                   Z-Add     *Date         Dataoggi
070200030703     c                   If        Dataoggi > DataScad
070300030703     c                   Seton                                        6628
070400030703     C                   movel     msg(13)       V2CMSG
070500030703     c                   GoTo      EndCtr
070600030703     c                   EndIf
070700030703    2c                   EndIf
070800941013     C*
070900940520     C     ENDCTR        ENDSR
071000940429     C*
071100940429     C*--- CARICAMENTO SUBFILE ---------------------------------------*
071200940520     C     RIES03        BEGSR
071300940520     C*
071400940520     C* RIEMPO CAMPI DEL SFLCONTROL
071500940520     C                   MOVEL     V2CKSC        VCCKSC
071600940520     C                   MOVEL     V2DKSC        VCDKSC
071700940520     C*
071800940429     C* PULIZIA SUBFILE
071900940520     C                   SETON                                        20
072000940520     C                   WRITE     TA01C03
072100940520     C                   SETOFF                                       20
072200940429     C*
072300940429     C                   Z-ADD     0             NRR               4 0
072400940429     C*
072500150210     C   05KTAM          READE     TNTAM000                               31
072600150210     C  N05V2CKSC        READE     TNTAM000                               31
072700940429     C*
072800940520    1C     *IN31         DOWEQ     *OFF
072900940429     C*
073000940429     C* ULTIMO PROGRESSIVO LETTO (COMPRESI QUELLI ANNULLATI)
073100940523     C                   Z-ADD     TAMPRG        WPRGLE
073200940429     C*
073300940429     C* ESCLUDO RECORD ANNULLATI
073400940429    2C     TAMATB        IFEQ      ' '
073500940429     C* ULTIMO PROGRESSIVO CARICATO A VIDEO
073600940520     C                   Z-ADD     TAMCTR        WCTRUL
073700940520     C                   Z-ADD     TAMPRG        WPRGUL
073800050818      * salvo il tamfie dell'ultimo progressivo letto
073900050818     c                   Eval      wfie = tamfie
074000940429     C*
074100940429     C* CAMPO SELEZIONE
074200940520     C                   MOVEL     *BLANKS       VSCSCE
074300940429     C*
074400991022     C* CODICE E PROGRESSIVO TARIFFA E DIVISA
074500940520     C                   MOVEL     TAMCTR        VSCCTR
074600940520     C                   MOVEL     TAMPRG        VSCPRG
074700991022     C                   MOVEL     TAMFLO        DSTA01
074800991022     C                   MOVEL     §TADIV        VSCDIV
074900940429     C*
075000940429     C* GIRO DATA DECORRENZA TARIFFA
075100940729     C                   Z-ADD     TAMDDT        DATA
075200940729     C                   Z-ADD     MM            GGMM              4 0
075300940729     C                   MOVEL     GG            GGMM
075400100407     C                   Z-ADD     AA2           VSCDDT
075500940729     C                   MOVEL     GGMM          VSCDDT
075600150210     C                   Z-ADD     TAMDDT        vshddt
075700940429     C* GIRO DATA SCADENZA   TARIFFA
075800940729     C                   Z-ADD     TAMDST        DATA
075900940729     C                   Z-ADD     MM            GGMM
076000940729     C                   MOVEL     GG            GGMM
076100100407     C                   Z-ADD     AA2           VSCDST
076200940729     C                   MOVEL     GGMM          VSCDST
076300050530     C                   Z-ADD     TAMDST        vshdst
076400990913     C* CERCO LA DATA ULTIMA VARIAZIONE PIU' ALTA DI TUTTI
076500990913     C                   EXSR      CERDUV
076600940429     C* GIRO DATA ULTIMA VARIAZIONE
076700940729     C                   Z-ADD     TAMDUV        DATA
076800940729     C                   Z-ADD     MM            GGMM
076900940729     C                   MOVEL     GG            GGMM
077000100407     C                   Z-ADD     AA2           VSCDUV
077100940729     C                   MOVEL     GGMM          VSCDUV
077200100407      /free
077300100407       //?Controllo se ci sono note tariffa (10)
077400100407         clear vscnote;
077500100407         NTCapl = 'C' ;
077600100407         NTCnk1 = %editc(dutkci: 'X') + %editc(v2cksc:'X');
077700100407         NTCnk2 = %editc(vscctr: 'X');
077800100407         NTCtnt = '10';
077900100407         setll (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
078000100407         IF  %equal(TFNTC01L);
078100100407           vscnote = 'SI';
078200100407         ELSE;
078300100407       //?provo con chiave generica senza codice tariffa
078400100407           clear NTCnk2;
078500100407           setll (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
078600100407           IF  %equal(TFNTC01L);
078700100407             vscnote = 'SI';
078800100407           ENDIF;
078900100407    2    ENDIF;
079000100407      /end-free
079100100407
079200940429     C*
079300940429     C* DESCRIZIONE TARIFFA
079400940520     C                   MOVEL     TAMDCV        VSCDCV
079500941110     C* ITALIA/ESTERO E TIPO SERVIZIO
079600941110     C                   MOVEL     TAMTSP        VSCTSP
079700020321     C* DPD/FEDEX/ITALIA/ESTERO
079800020321     C                   SELECT
079900020321  1A C     §TADPD        WHENEQ    'S'
080000020321     C                   MOVE      'D'           VSCFIE
080100020321     C*
080200020321  1A C     §TAFED        WHENEQ    'S'
080300020321     C                   MOVE      'F'           VSCFIE
080400020321     C*
080500020321     C                   OTHER
080600941110     C                   MOVEL     TAMFIE        VSCFIE
080700020321     C                   ENDSL
080800940429     C*
080900940429     C                   ADD       1             NRR
081000940520     C                   WRITE     TA01S03
081100940429    2C                   ENDIF
081200940429     C*
081300150210     C   05KTAM          READE     TNTAM000                               31
081400150210     C  N05V2CKSC        READE     TNTAM000                               31
081500940429    1C                   ENDDO
081600940429     C*
081700940429     C                   ENDSR
081800990913     C*
081900990913     C*--- RICERCA ULTIMA DATA VARIAZIONE TARIFFA --------------------*
082000990913     C     CERDUV        BEGSR
082100990913     C*
082200990913     C* LEGGO TUTTE LE TESTATE TARIFFE PARTICOLARI  CERCANDO LA DATA
082300990913     C* MAGGIORE DI ULTIMA MODIFICA
082400990913     C     KTPT          SETLL     TITPT01L
082500990913     C     KTPT          READE     TITPT01L                               33
082600990913     C*
082700990913     C     *IN33         DOWEQ     *OFF
082800990913     C* VERIFICO SE LA DATA ULTIMA VARIAZIONE E' MAGGIORE DI QUELLA
082900990913     C* DELLA TESTATA
083000990913     C     TPTDUV        IFGT      TAMDUV
083100990913     C     TPTATB        ANDEQ     ' '
083200990913     C                   Z-ADD     TPTDUV        TAMDUV
083300990913     C                   ENDIF
083400990913     C*
083500990913     C     KTPT          READE     TITPT01L                               33
083600990913     C*
083700990913     C                   ENDDO
083800990914     C* VERIFICO ANCHE L'ULTIMA DATA VARIAZIONE TARIFFA CHE C'E' IN
083900990914     C* TITGC TARIFFE GIACENZE  (SE ESISTE)
084000990914     C     KTPT          CHAIN     TITGC01L                           33
084100990914     C     *IN33         IFEQ      *OFF
084200990914     C     TGCATB        ANDEQ     ' '
084300990914     C     TGCDUV        ANDGT     TAMDUV
084400990914     C                   Z-ADD     TGCDUV        TAMDUV
084500990914     C                   ENDIF
084600990913     C*
084700990913     C                   ENDSR
084800120305
084900120305      /free
085000120305       //--------------------------------------------------------------
085100120305       //?Visualizza Dati manca tariffa-
085200120305       //--------------------------------------------------------------
085300120305       BEGSR GesW01;
085400120305
085500120321         wFine  = *off;
085600120314         W01dsp = ISB48dsp;
085700120314         W01tbl = ISB48tbl;
085800120314         W01lnp = ISB48lnp;
085900120314         W01lna = ISB48lna;
086000120314         W01pro = ISB48pro;
086100120314         W01cts = ISB48cts;
086200120314         W01ctr = ISB48ctr;
086300120314         W01ncl = ISB48ncl;
086400120314         W01pkf = ISB48pkf;
086500120314         W01vlf = ISB48vlf;
086600120314         W01err = ISB48err;
086700120305
086800120305         DOW wFine = *off;
086900120305           exfmt  TA01W01;
087000120305           //?- F12 = Ritorno
087100120305           IF  *inkl;
087200120305             wFine = *on;
087300120305           ENDIF;
087400120305         ENDDO;
087500120305
087600120305       ENDSR;
087700150210
087800150210       //--------------------------------------------------------------
087900150210       //?Cerco le date tariffa e ultima sped.fatturata.
088000150210       //--------------------------------------------------------------
088100150210       BEGSR CercaDate;
088200150210
088300150210       //?cerco l'ultimo progressivo della tariffa nuova
088400150210         clear wDataScadNw;
088500150210         sav_TAMksc = V2Cksc;
088600150210         sav_TAMctr = %dec(V2Cctr:3:0);
088700150210         sav_TAMprg = wprgul;
088800150210         sav_TAMddt = VSHddt;
088900150210         sav_TAMdst = VSHdst;
089000150210
089100150210       //?Cerco la data ultima spedizione fatturata con la tariffa
089200150210       //?che andrò a creare
089300150210         clear wDataSP;
089400150210         clear TASaas;
089500150210         clear TASmgs;
089600150210         setgt  (sav_TAMksc:sav_TAMctr) TITAS41C;
089700150210         readpe (sav_TAMksc:sav_TAMctr) TITAS41C;
089800150210         wDataSP = TASaas * 10000 + TASmgs;
089900150210
090000150210       ENDSR;
090100150210       //--------------------------------------------------------------
090200150210       //?WIN per incongruenza date deorrenza nuova tariffa.
090300150210       //--------------------------------------------------------------
090400150210       BEGSR GesW02;
090500150210
090600150210         wFineW02 = *off;
090700150210
090800150210       //?Inizializzo la videata
090900150210         IF  wInzW02;
091000150210           exsr InzW02;
091100150210           wInzW02 = *off;
091200150210         ENDIF;
091300150210
091400150210       //?Emetto la videata
091500150210         DOW  not wFineW02;
091600150210           exfmt  TA01W02;
091700150210           *in28 = *off;
091800150210           clear W02msg;
091900150210
092000150210           SELECT;
092100150210
092200150210         //?- F12=Ritorno
092300150210           WHEN  *inkl;
092400150210             *in90 = *on;
092500150210             wFineW02 = *on;
092600150210             leavesr;
092700150210
092800150210         //?- F06=Modifica tariffa
092900150210           WHEN  *inkf;
093000150210             exsr ContrW02;
093100150210             IF  *in28;
093200150210               iter;
093300150210             ENDIF;
093400150210           //?Imposto il flag per aggiornare la data scadenza
093500150210           //?ultimo progressivo della tariffa nuova da creare
093600150210             wokAGGdst = *on;
093700150210             wFineW02 = *on;
093800150210             leavesr;
093900150210
094000150210         //?- Enter
094100150210           OTHER;
094200150210             exsr ContrW02;
094300150210             IF  *in28;
094400150210               iter;
094500150210             ENDIF;
094600150210
094700150210           ENDSL;
094800150210
094900150210         ENDDO;
095000150210
095100150210       ENDSR;
095200150210
095300150210       //--------------------------------------------------------------
095400150210       //?Inizializzo la WIN.
095500150210       //--------------------------------------------------------------
095600150210       BEGSR InzW02 ;
095700150210
095800150210         clear TA01W02;
095900150210         W02ksc = sav_TAMksc;
096000150210         W02ctr = sav_TAMctr;
096100150210         W02prg = sav_TAMprg;
096200150210         W02rag = V2Dksc;
096300150210         *in55 = *off;
096400150210
096500150210         datadmy = %date(sav_TAMddt:*iso);
096600150210         W02ddt = %dec(datadmy);
096700150210
096800150210         datadmy = %date(sav_TAMdst:*iso);
096900150210         W02dst = %dec(datadmy);
097000150210
097100150210       //?Imposto già l'errore
097200150210         *in28 = *on;
097300150210         *in55 = *on;
097400150210         W02msg = 'Scadenza Tariffa < Ultima Sped. Fatt. ' +
097500150210                   %subst(%editc(wDataSP:'X'):7:2) + '/' +
097600150210                   %subst(%editc(wDataSP:'X'):5:2) + '/' +
097700150210                   %subst(%editc(wDataSP:'X'):1:4);
097800150210
097900150210       ENDSR;
098000150210
098100150210       //--------------------------------------------------------------
098200150210       //?Controllo i dati della WIN.
098300150210       //--------------------------------------------------------------
098400150210       BEGSR ContrW02;
098500150210
098600150210         *in55 = *off;
098700150210
098800150210       //?Controllo data scadenza tariffa
098900150210         IF  W02dst = 0;
099000150210           *in28 = *on;
099100150210           *in55 = *on;
099200150210           W02msg = msg(09);
099300150210           leavesr;
099400150210         ENDIF;
099500150210
099600150210         clear wlbdat;
099700150210         G02dat = W02dst;
099800150210         xsrda8(wlbdat);
099900150210         IF  G02err = '1';
100000150210           *in28 = *on;
100100150210           *in55 = *on;
100200150210           W02msg = msg(09);
100300150210           leavesr;
100400150210         ENDIF;
100500150210
100600150210         W02dst = G02dat;
100700150210         wDataScadNw = G02inv;
100800150210
100900150210       //?se < data ultima spedizione fatturata
101000150210       //?errore
101100150210         IF  wDataScadNw < wDataSP;
101200150210           *in28 = *on;
101300150210           *in55 = *on;
101400150210           W02msg = 'Scadenza Tariffa < Ultima Sped. Fatt. ' +
101500150210                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
101600150210                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
101700150210                    %subst(%editc(wDataSP:'X'):1:4);
101800150210           leavesr;
101900150210         ENDIF;
102000150210
102100150210         IF  wDataScadNw < sav_TAMdst;
102200150210           *in28 = *on;
102300150210           *in55 = *on;
102400150210           W02msg = 'Scadenza Tariffa < ' +
102500150210                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
102600150210                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
102700150210                    %subst(%editc(wDataSP:'X'):1:4) + ' ' +
102800150210                    'non si può anticipare.';
102900150210           leavesr;
103000150210         ENDIF;
103100150210
103200150210       ENDSR;
103300150210
103400150210       //--------------------------------------------------------------
103500150210       //?Aggiorno data scadenza ultimo progressivo tariffa.
103600150210       //--------------------------------------------------------------
103700150210       BEGSR AggDataDst;
103800150210
103900150210      //?Aggancio la tariffa da variare
104000150210         chain(e) (sav_TAMksc:sav_TAMctr:sav_TAMprg) TNTAM00L;
104100150210         IF  %error;
104200150210           *in28 = *on;
104300150210           W02msg = msg(10);
104400150210           exsr GesW02;
104500150210      /end-free
104600150210     c                   IF        *in90
104700150210     c                   goto      For03
104800150210     c                   ENDIF
104900150210      /free
105000150210         ENDIF;
105100150210         IF  not %found(TNTAM00L);
105200150210           leavesr;
105300150210         ENDIF;
105400150210      //?se modificata la data scadenza tariffa aggiorno TNTAM
105500150210         IF  TAMdst <> wDataScadNw;
105600150210           TAMdst = wDataScadNw;
105700150210           TAMdtr = %dec(%date());
105800150210           clear TAMftr;
105900150210           TAMduv = %dec(%date());
106000150210           update  TNTAML;
106100150210         ENDIF;
106200150210
106300150210       ENDSR;
106400150210
106500120305      /end-free
106600150210
106700040924      *------------------------------------------------------------------------*
106800040924      * - ROUTINE INIZIALE
106900040924      *------------------------------------------------------------------------*
107000040924     c     *Inzsr        BegSr
107100040924
107200040924     c     *Entry        Plist
107300040924     c                   Parm                    Kpjba
107400120315     c                   parm                    Parm48
107500120314
107600120314      /free
107700120314       //?controllo se passati tutti i parametri
107800120314         IF  %parms = 1;
107900120314           clear tnsb48ds;
108000120314           *in60 = *off;
108100120314         ELSE;
108200120315           clear tnsb48ds;
108300120315           tnsb48ds = parm48;
108400120314           *in60 = *on;
108500120314         ENDIF;
108600120223
108700120314      /end-free
108800040924
108900040924     c     *dtaara       define    §azute        azuteds
109000040924     c     *dtaara       define    §datiute      ddatiute
109100040924     c                   in(E)     *dtaara
109200040924     c                   If        %error  or RSUT = *blanks
109300040924     c                   Clear                   Tibs34ds
109400040924     c                   Call      'TIBS34R'
109500040924     c                   Parm                    Tibs34ds
109600040924     c                   In        *dtaara
109700040924     c                   EndIf
109800130124
109900130124       //?Recupero il flag di 'Modifica tariffe'
110000130124     c                   eval      dUTE01 = UTEfaf
110100120223
110200120223       // -?Il prossimo controllo lo faccio solo se non richiamato
110300120314     c                   IF        not *in60
110400040924
110500040924      * Verifico se il P.O. utente può manutenzionare le tariffe
110600040924      * deve esistere la tabella MTC per il p.o. utente
110700040924     c                   Clear                   Tibs02ds
110800040924     c                   Clear                   DMTC
110900040924     c                   Eval      T02Mod = 'C'
111000040924     c                   Eval      T02Sif = knsif
111100040924     c                   Eval      T02Cod = 'MTC'
111200040924     c                   Movel(p)  DutPou        T02Ke1
111300040924     c                   Call      'TIBS02R'
111400040924     c                   Parm                    Kpjba
111500040924     c                   Parm                    Tibs02ds
111600040924If  2c                   If        T02Err <> *Blanks
111700040924     c                   Seton                                        402808
111800040924     C                   movel     msg(11)       v2cmsg
111900040924     c                   GoTo      For01
112000040924   x2c                   Else
112100040924     c                   Movel     T02Uni        Dmtc
112200040924    2c                   EndIf
112300040924
112400090629     c* Richiamo solo per avere codice di abilitazione
112500090629     c                   clear                   tntaa1ds
112600120229     c                   eval      ITAA1tla = 'L'
112700090629     c                   eval      itaa1caut='§utegtc'
112800090629     c                   clear                   kpjbu
112900090629     c                   movel     tntaa1ds      kpjbu
113000090703     c                   call      'TNTAA1C'
113100090629     c                   parm                    kpjba
113200090629     c                   movel     kpjbu         tntaa1ds
113300090701     c
113400090701     c* Utente non abilitato alla gestione tariffe
113500090701     c                   if        otaa1fabi='N'
113600090701     c                   Seton                                        402808
113700090701     c                   movel     MSG(7)        v2cmsg
113800090701e   2c                   else
113900040924      *
114000040924      * Reperimento dei P.O. gestibili dall'utente
114100040924     c                   clear                   TRUL31DS
114200090629     c                   eval      I31abi = otaa1cabi
114300040924     c                   eval      I31cdi = DUTdis
114400040924     c                   eval      I31car = DUTare
114500040924     c                   eval      I31cpo = DUTpou
114600040924     c                   call      'TRUL31R'
114700040924     c                   parm                    KPJBA
114800090629     c                   parm                    TRUL31DS
114900040924if  2c                   if        O31pog > *zeros
115000040924     c                   movea     O31pog        fig
115100040924x   2c                   else
115200040924     c                   Seton                                        402808
115300040924     c                   movel     MSG(7)        v2cmsg
115400040924e   2c                   endif
115500090701e   2c                   endif
115600040924      *
115700090629      * Se l'utente ha autorizzazione 'AZ' è di sede
115800040924     C* SE SONO IN SEDE NON DEVO ABILITARE IL TASTO FUNZIONALE F7
115900040924     C* PIANO DEI CONTI
116000090629if  1c                   if        otaa1cabi <> 'AZ'
116100040924     C                   SETON                                        01
116200040924e   1c                   endif
116300120223
116400120223     c                   ENDIF
116500120223
116600090701     c
116700050530     c* Carico tabella tariffe particolari in scadenza, da eliminare
116800050530     C                   MOVE      '1P'          kCOD
116900050530     c                   clear                   xx                4 0
117000050530     c     ktab2         setll     tabel00f
117100050530     c     ktab2         reade     tabel00f
117200050530     c                   dow       not %eof(tabel00f)
117300050530     c                   if        tblflg=' '
117400050530     c                   movel     tbluni        ds1p
117500050530     c                   if        §1peli>*zeros
117600050530     c                   add       1             xx
117700050530     c                   movel     tblkey        tpareli(xx)
117800050530     c                   movel     §1peli        tpardat(xx)
117900050530     c                   endif
118000050530     c                   endif
118100050530     c
118200050530     c     ktab2         reade     tabel00f
118300050530     c                   enddo
118400040924
118500040924     C* ACCESSO TABEL00F
118600040924     C     KTAB          KLIST
118700040924     C                   KFLD                    KKUT
118800040924     C                   KFLD                    KCOD
118900040924     C                   KFLD                    KKEY
119000050530     C     KTAb2         KLIST
119100050530     C                   KFLD                    KKUT
119200050530     C                   KFLD                    KCOD
119300040924     C* ACCESSO CNACO00F
119400040924     C     KACO          KLIST
119500040924     C                   KFLD                    KKUT
119600040924     C                   KFLD                    dutKCI
119700040924     C                   KFLD                    WKSC
119800040924     C* ACCESSO TNTAM01L
119900040924     C     KTAM          KLIST
120000040924     C                   KFLD                    V2CKSC
120100040924     C                   KFLD                    WCTR
120200040924     C* ACCESSO TITPT01L
120300040924     C     KTPT          KLIST
120400040924     C                   KFLD                    TAMKSC
120500040924     C                   KFLD                    TAMCTR
120600040924     C                   KFLD                    TAMPRG
120700050530     C     Ktpt2         KLIST
120800050530     C                   KFLD                    v2cksc
120900050530     C                   KFLD                    kctr
121000050530     C                   KFLD                    wprgul
121100050530     C                   KFLD                    tpareli(xx)
121200040924     C*
121300040924     C* DEFINIZIONE DI ALCUNI CAMPI
121400040924     C     *LIKE         DEFINE    TBLCOD        KCOD
121500040924     C     *LIKE         DEFINE    TBLKEY        KKEY
121600040924     C     *LIKE         DEFINE    ACOKSC        WKSC
121700040924     C     *LIKE         DEFINE    TAMCTR        WCTR
121800040924     C     *LIKE         DEFINE    TAMCTR        WCTRUL
121900040924     C     *LIKE         DEFINE    TAMPRG        WPRGUL
122000040924     C     *LIKE         DEFINE    TAMPRG        WPRGLE
122100040924
122200040924     C* TESTATA PGM
122300040924     C                   MOVE      TES(1)        VTCTES
122400040924
122500040924     c                   Movel     rsut          VTCRSU
122600150210
122700150210      /free
122800150210       //?Imposto la libreria per il TITAS
122900150210         IF  %subst(knsif : 7 : 1) = 'P';
123000150210           wLib = 'GAITRAGRPS';
123100150210         ELSE;
123200150210           wLib = 'GAITRAGRU';
123300150210         ENDIF;
123400150210       //?Apro il file
123500150210         wFLib = %trim(wLib) + '/TITAS41C';
123600150210         open  TITAS41C;
123700150210      /end-free
123800040924
123900040924     c                   EndSr
124000950522     C*
124100940429     C*---------------------------------------------------------------*
124200001020**  TES
124300900409 ***  GESTIONE TARIFFE CLIENTI  ***
124400940622**  MSG
124500130124Utente non autorizzato al tasto funzione F9-Aggiunta Tariffa.                  1
124600940622Cliente non Codificato o Annullato                                             2
124700940622Codice Tariffa Inesistente                                                     3
124800941005Tariffa non manutenzionabile perche' gia' in uso !!                            4
124900941013Non esistono tariffe: per inserirle digitare il Codice Tariffa                 5
125000941128Effettuare almeno una selezione !!                                             6
125100090701UTENTE NON ABILITATO ALLA GESTIONE TARIFFE                                     7
125200090629Tariffa non in gestione all'utente                                             8
125300150210Data Errata.                                                                   9
125400150210Tariffa in modifica da altro utente riprovare più tardi.                       0
125500030703P.O. utente non abilitato alla manutenzione delle Tariffe Clienti              1
125600030703Password errata                                                                2
125700030703Password scaduta                                                               3
125800030703Immettere la password                                                          4
125900030703Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!               5
126000130926MANCA SCAGLIONE ISTAT  AVVISARE PRESIDIO VENDITE !!!!!!                        6
126100050530Presente TariffaPartic. "XX" da eliminare entro xx/xx/xx: non verrà duplicata  7
