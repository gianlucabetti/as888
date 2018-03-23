000100020708     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200940517     H* TNTA02R *----------------------------------------------------*
000300940502     H*             INTERROGAZIONE TARIFFE CLIENTI                   *
000400000000     H*--------------------------------------------------------------*
000500940517     FTNTA01D   CF   E             WORKSTN
000600940520     F                                     SFILE(TA01S03:NRR)
000700900409     FTABEL00F  IF   E           K DISK
000800940615     FTNTAM01L  IF   E           K DISK
000900120321     ftntam00l  if   e           k disk    rename(tntam000:tntaml)
001000990914     FTITPT01L  IF   E           K DISK
001100990914     FTITGC01L  IF   E           K DISK
001200000000     FCNACO00F  IF   E           K DISK
001300100407     fTFNTC01L  if   e           k disk
001400100427     fazorg01l  if   e           k disk
001500940502     D*
001600940502     D* DEFINIZIONE SCHIERE
001700940502     D TES             S             35    DIM(1) CTDATA PERRCD(1)
001800950421     D MSG             S             79    DIM(10) CTDATA PERRCD(1)
001900940502     D CTR             S              1  0 DIM(50)
002000951002     D* SCHIERA DI COMODO DOVE MUOVERCI IL CAMPO TBLUNI
002100951002     D SKI             S              3    DIM(29)
002200950805     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
002300020708     D COM             S              3    DIM(30)
002400040927
002500040927      * P.O. gestibili dall'utente
002600040927     d fig             s              3    dim(250) inz(*Zeros)
002700020708
002800020708     d paxdut          s             30
002900020708     d paxdmt          s                   LIKE(ACOrag)
003000020708     d paxccc          s                   LIKE(ACOkcc)
003100020708     d paxsta          s              1  0
003200020708     d paxflr          s             90
003300020708     d paxdit          s              3
003400020708     d paxnum          s              2  0
003500020708     d paxkcm          s             80
003600020708     d paxksm          s            140
003700020708     d paxkdm          s             60
003800090629
003900090629     d w0050           s              5  0
004000020708
004100040927     d codut           s              1  0 Inz(1)
004200040927     d kkut            s                   like(TblKut) Inz(1)
004300020708
004400940517     D* RICHIAMO PGM DI INTERROGAZ. TARIFFE CLIENTI   - TNTA02R2 -
004500000000     D PARAM           DS
004600950421     D* CODICE CLIENTE
004700940502     D  PARCLI                 2      8  0
004800950421     D* PROGRESSIVO TARIFFA
004900940502     D  PARPRG                 9     11  0
005000950421     D* CAPOCONTO CLIENTI
005100940502     D  PARKCI                12     15  0
005200950421     D* RAGIONE SOCIALE CLIENTE
005300940520     D  V2DKSC                16     63
005400950421     D* CODICE TARIFFA
005500940502     D  PARCTR                64     66
005600990914     D* FLAG DI RITORNO  DAI PROGRAMMI CHIAMATI
005700990914     D* 'C' FINE PGM
005800990914     D* 'R' RITORNO AL SUBFILE
005900990914     D  FLGRTN               127    127
006000940927     D                 DS
006100940927     D  AA                     1      4  0
006200100407     d  aa2                    3      4  0
006300940927     D  MM                     5      6  0
006400940927     D  GG                     7      8  0
006500940927     D  DATA                   1      8  0
006600000000     D KPJBA         E DS
006700991029     D DSTA01        E DS
006800100427     d OG141         e ds
006900040927
007000040927     d Azuteds       e ds                  extname(Azute00f)
007100040927     d dDatiute      e ds
007200090629     d tntaa1ds      e ds
007300040927     d Tibs02ds      e ds
007400040927     d Tibs34ds      e ds
007500040927     d Trul31ds      e ds
007600060207     d Tnta60ds      e ds
007700100427     d TNTA28DS      e ds
007800120321
007900120321       // -?ds dati passati da TNSB48R - Manca Tariffa
008000120321     d TNSB48ds      e ds
008100120321     d Parm48          s                   like(tnsb48ds)
008200120321
008300120321       // -?Flag booleani
008400120321     d wFine           s               n   inz(*off)
008500100427
008600100427      //---------------------------------------------------------------
008700100427      //?prototipi
008800100427      //---------------------------------------------------------------
008900120321      /copy gaitrasrc/srcprotopr,tnta02r2
009000100427      /copy gaitrasrc/srcprotopr,tnta28r
009100100427
009200940502     C*****************************************************************
009300940502     C* RIEPILOGO INDICATORI
009400940502     C*****************************************************************
009500940520     C* 05    - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
009600950421     C* 08    - QUALSIASI TASTO VENGA PREMUTO --> ESCO DAL PROGRAMMA
009700940520     C* 10    - ON --> INTERROGAZIONE
009800940520     C* 20    - PULIZIA SUBFILE (SFLCLR)
009900940520     C* 24    - DELETE  SUBFILE (SFLDLT)
010000940523     C* 27    - RICERCA ALFABETICA
010100940520     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
010200940520     C* 29    - LETTURA SUBFILE (READC)
010300940502     C* 30    - DI COMODO
010400940520     C* 31    - LETTURA PER CARICAMENTO SUBFILE
010500940520     C* 35    - USATO PER LOKUP
010600940520     C* 40/41 - ERRORI
010700940523     C* 42    - USATO IN MANUTENZIONE TARIFFE PER EMETTERE MESSAGGIO:
010800940523     C*         "PER INSERIRE UN NUOVO PRG DIGITARE IL CODICE TARIFFA"
010900120321      * 60    - Pgm richiamato da SB48
011000940502     C*****************************************************************
011100900409     C*
011200940502     C     INIZIO        TAG
011300940520     C                   CLEAR                   V2CKSC
011400940520     C                   CLEAR                   V2CCTR
011500940520     C                   CLEAR                   V2DKSC
011600120321       // -?Se richiamato
011700120321     c                   IF        *in60
011800120321       // -?imposto i dati del cliente
011900120321     c                   eval      v2cksc = ISB48ksc
012000120321     c                   eval      v2dksc = ISB48rag
012100120321       // -?controllo se esiste la tariffa
012200120321     c     V2Cksc        setll     TNTAM00L
012300120321    1c                   IF        not %equal(TNTAM00L)
012400120321     c                   eval      OSB48err = '1'
012500120321     c                   eval      OSB48msg = 'Non trovata tariffa'
012600120321     c                   goto      fine
012700120321     c                   ENDIF
012800120321       // -?mi posiziono sul file e vado subito in seconda videata
012900120321     c     V2Cksc        setll     TNTAM01L                               30
013000120321     c                   goto      for02
013100120321     c                   ENDIF
013200940502     C*
013300940502     C     FOR01         TAG
013400940520     C                   WRITE     TA01T01
013500940520     C                   EXFMT     TA01D02
013600940502     C*
013700940523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
013800940523     C                   CLEAR                   V2CMSG
013900940523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
014000940523     C                   SETOFF                                       284041
014100940523     C*
014200090629     C** F3  - FINE LAVORO
014300950418     C   KC
014400950421     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
014500090701     c**          perchè utente non abilitato
014600950421     COR 08              GOTO      FINE
014700940502     C*
014800941024     C** CMD7  - INTERROGAZIONE ANAGRAFICA PIANO DEI CONTI
014900940502     C     *INKG         IFEQ      *ON
015000060207     C**!!!              CALL      'CNC130R'
015100060207     C**!!!              PARM                    KPJBA
015200060207     c                   Clear                   tnta60ds
015300060207     c                   Eval      ta60flg = 'I'
015400060207     c                   Call      'TNTA60R'
015500060207     c                   Parm                    kpjba
015600060207     c                   Parm                    Tnta60ds
015700940502     C                   GOTO      FOR01
015800940502     C                   ENDIF
015900940502     C*
016000940502     C** CONTROLLI FORMATO
016100940520     C                   EXSR      CTRD02
016200940523     C*
016300940523     C   27
016400940523     COR 28              GOTO      FOR01
016500120321
016600120321     C     FOR02         TAG
016700940502     C*
016800940502     C* CARICAMENTO SUBFILE
016900940520     C                   EXSR      RIES03
017000900409     C*
017100940502     C* NRR = 0  --->  NESSUN PROGRESSIVO VALIDO CARICATO A VIDEO
017200940502    1C     NRR           IFEQ      0
017300940520     C   05              MOVEL     MSG(4)        V2CMSG
017400940520     C  N05              MOVEL     MSG(3)        V2CMSG
017500940520     C                   SETON                                        4028
017600940502     C                   GOTO      FOR01
017700940502    1C                   ENDIF
017800990914     C*
017900990914     C     FOR03         TAG
018000930202     C*
018100940502     C* EMETTO SUBFILE
018200940520     C                   WRITE     TA01T01
018300940520     C                   WRITE     TA01Z04
018400940520     C                   EXFMT     TA01C03
018500941110     C*
018600941110     C** CMD 3 - FINE
018700941110     C   KC              GOTO      FINE
018800120321
018900120321       //?Se richiamato con F12 esco dal pgm
019000120321     c                   IF        *in60 and *inkl
019100120321     c                   goto      fine
019200120321     c                   ENDIF
019300940502     C*
019400940502     C** CMD12 - RITORNO
019500940502     C   KL              GOTO      INIZIO
019600100427
019700100427      /free
019800120321       //?F1 - dati manca tariffa
019900120321        IF  *inka;
020000120321          exsr gesw01;
020100120321      /end-free
020200120321     c  N05V2CKSC        SETLL     TNTAM000                               30
020300120321     c   05KTAM          SETLL     TNTAM000                               30
020400120321     c                   exsr      ries03
020500120321     c                   GOTO      for03
020600120321     c                   ENDIF
020700120321      /free
020800100427       //?F8 - Note tariffa
020900110105        IF   *inkh;
021000100427          clear TNTA28ds;
021100100427          ITA28ela = 'I';
021200100427          ITA28tip = 'C';
021300100427          ITA28cod = V2cksc;
021400100427          ITA28rsc = V2dksc;
021500100427          kpjbu    = TNTA28DS;
021600100427          tnta28r (kpjba);
021700100427      /end-free
021800100427     C  N05V2CKSC        SETLL     TNTAM000                               30
021900100427     C   05KTAM          SETLL     TNTAM000                               30
022000100427     C                   EXSR      RIES03
022100100427     C                   GOTO      FOR03
022200100427     c                   ENDIF
022300100427
022400940502     C*
022500940502     C* CONTROLLO SUBFILE
022600940520     C                   READC     TA01S03                                29
022700940502     C*
022800940520    1C     *IN29         IFEQ      *ON
022900940502     C* CON ENTER VIENE PROPOSTA L'ULTIMA TARIFFA ELENCATA NEL SUBFILE
023000940520     C                   MOVEL     WCTRUL        PARCTR
023100940520     C                   Z-ADD     WPRGUL        PARPRG
023200940502   X1C                   ELSE
023300940502     C* CON 'X' O BARRA SPAZ. VIENE PROPOSTA LA TARIFFA SELEZIONATA
023400940520     C                   MOVEL     VSCCTR        PARCTR
023500940520     C                   Z-ADD     VSCPRG        PARPRG
023600940502    1C                   ENDIF
023700940502     C*
023800940502     C******
023900940517     C*** T N T A 0 2 R 2
024000940502     C******
024100940520     C                   MOVEL     V2CKSC        PARCLI
024200000000     C                   MOVEL     PARAM         KPJBU
024300120321       //?se non richiamato faccio richiamo semplice al pgm gestione dettaglio
024400120321     c                   IF        not *in60
024500940517     C                   CALL      'TNTA02R2'
024600000000     C                   PARM                    KPJBA
024700120321     C                   ELSE
024800120321       //?se richiamato devo passare la DS del TNSB48R al TNTA01R2
024900120321      /free
025000120321         tnta02r2 (kpjba:TNSB48DS);
025100120321      /end-free
025200120321
025300120321     C                   ENDIF
025400990914     C                   MOVEL     KPJBU         PARAM
025500940502     C*
025600990914     C*
025700990914     C* CONTROLLO IL FLAG DI RITORNO
025800990914     C* SE E' UGUALE A "R" RITORNO AL SUBFILE ALTRIMENTI VADO ALL'INIZIO
025900990914     C     FLGRTN        IFEQ      'R'
026000990914     C  N05V2CKSC        SETLL     TNTAM000                               30
026100990914     C   05KTAM          SETLL     TNTAM000                               30
026200990914     C                   EXSR      RIES03
026300990914     C                   GOTO      FOR03
026400990914     C                   ELSE
026500990914     C                   GOTO      INIZIO
026600990914     C                   ENDIF
026700990914     C*
026800000000     C     FINE          TAG
026900120321     c                   IF        *in60
027000120321     c                   eval      parm48 = tnsb48ds
027100120321     c                   ENDIF
027200000000     C                   SETON                                        LR
027300090629     C*
027400090629     C                   clear                   tntaa1ds
027500090629     C                   movel     'C'           Itaa1Tla
027600090629     C                   movel(p)  tntaa1ds      kpjbu
027700090703     C                   CALL      'TNTAA1C'
027800090629     C                   PARM                    KPJBA
027900090629     c
028000940502     C*
028100940502     C* DELETE SUBFILE
028200940520     C                   SETON                                        24
028300940520     C                   WRITE     TA01C03
028400940520     C                   SETOFF                                       24
028500900409     C*
028600940502     C*--- CARICAMENTO TABELLE ---------------------------------------*
028700900409     C     CARTAB        BEGSR
028800940502     C*
028900940502     C****  CODICI TARIFFA  ****
029000940502     C                   Z-ADD     1             X                 3 0
029100940523     C                   MOVEL     'TR'          KCOD
029200940502     C     KTAB          SETLL     TABEL
029300940502     C     KTAB          READE     TABEL                                  30
029400940502     C*
029500940502     C     *IN30         DOWEQ     *OFF
029600940502     C     TBLFLG        IFEQ      ' '
029700940502     C                   MOVEL     TBLKEY        CTR(X)
029800940502     C                   ADD       1             X
029900940502     C                   ENDIF
030000940502     C     KTAB          READE     TABEL                                  30
030100940502     C                   ENDDO
030200951009     C*
030300940502     C                   ENDSR
030400940502     C*
030500940502     C*--- CONTROLLI FORMATO -----------------------------------------*
030600940520     C     CTRD02        BEGSR
030700040927
030800940523     C                   SETOFF                                       2705
030900040927     C                   Z-ADD     dutkci        PARKCI
031000940502     C*
031100940502     C****  RICERCA ALFABETICA  ****
031200940502     C* SE CODICE CLIENTE A ZERO RICERCA ALFABETICA
031300940520    1C     V2CKSC        IFEQ      0
031400941128     C*
031500941128     C* SE ANCHE IL CAMPO DELLA RICERCA E' VUOTO ---> ERRORE
031600941128    2C     V2DKSC        IFEQ      *BLANKS
031700941128     C                   MOVEL     MSG(6)        V2CMSG
031800941128     C                   SETON                                        4028
031900941128     C                   GOTO      ENDCTR
032000941128    2C                   ENDIF
032100941128     C*
032200040927     c                   Z-add     1             xx
032300040927     c                   Clear                   com
032400040927     c     *zeros        Lookup    fig(xx)                                30
032500041008     c                   If        *In30 and xx <= 30
032600041008     C                   DO        30            XX                3 0
032700040927     C                   MOVE      FIG(XX)       COM(XX)
032800040927     C                   ENDDO
032900040927     c                   EndIf
033000020708     c                   movea     com           paxflr
033100020708
033200040927     c                   movel     rsut          paxdut
033300020708     c                   z-add     9             paxsta
033400040927     c                   z-add     dutkci        paxccc
033500020708     c                   movel(P)  v2dksc        paxdmt
033600020708     c                   z-add     1             paxnum
033700020708
033800020708     c                   call      'XALFA3BR'
033900020708     c                   parm                    paxdut
034000020708     c                   parm                    codut
034100020708     c                   parm                    paxdmt
034200020708     c                   parm                    paxccc
034300020708     c                   parm                    paxsta
034400020708     c                   parm                    paxflr
034500020708     c                   parm      *blanks       paxdit
034600020708     c                   parm                    paxnum
034700020708     c                   parm                    paxkcm
034800020708     c                   parm                    paxksm
034900020708     c                   parm                    paxkdm
035000020708
035100020708      * Non trovato o premuto F12
035200020708    2c                   if        paxsta > -1
035300020708     c                   movel     paxksm        v2cksc
035400020708    2c                   endif
035500020708
035600940523     C                   SETON                                        27
035700940520     C                   GOTO      ENDCTR
035800940502    1C                   ENDIF
035900040506     C*
036000040506     C****  CODICE CLIENTE  ****
036100040506     C                   Z-ADD     V2CKSC        WKSC
036200040506     C     KACO          CHAIN     CNACO000                           30
036300040506     C  N30ACOFLG        COMP      '*'                                    30
036400040506     C*
036500040506    1C     *IN30         IFEQ      *ON
036600040506     C                   MOVEL     MSG(1)        V2CMSG
036700040506     C                   SETON                                        4028
036800040506     C                   GOTO      ENDCTR
036900040506    1C                   ENDIF
037000040506     C*
037100040506     C* DECODIFICA CODICE CLIENTE
037200040506     C                   MOVEL     ACORAG        V2DKSC
037300950421     C*
037400950421     C****  CONTROLLO SE CONTO GESTIBILE  ****
037500090629     c                   clear                   tntaa1ds
037600120321     c                   eval      ITAA1tla = 'L'
037700090629     c                   eval      itaa1caut='§utegtc'
037800090629     c                   eval      itaa1ksc=v2cksc
037900090629     c                   clear                   kpjbu
038000090629     c                   movel     tntaa1ds      kpjbu
038100090703     c                   call      'TNTAA1C'
038200090629     c                   parm                    kpjba
038300090629     c                   movel     kpjbu         tntaa1ds
038400090629     c
038500090629      *Tariffa di cartello sempre OK
038600090629    1c                   if        otaa1fabi='N'
038700090629     c                   Movel     v2cksc        w0050
038800090629    2c                   If        w0050 <>88888
038900090629     C                   MOVEL     MSG(8)        V2CMSG
039000090629     c                   Seton                                        4028
039100090629     c                   goto      endctr
039200090629    2c                   endif
039300090629    1c                   endif
039400090629     c*
039500090629     C**                 MOVEL     V2CKSC        W003a             3
039600950805     C*
039700950706     C* CONTROLLO SE LA FILIALE COLLEGATA PUO' GESTIRE LE TARIFFE DEL
039800950706     C*   CODICE CLIENTE DIGITATO
039900090629     C**   W003a         LOOKUP    FIG                                    35
040000951002     C*
040100090629    2C**   *IN35         IFEQ      *OFF
040200040506     c* se non può verifico cod.commerciale da anagrafica cliente
040300090629     c**                 eval      wksc=v2cksc
040400090629     c**   kaco          chain     cnclp000
040500090629    3c**                 if        not %found(cnclp00f)
040600090629     c**                 clear                   wporagru          3
040700090629     c**                 else
040800090629     c**                 eval      wcmm=clpage
040900090629     c**                 exsr      cercmm
041000090629     c**                 movel     §01rgf        wporagru
041100090629    3c**                 endif
041200090629     C**   Wporagru      LOOKUP    FIG                                    35
041300090629    2C**   *IN35         IFEQ      *OFF
041400090629     C**                 MOVEL     MSG(7)        V2CMSG
041500090629     C*+                 SETON                                        4028
041600090629     C**                 GOTO      ENDCTR
041700090629    2C**                 ENDIF
041800090629    1C**                 ENDIF
041900940502     C*
042000940502     C****  CODICE TARIFFA  ****
042100940502     C* CONTROLLO TIPO TARIFFA SE IMMESSO
042200940520    1C     V2CCTR        IFNE      *BLANKS
042300960422     C*
042400960422     C                   TESTN                   V2CCTR               35
042500960422     C*
042600960422     C* CONTROLLO CHE L'ULTIMO CARATTERE DEL CODICE TARIFFA NON SIA
042700960422     C*   UNA LETTERA
042800960422     C   35              MOVE      V2CCTR        W001              1
042900960422    2C   35W001          IFLT      '0'
043000960422     C                   SETOFF                                       35
043100960422    2C                   ENDIF
043200960422     C*
043300960422    2C     *IN35         IFEQ      *OFF
043400960422     C                   MOVEL     MSG(2)        V2CMSG
043500960422     C                   SETON                                        4128
043600960422     C                   GOTO      ENDCTR
043700960422    2C                   ENDIF
043800960422     C*
043900940520     C                   MOVEL     V2CCTR        W0010             1 0
044000940520     C     W0010         LOOKUP    CTR                                    35
044100940520     C     *IN35         IFEQ      *OFF
044200940520     C                   MOVEL     MSG(2)        V2CMSG
044300940520     C                   SETON                                        4128
044400940520     C                   GOTO      ENDCTR
044500940502     C                   ENDIF
044600940502     C*
044700940520     C* 05 ON  - E' STATO DIGITATO IL CODICE TARIFFA IN PRIMA VIDEATA
044800940520     C                   SETON                                        05
044900940520     C                   MOVEL     V2CCTR        WCTR
045000940502    1C                   ENDIF
045100940502     C*
045200940502     C****  CONTROLLO SE ESISTONO TARIFFE PER LE SELEZIONI FATTE  ****
045300940615     C   05KTAM          SETLL     TNTAM000                               30
045400940615     C  N05V2CKSC        SETLL     TNTAM000                               30
045500940502    1C     *IN30         IFEQ      *OFF
045600940520     C   05              MOVEL     MSG(4)        V2CMSG
045700940520     C  N05              MOVEL     MSG(3)        V2CMSG
045800940520     C                   SETON                                        4028
045900940520     C                   GOTO      ENDCTR
046000940502    1C                   ENDIF
046100940502     C*
046200940520     C     ENDCTR        ENDSR
046300940502     C*
046400940502     C*--- CARICAMENTO SUBFILE ---------------------------------------*
046500940520     C     RIES03        BEGSR
046600940520     C*
046700940520     C* RIEMPO CAMPI DEL SFLCONTROL
046800940520     C                   MOVEL     V2CKSC        VCCKSC
046900940520     C                   MOVEL     V2DKSC        VCDKSC
047000940520     C*
047100940502     C* PULIZIA SUBFILE
047200940520     C                   SETON                                        20
047300940520     C                   WRITE     TA01C03
047400940520     C                   SETOFF                                       20
047500940502     C*
047600940502     C                   Z-ADD     0             NRR               4 0
047700940502     C*
047800940615     C   05KTAM          READE     TNTAM000                               31
047900940615     C  N05V2CKSC        READE     TNTAM000                               31
048000940502     C*
048100940520    1C     *IN31         DOWEQ     *OFF
048200940502     C*
048300940502     C* ESCLUDO RECORD ANNULLATI
048400940502    2C     TAMATB        IFEQ      ' '
048500940502     C* ULTIMO PROGRESSIVO CARICATO A VIDEO
048600940520     C                   Z-ADD     TAMCTR        WCTRUL
048700940520     C                   Z-ADD     TAMPRG        WPRGUL
048800940502     C*
048900940502     C* CAMPO SELEZIONE
049000940520     C                   MOVEL     *BLANKS       VSCSCE
049100940502     C*
049200940502     C* CODICE E PROGRESSIVO TARIFFA
049300940520     C                   MOVEL     TAMCTR        VSCCTR
049400940520     C                   MOVEL     TAMPRG        VSCPRG
049500991029     C                   MOVEL     TAMFLO        DSTA01
049600991029     C                   MOVEL     §TADIV        VSCDIV
049700940502     C*
049800940502     C* GIRO DATA DECORRENZA TARIFFA
049900940927     C                   Z-ADD     TAMDDT        DATA
050000940927     C                   Z-ADD     MM            GGMM              4 0
050100940927     C                   MOVEL     GG            GGMM
050200100407     C                   Z-ADD     AA2           VSCDDT
050300940927     C                   MOVEL     GGMM          VSCDDT
050400940502     C* GIRO DATA SCADENZA   TARIFFA
050500940927     C                   Z-ADD     TAMDST        DATA
050600940927     C                   Z-ADD     MM            GGMM
050700940927     C                   MOVEL     GG            GGMM
050800100407     C                   Z-ADD     AA2           VSCDST
050900940927     C                   MOVEL     GGMM          VSCDST
051000990914     C* CERCO LA DATA ULTIMA VARIAZIONE PIU' ALTA DI TUTTI
051100990914     C                   EXSR      CERDUV
051200940502     C* GIRO DATA ULTIMA VARIAZIONE
051300940927     C                   Z-ADD     TAMDUV        DATA
051400940927     C                   Z-ADD     MM            GGMM
051500940927     C                   MOVEL     GG            GGMM
051600100407     C                   Z-ADD     AA2           VSCDUV
051700940927     C                   MOVEL     GGMM          VSCDUV
051800100407      /free
051900100407       //?Controllo se ci sono note tariffa (10)
052000100407         clear vscnote;
052100100407         NTCapl = 'C' ;
052200100407         NTCnk1 = %editc(dutkci: 'X') + %editc(v2cksc:'X');
052300100407         NTCnk2 = %editc(vscctr: 'X');
052400100407         NTCtnt = '10';
052500100407         setll (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
052600100407         IF  %equal(TFNTC01L);
052700100407           vscnote = 'SI';
052800100407         ELSE;
052900100407       //?provo con chiave generica senza codice tariffa
053000100407           clear NTCnk2;
053100100407           setll (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
053200100407           IF  %equal(TFNTC01L);
053300100407             vscnote = 'SI';
053400100407           ENDIF;
053500100407    2    ENDIF;
053600100407      /end-free
053700940502     C*
053800940502     C* DESCRIZIONE TARIFFA
053900940520     C                   MOVEL     TAMDCV        VSCDCV
054000020320     C* TIPO SERVIZIO
054100941110     C                   MOVEL     TAMTSP        VSCTSP
054200020320     C* DPD/FEDEX/ITALIA/ESTERO
054300020320     C                   SELECT
054400020321  1A C     §TADPD        WHENEQ    'S'
054500020320     C                   MOVE      'D'           VSCFIE
054600020320     C*
054700020321  1A C     §TAFED        WHENEQ    'S'
054800020320     C                   MOVE      'F'           VSCFIE
054900020320     C*
055000020321     C                   OTHER
055100941110     C                   MOVEL     TAMFIE        VSCFIE
055200020320     C                   ENDSL
055300940502     C*
055400940502     C                   ADD       1             NRR
055500940520     C                   WRITE     TA01S03
055600940502    2C                   ENDIF
055700940502     C*
055800940615     C   05KTAM          READE     TNTAM000                               31
055900940615     C  N05V2CKSC        READE     TNTAM000                               31
056000940502    1C                   ENDDO
056100940502     C*
056200940502     C                   ENDSR
056300990914     C*
056400990914     C*--- RICERCA ULTIMA DATA VARIAZIONE TARIFFA --------------------*
056500990914     C     CERDUV        BEGSR
056600990914     C*
056700990914     C* LEGGO TUTTE LE TESTATE TARIFFE PARTICOLARI  CERCANDO LA DATA
056800990914     C* MAGGIORE DI ULTIMA MODIFICA
056900990914     C     KTPT          SETLL     TITPT01L
057000990914     C     KTPT          READE     TITPT01L                               33
057100990914     C*
057200990914     C     *IN33         DOWEQ     *OFF
057300990914     C* VERIFICO SE LA DATA ULTIMA VARIAZIONE E' MAGGIORE DI QUELLA
057400990914     C* DELLA TESTATA
057500990914     C     TPTDUV        IFGT      TAMDUV
057600990914     C     TPTATB        ANDEQ     ' '
057700990914     C                   Z-ADD     TPTDUV        TAMDUV
057800990914     C                   ENDIF
057900990914     C*
058000990914     C     KTPT          READE     TITPT01L                               33
058100990914     C*
058200990914     C                   ENDDO
058300990914     C* VERIFICO ANCHE L'ULTIMA DATA VARIAZIONE TARIFFA CHE C'E' IN
058400990914     C* TITGC TARIFFE GIACENZE  (SE ESISTE)
058500990914     C     KTPT          CHAIN     TITGC01L                           33
058600990914     C     *IN33         IFEQ      *OFF
058700990914     C     TGCATB        ANDEQ     ' '
058800990914     C     TGCDUV        ANDGT     TAMDUV
058900990914     C                   Z-ADD     TGCDUV        TAMDUV
059000990914     C                   ENDIF
059100990914     C*
059200990914     C                   ENDSR
059300120321
059400120321      /free
059500120321       //--------------------------------------------------------------
059600120321       //?Visualizza Dati manca tariffa
059700120321       //--------------------------------------------------------------
059800120321       BEGSR GesW01;
059900120321
060000120321         wFine  = *off;
060100120321         W01dsp = ISB48dsp;
060200120321         W01tbl = ISB48tbl;
060300120321         W01lnp = ISB48lnp;
060400120321         W01lna = ISB48lna;
060500120321         W01pro = ISB48pro;
060600120321         W01cts = ISB48cts;
060700120321         W01ctr = ISB48ctr;
060800120321         W01ncl = ISB48ncl;
060900120321         W01pkf = ISB48pkf;
061000120321         W01vlf = ISB48vlf;
061100120321         W01err = ISB48err;
061200120321
061300120321         DOW wFine = *off;
061400120321           exfmt  TA01W01;
061500120321           //?- F12 = Ritorno
061600120321           IF  *inkl;
061700120321             wFine = *on;
061800120321           ENDIF;
061900120321         ENDDO;
062000120321
062100120321       ENDSR;
062200120321      /end-free
062300120321
062400040927      *------------------------------------------------------------------------*
062500040927      * - ROUTINE INIZIALE
062600040927      *------------------------------------------------------------------------*
062700040927     c     *Inzsr        BegSr
062800040927
062900040927     c     *Entry        Plist
063000040927     c                   Parm                    Kpjba
063100120321     c                   parm                    Parm48
063200120321
063300120321      /free
063400120321       //?controllo se passati tutti i parametri
063500120321         IF  %parms = 1;
063600120321           clear tnsb48ds;
063700120321           *in60 = *off;
063800120321         ELSE;
063900120321           clear tnsb48ds;
064000120321           tnsb48ds = parm48;
064100120321           *in60 = *on;
064200120321         ENDIF;
064300120321
064400120321      /end-free
064500040927
064600040927     c     *dtaara       define    §azute        azuteds
064700040927     c     *dtaara       define    §datiute      ddatiute
064800040927     c                   in(E)     *dtaara
064900040927     c                   If        %error  or RSUT = *blanks
065000040927     c                   Clear                   Tibs34ds
065100040927     c                   Call      'TIBS34R'
065200040927     c                   Parm                    Tibs34ds
065300040927     c                   In        *dtaara
065400040927     c                   EndIf
065500120321
065600120321       // -?Il prossimo controllo lo faccio solo se non richiamato
065700120321     c                   IF        not *in60
065800040927      *
065900090629     c* Richiamo solo per avere codice di abilitazione
066000090629     c                   clear                   tntaa1ds
066100090629     c                   eval      itaa1caut='§utegtc'
066200090629     c                   clear                   kpjbu
066300090629     c                   movel     tntaa1ds      kpjbu
066400090703     c                   call      'TNTAA1C'
066500090629     c                   parm                    kpjba
066600090629     c                   movel     kpjbu         tntaa1ds
066700090701     c
066800090701     c* Errore per utente non abilitato alle tariffe
066900090701     c                   if        otaa1fabi='N'
067000090701     c                   Seton                                        402808
067100090701     c                   movel     MSG(5)        v2cmsg
067200090701     c                   else
067300090629      *
067400040927      * Reperimento dei P.O. gestibili dall'utente
067500040927     c                   clear                   TRUL31DS
067600090629     c                   eval      I31abi = otaa1cabi
067700040927     c                   eval      I31cdi = DUTdis
067800040927     c                   eval      I31car = DUTare
067900040927     c                   eval      I31cpo = DUTpou
068000040927     c                   call      'TRUL31R'
068100040927     c                   parm                    KPJBA
068200040927     c                   parm                    TRUL31DS
068300040927if  2c                   if        O31pog > *zeros
068400040927     c                   movea     O31pog        fig
068500040927x   2c                   else
068600040927     c                   Seton                                        402808
068700040928     c                   movel     MSG(5)        v2cmsg
068800040927e   2c                   endif
068900090701e   2c                   endif
069000090701     c
069100040927      * Aggiungo 888 nella sk fig x poter interrogare le tariffe di cartello
069200040927     c                   Z-add     1             xx                3 0
069300040927     c     *zeros        Lookup    fig(xx)                                30
069400040927     c                   If        *In30
069500040927     c                   Move      '888'         fig(xx)
069600040927     c                   EndIf
069700120321
069800120321     c                   ENDIF
069900120321
070000040927
070100040927     C* ACCESSO TABEL00F
070200040927     C     KTAB          KLIST
070300040927     C                   KFLD                    KKUT
070400040927     C                   KFLD                    KCOD
070500040927     C     KTAB2         KLIST
070600040927     C                   KFLD                    KKUT
070700040927     C                   KFLD                    KCOD
070800040927     C                   KFLD                    KKEY
070900040927     C* ACCESSO CNACO00F
071000040927     C     KACO          KLIST
071100040927     C                   KFLD                    KKUT
071200040927     C                   KFLD                    dutKCI
071300040927     C                   KFLD                    WKSC
071400040927     C* ACCESSO TNTAM01L
071500040927     C     KTAM          KLIST
071600040927     C                   KFLD                    V2CKSC
071700040927     C                   KFLD                    WCTR
071800040927     C* ACCESSO TITPT01L
071900040927     C     KTPT          KLIST
072000040927     C                   KFLD                    TAMKSC
072100040927     C                   KFLD                    TAMCTR
072200040927     C                   KFLD                    TAMPRG
072300040927     C*
072400040927     C* DEFINIZIONE DI ALCUNI CAMPI
072500040927     C     *LIKE         DEFINE    TBLCOD        KCOD
072600040927     C     *LIKE         DEFINE    TBLKEY        KKEY
072700040927     C     *LIKE         DEFINE    ACOKSC        WKSC
072800040927     C     *LIKE         DEFINE    TAMCTR        WCTR
072900040927     C     *LIKE         DEFINE    TAMCTR        WCTRUL
073000040927     C     *LIKE         DEFINE    TAMPRG        WPRGUL
073100040927
073200040927     C* TESTATA PGM
073300040927     C                   MOVE      TES(1)        VTCTES
073400040927
073500040927     c                   Movel     rsut          VTCRSU
073600040927
073700040927     C* 10 ON  - INTERROGAZIONE
073800040927     C                   SETON                                        10
073900040927     C* CARICAMENTO TABELLE
074000040927     C                   EXSR      CARTAB
074100040927
074200040927     c                   EndSr
074300990914     C*
074400000000     C*---------------------------------------------------------------*
074500950421**  TES
074600900409 ** INTERROGAZ. TARIFFE  CLIENTI **
074700940502**  MSG
074800940622Cliente non Codificato o Annullato                                             1
074900940622Codice Tariffa Inesistente                                                     2
075000940622Non esistono Tariffe per questo Cliente                                        3
075100940622Non esistono Tariffe per questo Codice Tariffa                                 4
075200090701UTENTE NON ABILITATO ALLA GESTIONE TARIFFE                                     5
075300941128Effettuare almeno una selezione !!                                             6
075400090629
075500090629Tariffa non in gestione all'utente                                             8
