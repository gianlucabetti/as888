000100100115     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200130927     H DFTACTGRP(*NO)
000300011203      *****************************************************************
000400011114      *
000500011114      * Gestione Profili utente
000600011114      *
000700011115      *****************************************************************
000800011115      *  RIEPILOGO INDICATORI
000900040728      *****************************************************************
001000150618      * 02      - Modifica: Protect campi chiave
001100150618      * 03      - Copia
001200060126      * 05      - Interrogazione
001300011211      * 30      - chnin su AzUte
001400011120      * 28      - Errore generico
001500040922      * 60-67     Ricerche
001600130618      * da 39   - Errori videate dettaglio
001700011114      *--------------------------------------------------------------------------------------------*
001800011114      * Data base                                                                                  *
001900011114      *--------------------------------------------------------------------------------------------*
002000011115     FAzOrg01l  IF   E           K DISK
002100100114     FAzUte02l  IF   E           K DISK     rename(azute000:azute02)
002200100114     FAzUte01l  UF A E           K DISK
002300100114     FKpPrf01l  UF   E           K DISK
002400011114     FTibs31d   CF   E             WORKSTN
002500130705     FTNTBE01L  if   e           k disk
002600011114      *--------------------------------------------------------------------------------------------*
002700011114      * Data structure                                                                             *
002800011114      *--------------------------------------------------------------------------------------------*
002900101117      * Costanti
003000101117      *---
003100101117     d c_Digits        c                   const('0123456789')
003200130708
003300130708      * Campo per dspatr "REVERSE IMAGE"
003400130708     d rimm            c                   const(X'21')
003500101117      *---
003600011114      * Schiere a tempo di compilazione
003700011114      *---
003800011114     D Opz             S             15    DIM(4) CTDATA PERRCD(1)              *Messaggi video
003900150618     D Msg             S             78    DIM(26) CTDATA PERRCD(1)             *Messaggi video
004000130705
004100130705       // -?Schiere per abilitazioni profili standard
004200130705     d skUTE           s             10    dim(999)
004300130705     d skABI           s            500    dim(999)
004400130705     d skAUT           s              2    dim(999)
004500130705     d skFUN           s              5    dim(999)
004600130705     d skABI1          s             10    dim(999)
004700130705
004800160830       // -?Status ds?
004900160830     d Status         sds
005000160830     d   SDSprm          *parms
005100080218      *---
005200080218      * Architettura
005300080218      *---
005400080218     D Kpjba         E DS                                                       *architettura
005500011114      *---
005600011114      * Schiere memorizzazione dati correnti
005700011114      *---
005800011203     D Tibs31Ds      E DS
005900160830     D Tibs31Ds2     e ds                  qualified
006000040729     D Tibs32Ds      E DS                  inz
006100080218     D UT§DSE0F      E DS
006200020329     D DUte01        E DS
006300100114     d DUte02        e ds                  inz
006400130705     d sUTE01        e ds                  extname(dUTE01) prefix(S:1)
006500130705     d sUTE02        e ds                  extname(dUTE02) inz prefix(S:1)
006600130927     d uteautds      e ds
006700130705
006800130705       // -?File AZUTE00F
006900130705     D AzuteDs       E DS                  ExtName(AzUte00F)
007000130705
007100011114      *---
007200020402      * Ds esterne
007300011114      *---
007400080218     D Tibs02Ds      E DS                  inz
007500080218     d   T02mod      e                     inz('C')
007600080218     d   T02cod      e                     inz('LAT')
007700080218     D DLat          E DS
007800080218      *
007900080218     D TBCMUS        E DS                  EXTNAME(TNTBCMUS)
008000140522
008100140522      * Campo ORGde0 di AZORG00F
008200140522     d OG140         e ds                  inz qualified
008300140522      * Campo ORGde3 di AZORG00F
008400140522     d OG143         e ds                  inz qualified
008500140522
008600011115      *------------
008700011115      * DS "XSRDA8" - CONTROLLA DATA (8)
008800011115      *-------------
008900011115     D WLBDA8          DS                  INZ
009000011115     D  G08DAT                 1      8  0
009100011115     D  G08INV                 9     16  0
009200011115     D  G08ERR                17     17
009300011115     D  G08TGI                18     22  0
009400011221      *
009500011221     D EMA             DS                  INZ
009600011221     D  EMA1                   1     50
009700011221     D  EMA2                  51    200
009800080218      *----------
009900080218      * Variabili
010000080218      *----------
010100080218     D $Fil            S                   LIKE(UteFil)
010200080218     D $cdu            S                   LIKE(Utecdu)
010300080218     D $Ute            S                   LIKE(UteUte)
010400080218      *
010500080218     D $Fine           S              1    INZ('N')                             *fine programma
010600080218     D TipVid          S              1    INZ('1')                             *video in gestione
010700080218     D $CarD01         S              1    INZ('2')
010800080218     D $Mod            s              1    inz
010900080218     D $Riemetti       s               n   inz(*off)
011000080218     D $PostSearch     s               n   inz(*off)
011100080218     D $NoPrf          s               n   inz(*off)
011200080218     D $Prima          s               n   inz(*off)
011300100114     D $InzW02         s               n   inz(*on)
011400080218     D Win             S             99A
011500080218     D in2841          s             14a   inz(*zeros)
011600130709     d savfun          s                   like(UTEfun)
011700140522     d wrkUTE          s             10    inz
011800130705
011900130705     d wData           s              8  0
012000130705     d wData_EUR       s               d   datfmt(*eur)
012100130705     d wData_ISO       s               d   datfmt(*iso)
012200130705
012300130705       // -?Flags booleani
012400130705     d wEnd            s               n   inz(*off)
012500130709     d wRIE01          s               n   inz(*off)
012600130806     d wOKuteSTD       s               n   inz(*off)
012700130705
012800130705       // -?Indici di schiera
012900130705     d xx              s              4  0 inz
013000130705
013100100115       // -?Parametri per TIBS31C (Modifica descriz. profilo utente)?
013200100115     d BS31err         s              1a   inz
013300101117
013400101117       // -?Progressivo per nuovo codice utente di filiale (xxxyyyNN)?
013500101117     d wProgr          s              2  0 inz
013600100115
013700100115       //--------------------------------------------------------------
013800100115       //?Definizione prototipi procedure.                             ?
013900100115       //--------------------------------------------------------------
014000100115
014100100115       // -?Modifica descrizione del profilo utente?
014200100115      /copy gaitrasrc/srcProtoPR,TIBS31C
014300161006
014400161006       // -?Gestione NOTE profilo utente?
014500161006     d TIBS43ds      e ds                  inz
014600161006     d   iBS43tla    e                     inz('L')
014700161006     d   oBS43err    e                     inz(*off)
014800161006      *
014900161006     d TIBS43R         pr                  extpgm('TIBS43R')
015000161006     d   kpjba                             likeds(KPJBA)
015100150317
015200150317       // -?Ricerca/Controllo Tabelle
015300150317      /copy gaitrasrc/srcProtoPR,TIBS02R
015400130705
015500130705     D*--------------------------------------------------
015600130705     D* Procedure name: GetProfiloUtenteProj
015700130705     D* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
015800130705     D* Returns:        Profilo utente in Proj.
015900130705     D* Parameter:      priProfiloUtente => Profilo Utente
016000130705     D*--------------------------------------------------
016100130705     D GetProfiloUtenteProj...
016200130705     D                 PR             1A
016300130705     D  priProfiloUtente...
016400130705     D                               10A
016500130930      *-----------------------------------------------
016600011114      * Main lines
016700011114      *--------------------------------------------------------------------------------------------*
016800040816      *
016900100114     c                   if        I31opz = '9'
017000100114     c                   eval      TipVid = 'B'
017100100114     c                   endif
017200160901      *
017300160901     c                   if        I31opz  = '3'  and
017400160901     c                             SDSprm  > 2
017500160901     c                   eval      $CarD01 = *on
017600160901     c                   endif
017700100114      *
017800040816      * Inizializzazione videate
017900080530     c                   clear                   Bs31D01
018000040816     C                   clear                   Bs31D02
018100040922     C                   clear                   Bs31D03
018200080218     C                   clear                   Bs31D04
018300130806     C                   clear                   Bs31D05
018400011114      *
018500011114      * Gestione video
018600011114     C     $Fine         DOUEQ     'S'
018700080218     C     TipVid        CASEQ     '1'           GesD01
018800080218     C     TipVid        CASEQ     '2'           GesD02
018900040922     C     TipVid        CASEQ     '3'           GesD03
019000100917     C     TipVid        CASEQ     '³'           GesD03B
019100080218     C     TipVid        CASEQ     '4'           GesD04
019200130806     C     TipVid        CASEQ     '5'           GesD05
019300130927     C     TipVid        CASEQ     '6'           GesD06
019400080530     C     TipVid        CASEQ     'W'           GesW01
019500100114     c     TipVid        casEQ     'B'           sr_GesW02
019600011114     C                   ENDCS
019700011114     C                   ENDDO
019800011114      *
019900080218     C                   eval      *inLR = *on
020000011114      *--------------------------------------------------------------------------------------------*
020100011120      * GesD01 - Gestione videata dettaglio -
020200011114      *--------------------------------------------------------------------------------------------*
020300011120     C     GesD01        BEGSR
020400011114      *
020500011114      * Inizializzazione videata
020600080218if  1C                   if        $CarD01 <> *off
020700080218     C                   exsr      CarD01
020800011120     C                   move      *off          $CarD01
020900160830      * -?Richiamato per copia  e  Copia eseguita?
021000160830     c                   if        SDSprm > 2  and  $Fine = 'S'
021100160830     c                   leavesr
021200160830     c                   endif
021300080218e   1C                   endif
021400011210      *
021500011210      * Se non ci sono errori nel caricamento della videta proseguo
021600080218if  1C                   if        O31Err = 'E'
021700080218     C                   leavesr
021800080218e   1C                   endif
021900080218      *
022000011114      * Scrivo la testata
022100080218     C                   write     Bs31T01
022200011114      *
022300011114      * Se esistono errori sulla videata
022400011114      * emetto la write del formato a indicatori spenti per vedere
022500011114      * le eventuali decodifiche
022600080218if  1C                   if        *in28
022700080218     C                   movea     *in           Win
022800011121     C                   move      *ALL'0'       in2849           22
022900080218     C                   movea     in2849        *in(28)
023000080218     C                   write     Bs31D01
023100080218     C                   movea     Win           *in
023200080218e   1C                   endif
023300011114      *
023400080218if  1C                   if        I31Opz = '5'
023500080218      * Visualizzazione
023600080218     C                   write     Bs31D01
023700080218     C                   exfmt     PROTECT
023800080218x   1C                   else
023900011114      * Immissione/Modifica/Copia
024000080218     C                   exfmt     Bs31D01
024100080218e   1C                   endif
024200011114      *
024300080218     c                   setoff                                       28  90
024400080218     c                   clear                   V1Dmsg
024500011114      *
024600080218sel 1C                   SELECT
024700011114      * F3=Fine
024800080218w   1C                   WHEN      *inKC
024900080218     C                   exsr      F03Ges
025000080218     c                   leavesr
025100040729      * F8=Modulo_Base + AS/400 del profilo
025200080218w   1C                   WHEN      *inKH
025300040729     c                   exsr      CallTIBS32
025400080218     c                   leavesr
025500011114      * F12=Ritorno
025600080218w   1C                   WHEN      *inKL
025700080218     C                   exsr      F12Ges
025800080218     c                   leavesr
025900161006      * F18=Note
026000161006w   1c                   WHEN      *inKS
026100161006     c                   exsr      sr_CallTIBS43
026200161006     c                   leavesr
026300011122      *
026400080218e   1C                   ENDSL
026500011114      *
026600011114      * Controlli e decodifiche
026700080218     C                   exsr      CtrD01
026800011114      *
026900080218if  1C                   if             *in28       = *off
027000080218     C                             and  $PostSearch = *off
027100080218     C                             and  $Riemetti   = *off
027200011126      * Carico la seconda videata
027300011126     C                   eval      TipVid  = '2'
027400080218e   1C                   endif
027500011114      *
027600080218     C                   ENDSR
027700011114      *--------------------------------------------------------------------------------------------*
027800011114      * F03Ges - Tasto funzionale F03 -> Fine programma
027900011114      *--------------------------------------------------------------------------------------------*
028000011114     C     F03Ges        BEGSR
028100011114      *
028200011203     C                   clear                   O31Tfu
028300011203     C                   clear                   O31Err
028400011203     C                   clear                   O31Msg
028500011114     C                   movel     'S'           $Fine                          *fine pgm
028600011203     C                   movel     'F03'         O31Tfu                         *fine pgm
028700011114      *
028800011114     C                   ENDSR
028900011120      *--------------------------------------------------------------------------------------------*
029000011120      * F12Ges - Tasto funzionale f12 -> Ritorno
029100011120      *--------------------------------------------------------------------------------------------*
029200011120     C     F12Ges        BEGSR
029300011120      *
029400080218     c                   SELECT
029500011121      * Dalla prima videata torno al sfl
029600080218     C                   WHEN      TipVid = '1'
029700011203     C                   clear                   O31Tfu
029800011203     C                   clear                   O31Err
029900011203     C                   clear                   O31Msg
030000011203     C                   eval      O31Tfu = 'F12'
030100011120     C                   movel     'S'           $Fine                          *fine pgm
030200011121      *
030300011121      * Dalla seconda videata torno alla prima
030400080218     C                   WHEN      TipVid = '2'
030500080218     C                   eval      TipVid = '1'
030600080218     C                   eval      *in28  = *off
030700040922      *
030800040922      * Dalla terza videata torno alla seconda
030900080218     C                   WHEN      TipVid = '3'
031000080218     C                   eval      TipVid = '2'
031100080218     C                   eval      *in28  = *off
031200100917      *
031300100917      * Dalla terza/B videata torno alla terza
031400100917     C                   WHEN      TipVid = '³'
031500100917     C                   eval      TipVid = '3'
031600100917     C                   eval      *in28  = *off
031700080218      *
031800100917      * Dalla quarta videata torno alla terza/B
031900080218     C                   WHEN      TipVid = '4'
032000100917     C                   eval      TipVid = '³'
032100080218     C                   eval      *in28  = *off
032200080530      *
032300080530      * Dalla prima (e per ora unica) window torno alla quarta videata
032400080530     C                   WHEN      TipVid = 'W'
032500080530     C                   eval      TipVid = '4'
032600080530     C                   eval      *in28  = *off
032700130806      *
032800130806      * Dalla quinta videata torno alla quarta
032900130806     C                   WHEN      TipVid = '5'
033000130806     C                   eval      TipVid = '4'
033100130806     C                   eval      *in28  = *off
033200080218      *
033300130927      * Dalla sesta  videata torno alla quinta
033400130927     C                   WHEN      TipVid = '6'
033500130927     C                   eval      TipVid = '5'
033600130927     C                   eval      *in28  = *off
033700130927      *
033800080218     C                   ENDSL
033900011120      *
034000011120     C                   ENDSR
034100011114      *--------------------------------------------------------------------------------------------*
034200011120      * CarD01 - Carica video di dettaglio
034300011114      *--------------------------------------------------------------------------------------------*
034400011120     C     CarD01        BEGSR
034500011114      *
034600011126     C                   setoff                                       020305
034700020913     C                   move      *off          $prima
034800080218      *
034900080218      * IMMISSIONE
035000080218if  1C                   IF        I31Opz = 'I'
035100080218      * - Inizializzazione videate
035200080218     C                   clear                   Bs31D01
035300080218     C                   clear                   Bs31D02
035400080218     C                   clear                   Bs31D03
035500080218     C                   clear                   Bs31D04
035600130806     C                   clear                   Bs31D05
035700131022     C                   clear                   Bs31D06
035800080218      * - Se il pgm viene richiamato in immissione passando il
035900080218      *   codice profilo, lo porto a video
036000080218     C                   eval      V1CUte = I31Ute
036100080218      * - Impostazione default su data valitità
036200080218     C                   z-add     31122039      V1Cdsc
036300080218     C                   movel     '?'           V1CFil
036400080218     C                   movel     '?'           V1CFun
036500080218     C                   movel     '?'           V1CAut
036600080218     C                   movel     '?'           V1CPas
036700081013      * imposto a NO le autorizzazioni per modifica password utenti di rete
036800081013     c                   eval      v4cpswaut = 'NO'
036900081013     c                   eval      v4cpswmag = 'NO'
037000081013     c                   eval      v4cpswr   = 'NO'
037100131022     c                   move      'NO'          v6autautor
037200140430     c                   movel     'NO'          V3Cabres
037300131022     c                   movel     *all'N'       uteautds
037400131022     c                   movel     §AUTMTC       v6AUTMTC
037500131022     c                   movel     §AUTSDT       v6AUTSDT
037600131022     c                   movel     §AUTRET       v6AUTRET
037700131022     c                   movel     §AUTCON       v6AUTCON
037800131022     c                   movel     §AUTCRS       v6AUTCRS
037900131022     c                   movel     §AUTVTCC      v6AUTVTCC
038000131022     c                   movel     §AUTMTCC      v6AUTMTCC
038100131022     c                   movel     §AUTSDTC      v6AUTSDTC
038200131022     c                   movel     §AUTRETC      v6AUTRETC
038300131022     c                   movel     §AUTCRSC      v6AUTCRSC
038400131022     c                   movel     §AUTCONC      v6AUTCONC
038500131022     c                   movel     §RIEABI       v6RIEABI
038600131022     c                   movel     §AFFMTC       v6AFFMTC
038700131022     c                   movel     §AFFCON       v6AFFCON
038800131022     c                   movel     §AFFCRS       v6AFFCRS
038900131022     c                   movel     §AFFGVSTD     v6AFFGVSTD
039000131022     c                   movel     §AFFAGV       v6AFFAGV
039100131022     c                   movel     §AFFCPSTD     v6AFFCPSTD
039200131022     c                   movel     §AFFARRM      v6AFFARRM
039300131022     c                   movel     §AUTCPRRE     v6AUTCPRRE
039400131022     c                   movel     §AUTSTDCC     v6AUTSTDCC
039500131022     c                   movel     §AUTRFCIT     v6AUTRFCIT
039600131022     c                   movel     §AUTRFCOP     v6AUTRFCOP
039700131022     c                   movel     §AUTRFAFD     v6AUTRFAFD
039800131022     c                   movel     §AUTRFPRE     v6AUTRFPRE
039900130709
040000130709     c                   clear                   savfun
040100080218      *
040200080218x   1C                   ELSE
040300080218      *
040400080218      * MODIFICA / COPIA / VISUALIZZAZIONE
040500080218      *
040600080218      * - Modifica
040700080218     c                   eval      *in02  = (I31opz = '2')
040800080218      * - Copia
040900080218     c                   eval      *in03  = (I31opz = '3')
041000080218      * - Visualizzazione
041100080218     c                   eval      *in05  = (I31opz = '5')
041200080218      *
041300080218      * - Lettura file profili utente
041400011203     C                   movel     I31Ute        $Ute
041500011114     C                   movel     '1'           $Mod
041600080218     C                   exsr      ChnUte
041700080218      * - Riempimento della videata
041800080218if  2C                   if        NOT *in30
041900080218     C                   exsr      RieD01
042000101008      * - - Decodifica dati nelle videate
042100080218     C                   exsr      CtrD01
042200101008     C                   exsr      CtrD02
042300101008     C                   exsr      CtrD03
042400101008     C                   exsr      CtrD03B
042500101008     C                   exsr      CtrD04
042600130806     C                   exsr      CtrD05
042700131021     C                   exsr      CtrD06
042800160830      *
042900160830if  3c                   If        SDSprm > 2  and  Not *in28
043000160830      * -?Eseguo aggiornamento?
043100160830     c                   exsr      AggUte
043200160830      * -?Eseguo operazioni del dopo-aggiornamento?
043300160830if  4c                   if        NOT *in28
043400160830     c                   exsr      GesAGG
043500160830e   4c                   endif
043600160830      * -?Richiamo la gestione "tecnica" (Modulo_Base+AS/400) del profilo?
043700160830if  4c                   if        NOT *in28
043800160830     c                   exsr      CallTIBS32
043900160830e   4c                   endif
044000160830      * -?Esco dal *pgm?
044100160830if  4c                   if        NOT *in28
044200160830     c                   eval      $Fine = 'S'
044300160830     c                   leavesr
044400160830e   4c                   endif
044500160830e   3c                   EndIf
044600160830      *
044700101008     c                   eval      *in28 = *off
044800101008     c                   eval      $PostSearch = *off
044900080218e   2C                   endif
045000080218      *
045100080218e   1C                   ENDIF
045200011114      *
045300011114     C                   ENDSR
045400011121      *-----------------------------------------------------------
045500011114      * Controllo e decodifica prima videata
045600011121      *-----------------------------------------------------------
045700080218     C     CtrD01        BEGSR
045800011122      *
045900011114     C                   setoff                                       414243
046000011114     C                   setoff                                       444546
046100011127     C                   setoff                                       474849
046200150211     C                   setoff                                       503575
046300080218     C                   eval      $Riemetti = *off
046400130708
046500130708     c                   clear                   ricfun
046600130708     c                   clear                   ricaut
046700011127      *
046800011127      * Routine per gestione preparazione ricerche
046900080218     C                   exsr      GesSearch
047000020319      *
047100020319      * Controllo prima i campi chiave: Profilo, Filiale, Codice utente
047200011114      *
047300011130      * Profilo
047400011130      *
047500011210      * Salto il controllo se sono in copia e se è il primo giro di
047600011130      * controlli dopo il caricamneto della videata
047700011203 1a  C     I31Opz        ifne      '3'
047800040816     C     $Card01       orne      '2'
047900011130 2a  C     V1CUte        ifeq      *blanks
048000011120     C                   seton                                        2841
048100080218     C                   eval      V1Dmsg = Msg(1)
048200011114      *
048300011130 2e  C                   else
048400020909      *
048500020909      * controllo esistenza profilo su modulo base SEMPRE
048600080218     C                   eval      $NoPrf = *off
048700020909     C     V1CUte        setll     KpPrf01l                               30
048800020909 3a  C     *in30         ifeq      *off
048900020909     C                   eval      $NoPrf = *on
049000020909 3-  C                   endif
049100020909      *
049200011115      * Controllo chiave univoca se sono in immissione
049300011218      * e se non è il primo giro di controlli dopo il caricamento
049400011210      * delle videata.
049500011203 3a  C     I31Opz        ifeq      'I'
049600011203     C     I31Opz        oreq      '3'
049700040816     C     $Card01       andne     '2'
049800011115     C                   move      '1'           $Mod
049900011120     C                   eval      $Ute = V1CUte
050000011115      *
050100011115     C                   EXSR      ChnUte
050200011130 4a  C     *in30         ifeq      *off
050300011120     C                   seton                                        2841
050400080218     C                   eval      V1Dmsg = Msg(2)
050500011130 4-  C                   endif
050600020913 4a  C                   if        $NoPrf=*on and $prima = *Off
050700160901     c                                        and SDSprm < 3
050800020909     C                   seton                                        2841
050900080218     C                   eval      V1Dmsg = Msg(19)
051000080218     C                   eval      $prima = *on
051100020909 4-  C                   endif
051200020909      *
051300011130 3-  C                   endif
051400011116 2-  C                   endif
051500011116 1-  C                   endif
051600130705      * Modulo
051700130705     c                   clear                   TIBS02DS
051800130705     c                   eval      T02mod = 'C'
051900130705     c                   eval      T02sif = KnSif
052000130705     c                   eval      T02cod = 'MDP'
052100130705     c                   eval      T02ke1 = V1Cmopr
052200130705     c                   call      'TIBS02R'
052300130705     c                   parm                    kpjba
052400130705     c                   parm                    TIBS02DS
052500130705     c                   IF        T02err = *blanks
052600130705     c                   eval      V1Dmopr = T02uni
052700130705     c                   ELSE
052800130705     c                   eval      V1Dmopr = *all'?'
052900130705     c                   ENDIF
053000130705      * modulo STANDARD può essere inserito o modifcato solo da alcuni utenti
053100130705     c                   eval      *in69 = *off
053200130705     c                   IF        V1Cmopr <> §UT2mopr and
053300130705     c                             (V1Cmopr = 'S' or §UT2mopr = 'S') and
053400130705     c                             knmus <> 'EDPAC' and
053500130705     c                             knmus <> 'EDPND' and
053600130705     c                             knmus <> 'EDPMB' and
053700130705     c                             knmus <> 'EDPGA' and
053800130705     c                             knmus <> 'EDPLB'
053900130705     c                   eval      *in28 = *on
054000130705     c                   eval      *in69 = *on
054100130705     c                   eval      V1Dmsg = 'Modulo profilo non modificabile'
054200130705     c                   leavesr
054300130705     c                   ENDIF
054400130705
054500011114      * Filiale
054600011122     c                   clear                   V1DFil
054700011122 1a  C     *In60         ifeq      *on
054800011122     C     *In28         andeq     *off
054900011122      * Ricerche
055000011122     C                   MOVEL     *BLANKS       V1DFil
055100011122     C                   MOVEL     *BLANKS       §COD01
055200011122     C                   MOVE      *BLANKS       §DES1
055300011122     C                   MOVE      *BLANKS       §PRA
055400011122     C                   CALL      'TNSD24R'
055500011122     C                   PARM                    §COD01            3
055600011122     C                   PARM                    §TIP              1
055700011122     C                   PARM                    §DES1            25
055800011122     C                   PARM                    §PRA              3
055900011122      *
056000011122     C                   movel     §COD01        V1CFil
056100011122     C                   movel     §DES1         V1DFil
056200011127     C                   move      *off          *in60
056300011126 1-  C                   endif
056400011126      *
056500011130      * Non devo eseguire i controlli se ho richesto la ricerca
056600011127 1a  c     *in60         ifeq      *off
056700011127      *
056800011127 2a  c     V1CFil        ifne      *zeros
056900011127     c     V1CFil        andne     *blanks
057000011127      *
057100011120     c                   move      V1CFil        kFil              3 0
057200011120     c     KFil          chain     AzOrg01l                           31
057300011127 3a  c     *in31         ifeq      *off
057400011120     c                   movel     OrgDes        V1DFil
057500011127 3e  C                   else
057600080218     C  n28              eval      V1Dmsg = msg(7)
057700011120     C  n28              seton                                        2842
057800011127 3-  C                   endif
057900011127      *
058000020315 2e  C                   else
058100080218     C  n28              eval      V1Dmsg = msg(13)
058200011120     C  n28              seton                                        2842
058300011127 2-  C                   endif
058400130705      * se Modulo Standard filiale 789
058500130705     c                   IF        V1Cmopr = 'S' and V1Cfil <> '789'
058600130705     c                             and %scan('?':V1Cfil) = 0
058700130705     c                   eval      *in28 = *on
058800130705     c                   eval      *in42 = *on
058900130705     c                   eval      V1Dmsg = 'Per profilo Standard immettere -
059000130705     c                             filiale 789'
059100130705     c                   ENDIF
059200011126 1-  C                   endif
059300011121      *
059400011121      * Cod. utente
059500011210      * In copia, o in immissione, dopo la prima emissione,
059600011130      * propongo il progressivo cod. utente per la filiale.
059700020102      * Solo se il codice filiale è a zero
059800011203 1a  C     I31Opz        ifeq      '3'
059900011210     C     I31Opz        oreq      'I'
060000020102      *
060100011210 2a  C     $CarD01       ifne      '2'
060200020329     C     V1Ccdu        andeq     *zeros
060300011203     C                   eval      $Ute = UteUte
060400011203      *
060500020102      *
060600020329     C                   move      V1Ccdu        Wcdu              3 0
060700011210 3a  C     V1Cfil        ifne      *blanks
060800011210     C     *in60         andne     *on
060900021003     C                   move      V1CFil        $FilV
061000011210      *
061100021003     C     $FilV         setll     AzUte02l
061200021003     C     $FilV         reade     AzUte02l                               30
061300011203      *
061400021003     C                   if        *in30 = *on
061500021003      * Se non ho mai inserito utenti per questa filiale propongo 1
061600021003     C                   z-add     1             V1Ccdu
061700021003     C                   else
061800021003      * altrimenti propongo il primo numero libero
061900021003     C                   setoff                                       35
062000021003     C     *in35         doweq     *off
062100021003     C     Utecdu        add       1             V1Ccdu
062200021003     C     K02UteV       chain     Azute02l                           35
062300021003     c                   enddo
062400021003     C                   endif
062500011203      * mi riposiziono
062600011203     C     $Ute          chain     AzUte01l                           30
062700011203      *
062800020329 4a  C     V1Ccdu        ifne      Wcdu
062900080218     C                   eval      $Riemetti = *on
063000020102 4-  C                   endif
063100020102 3-  C                   endif
063200011210 2-  C                   endif
063300011210 1-  C                   endif
063400011210      *
063500011210 1a  C     $CarD01       ifne      '2'
063600020329 2a  C     V1Ccdu        ifeq      *zeros
063700080218     C  n28              eval      V1Dmsg = Msg(8)
063800011121     C  n28              seton                                        2843
063900011203 2-  C                   endif
064000011203 1-  C                   endif
064100011121      *
064200011115      * Controllo chiave univoca Filiale + codice
064300011115      * se in immissione o copia
064400011127 1a  C     *in28         ifeq      *off
064500011203 2a  C     I31Opz        ifeq      'I'
064600011203     C     I31Opz        oreq      '3'
064700011210     C     $Card01       andne     '2'
064800011115     C                   move      '2'           $Mod
064900020329     C                   z-add     V1Ccdu        $cdu
065000011120     C                   move      V1CFil        $Fil
065100011115      *
065200011115     C                   EXSR      ChnUte
065300011127 3a  C     *in30         ifeq      *off
065400080218     C                   eval      V1Dmsg = Msg(3)
065500011122     C                   seton                                        2844
065600011127 3-  C                   endif
065700011127 2-  C                   endif
065800011127 1-  C                   endif
065900130705      * se Utente standard e immissione la data scadenza la imposto a oggi -1 gg.
066000130705     c                   IF        V1Cmopr = 'S' and I31opz = 'I'
066100130705     c                   eval      wData_ISO = %date()
066200130705     c                   eval      wData_EUR = wData_ISO - %days(1)
066300130705     c                   eval      V1Cdsc = %dec(wData_EUR)
066400130705     c                   ENDIF
066500020319      *
066600020319      * Se ho trovato errori fra i campi chiave non vado avanti nei
066700020319      * controlli a meno che non sia in modifica
066800020319 1a  C     *in28         ifeq      *off
066900020319     C     I31Opz        oreq      '2'
067000020315      *
067100020315      * Pgm iniziale/libreria
067200020319 2a  C     V1CPgm        ifeq      *blanks
067300020315     C     V1CLib        andne     *blanks
067400020315     C     V1CPgm        orne      *blanks
067500020315     C     V1CLib        andeq     *blanks
067600080218     C                   eval      V1Dmsg = Msg(4)
067700020315     C                   seton                                        2848
067800020319 2-  C                   endif
067900020315      * Profilo Posta AS
068000020315     C                   clear                   V1DPas
068100020319 2a  C     *In63         ifeq      *on
068200020315     C**   *In28         andeq     *off
068300020315     C                   clear                   KPJBU
068400020315     C                   clear                   TbcMus
068500020315     C                   move      '1'           TbcFun
068600020315     C                   movel     TbcMus        KPJBU
068700020315     C                   CALL      'TNTBCMUR'
068800020315     C                   PARM                    KPJBA
068900020315     C                   movel     KPJBU         TbcMus
069000020319 3a  C     TbcEsito      ifeq      *off
069100020315     C                   movel     TbcKe1        V1CPas
069200020315     C                   movel     TbcUni        V1DPas
069300020319 3-  C                   endif
069400020315     C                   setoff                                       63
069500020319 2-  C                   endif
069600020315      *
069700020315      * Controllo esistenza utente posta
069800020319 2a  C     *in63         ifeq      *off
069900020315     C     V1CPas        andne     *blanks
070000020315      *
070100020315     C                   clear                   V1DPas
070200020319     C                   testn                   V1CFil               21
070300020319 3a  C     *in21         ifeq      *on
070400020315     C                   move      V1CFil        PrmFil
070500020319 3e  C                   else
070600020315     C                   move      *zeros        PrmFil
070700020319 3-  C                   endif
070800020315      *
070900020315     C                   movel     V1CEma        W1CEma           50
071000020315      *
071100020315     C                   CALL      'CMZM08R'
071200020315     C                   PARM      V1CPas        PrmUti           10
071300020315     C                   PARM                    PrmFil            3 0
071400020315     C                   PARM      *blanks       PrmEsi            1
071500020315     C                   PARM      *blanks       PrmUni           20
071600020315      *
071700020315     C                   movel     W1CEma        V1CEma           50
071800020315      *
071900020319 3a  C                   SELECT
072000020315     C     PrmEsi        wheneq    '1'
072100020315      * Utente errato
072200080218     C                   eval      V1Dmsg = Msg(17)
072300020315     C                   seton                                        2849
072400020315     C     PrmEsi        wheneq    '2'
072500020315      * Incongruenza utente/filiale
072600080218     C                   eval      V1Dmsg = Msg(5)
072700020315     C                   seton                                        2849
072800020315     C                   OTHER
072900020315     C                   eval      V1DPas = PrmUni
073000020319 3-  C                   ENDSL
073100020315      *
073200020319 2-  C                   endif
073300011114      *
073400011114      * Autorità
073500011120     C                   clear                   V1DAut
073600020319 2a  C     *In62         ifeq      *on
073700080218     C                   clear                   Tibs02Ds
073800080218     C                   movel     'R'           T02Mod
073900011122     C                   movel     KnSif         T02Sif
074000080218     C                   movel     'LAT'         T02Cod
074100011122     C                   CALL      'TIBS02R'
074200011122     C                   PARM                    kpjba
074300011122     C                   PARM                    Tibs02Ds
074400011122     C                   movel     T02ke1        V1CAut
074500011122     C                   movel     T02uni        V1DAut
074600011127     C                   setoff                                       62
074700020319 2-  C                   endif
074800011126      *
074900011127      * Non devo segnalare nulla se ho richiesto i controlli
075000020319 2a  C     *in62         ifeq      *off
075100020319 3a  C     V1CAut        ifeq      *blanks
075200080218     C                   eval      V1Dmsg = Msg(11)
075300020315     C                   seton                                        2847
075400011114      *
075500020402      * Se valorizzato controllo ch ese utente di sede
075600020402      * l'autorizzazione si apermessa e se tutto OK decodifico.
075700020319 3e  c                   else
075800080218     C                   reset                   tibs02ds
075900011120     C                   movel     V1CAut        t02ke1
076000011114     C                   call      'TIBS02R'
076100011114     C                   parm                    KPJBA
076200011114     C                   parm                    TIBS02DS
076300011203      *
076400020319 4a  C                   if        t02err =  *blanks
076500020402     C                   movel     t02uni        dLAT
076600020402     C     V1CFil        ifeq      '046'
076700020402     C     §LatUus       andne     'S'
076800080218     C                   eval      V1Dmsg = Msg(18)
076900020402     C                   seton                                        2847
077000020402     C                   else
077100020402     C                   movel     §LatDes       V1DAut
077200020402     C                   endif
077300020319 4e  C                   else
077400080218     C                   eval      V1Dmsg = Msg(16)
077500020315     C                   seton                                        2847
077600020319 4-  C                   endif
077700020319 3-  C                   endif
077800020319 2-  C                   endif
077900130708      /free
078000130708        // confronto con lo STD
078100130806         IF  V1Caut <> V1Saut and V1Cmopr <> 'S' and wOKuteSTD;
078200130708           ricaut = rimm;
078300130708         ENDIF;
078400130708      /end-free
078500020315      *
078600020315      * Funzione
078700020315     C                   clear                   V1DFun
078800020319 2a  C     *In61         ifeq      *on
078900080218     C                   clear                   Tibs02Ds
079000020315     C                   move      'R'           T02Mod
079100020315     C                   movel     KnSif         T02Sif
079200080218     C                   move      'FUN'         T02Cod
079300020315     C                   CALL      'TIBS02R'
079400020315     C                   PARM                    kpjba
079500020315     C                   PARM                    Tibs02Ds
079600020315     C                   movel     T02ke1        V1CFun
079700020315     C                   movel     T02uni        V1DFun
079800020315     C                   setoff                                       61
079900020319 2-  C                   endif
080000020315      *
080100020315      * Non devo segnalare nulla se ho richiesto i controlli
080200020319 2a  C     *in61         ifeq      *off
080300020319 3a  C     V1CFun        ifeq      *blanks
080400020315      *
080500080218     C                   eval      V1Dmsg = Msg(10)
080600020315     C                   seton                                        2846
080700020315      *
080800020315      * Se esiste decodifico
080900020319 3e  c                   else
081000020315     C                   clear                   tibs02ds
081100020315     C                   movel     'C'           t02mod
081200020315     C                   movel     *blanks       t02sif
081300020315     C                   movel     'FUN'         t02cod
081400020315     C                   movel     V1CFun        t02ke1
081500020315     C                   call      'TIBS02R'
081600020315     C                   parm                    KPJBA
081700020315     C                   parm                    TIBS02DS
081800020319 4a  C                   if        t02err =  *blanks
081900020402     C                   movel     t02uni        V1DFun
082000020319 4e  C                   else
082100080218     C                   eval      V1Dmsg = Msg(15)
082200020315     C                   seton                                        2846
082300020319 4-  C                   endif
082400020319 3-  C                   endif
082500020319 2-  C                   endif
082600130708      /free
082700130709        // se variata la funzione da COCOM a altra funzione o
082800130709        // da altra funzione a COCOM devo ricaricare la videata con tutte
082900130709        // le abilitazioni del nuovo profilo STD
083000140710        // stessa cosa per funzioni REA e RED
083100130709          IF  I31opz <> 'I' and
083200130709              ((savfun = 'COCOM' and V1Cfun <> 'COCOM') or
083300130709              (savfun <> 'COCOM' and V1Cfun = 'COCOM'));
083400130709            wrie01 = *on;
083500130709            savfun = V1Cfun;
083600130709            $carD01 = *on;
083700130709            $riemetti = *on;
083800130709            leavesr;
083900130709          ENDIF;
084000140710          IF  I31opz <> 'I' and
084100140710              ((savfun = 'RED' and V1Cfun <> 'RED') or
084200140710              (savfun <> 'RED' and V1Cfun = 'RED'));
084300140710            wrie01 = *on;
084400140710            savfun = V1Cfun;
084500140710            $carD01 = *on;
084600140710            $riemetti = *on;
084700140710            leavesr;
084800140710          ENDIF;
084900140710          IF  I31opz <> 'I' and
085000140710              ((savfun = 'RAD' and V1Cfun <> 'REA') or
085100140710              (savfun <> 'REA' and V1Cfun = 'REA'));
085200140710            wrie01 = *on;
085300140710            savfun = V1Cfun;
085400140710            $carD01 = *on;
085500140710            $riemetti = *on;
085600140710            leavesr;
085700140710          ENDIF;
085800130708        // confronto con lo STD
085900130806         IF  V1Cfun <> V1Sfun and V1Cmopr <> 'S' and wOKuteSTD;
086000130708           ricfun = rimm;
086100130708         ENDIF;
086200130708      /end-free
086300020315      *
086400020315      * Validità
086500020329 2a  C     V1Cdsc        ifeq      *zeros
086600080218     C                   eval      V1Dmsg = Msg(9)
086700020315     C                   seton                                        2845
086800020319 2e  C                   else
086900020315     C                   reset                   WLBDA8
087000020315     C                   Z-ADD     *zeros        G08INV
087100020329     C                   Z-ADD     V1Cdsc        G08DAT
087200020315     C                   move      '0'           G08ERR
087300020315     C                   CALL      'XSRDA8'
087400020315     C                   PARM                    WLBDA8
087500020315      *
087600020319 3a  C     G08err        ifeq      '1'
087700080218     C                   eval      V1Dmsg = Msg(14)
087800020315     C                   seton                                        2845
087900040816     C                   else
088000040816     C                   z-add     G08DAT        V1Cdsc
088100020319 3-  C                   endif
088200020319 2-  C                   endif
088300020319 1-  C                   endif
088400011127      *
088500020315      * Se sono in visualizzazione spengo il 28
088600080218     c                   if        I31opz = '5'
088700080218     c                   eval      *in28  = *off
088800080218     c                   endif
088900020315      *
089000020315      * Se sono in modifica ed ho errori sui campi chiave
089100020315      * devo poter permettere la conferma.
089200080218 1a  C                   if             I31opz = '2'
089300080218     C                             and  *in28  = *on
089400080218 2a  C                   if             NOT *in45
089500080218     C                             and  NOT *in46
089600080218     C                             and  NOT *in47
089700080218     C                             and  NOT *in48
089800080218     C                             and  NOT *in49
089900080218     C                   eval      *in28  = *off
090000020319 2-  C                   endif
090100020909      * se sono modifica e il profilo non esiste nel modulo base
090200020909      * segnalo per prima cosa l'anomalia, emetto il messaggio
090300020909      * una sola volta
090400020909 2   C                   if        $NoPrf=*on and $prima =*off
090500080218     C                   eval      $prima = *on
090600080218     C                   eval      *in28  = *on
090700080218     C                   eval      V1Dmsg = Msg(19)
090800020909 2-  C                   endif
090900020319 1-  C                   endif
091000020315      *
091100011127     C                   ENDSR
091200011127      *-----------------------------------------------------------
091300011127      * Gestione per le richieste di ricerca
091400011127      *-----------------------------------------------------------
091500011127     C     GesSearch     BEGSR
091600011127      *
091700011127     C                   setoff                                       606162
091800011127     C                   setoff                                       63
091900040922     c                   setoff                                       646566
092000140403     c                   setoff                                       6770
092100011127      * Gestisco un indicatore che mi segnali l'avvenuta richiesta
092200011127      * ricerche, per riemette il formato video, almeno una volta,
092300011127      * dopo le ricerche.
092400080218     C                   eval      $PostSearch = *off
092500080218      *
092600080218sel 1c                   select
092700080218      *
092800040922      * prima videata
092900080218w   1c                   when      TipVid = '1'
093000011127      *
093100011127      * testo la richiesta ricerche
093200011127     C     '?'           scan      V1CFil                                 60
093300011127     C     '?'           scan      V1CFun                                 61
093400011127     C     '?'           scan      V1CAut                                 62
093500011127     C     '?'           scan      V1CPas                                 63
093600011127      *
093700011127     C     *in60         ifeq      *on
093800011127     C     *in61         oreq      *on
093900011127     C     *in62         oreq      *on
094000011127     C     *in63         oreq      *on
094100080218     C                   eval      $PostSearch = *on
094200011127     C                   endif
094300080218      *
094400040922      * seconda videata
094500080218w   1c                   when      TipVid = '2'
094600040922      *
094700040922      * testo la richiesta ricerche
094800040922     C     '?'           scan      v2cibs                                 64
094900040922     C     '?'           scan      v2caub                                 65
095000040922     C     '?'           scan      v2ciun                                 66
095100140403     C     '?'           scan      v3cabres                               70
095200040922      *
095300040922     C     *in64         ifeq      *on
095400040922     C     *in65         oreq      *on
095500040922     C     *in66         oreq      *on
095600140403     C     *in70         oreq      *on
095700080218     C                   eval      $PostSearch = *on
095800040922     C                   endif
095900101008      *
096000101008      * terza videata
096100101008w   1c                   when      TipVid = '3'
096200101008      *
096300101008      * testo la richiesta ricerche
096400101008     C     '?'           scan      v2cist                                 67
096500101008      *
096600101008     C     *in67         ifeq      *on
096700101008     C                   eval      $PostSearch = *on
096800101008     C                   endif
096900080218      *
097000080218e   1c                   endsl
097100011127      *
097200011127     C                   ENDSR
097300011121      *-----------------------------------------------------------
097400011121      * Riempimento videata  D01
097500011121      *-----------------------------------------------------------
097600011120     C     RieD01        BEGSR
097700011210      *
097800011114      * Profilo
097900011120     C                   eval      V1CUte  = UteUte
098000130705      * Se copia aggiungo al profilo anche la filiale se ricevuta
098100130709     c                   IF        I31opz = '3' and I31fil <> *blanks and
098200130709     c                             I31fil <> *zeros
098300130705     c                   eval      V1Cute = %subst(UTEute:1:3) + %trim(I31Fil)
098400130705     c                   ENDIF
098500011203      * Codice
098600011210 1a  C     I31Opz        ifne      '3'
098700020329     C                   z-add     Utecdu        V1Ccdu
098800131002      * Recupero se profilo pesente in Proj e Abilitazioni AUT
098900131002      * ma solo se non è COPIA
099000131002      * Presente in Proj
099100131002     c                   eval      V1Dproj = GetProfiloUtenteProj(V1Cute)
099200011210 1e  C                   else
099300020329     C                   z-add     0             V1Ccdu
099400011210 1-  C                   endif
099500011115      * Data
099600130710      * se copia da profilo STD imposto 31/12/2039
099700130710     c                   IF        I31opz = '3' and I31fil <> *blanks and
099800130710     c                             I31fil <> *zeros
099900130710     c                   eval      UTEdsc = 20391231
100000130710     c                   ENDIF
100100011115     C                   reset                   WLBDA8
100200020329     C                   z-add     Utedsc        G08INV
100300011115     C                   z-add     *zeros        G08DAT
100400011115     C                   Move      '3'           G08ERR
100500011115     C                   CALL      'XSRDA8'
100600011115     C                   PARM                    WLBDA8
100700020329     C                   z-add     G08DAT        V1Cdsc
100800011115      * Filiale
100900011120     C                   move      UteFil        V1CFil
101000130705      * Se copia imposto la filiale che ho ricevuto
101100130705     c                   IF        I31opz = '3' and I31fil <> *blanks
101200130709     c                             and I31fil <> *zeros
101300130705     c                   move      I31fil        V1CFil
101400130705     c                   ENDIF
101500011115      * Funzione
101600130709      * se ho riemesso la videata per il cambio della funzione devo impostare quella
101700130709      * messa dall'utente
101800130709     c                   IF        wRIE01
101900130709     c                   eval      V1Cfun = savfun
102000130709     c                   eval      wRIE01 = *off
102100130709     c                   ELSE
102200011120     C                   move      UteFun        V1CFun
102300130709     c                   ENDIF
102400130717      * Se copia aggiungo al profilo anche la funzione se ricevuta
102500130717     c                   IF        I31opz = '3' and I31fun <> *blanks
102600130717     c                   eval      V1Cfun = I31fun
102700130717     c                   ENDIF
102800130709     c                   eval      savfun = V1Cfun
102900011115      * Pgm iniziale/libreria
103000011120     C                   movel     UtePgm        V1CPgm
103100011120     C                   movel     UteLib        V1CLib
103200011115      * Autorità
103300011120     C                   movel     UteAut        V1CAut
103400011115      * Profilo posta AS
103500011120     C                   movel     UtePas        V1CPas
103600011115      * E-mail
103700011221     C                   movel     UteEma        V1CEmA
103800011120     C                   move      UteEma        V1CEm2
103900011115      * Azienda
104000011120     C                   movel     UteKdt        V1CKdc
104100011115      * Legame con persona fisica
104200011120     C                   movel     UteKcd        V1CKcd
104300100115      * Flags
104400100115      * (NON visualizzati in "copia")
104500100114     c                   clear                   dUte02
104600100114     c                   movel     UteFa1        dUte02
104700140731     c                   if        I31opz = '3'
104800140731     c                   movel     *blanks       §UT2mopr
104900160830     c                   if        SDSprm > 2
105000160830     c                   eval      §UT2moPr = TIBS31ds2.I31moPr
105100160830     c                   eval      V1CmoPr  = TIBS31ds2.I31moPr
105200160830     c                   endif
105300140731     c                   endif
105400100115     c                   if        I31opz <> '3'
105500100114     c                   movel     §UT2mopr      V1Cmopr
105600100115     c                   endif
105700130705      * Decodifico Modulo
105800130705     c                   clear                   TIBS02DS
105900130705     c                   eval      T02mod = 'C'
106000130705     c                   eval      T02sif = KnSif
106100130705     c                   eval      T02cod = 'MDP'
106200130705     c                   eval      T02ke1 = V1Cmopr
106300130705     c                   call      'TIBS02R'
106400130705     c                   parm                    kpjba
106500130705     c                   parm                    TIBS02DS
106600130705     c                   IF        T02err = *blanks
106700130705     c                   eval      V1Dmopr = T02uni
106800130705     c                   ELSE
106900130705     c                   eval      V1Dmopr = *all'?'
107000130705     c                   ENDIF
107100100114      *
107200011121      * Abilitazioni
107300020329     C                   clear                   DUte01
107400020329     C                   movel     UteFaf        DUte01
107500040816     C                   movel     §UteMbs       V2CAuB
107600040816     C                   movel     §UteIbs       V2CIbs
107700040816     C                   movel     §UteIun       V2CIun
107800040816     C                   movel     §UteCUn       V2CCUn
107900040816     C                   movel     §UteIst       V2CIst
108000080218     C                   movel     §UteMaz       V4CMaz
108100080218     c                   movel     §UteIas       V2CIas
108200040816     C                   movel     §UteDST       V2CDST
108300040816     C                   movel     §UteDSS       V2CDSS
108400040816     C                   movel     §UtePRO       V2CPRO
108500050413     C                   movel     §UteCAP       V2CCAP
108600080218     c                   movel     §UteGtc       V3Cgtc
108700130124     c                   movel     §UTEmodtar    V3modtar
108800080218     c                   movel     §UteCar       V3Ccar
108900080218     c                   movel     §UteCtc       V3Cctc
109000090423     c                   movel     §Utedupcli    V3Cdupcli
109100080218     c                   movel     §UtePot       V3Cpot
109200090514     c                   movel     §UtePotst     V3Cpotst
109300080218     c                   movel     §UteCli       V3Ccli
109400091006     c                   movel     §UteMntSp     V3CmntSp
109500080218     c                   movel     §UteEca       V3Ceca
109600080218     c                   movel     §UteMbf       V3Cmbf
109700080218     c                   movel     §UteMtf       V3Cmtf
109800080218     c                   movel     §UteMfcp      V3Cmfcp
109900080218     c                   movel     §UteCep       V3Ccep
110000080218     c                   movel     §UteFgia      V3Cfgia
110100140403     c                   movel     §Uteabres     V3Cabres
110200080218     c                   movel     §UteFlna      V3Cflna
110300080218     c                   movel     §UteGORG      V4Cgorg
110400080530     c                   movel     §UtePWD       V4Cpwd
110500080526     c                   movel     §UteMPS       V4Cmps
110600081013     c                   movel     §UtePswaut    V4Cpswaut
110700081013     c                   movel     §UtePswmag    V4Cpswmag
110800081013     c                   movel     §UtePswr      V4Cpswr
110900150212     c                   movel     §Uteipt       V4Cipt
111000150618     c                   movel     §UteFUF       V4Cfuf
111100150210     c                   movel     §Utecgb       V4Ccgb
111200150210     c                   movel     §Utemgb       V4Cmgb
111300160808     c                   movel     §Utecanc      V4Ccanc
111400100917     c                   movel     §UtePotMsf    V3CPotMsf
111500100917     c                   movel     §UteChgCmm    V3CChgCmm
111600160905     c                   movel     §UteInvPwd    V4CInvPwd
111700110429     c                   movel     §UteStp       V3Cstp
111800120613     c                   movel     §UteCLPfft    V3CLPfft
111900160905     c                   movel     §UteKscLog    V3Cksclog
112000120907     C                   movel     §UteGDCksc    V4GDCksc
112100130618     c                   movel     §UTEorm       V3Corm
112200160422     c                   movel     §UTEistmo     V3Cstmo
112300130806     c                   movel     §UTEvrac      V5Cvrac
112400150317     c                   movel     §UTErac       V5Crac
112500130806     c                   movel     §UTEmresca    V5Cmresca
112600150810     c                   movel     §UTEaddt      V5Cvadd
112700150810     c                   movel     §UTEadd       V5Cadd
112800130927      * verifica se inserito livello di autorità per gestione abilitazioni aut
112900131022     c                   if        §uteautor <> *blank and I31opz <> '3'
113000130927     c                   movel     §UTEautor     V6autautor
113100131022     c                   movel     §UTEdsaut     uteautds
113200130927     c                   else
113300130927     c                   movel     'NO'          v6autautor
113400131022     c                   movel     *all'N'       uteautds
113500130927     c                   endif
113600130927     c                   movel     §AUTMTC       v6AUTMTC
113700130927     c                   movel     §AUTSDT       v6AUTSDT
113800130927     c                   movel     §AUTRET       v6AUTRET
113900130927     c                   movel     §AUTCON       v6AUTCON
114000130927     c                   movel     §AUTCRS       v6AUTCRS
114100130927     c                   movel     §AUTVTCC      v6AUTVTCC
114200130927     c                   movel     §AUTMTCC      v6AUTMTCC
114300130927     c                   movel     §AUTSDTC      v6AUTSDTC
114400130927     c                   movel     §AUTRETC      v6AUTRETC
114500130927     c                   movel     §AUTCRSC      v6AUTCRSC
114600130927     c                   movel     §AUTCONC      v6AUTCONC
114700130927     c                   movel     §RIEABI       v6RIEABI
114800130927     c                   movel     §AFFMTC       v6AFFMTC
114900130927     c                   movel     §AFFCON       v6AFFCON
115000130927     c                   movel     §AFFCRS       v6AFFCRS
115100130927     c                   movel     §AFFGVSTD     v6AFFGVSTD
115200130927     c                   movel     §AFFAGV       v6AFFAGV
115300130927     c                   movel     §AFFCPSTD     v6AFFCPSTD
115400130927     c                   movel     §AFFARRM      v6AFFARRM
115500130927     c                   movel     §AUTCPRRE     v6AUTCPRRE
115600130927     c                   movel     §AUTSTDCC     v6AUTSTDCC
115700130927     c                   movel     §AUTRFCIT     v6AUTRFCIT
115800130927     c                   movel     §AUTRFCOP     v6AUTRFCOP
115900130927     c                   movel     §AUTRFAFD     v6AUTRFAFD
116000130927     c                   movel     §AUTRFPRE     v6AUTRFPRE
116100080218      *
116200080218     c                   clear                   V3Dgtc
116300080218     c                   clear                   V3Dctc
116400080218     c                   clear                   V3Dpot
116500080218     c                   clear                   V3Dcli
116600080218     c                   clear                   V3Deca
116700080218     c                   clear                   V3Dmbf
116800081014     c                   clear                   V4Dpswaut
116900081014     c                   clear                   V4Dpswmag
117000081014     c                   clear                   V4Dpswr
117100130618     c                   clear                   V3Dorm
117200150317     c                   clear                   V5Drac
117300080218      *
117400041027      * se inserite decodifico già le abilitazioni
117500041027 2a  C                   if        v3cgtc <> *blanks
117600080218     C                   reset                   tibs02ds
117700041027     C                   movel(p)  v3cgtc        t02ke1
117800041027     C                   call      'TIBS02R'
117900041027     C                   parm                    KPJBA
118000041027     C                   parm                    TIBS02DS
118100041027     C                   movel     t02uni        dLAT
118200041027     C                   movel     §LatDes       v3dgtc
118300041027 4-  C                   endif
118400041027 2a  C                   if        v3cctc <> *blanks
118500080218     C                   reset                   tibs02ds
118600041027     C                   movel     v3cctc        t02ke1
118700041027     C                   call      'TIBS02R'
118800041027     C                   parm                    KPJBA
118900041027     C                   parm                    TIBS02DS
119000041207     C                   movel     t02uni        dLAT
119100041027     C                   movel     §LatDes       V3dctc
119200041027 4-  C                   endif
119300080218      *
119400041207 2a  C                   if        v3cpot <> *blanks
119500080218     C                   reset                   tibs02ds
119600041207     C                   movel     v3cpot        t02ke1
119700041207     C                   call      'TIBS02R'
119800041207     C                   parm                    KPJBA
119900041207     C                   parm                    TIBS02DS
120000041207     C                   movel     t02uni        dLAT
120100041207     C                   movel     §LatDes       V3dpot
120200041207 4-  C                   endif
120300041207 2a  C                   if        v3ccli <> *blanks
120400080218     C                   reset                   tibs02ds
120500041207     C                   movel     v3ccli        t02ke1
120600041207     C                   call      'TIBS02R'
120700041207     C                   parm                    KPJBA
120800041207     C                   parm                    TIBS02DS
120900041207     C                   movel     t02uni        dLAT
121000041207     C                   movel     §LatDes       V3dcli
121100041207 4-  C                   endif
121200050609 2a  c                   if        v3ceca <> *blanks
121300080218     C                   reset                   tibs02ds
121400050609     C                   movel     v3ceca        t02ke1
121500050609     C                   call      'TIBS02R'
121600050609     C                   parm                    KPJBA
121700050609     C                   parm                    TIBS02DS
121800050609     C                   movel     t02uni        dLAT
121900050609     C                   movel     §LatDes       V3deca
122000050609 4-  C                   endif
122100080218      *
122200060126 2a  c                   if        v3cmbf <> *blanks
122300080218     C                   reset                   tibs02ds
122400060126     C                   movel     v3cmbf        t02ke1
122500060126     C                   call      'TIBS02R'
122600060126     C                   parm                    KPJBA
122700060126     C                   parm                    TIBS02DS
122800060126     C                   movel     t02uni        dLAT
122900060126     C                   movel     §LatDes       V3dmbf
123000060126 4-  C                   endif
123100130618
123200130618      * ORM
123300130618 2a  c                   IF        V3Corm <> *blanks
123400130618     c                   reset                   TIBS02DS
123500130618     c                   eval      T02ke1 = V3Corm
123600130618     c                   call      'TIBS02R'
123700130618     c                   parm                    KPJBA
123800130618     c                   parm                    TIBS02DS
123900130618     c                   eval      dLAT = T02uni
124000130618     c                   eval      V3Dorm = §LATdes
124100130618 4-  c                   ENDIF
124200160422      * statistica movimento
124300160422 2a  c                   IF        V3Cstmo <> *blanks
124400160422     c                   reset                   TIBS02DS
124500160422     c                   eval      T02ke1 = V3Cstmo
124600160422     c                   call      'TIBS02R'
124700160422     c                   parm                    KPJBA
124800160422     c                   parm                    TIBS02DS
124900160422     c                   eval      dLAT = T02uni
125000160422     c                   eval      V3Dstmo = §LATdes
125100160422 4-  c                   ENDIF
125200011121      *
125300081013      * decodifico abilitazioni modifica password utenti di rete
125400081013     c                   if        v4cpswaut <> *blanks
125500081013     c                   reset                   tibs02ds
125600081013     c                   movel     v4cpswaut     t02ke1
125700081013     c                   call      'TIBS02R'
125800081013     c                   parm                    kpjba
125900081013     c                   parm                    tibs02ds
126000081013     c                   eval      dlat = t02uni
126100081013     c                   eval      v4dpswaut = §latdes
126200081013     c                   endif
126300081013
126400081013     c                   if        v4cpswmag <> *blanks
126500081013     c                   reset                   tibs02ds
126600081013     c                   movel     v4cpswmag     t02ke1
126700081013     c                   call      'TIBS02R'
126800081013     c                   parm                    kpjba
126900081013     c                   parm                    tibs02ds
127000081013     c                   eval      dlat = t02uni
127100081013     c                   eval      v4dpswmag = §latdes
127200081013     c                   endif
127300081013
127400081013     c                   if        v4cpswr   <> *blanks
127500081013     c                   reset                   tibs02ds
127600081013     c                   movel     v4cpswr       t02ke1
127700081013     c                   call      'TIBS02R'
127800081013     c                   parm                    kpjba
127900081013     c                   parm                    tibs02ds
128000081013     c                   eval      dlat = t02uni
128100081013     c                   eval      v4dpswr   = §latdes
128200081013     c                   endif
128300150210      * gestione bancali al cliente
128400150210 2a  C                   if        v4ccgb <> *blanks
128500150210     C                   reset                   tibs02ds
128600150210     C                   movel(p)  v4ccgb        t02ke1
128700150210     C                   call      'TIBS02R'
128800150210     C                   parm                    KPJBA
128900150210     C                   parm                    TIBS02DS
129000150210     C                   movel     t02uni        dLAT
129100150210     C                   movel     §LatDes       v4dcgb
129200150210 4-  C                   endif
129300150210 2a  C                   if        v4cmgb <> *blanks
129400150210     C                   reset                   tibs02ds
129500150210     C                   movel(p)  v4cmgb        t02ke1
129600150210     C                   call      'TIBS02R'
129700150210     C                   parm                    KPJBA
129800150210     C                   parm                    TIBS02DS
129900150210     C                   movel     t02uni        dLAT
130000150210     C                   movel     §LatDes       v4dmgb
130100150210 4-  C                   endif
130200150212      * gestione IP TERMINALE
130300150212 2a  C                   if        v4cipt <> *blanks
130400150212     C                   reset                   tibs02ds
130500150212     C                   movel(p)  v4cipt        t02ke1
130600150212     C                   call      'TIBS02R'
130700150212     C                   parm                    KPJBA
130800150212     C                   parm                    TIBS02DS
130900150212     C                   movel     t02uni        dLAT
131000150212     C                   movel     §LatDes       v4dipt
131100150212 4-  C                   endif
131200150618      *
131300150618      * -?Gestione Filiale Ufficio?
131400150618     c                   clear                   V4Dfuf
131500150618if  1c                   If        V4Cfuf <> *blanks  and
131600150618     c                             V4Cfuf <> *zeros
131700150618     c                   move      V4Cfuf        kFil
131800150618     c     KFil          chain     AZORG
131900150618if  2c                   if        %found(AZORG01L)
132000150618     c                   movel     ORGdes        V4Dfuf
132100150618x   2c                   else
132200150618     c                   eval      V4Dfuf = *all'? '
132300150618e   2c                   endif
132400150618e   1c                   EndIf
132500130705
132600101117      /free
132700150618
132800150317       //?Manutenzione RA
132900150317         IF  V5Crac <> *blanks;
133000150317           reset TIBS02DS;
133100150317           T02ke1 = V5Crac;
133200150317           TNTBE_RicercaControllo (kpjba : TIBS02DS);
133300150317           dLAT = T02uni;
133400150317           V5Drac = §LATdes;
133500150317         ENDIF;
133600150810       //?Addebiti CE
133700150811         IF  V5Cadd <> *blanks;
133800150811           reset TIBS02DS;
133900150811           T02ke1 = V5Cadd;
134000150811           TNTBE_RicercaControllo (kpjba : TIBS02DS);
134100150811           dLAT = T02uni;
134200150811           V5Dadd = §LATdes;
134300150811         ENDIF;
134400150317
134500101117         // -?Ricerca nuovo progressivo per profilo?
134600101117         //  ?(SE copia di profilo di filiale)?
134700130705         If  I31Opz =  '3';
134800130705       //?se ho ricevuo la filiale è copia da utente STD quindi aggiunto '00'
134900130705       //?alla fine del nome utente che ho composto prim con PROFILO STD + FIL
135000130709           IF  I31fil <> *blanks and I31fil <> *zeros;
135100130705             %subst(V1Cute:7:2) = '00';
135200130705           ENDIF;
135300130705           IF  %len(%trimr(V1Cute)) = 8  and
135400130705               %check(c_Digits : %subst(V1Cute:7:2)) = *zeros;
135500101117           exsr  sr_FndUsrPrg;
135600130705           ENDIF;
135700101117         EndIf;
135800130705
135900130705        // se non è un utente STANDARD imposto a video i campi del profilo STD
136000130806         wOKuteSTD = *off;
136100130705         clear xx;
136200130705         clear V1Sfun;
136300130705         clear V1Saut;
136400130705         clear V2Sibs;
136500130708         clear V2Saub;
136600130705         clear V2Sias;
136700130705         clear V2Siun;
136800130705         clear V2Scun;
136900130705         clear V2Sdst;
137000130705         clear V2Sdss;
137100130705         clear V2Spro;
137200130705         clear V2Scap;
137300130705         clear V3Scep;
137400130705         clear V3Sfgia;
137500140603         clear V3Sabres;
137600130705         clear V3Sgtc;
137700130705         clear V3Smodtar;
137800130705         clear V3Scar;
137900130705         clear V3Sctc;
138000130705         clear V3Sdupcli;
138100130705         clear V3Spot;
138200130705         clear V3Spotmsf;
138300130705         clear V3Spotst;
138400130705         clear V3Scli;
138500130705         clear V3Sclpfft;
138600160905         clear V3Sksclog;
138700130705         clear V3Smntsp;
138800130705         clear V3Schgcmm;
138900130705         clear V3Seca;
139000130705         clear V3Sflna;
139100130705         clear V2Sist;
139200160905         clear V4Sinvpwd;
139300130705         clear V3Sstp;
139400130705         clear V3Smbf;
139500130705         clear V3Smtf;
139600130705         clear V3Smfcp;
139700130705         clear V3Sorm;
139800160422         clear V3Sstmo;
139900130705         clear V4Smaz;
140000130705         clear V4Sgorg;
140100130705         clear V4Smps;
140200130705         clear V4Spwd;
140300150212         clear V4Sipt;
140400150618         clear V4Sfuf;
140500150210         clear V4Scgb;
140600150210         clear V4Smgb;
140700160808         clear V4Scanc;
140800130705         clear V4Spswaut;
140900130705         clear V4Spswmag;
141000130705         clear V4Spswr;
141100130705         clear V4Sgdcksc;
141200130806         clear V5Svrac;
141300150317         clear V5Srac;
141400130806         clear V5Smresca;
141500150810         clear V5Svadd;
141600150810         clear V5Sadd;
141700130705         IF  V1Cmopr <> 'S';
141800130709         // se funzione aziendale NO COCOM devo cercare STD uguale ai primi 3 dell'utente
141900140710         // stessa cosa se funzione aziendale NO REA e RED
142000140522         // se non è filiale utente messaggeria di area devo aggiungere una 'M' al profilo per
142100140522         // cercare il profilo STD delle messaggerie standard
142200140710           IF  V1Cfun <> 'COCOM' and V1Cfun <> 'RED' and V1Cfun <> 'REA';
142300140522             wrkUTE = %subst(V1Cute:1:3);
142400140523             IF  IsFilMess;
142500140522               wrkUTE = %trim(wrkUTE) + %trim('M');
142600140522             ENDIF;
142700140522             //xx = %lookup(%subst(V1Cute:1:3):skUTE);
142800140522             xx = %lookup(wrkUTE:skUTE);
142900130709           ELSE;
143000130709         // se funzione aziendale COCOM devo cercare STD COCOM
143100140710             IF  V1Cfun = 'COCOM';
143200130709             xx = %lookup('COCOM':skUTE);
143300140710             ENDIF;
143400140710         // se funzione aziendale RED devo cercare STD RDRED
143500140710             IF  V1Cfun = 'RED';
143600140710               xx = %lookup('RDRED':skUTE);
143700140710             ENDIF;
143800140710         // se funzione aziendale REA devo cercare STD RAREA
143900140710             IF  V1Cfun = 'REA';
144000140710               xx = %lookup('RAREA':skUTE);
144100140710             ENDIF;
144200130709           ENDIF;
144300130705           IF  xx > 0;
144400130806             wOKuteSTD = *on;
144500130705             sUTE01 = skABI(xx);
144600130705             sUTE02 = skABI1(xx);
144700130705             V1Sfun = skFUN(xx);
144800130705             V1Saut = skAUT(xx);
144900130705             V2Sibs = sUTEibs;
145000130708             V2Saub = sUTEmbs;
145100130705             V2Sias = sUTEias;
145200130705             V2Siun = sUTEiun;
145300130705             V2Scun = sUTEcun;
145400130705             V2Sdst = sUTEdst;
145500130705             V2Sdss = sUTEdss;
145600130705             V2Spro = sUTEpro;
145700130705             V2Scap = sUTEcap;
145800130705             V3Scep = sUTEcep;
145900130705             V3Sfgia = sUTEfgia;
146000140603             V3Sabres = sUTEabres;
146100130705             V3Sgtc = sUTEgtc;
146200130705             V3Smodtar = sUTEmodtar;
146300130705             V3Scar = sUTEcar;
146400130705             V3Sctc = sUTEctc;
146500130705             V3Sdupcli = sUTEdupcli;
146600130705             V3Spot = sUTEpot;
146700130705             V3Spotmsf = sUTEpotmsf;
146800130705             V3Spotst = sUTEpotst;
146900130705             V3Scli = sUTEcli;
147000130705             V3Sclpfft = sUTEclpfft;
147100160905             V3Sksclog = sUTEksclog;
147200130705             V3Smntsp = sUTEmntsp;
147300130705             V3Schgcmm = sUTEchgcmm;
147400130705             V3Seca = sUTEeca;
147500130705             V3Sflna = sUTEflna;
147600130705             V2Sist = sUTEist;
147700160905             V4Sinvpwd = sUTEinvpwd;
147800130705             V3Sstp = sUTEstp;
147900130705             V3Smbf = sUTEmbf;
148000130705             V3Smtf = sUTEmtf;
148100130705             V3Smfcp = sUTEmfcp;
148200130705             V3Sorm = sUTEorm;
148300160422             V3Sstmo = sUTEistmo;
148400130705             V4Smaz = sUTEmaz;
148500130705             V4Sgorg = sUTEgorg;
148600130705             V4Smps = sUTEmps;
148700130705             V4Spwd = sUTEpwd;
148800150212             V4Sipt = suteipt;
148900150618             V4Sfuf = sUTEfuf;
149000150210             V4Scgb = sUTEcgb;
149100150210             V4Smgb = sUTEmgb;
149200160808             V4Scanc = sUTEcanc;
149300130705             V4Spswaut = sUTEpswaut;
149400130705             V4Spswmag = sUTEpswmag;
149500130705             V4Spswr = sUTEpswr;
149600130705             V4Sgdcksc = sUTEgdcksc;
149700130806             V5Svrac = sUTEvrac;
149800150317             V5Srac = sUTErac;
149900130806             V5Smresca = sUTEmresca;
150000150810             V5Svadd = sUTEaddt;
150100150810             V5Sadd = sUTEadd;
150200130705           ENDIF;
150300130708        // confronto le abilitazioni RI quelle diverse
150400130708           IF  V2Cibs <> V2Sibs;
150500130708             ricibs = rimm;
150600130708           ENDIF;
150700130708           IF  V2Caub <> V2Saub;
150800130708             ricmbs = rimm;
150900130708           ENDIF;
151000130708           IF  V2Cias <> V2Sias;
151100130708             ricias = rimm;
151200130708           ENDIF;
151300130708           IF  V2Ciun <> V2Siun;
151400130708             riciun = rimm;
151500130708           ENDIF;
151600130708           IF  V2Ccun <> V2Scun;
151700130708             riccun = rimm;
151800130708           ENDIF;
151900130705         ENDIF;
152000130705
152100101117      /end-free
152200080218     C                   ENDSR
152300101117
152400101117      /free
152500101117
152600101117       //--------------------------------------------------------------
152700101117       //?Ricerca Progressivo per Utente di Filiale (in caso di COPIA) ?
152800101117       //--------------------------------------------------------------
152900101117       BEGSR  sr_FndUsrPrg;
153000101117
153100101117         wProgr = 1;
153200130709
153300130705         $Ute = %subst( V1Cute : 1 : 6 ) + %editc( wProgr : 'X' );
153400130709
153500101117         setll  ($Ute)  AZUTE000;
153600101117         read   AZUTE000;
153700101117
153800101117         Dow  Not %eof(AZUTE01L)   and
153900130718              %subst( UTEute : 1 : 6 ) = %subst( $Ute : 1 : 6 );
154000101117
154100101117           select;
154200130718             when  %check( c_Digits : %subst( UTEute : 7 : 2 ) ) > *zero;
154300101117               leave;
154400130718             when  wProgr <> %int( %subst( UTEute : 7 : 2 ) );
154500101117               leave;
154600101117             when  wProgr = *hival;
154700101117               clear  wProgr;
154800101117               leave;
154900101117           endsl;
155000101117
155100101117           wProgr += 1;
155200101117           read   AZUTE000;
155300101117
155400101117         EndDo;
155500101117
155600101117         V1Cute = %subst( $Ute : 1 : 6 ) + %editc( wProgr : 'X' );
155700101117
155800101117       ENDSR;
155900101117
156000101117      /end-free
156100101117
156200011121      *--------------------------------------------------------------------------------------------*
156300011121      * GesD02 - Gestione seconda videata -
156400011121      *--------------------------------------------------------------------------------------------*
156500011121     C     GesD02        BEGSR
156600011121      *
156700011121      * Scrivo la testata
156800080218     C                   write     Bs31T01
156900080218     c                   write     Bs31T02
157000011121      *
157100011121      * Se esistono errori sulla videata
157200011121      * emetto la write del formato a indicatori spenti per vedere
157300011121      * le eventuali decodifiche
157400080218if  1C                   if        *in28
157500080218     C                   movea     *in           Win
157600080218     C                   move      *all'0'       in2841
157700080218     C                   movea     in2841        *in(28)
157800080218     C                   write     Bs31D02
157900080218     C                   movea     Win           *in
158000080218e   1C                   endif
158100011121      *
158200080218if  1C                   if        I31Opz = '5'
158300080218      * Visualizzazione
158400080218     C                   write     Bs31D02
158500080218     C                   exfmt     PROTECT
158600080218x   1C                   else
158700011121      * Immissione/Modifica/Copia
158800080218     C                   exfmt     Bs31D02
158900080218e   1C                   endif
159000011121      *
159100080218     c                   setoff                                       28  90
159200080218     c                   clear                   V1Dmsg
159300080218      *
159400080218sel 1C                   SELECT
159500011121      * F3=Fine
159600080218w   1C                   when      *inKC
159700080218     C                   exsr      F03Ges
159800080218     c                   leavesr
159900040729      * F8=Modulo_Base + AS/400 del profilo
160000080218w   1C                   when      *inKH
160100040729     c                   exsr      CallTIBS32
160200080218     c                   leavesr
160300011121      * F12=Ritorno
160400080218w   1C                   when      *inKL
160500011121     C                   EXSR      F12Ges
160600080218     c                   leavesr
160700161006      * F18=Note
160800161006w   1c                   WHEN      *inKS
160900161006     c                   exsr      sr_CallTIBS43
161000161006     c                   leavesr
161100011122      *
161200080218e   1C                   ENDSL
161300011121      *
161400011121      * Controlli e decodifiche
161500080218     C                   exsr      CtrD02
161600040922      *
161700080218if  1C                   if             *in28       = *off
161800080218     C                             and  $PostSearch = *off
161900040922      * Carico la terza videata
162000040922     C                   eval      TipVid  = '3'
162100080218e   1C                   endif
162200011121      *
162300080218     C                   ENDSR
162400011121      *-----------------------------------------------------------
162500020315      * Controllo e decodifica seconda videata
162600011121      *-----------------------------------------------------------
162700011126     C     CtrD02        BEGSR
162800011121      *
162900080218     C                   setoff                                       404142
163000140403     C                   setoff                                       5071
163100130708
163200130708     c                   clear                   ricibs
163300130708     c                   clear                   ricmbs
163400130708     c                   clear                   ricias
163500130708     c                   clear                   riciun
163600130708     c                   clear                   riccun
163700130708     c                   clear                   ricdst
163800130708     c                   clear                   ricdss
163900130708     c                   clear                   ricpro
164000130708     c                   clear                   riccep
164100130708     c                   clear                   ricfgia
164200040922      *
164300040922      * Routine per gestione preparazione ricerche
164400080218     C                   exsr      GesSearch
164500040922      *
164600020516      * Autorizzazione interrogazione bolle sede
164700020516     C                   clear                   V2DIbs
164800040922 1a  C     *In64         ifeq      *on
164900020516     C     *In28         andeq     *off
165000020516      *
165100020516     C                   move      *blanks       Tibs02Ds
165200080218     C                   move      'R'           T02Mod
165300080218     C                   movel     KnSif         T02Sif
165400020516     C                   move      'LAT'         T02Cod
165500020516     C                   CALL      'TIBS02R'
165600020516     C                   PARM                    kpjba
165700020516     C                   PARM                    Tibs02Ds
165800020516     C                   movel     T02ke1        V2CIbs
165900020516     C                   movel     T02uni        V2DIbs
166000040922     C                   setoff                                       64
166100020516 1-  C                   endif
166200020516      *
166300020516      * Non devo eseguire i controlli se richiesta ricerca
166400040922 1a  C     *in64         ifeq      *off
166500020516 2a  C     V2CIbs        ifne      *blanks
166600020516      * Se esiste decodifico
166700080218     C                   reset                   tibs02ds
166800020516     C                   movel(p)  V2CIbs        t02ke1
166900020516     C                   call      'TIBS02R'
167000020516     C                   parm                    KPJBA
167100020516     C                   parm                    TIBS02DS
167200020516 3a  C                   if        t02err =  *blanks
167300020516     C                   movel     t02uni        dLAT
167400020516 4a  C     V1CFil        ifeq      '046'
167500020516     C     §LatUus       andne     'S'
167600080218     C  n28              eval      V1Dmsg = Msg(18)
167700020516     C  n28              seton                                        2842
167800020516     C                   else
167900020516     C                   movel     §LatDes       V2DIbs
168000020516 4-  C                   endif
168100020516 3e  C                   else
168200080218     C  n28              eval      V1Dmsg = Msg(12)
168300020516     C  n28              seton                                        2842
168400020516 3-  C                   endif
168500020516 2-  C                   endif
168600020516 1-  C                   endif
168700130708      /free
168800130708        // confronto con lo STD
168900130806         IF  V2Cibs <> V2Sibs and V1Cmopr <> 'S' and wOKuteSTD;
169000130708           ricibs = rimm;
169100130708         ENDIF;
169200130708      /end-free
169300020516      *
169400020516      * Autorizzazione manutenzione bolle sede
169500011126     C                   clear                   V2DAuB
169600040922 1a  C     *In65         ifeq      *on
169700011122     C     *In28         andeq     *off
169800011122      *
169900011122     C                   move      *blanks       Tibs02Ds
170000080218     C                   move      'R'           T02Mod
170100080218     C                   movel     KnSif         T02Sif
170200011122     C                   move      'LAT'         T02Cod
170300011122     C                   CALL      'TIBS02R'
170400011122     C                   PARM                    kpjba
170500011122     C                   PARM                    Tibs02Ds
170600011126     C                   movel     T02ke1        V2CAuB
170700011126     C                   movel     T02uni        V2DAuB
170800040922     C                   setoff                                       65
170900011122 1-  C                   endif
171000011122      *
171100011127      * Non devo eseguire i controlli se richiesta ricerca
171200040922 1a  C     *in65         ifeq      *off
171300011127 2a  C     V2CAuB        ifne      *blanks
171400011121      *
171500011121      * Se esiste decodifico
171600080218     C                   reset                   tibs02ds
171700011126     C                   movel     V2CAuB        t02ke1
171800011121     C                   call      'TIBS02R'
171900011121     C                   parm                    KPJBA
172000011121     C                   parm                    TIBS02DS
172100011127 3a  C                   if        t02err =  *blanks
172200020402     C                   movel     t02uni        dLAT
172300020402 4a  C     V1CFil        ifeq      '046'
172400020402     C     §LatUus       andne     'S'
172500080218     C  n28              eval      V1Dmsg = Msg(18)
172600020402     C  n28              seton                                        2840
172700020402     C                   else
172800020402     C                   movel     §LatDes       V2DAuB
172900020402 4-  C                   endif
173000011127 3e  C                   else
173100080218     C  n28              eval      V1Dmsg = Msg(12)
173200020315     C  n28              seton                                        2840
173300011127 3-  C                   endif
173400011127 2-  C                   endif
173500011122 1-  C                   endif
173600130708      /free
173700130708        // confronto con lo STD
173800130806         IF  V2Caub <> V2Saub and V1Cmopr <> 'S' and wOKuteSTD;
173900130708           ricmbs = rimm;
174000130708         ENDIF;
174100130806         IF  V2Cias <> V2Sias and V1Cmopr <> 'S' and wOKuteSTD;
174200130708           ricias = rimm;
174300130708         ENDIF;
174400130708      /end-free
174500011128      *
174600020315      * Autorizzazione file unità
174700020402     C                   clear                   V2DIUn
174800040922 1a  C     *In66         ifeq      *on
174900020315     C     *In28         andeq     *off
175000080218     C                   clear                   Tibs02Ds
175100080218     C                   move      'R'           T02Mod
175200080218     C                   movel     KnSif         T02Sif
175300020315     C                   move      'LAT'         T02Cod
175400020315     C                   CALL      'TIBS02R'
175500020315     C                   PARM                    kpjba
175600020315     C                   PARM                    Tibs02Ds
175700020402     C                   movel     T02ke1        V2CIUn
175800020402     C                   movel     T02uni        V2DIUn
175900040922     C                   setoff                                       66
176000020315 1-  C                   endif
176100020315      *
176200020315      * Non devo eseguire i controlli se richiesta ricerca
176300040922 1a  C     *in66         ifeq      *off
176400020402 2a  C     V2CIUn        ifne      *blanks
176500020315      *
176600020315      * Se esiste decodifico
176700080218     C                   reset                   tibs02ds
176800020402     C                   movel     V2CIUn        t02ke1
176900020315     C                   call      'TIBS02R'
177000020315     C                   parm                    KPJBA
177100020315     C                   parm                    TIBS02DS
177200020315 3a  C                   if        t02err =  *blanks
177300020402     C                   movel     t02uni        dLAT
177400020402 4a  C     V1CFil        ifeq      '046'
177500020402     C     §LatUus       andne     'S'
177600080218     C  n28              eval      V1Dmsg = Msg(18)
177700020402     C  n28              seton                                        2841
177800020402     C                   else
177900020402     C                   movel     §LatDes       V2DIUn
178000020402 4-  C                   endif
178100020315 3e  C                   else
178200080218     C  n28              eval      V1Dmsg = Msg(12)
178300020315     C  n28              seton                                        2841
178400020315 3-  C                   endif
178500020315 2-  C                   endif
178600020315 1-  C                   endif
178700130708      /free
178800130708        // confronto con lo STD
178900130806         IF  V2Ciun <> V2Siun and V1Cmopr <> 'S' and wOKuteSTD;
179000130708           riciun = rimm;
179100130708         ENDIF;
179200130806         IF  V2Ccun <> V2Scun and V1Cmopr <> 'S' and wOKuteSTD;
179300130708           riccun = rimm;
179400130708         ENDIF;
179500130806         IF  V2Cdst <> V2Sdst and V1Cmopr <> 'S' and wOKuteSTD;
179600130708           ricdst = rimm;
179700130708         ENDIF;
179800130806         IF  V2Cdss <> V2Sdss and V1Cmopr <> 'S' and wOKuteSTD;
179900130708           ricdss = rimm;
180000130708         ENDIF;
180100130806         IF  V2Cpro <> V2Spro and V1Cmopr <> 'S' and wOKuteSTD;
180200130708           ricpro = rimm;
180300130708         ENDIF;
180400130806         IF  V2Ccap <> V2Scap and V1Cmopr <> 'S' and wOKuteSTD;
180500130708           riccap = rimm;
180600130708         ENDIF;
180700130806         IF  V3Ccep <> V3Scep and V1Cmopr <> 'S' and wOKuteSTD;
180800130708           riccep = rimm;
180900130708         ENDIF;
181000130806         IF  V3Cfgia <> V3Sfgia and V1Cmopr <> 'S' and wOKuteSTD;
181100130708           ricfgia = rimm;
181200130708         ENDIF;
181300140603         IF  V3Cabres <> V3Sabres and V1Cmopr <> 'S' and wOKuteSTD;
181400140603           ricabres = rimm;
181500140603         ENDIF;
181600130708      /end-free
181700140403      * gestione abilitazione ai resi in periodo di traizoni ridotte
181800140403     C                   clear                   V3Dabres
181900140403 1a  C     *In70         ifeq      *on
182000140403     C     *In28         andeq     *off
182100140403     C                   clear                   Tibs02Ds
182200140403     C                   move      'R'           T02Mod
182300140403     C                   movel     KnSif         T02Sif
182400140403     C                   move      'LAT'         T02Cod
182500140403     C                   CALL      'TIBS02R'
182600140403     C                   PARM                    kpjba
182700140403     C                   PARM                    Tibs02Ds
182800140403     C                   movel     T02ke1        V3Cabres
182900140403     C                   movel     T02uni        V3dabres
183000140403     C                   setoff                                       70
183100140403 1-  C                   endif
183200140403      *
183300140403      * Non devo eseguire i controlli se richiesta ricerca
183400140403 1a  C     *in70         ifeq      *off
183500140403 2a  C     V3Cabres      ifne      *blanks
183600140403      *
183700140403      * Se esiste decodifico
183800140403     C                   reset                   tibs02ds
183900140403     C                   movel     V3Cabres      t02ke1
184000140403     C                   call      'TIBS02R'
184100140403     C                   parm                    KPJBA
184200140403     C                   parm                    TIBS02DS
184300140403 3a  C                   if        t02err =  *blanks
184400140403     C                   movel     t02uni        dLAT
184500140403 4a  C     V1CFil        ifeq      '046'
184600140403     C     §LatUus       andne     'S'
184700140403     C  n28              eval      V1Dmsg = Msg(18)
184800140403     C  n28              seton                                        2871
184900140403     C                   else
185000140403     C                   movel     §LatDes       V3Dabres
185100140403 4-  C                   endif
185200140403 3e  C                   else
185300140403     C  n28              eval      V1Dmsg = Msg(12)
185400140403     C  n28              seton                                        2871
185500140403 3-  C                   endif
185600140403 2-  C                   endif
185700140403 1-  C                   endif
185800020315      *
185900020315      * Se sono in visualizzazione spengo il 28
186000080218if  1c                   if        I31opz = '5'
186100080218     c                   eval      *in28  = *off
186200080218e   1c                   endif
186300020315      *
186400011121     C                   ENDSR
186500040922      *--------------------------------------------------------------------------------------------*
186600040922      * GesD03 - Gestione terza videata -
186700040922      *--------------------------------------------------------------------------------------------*
186800040922     C     GesD03        BEGSR
186900040922      *
187000040922      * Scrivo la testata
187100080218     C                   write     Bs31T01
187200080218     c                   write     Bs31T02
187300040922      *
187400040922      * Se esistono errori sulla videata
187500040922      * emetto la write del formato a indicatori spenti per vedere
187600040922      * le eventuali decodifiche
187700080218if  1C                   if        *in28
187800080218     C                   movea     *in           Win
187900080218     C                   move      *all'0'       in2841
188000080218     C                   movea     in2841        *in(28)
188100080218     C                   write     Bs31D03
188200080218     C                   movea     Win           *in
188300080218e   1C                   endif
188400040922      *
188500080218if  1C                   if        I31opz = '5'
188600080218      * Visualizzazione
188700080218     C                   write     Bs31D03
188800080218     C                   exfmt     PROTECT
188900080218x   1C                   else
189000040922      * Immissione/Modifica/Copia
189100080218     C                   exfmt     Bs31D03
189200080218e   1C                   endif
189300040922      *
189400080218     c                   setoff                                       28  90
189500080218     c                   clear                   V1Dmsg
189600080218      *
189700040922     C                   SELECT
189800040922      * F3=Fine
189900080218     C                   WHEN      *inKC
190000080218     C                   exsr      F03Ges
190100080218     c                   leavesr
190200040922      * F8=Modulo_Base + AS/400 del profilo
190300080218     c                   WHEN      *inKH
190400040922     c                   exsr      CallTIBS32
190500080218     c                   leavesr
190600040922      * F12=Ritorno
190700080218     C                   WHEN      *inKL
190800080218     C                   exsr      F12Ges
190900080218     c                   leavesr
191000161006      * F18=Note
191100161006w   1c                   WHEN      *inKS
191200161006     c                   exsr      sr_CallTIBS43
191300161006     c                   leavesr
191400040922      *
191500080218     C                   ENDSL
191600040922      *
191700040922      * Controlli e decodifiche
191800080218     C                   exsr      CtrD03
191900080218      *
192000080218if  1C                   if             *in28       = *off
192100080218     C                             and  $PostSearch = *off
192200080218      * Carico la quarta videata
192300100917     C                   eval      TipVid  = '³'
192400080218e   1C                   endif
192500040922      *
192600080218     C                   ENDSR
192700040922      *-----------------------------------------------------------
192800040922      * Controllo e decodifica terza videata
192900040922      *-----------------------------------------------------------
193000040922     C     CtrD03        BEGSR
193100040922      *
193200080218     C                   setoff                                       60
193300040922     C                   setoff                                       5152
193400041207     C                   setoff                                       5253
193500080218     C                   clear                   V1Dmsg
193600130708
193700130708     c                   clear                   ricgtc
193800130708     c                   clear                   ricmodtar
193900130708     c                   clear                   riccar
194000130708     c                   clear                   ricctc
194100130708     c                   clear                   ricdupcli
194200130708     c                   clear                   ricpot
194300130708     c                   clear                   ricpotmsf
194400130708     c                   clear                   ricpotst
194500130708     c                   clear                   riccli
194600130708     c                   clear                   ricclpfft
194700160905     c                   clear                   ricksclog
194800130708     c                   clear                   ricmntsp
194900130708     c                   clear                   ricchgcmm
195000130708     c                   clear                   riceca
195100130708     c                   clear                   ricflna
195200130708     c                   clear                   ricist
195300130708     c                   clear                   ricinvpwd
195400130708     c                   clear                   ricstp
195500101008      *
195600101008      * Routine per gestione preparazione ricerche
195700101008     C                   exsr      GesSearch
195800101008      *
195900040922      * Autorizzazione Gestione tariffe clienti
196000040922     C     '?'           scan      v3cgtc                                 60
196100040922     C                   clear                   v3dgtc
196200040922 1a  C                   if        *In60 and Not *In28
196300080218     C                   clear                   Tibs02Ds
196400040922     C                   move      'R'           T02Mod
196500040922     C                   movel     KnSif         T02Sif
196600080218     C                   move      'LAT'         T02Cod
196700040922     C                   CALL      'TIBS02R'
196800040922     C                   PARM                    kpjba
196900040922     C                   PARM                    Tibs02Ds
197000040922     C                   movel     T02ke1        v3cgtc
197100040922     C                   movel     T02uni        v3dgtc
197200040922     C                   setoff                                       60
197300040922 1-  C                   endif
197400040922      *
197500040922      * Non devo eseguire i controlli se richiesta ricerca
197600040922 1a  C                   if        Not *In60
197700040922 2a  C                   if        v3cgtc <> *blanks
197800040922      * Se esiste decodifico
197900080218     C                   reset                   tibs02ds
198000040922     C                   movel(p)  v3cgtc        t02ke1
198100040922     C                   call      'TIBS02R'
198200040922     C                   parm                    KPJBA
198300040922     C                   parm                    TIBS02DS
198400040922 3a  C                   if        t02err =  *blanks
198500040922     C                   movel     t02uni        dLAT
198600040922 4a  C     V1CFil        ifeq      '046'
198700040922     C     §LatUus       andne     'S'
198800080218     C  n28              eval      V1Dmsg = Msg(18)
198900040922     C  n28              seton                                        2851
199000040922     C                   else
199100040922     C                   movel     §LatDes       v3dgtc
199200040922 4-  C                   endif
199300040922 3e  C                   else
199400080218     C  n28              eval      V1Dmsg = Msg(12)
199500040922     C  n28              seton                                        2851
199600040922 3-  C                   endif
199700040922 2-  C                   endif
199800040922 1-  C                   endif
199900130708      /free
200000130708        // confronto con lo STD
200100130806         IF  V3Cgtc <> V3Sgtc and V1Cmopr <> 'S' and wOKuteSTD;
200200130708           ricgtc = rimm;
200300130708         ENDIF;
200400130806         IF  V3modtar <> V3Smodtar and V1Cmopr <> 'S' and wOKuteSTD;
200500130708           ricmodtar = rimm;
200600130708         ENDIF;
200700130806         IF  V3Ccar <> V3Scar and V1Cmopr <> 'S' and wOKuteSTD;
200800130708           riccar = rimm;
200900130708         ENDIF;
201000130708      /end-free
201100040922      *
201200040922      * Autorizzazione codice cliente da cui copiare
201300040922     C     '?'           scan      v3cctc                                 60
201400040922     C                   clear                   v3dctc
201500040922 1a  C                   if        *In60 and Not *In28
201600080218     C                   clear                   Tibs02Ds
201700040922     C                   move      'R'           T02Mod
201800040922     C                   movel     KnSif         T02Sif
201900080218     C                   move      'LAT'         T02Cod
202000040922     C                   CALL      'TIBS02R'
202100040922     C                   PARM                    kpjba
202200040922     C                   PARM                    Tibs02Ds
202300040922     C                   movel     T02ke1        v3cctc
202400040922     C                   movel     T02uni        v3dctc
202500040922     C                   setoff                                       60
202600040922 1-  C                   endif
202700040922      *
202800040922      * Non devo eseguire i controlli se richiesta ricerca
202900040922 1a  C                   if        Not *In60
203000040922 2a  C                   if        v3cctc <> *blanks
203100040922      * Se esiste decodifico
203200080218     C                   reset                   tibs02ds
203300040922     C                   movel     v3cctc        t02ke1
203400040922     C                   call      'TIBS02R'
203500040922     C                   parm                    KPJBA
203600040922     C                   parm                    TIBS02DS
203700040922 3a  C                   if        t02err =  *blanks
203800040922     C                   movel     t02uni        dLAT
203900040922 4a  C     V1CFil        ifeq      '046'
204000040922     C     §LatUus       andne     'S'
204100080218     C  n28              eval      V1Dmsg = Msg(18)
204200040922     C  n28              seton                                        2852
204300040922     C                   else
204400040923     C                   movel     §LatDes       V3dctc
204500040922 4-  C                   endif
204600040922 3e  C                   else
204700080218     C  n28              eval      V1Dmsg = Msg(12)
204800040922     C  n28              seton                                        2852
204900040922 3-  C                   endif
205000040922 2-  C                   endif
205100040922 1-  C                   endif
205200130708      /free
205300130708        // confronto con lo STD
205400130806         IF  V3Cctc <> V3Sctc and V1Cmopr <> 'S' and wOKuteSTD;
205500130708           ricctc = rimm;
205600130708         ENDIF;
205700130806         IF  V3Cdupcli <> V3Sdupcli and V1Cmopr <> 'S' and wOKuteSTD;
205800130708           ricdupcli = rimm;
205900130708         ENDIF;
206000130708      /end-free
206100041207      *
206200041207      * Autorizzazione clienti potenziali
206300041207     C     '?'           scan      v3cpot                                 60
206400041207     C                   clear                   v3dpot
206500041207 1a  C                   if        *In60 and Not *In28
206600080218     C                   clear                   Tibs02Ds
206700041207     C                   move      'R'           T02Mod
206800041207     C                   movel     KnSif         T02Sif
206900080218     C                   move      'LAT'         T02Cod
207000041207     C                   CALL      'TIBS02R'
207100041207     C                   PARM                    kpjba
207200041207     C                   PARM                    Tibs02Ds
207300041207     C                   movel     T02ke1        v3cpot
207400041207     C                   movel     T02uni        v3dpot
207500041207     C                   setoff                                       60
207600041207 1-  C                   endif
207700041207      *
207800041207      * Non devo eseguire i controlli se richiesta ricerca
207900041207 1a  C                   if        Not *In60
208000041207 2a  C                   if        v3cpot <> *blanks
208100041207      * Se esiste decodifico
208200080218     C                   reset                   tibs02ds
208300041207     C                   movel     v3cpot        t02ke1
208400041207     C                   call      'TIBS02R'
208500041207     C                   parm                    KPJBA
208600041207     C                   parm                    TIBS02DS
208700041207 3a  C                   if        t02err =  *blanks
208800041207     C                   movel     t02uni        dLAT
208900041207 4a  C     V1CFil        ifeq      '046'
209000041207     C     §LatUus       andne     'S'
209100080218     C  n28              eval      V1Dmsg = Msg(18)
209200041207     C  n28              seton                                        2853
209300041207     C                   else
209400041207     C                   movel     §LatDes       V3dpot
209500041207 4-  C                   endif
209600041207 3e  C                   else
209700080218     C  n28              eval      V1Dmsg = Msg(12)
209800041207     C  n28              seton                                        2853
209900041207 3-  C                   endif
210000041207 2-  C                   endif
210100041207 1-  C                   endif
210200130708      /free
210300130708        // confronto con lo STD
210400130806         IF  V3Cpot <> V3Spot and V1Cmopr <> 'S' and wOKuteSTD;
210500130708           ricpot = rimm;
210600130708         ENDIF;
210700130806         IF  V3Cpotmsf <> V3Spotmsf and V1Cmopr <> 'S' and wOKuteSTD;
210800130708           ricpotmsf = rimm;
210900130708         ENDIF;
211000130806         IF  V3Cpotst <> V3Spotst and V1Cmopr <> 'S' and wOKuteSTD;
211100130708           ricpotst = rimm;
211200130708         ENDIF;
211300130708      /end-free
211400041207      *
211500041207      * Autorizzazione anagrafica clienti
211600041207     C     '?'           scan      v3ccli                                 60
211700041207     C                   clear                   v3dcli
211800041207 1a  C                   if        *In60 and Not *In28
211900080218     C                   clear                   Tibs02Ds
212000041207     C                   move      'R'           T02Mod
212100041207     C                   movel     KnSif         T02Sif
212200080218     C                   move      'LAT'         T02Cod
212300041207     C                   CALL      'TIBS02R'
212400041207     C                   PARM                    kpjba
212500041207     C                   PARM                    Tibs02Ds
212600041207     C                   movel     T02ke1        v3ccli
212700041207     C                   movel     T02uni        v3dcli
212800041207     C                   setoff                                       60
212900041207 1-  C                   endif
213000041207      *
213100041207      * Non devo eseguire i controlli se richiesta ricerca
213200041207 1a  C                   if        Not *In60
213300041207 2a  C                   if        v3ccli <> *blanks
213400041207      * Se esiste decodifico
213500080218     C                   reset                   tibs02ds
213600041207     C                   movel     v3ccli        t02ke1
213700041207     C                   call      'TIBS02R'
213800041207     C                   parm                    KPJBA
213900041207     C                   parm                    TIBS02DS
214000041207 3a  C                   if        t02err =  *blanks
214100041207     C                   movel     t02uni        dLAT
214200041207 4a  C     V1CFil        ifeq      '046'
214300041207     C     §LatUus       andne     'S'
214400080218     C  n28              eval      V1Dmsg = Msg(18)
214500041207     C  n28              seton                                        2854
214600041207     C                   else
214700041207     C                   movel     §LatDes       V3dcli
214800041207 4-  C                   endif
214900041207 3e  C                   else
215000080218     C  n28              eval      V1Dmsg = Msg(12)
215100041207     C  n28              seton                                        2854
215200041207 3-  C                   endif
215300041207 2-  C                   endif
215400041207 1-  C                   endif
215500130708      /free
215600130708        // confronto con lo STD
215700130806         IF  V3Ccli <> V3Scli and V1Cmopr <> 'S' and wOKuteSTD;
215800130708           riccli = rimm;
215900130708         ENDIF;
216000130806         IF  V3clpfft <> V3Sclpfft and V1Cmopr <> 'S' and wOKuteSTD;
216100130708           ricclpfft = rimm;
216200130708         ENDIF;
216300160905         IF  V3Cksclog <> V3Sksclog and V1Cmopr <> 'S' and wOKuteSTD;
216400160905           ricksclog = rimm;
216500160905         ENDIF;
216600130806         IF  V3Cmntsp <> V3Smntsp and V1Cmopr <> 'S' and wOKuteSTD;
216700130708           ricmntsp = rimm;
216800130708         ENDIF;
216900130806         IF  V3Cchgcmm <> V3Schgcmm and V1Cmopr <> 'S' and wOKuteSTD;
217000130708           ricchgcmm = rimm;
217100130708         ENDIF;
217200130708      /end-free
217300040922      *
217400040922      * Se sono in visualizzazione spengo il 28
217500080218     c                   if        I31opz = '5'
217600080218     c                   eval      *in28  = *off
217700080218     c                   endif
217800050609     c
217900050609      * Autorizzazione elenco C.A.
218000050609     C     '?'           scan      v3ceca                                 60
218100050609     C                   clear                   v3deca
218200050609 1a  C                   if        *In60 and Not *In28
218300080218     C                   clear                   Tibs02Ds
218400050609     C                   move      'R'           T02Mod
218500050609     C                   movel     KnSif         T02Sif
218600080218     C                   move      'LAT'         T02Cod
218700050609     C                   CALL      'TIBS02R'
218800050609     C                   PARM                    kpjba
218900050609     C                   PARM                    Tibs02Ds
219000050609     C                   movel     T02ke1        v3ceca
219100050609     C                   movel     T02uni        v3deca
219200050609     C                   setoff                                       60
219300050609 1-  C                   endif
219400050609      *
219500050609 1a  C                   if        Not *In60
219600050609 2a  C                   if        v3ceca <> *blanks
219700050609      * Se esiste decodifico
219800080218     C                   reset                   tibs02ds
219900050609     C                   movel     v3ceca        t02ke1
220000050609     C                   call      'TIBS02R'
220100050609     C                   parm                    KPJBA
220200050609     C                   parm                    TIBS02DS
220300050609 3a  C                   if        t02err =  *blanks
220400050609     C                   movel     t02uni        dLAT
220500050609 4a  C     V1CFil        ifeq      '046'
220600050609     C     §LatUus       andne     'S'
220700080218     C  n28              eval      V1Dmsg = Msg(18)
220800050609     C  n28              seton                                        2855
220900050609     C                   else
221000060126     C                   movel     §LatDes       V3deca
221100050609 4-  C                   endif
221200050609 3e  C                   else
221300080218     C  n28              eval      V1Dmsg = Msg(12)
221400050609     C  n28              seton                                        2855
221500050609 3-  C                   endif
221600050609 2-  C                   endif
221700050609 1-  C                   endif
221800130708      /free
221900130708        // confronto con lo STD
222000130806         IF  V3Ceca <> V3Seca and V1Cmopr <> 'S' and wOKuteSTD;
222100130708           riceca = rimm;
222200130708         ENDIF;
222300130806         IF  V3Cflna <> V3Sflna and V1Cmopr <> 'S' and wOKuteSTD;
222400130708           ricflna = rimm;
222500130708         ENDIF;
222600130708      /end-free
222700101008      *
222800101008      * Autorizzazione statistica cliente (azn. SE60)
222900101008     C                   clear                   V2DISt
223000101008 1a  C     *In67         ifeq      *on
223100101008     C     *In28         andeq     *off
223200101008     C                   move      *blanks       Tibs02Ds
223300101008     C                   move      'R'           T02Mod
223400101008     C                   movel     KnSif         T02Sif
223500101008     C                   move      'LAT'         T02Cod
223600101008     C                   CALL      'TIBS02R'
223700101008     C                   PARM                    kpjba
223800101008     C                   PARM                    Tibs02Ds
223900101008     C                   movel     T02ke1        V2CISt
224000101008     C                   movel     T02uni        V2DISt
224100101008     C                   setoff                                       67
224200101008 1-  C                   endif
224300101008      *
224400101008      * Non devo eseguire i controlli se richiesta ricerca
224500101008 1a  C     *in67         ifeq      *off
224600101008 2a  C     V2CISt        ifne      *blanks
224700101008      * Se esiste decodifico
224800101008     C                   reset                   tibs02ds
224900101008     C                   movel     V2CISt        t02ke1
225000101008     C                   call      'TIBS02R'
225100101008     C                   parm                    KPJBA
225200101008     C                   parm                    TIBS02DS
225300101008 3a  C                   if        t02err =  *blanks
225400101008     C                   movel     t02uni        dLAT
225500101008 4a  C     V1CFil        ifeq      '046'
225600101008     C     §LatUus       andne     'S'
225700101008     C  n28              eval      V1Dmsg = Msg(18)
225800101008     C  n28              seton                                        2850
225900101008     C                   else
226000101008     C                   movel     §LatDes       V2DISt
226100101008 4-  C                   endif
226200101008 3e  C                   else
226300101008     C  n28              eval      V1Dmsg = Msg(12)
226400101008     C  n28              seton                                        2850
226500101008 3-  C                   endif
226600101008 2-  C                   endif
226700101008 1-  C                   endif
226800130708      /free
226900130708        // confronto con lo STD
227000130806         IF  V2Cist <> V2Sist and V1Cmopr <> 'S' and wOKuteSTD;
227100130708           ricist = rimm;
227200130708         ENDIF;
227300160905         IF  V4Cinvpwd <> V4Sinvpwd and V1Cmopr <> 'S' and wOKuteSTD;
227400130708           ricinvpwd = rimm;
227500130708         ENDIF;
227600130806         IF  V3Cstp <> V3Sstp and V1Cmopr <> 'S' and wOKuteSTD;
227700130708           ricstp = rimm;
227800130708         ENDIF;
227900130708      /end-free
228000050609      *
228100050609      * Se sono in visualizzazione spengo il 28
228200080218     c                   if        I31opz = '5'
228300080218     c                   eval      *in28  = *off
228400080218     c                   endif
228500040922      *
228600040922     C                   ENDSR
228700100917      *--------------------------------------------------------------------------------------------*
228800100917      * GesD03B - Gestione terza videata / B
228900100917      *--------------------------------------------------------------------------------------------*
229000100917     C     GesD03B       BEGSR
229100100917      *
229200100917      * Scrivo la testata
229300100917     C                   write     Bs31T01
229400100917     c                   write     Bs31T02
229500100917      *
229600100917      * Se esistono errori sulla videata
229700100917      * emetto la write del formato a indicatori spenti per vedere
229800100917      * le eventuali decodifiche
229900100917if  1C                   if        *in28
230000100917     C                   movea     *in           Win
230100100917     C                   move      *all'0'       in2841
230200100917     C                   movea     in2841        *in(28)
230300100917     C                   write     Bs31D03
230400100917     C                   movea     Win           *in
230500100917e   1C                   endif
230600100917      *
230700100917if  1C                   if        I31opz = '5'
230800100917      * Visualizzazione
230900100917     C                   write     Bs31D03B
231000100917     C                   exfmt     PROTECT
231100100917x   1C                   else
231200100917      * Immissione/Modifica/Copia
231300100917     C                   exfmt     Bs31D03B
231400100917e   1C                   endif
231500100917      *
231600100917     c                   setoff                                       28  90
231700100917     c                   clear                   V1Dmsg
231800100917      *
231900100917     C                   SELECT
232000100917      * F3=Fine
232100100917     C                   WHEN      *inKC
232200100917     C                   exsr      F03Ges
232300100917     c                   leavesr
232400100917      * F8=Modulo_Base + AS/400 del profilo
232500100917     c                   WHEN      *inKH
232600100917     c                   exsr      CallTIBS32
232700100917     c                   leavesr
232800100917      * F12=Ritorno
232900100917     C                   WHEN      *inKL
233000100917     C                   exsr      F12Ges
233100100917     c                   leavesr
233200161006      * F18=Note
233300161006w   1c                   WHEN      *inKS
233400161006     c                   exsr      sr_CallTIBS43
233500161006     c                   leavesr
233600100917      *
233700100917     C                   ENDSL
233800100917      *
233900100917      * Controlli e decodifiche
234000100917     C                   exsr      CtrD03B
234100100917      *
234200100917if  1C                   if             *in28       = *off
234300100917     C                             and  $PostSearch = *off
234400100917      * Carico la quarta videata
234500100917     C                   eval      TipVid  = '4'
234600100917e   1C                   endif
234700100917      *
234800100917     C                   ENDSR
234900100917      *-----------------------------------------------------------
235000100917      * Controllo e decodifica terza videata
235100100917      *-----------------------------------------------------------
235200100917     C     CtrD03B       BEGSR
235300130708
235400130708     c                   clear                   ricmbf
235500130708     c                   clear                   ricmtf
235600130708     c                   clear                   ricmfcp
235700130708     c                   clear                   ricorm
235800160422     c                   clear                   ricstmo
235900140605      /free
236000140605        // confronto con lo STD
236100140605         IF  V3Cmbf <> V3Smbf and V1Cmopr <> 'S' and wOKuteSTD;
236200140605           ricmbf = rimm;
236300140605         ENDIF;
236400140605         IF  V3Cmtf <> V3Smtf and V1Cmopr <> 'S' and wOKuteSTD;
236500140605           ricmtf = rimm;
236600140605         ENDIF;
236700140605         IF  V3Cmfcp <> V3Smfcp and V1Cmopr <> 'S' and wOKuteSTD;
236800140605           ricmfcp = rimm;
236900140605         ENDIF;
237000140605         IF  V3Corm <> V3Sorm and V1Cmopr <> 'S' and wOKuteSTD;
237100140605           ricorm = rimm;
237200140605         ENDIF;
237300160422         IF  V3Cstmo <> V3Sstmo and V1Cmopr <> 'S' and wOKuteSTD;
237400160422           ricstmo = rimm;
237500160422         ENDIF;
237600140605      /end-free
237700130618
237800130618      * Se visualizzazione non controllo
237900130618     c                   IF        *in05
238000130618     c                   leavesr
238100130618     c                   ENDIF
238200130618
238300160422     c                   setoff                                       3938
238400100917      *
238500100917      * Autorizzazione Manutenzione bolle di filiale
238600100917     C     '?'           scan      v3cmbf                                 60
238700100917     C                   clear                   v3dmbf
238800100917 1a  C                   if        *In60 and Not *In28
238900100917     C                   clear                   Tibs02Ds
239000100917     C                   move      'R'           T02Mod
239100100917     C                   movel     KnSif         T02Sif
239200100917     C                   move      'LAT'         T02Cod
239300100917     C                   CALL      'TIBS02R'
239400100917     C                   PARM                    kpjba
239500100917     C                   PARM                    Tibs02Ds
239600100917     C                   movel     T02ke1        v3cmbf
239700100917     C                   movel     T02uni        v3dmbf
239800100917     C                   setoff                                       60
239900100917 1-  C                   endif
240000100917      *
240100100917 1a  C                   if        Not *In60
240200100917 2a  C                   if        v3cmbf <> *blanks
240300100917      * Se esiste decodifico
240400100917     C                   reset                   tibs02ds
240500100917     C                   movel     v3cmbf        t02ke1
240600100917     C                   call      'TIBS02R'
240700100917     C                   parm                    KPJBA
240800100917     C                   parm                    TIBS02DS
240900100917 3a  C                   if        t02err =  *blanks
241000100917     C                   movel     t02uni        dLAT
241100100917 4a  C     V1CFil        ifeq      '046'
241200100917     C     §LatUus       andne     'S'
241300100917     C  n28              eval      V1Dmsg = Msg(18)
241400100917     C  n28              seton                                        2856
241500100917     C                   else
241600100917     C                   movel     §LatDes       V3dmbf
241700100917 4-  C                   endif
241800100917 3e  C                   else
241900100917     C  n28              eval      V1Dmsg = Msg(12)
242000100917     C  n28              seton                                        2856
242100100917 3-  C                   endif
242200100917 2-  C                   endif
242300100917 1-  C                   endif
242400130618
242500130618      * Autorizzazione ORM
242600130618     c                   clear                   V3Dorm
242700130618     c                   IF        %scan('?':V3Corm) > 0
242800130618     c                   clear                   TIBS02DS
242900130618     c                   eval      T02mod = 'R'
243000130618     c                   eval      T02sif = KnSif
243100130618     c                   eval      T02cod = 'LAT'
243200130618     c                   CALL      'TIBS02R'
243300130618     c                   parm                    KPJBA
243400130618     c                   parm                    TIBS02DS
243500130618     c                   eval      V3Corm = T02ke1
243600130618     c                   eval      dLAT = T02uni
243700130618     c                   eval      V3Dorm = §LATdes
243800130618     c                   leavesr
243900130618 1-  c                   ENDIF
244000130618
244100160422 2a  c                   IF        V3Corm <> *blanks
244200130618     c                   reset                   TIBS02DS
244300130618     c                   eval      T02ke1 = V3Corm
244400130618     c                   call      'TIBS02R'
244500130618     c                   parm                    KPJBA
244600130618     c                   parm                    TIBS02DS
244700130618 3a  c                   IF        T02err <> *blanks
244800130618     c                   eval      V1Dmsg = Msg(12)
244900130618     c                   seton                                        2839
245000130618     c                   leavesr
245100130618     c                   ENDIF
245200130618     c                   eval      dLAT = T02uni
245300130618     c                   eval      V3Dorm = §LATdes
245400130618     c                   IF        V1Cfil = '046' and §LATuus <> 'S'
245500130618     c                   eval      V1Dmsg = Msg(18)
245600130618     c                   seton                                        2839
245700130618     c                   leavesr
245800130618     c                   ENDIF
245900160422     c                   ENDIF
246000130618
246100160422      * Autorizzazione statistica movimento
246200160422     c                   clear                   V3Dstmo
246300160422     c                   IF        %scan('?':V3Cstmo) > 0
246400160422     c                   clear                   TIBS02DS
246500160422     c                   eval      T02mod = 'R'
246600160422     c                   eval      T02sif = KnSif
246700160422     c                   eval      T02cod = 'LAT'
246800160422     c                   CALL      'TIBS02R'
246900160422     c                   parm                    KPJBA
247000160422     c                   parm                    TIBS02DS
247100160422     c                   eval      V3Cstmo = T02ke1
247200160422     c                   eval      dLAT = T02uni
247300160422     c                   eval      V3Dstmo = §LATdes
247400160422     c                   leavesr
247500160422 1-  c                   ENDIF
247600160422
247700160422 2a  c                   IF        V3Cstmo =  *blanks
247800160422     c                   leavesr
247900160422     c                   ENDIF
248000160422     c                   reset                   TIBS02DS
248100160422     c                   eval      T02ke1 = V3Cstmo
248200160422     c                   call      'TIBS02R'
248300160422     c                   parm                    KPJBA
248400160422     c                   parm                    TIBS02DS
248500160422 3a  c                   IF        T02err <> *blanks
248600160422     c                   eval      V1Dmsg = Msg(12)
248700160422     c                   seton                                        2838
248800160422     c                   leavesr
248900160422     c                   ENDIF
249000160422     c                   eval      dLAT = T02uni
249100160422     c                   eval      V3Dstmo = §LATdes
249200160422     c                   IF        V1Cfil = '046' and §LATuus <> 'S'
249300160422     c                   eval      V1Dmsg = Msg(18)
249400160422     c                   seton                                        2838
249500160422     c                   leavesr
249600160422     c                   ENDIF
249700100917      *
249800100917      * Se sono in visualizzazione spengo il 28
249900100917     c                   if        I31opz = '5'
250000100917     c                   eval      *in28  = *off
250100100917     c                   endif
250200100917      *
250300100917     C                   ENDSR
250400080218      *---------------------------------------------------------------*
250500080218      * GesD04 - Gestione quarta videata -                            *
250600080218      *---------------------------------------------------------------*
250700080218     c     GesD04        BEGSR
250800080218      *
250900080218      * Emissione testata
251000080218     c                   write     Bs31T01
251100080218     c                   write     Bs31T02
251200080218      *
251300080218      * Se esistono errori sulla videata
251400080218      *   emetto la videata ad indicatori spenti per visualizzare le
251500080218      *   eventuali decodifiche
251600080218if  1c                   if        *in28
251700080218     c                   movea     *in           Win
251800080218     c                   move      *all'0'       in2841
251900080218     c                   movea     in2841        *in(28)
252000080218     c                   write     Bs31D04
252100080218     c                   movea     Win           *in
252200080218e   1c                   endif
252300080218      *
252400080218if  1c                   if        I31opz = '5'
252500080218      * Visualizzazione
252600080218     c                   write     Bs31D04
252700080218     c                   exfmt     PROTECT
252800080218x   1c                   else
252900080218      *
253000080218      * Immissione/Modifica/Copia
253100080218     c                   exfmt     Bs31D04
253200080218e   1c                   endif
253300080218      *
253400080218     c                   setoff                                       28  90
253500080218     c                   clear                   V1Dmsg
253600080218      *
253700080218sel 1c                   SELECT
253800080218      * F3=Fine
253900080218w   1c                   WHEN      *inKC
254000080218     c                   exsr      F03Ges
254100080218     c                   leavesr
254200080218      * F8=Modulo_Base + AS/400 del profilo
254300080218w   1c                   WHEN      *inKH
254400080218     c                   exsr      CallTIBS32
254500080218     c                   leavesr
254600080218      * F12=Ritorno
254700080218w   1c                   WHEN      *inKL
254800080218     c                   exsr      F12Ges
254900080218     c                   leavesr
255000161006      * F18=Note
255100161006w   1c                   WHEN      *inKS
255200161006     c                   exsr      sr_CallTIBS43
255300161006     c                   leavesr
255400080530      * '?' gestione password
255500080530     c                   WHEN      V4Cpwdint = '?'
255600080530     c                   eval      TipVid    = 'W'
255700080530     c                   leavesr
255800080218      *
255900080218e   1c                   ENDSL
256000080218      *
256100080218      * Controlli e decodifiche
256200080218     c                   exsr      CtrD04
256300130806      *
256400130806if  1C                   if             *in28       = *off
256500130806     C                             and  $PostSearch = *off
256600130806      * Carico la quinta videata
256700130806     C                   eval      TipVid  = '5'
256800130806e   1C                   endif
256900080218      *
257000080218     c                   ENDSR
257100080218      *---------------------------------------------------------------*
257200080218      * Controllo e decodifica quarta videata                         *
257300080218      *---------------------------------------------------------------*
257400080218     c     CtrD04        BEGSR
257500130708
257600150210     C                   setoff                                       7374
257700130708     c                   clear                   ricmaz
257800130708     c                   clear                   ricgorg
257900130708     c                   clear                   ricmps
258000130708     c                   clear                   ricpwd
258100150210     c                   clear                   riccgb
258200150210     c                   clear                   ricmgb
258300160808     c                   clear                   riccanc
258400150212     c                   clear                   ricipt
258500150618     c                   clear                   ricFUF
258600080218      *
258700080218      * Se sono in visualizzazione spengo il 28
258800080218     c                   if        I31opz = '5'
258900080218     c                   eval      *in28  = *off
259000080218     c                   endif
259100150210      * Autorizzazione anagrafico bancali clienti
259200150210     C     '?'           scan      v4ccgb                                 60
259300150210     C                   clear                   v4dcgb
259400150210 1a  C                   if        *In60 and Not *In28
259500150210     C                   clear                   Tibs02Ds
259600150210     C                   move      'R'           T02Mod
259700150210     C                   movel     KnSif         T02Sif
259800150210     C                   move      'LAT'         T02Cod
259900150210     C                   CALL      'TIBS02R'
260000150210     C                   PARM                    kpjba
260100150210     C                   PARM                    Tibs02Ds
260200150210     C                   movel     T02ke1        v4ccgb
260300150210     C                   movel     T02uni        v4dcgb
260400150210     C                   setoff                                       60
260500150210 1-  C                   endif
260600150210      *
260700150210      * Non devo eseguire i controlli se richiesta ricerca
260800150210 1a  C                   if        Not *In60
260900150210 2a  C                   if        v4ccgb <> *blanks
261000150210      * Se esiste decodifico
261100150210     C                   reset                   tibs02ds
261200150210     C                   movel(p)  v4ccgb        t02ke1
261300150210     C                   call      'TIBS02R'
261400150210     C                   parm                    KPJBA
261500150210     C                   parm                    TIBS02DS
261600150210 3a  C                   if        t02err =  *blanks
261700150210     C                   movel     t02uni        dLAT
261800150210 4a  C     V1CFil        ifeq      '046'
261900150210     C     §LatUus       andne     'S'
262000150210     C  n28              eval      V1Dmsg = Msg(18)
262100150210     C  n28              seton                                        2873
262200150210     C                   else
262300150210     C                   movel     §LatDes       v4dcgb
262400150210 4-  C                   endif
262500150210 3e  C                   else
262600150210     C  n28              eval      V1Dmsg = Msg(12)
262700150210     C  n28              seton                                        2873
262800150210 3-  C                   endif
262900150210 2-  C                   endif
263000150210 1-  C                   endif
263100150210      * Autorizzazione movimenti bancali clienti
263200150210     C     '?'           scan      v4cmgb                                 60
263300150210     C                   clear                   v4dmgb
263400150210 1a  C                   if        *In60 and Not *In28
263500150210     C                   clear                   Tibs02Ds
263600150210     C                   move      'R'           T02Mod
263700150210     C                   movel     KnSif         T02Sif
263800150210     C                   move      'LAT'         T02Cod
263900150210     C                   CALL      'TIBS02R'
264000150210     C                   PARM                    kpjba
264100150210     C                   PARM                    Tibs02Ds
264200150210     C                   movel     T02ke1        v4cmgb
264300150210     C                   movel     T02uni        v4dmgb
264400150210     C                   setoff                                       60
264500150210 1-  C                   endif
264600150210      *
264700150210      * Non devo eseguire i controlli se richiesta ricerca
264800150210 1a  C                   if        Not *In60
264900150210 2a  C                   if        v4cmgb <> *blanks
265000150210      * Se esiste decodifico
265100150210     C                   reset                   tibs02ds
265200150210     C                   movel(p)  v4cmgb        t02ke1
265300150210     C                   call      'TIBS02R'
265400150210     C                   parm                    KPJBA
265500150210     C                   parm                    TIBS02DS
265600150210 3a  C                   if        t02err =  *blanks
265700150210     C                   movel     t02uni        dLAT
265800150210 4a  C     V1CFil        ifeq      '046'
265900150210     C     §LatUus       andne     'S'
266000150210     C  n28              eval      V1Dmsg = Msg(18)
266100150210     C  n28              seton                                        2874
266200150210     C                   else
266300150210     C                   movel     §LatDes       v4dmgb
266400150210 4-  C                   endif
266500150210 3e  C                   else
266600150210     C  n28              eval      V1Dmsg = Msg(12)
266700150210     C  n28              seton                                        2874
266800150210 3-  C                   endif
266900150210 2-  C                   endif
267000150210 1-  C                   endif
267100150212      * Autorizzazione ip terminale
267200150212     C     '?'           scan      v4cipt                                 60
267300150212     C                   clear                   v4dipt
267400150212 1a  C                   if        *In60 and Not *In28
267500150212     C                   clear                   Tibs02Ds
267600150212     C                   move      'R'           T02Mod
267700150212     C                   movel     KnSif         T02Sif
267800150212     C                   move      'LAT'         T02Cod
267900150212     C                   CALL      'TIBS02R'
268000150212     C                   PARM                    kpjba
268100150212     C                   PARM                    Tibs02Ds
268200150212     C                   movel     T02ke1        v4cipt
268300150212     C                   movel     T02uni        v4dipt
268400150212     C                   setoff                                       60
268500150212 1-  C                   endif
268600150212      *
268700150212      * Non devo eseguire i controlli se richiesta ricerca
268800150212 1a  C                   if        Not *In60
268900150212 2a  C                   if        v4cipt <> *blanks
269000150212      * Se esiste decodifico
269100150212     C                   reset                   tibs02ds
269200150212     C                   movel(p)  v4cipt        t02ke1
269300150212     C                   call      'TIBS02R'
269400150212     C                   parm                    KPJBA
269500150212     C                   parm                    TIBS02DS
269600150212 3a  C                   if        t02err =  *blanks
269700150212     C                   movel     t02uni        dLAT
269800150212 4a  C     V1CFil        ifeq      '046'
269900150212     C     §LatUus       andne     'S'
270000150212     C  n28              eval      V1Dmsg = Msg(18)
270100150212     C  n28              seton                                        2875
270200150212     C                   else
270300150212     C                   movel     §LatDes       v4dipt
270400150212 4-  C                   endif
270500150212 3e  C                   else
270600150212     C  n28              eval      V1Dmsg = Msg(12)
270700150212     C  n28              seton                                        2875
270800150212 3-  C                   endif
270900150212 2-  C                   endif
271000150212 1-  C                   endif
271100150618      *
271200150618      * -?Filiale dell'Ufficio?
271300150618      * ·?Controllo (se NON richiesta la Ricerca)?
271400150618     c                   if        V4Cfuf = *zeros
271500150618     c                   clear                   V4Cfuf
271600150618     c                   endif
271700150618     c                   if        V4Cfuf = *blanks
271800150618     c                   clear                   V4Dfuf
271900150618     c                   endif
272000150618if  1c                   If        %scan( '?' : V4Cfuf ) = *zero
272100150618     c                             and  V4Cfuf <> *blanks
272200150618     c                   move      V4Cfuf        kFil
272300150618     c     KFil          chain     AZORG
272400150618if  2c                   if        %found(AZORG01L)
272500150618     c                   movel     ORGfil        V4Cfuf
272600150618     c                   movel     ORGdes        V4Dfuf
272700150618x   2c                   else
272800150618     c  n28              eval      V1Dmsg = Msg(26)
272900150618     c  n28              seton                                        2877
273000150618e   2c                   endif
273100150618e   1c                   EndIf
273200150618      * ·?Ricerca (se NON già rilevati Errori)?
273300150618if  1c                   If        NOT  *in28  and
273400150618     c                             %scan( '?' : V4Cfuf ) > *zero
273500150618     c                   clear                   V4Dfuf
273600150618     c                   clear                   §Cod01
273700150618     c                   clear                   §Des1
273800150618     c                   clear                   §Pra
273900150618     c                   call      'TNSD24R'
274000150618     c                   parm                    §Cod01            3
274100150618     c                   parm                    §Tip              1
274200150618     c                   parm                    §Des1            25
274300150618     c                   parm                    §Pra              3
274400150618     c                   movel     §Cod01        V4Cfuf
274500150618     c                   movel     §Des1         V4Dfuf
274600150618e   1c                   EndIf
274700150618      *
274800130708      /free
274900130708        // confronto con lo STD
275000150212         IF  V4Cipt <> V4Sipt and V1Cmopr <> 'S' and wOKuteSTD;
275100150212           ricipt = rimm;
275200150212         ENDIF;
275300150618         IF  V4Cfuf <> V4Sfuf and V1Cmopr <> 'S' and wOKuteSTD;
275400150618           ricFUF = rimm;
275500150618         ENDIF;
275600150210         IF  V4Ccgb <> V4Scgb and V1Cmopr <> 'S' and wOKuteSTD;
275700150210           riccgb = rimm;
275800150210         ENDIF;
275900150210         IF  V4Cmgb <> V4Smgb and V1Cmopr <> 'S' and wOKuteSTD;
276000150210           ricmgb = rimm;
276100150210         ENDIF;
276200160808         IF  V4Ccanc <> V4Scanc and V1Cmopr <> 'S' and wOKuteSTD;
276300160808           riccanc = rimm;
276400160808         ENDIF;
276500150210         IF  V4Cmaz <> V4Smaz and V1Cmopr <> 'S' and wOKuteSTD;
276600150210           ricmaz = rimm;
276700150210         ENDIF;
276800130806         IF  V4Cgorg <> V4Sgorg and V1Cmopr <> 'S' and wOKuteSTD;
276900130708           ricgorg = rimm;
277000130708         ENDIF;
277100130806         IF  V4Cmps <> V4Smps and V1Cmopr <> 'S' and wOKuteSTD;
277200130708           ricmps = rimm;
277300130708         ENDIF;
277400130806         IF  V4Cpwd <> V4Spwd and V1Cmopr <> 'S' and wOKuteSTD;
277500131021           ricpwd = rimm;
277600131021         ENDIF;
277700131021      /end-free
277800131021
277900131021     c                   ENDSR
278000131021      *---------------------------------------------------------------*
278100131021      * GesD05 - Gestione quinta videata -                            *
278200131021      *---------------------------------------------------------------*
278300131021     c     GesD05        BEGSR
278400131021      *
278500131021      * Emissione testata
278600130806     c                   write     Bs31T01
278700130806     c                   write     Bs31T02
278800130806      *
278900130806      * Se esistono errori sulla videata
279000130806      *   emetto la videata ad indicatori spenti per visualizzare le
279100130806      *   eventuali decodifiche
279200130806if  1c                   if        *in28
279300130806     c                   movea     *in           Win
279400130806     c                   move      *all'0'       in2841
279500130806     c                   movea     in2841        *in(28)
279600130806     c                   write     Bs31D05
279700130806     c                   movea     Win           *in
279800130806e   1c                   endif
279900130806      *
280000130806if  1c                   if        I31opz = '5'
280100130806      * Visualizzazione
280200130806     c                   write     Bs31D05
280300130806     c                   exfmt     PROTECT
280400130806x   1c                   else
280500130806      *
280600130806      * Immissione/Modifica/Copia
280700130806     c                   exfmt     Bs31D05
280800130806e   1c                   endif
280900130806      *
281000130806     c                   setoff                                       28  90
281100130806     c                   clear                   V1Dmsg
281200130806      *
281300130806sel 1c                   SELECT
281400130806      * F3=Fine
281500130806w   1c                   WHEN      *inKC
281600130806     c                   exsr      F03Ges
281700130806     c                   leavesr
281800130806      * F8=Modulo_Base + AS/400 del profilo
281900130806w   1c                   WHEN      *inKH
282000130806     c                   exsr      CallTIBS32
282100130806     c                   leavesr
282200130806      * F12=Ritorno
282300130806w   1c                   WHEN      *inKL
282400130806     c                   exsr      F12Ges
282500130806     c                   leavesr
282600161006      * F18=Note
282700161006w   1c                   WHEN      *inKS
282800161006     c                   exsr      sr_CallTIBS43
282900161006     c                   leavesr
283000130806      *
283100130806e   1c                   ENDSL
283200130806      *
283300130806      * Controlli e decodifiche
283400130806     c                   exsr      CtrD05
283500130927      *
283600130927if  1C                   if             *in28       = *off
283700130927     C                             and  $PostSearch = *off
283800130927      * Carico la quinta videata
283900130927     C                   eval      TipVid  = '6'
284000130927e   1C                   endif
284100130806      *
284200130806      *
284300130806     c                   ENDSR
284400130806      *---------------------------------------------------------------*
284500130806      * Controllo e decodifica quinta videata                         *
284600130806      *---------------------------------------------------------------*
284700130806     c     CtrD05        BEGSR
284800130806
284900130806     c                   clear                   ricpswaut
285000130806     c                   clear                   ricpswmag
285100130806     c                   clear                   ricpswr
285200130806     c                   clear                   ricgdcksc
285300130806     c                   clear                   ricvrac
285400150317     c                   clear                   ricrac
285500150810     c                   clear                   ricvadd
285600150810     c                   clear                   ricadd
285700130806     c                   clear                   ricmresca
285800130806      *
285900130806     c                   setoff                                       575859
286000150317     c                   setoff                                       76
286100130806      * Se sono in visualizzazione spengo il 28
286200130806     c                   if        I31opz = '5'
286300130806     c                   eval      *in28  = *off
286400130806     c                   endif
286500130806
286600130806      * Controllo autorizzazioni
286700130806
286800130806      * ricerca utente bartaut
286900130806     c                   clear                   v4dpswaut
287000130806     c                   if        %scan('?':v4cpswaut) > *zeros
287100130806     c                   clear                   tibs02ds
287200130806     c                   eval      t02mod = 'R'
287300130806     c                   eval      t02sif = knsif
287400130806     c                   eval      t02cod = 'LAT'
287500130806     c                   call      'TIBS02R'
287600130806     c                   parm                    kpjba
287700130806     c                   parm                    tibs02ds
287800130806     c                   eval      v4cpswaut = t02ke1
287900130806     c                   eval      v4dpswaut = t02uni
288000130806 1-  c                   endif
288100130806      * obbligatorio
288200130806 2a  c                   if        v4cpswaut = *blanks
288300130806     c                   eval      v1dmsg = msg(20)
288400130806     c                   seton                                        2857
288500130806     c                   leavesr
288600130806 1-  c                   endif
288700130806      * controllo
288800130806     c                   reset                   tibs02ds
288900130806     c                   eval      t02ke1 = v4cpswaut
289000130806     c                   call      'TIBS02R'
289100130806     c                   parm                    kpjba
289200130806     c                   parm                    tibs02ds
289300130806      * errore: tabella
289400130806 3a  c                   if        t02err <> *blanks
289500130806     c                   eval      v1dmsg = msg(12)
289600130806     c                   seton                                        2857
289700130806     c                   leavesr
289800130806 3-  c                   endif
289900130806      * errore: non per utenti di sede
290000130806     c                   eval      dlat = t02uni
290100130806 4a  c                   if        v1cfil = '046' and
290200130806     c                             §latuus <> 'S'
290300130806     c                   eval      v1dmsg = msg(18)
290400130806     c                   seton                                        2857
290500130806     c                   leavesr
290600130806     c                   endif
290700130806     c                   eval      v4dpswaut = §latdes
290800130806      /free
290900130806        // confronto con lo STD
291000130806         IF  V4Cpswaut <> V4Spswaut and V1Cmopr <> 'S' and wOKuteSTD;
291100130806           ricpswaut = rimm;
291200130806         ENDIF;
291300130806      /end-free
291400130806
291500130806      * ricerca utente bartmag
291600130806     c                   clear                   v4dpswmag
291700130806     c                   if        %scan('?':v4cpswmag) > *zeros
291800130806     c                   clear                   tibs02ds
291900130806     c                   eval      t02mod = 'R'
292000130806     c                   eval      t02sif = knsif
292100130806     c                   eval      t02cod = 'LAT'
292200130806     c                   call      'TIBS02R'
292300130806     c                   parm                    kpjba
292400130806     c                   parm                    tibs02ds
292500130806     c                   eval      v4cpswmag = t02ke1
292600130806     c                   eval      v4dpswmag = t02uni
292700130806 1-  c                   endif
292800130806      * obbligatorio
292900130806 2a  c                   if        v4cpswmag = *blanks
293000130806     c                   eval      v1dmsg = msg(20)
293100130806     c                   seton                                        2858
293200130806     c                   leavesr
293300130806 1-  c                   endif
293400130806      * controllo
293500130806     c                   reset                   tibs02ds
293600130806     c                   eval      t02ke1 = v4cpswmag
293700130806     c                   call      'TIBS02R'
293800130806     c                   parm                    kpjba
293900130806     c                   parm                    tibs02ds
294000130806      * errore: tabella
294100130806 3a  c                   if        t02err <> *blanks
294200130806     c                   eval      v1dmsg = msg(12)
294300130806     c                   seton                                        2858
294400130806     c                   leavesr
294500130806 3-  c                   endif
294600130806      * errore: non per utenti di sede
294700130806     c                   eval      dlat = t02uni
294800130806 4a  c                   if        v1cfil = '046' and
294900130806     c                             §latuus <> 'S'
295000130806     c                   eval      v1dmsg = msg(18)
295100130806     c                   seton                                        2858
295200130806     c                   leavesr
295300130806     c                   endif
295400130806     c                   eval      v4dpswmag = §latdes
295500130806      /free
295600130806        // confronto con lo STD
295700130806         IF  V4Cpswmag <> V4Spswmag and V1Cmopr <> 'S' and wOKuteSTD;
295800130806           ricpswmag = rimm;
295900130806         ENDIF;
296000130806      /end-free
296100130806
296200130806      * ricerca altri utenti
296300130806     c                   clear                   v4dpswr
296400130806     c                   if        %scan('?':v4cpswr) > *zeros
296500130806     c                   clear                   tibs02ds
296600130806     c                   eval      t02mod = 'R'
296700130806     c                   eval      t02sif = knsif
296800130806     c                   eval      t02cod = 'LAT'
296900130806     c                   call      'TIBS02R'
297000130806     c                   parm                    kpjba
297100130806     c                   parm                    tibs02ds
297200130806     c                   eval      v4cpswr = t02ke1
297300130806     c                   eval      v4dpswr = t02uni
297400130806 1-  c                   endif
297500130806      * obbligatorio
297600130806 2a  c                   if        v4cpswr = *blanks
297700130806     c                   eval      v1dmsg = msg(20)
297800130806     c                   seton                                        2859
297900130806     c                   leavesr
298000130806 1-  c                   endif
298100130806      * controllo
298200130806     c                   reset                   tibs02ds
298300130806     c                   eval      t02ke1 = v4cpswr
298400130806     c                   call      'TIBS02R'
298500130806     c                   parm                    kpjba
298600130806     c                   parm                    tibs02ds
298700130806      * errore: tabella
298800130806 3a  c                   if        t02err <> *blanks
298900130806     c                   eval      v1dmsg = msg(12)
299000130806     c                   seton                                        2859
299100130806     c                   leavesr
299200130806 3-  c                   endif
299300130806      * errore: non per utenti di sede
299400130806     c                   eval      dlat = t02uni
299500130806 4a  c                   if        v1cfil = '046' and
299600130806     c                             §latuus <> 'S'
299700130806     c                   eval      v1dmsg = msg(18)
299800130806     c                   seton                                        2859
299900130806     c                   leavesr
300000130806     c                   endif
300100130806     c                   eval      v4dpswr = §latdes
300200130806      /free
300300130806        // confronto con lo STD
300400130806         IF  V4Cpswr <> V4Spswr and V1Cmopr <> 'S' and wOKuteSTD;
300500130806           ricpswr = rimm;
300600130806         ENDIF;
300700130806         IF  V4gdcksc <> V4Sgdcksc and V1Cmopr <> 'S' and wOKuteSTD;
300800130806           ricgdcksc = rimm;
300900130806         ENDIF;
301000150317
301100130806         IF  V5cvrac <> V5Svrac and V1Cmopr <> 'S' and wOKuteSTD;
301200130806           ricvrac = rimm;
301300130806         ENDIF;
301400150317
301500150317       //?Abilitazione Manutenzione RA
301600150317         clear V5Drac;
301700150317         IF  %scan('?':V5Crac) > 0;
301800150317           clear TIBS02DS;
301900150317           T02mod = 'R';
302000150317           T02sif = knsif;
302100150317           T02cod = 'LAT';
302200150317           TNTBE_RicercaControllo (kpjba : TIBS02DS);
302300150317           V5Crac = T02ke1;
302400150317           dLAT = T02uni;
302500150317           V5Drac = §LATdes;
302600150317         ENDIF;
302700150317
302800150810        if v5crac <> *blank;
302900150317         reset TIBS02DS;
303000150317         T02ke1 = V5Crac;
303100150317         TNTBE_RicercaControllo (kpjba : TIBS02DS);
303200150317         IF  T02err <> *blanks;
303300150317           V1Dmsg = Msg(12);
303400150317           *in28 = *on;
303500150317           *in76 = *on;
303600150317           leavesr;
303700150317         ENDIF;
303800150317         dLAT = T02uni;
303900150317         V5Drac = §LATdes;
304000150317         IF  V1Cfil = '046' and §LATuus <> 'S';
304100150317           V1Dmsg = Msg(18);
304200150317           *in28 = *on;
304300150317           *in76 = *on;
304400150317           leavesr;
304500150317         ENDIF;
304600150810         ENDIF;
304700150317
304800150317         IF  V5Crac <> V5Srac and V1Cmopr <> 'S' and wOKuteSTD;
304900150317           ricrac = rimm;
305000150317         ENDIF;
305100150810
305200150810         IF  V5cvadd <> V5Svadd and V1Cmopr <> 'S' and wOKuteSTD;
305300150810           ricvadd = rimm;
305400150810         ENDIF;
305500150810       //?Abilitazione addebiti CE
305600150810         clear V5Dadd;
305700150810         IF  %scan('?':V5Cadd) > 0;
305800150810           clear TIBS02DS;
305900150810           T02mod = 'R';
306000150810           T02sif = knsif;
306100150810           T02cod = 'LAT';
306200150810           TNTBE_RicercaControllo (kpjba : TIBS02DS);
306300150810           V5Cadd = T02ke1;
306400150810           dLAT = T02uni;
306500150810           V5Dadd = §LATdes;
306600150810         ENDIF;
306700150810
306800150810
306900150810         IF  V5Cadd <> *blanks;
307000150810         reset TIBS02DS;
307100150810         T02ke1 = V5Cadd;
307200150810         TNTBE_RicercaControllo (kpjba : TIBS02DS);
307300150810         IF  T02err <> *blanks;
307400150810           V1Dmsg = Msg(12);
307500150810           *in28 = *on;
307600150810           *in78 = *on;
307700150810           leavesr;
307800150810         ENDIF;
307900150810         dLAT = T02uni;
308000150810         V5Dadd = §LATdes;
308100150810         IF  V1Cfil = '046' and §LATuus <> 'S';
308200150810           V1Dmsg = Msg(18);
308300150810           *in28 = *on;
308400150810           *in78 = *on;
308500150810           leavesr;
308600150810         ENDIF;
308700150810         ENDIF;
308800150810
308900150810         IF  V5Cadd <> V5Sadd and V1Cmopr <> 'S' and wOKuteSTD;
309000150810           ricadd = rimm;
309100150810         ENDIF;
309200150317
309300130806         IF  V5cmresca <> V5Smresca and V1Cmopr <> 'S' and wOKuteSTD;
309400130806           ricmresca = rimm;
309500130806         ENDIF;
309600130806      /end-free
309700130806
309800130806     c                   ENDSR
309900130927      *---------------------------------------------------------------*
310000130927      * GesD06 - Gestione quarta videata -                            *
310100130927      *---------------------------------------------------------------*
310200130927     c     GesD06        BEGSR
310300130927      *
310400130927      * Emissione testata
310500130927     c                   write     Bs31T01
310600130927     c                   write     Bs31T02
310700130927      *
310800130927      * Se esistono errori sulla videata
310900130927      *   emetto la videata ad indicatori spenti per visualizzare le
311000130927      *   eventuali decodifiche
311100130927if  1c                   if        *in28
311200130927     c                   movea     *in           Win
311300130927     c                   move      *all'0'       in2841
311400130927     c                   movea     in2841        *in(28)
311500130927     c                   write     Bs31D06
311600130927     c                   movea     Win           *in
311700130927e   1c                   endif
311800130927      *
311900131022if  1c*m                 if        I31opz = '3' and primo = *blank
312000131022     c*m                 clear                   bs31d06
312100131022     c*m                 move      'NO'          V6autautor
312200131022     c*m                 move      'X'           primo             1
312300131022     c*m                 endif
312400131002      * Copia non riporto i flag dal profilo copiato
312500130927if  1c                   if        I31opz = '5'
312600130927      * Visualizzazione
312700130927     c                   write     Bs31D06
312800130927     c                   exfmt     PROTECT
312900130927x   1c                   else
313000130927      *
313100130927      * Immissione/Modifica/Copia
313200130927     c                   exfmt     Bs31D06
313300130927e   1c                   endif
313400130927      *
313500130927     c                   setoff                                       28  90
313600130927     c                   clear                   V1Dmsg
313700130927      *
313800130927sel 1c                   SELECT
313900130927      * F3=Fine
314000130927w   1c                   WHEN      *inKC
314100130927     c                   exsr      F03Ges
314200130927     c                   leavesr
314300130927      * F8=Modulo_Base + AS/400 del profilo
314400130927w   1c                   WHEN      *inKH
314500130927     c                   exsr      CallTIBS32
314600130927     c                   leavesr
314700130927      * F12=Ritorno
314800130927w   1c                   WHEN      *inKL
314900130927     c                   exsr      F12Ges
315000130927     c                   leavesr
315100161006      * F18=Note
315200161006w   1c                   WHEN      *inKS
315300161006     c                   exsr      sr_CallTIBS43
315400161006     c                   leavesr
315500130927      *
315600130927e   1c                   ENDSL
315700130927      *
315800130927      * Controlli e decodifiche
315900130927     c                   exsr      CtrD06
316000130927      *
316100130927      * Eseguo conferma
316200130927if  1c                   if             *inKF
316300130927     c                             and  NOT *in28
316400130927      *
316500130927      * Eseguo aggiornamento
316600130927     c                   exsr      AggUte
316700130927      *
316800130927      * eseguo operazioni del dopo-aggiornamento
316900130927if  2c                   if        NOT *in28
317000130927     c                   exsr      GesAGG
317100130927e   2c                   endif
317200130927      *
317300130927e   1c                   endif
317400130927      * Richiamo la gestione "tecnica" (Modulo_Base+AS/400) del profilo
317500130927if  1c                   if             *in28  = *off
317600130927     c                             and  *inKF  = *on
317700130927     c                             and (I31opz = 'I'
317800130927     c                              or  I31opz = '3')
317900130927     c                   exsr      CallTIBS32
318000130927e   1c                   endif
318100130927      *
318200130927     c                   ENDSR
318300130927      *---------------------------------------------------------------*
318400130927      * Controllo e decodifica quarta videata                         *
318500130927      *---------------------------------------------------------------*
318600130927     c     CtrD06        BEGSR
318700130927
318800130927      * Se sono in visualizzazione spengo il 28
318900130927     c                   if        I31opz = '5'
319000130927     c                   eval      *in28  = *off
319100130927     c                   endif
319200130930      * obbligatorio
319300130930 2a  c                   if        V6AUTAUTOR = *blanks
319400130930     c                   eval      v1dmsg = msg(20)
319500130930     c                   seton                                        2859
319600130930     c                   leavesr
319700130930 1-  c                   endif
319800130930      * ricerca livello autorità
319900130930     C     '?'           scan      v6autautor                             60
320000130930     c                   if        *in60
320100130927     c                   clear                   tibs02ds
320200130927     c                   eval      t02mod = 'R'
320300130927     c                   eval      t02sif = knsif
320400130927     c                   eval      t02cod = 'LAT'
320500130927     c                   call      'TIBS02R'
320600130927     c                   parm                    kpjba
320700130927     c                   parm                    tibs02ds
320800130927     c                   movel     t02ke1        V6AUTAUTOR
320900130930      *
321000130930 1-  c                   else
321100130927      * controllo
321200130927     c                   reset                   tibs02ds
321300130927     c                   eval      t02ke1 = V6AUTAUTOR
321400130927     c                   call      'TIBS02R'
321500130927     c                   parm                    kpjba
321600130927     c                   parm                    tibs02ds
321700130927      * errore: tabella
321800130927 3a  c                   if        t02err <> *blanks
321900130927     c                   eval      v1dmsg = msg(12)
322000130927     c                   seton                                        2859
322100130927     c                   leavesr
322200130927 3-  c                   endif
322300130927      * errore: non per utenti di sede
322400130927     c                   eval      dlat = t02uni
322500130927 4a  c                   if        v1cfil = '046' and
322600130927     c                             §latuus <> 'S'
322700130927     c                   eval      v1dmsg = msg(18)
322800130927     c                   seton                                        2859
322900130927     c                   leavesr
323000130930     c                   endif
323100130927     c                   endif
323200131011     c                   setoff                                       72
323300130930 2a  c                   if        V6AUTAUTOR  = 'NO'
323400130930     c     V6AUTMTC      comp      'N'                                7272
323500130930     c                   if        *in72
323600130930     c                   eval      v1dmsg = msg(23)
323700130930     c                   seton                                        2859
323800130930     c                   leavesr
323900130930     c                   endif
324000130930     c     V6AFFMTC      comp      'N'                                7272
324100130930     c                   if        *in72
324200130930     c                   eval      v1dmsg = msg(23)
324300130930     c                   seton                                        2859
324400130930     c                   leavesr
324500130930     c                   endif
324600130930     c     V6AUTRET      comp      'N'                                7272
324700130930     c                   if        *in72
324800130930     c                   eval      v1dmsg = msg(23)
324900130930     c                   seton                                        2859
325000130930     c                   leavesr
325100130930     c                   endif
325200130930     c     V6AFFGVSTD    comp      'N'                                7272
325300130930     c                   if        *in72
325400130930     c                   eval      v1dmsg = msg(23)
325500130930     c                   seton                                        2859
325600130930     c                   leavesr
325700130930     c                   endif
325800130930     c     V6AUTCON      comp      'N'                                7272
325900130930     c                   if        *in72
326000130930     c                   eval      v1dmsg = msg(23)
326100130930     c                   seton                                        2859
326200130930     c                   leavesr
326300130930     c                   endif
326400130930     c     V6AFFAGV      comp      'N'                                7272
326500130930     c                   if        *in72
326600130930     c                   eval      v1dmsg = msg(23)
326700130930     c                   seton                                        2859
326800130930     c                   leavesr
326900130930     c                   endif
327000130930     c     V6AUTCRS      comp      'N'                                7272
327100130930     c                   if        *in72
327200130930     c                   eval      v1dmsg = msg(23)
327300130930     c                   seton                                        2859
327400130930     c                   leavesr
327500130930     c                   endif
327600130930     c     V6AFFCPSTD    comp      'N'                                7272
327700130930     c                   if        *in72
327800130930     c                   eval      v1dmsg = msg(23)
327900130930     c                   seton                                        2859
328000130930     c                   leavesr
328100130930     c                   endif
328200130930     c     V6AUTSTDCC    comp      'N'                                7272
328300130930     c                   if        *in72
328400130930     c                   eval      v1dmsg = msg(23)
328500130930     c                   seton                                        2859
328600130930     c                   leavesr
328700130930     c                   endif
328800130930     c     V6AFFARRM     comp      'N'                                7272
328900130930     c                   if        *in72
329000130930     c                   eval      v1dmsg = msg(23)
329100130930     c                   seton                                        2859
329200130930     c                   leavesr
329300130930     c                   endif
329400130930     c     V6RIEABI      comp      'N'                                7272
329500130930     c                   if        *in72
329600130930     c                   eval      v1dmsg = msg(23)
329700130930     c                   seton                                        2859
329800130930     c                   leavesr
329900130930     c                   endif
330000130930     c     V6AFFCON      comp      'N'                                7272
330100130930     c                   if        *in72
330200130930     c                   eval      v1dmsg = msg(23)
330300130930     c                   seton                                        2859
330400130930     c                   leavesr
330500130930     c                   endif
330600130930     c     V6AUTRFCIT    comp      'N'                                7272
330700130930     c                   if        *in72
330800130930     c                   eval      v1dmsg = msg(23)
330900130930     c                   seton                                        2859
331000130930     c                   leavesr
331100130930     c                   endif
331200130930     c     V6AUTRFCOP    comp      'N'                                7272
331300130930     c                   if        *in72
331400130930     c                   eval      v1dmsg = msg(23)
331500130930     c                   seton                                        2859
331600130930     c                   leavesr
331700130930     c                   endif
331800130930     c     V6AUTSDT      comp      'N'                                7272
331900130930     c                   if        *in72
332000130930     c                   eval      v1dmsg = msg(23)
332100130930     c                   seton                                        2859
332200130930     c                   leavesr
332300130930     c                   endif
332400130930     c     V6AFFCRS      comp      'N'                                7272
332500130930     c                   if        *in72
332600130930     c                   eval      v1dmsg = msg(23)
332700130930     c                   seton                                        2859
332800130930     c                   leavesr
332900130930     c                   endif
333000130930     c     V6AUTMTCC     comp      'N'                                7272
333100130930     c                   if        *in72
333200130930     c                   eval      v1dmsg = msg(23)
333300130930     c                   seton                                        2859
333400130930     c                   leavesr
333500130930     c                   endif
333600130930     c     V6AUTVTCC     comp      'N'                                7272
333700130930     c                   if        *in72
333800130930     c                   eval      v1dmsg = msg(23)
333900130930     c                   seton                                        2859
334000130930     c                   leavesr
334100130930     c                   endif
334200130930     c     V6AUTCPRRE    comp      'N'                                7272
334300130930     c                   if        *in72
334400130930     c                   eval      v1dmsg = msg(23)
334500130930     c                   seton                                        2859
334600130930     c                   leavesr
334700130930     c                   endif
334800130930     c     V6AUTSDTC     comp      'N'                                7272
334900130930     c                   if        *in72
335000130930     c                   eval      v1dmsg = msg(23)
335100130930     c                   seton                                        2859
335200130930     c                   leavesr
335300130930     c                   endif
335400130930     c     V6AUTRFPRE    comp      'N'                                7272
335500130930     c                   if        *in72
335600130930     c                   eval      v1dmsg = msg(23)
335700130930     c                   seton                                        2859
335800130930     c                   leavesr
335900130930     c                   endif
336000130930     c     V6AUTRETC     comp      'N'                                7272
336100130930     c                   if        *in72
336200130930     c                   eval      v1dmsg = msg(23)
336300130930     c                   seton                                        2859
336400130930     c                   leavesr
336500130930     c                   endif
336600130930     c     V6AUTCONC     comp      'N'                                7272
336700130930     c                   if        *in72
336800130930     c                   eval      v1dmsg = msg(23)
336900130930     c                   seton                                        2859
337000130930     c                   leavesr
337100130930     c                   endif
337200130930     c     V6AUTRFAFD    comp      'N'                                7272
337300130930     c                   if        *in72
337400130930     c                   eval      v1dmsg = msg(23)
337500130930     c                   seton                                        2859
337600130930     c                   leavesr
337700130930     c                   endif
337800130930     c     V6AUTCRSC     comp      'N'                                7272
337900130930     c                   if        *in72
338000130930     c                   eval      v1dmsg = msg(23)
338100130930     c                   seton                                        2859
338200130930     c                   leavesr
338300130930     c                   endif
338400130930     c                   else
338500130930     c     V6AUTMTC      comp      'S'                                    72
338600131014     c  n72V6AFFMTC      comp      'S'                                    72
338700130930     c  n72V6AUTRET      comp      'S'                                    72
338800130930     c  n72V6AFFGVSTD    comp      'S'                                    72
338900130930     c  n72V6AUTCON      comp      'S'                                    72
339000130930     c  n72V6AFFAGV      comp      'S'                                    72
339100130930     c  n72V6AUTCRS      comp      'S'                                    72
339200130930     c  n72V6AFFCPSTD    comp      'S'                                    72
339300130930     c  n72V6AUTSTDCC    comp      'S'                                    72
339400130930     c  n72V6AFFARRM     comp      'S'                                    72
339500130930     c  n72V6RIEABI      comp      'S'                                    72
339600130930     c  n72V6AFFCON      comp      'S'                                    72
339700130930     c  n72V6AUTRFCIT    comp      'S'                                    72
339800130930     c  n72V6AUTRFCOP    comp      'S'                                    72
339900130930     c  n72V6AUTSDT      comp      'S'                                    72
340000130930     c  n72V6AFFCRS      comp      'S'                                    72
340100130930     c  n72V6AUTMTCC     comp      'S'                                    72
340200130930     c  n72V6AUTVTCC     comp      'S'                                    72
340300130930     c  n72V6AUTCPRRE    comp      'S'                                    72
340400130930     c  n72V6AUTSDTC     comp      'S'                                    72
340500130930     c  n72V6AUTRFPRE    comp      'S'                                    72
340600130930     c  n72V6AUTRETC     comp      'S'                                    72
340700130930     c  n72V6AUTCONC     comp      'S'                                    72
340800130930     c  n72V6AUTRFAFD    comp      'S'                                    72
340900130930     c  n72V6AUTCRSC     comp      'S'                                    72
341000130930     c                   if        not *in72
341100130930     c                   eval      v1dmsg = msg(24)
341200130930     c                   seton                                        2859
341300130930     c                   leavesr
341400130930     c                   endif
341500130930      *
341600130930     c                   endif
341700131014      *
341800131014     c                   if        v1cfil <> '046' and
341900131014     c                             (v6autSDT ='S' or
342000131014     c                             v6autcrsc ='S' or
342100131014     c                             v6AFFCRS ='S')
342200131014     c                   eval      v1dmsg = msg(25)
342300131014     c                   seton                                        2859
342400131014     c                   leavesr
342500131014     c                   endif
342600131014      *
342700130927
342800130927     c                   ENDSR
342900080530      *---------------------------------------------------------------*
343000080530      * GesW01 - Gestione prima window -                              *
343100080530      *---------------------------------------------------------------*
343200080530     c     GesW01        BEGSR
343300080530      *
343400080530      * Emissione window
343500080530     c                   exfmt     BS31W01
343600080530      *
343700080530      * F12=Ritorno
343800080530if  1c                   if        *inKL
343900080530     c                   clear                   V4Cpwdint
344000080530     c                   exsr      F12Ges
344100080530     c                   endif
344200080530      *
344300080530     c                   ENDSR
344400100114      /free
344500100114       //--------------------------------------------------------------
344600100114       //?Gestione videata per la manutenzione del "Modulo Profilo"    ?
344700100114       //--------------------------------------------------------------
344800100114       BEGSR  sr_GesW02;
344900100114
345000100114         //?Inizializzazione videata?
345100100114         if  $InzW02 = *on;
345200100114           exsr  sr_InzW02;
345300100114         endif;
345400100114
345500100114         //?Emissione window?
345600100114         exfmt  BS31W02;
345700100114
345800100114         select;
345900100115
346000100115           //?F3=Fine?
346100100115           when *inKC;
346200100115             unlock  KPPRF01L;
346300100115             unlock  AZUTE01L;
346400100115             clear  O31err;
346500100115             clear  O31msg;
346600100115             O31tfu = 'F03';
346700100115             $Fine  = 'S';
346800100114
346900100114           //?F12=Ritorno?
347000100114           when *inKL;
347100100114             unlock  KPPRF01L;
347200100115             unlock  AZUTE01L;
347300100114             clear  O31err;
347400100114             clear  O31msg;
347500100114             O31tfu = 'F12';
347600100115             $Fine  = 'S';
347700100114
347800100114           //?Enter o F6=Conferma?
347900100114           other;
348000100114             exsr  sr_CtrW02;
348100100114             if  *inKF   and   NOT *in28;
348200100114               exsr  sr_F06W02;
348300100115               if  BS31err = *blank;
348400100114                 clear  O31err;
348500100114                 clear  O31msg;
348600100114                 O31tfu = 'F06';
348700100115                 $Fine  = 'S';
348800100114               endif;
348900100114             endif;
349000100114
349100100114         endsl;
349200100114
349300100114         KPJBU = TIBS31ds;
349400100114
349500100114       ENDSR;
349600100114
349700100114       //--------------------------------------------------------------
349800100114       //?Inizializzazione videata x la manutenz. del "Modulo Profilo"
349900100114       //--------------------------------------------------------------
350000100114       BEGSR  sr_InzW02;
350100100114
350200100114         // - Reperimento dati da KPPRF00F
350300100115         chain  (I31ute)  KPPRF000;
350400100114         if  Not %found(KPPRF01L);
350500100114           clear  PRFtxt;
350600100114         endif;
350700100114
350800100114         // - Reperimento dati da AZUTE00F
350900100114         clear  dUte02;
351000100114         $Ute = I31Ute;
351100100114         $Mod = '1';
351200100114         exsr  ChnUte;
351300100114         if  %found(AZUTE01L);
351400100114           dUte02 = UteFa1;
351500100114         endif;
351600100114
351700100114
351800100115         //clear  BS31W02;        (NO: emessi anche dati della KPJBA)
351900100114
352000100115         W2Cute  = I31ute;
352100100114         W2Cmopr = §UT2mopr;
352200100114         W2Ctxt  = PRFtxt;
352300100114
352400100114         $InzW02 = *off;
352500100114
352600100114       ENDSR;
352700100114
352800100114       //--------------------------------------------------------------
352900100114       //?Controllo dati del "Modulo Profilo"
353000100114       //--------------------------------------------------------------
353100100114       BEGSR  sr_CtrW02;
353200100114
353300100114         *in68 = *off;
353400100114
353500100114         if  W2Ctxt = *blank;
353600100114           *in28 = *on;
353700100114           *in68 = *on;
353800100114           V1Dmsg = Msg(21);
353900100114           leavesr;
354000100114         endif;
354100100114
354200100114       ENDSR;
354300100114
354400100114       //--------------------------------------------------------------
354500100114       //?Gestione conferma per la manutenzione del "Modulo Profilo"
354600100114       //--------------------------------------------------------------
354700100114       BEGSR  sr_F06W02;
354800100114
354900100114         // - Esegue aggiornamento di AZUTE00F
355000100114         clear  dUte02;
355100100114         §UT2mopr = W2Cmopr;
355200100114         UTEfa1   = dUTE02;
355300100114         if  %found(AZUTE01L);
355400100114           //_______________
355500100114           update  AZUTE000;
355600100114           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
355700100114         endif;
355800100114
355900100114         // - Esegue aggiornamento di KNMUS00F
356000100114         PRFtxt = W2Ctxt;
356100100114         if  %found(KPPRF01L);
356200100114           //_______________
356300100114           update  KPPRF000;
356400100114           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
356500100114         endif;
356600100114
356700100114         // - Esegue aggiornamento dei dati AS/400
356800100115         clear BS31err;
356900100115         tibs31c ( I31ute : W2Ctxt : BS31err );
357000100114
357100100115         if  BS31err = 'E';
357200100114           *in28  = *on;
357300100114           V1CMsg = Msg(22);
357400100114           leavesr;
357500100114         endif;
357600100114
357700100114         // - Eseguo operazioni del dopo-aggiornamento
357800100114         exsr  GesAGG;
357900100114
358000100114       ENDSR;
358100100114
358200100114      /end-free
358300011121      *-----------------------------------------------------------
358400011114      * Aggiornamento File Utenti aziendali
358500011121      *-----------------------------------------------------------
358600011114     C     AggUte        BEGSR
358700011114      *
358800011114      * Immissione
358900011203     C     I31Opz        ifeq      'I'
359000011203     C     I31Opz        oreq      '3'
359100011114     C                   EXSR      RieUte
359200011114     C                   WRITE     AzUte000                             22
359300011114     C                   endif
359400011114      *
359500011114      * Modifica
359600011114     C     *in02         Ifeq      *on
359700011114     C                   EXSR      RieUte
359800011114     C                   UPDATE    AzUte000
359900011114     C                   endif
360000011114      *
360100011114     C                   ENDSR
360200011121      *-----------------------------------------------------------
360300011114      * Operazioni dopo aggiornamento
360400011121      *-----------------------------------------------------------
360500011114     C     GesAgg        BEGSR
360600011114      *
360700011115     C                   eval      $Fine ='S'
360800011126      * Segnalo che torno al primo pgm con F6, per impostare
360900011126      * il caricamneto sfl.
361000011203     C                   eval      O31Tfu = 'F06'
361100161006      *
361200161006     c                   exsr      sr_CallTIBS43
361300011115      *
361400011114     C                   ENDSR
361500040729      *-----------------------------------------------------------
361600040729      * Richiamo pgm. TIBS32R
361700040729      *-----------------------------------------------------------
361800040729     c     CallTIBS32    BEGSR
361900040729      *
362000040729     c                   clear                   TIBS32ds
362100040826     c                   if        I31opz = 'I'
362200040729     c                   eval      I32opz = '1'
362300040729     c                   else
362400040729     c                   eval      I32opz = I31opz
362500040729     c                   endif
362600040729     c                   eval      I32ute = V1Cute
362700040826     c                   if        I31opz = '3'
362800040826     c                   eval      I32uco = I31ute
362900040826     c                   endif
363000040729     c                   movel(p)  TIBS32ds      KPJBU
363100040729      *
363200160830if  1c                   If        SDSprm > 2
363300160830     c                   call      'TIBS32R'
363400160830     c                   parm                    KPJBA
363500160830     c                   parm                    TIBS31ds2
363600160830x   1c                   else
363700040729     c                   call      'TIBS32R'
363800040729     c                   parm                    KPJBA
363900160830e   1c                   endif
364000040729      *
364100040729     c                   movel     KPJBU         TIBS32ds
364200040729     c                   if        O32err = 'E'
364300040729     c                   clear                   $Fine
364400040729     c                   eval      *in28  = *on
364500080218     c                   movel     O32msg        V1Dmsg
364600040729     c                   endif
364700040729      *
364800040729     c                   ENDSR
364900161006      *-----------------------------------------------------------
365000161006      * Richiamo pgm. TIBS43R
365100161006      *-----------------------------------------------------------
365200161006     c     sr_CallTIBS43 BEGSR
365300161006      *
365400161006     c                   reset                   TIBS43ds
365500161006     c*//                eval      iBS43tla = 'L'       (già così)?
365600161010     c*//                if        *inKS
365700161010if  1C                   if        I31Opz = '5'
365800161010     c                   eval      iBS43opz = '5'
365900161010     c*//                else
366000161010     c*//                eval      iBS43opz = *blank    (già così)?
366100161010     c                   endif
366200161006     c                   eval      iBS43ute = V1Cute
366300161006     c*//                eval      iBS43trc = *blank    (già così)?
366400161006      *
366500161006     c                   eval      kpjbu    = TIBS43ds
366600161006     c                   call      'TIBS43R'
366700161006     c                   parm                    KPJBA
366800161006     c                   eval      TIBS43ds = kpjbu
366900161018      *
367000161018     c                   if        oBS43err = *on
367100161018     c                   eval      *in28  = *on
367200161018     c                   eval      V1Dmsg = oBS43msg
367300161018     c                   endif
367400161006      *
367500161006     c                   ENDSR
367600011121      *-----------------------------------------------------------
367700011114      * Riempimento file tabella
367800011121      *-----------------------------------------------------------
367900011114     C     RieUte        BEGSR
368000011114      *
368100011114     C                   clear                   azute000
368200011115      * Profilo
368300011120     C                   Eval      UteUte = V1CUte
368400011115      * Data
368500011115     C                   reset                   WLBDA8
368600011115     C                   Z-ADD     *zeros        G08INV
368700020329     C                   Z-ADD     V1Cdsc        G08DAT
368800011115     C                   move      '0'           G08ERR
368900011115     C                   CALL      'XSRDA8'
369000011115     C                   PARM                    WLBDA8
369100020329     C                   Z-ADD     G08INV        Utedsc
369200011115      * Codice utente
369300020329     C                   z-add     V1Ccdu        Utecdu
369400011115      * Filiale
369500011120     C                   move      V1CFil        UteFil
369600011115      * Funzione
369700011120     C                   Eval      UteFun = V1CFun
369800011115      * Pgm iniziale/libreria
369900011120     C                   Eval      UtePgm = V1CPgm
370000011120     C                   Eval      UteLib = V1CLib
370100011115      * Livello autorità
370200011120     C                   Eval      UteAut = V1CAut
370300011115      * Profilo posta AS
370400011120     C                   Eval      UtePas = V1CPas
370500011115      * E-mail
370600011221     C                   clear                   Ema
370700011221     C                   Eval      Ema1   = V1CEmA
370800011221     C                   Eval      Ema2   = V1CEm2
370900011221     C                   Eval      UteEma = Ema
371000011115      * Azienda
371100011120     C                   Eval      UteKdt = V1CKdc
371200011115      * Legame con persona fisica
371300011120     C                   Eval      UteKcd = V1CKcd
371400011121      * Abilitazioni
371500150211     C                   Movel     V2CAuB        §UteMbs
371600020516     C                   Movel     V2CIbs        §UteIbs
371700020402     C                   Movel     V2CIUn        §UteIUn
371800020329     C                   Movel     V2CCUn        §UteCUn
371900020913     C                   Movel     V2CISt        §UteISt
372000080218     C                   Movel     V4CMaz        §UteMaz
372100030709     c                   Movel     V2CIas        §UteIas
372200040603     C                   Movel     V2CDST        §UteDST
372300040603     C                   Movel     V2CDSS        §UteDSS
372400040603     C                   Movel     V2CPRO        §UtePRO
372500050413     C                   Movel     V2CCAP        §UteCAP
372600040922     c                   Movel     v3cgtc        §UteGtc
372700130124     c                   Movel     v3modtar      §UTEmodtar
372800040922     c                   Movel     v3ccar        §UteCar
372900040922     c                   Movel     v3cctc        §UteCtc
373000090423     c                   Movel     v3cdupcli     §Utedupcli
373100041207     c                   Movel     v3cpot        §UtePot
373200090514     c                   Movel     v3cpotst      §UtePotst
373300041207     c                   Movel     v3ccli        §UteCli
373400091006     c                   movel     V3CmntSp      §UteMntSp
373500050609     c                   Movel     v3ceca        §Uteeca
373600060315     c                   Movel     v3ccep        §Utecep
373700060804     c                   Movel     v3cfgia       §Utefgia
373800140403     c                   Movel     v3cabres      §Uteabres
373900070410     c                   movel     v3cflna       §Uteflna
374000100917     c                   movel     V3CPotMsf     §UtePotMsf
374100100917     c                   movel     V3CChgCmm     §UteChgCmm
374200160905     c                   movel     V4CInvPwd     §UteInvPwd
374300100917     c                   Movel     v3cmbf        §Utembf
374400100917     c                   Movel     v3cmtf        §Utemtf
374500100917     c                   Movel     v3cmfcp       §Utemfcp
374600080218     c                   movel     V4Cgorg       §UteGORG
374700080530     c                   movel     V4Cpwd        §UtePWD
374800150212     C                   Movel     V4Cipt        §Uteipt
374900150618     c                   movel     V4Cfuf        §uteFUF
375000080526     c                   movel     V4Cmps        §UteMPS
375100081014     c                   movel     V4Cpswaut     §Utepswaut
375200081014     c                   movel     V4Cpswmag     §Utepswmag
375300081014     c                   movel     V4Cpswr       §Utepswr
375400150210     c                   movel     V4Ccgb        §Utecgb
375500150210     c                   movel     V4Cmgb        §Utemgb
375600160808     c                   movel     V4Ccanc       §Utecanc
375700110429     c                   Movel     v3cstp        §Utestp
375800120613     c                   movel     V3CLPfft      §UteCLPfft
375900160905     c                   movel     V3Cksclog     §UteKscLog
376000120907     c                   movel     V4GDCksc      §UteGDCksc
376100130619     c                   movel     V3Corm        §UTEorm
376200160422     c                   movel     V3Cstmo       §UTEistmo
376300130806     c                   movel     V5Cvrac       §UTEvrac
376400150317     c                   movel     V5Crac        §UTErac
376500130806     c                   movel     V5Cmresca     §UTEmresca
376600150810     c                   movel     V5Cvadd       §UTEaddt
376700150810     c                   movel     V5Cadd        §UTEadd
376800130927     c                   movel     V6autautor    §UTEautor
376900130927     c                   movel     v6AUTMTC      §AUTMTC
377000130927     c                   movel     v6AUTSDT      §AUTSDT
377100130927     c                   movel     v6AUTRET      §AUTRET
377200130927     c                   movel     v6AUTCON      §AUTCON
377300130927     c                   movel     v6AUTCRS      §AUTCRS
377400130927     c                   movel     v6AUTVTCC     §AUTVTCC
377500130927     c                   movel     v6AUTMTCC     §AUTMTCC
377600130927     c                   movel     v6AUTSDTC     §AUTSDTC
377700130927     c                   movel     v6AUTRETC     §AUTRETC
377800130927     c                   movel     v6AUTCRSC     §AUTCRSC
377900130927     c                   movel     v6AUTCONC     §AUTCONC
378000130927     c                   movel     v6RIEABI      §RIEABI
378100130927     c                   movel     v6AFFMTC      §AFFMTC
378200130927     c                   movel     v6AFFCON      §AFFCON
378300130927     c                   movel     v6AFFCRS      §AFFCRS
378400130927     c                   movel     v6AFFGVSTD    §AFFGVSTD
378500130927     c                   movel     v6AFFAGV      §AFFAGV
378600130927     c                   movel     v6AFFCPSTD    §AFFCPSTD
378700130927     c                   movel     v6AFFARRM     §AFFARRM
378800130927     c                   movel     v6AUTCPRRE    §AUTCPRRE
378900130927     c                   movel     v6AUTSTDCC    §AUTSTDCC
379000130927     c                   movel     v6AUTRFCIT    §AUTRFCIT
379100130927     c                   movel     v6AUTRFCOP    §AUTRFCOP
379200130927     c                   movel     v6AUTRFAFD    §AUTRFAFD
379300130927     c                   movel     v6AUTRFPRE    §AUTRFPRE
379400131002     c     ' ':'N'       xlate     uteautds      uteautds
379500130927     c                   movel     uteautds      §UTEdsaut
379600020329     C                   Movel     DUte01        UteFaf
379700100115      * Flags
379800100115     c                   movel     V1Cmopr       §UT2mopr
379900100115     c                   movel     dUte02        UteFa1
380000011114      *
380100011114     C                   ENDSR
380200011114      *--------------------------------------------------------------------------------------------*
380300011114      * Aggancio File Utenti aziendali
380400011114      *--------------------------------------------------------------------------------------------*
380500011218     C     ChnUte        BEGSR
380600011218     C                   setoff                                       3031
380700011210      *
380800011218     C     *IN31         doueq     *OFF
380900011114      *
381000011114      * A seconda del modo richiesto aggancio il file
381100011218      *  Modalità 1:utilizzo v.l AzUte01l, gestisco l'allocazione
381200011114     C     $Mod          ifeq      '1'
381300011218     C     I31Opz        Ifeq      '2'
381400100115     c     I31opz        oreq      '9'
381500011210     C     $Ute          chain     AzUte01l                           3031
381600011218      *
381700011218      * non alloco il record
381800011218     C                   else
381900011218     C     $Ute          chain(n)  AzUte01l                           30
382000011114     C                   endif
382100011218     C                   endif
382200011114      *
382300011218      *  Modalità 2:utilizzo v.l AzUte02l, gestisco il riposizionamento
382400011114     C     $Mod          ifeq      '2'
382500011218     C     K02Ute        chain     AzUte02l                           30
382600011115      *
382700011115      * Riaggancio il record di dettaglio se sono in modifica
382800011203     C     I31Opz        Ifeq      '2'
382900011210     C     I31Ute        chain     AzUte01l                           3031
383000011115     C                   endif
383100011218     C                   endif
383200011115      *
383300011210      * Record allocato
383400011210     C     *in31         ifeq      *on
383500011210     C                   clear                   O31Msg
383600011210     C                   clear                   O31Tfu
383700011211      *
383800011210     C                   move      'E'           O31Err
383900020315     C                   movel     msg(6)        O31Msg
384000011210     C                   movel     'S'           $Fine                          *fine pgm
384100011210     C                   endif
384200011210     C                   enddo
384300011210      *
384400011115     C                   ENDSR
384500011114      *--------------------------------------------------------------------------------------------*
384600011119      * Operazioni iniziali
384700011114      *--------------------------------------------------------------------------------------------*
384800011119     C     *inzsr        BEGSR
384900011114      *
385000011114      * Ricevimento parametri
385100011114     C     *ENTRY        PLIST
385200011114     C                   PARM                    kpjba
385300011203     C                   PARM                    Tibs31Ds
385400160830     c                   parm                    Tibs31Ds2
385500020408      *
385600020408     C                   z-add     1             CODUT
385700020408     C                   call      'X§PARUT'
385800020408     C                   parm                    UT§DSE0F
385900020516     C                   movel     RAGUT         RSUT
386000011114      *
386100011114      * Chiavi di lettura
386200011114     C     K02Ute        KLIST                                                  *tntbe01l
386300011114     C                   KFLD                    $Fil              3 0
386400020329     C                   KFLD                    $cdu              3 0
386500021003      * Chiavi di lettura per lettura file con campi a video
386600021003     C     K02UteV       KLIST                                                  *tntbe01l
386700021003     C                   KFLD                    $FilV             3 0
386800021003     C                   KFLD                    V1CCdu            3 0
386900130705      * Key per Tabella AUT
387000130705     c     TBE01L        Klist
387100130705     c                   Kfld                    TBEcod
387200130705     c                   Kfld                    TBEke1
387300011114      *
387400011114      * Imposto tipo opzione in testata a seconda del
387500011114      * campo opzione della DS
387600011114     C                   SELECT
387700011114      * Inserimento
387800011203     C     I31Opz        Wheneq    'I'
387900011120     C                   eval      VTCOpz = Opz(1)
388000011114      * Modifica
388100011203     C     I31Opz        Wheneq    '2'
388200011120     C                   eval      VTCOpz = Opz(2)
388300011114      * Copia
388400011203     C     I31Opz        Wheneq    '3'
388500011120     C                   eval      VTCOpz = Opz(3)
388600130705      * Visualizzazione
388700011203     C     I31Opz        Wheneq    '5'
388800011120     C                   eval      VTCOpz = Opz(4)
388900011114     C                   ENDSL
389000011114      *
389100011114      * Inizializzo campi di work
389200011114     C                   move      *zeros        $Fil
389300020329     C                   move      *zeros        $cdu
389400011114     C                   move      *blanks       $Ute
389500080218     C                   move      *blanks       $Mod
389600130705
389700130705      * carico in sk le abilitazioni dei profili STANDARD
389800130705     c                   exsr      ABISTD
389900011114      *
390000011114     C                   ENDSR
390100130705
390200130705      /free
390300130705       //--------------------------------------------------------------
390400130705       //?Carico abilitazioni per profili standard.                    ?
390500130705       //--------------------------------------------------------------
390600130705       BEGSR  ABISTD;
390700130705
390800130705         clear xx;
390900130705         clear skUTE;
391000130705         clear skABI;
391100130705         clear skAUT;
391200130705         clear skFUN;
391300130705         clear skABI1;
391400130705
391500130705       //?Carico gli utenti standard in SK + abilitazioni
391600130705         exec sql
391700130705         DECLARE AZUTE cursor for
391800130705         SELECT * from AZUTE00F
391900130705         ORDER BY UTEute;
392000130705
392100130705       //?Apertura del cursore
392200130705         exec sql
392300130705         OPEN AZUTE;
392400130705
392500130705         IF  sqlcode < 0;
392600130705           wEnd = *on;
392700130705         ENDIF;
392800130705
392900130705         DOW  not wEnd;
393000130705           exec sql
393100130705           FETCH next from AZUTE into :azuteds;
393200130705           IF  sqlcod = 100 or sqlcod < 0;
393300130705             wEnd = *on;
393400130705             leave;
393500130705           ENDIF;
393600130705
393700130705       //?solo utenti standard
393800130705           dUte02 = UTEfa1;
393900130705           IF  §UT2mopr = 'S';
394000130705             xx += 1;
394100130705             skUTE(xx) = UTEute;
394200130705             skABI(xx) = UTEfaf;
394300130705             skAUT(xx) = UTEaut;
394400130705             skFUN(xx) = UTEfun;
394500130705             skABI1(xx) = UTEfa1;
394600130705           ENDIF;
394700130705
394800130705         ENDDO;
394900130705
395000130705       //?Chiusura del cursore
395100130705         exec sql
395200130705         CLOSE AZUTE;
395300130705
395400130705       ENDSR;
395500130705
395600130705      /end-free
395700130705
395800130705     P*--------------------------------------------------
395900130705     P* Procedure name: GetProfiloUtenteProj
396000130705     P* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
396100130705     P* Returns:        Profilo utente in Proj.
396200130705     P* Parameter:      priProfiloUtente => Profilo Utente
396300130705     P*--------------------------------------------------
396400130705     P GetProfiloUtenteProj...
396500130705     P                 B
396600130705     D GetProfiloUtenteProj...
396700130705     D                 PI             1A
396800130705     D  priProfiloUtente...
396900130705     D                               10A
397000130705
397100130705     D retField        S              1A   STATIC
397200130705
397300130705      /FREE
397400130705
397500130705       CLEAR retField;
397600130705
397700130705       EXEC SQL
397800130705         SELECT CASE
397900130705                WHEN utnAnn = '1' THEN 'A'
398000130705                WHEN utnGrp = 999 THEN 'D'
398100130705                ELSE 'S'
398200130705                END
398300130705           INTO :retField
398400130705           FROM anutn00f
398500130705           WHERE utnPrf = :priProfiloUtente
398600130705       ;
398700130705
398800130705       SELECT;
398900130705         WHEN sqlCode < *ZERO;
399000130705           DUMP(A);
399100130705           retField = '?';
399200130705       ENDSL;
399300130705
399400130705       RETURN retField;
399500130705
399600130705      /END-FREE
399700130705     P GetProfiloUtenteProj...
399800130705     P                 E
399900140522
400000140522      *----------------------------------------------------
400100140523      * Procedure name: IsFilMess
400200140523      * Purpose:        Filiale utente Messaggeria
400300140522      * Returns:        ON - OFF
400400140522      *----------------------------------------------------
400500140523     p IsFilMess       B
400600140523     d IsFilMess       PI              n
400700140522
400800140522     d isErrore        s               n   inz
400900140522
401000140522      /free
401100140522
401200140522       clear OG140;
401300140522       clear OG143;
401400140526       kfil = %int(V1Cfil);
401500140526       chain (kfil) AZORG01L;
401600140522       IF  %found(AZORG01L);
401700140522         OG140 = ORGde0;
401800140522         OG143 = ORGde3;
401900140522       ENDIF;
402000140522
402100140523       IF og143.§OGntw = 'MES' and og140.§OGapo <> %editc(ORGfil:'X');
402200140522         return *on;
402300140522       ELSE;
402400140522         return *off;
402500140522       ENDIF;
402600140522
402700140522      /end-free
402800140523     p IsFilMess       E
402900140522
403000011114     O*--------------------------------------------------------------------------------------------*
403100011114** -OPZ-
403200011114  Inserimento
403300011114   Modifica
403400011114    Copia
403500011114Visualizzazione
403600011114** -MSG-
403700050609TIBS31R- Profilo obbligatorio                                                  1
403800050609TIBS31R- Profilo già esistente                                                 2
403900050609TIBS31R- Codice utente già presente per la filiale                             3
404000050609TIBS31R- Immettere entrambi i campi                                            4
404100050609TIBS31R- Incongruenza utente posta/filiale                                     5
404200050609TIBS31R- Record allocato                                                       6
404300130709TIBS31R- Filiale di appartenenza non corretta                                  7
404400050609TIBS31R- Codice utente obbligatorio                                            8
404500050609TIBS31R- Data scadenza validità obbligatoria                                   9
404600050609TIBS31R- Funzione aziendale obbligatoria                                      10
404700050609TIBS31R- Livello di autorità obbligatorio                                     11
404800050609TIBS31R- Codice autorizzazione non valido                                     12
404900130709TIBS31R- Filiale di appartenenza obbligatoria                                 13
405000050609TIBS31R- Data scadenza validità non corretta                                  14
405100050609TIBS31R- Funzione aziendale non corretta                                      15
405200050609TIBS31R- Livello di autorità non corretto                                     16
405300050609TIBS31R- Profilo posta AS/400 errato                                          17
405400050609TIBS31R- Autorizzazione non utilizzabile da utenti di sede                    18
405500050609TIBS31R- Profilo inesistente nel modulo base                                  19
405600081014TIBS31R- Codice autorizzazione obbligatorio                                   20
405700100114TIBS31R- Descrizione obbligatoria                                             21
405800100114TIBS31R- Modifica del profilo utente sul sistema non riuscita - vedi JOBLOG   22
405900130930TIBS31R- Se impostato AUTORITA AUT uguale a NO i flag devono essere = a N     23
406000131014TIBS31R- Se impostato AUTORITA AUT diversa da NO almeno un flag deve essere S 24
406100131014TIBS31R- Se Profilo di filiale i flag raggruppamento SEDE devono essere = N   25
406200150618TIBS31R- Filiale ufficio errata                                               26
