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
017101170707      *
017102170707     c                   if        I31opz = 'S'
017103170707     c                   eval      TipVid = 'S'
017104170707     c                   endif
017200160901      *
017300160901     c                   if        I31opz  = '3'  and
017400160901     c                             SDSprm  > 2
017500160901     c                   eval      $CarD01 = *on
017600160901     c                   endif
017700100114      *
017800040816      * Inizializzazione videate
017900080530     c                   clear                   Bs31D01
018000170705     C                   clear                   Bs31D02_BO
018100170705     C                   clear                   Bs31D02_C
018200170705     C                   clear                   Bs31D02_V1
018300170705     C                   clear                   Bs31D02_V2
018301170705     C                   clear                   Bs31D02_T
018302170705     C                   clear                   Bs31D02_B
018400011114      *
018500011114      * Gestione video
018600011114     C     $Fine         DOUEQ     'S'
018700080218     C     TipVid        CASEQ     '1'           GesD01
018800170705     C     TipVid        CASEQ     'a'           GesD02_BO
018900170705     C     TipVid        CASEQ     'b'           GesD02_C
019000170705     C     TipVid        CASEQ     'c'           GesD02_V1
019100170705     C     TipVid        CASEQ     'd'           GesD02_V2
019200170705     C     TipVid        CASEQ     'e'           GesD02_B
019201170705     C     TipVid        CASEQ     'f'           GesD02_T
019300130927     C     TipVid        CASEQ     '6'           GesD06
019301170707     C     TipVid        CASEQ     'S'           GesD06_s
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
027300170706     C                   eval      TipVid  = 'a'
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
030400170707     C                   WHEN      TipVid = 'a'
030500080218     C                   eval      TipVid = '1'
030600080218     C                   eval      *in28  = *off
030700040922      *
030800040922      * Dalla terza videata torno alla seconda
030900170707     C                   WHEN      TipVid = 'b'
031000170707     C                   eval      TipVid = 'a'
031100080218     C                   eval      *in28  = *off
031200100917      *
031300170707      * Dalla quarta videata torno alla terza
031400170707     C                   WHEN      TipVid = 'c'
031500170707     C                   eval      TipVid = 'b'
031600100917     C                   eval      *in28  = *off
031700080218      *
031800170707      * Dalla quinta videata torno alla terza
031900170707     C                   WHEN      TipVid = 'd'
032000170707     C                   eval      TipVid = 'c'
032100080218     C                   eval      *in28  = *off
032700130806      *
032800170707      * Dalla sesta videata torno alla quinta
032900170707     C                   WHEN      TipVid = 'e'
033000170707     C                   eval      TipVid = 'd'
033100130806     C                   eval      *in28  = *off
033101170707      *
033102170707      * Dalla settima videata torno alla sesta
033103170707     C                   WHEN      TipVid = 'f'
033104170707     C                   eval      TipVid = 'e'
033105170707     C                   eval      *in28  = *off
033106170707      *
033107170707      * Dalla prima (e per ora unica) window torno alla settima videata
033108170707     C                   WHEN      TipVid = 'W'
033109170707     C                   eval      TipVid = 'f'
033110170707     C                   eval      *in28  = *off
033200080218      *
033300170707      * Dall'ultima videata torno alla settima
033400170707     C                   WHEN      TipVid = '6'
033500170707     C                   eval      TipVid = 'f'
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
035300170706     C*                  clear                   Bs31D02
035400170706     C*                  clear                   Bs31D03
035500170706     C*                  clear                   Bs31D04
035600170706     C*                  clear                   Bs31D05
035601170706     C                   clear                   Bs31D02_BO
035602170706     C                   clear                   Bs31D02_C
035603170706     C                   clear                   Bs31D02_V1
035604170706     C                   clear                   Bs31D02_V2
035605170706     C                   clear                   Bs31D02_T
035606170706     C                   clear                   Bs31D02_B
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
040701170712     c  n02              eval      *in02  = (I31opz = 'S')
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
042200170706     C*                  exsr      CtrD02
042300170706     C*                  exsr      CtrD03
042400170706     C*                  exsr      CtrD03B
042500170706     C*                  exsr      CtrD04
042600170706     C*                  exsr      CtrD05
042601170706     C                   exsr      CtrD02_BO
042602170706     C                   exsr      CtrD02_C
042603170706     C                   exsr      CtrD02_V1
042604170706     C                   exsr      CtrD02_V2
042605170706     C                   exsr      CtrD02_B
042606170706     C                   exsr      CtrD02_T
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
066901170712     C     I31Opz        oreq      'S'
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
089200170712 1a  C                   if             (I31opz = '2' or i31opz='S')
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
094400170707      * videata Bolle e orm
094500170707w   1c                   when      TipVid = 'a'
094600040922      *
094700040922      * testo la richiesta ricerche
094800040922     C     '?'           scan      v2cibs                                 64
094900040922     C     '?'           scan      v2caub                                 65
095200040922      *
095300170711     C**   *in64         ifeq      *on
095400170711     C**   *in65         oreq      *on
095401170711     c                   if        *in64 or
095402170711     c                             *in65 or
095403170711     c                             %scan('?':v3corm)>0   or
095404170711     c                             %scan('?':v3cmbf)>0
095700080218     C                   eval      $PostSearch = *on
095800040922     C                   endif
095801170707      * videata  clienti
095802170707w   1c                   when      TipVid = 'b'
095803170707      *
095804170707      * testo la richiesta ricerche
095805170707     C     '?'           scan      v2cist                                 67
095806170707      *
095807170711     C***  *in67         ifeq      *on
095808170711     c                   if        *in67 or
095809170711     c                             %scan('?':v3cpot)>0 or
095810170711     c                             %scan('?':v3ccli)>0 or
095811170711     c                             %scan('?':v3cgtc)>0 or
095812170711     c                             %scan('?':v3cctc)>0
095813170707     C                   eval      $PostSearch = *on
095814170707     C                   endif
095815170707      * videata varie 1
095816170707w   1c                   when      TipVid = 'c'
095817170707     C     '?'           scan      v2ciun                                 66
095818170711     C**   *in66         ifeq      *on
095819170711     c                   if        *in66 or
095820170711     c                             %scan('?':v3ceca)>0 or
095821170711     c                             %scan('?':V5Cadd)>0 or
095822170711     c                             %scan('?':V3cstmo)>0
095824170707     C                   eval      $PostSearch = *on
095825170707     C                   endif
095826170711      * videata varie 2
095827170711w   1c                   when      TipVid = 'd'
095831170711     c                   if        %scan('?':V5Crac)>0 or
095832170711     c                             %scan('?':V4Ccgb)>0 or
095833170711     c                             %scan('?':V4cmgb)>0
095834170711     C                   eval      $PostSearch = *on
095835170711     C                   endif
095836170707      * videata abilitazioni di base
095837170707w   1c                   when      TipVid = 'e'
095838170707     C     '?'           scan      v3cabres                               70
095839170711     C***  *in70         ifeq      *on
095840170711     c                   if        *in70
095841170707     C                   eval      $PostSearch = *on
095842170707     C                   endif
095843170711      * videata abilitazioni di tecniche
095844170711w   1c                   when      TipVid = 'f'
095847170711     c                   if        %scan('?':V4cipt)>0 or
095848170711     c                             %scan('?':V4cfuf)>0 or
095849170711     c                             %scan('?':V4cpswaut)>0 or
095850170711     c                             %scan('?':V4cpswmag)>0 or
095851170711     c                             %scan('?':V4cpswr)>0
095852170711     C                   eval      $PostSearch = *on
095853170711     C                   endif
095900101008      *
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
112000170705     C***                movel     §UteGDCksc    V4GDCksc
112100130618     c                   movel     §UTEorm       V3Corm
112200160422     c                   movel     §UTEistmo     V3Cstmo
112300130806     c                   movel     §UTEvrac      V5Cvrac
112400150317     c                   movel     §UTErac       V5Crac
112500130806     c                   movel     §UTEmresca    V5Cmresca
112600170515     c                   movel     §UTEmdppca    V5Cmdppca
112700150810     c                   movel     §UTEaddt      V5Cvadd
112800150810     c                   movel     §UTEadd       V5Cadd
112900130927      * verifica se inserito livello di autorità per gestione abilitazioni aut
113000131022     c                   if        §uteautor <> *blank and I31opz <> '3'
113100130927     c                   movel     §UTEautor     V6autautor
113200131022     c                   movel     §UTEdsaut     uteautds
113300130927     c                   else
113400130927     c                   movel     'NO'          v6autautor
113500131022     c                   movel     *all'N'       uteautds
113600130927     c                   endif
113700130927     c                   movel     §AUTMTC       v6AUTMTC
113800130927     c                   movel     §AUTSDT       v6AUTSDT
113900130927     c                   movel     §AUTRET       v6AUTRET
114000130927     c                   movel     §AUTCON       v6AUTCON
114100130927     c                   movel     §AUTCRS       v6AUTCRS
114200130927     c                   movel     §AUTVTCC      v6AUTVTCC
114300130927     c                   movel     §AUTMTCC      v6AUTMTCC
114400130927     c                   movel     §AUTSDTC      v6AUTSDTC
114500130927     c                   movel     §AUTRETC      v6AUTRETC
114600130927     c                   movel     §AUTCRSC      v6AUTCRSC
114700130927     c                   movel     §AUTCONC      v6AUTCONC
114800130927     c                   movel     §RIEABI       v6RIEABI
114900130927     c                   movel     §AFFMTC       v6AFFMTC
115000130927     c                   movel     §AFFCON       v6AFFCON
115100130927     c                   movel     §AFFCRS       v6AFFCRS
115200130927     c                   movel     §AFFGVSTD     v6AFFGVSTD
115300130927     c                   movel     §AFFAGV       v6AFFAGV
115400130927     c                   movel     §AFFCPSTD     v6AFFCPSTD
115500130927     c                   movel     §AFFARRM      v6AFFARRM
115600130927     c                   movel     §AUTCPRRE     v6AUTCPRRE
115700130927     c                   movel     §AUTSTDCC     v6AUTSTDCC
115800130927     c                   movel     §AUTRFCIT     v6AUTRFCIT
115900130927     c                   movel     §AUTRFCOP     v6AUTRFCOP
116000130927     c                   movel     §AUTRFAFD     v6AUTRFAFD
116100130927     c                   movel     §AUTRFPRE     v6AUTRFPRE
116200080218      *
116300080218     c                   clear                   V3Dgtc
116400080218     c                   clear                   V3Dctc
116500080218     c                   clear                   V3Dpot
116600080218     c                   clear                   V3Dcli
116700080218     c                   clear                   V3Deca
116800080218     c                   clear                   V3Dmbf
116900081014     c                   clear                   V4Dpswaut
117000081014     c                   clear                   V4Dpswmag
117100081014     c                   clear                   V4Dpswr
117200130618     c                   clear                   V3Dorm
117300150317     c                   clear                   V5Drac
117400080218      *
117500041027      * se inserite decodifico già le abilitazioni
117600041027 2a  C                   if        v3cgtc <> *blanks
117700080218     C                   reset                   tibs02ds
117800041027     C                   movel(p)  v3cgtc        t02ke1
117900041027     C                   call      'TIBS02R'
118000041027     C                   parm                    KPJBA
118100041027     C                   parm                    TIBS02DS
118200041027     C                   movel     t02uni        dLAT
118300041027     C                   movel     §LatDes       v3dgtc
118400041027 4-  C                   endif
118500041027 2a  C                   if        v3cctc <> *blanks
118600080218     C                   reset                   tibs02ds
118700041027     C                   movel     v3cctc        t02ke1
118800041027     C                   call      'TIBS02R'
118900041027     C                   parm                    KPJBA
119000041027     C                   parm                    TIBS02DS
119100041207     C                   movel     t02uni        dLAT
119200041027     C                   movel     §LatDes       V3dctc
119300041027 4-  C                   endif
119400080218      *
119500041207 2a  C                   if        v3cpot <> *blanks
119600080218     C                   reset                   tibs02ds
119700041207     C                   movel     v3cpot        t02ke1
119800041207     C                   call      'TIBS02R'
119900041207     C                   parm                    KPJBA
120000041207     C                   parm                    TIBS02DS
120100041207     C                   movel     t02uni        dLAT
120200041207     C                   movel     §LatDes       V3dpot
120300041207 4-  C                   endif
120400041207 2a  C                   if        v3ccli <> *blanks
120500080218     C                   reset                   tibs02ds
120600041207     C                   movel     v3ccli        t02ke1
120700041207     C                   call      'TIBS02R'
120800041207     C                   parm                    KPJBA
120900041207     C                   parm                    TIBS02DS
121000041207     C                   movel     t02uni        dLAT
121100041207     C                   movel     §LatDes       V3dcli
121200041207 4-  C                   endif
121300050609 2a  c                   if        v3ceca <> *blanks
121400080218     C                   reset                   tibs02ds
121500050609     C                   movel     v3ceca        t02ke1
121600050609     C                   call      'TIBS02R'
121700050609     C                   parm                    KPJBA
121800050609     C                   parm                    TIBS02DS
121900050609     C                   movel     t02uni        dLAT
122000050609     C                   movel     §LatDes       V3deca
122100050609 4-  C                   endif
122200080218      *
122300060126 2a  c                   if        v3cmbf <> *blanks
122400080218     C                   reset                   tibs02ds
122500060126     C                   movel     v3cmbf        t02ke1
122600060126     C                   call      'TIBS02R'
122700060126     C                   parm                    KPJBA
122800060126     C                   parm                    TIBS02DS
122900060126     C                   movel     t02uni        dLAT
123000060126     C                   movel     §LatDes       V3dmbf
123100060126 4-  C                   endif
123200130618
123300130618      * ORM
123400130618 2a  c                   IF        V3Corm <> *blanks
123500130618     c                   reset                   TIBS02DS
123600130618     c                   eval      T02ke1 = V3Corm
123700130618     c                   call      'TIBS02R'
123800130618     c                   parm                    KPJBA
123900130618     c                   parm                    TIBS02DS
124000130618     c                   eval      dLAT = T02uni
124100130618     c                   eval      V3Dorm = §LATdes
124200130618 4-  c                   ENDIF
124300160422      * statistica movimento
124400160422 2a  c                   IF        V3Cstmo <> *blanks
124500160422     c                   reset                   TIBS02DS
124600160422     c                   eval      T02ke1 = V3Cstmo
124700160422     c                   call      'TIBS02R'
124800160422     c                   parm                    KPJBA
124900160422     c                   parm                    TIBS02DS
125000160422     c                   eval      dLAT = T02uni
125100160422     c                   eval      V3Dstmo = §LATdes
125200160422 4-  c                   ENDIF
125300011121      *
125400081013      * decodifico abilitazioni modifica password utenti di rete
125500081013     c                   if        v4cpswaut <> *blanks
125600081013     c                   reset                   tibs02ds
125700081013     c                   movel     v4cpswaut     t02ke1
125800081013     c                   call      'TIBS02R'
125900081013     c                   parm                    kpjba
126000081013     c                   parm                    tibs02ds
126100081013     c                   eval      dlat = t02uni
126200081013     c                   eval      v4dpswaut = §latdes
126300081013     c                   endif
126400081013
126500081013     c                   if        v4cpswmag <> *blanks
126600081013     c                   reset                   tibs02ds
126700081013     c                   movel     v4cpswmag     t02ke1
126800081013     c                   call      'TIBS02R'
126900081013     c                   parm                    kpjba
127000081013     c                   parm                    tibs02ds
127100081013     c                   eval      dlat = t02uni
127200081013     c                   eval      v4dpswmag = §latdes
127300081013     c                   endif
127400081013
127500081013     c                   if        v4cpswr   <> *blanks
127600081013     c                   reset                   tibs02ds
127700081013     c                   movel     v4cpswr       t02ke1
127800081013     c                   call      'TIBS02R'
127900081013     c                   parm                    kpjba
128000081013     c                   parm                    tibs02ds
128100081013     c                   eval      dlat = t02uni
128200081013     c                   eval      v4dpswr   = §latdes
128300081013     c                   endif
128400150210      * gestione bancali al cliente
128500150210 2a  C                   if        v4ccgb <> *blanks
128600150210     C                   reset                   tibs02ds
128700150210     C                   movel(p)  v4ccgb        t02ke1
128800150210     C                   call      'TIBS02R'
128900150210     C                   parm                    KPJBA
129000150210     C                   parm                    TIBS02DS
129100150210     C                   movel     t02uni        dLAT
129200150210     C                   movel     §LatDes       v4dcgb
129300150210 4-  C                   endif
129400150210 2a  C                   if        v4cmgb <> *blanks
129500150210     C                   reset                   tibs02ds
129600150210     C                   movel(p)  v4cmgb        t02ke1
129700150210     C                   call      'TIBS02R'
129800150210     C                   parm                    KPJBA
129900150210     C                   parm                    TIBS02DS
130000150210     C                   movel     t02uni        dLAT
130100150210     C                   movel     §LatDes       v4dmgb
130200150210 4-  C                   endif
130300150212      * gestione IP TERMINALE
130400150212 2a  C                   if        v4cipt <> *blanks
130500150212     C                   reset                   tibs02ds
130600150212     C                   movel(p)  v4cipt        t02ke1
130700150212     C                   call      'TIBS02R'
130800150212     C                   parm                    KPJBA
130900150212     C                   parm                    TIBS02DS
131000150212     C                   movel     t02uni        dLAT
131100150212     C                   movel     §LatDes       v4dipt
131200150212 4-  C                   endif
131300150618      *
131400150618      * -?Gestione Filiale Ufficio?
131500150618     c                   clear                   V4Dfuf
131600150618if  1c                   If        V4Cfuf <> *blanks  and
131700150618     c                             V4Cfuf <> *zeros
131800150618     c                   move      V4Cfuf        kFil
131900150618     c     KFil          chain     AZORG
132000150618if  2c                   if        %found(AZORG01L)
132100150618     c                   movel     ORGdes        V4Dfuf
132200150618x   2c                   else
132300150618     c                   eval      V4Dfuf = *all'? '
132400150618e   2c                   endif
132500150618e   1c                   EndIf
132600130705
132700101117      /free
132800150618
132900150317       //?Manutenzione RA
133000150317         IF  V5Crac <> *blanks;
133100150317           reset TIBS02DS;
133200150317           T02ke1 = V5Crac;
133300150317           TNTBE_RicercaControllo (kpjba : TIBS02DS);
133400150317           dLAT = T02uni;
133500150317           V5Drac = §LATdes;
133600150317         ENDIF;
133700150810       //?Addebiti CE
133800150811         IF  V5Cadd <> *blanks;
133900150811           reset TIBS02DS;
134000150811           T02ke1 = V5Cadd;
134100150811           TNTBE_RicercaControllo (kpjba : TIBS02DS);
134200150811           dLAT = T02uni;
134300150811           V5Dadd = §LATdes;
134400150811         ENDIF;
134500150317
134600101117         // -?Ricerca nuovo progressivo per profilo?
134700101117         //  ?(SE copia di profilo di filiale)?
134800130705         If  I31Opz =  '3';
134900130705       //?se ho ricevuo la filiale è copia da utente STD quindi aggiunto '00'
135000130705       //?alla fine del nome utente che ho composto prim con PROFILO STD + FIL
135100130709           IF  I31fil <> *blanks and I31fil <> *zeros;
135200130705             %subst(V1Cute:7:2) = '00';
135300130705           ENDIF;
135400130705           IF  %len(%trimr(V1Cute)) = 8  and
135500130705               %check(c_Digits : %subst(V1Cute:7:2)) = *zeros;
135600101117           exsr  sr_FndUsrPrg;
135700130705           ENDIF;
135800101117         EndIf;
135900130705
136000130705        // se non è un utente STANDARD imposto a video i campi del profilo STD
136100130806         wOKuteSTD = *off;
136200130705         clear xx;
136300130705         clear V1Sfun;
136400130705         clear V1Saut;
136500130705         clear V2Sibs;
136600130708         clear V2Saub;
136700130705         clear V2Sias;
136800130705         clear V2Siun;
136900130705         clear V2Scun;
137000130705         clear V2Sdst;
137100130705         clear V2Sdss;
137200130705         clear V2Spro;
137300130705         clear V2Scap;
137400130705         clear V3Scep;
137500130705         clear V3Sfgia;
137600140603         clear V3Sabres;
137700130705         clear V3Sgtc;
137800130705         clear V3Smodtar;
137900130705         clear V3Scar;
138000130705         clear V3Sctc;
138100130705         clear V3Sdupcli;
138200130705         clear V3Spot;
138300130705         clear V3Spotmsf;
138400130705         clear V3Spotst;
138500130705         clear V3Scli;
138600130705         clear V3Sclpfft;
138700160905         clear V3Sksclog;
138800130705         clear V3Smntsp;
138900130705         clear V3Schgcmm;
139000130705         clear V3Seca;
139100130705         clear V3Sflna;
139200130705         clear V2Sist;
139300160905         clear V4Sinvpwd;
139400130705         clear V3Sstp;
139500130705         clear V3Smbf;
139600130705         clear V3Smtf;
139700130705         clear V3Smfcp;
139800130705         clear V3Sorm;
139900160422         clear V3Sstmo;
140000130705         clear V4Smaz;
140100130705         clear V4Sgorg;
140200130705         clear V4Smps;
140300130705         clear V4Spwd;
140400150212         clear V4Sipt;
140500150618         clear V4Sfuf;
140600150210         clear V4Scgb;
140700150210         clear V4Smgb;
140800160808         clear V4Scanc;
140900130705         clear V4Spswaut;
141000130705         clear V4Spswmag;
141100130705         clear V4Spswr;
141200170705         //clear V4Sgdcksc;
141300130806         clear V5Svrac;
141400150317         clear V5Srac;
141500130806         clear V5Smresca;
141600170515         clear V5Smdppca;
141700150810         clear V5Svadd;
141800150810         clear V5Sadd;
141900130705         IF  V1Cmopr <> 'S';
142000130709         // se funzione aziendale NO COCOM devo cercare STD uguale ai primi 3 dell'utente
142100140710         // stessa cosa se funzione aziendale NO REA e RED
142200140522         // se non è filiale utente messaggeria di area devo aggiungere una 'M' al profilo per
142300140522         // cercare il profilo STD delle messaggerie standard
142400140710           IF  V1Cfun <> 'COCOM' and V1Cfun <> 'RED' and V1Cfun <> 'REA';
142500140522             wrkUTE = %subst(V1Cute:1:3);
142600140523             IF  IsFilMess;
142700140522               wrkUTE = %trim(wrkUTE) + %trim('M');
142800140522             ENDIF;
142900140522             //xx = %lookup(%subst(V1Cute:1:3):skUTE);
143000140522             xx = %lookup(wrkUTE:skUTE);
143100130709           ELSE;
143200130709         // se funzione aziendale COCOM devo cercare STD COCOM
143300140710             IF  V1Cfun = 'COCOM';
143400130709             xx = %lookup('COCOM':skUTE);
143500140710             ENDIF;
143600140710         // se funzione aziendale RED devo cercare STD RDRED
143700140710             IF  V1Cfun = 'RED';
143800140710               xx = %lookup('RDRED':skUTE);
143900140710             ENDIF;
144000140710         // se funzione aziendale REA devo cercare STD RAREA
144100140710             IF  V1Cfun = 'REA';
144200140710               xx = %lookup('RAREA':skUTE);
144300140710             ENDIF;
144400130709           ENDIF;
144500130705           IF  xx > 0;
144600130806             wOKuteSTD = *on;
144700130705             sUTE01 = skABI(xx);
144800130705             sUTE02 = skABI1(xx);
144900130705             V1Sfun = skFUN(xx);
145000130705             V1Saut = skAUT(xx);
145100130705             V2Sibs = sUTEibs;
145200130708             V2Saub = sUTEmbs;
145300130705             V2Sias = sUTEias;
145400130705             V2Siun = sUTEiun;
145500130705             V2Scun = sUTEcun;
145600130705             V2Sdst = sUTEdst;
145700130705             V2Sdss = sUTEdss;
145800130705             V2Spro = sUTEpro;
145900130705             V2Scap = sUTEcap;
146000130705             V3Scep = sUTEcep;
146100130705             V3Sfgia = sUTEfgia;
146200140603             V3Sabres = sUTEabres;
146300130705             V3Sgtc = sUTEgtc;
146400130705             V3Smodtar = sUTEmodtar;
146500130705             V3Scar = sUTEcar;
146600130705             V3Sctc = sUTEctc;
146700130705             V3Sdupcli = sUTEdupcli;
146800130705             V3Spot = sUTEpot;
146900130705             V3Spotmsf = sUTEpotmsf;
147000130705             V3Spotst = sUTEpotst;
147100130705             V3Scli = sUTEcli;
147200130705             V3Sclpfft = sUTEclpfft;
147300160905             V3Sksclog = sUTEksclog;
147400130705             V3Smntsp = sUTEmntsp;
147500130705             V3Schgcmm = sUTEchgcmm;
147600130705             V3Seca = sUTEeca;
147700130705             V3Sflna = sUTEflna;
147800130705             V2Sist = sUTEist;
147900160905             V4Sinvpwd = sUTEinvpwd;
148000130705             V3Sstp = sUTEstp;
148100130705             V3Smbf = sUTEmbf;
148200130705             V3Smtf = sUTEmtf;
148300130705             V3Smfcp = sUTEmfcp;
148400130705             V3Sorm = sUTEorm;
148500160422             V3Sstmo = sUTEistmo;
148600130705             V4Smaz = sUTEmaz;
148700130705             V4Sgorg = sUTEgorg;
148800130705             V4Smps = sUTEmps;
148900130705             V4Spwd = sUTEpwd;
149000150212             V4Sipt = suteipt;
149100150618             V4Sfuf = sUTEfuf;
149200150210             V4Scgb = sUTEcgb;
149300150210             V4Smgb = sUTEmgb;
149400160808             V4Scanc = sUTEcanc;
149500130705             V4Spswaut = sUTEpswaut;
149600130705             V4Spswmag = sUTEpswmag;
149700130705             V4Spswr = sUTEpswr;
149800170705        //   V4Sgdcksc = sUTEgdcksc;
149900130806             V5Svrac = sUTEvrac;
150000150317             V5Srac = sUTErac;
150100130806             V5Smresca = sUTEmresca;
150200170515             V5Smdppca = sUTEmdppca;
150300150810             V5Svadd = sUTEaddt;
150400150810             V5Sadd = sUTEadd;
150500130705           ENDIF;
150600130708        // confronto le abilitazioni RI quelle diverse
150700130708           IF  V2Cibs <> V2Sibs;
150800130708             ricibs = rimm;
150900130708           ENDIF;
151000130708           IF  V2Caub <> V2Saub;
151100130708             ricmbs = rimm;
151200130708           ENDIF;
151300130708           IF  V2Cias <> V2Sias;
151400130708             ricias = rimm;
151500130708           ENDIF;
151600130708           IF  V2Ciun <> V2Siun;
151700130708             riciun = rimm;
151800130708           ENDIF;
151900130708           IF  V2Ccun <> V2Scun;
152000130708             riccun = rimm;
152100130708           ENDIF;
152200130705         ENDIF;
152300130705
152400101117      /end-free
152500080218     C                   ENDSR
152600101117
152700101117      /free
152800101117
152900101117       //--------------------------------------------------------------
153000101117       //?Ricerca Progressivo per Utente di Filiale (in caso di COPIA) ?
153100101117       //--------------------------------------------------------------
153200101117       BEGSR  sr_FndUsrPrg;
153300101117
153400101117         wProgr = 1;
153500130709
153600130705         $Ute = %subst( V1Cute : 1 : 6 ) + %editc( wProgr : 'X' );
153700130709
153800101117         setll  ($Ute)  AZUTE000;
153900101117         read   AZUTE000;
154000101117
154100101117         Dow  Not %eof(AZUTE01L)   and
154200130718              %subst( UTEute : 1 : 6 ) = %subst( $Ute : 1 : 6 );
154300101117
154400101117           select;
154500130718             when  %check( c_Digits : %subst( UTEute : 7 : 2 ) ) > *zero;
154600101117               leave;
154700130718             when  wProgr <> %int( %subst( UTEute : 7 : 2 ) );
154800101117               leave;
154900101117             when  wProgr = *hival;
155000101117               clear  wProgr;
155100101117               leave;
155200101117           endsl;
155300101117
155400101117           wProgr += 1;
155500101117           read   AZUTE000;
155600101117
155700101117         EndDo;
155800101117
155900101117         V1Cute = %subst( $Ute : 1 : 6 ) + %editc( wProgr : 'X' );
156000101117
156100101117       ENDSR;
156200101117
156300101117      /end-free
156400101117
310501170706      *---------------------------------------------------------------*
310502170706      * GesD02_BO - Gestione videata bolle e ORM                      *
310504170706      *---------------------------------------------------------------*
310505170706     c     GesD02_BO     BEGSR
310506170706      * Emissione testata
310507170706     c                   write     Bs31T01
310508170706     c                   write     Bs31T02
310509170706      * Se esistono errori sulla videata
310510170706      *   emetto la videata ad indicatori spenti per visualizzare le
310511170706      *   eventuali decodifiche
310512170706if  1c                   if        *in28
310513170706     c                   movea     *in           Win
310514170706     c                   move      *all'0'       in2841
310515170706     c                   movea     in2841        *in(28)
310516170706     c                   write     Bs31D02_BO
310517170706     c                   movea     Win           *in
310518170706e   1c                   endif
310519170706      *
310520170706if  1c                   if        I31opz = '5'
310521170706      * Visualizzazione
310522170706     c                   write     Bs31D02_BO
310523170706     c                   exfmt     PROTECT
310524170706x   1c                   else
310525170706      *
310526170706      * Immissione/Modifica/Copia
310527170706     c                   exfmt     Bs31D02_BO
310528170706e   1c                   endif
310529170706      *
310530170706     c                   setoff                                       28  90
310531170706     c                   clear                   V1Dmsg
310532170706      *
310533170706sel 1c                   SELECT
310534170706      * F3=Fine
310535170706w   1c                   WHEN      *inKC
310536170706     c                   exsr      F03Ges
310537170706     c                   leavesr
310538170706      * F8=Modulo_Base + AS/400 del profilo
310539170706w   1c                   WHEN      *inKH
310540170706     c                   exsr      CallTIBS32
310541170706     c                   leavesr
310542170706      * F12=Ritorno
310543170706w   1c                   WHEN      *inKL
310544170706     c                   exsr      F12Ges
310545170706     c                   leavesr
310546170706      * F18=Note
310547170706w   1c                   WHEN      *inKS
310548170706     c                   exsr      sr_CallTIBS43
310549170706     c                   leavesr
310550170706      *
310551170706e   1c                   ENDSL
310552170706      *
310553170706      * Controlli e decodifiche
310554170706     c                   exsr      CtrD02_BO
310555170706      *
310556170706if  1C                   if             *in28       = *off
310557170706     C                             and  $PostSearch = *off
310558170706      * Carico la videata "Gestione Clienti"
310560170706     C                   eval      TipVid  = 'b'
310561170706e   1C                   endif
310562170706     c                   ENDSR
310563170706      *-----------------------------------------------------------
310564170706      * Controllo e decodifica videata Gestione Bolle e ORM
310566170706      *-----------------------------------------------------------
310567170706     C     CtrD02_BO     BEGSR
310568170706      *
310570170706     C                   setoff                                       394042
310571170706     C                   setoff                                       56
310573170706     C                   clear                   V1Dmsg
310574170706
310575170706     c                   clear                   ricibs
310582170706     c                   clear                   ricmbs
310583170706     c                   clear                   ricias
310584170707     c                   clear                   ricmbf
310585170706     c                   clear                   ricmtf
310586170706     c                   clear                   ricmfcp
310587170706     c                   clear                   ricorm
310599170706      *
310600170706      * Routine per gestione preparazione ricerche
310601170706     C                   exsr      GesSearch
310602170706      * Autorizzazione ORM
310603170706     c                   clear                   V3Dorm
310604170706     c                   IF        %scan('?':V3Corm) > 0
310605170706     c                   clear                   TIBS02DS
310606170706     c                   eval      T02mod = 'R'
310607170706     c                   eval      T02sif = KnSif
310608170706     c                   eval      T02cod = 'LAT'
310609170706     c                   CALL      'TIBS02R'
310610170706     c                   parm                    KPJBA
310611170706     c                   parm                    TIBS02DS
310612170706     c                   eval      V3Corm = T02ke1
310613170706     c                   eval      dLAT = T02uni
310614170706     c                   eval      V3Dorm = §LATdes
310615170711     c                   eval      *in39=*on
310616170706     c                   leavesr
310617170706 1-  c                   ENDIF
310618170706
310619170706 2a  c                   IF        V3Corm <> *blanks
310620170706     c                   reset                   TIBS02DS
310621170706     c                   eval      T02ke1 = V3Corm
310622170706     c                   call      'TIBS02R'
310623170706     c                   parm                    KPJBA
310624170706     c                   parm                    TIBS02DS
310625170706 3a  c                   IF        T02err <> *blanks
310626170706     c                   eval      V1Dmsg = Msg(12)
310627170706     c                   seton                                        2839
310628170706     c                   leavesr
310629170706     c                   ENDIF
310630170706     c                   eval      dLAT = T02uni
310631170706     c                   eval      V3Dorm = §LATdes
310632170706     c                   IF        V1Cfil = '046' and §LATuus <> 'S'
310633170706     c                   eval      V1Dmsg = Msg(18)
310634170706     c                   seton                                        2839
310635170706     c                   leavesr
310636170706     c                   ENDIF
310637170706     c                   ENDIF
310638170706      * Autorizzazione Manutenzione bolle di filiale
310639170706     C     '?'           scan      v3cmbf                                 60
310640170706     C                   clear                   v3dmbf
310641170706 1a  C                   if        *In60 and Not *In28
310642170706     C                   clear                   Tibs02Ds
310643170706     C                   move      'R'           T02Mod
310644170706     C                   movel     KnSif         T02Sif
310645170706     C                   move      'LAT'         T02Cod
310646170706     C                   CALL      'TIBS02R'
310647170706     C                   PARM                    kpjba
310648170706     C                   PARM                    Tibs02Ds
310649170706     C                   movel     T02ke1        v3cmbf
310650170706     C                   movel     T02uni        v3dmbf
310651170706     C                   setoff                                       60
310652170711     c                   seton                                        56
310653170711     c                   leavesr
310654170706 1-  C                   endif
310655170706      *
310656170706 1a  C                   if        Not *In60
310657170706 2a  C                   if        v3cmbf <> *blanks
310658170706      * Se esiste decodifico
310659170706     C                   reset                   tibs02ds
310660170706     C                   movel     v3cmbf        t02ke1
310661170706     C                   call      'TIBS02R'
310662170706     C                   parm                    KPJBA
310663170706     C                   parm                    TIBS02DS
310664170706 3a  C                   if        t02err =  *blanks
310665170706     C                   movel     t02uni        dLAT
310666170706 4a  C     V1CFil        ifeq      '046'
310667170706     C     §LatUus       andne     'S'
310668170706     C  n28              eval      V1Dmsg = Msg(18)
310669170706     C  n28              seton                                        2856
310670170706     C                   else
310671170706     C                   movel     §LatDes       V3dmbf
310672170706 4-  C                   endif
310673170706 3e  C                   else
310674170706     C  n28              eval      V1Dmsg = Msg(12)
310675170706     C  n28              seton                                        2856
310676170706 3-  C                   endif
310677170706 2-  C                   endif
310678170706 1-  C                   endif
310679170706      *
310680170706      * Autorizzazione interrogazione bolle sede
310681170706     C                   clear                   V2DIbs
310682170706 1a  C     *In64         ifeq      *on
310683170706     C     *In28         andeq     *off
310684170706      *
310685170706     C                   move      *blanks       Tibs02Ds
310686170706     C                   move      'R'           T02Mod
310687170706     C                   movel     KnSif         T02Sif
310688170706     C                   move      'LAT'         T02Cod
310689170706     C                   CALL      'TIBS02R'
310690170706     C                   PARM                    kpjba
310691170706     C                   PARM                    Tibs02Ds
310692170706     C                   movel     T02ke1        V2CIbs
310693170706     C                   movel     T02uni        V2DIbs
310694170706     C                   setoff                                       64
310695170711     c                   seton                                        42
310696170711     c                   leavesr
310697170706 1-  C                   endif
310698170706      *
310699170706      * Non devo eseguire i controlli se richiesta ricerca
310700170706 1a  C     *in64         ifeq      *off
310701170706 2a  C     V2CIbs        ifne      *blanks
310702170706      * Se esiste decodifico
310703170706     C                   reset                   tibs02ds
310704170706     C                   movel(p)  V2CIbs        t02ke1
310705170706     C                   call      'TIBS02R'
310706170706     C                   parm                    KPJBA
310707170706     C                   parm                    TIBS02DS
310708170706 3a  C                   if        t02err =  *blanks
310709170706     C                   movel     t02uni        dLAT
310710170706 4a  C     V1CFil        ifeq      '046'
310711170706     C     §LatUus       andne     'S'
310712170706     C  n28              eval      V1Dmsg = Msg(18)
310713170706     C  n28              seton                                        2842
310714170706     C                   else
310715170706     C                   movel     §LatDes       V2DIbs
310716170706 4-  C                   endif
310717170706 3e  C                   else
310718170706     C  n28              eval      V1Dmsg = Msg(12)
310719170706     C  n28              seton                                        2842
310720170706 3-  C                   endif
310721170706 2-  C                   endif
310722170706 1-  C                   endif
310723170706      /free
310724170706        // confronto con lo STD
310725170706         IF  V2Cibs <> V2Sibs and V1Cmopr <> 'S' and wOKuteSTD;
310726170706           ricibs = rimm;
310727170706         ENDIF;
310728170706      /end-free
310729170706      *
310730170706      * Autorizzazione manutenzione bolle sede
310731170706     C                   clear                   V2DAuB
310732170706 1a  C     *In65         ifeq      *on
310733170706     C     *In28         andeq     *off
310734170706      *
310735170706     C                   move      *blanks       Tibs02Ds
310736170706     C                   move      'R'           T02Mod
310737170706     C                   movel     KnSif         T02Sif
310738170706     C                   move      'LAT'         T02Cod
310739170706     C                   CALL      'TIBS02R'
310740170706     C                   PARM                    kpjba
310741170706     C                   PARM                    Tibs02Ds
310742170706     C                   movel     T02ke1        V2CAuB
310743170706     C                   movel     T02uni        V2DAuB
310744170706     C                   setoff                                       65
310745170711     c                   seton                                        40
310746170711     c                   leavesr
310747170706 1-  C                   endif
310748170706      *
310749170706      * Non devo eseguire i controlli se richiesta ricerca
310750170706 1a  C     *in65         ifeq      *off
310751170706 2a  C     V2CAuB        ifne      *blanks
310752170706      *
310753170706      * Se esiste decodifico
310754170706     C                   reset                   tibs02ds
310755170706     C                   movel     V2CAuB        t02ke1
310756170706     C                   call      'TIBS02R'
310757170706     C                   parm                    KPJBA
310758170706     C                   parm                    TIBS02DS
310759170706 3a  C                   if        t02err =  *blanks
310760170706     C                   movel     t02uni        dLAT
310761170706 4a  C     V1CFil        ifeq      '046'
310762170706     C     §LatUus       andne     'S'
310763170706     C  n28              eval      V1Dmsg = Msg(18)
310764170706     C  n28              seton                                        2840
310765170706     C                   else
310766170706     C                   movel     §LatDes       V2DAuB
310767170706 4-  C                   endif
310768170706 3e  C                   else
310769170706     C  n28              eval      V1Dmsg = Msg(12)
310770170706     C  n28              seton                                        2840
310771170706 3-  C                   endif
310772170706 2-  C                   endif
310773170706 1-  C                   endif
310774170706      /free
310775170706        // confronto con lo STD
310776170706         IF  V2Caub <> V2Saub and V1Cmopr <> 'S' and wOKuteSTD;
310777170706           ricmbs = rimm;
310778170706         ENDIF;
310779170706         IF  V2Cias <> V2Sias and V1Cmopr <> 'S' and wOKuteSTD;
310780170706           ricias = rimm;
310781170706         ENDIF;
310782170706        // confronto con lo STD
310783170706         IF  V3Cmbf <> V3Smbf and V1Cmopr <> 'S' and wOKuteSTD;
310784170706           ricmbf = rimm;
310785170706         ENDIF;
310786170706         IF  V3Cmtf <> V3Smtf and V1Cmopr <> 'S' and wOKuteSTD;
310787170706           ricmtf = rimm;
310788170706         ENDIF;
310789170706         IF  V3Cmfcp <> V3Smfcp and V1Cmopr <> 'S' and wOKuteSTD;
310790170706           ricmfcp = rimm;
310791170706         ENDIF;
310792170706         IF  V3Corm <> V3Sorm and V1Cmopr <> 'S' and wOKuteSTD;
310793170706           ricorm = rimm;
310794170706         ENDIF;
310795170706      /end-free
310796170706
310797170706      *
310798170706      * Se sono in visualizzazione spengo il 28
310799170706     c                   if        I31opz = '5'
310800170706     c                   eval      *in28  = *off
310801170706     c                   endif
310929170706      *
310930170706     C                   ENDSR
310931170706      *---------------------------------------------------------------*
310932170706      * GesD02_C  - Gestione videata Clienti                          *
310933170706      *---------------------------------------------------------------*
310934170706     c     GesD02_C      BEGSR
310935170706      * Emissione testata
310936170706     c                   write     Bs31T01
310937170706     c                   write     Bs31T02
310938170706      * Se esistono errori sulla videata
310939170706      *   emetto la videata ad indicatori spenti per visualizzare le
310940170706      *   eventuali decodifiche
310941170706if  1c                   if        *in28
310942170706     c                   movea     *in           Win
310943170706     c                   move      *all'0'       in2841
310944170706     c                   movea     in2841        *in(28)
310945170706     c                   write     Bs31D02_C
310946170706     c                   movea     Win           *in
310947170706e   1c                   endif
310948170706      *
310949170706if  1c                   if        I31opz = '5'
310950170706      * Visualizzazione
310951170706     c                   write     Bs31D02_C
310952170706     c                   exfmt     PROTECT
310953170706x   1c                   else
310954170706      *
310955170706      * Immissione/Modifica/Copia
310956170706     c                   exfmt     Bs31D02_C
310957170706e   1c                   endif
310958170706      *
310959170706     c                   setoff                                       28  90
310960170706     c                   clear                   V1Dmsg
310961170706      *
310962170706sel 1c                   SELECT
310963170706      * F3=Fine
310964170706w   1c                   WHEN      *inKC
310965170706     c                   exsr      F03Ges
310966170706     c                   leavesr
310967170706      * F8=Modulo_Base + AS/400 del profilo
310968170706w   1c                   WHEN      *inKH
310969170706     c                   exsr      CallTIBS32
310970170706     c                   leavesr
310971170706      * F12=Ritorno
310972170706w   1c                   WHEN      *inKL
310973170706     c                   exsr      F12Ges
310974170706     c                   leavesr
310975170706      * F18=Note
310976170706w   1c                   WHEN      *inKS
310977170706     c                   exsr      sr_CallTIBS43
310978170706     c                   leavesr
310979170706      *
310980170706e   1c                   ENDSL
310981170706      *
310982170706      * Controlli e decodifiche
310983170706     c                   exsr      CtrD02_C
310984170706      *
310985170706if  1C                   if             *in28       = *off
310986170706     C                             and  $PostSearch = *off
310987170706      * Carico la videata "Gestione Clienti"
310988170706     C                   eval      TipVid  = 'c'
310989170706e   1C                   endif
310990170706     c                   ENDSR
310991170706      *-----------------------------------------------------------
310992170706      * Controllo e decodifica videata Gestione Clienti
310993170706      *-----------------------------------------------------------
310994170706     C     CtrD02_C      BEGSR
310995170706      *
310996170706     C                   setoff                                       505152
310997170706     C                   setoff                                       5354
310998170706     C                   clear                   V1Dmsg
310999170706
311000170706     c                   clear                   RICGTC
311001170706     C                   clear                   RICMODTAR
311002170706     c                   clear                   RICCAR
311003170706     C                   clear                   RICCTC
311004170706     c                   clear                   RICDUPCLI
311005170706     C                   clear                   RICPOT
311006170706     c                   clear                   RICPOTMSF
311007170706     C                   clear                   RICPOTST
311008170706     c                   clear                   RICCLI
311009170706     C                   clear                   RICCLPFFT
311010170706     c                   clear                   RICKSCLOG
311011170706     c                   clear                   RICCHGCMM
311012170706     C                   clear                   RICIST
311013170706     c                   clear                   RICMNTSP
311016170706      * Routine per gestione preparazione ricerche
311017170706     C                   exsr      GesSearch
311018170706
311019170706      * Autorizzazione clienti potenziali
311020170706     C     '?'           scan      v3cpot                                 60
311021170706     C                   clear                   v3dpot
311022170706 1a  C                   if        *In60 and Not *In28
311023170706     C                   clear                   Tibs02Ds
311024170706     C                   move      'R'           T02Mod
311025170706     C                   movel     KnSif         T02Sif
311026170706     C                   move      'LAT'         T02Cod
311027170706     C                   CALL      'TIBS02R'
311028170706     C                   PARM                    kpjba
311029170706     C                   PARM                    Tibs02Ds
311030170706     C                   movel     T02ke1        v3cpot
311031170706     C                   movel     T02uni        v3dpot
311032170706     C                   setoff                                       60
311033170711     c                   seton                                        53
311034170711     c                   leavesr
311035170706 1-  C                   endif
311036170706      *
311037170706      * Non devo eseguire i controlli se richiesta ricerca
311038170706 1a  C                   if        Not *In60
311039170706 2a  C                   if        v3cpot <> *blanks
311040170706      * Se esiste decodifico
311041170706     C                   reset                   tibs02ds
311042170706     C                   movel     v3cpot        t02ke1
311043170706     C                   call      'TIBS02R'
311044170706     C                   parm                    KPJBA
311045170706     C                   parm                    TIBS02DS
311046170706 3a  C                   if        t02err =  *blanks
311047170706     C                   movel     t02uni        dLAT
311048170706 4a  C     V1CFil        ifeq      '046'
311049170706     C     §LatUus       andne     'S'
311050170706     C  n28              eval      V1Dmsg = Msg(18)
311051170706     C  n28              seton                                        2853
311052170706     C                   else
311053170706     C                   movel     §LatDes       V3dpot
311054170706 4-  C                   endif
311055170706 3e  C                   else
311056170706     C  n28              eval      V1Dmsg = Msg(12)
311057170706     C  n28              seton                                        2853
311058170706 3-  C                   endif
311059170706 2-  C                   endif
311060170706 1-  C                   endif
311061170706      /free
311062170706        // confronto con lo STD
311063170706         IF  V3Cpot <> V3Spot and V1Cmopr <> 'S' and wOKuteSTD;
311064170706           ricpot = rimm;
311065170706         ENDIF;
311066170706         IF  V3Cpotmsf <> V3Spotmsf and V1Cmopr <> 'S' and wOKuteSTD;
311067170706           ricpotmsf = rimm;
311068170706         ENDIF;
311069170706         IF  V3Cpotst <> V3Spotst and V1Cmopr <> 'S' and wOKuteSTD;
311070170706           ricpotst = rimm;
311071170706         ENDIF;
311072170706      /end-free
311073170706      * Autorizzazione anagrafica clienti
311074170706     C     '?'           scan      v3ccli                                 60
311075170706     C                   clear                   v3dcli
311076170706 1a  C                   if        *In60 and Not *In28
311077170706     C                   clear                   Tibs02Ds
311078170706     C                   move      'R'           T02Mod
311079170706     C                   movel     KnSif         T02Sif
311080170706     C                   move      'LAT'         T02Cod
311081170706     C                   CALL      'TIBS02R'
311082170706     C                   PARM                    kpjba
311083170706     C                   PARM                    Tibs02Ds
311084170706     C                   movel     T02ke1        v3ccli
311085170706     C                   movel     T02uni        v3dcli
311086170706     C                   setoff                                       60
311087170711     c                   seton                                        54
311088170711     c                   leavesr
311089170706 1-  C                   endif
311090170706      *
311091170706      * Non devo eseguire i controlli se richiesta ricerca
311092170706 1a  C                   if        Not *In60
311093170706 2a  C                   if        v3ccli <> *blanks
311094170706      * Se esiste decodifico
311095170706     C                   reset                   tibs02ds
311096170706     C                   movel     v3ccli        t02ke1
311097170706     C                   call      'TIBS02R'
311098170706     C                   parm                    KPJBA
311099170706     C                   parm                    TIBS02DS
311100170706 3a  C                   if        t02err =  *blanks
311101170706     C                   movel     t02uni        dLAT
311102170706 4a  C     V1CFil        ifeq      '046'
311103170706     C     §LatUus       andne     'S'
311104170706     C  n28              eval      V1Dmsg = Msg(18)
311105170706     C  n28              seton                                        2854
311106170706     C                   else
311107170706     C                   movel     §LatDes       V3dcli
311108170706 4-  C                   endif
311109170706 3e  C                   else
311110170706     C  n28              eval      V1Dmsg = Msg(12)
311111170706     C  n28              seton                                        2854
311112170706 3-  C                   endif
311113170706 2-  C                   endif
311114170706 1-  C                   endif
311115170706      /free
311116170706        // confronto con lo STD
311117170706         IF  V3Ccli <> V3Scli and V1Cmopr <> 'S' and wOKuteSTD;
311118170706           riccli = rimm;
311119170706         ENDIF;
311120170706         IF  V3clpfft <> V3Sclpfft and V1Cmopr <> 'S' and wOKuteSTD;
311121170706           ricclpfft = rimm;
311122170706         ENDIF;
311123170706         IF  V3Cksclog <> V3Sksclog and V1Cmopr <> 'S' and wOKuteSTD;
311124170706           ricksclog = rimm;
311125170706         ENDIF;
311126170706
311127170706      * Autorizzazione Gestione tariffe clienti
311128170706     C     '?'           scan      v3cgtc                                 60
311129170706     C                   clear                   v3dgtc
311130170706 1a  C                   if        *In60 and Not *In28
311131170706     C                   clear                   Tibs02Ds
311132170706     C                   move      'R'           T02Mod
311133170706     C                   movel     KnSif         T02Sif
311134170706     C                   move      'LAT'         T02Cod
311135170706     C                   CALL      'TIBS02R'
311136170706     C                   PARM                    kpjba
311137170706     C                   PARM                    Tibs02Ds
311138170706     C                   movel     T02ke1        v3cgtc
311139170706     C                   movel     T02uni        v3dgtc
311140170706     C                   setoff                                       60
311141170711     c                   seton                                        51
311142170711     c                   leavesr
311143170706 1-  C                   endif
311144170706      *
311145170706      * Non devo eseguire i controlli se richiesta ricerca
311146170706 1a  C                   if        Not *In60
311147170706 2a  C                   if        v3cgtc <> *blanks
311148170706      * Se esiste decodifico
311149170706     C                   reset                   tibs02ds
311150170706     C                   movel(p)  v3cgtc        t02ke1
311151170706     C                   call      'TIBS02R'
311152170706     C                   parm                    KPJBA
311153170706     C                   parm                    TIBS02DS
311154170706 3a  C                   if        t02err =  *blanks
311155170706     C                   movel     t02uni        dLAT
311156170706 4a  C     V1CFil        ifeq      '046'
311157170706     C     §LatUus       andne     'S'
311158170706     C  n28              eval      V1Dmsg = Msg(18)
311159170706     C  n28              seton                                        2851
311160170706     C                   else
311161170706     C                   movel     §LatDes       v3dgtc
311162170706 4-  C                   endif
311163170706 3e  C                   else
311164170706     C  n28              eval      V1Dmsg = Msg(12)
311165170706     C  n28              seton                                        2851
311166170706 3-  C                   endif
311167170706 2-  C                   endif
311168170706 1-  C                   endif
311169170706      /free
311170170706        // confronto con lo STD
311171170706         IF  V3Cgtc <> V3Sgtc and V1Cmopr <> 'S' and wOKuteSTD;
311172170706           ricgtc = rimm;
311173170706         ENDIF;
311174170706         IF  V3modtar <> V3Smodtar and V1Cmopr <> 'S' and wOKuteSTD;
311175170706           ricmodtar = rimm;
311176170706         ENDIF;
311177170706         IF  V3Ccar <> V3Scar and V1Cmopr <> 'S' and wOKuteSTD;
311178170706           riccar = rimm;
311179170706         ENDIF;
311180170706         IF  V3Cchgcmm <> V3Schgcmm and V1Cmopr <> 'S' and wOKuteSTD;
311181170706           ricchgcmm = rimm;
311182170706         ENDIF;
311183170706      /end-free
311184170706      *
311185170706      * Autorizzazione codice cliente da cui copiare
311186170706     C     '?'           scan      v3cctc                                 60
311187170706     C                   clear                   v3dctc
311188170706 1a  C                   if        *In60 and Not *In28
311189170706     C                   clear                   Tibs02Ds
311190170706     C                   move      'R'           T02Mod
311191170706     C                   movel     KnSif         T02Sif
311192170706     C                   move      'LAT'         T02Cod
311193170706     C                   CALL      'TIBS02R'
311194170706     C                   PARM                    kpjba
311195170706     C                   PARM                    Tibs02Ds
311196170706     C                   movel     T02ke1        v3cctc
311197170706     C                   movel     T02uni        v3dctc
311198170706     C                   setoff                                       60
311199170711     c                   seton                                        52
311200170711     c                   leavesr
311201170706 1-  C                   endif
311202170706      *
311203170706      * Non devo eseguire i controlli se richiesta ricerca
311204170706 1a  C                   if        Not *In60
311205170706 2a  C                   if        v3cctc <> *blanks
311206170706      * Se esiste decodifico
311207170706     C                   reset                   tibs02ds
311208170706     C                   movel     v3cctc        t02ke1
311209170706     C                   call      'TIBS02R'
311210170706     C                   parm                    KPJBA
311211170706     C                   parm                    TIBS02DS
311212170706 3a  C                   if        t02err =  *blanks
311213170706     C                   movel     t02uni        dLAT
311214170706 4a  C     V1CFil        ifeq      '046'
311215170706     C     §LatUus       andne     'S'
311216170706     C  n28              eval      V1Dmsg = Msg(18)
311217170706     C  n28              seton                                        2852
311218170706     C                   else
311219170706     C                   movel     §LatDes       V3dctc
311220170706 4-  C                   endif
311221170706 3e  C                   else
311222170706     C  n28              eval      V1Dmsg = Msg(12)
311223170706     C  n28              seton                                        2852
311224170706 3-  C                   endif
311225170706 2-  C                   endif
311226170706 1-  C                   endif
311227170706      /free
311228170706        // confronto con lo STD
311229170706         IF  V3Cctc <> V3Sctc and V1Cmopr <> 'S' and wOKuteSTD;
311230170706           ricctc = rimm;
311231170706         ENDIF;
311232170706         IF  V3Cdupcli <> V3Sdupcli and V1Cmopr <> 'S' and wOKuteSTD;
311233170706           ricdupcli = rimm;
311234170706         ENDIF;
311235170706      /end-free
311236170706      *
311237170706      * Autorizzazione statistica cliente (azn. SE60)
311238170706     C                   clear                   V2DISt
311239170706 1a  C     *In67         ifeq      *on
311240170706     C     *In28         andeq     *off
311241170706     C                   move      *blanks       Tibs02Ds
311242170706     C                   move      'R'           T02Mod
311243170706     C                   movel     KnSif         T02Sif
311244170706     C                   move      'LAT'         T02Cod
311245170706     C                   CALL      'TIBS02R'
311246170706     C                   PARM                    kpjba
311247170706     C                   PARM                    Tibs02Ds
311248170706     C                   movel     T02ke1        V2CISt
311249170706     C                   movel     T02uni        V2DISt
311250170706     C                   setoff                                       67
311251170711     c                   seton                                        50
311252170711     c                   leavesr
311253170706 1-  C                   endif
311254170706      *
311255170706      * Non devo eseguire i controlli se richiesta ricerca
311256170706 1a  C     *in67         ifeq      *off
311257170706 2a  C     V2CISt        ifne      *blanks
311258170706      * Se esiste decodifico
311259170706     C                   reset                   tibs02ds
311260170706     C                   movel     V2CISt        t02ke1
311261170706     C                   call      'TIBS02R'
311262170706     C                   parm                    KPJBA
311263170706     C                   parm                    TIBS02DS
311264170706 3a  C                   if        t02err =  *blanks
311265170706     C                   movel     t02uni        dLAT
311266170706 4a  C     V1CFil        ifeq      '046'
311267170706     C     §LatUus       andne     'S'
311268170706     C  n28              eval      V1Dmsg = Msg(18)
311269170706     C  n28              seton                                        2850
311270170706     C                   else
311271170706     C                   movel     §LatDes       V2DISt
311272170706 4-  C                   endif
311273170706 3e  C                   else
311274170706     C  n28              eval      V1Dmsg = Msg(12)
311275170706     C  n28              seton                                        2850
311276170706 3-  C                   endif
311277170706 2-  C                   endif
311278170706 1-  C                   endif
311279170706      /free
311280170706        // confronto con lo STD
311281170706         IF  V2Cist <> V2Sist and V1Cmopr <> 'S' and wOKuteSTD;
311282170706           ricist = rimm;
311283170706         ENDIF;
311284170706         IF  V3Cmntsp <> V3Smntsp and V1Cmopr <> 'S' and wOKuteSTD;
311285170706           ricmntsp = rimm;
311286170706         ENDIF;
311287170706
311288170706      *
311289170706      * Se sono in visualizzazione spengo il 28
311290170706     c                   if        I31opz = '5'
311291170706     c                   eval      *in28  = *off
311292170706     c                   endif
311293170706      *
311294170706     C                   ENDSR
311295170706      *--------------------------------------------------------------------------------------------*
311296170706      * GesD02_V1 - Gestione videata abilitazioni Varie 1
311297170706      *--------------------------------------------------------------------------------------------*
311298170706     C     GesD02_V1     BEGSR
311299170706      *
311300170706      * Scrivo la testata
311301170706     C                   write     Bs31T01
311302170706     c                   write     Bs31T02
311303170706      *
311304170706      * Se esistono errori sulla videata
311305170706      * emetto la write del formato a indicatori spenti per vedere
311306170706      * le eventuali decodifiche
311307170706if  1C                   if        *in28
311308170706     C                   movea     *in           Win
311309170706     C                   move      *all'0'       in2841
311310170706     C                   movea     in2841        *in(28)
311311170706     C                   write     Bs31D02_V1
311312170706     C                   movea     Win           *in
311313170706e   1C                   endif
311314170706      *
311315170706if  1C                   if        I31Opz = '5'
311316170706      * Visualizzazione
311317170706     C                   write     Bs31D02_V1
311318170706     C                   exfmt     PROTECT
311319170706x   1C                   else
311320170706      * Immissione/Modifica/Copia
311321170706     C                   exfmt     Bs31D02_V1
311322170706e   1C                   endif
311323170706      *
311324170706     c                   setoff                                       28  90
311325170706     c                   clear                   V1Dmsg
311326170706      *
311327170706sel 1C                   SELECT
311328170706      * F3=Fine
311329170706w   1C                   when      *inKC
311330170706     C                   exsr      F03Ges
311331170706     c                   leavesr
311332170706      * F8=Modulo_Base + AS/400 del profilo
311333170706w   1C                   when      *inKH
311334170706     c                   exsr      CallTIBS32
311335170706     c                   leavesr
311336170706      * F12=Ritorno
311337170706w   1C                   when      *inKL
311338170706     C                   EXSR      F12Ges
311339170706     c                   leavesr
311340170706      * F18=Note
311341170706w   1c                   WHEN      *inKS
311342170706     c                   exsr      sr_CallTIBS43
311343170706     c                   leavesr
311344170706      *
311345170706e   1C                   ENDSL
311346170706      *
311347170706      * Controlli e decodifiche
311348170706     C                   exsr      CtrD02_V1
311349170706      *
311350170706if  1C                   if             *in28       = *off
311351170706     C                             and  $PostSearch = *off
311352170706      * Carico la terza videata
311353170706     C                   eval      TipVid  = 'd'
311354170706e   1C                   endif
311355170706      *
311356170706     C                   ENDSR
311357170706      *-----------------------------------------------------------
311358170706      * Controllo e decodifica videata Gestioni Varie 1
311359170706      *-----------------------------------------------------------
311360170706     C     CtrD02_V1     BEGSR
311361170706      *
311362170706     C                   setoff                                       3841
311363170706     C                   setoff                                       5578
311364170706     C                   clear                   V1Dmsg
311365170706
311366170706     c                   clear                   RICFGIA
311367170706     c                   clear                   RICECA
311368170706     c                   clear                   RICMRESCA
311369170706     c                   clear                   RICMDPPCA
311370170706     c                   clear                   RICIUN
311371170706     c                   clear                   RICCUN
311372170706     c                   clear                   RICVADD
311373170706     c                   clear                   RICADD
311374170706     c                   clear                   RICSTMO
311375170706     c                   clear                   RICSTP
311376170706      *
311377170706      * Routine per gestione preparazione ricerche
311378170706     C                   exsr      GesSearch
311379170706
311380170706      *
311405170706      * Autorizzazione elenco C.A.
311406170706      *
311407170706     C     '?'           scan      v3ceca                                 60
311408170706     C                   clear                   v3deca
311409170706 1a  C                   if        *In60 and Not *In28
311410170706     C                   clear                   Tibs02Ds
311411170706     C                   move      'R'           T02Mod
311412170706     C                   movel     KnSif         T02Sif
311413170706     C                   move      'LAT'         T02Cod
311414170706     C                   CALL      'TIBS02R'
311415170706     C                   PARM                    kpjba
311416170706     C                   PARM                    Tibs02Ds
311417170706     C                   movel     T02ke1        v3ceca
311418170706     C                   movel     T02uni        v3deca
311419170706     C                   setoff                                       60
311420170711     c                   seton                                        55
311421170711     c                   leavesr
311422170706 1-  C                   endif
311423170706      *
311424170706 1a  C                   if        Not *In60
311425170706 2a  C                   if        v3ceca <> *blanks
311426170706      * Se esiste decodifico
311427170706     C                   reset                   tibs02ds
311428170706     C                   movel     v3ceca        t02ke1
311429170706     C                   call      'TIBS02R'
311430170706     C                   parm                    KPJBA
311431170706     C                   parm                    TIBS02DS
311432170706 3a  C                   if        t02err =  *blanks
311433170706     C                   movel     t02uni        dLAT
311434170706 4a  C     V1CFil        ifeq      '046'
311435170706     C     §LatUus       andne     'S'
311436170706     C  n28              eval      V1Dmsg = Msg(18)
311437170706     C  n28              seton                                        2855
311438170706     C                   else
311439170706     C                   movel     §LatDes       V3deca
311440170706 4-  C                   endif
311441170706 3e  C                   else
311442170706     C  n28              eval      V1Dmsg = Msg(12)
311443170706     C  n28              seton                                        2855
311444170706 3-  C                   endif
311445170706 2-  C                   endif
311446170706 1-  C                   endif
311447170706        // confronto con lo STD
311448170706         IF  V3Ceca <> V3Seca and V1Cmopr <> 'S' and wOKuteSTD;
311449170706           riceca = rimm;
311450170706         ENDIF;
311451170706         IF  V5cmresca <> V5Smresca and V1Cmopr <> 'S' and wOKuteSTD;
311452170706           ricmresca = rimm;
311453170706         ENDIF;
311488170706         IF  V5cmdppca <> V5Smdppca and V1Cmopr <> 'S' and wOKuteSTD;
311489170706           ricmdppca = rimm;
311490170706         ENDIF;
311491170706         IF  V3Cfgia <> V3Sfgia and V1Cmopr <> 'S' and wOKuteSTD;
311492170706           ricfgia = rimm;
311493170706         ENDIF;
311494170706      *
311495170706      * Autorizzazione file unità
311496170706     C                   clear                   V2DIUn
311497170706 1a  C     *In66         ifeq      *on
311498170706     C     *In28         andeq     *off
311499170706     C                   clear                   Tibs02Ds
311500170706     C                   move      'R'           T02Mod
311501170706     C                   movel     KnSif         T02Sif
311502170706     C                   move      'LAT'         T02Cod
311503170706     C                   CALL      'TIBS02R'
311504170706     C                   PARM                    kpjba
311505170706     C                   PARM                    Tibs02Ds
311506170706     C                   movel     T02ke1        V2CIUn
311507170706     C                   movel     T02uni        V2DIUn
311508170706     C                   setoff                                       66
311509170711     c                   seton                                        41
311510170711     c                   leavesr
311511170706 1-  C                   endif
311512170706      *
311513170706      * Non devo eseguire i controlli se richiesta ricerca
311514170706 1a  C     *in66         ifeq      *off
311515170706 2a  C     V2CIUn        ifne      *blanks
311516170706      *
311517170706      * Se esiste decodifico
311518170706     C                   reset                   tibs02ds
311519170706     C                   movel     V2CIUn        t02ke1
311520170706     C                   call      'TIBS02R'
311521170706     C                   parm                    KPJBA
311522170706     C                   parm                    TIBS02DS
311523170706 3a  C                   if        t02err =  *blanks
311524170706     C                   movel     t02uni        dLAT
311525170706 4a  C     V1CFil        ifeq      '046'
311526170706     C     §LatUus       andne     'S'
311527170706     C  n28              eval      V1Dmsg = Msg(18)
311528170706     C  n28              seton                                        2841
311529170706     C                   else
311530170706     C                   movel     §LatDes       V2DIUn
311531170706 4-  C                   endif
311532170706 3e  C                   else
311533170706     C  n28              eval      V1Dmsg = Msg(12)
311534170706     C  n28              seton                                        2841
311535170706 3-  C                   endif
311536170706 2-  C                   endif
311537170706 1-  C                   endif
311538170706        // confronto con lo STD
311539170706         IF  V2Ciun <> V2Siun and V1Cmopr <> 'S' and wOKuteSTD;
311540170706           riciun = rimm;
311541170706         ENDIF;
311542170706         IF  V2Ccun <> V2Scun and V1Cmopr <> 'S' and wOKuteSTD;
311543170706           riccun = rimm;
311544170706         ENDIF;
311545170706       //?Abilitazione addebiti CE
311546170706         clear V5Dadd;
311547170706         IF  %scan('?':V5Cadd) > 0;
311548170706           clear TIBS02DS;
311549170706           T02mod = 'R';
311550170706           T02sif = knsif;
311551170706           T02cod = 'LAT';
311552170706           TNTBE_RicercaControllo (kpjba : TIBS02DS);
311553170706           V5Cadd = T02ke1;
311554170706           dLAT = T02uni;
311555170706           V5Dadd = §LATdes;
311556170711           *in78 = *on;
311557170711           leavesr;
311558170706         ENDIF;
311559170706
311560170706
311561170706         IF  V5Cadd <> *blanks;
311562170706         reset TIBS02DS;
311563170706         T02ke1 = V5Cadd;
311564170706         TNTBE_RicercaControllo (kpjba : TIBS02DS);
311565170706         IF  T02err <> *blanks;
311566170706           V1Dmsg = Msg(12);
311567170706           *in28 = *on;
311568170706           *in78 = *on;
311569170706           leavesr;
311570170706         ENDIF;
311571170706         dLAT = T02uni;
311572170706         V5Dadd = §LATdes;
311573170706         IF  V1Cfil = '046' and §LATuus <> 'S';
311574170706           V1Dmsg = Msg(18);
311575170706           *in28 = *on;
311576170706           *in78 = *on;
311577170706           leavesr;
311578170706         ENDIF;
311579170706         ENDIF;
311580170706
311581170706         IF  V5Cadd <> V5Sadd and V1Cmopr <> 'S' and wOKuteSTD;
311582170706           ricadd = rimm;
311583170706         ENDIF;
311584170706         IF  V5cvadd <> V5Svadd and V1Cmopr <> 'S' and wOKuteSTD;
311585170706           ricvadd = rimm;
311586170706         ENDIF;
311587170706      * Autorizzazione statistica movimento
311588170706     c                   clear                   V3Dstmo
311589170706     c                   IF        %scan('?':V3Cstmo) > 0
311590170706     c                   clear                   TIBS02DS
311591170706     c                   eval      T02mod = 'R'
311592170706     c                   eval      T02sif = KnSif
311593170706     c                   eval      T02cod = 'LAT'
311594170706     c                   CALL      'TIBS02R'
311595170706     c                   parm                    KPJBA
311596170706     c                   parm                    TIBS02DS
311597170706     c                   eval      V3Cstmo = T02ke1
311598170706     c                   eval      dLAT = T02uni
311599170706     c                   eval      V3Dstmo = §LATdes
311600170711     c                   seton                                        38
311601170706     c                   leavesr
311602170706 1-  c                   ENDIF
311603170706
311604170706 2a  c                   IF        V3Cstmo =  *blanks
311605170706     c                   leavesr
311606170706     c                   ENDIF
311607170706     c                   reset                   TIBS02DS
311608170706     c                   eval      T02ke1 = V3Cstmo
311609170706     c                   call      'TIBS02R'
311610170706     c                   parm                    KPJBA
311611170706     c                   parm                    TIBS02DS
311612170706 3a  c                   IF        T02err <> *blanks
311613170706     c                   eval      V1Dmsg = Msg(12)
311614170706     c                   seton                                        2838
311615170706     c                   leavesr
311616170706     c                   ENDIF
311617170706     c                   eval      dLAT = T02uni
311618170706     c                   eval      V3Dstmo = §LATdes
311619170706     c                   IF        V1Cfil = '046' and §LATuus <> 'S'
311620170706     c                   eval      V1Dmsg = Msg(18)
311621170706     c                   seton                                        2838
311622170706     c                   leavesr
311623170706     c                   ENDIF
311624170706         IF  V3Cstmo <> V3Sstmo and V1Cmopr <> 'S' and wOKuteSTD;
311625170706           ricstmo = rimm;
311626170706         ENDIF;
311627170706         IF  V3Cstp <> V3Sstp and V1Cmopr <> 'S' and wOKuteSTD;
311628170706           ricstp = rimm;
311629170706         ENDIF;
311630170706      *
311631170706      * Se sono in visualizzazione spengo il 28
311632170706if  1c                   if        I31opz = '5'
311633170706     c                   eval      *in28  = *off
311634170706e   1c                   endif
311635170706      *
311636170706     C                   ENDSR
311637170706      *---------------------------------------------------------------*
311638170707      * GesD02_V2 - Gestione videata abilitazioni Varie 2             *
311639170706      *---------------------------------------------------------------*
311640170706     c     GesD02_V2     BEGSR
311641170706      *
311642170706      * Emissione testata
311643170706     c                   write     Bs31T01
311644170706     c                   write     Bs31T02
311645170706      *
311646170706      * Se esistono errori sulla videata
311647170706      *   emetto la videata ad indicatori spenti per visualizzare le
311648170706      *   eventuali decodifiche
311649170706if  1c                   if        *in28
311650170706     c                   movea     *in           Win
311651170706     c                   move      *all'0'       in2841
311652170706     c                   movea     in2841        *in(28)
311653170706     c                   write     Bs31D02_V2
311654170706     c                   movea     Win           *in
311655170706e   1c                   endif
311656170706      *
311657170706      * Copia non riporto i flag dal profilo copiato
311658170706if  1c                   if        I31opz = '5'
311659170706      * Visualizzazione
311660170706     c                   write     Bs31D02_V2
311661170706     c                   exfmt     PROTECT
311662170706x   1c                   else
311663170706      *
311664170706      * Immissione/Modifica/Copia
311665170706     c                   exfmt     Bs31D02_V2
311666170706e   1c                   endif
311667170706      *
311668170706     c                   setoff                                       28  90
311669170706     c                   clear                   V1Dmsg
311670170706      *
311671170706sel 1c                   SELECT
311672170706      * F3=Fine
311673170706w   1c                   WHEN      *inKC
311674170706     c                   exsr      F03Ges
311675170706     c                   leavesr
311676170706      * F8=Modulo_Base + AS/400 del profilo
311677170706w   1c                   WHEN      *inKH
311678170706     c                   exsr      CallTIBS32
311679170706     c                   leavesr
311680170706      * F12=Ritorno
311681170706w   1c                   WHEN      *inKL
311682170706     c                   exsr      F12Ges
311683170706     c                   leavesr
311684170706      * F18=Note
311685170706w   1c                   WHEN      *inKS
311686170706     c                   exsr      sr_CallTIBS43
311687170706     c                   leavesr
311688170706      *
311689170706e   1c                   ENDSL
311690170706      *
311691170706      * Controlli e decodifiche
311692170706     c                   exsr      CtrD02_V2
311693170706      *
311694170706if  1C                   if             *in28       = *off
311695170706     C                             and  $PostSearch = *off
311696170706      * Carico la terza videata
311697170706     C                   eval      TipVid  = 'e'
311698170706e   1C                   endif
311711170706      *
311712170706     c                   ENDSR
311713170706      *---------------------------------------------------------------*
311714170706      * Controllo e decodifica videata gestione Varie 2               *
311715170706      *---------------------------------------------------------------*
311716170706     c     CtrD02_V2     BEGSR
311717170706
311718170706     c                   setoff                                       737476
311719170706     C                   clear                   V1Dmsg
311720170706
311721170706     c                   clear                   RICVRAC
311722170706     c                   clear                   RICRAC
311723170706     c                   clear                   RICCGB
311724170706     c                   clear                   RICMGB
311725170706     c                   clear                   RICCANC
311726170711      * Routine per gestione preparazione ricerche
311727170711     C                   exsr      GesSearch
311728170706      * Se sono in visualizzazione spengo il 28
311729170706     c                   if        I31opz = '5'
311730170706     c                   eval      *in28  = *off
311731170706     c                   endif
311732170706         IF  V5cvrac <> V5Svrac and V1Cmopr <> 'S' and wOKuteSTD;
311733170706           ricvrac = rimm;
311734170706         ENDIF;
311735170706       //?Abilitazione Manutenzione RA
311736170706         clear V5Drac;
311737170706         IF  %scan('?':V5Crac) > 0;
311738170706           clear TIBS02DS;
311739170706           T02mod = 'R';
311740170706           T02sif = knsif;
311741170706           T02cod = 'LAT';
311742170706           TNTBE_RicercaControllo (kpjba : TIBS02DS);
311743170706           V5Crac = T02ke1;
311744170706           dLAT = T02uni;
311745170706           V5Drac = §LATdes;
311746170711           *in76 = *on;
311747170711           leavesr;
311748170706         ENDIF;
311749170706
311750170706        if v5crac <> *blank;
311751170706         reset TIBS02DS;
311752170706         T02ke1 = V5Crac;
311753170706         TNTBE_RicercaControllo (kpjba : TIBS02DS);
311754170706         IF  T02err <> *blanks;
311755170706           V1Dmsg = Msg(12);
311756170706           *in28 = *on;
311757170706           *in76 = *on;
311758170706           leavesr;
311759170706         ENDIF;
311760170706         dLAT = T02uni;
311761170706         V5Drac = §LATdes;
311762170706         IF  V1Cfil = '046' and §LATuus <> 'S';
311763170706           V1Dmsg = Msg(18);
311764170706           *in28 = *on;
311765170706           *in76 = *on;
311766170706           leavesr;
311767170706         ENDIF;
311768170706         ENDIF;
311769170706         IF  V5Crac <> V5Srac and V1Cmopr <> 'S' and wOKuteSTD;
311770170706           ricrac = rimm;
311771170706         ENDIF;
311772170706      * Autorizzazione anagrafico bancali clienti
311773170706     C     '?'           scan      v4ccgb                                 60
311774170706     C                   clear                   v4dcgb
311775170706 1a  C                   if        *In60 and Not *In28
311776170706     C                   clear                   Tibs02Ds
311777170706     C                   move      'R'           T02Mod
311778170706     C                   movel     KnSif         T02Sif
311779170706     C                   move      'LAT'         T02Cod
311780170706     C                   CALL      'TIBS02R'
311781170706     C                   PARM                    kpjba
311782170706     C                   PARM                    Tibs02Ds
311783170706     C                   movel     T02ke1        v4ccgb
311784170706     C                   movel     T02uni        v4dcgb
311785170706     C                   setoff                                       60
311786170711     c                   seton                                        73
311787170711     c                   leavesr
311788170706 1-  C                   endif
311789170706      *
311790170706      * Non devo eseguire i controlli se richiesta ricerca
311791170706 1a  C                   if        Not *In60
311792170706 2a  C                   if        v4ccgb <> *blanks
311793170706      * Se esiste decodifico
311794170706     C                   reset                   tibs02ds
311795170706     C                   movel(p)  v4ccgb        t02ke1
311796170706     C                   call      'TIBS02R'
311797170706     C                   parm                    KPJBA
311798170706     C                   parm                    TIBS02DS
311799170706 3a  C                   if        t02err =  *blanks
311800170706     C                   movel     t02uni        dLAT
311801170706 4a  C     V1CFil        ifeq      '046'
311802170706     C     §LatUus       andne     'S'
311803170706     C  n28              eval      V1Dmsg = Msg(18)
311804170706     C  n28              seton                                        2873
311805170706     C                   else
311806170706     C                   movel     §LatDes       v4dcgb
311807170706 4-  C                   endif
311808170706 3e  C                   else
311809170706     C  n28              eval      V1Dmsg = Msg(12)
311810170706     C  n28              seton                                        2873
311811170706 3-  C                   endif
311812170706 2-  C                   endif
311813170706 1-  C                   endif
311814170706      * Autorizzazione movimenti bancali clienti
311815170706     C     '?'           scan      v4cmgb                                 60
311816170706     C                   clear                   v4dmgb
311817170706 1a  C                   if        *In60 and Not *In28
311818170706     C                   clear                   Tibs02Ds
311819170706     C                   move      'R'           T02Mod
311820170706     C                   movel     KnSif         T02Sif
311821170706     C                   move      'LAT'         T02Cod
311822170706     C                   CALL      'TIBS02R'
311823170706     C                   PARM                    kpjba
311824170706     C                   PARM                    Tibs02Ds
311825170706     C                   movel     T02ke1        v4cmgb
311826170706     C                   movel     T02uni        v4dmgb
311827170706     C                   setoff                                       60
311828170711     c                   seton                                        74
311829170711     c                   leavesr
311830170706 1-  C                   endif
311831170706      *
311832170706      * Non devo eseguire i controlli se richiesta ricerca
311833170706 1a  C                   if        Not *In60
311834170706 2a  C                   if        v4cmgb <> *blanks
311835170706      * Se esiste decodifico
311836170706     C                   reset                   tibs02ds
311837170706     C                   movel(p)  v4cmgb        t02ke1
311838170706     C                   call      'TIBS02R'
311839170706     C                   parm                    KPJBA
311840170706     C                   parm                    TIBS02DS
311841170706 3a  C                   if        t02err =  *blanks
311842170706     C                   movel     t02uni        dLAT
311843170706 4a  C     V1CFil        ifeq      '046'
311844170706     C     §LatUus       andne     'S'
311845170706     C  n28              eval      V1Dmsg = Msg(18)
311846170706     C  n28              seton                                        2874
311847170706     C                   else
311848170706     C                   movel     §LatDes       v4dmgb
311849170706 4-  C                   endif
311850170706 3e  C                   else
311851170706     C  n28              eval      V1Dmsg = Msg(12)
311852170706     C  n28              seton                                        2874
311853170706 3-  C                   endif
311854170706 2-  C                   endif
311855170706 1-  C                   endif
311856170706         IF  V4Ccgb <> V4Scgb and V1Cmopr <> 'S' and wOKuteSTD;
311857170706           riccgb = rimm;
311858170706         ENDIF;
311859170706         IF  V4Cmgb <> V4Smgb and V1Cmopr <> 'S' and wOKuteSTD;
311860170706           ricmgb = rimm;
311861170706         ENDIF;
311862170706         IF  V4Ccanc <> V4Scanc and V1Cmopr <> 'S' and wOKuteSTD;
311863170706           riccanc = rimm;
311864170706         ENDIF;
312092170706      *
312093170706
312094170706     c                   ENDSR
312095170706      *--------------------------------------------------------------------------------------------*
312096170706      * GesD02_B Gestione videata Archivi di Base
312097170706      *--------------------------------------------------------------------------------------------*
312098170706     C     GesD02_B      BEGSR
312099170706      *
312100170706      * Scrivo la testata
312101170706     C                   write     Bs31T01
312102170706     c                   write     Bs31T02
312103170706      *
312104170706      * Se esistono errori sulla videata
312105170706      * emetto la write del formato a indicatori spenti per vedere
312106170706      * le eventuali decodifiche
312107170706if  1C                   if        *in28
312108170706     C                   movea     *in           Win
312109170706     C                   move      *all'0'       in2841
312110170706     C                   movea     in2841        *in(28)
312111170706     C                   write     Bs31D02_B
312112170706     C                   movea     Win           *in
312113170706e   1C                   endif
312114170706      *
312115170706if  1C                   if        I31opz = '5'
312116170706      * Visualizzazione
312117170706     C                   write     Bs31D02_B
312118170706     C                   exfmt     PROTECT
312119170706x   1C                   else
312120170706      * Immissione/Modifica/Copia
312121170706     C                   exfmt     Bs31D02_B
312122170706e   1C                   endif
312123170706      *
312124170706     c                   setoff                                       28  90
312125170706     c                   clear                   V1Dmsg
312126170706      *
312127170706     C                   SELECT
312128170706      * F3=Fine
312129170706     C                   WHEN      *inKC
312130170706     C                   exsr      F03Ges
312131170706     c                   leavesr
312132170706      * F8=Modulo_Base + AS/400 del profilo
312133170706     c                   WHEN      *inKH
312134170706     c                   exsr      CallTIBS32
312135170706     c                   leavesr
312136170706      * F12=Ritorno
312137170706     C                   WHEN      *inKL
312138170706     C                   exsr      F12Ges
312139170706     c                   leavesr
312140170706      * F18=Note
312141170706w   1c                   WHEN      *inKS
312142170706     c                   exsr      sr_CallTIBS43
312143170706     c                   leavesr
312144170706      *
312145170706     C                   ENDSL
312146170706      *
312147170706      * Controlli e decodifiche
312148170706     C                   exsr      CtrD02_B
312149170706      *
312150170706if  1C                   if             *in28       = *off
312151170706     C                             and  $PostSearch = *off
312152170706      * Carico la quarta videata
312153170706     C                   eval      TipVid  = 'f'
312154170706e   1C                   endif
312155170706      *
312156170706     C                   ENDSR
312157170706      *-----------------------------------------------------------
312158170706      * Controllo e decodifica videata archivi di Base
312159170706      *-----------------------------------------------------------
312160170706     C     CtrD02_B      BEGSR
312161170706      *
312162170706     C                   setoff                                       60
312163170706     C                   setoff                                       70
312165170706     C                   clear                   V1Dmsg
312166170706
312167170706     c                   clear                   RICDST
312168170706     c                   clear                   RICDSS
312169170706     c                   clear                   RICPRO
312170170706     c                   clear                   RICCAP
312171170706     c                   clear                   RICCEP
312172170706     c                   clear                   RICGORG
312173170706     c                   clear                   RICFLNA
312174170706     c                   clear                   RICABRES
312175170706     c                   clear                   RICMPS
312185170706      *
312186170706      * Routine per gestione preparazione ricerche
312187170706     C                   exsr      GesSearch
312188170706
312189170706         IF  V4Cgorg <> V4Sgorg and V1Cmopr <> 'S' and wOKuteSTD;
312190170706           ricgorg = rimm;
312191170706         ENDIF;
312192170706         IF  V2Ccap <> V2Scap and V1Cmopr <> 'S' and wOKuteSTD;
312193170706           riccap = rimm;
312194170706         ENDIF;
312195170706         IF  V2Cdst <> V2Sdst and V1Cmopr <> 'S' and wOKuteSTD;
312196170706           ricdst = rimm;
312197170706         ENDIF;
312198170706         IF  V2Cdss <> V2Sdss and V1Cmopr <> 'S' and wOKuteSTD;
312199170706           ricdss = rimm;
312200170706         ENDIF;
312201170706         IF  V2Cpro <> V2Spro and V1Cmopr <> 'S' and wOKuteSTD;
312202170706           ricpro = rimm;
312203170706         ENDIF;
312204170706         IF  V3Ccep <> V3Scep and V1Cmopr <> 'S' and wOKuteSTD;
312205170706           riccep = rimm;
312206170706         ENDIF;
312207170706         IF  V4Cmps <> V4Smps and V1Cmopr <> 'S' and wOKuteSTD;
312208170706           ricmps = rimm;
312209170706         ENDIF;
312210170706         IF  V3Cflna <> V3Sflna and V1Cmopr <> 'S' and wOKuteSTD;
312211170706           ricflna = rimm;
312212170706         ENDIF;
312213170706         IF  V3Cabres <> V3Sabres and V1Cmopr <> 'S' and wOKuteSTD;
312214170706           ricabres = rimm;
312215170706         ENDIF;
312216170706      * gestione abilitazione ai resi in periodo di traizoni ridotte
312217170706     C                   clear                   V3Dabres
312218170706 1a  C     *In70         ifeq      *on
312219170706     C     *In28         andeq     *off
312220170706     C                   clear                   Tibs02Ds
312221170706     C                   move      'R'           T02Mod
312222170706     C                   movel     KnSif         T02Sif
312223170706     C                   move      'LAT'         T02Cod
312224170706     C                   CALL      'TIBS02R'
312225170706     C                   PARM                    kpjba
312226170706     C                   PARM                    Tibs02Ds
312227170706     C                   movel     T02ke1        V3Cabres
312228170706     C                   movel     T02uni        V3dabres
312229170706     C                   setoff                                       70
312230170711     c                   seton                                        71
312231170711     c                   leavesr
312232170706 1-  C                   endif
312233170706      *
312234170706      * Non devo eseguire i controlli se richiesta ricerca
312235170706 1a  C     *in70         ifeq      *off
312236170706 2a  C     V3Cabres      ifne      *blanks
312237170706      *
312238170706      * Se esiste decodifico
312239170706     C                   reset                   tibs02ds
312240170706     C                   movel     V3Cabres      t02ke1
312241170706     C                   call      'TIBS02R'
312242170706     C                   parm                    KPJBA
312243170706     C                   parm                    TIBS02DS
312244170706 3a  C                   if        t02err =  *blanks
312245170706     C                   movel     t02uni        dLAT
312246170706 4a  C     V1CFil        ifeq      '046'
312247170706     C     §LatUus       andne     'S'
312248170706     C  n28              eval      V1Dmsg = Msg(18)
312249170706     C  n28              seton                                        2871
312250170706     C                   else
312251170706     C                   movel     §LatDes       V3Dabres
312252170706 4-  C                   endif
312253170706 3e  C                   else
312254170706     C  n28              eval      V1Dmsg = Msg(12)
312255170706     C  n28              seton                                        2871
312256170706 3-  C                   endif
312257170706 2-  C                   endif
312258170706 1-  C                   endif
312259170706      *
312580170706      * Se sono in visualizzazione spengo il 28
312581170706     c                   if        I31opz = '5'
312582170706     c                   eval      *in28  = *off
312583170706     c                   endif
312584170706      *
312585170706     C                   ENDSR
312586170706      *---------------------------------------------------------------*
312587170706      * GesD02_T - Gestione videata abilitazioni "tecniche"            *
312588170706      *---------------------------------------------------------------*
312589170706     c     GesD02_T      BEGSR
312590170706      *
312591170706      * Emissione testata
312592170706     c                   write     Bs31T01
312593170706     c                   write     Bs31T02
312594170706      *
312595170706      * Se esistono errori sulla videata
312596170706      *   emetto la videata ad indicatori spenti per visualizzare le
312597170706      *   eventuali decodifiche
312598170706if  1c                   if        *in28
312599170706     c                   movea     *in           Win
312600170706     c                   move      *all'0'       in2841
312601170706     c                   movea     in2841        *in(28)
312602170706     c                   write     Bs31D02_T
312603170706     c                   movea     Win           *in
312604170706e   1c                   endif
312605170706      *
312606170706if  1c                   if        I31opz = '5'
312607170706      * Visualizzazione
312608170706     c                   write     Bs31D02_T
312609170706     c                   exfmt     PROTECT
312610170706x   1c                   else
312611170706      *
312612170706      * Immissione/Modifica/Copia
312613170706     c                   exfmt     Bs31D02_T
312614170706e   1c                   endif
312615170706      *
312616170706     c                   setoff                                       28  90
312617170706     c                   clear                   V1Dmsg
312618170706      *
312619170706sel 1c                   SELECT
312620170706      * F3=Fine
312621170706w   1c                   WHEN      *inKC
312622170706     c                   exsr      F03Ges
312623170706     c                   leavesr
312624170706      * F8=Modulo_Base + AS/400 del profilo
312625170706w   1c                   WHEN      *inKH
312626170706     c                   exsr      CallTIBS32
312627170706     c                   leavesr
312628170706      * F12=Ritorno
312629170706w   1c                   WHEN      *inKL
312630170706     c                   exsr      F12Ges
312631170706     c                   leavesr
312632170706      * F18=Note
312633170706w   1c                   WHEN      *inKS
312634170706     c                   exsr      sr_CallTIBS43
312635170706     c                   leavesr
312636170706      * '?' gestione password
312637170706     c                   WHEN      V4Cpwdint = '?'
312638170706     c                   eval      TipVid    = 'W'
312639170706     c                   leavesr
312640170706      *
312641170706e   1c                   ENDSL
312642170706      *
312643170706      * Controlli e decodifiche
312644170707     c                   exsr      CtrD02_T
312645170706      *
312646170706if  1C                   if             *in28       = *off
312647170706     C                             and  $PostSearch = *off
312648170706      * Carico la quinta videata
312649170706     C                   eval      TipVid  = '6'
312650170706e   1C                   endif
312651170706      *
312652170706     c                   ENDSR
312653170706      *---------------------------------------------------------------*
312654170706      * Controllo e decodifica videata  abilitazini Tecniche          *
312655170706      *---------------------------------------------------------------*
312656170706     c     CtrD02_T      BEGSR
312657170706
312658170706     C                   setoff                                       575859
312659170706     C                   setoff                                       7577
312660170706     c                   clear                   RICMAZ
312661170706     c                   clear                   RICIPT
312662170706     c                   clear                   RICFUF
312663170706     c                   clear                   RICPWD
312664170706     c                   clear                   RICINVPWD
312665170706     c                   clear                   RICPSWAUT
312666170706     c                   clear                   RICPSWMAG
312667170706     c                   clear                   RICPSWR
312670170706      *
312671170711      * Routine per gestione preparazione ricerche
312672170711     C                   exsr      GesSearch
312673170706      * Se sono in visualizzazione spengo il 28
312674170706     c                   if        I31opz = '5'
312675170706     c                   eval      *in28  = *off
312676170706     c                   endif
312754170706      * Autorizzazione ip terminale
312755170706     C     '?'           scan      v4cipt                                 60
312756170706     C                   clear                   v4dipt
312757170706 1a  C                   if        *In60 and Not *In28
312758170706     C                   clear                   Tibs02Ds
312759170706     C                   move      'R'           T02Mod
312760170706     C                   movel     KnSif         T02Sif
312761170706     C                   move      'LAT'         T02Cod
312762170706     C                   CALL      'TIBS02R'
312763170706     C                   PARM                    kpjba
312764170706     C                   PARM                    Tibs02Ds
312765170706     C                   movel     T02ke1        v4cipt
312766170706     C                   movel     T02uni        v4dipt
312767170706     C                   setoff                                       60
312768170711     c                   seton                                        75
312769170711     c                   leavesr
312770170706 1-  C                   endif
312771170706      *
312772170706      * Non devo eseguire i controlli se richiesta ricerca
312773170706 1a  C                   if        Not *In60
312774170706 2a  C                   if        v4cipt <> *blanks
312775170706      * Se esiste decodifico
312776170706     C                   reset                   tibs02ds
312777170706     C                   movel(p)  v4cipt        t02ke1
312778170706     C                   call      'TIBS02R'
312779170706     C                   parm                    KPJBA
312780170706     C                   parm                    TIBS02DS
312781170706 3a  C                   if        t02err =  *blanks
312782170706     C                   movel     t02uni        dLAT
312783170706 4a  C     V1CFil        ifeq      '046'
312784170706     C     §LatUus       andne     'S'
312785170706     C  n28              eval      V1Dmsg = Msg(18)
312786170706     C  n28              seton                                        2875
312787170706     C                   else
312788170706     C                   movel     §LatDes       v4dipt
312789170706 4-  C                   endif
312790170706 3e  C                   else
312791170706     C  n28              eval      V1Dmsg = Msg(12)
312792170706     C  n28              seton                                        2875
312793170706 3-  C                   endif
312794170706 2-  C                   endif
312795170706 1-  C                   endif
312796170706      *
312797170706      * -?Filiale dell'Ufficio?
312798170706      * ·?Controllo (se NON richiesta la Ricerca)?
312799170706     c                   if        V4Cfuf = *zeros
312800170706     c                   clear                   V4Cfuf
312801170706     c                   endif
312802170706     c                   if        V4Cfuf = *blanks
312803170706     c                   clear                   V4Dfuf
312804170706     c                   endif
312805170706if  1c                   If        %scan( '?' : V4Cfuf ) = *zero
312806170706     c                             and  V4Cfuf <> *blanks
312807170706     c                   move      V4Cfuf        kFil
312808170706     c     KFil          chain     AZORG
312809170706if  2c                   if        %found(AZORG01L)
312810170706     c                   movel     ORGfil        V4Cfuf
312811170706     c                   movel     ORGdes        V4Dfuf
312812170706x   2c                   else
312813170706     c  n28              eval      V1Dmsg = Msg(26)
312814170706     c  n28              seton                                        2877
312815170706e   2c                   endif
312816170706e   1c                   EndIf
312817170706      * ·?Ricerca (se NON già rilevati Errori)?
312818170706if  1c                   If        NOT  *in28  and
312819170706     c                             %scan( '?' : V4Cfuf ) > *zero
312820170706     c                   clear                   V4Dfuf
312821170706     c                   clear                   §Cod01
312822170706     c                   clear                   §Des1
312823170706     c                   clear                   §Pra
312824170706     c                   call      'TNSD24R'
312825170706     c                   parm                    §Cod01            3
312826170706     c                   parm                    §Tip              1
312827170706     c                   parm                    §Des1            25
312828170706     c                   parm                    §Pra              3
312829170706     c                   movel     §Cod01        V4Cfuf
312830170706     c                   movel     §Des1         V4Dfuf
312831170711     c                   seton                                        77
312832170711     c                   leavesr
312833170706e   1c                   EndIf
312834170706      *
312835170706        // confronto con lo STD
312836170706         IF  V4Cmaz <> V4Smaz and V1Cmopr <> 'S' and wOKuteSTD;
312837170706           ricmaz = rimm;
312838170706         ENDIF;
312839170706         IF  V4Cipt <> V4Sipt and V1Cmopr <> 'S' and wOKuteSTD;
312840170706           ricipt = rimm;
312841170706         ENDIF;
312842170706         IF  V4Cfuf <> V4Sfuf and V1Cmopr <> 'S' and wOKuteSTD;
312843170706           ricFUF = rimm;
312844170706         ENDIF;
312857170706         IF  V4Cpwd <> V4Spwd and V1Cmopr <> 'S' and wOKuteSTD;
312858170706           ricpwd = rimm;
312859170706         ENDIF;
312860170706         IF  V4Cinvpwd <> V4Sinvpwd and V1Cmopr <> 'S' and wOKuteSTD;
312861170706           ricinvpwd = rimm;
312862170706         ENDIF;
312863170706      * ricerca utente bartaut
312864170706     c                   clear                   v4dpswaut
312865170706     c                   if        %scan('?':v4cpswaut) > *zeros
312866170706     c                   clear                   tibs02ds
312867170706     c                   eval      t02mod = 'R'
312868170706     c                   eval      t02sif = knsif
312869170706     c                   eval      t02cod = 'LAT'
312870170706     c                   call      'TIBS02R'
312871170706     c                   parm                    kpjba
312872170706     c                   parm                    tibs02ds
312873170706     c                   eval      v4cpswaut = t02ke1
312874170706     c                   eval      v4dpswaut = t02uni
312875170711     c                   seton                                        57
312876170711     c                   leavesr
312877170706 1-  c                   endif
312878170706      * obbligatorio
312879170706 2a  c                   if        v4cpswaut = *blanks
312880170706     c                   eval      v1dmsg = msg(20)
312881170706     c                   seton                                        2857
312882170706     c                   leavesr
312883170706 1-  c                   endif
312884170706      * controllo
312885170706     c                   reset                   tibs02ds
312886170706     c                   eval      t02ke1 = v4cpswaut
312887170706     c                   call      'TIBS02R'
312888170706     c                   parm                    kpjba
312889170706     c                   parm                    tibs02ds
312890170706      * errore: tabella
312891170706 3a  c                   if        t02err <> *blanks
312892170706     c                   eval      v1dmsg = msg(12)
312893170706     c                   seton                                        2857
312894170706     c                   leavesr
312895170706 3-  c                   endif
312896170706      * errore: non per utenti di sede
312897170706     c                   eval      dlat = t02uni
312898170706 4a  c                   if        v1cfil = '046' and
312899170706     c                             §latuus <> 'S'
312900170706     c                   eval      v1dmsg = msg(18)
312901170706     c                   seton                                        2857
312902170706     c                   leavesr
312903170706     c                   endif
312904170706     c                   eval      v4dpswaut = §latdes
312905170706      /free
312906170706        // confronto con lo STD
312907170706         IF  V4Cpswaut <> V4Spswaut and V1Cmopr <> 'S' and wOKuteSTD;
312908170706           ricpswaut = rimm;
312909170706         ENDIF;
312910170706      /end-free
312911170706
312912170706      * ricerca utente bartmag
312913170706     c                   clear                   v4dpswmag
312914170706     c                   if        %scan('?':v4cpswmag) > *zeros
312915170706     c                   clear                   tibs02ds
312916170706     c                   eval      t02mod = 'R'
312917170706     c                   eval      t02sif = knsif
312918170706     c                   eval      t02cod = 'LAT'
312919170706     c                   call      'TIBS02R'
312920170706     c                   parm                    kpjba
312921170706     c                   parm                    tibs02ds
312922170706     c                   eval      v4cpswmag = t02ke1
312923170706     c                   eval      v4dpswmag = t02uni
312924170711     c                   seton                                        58
312925170711     c                   leavesr
312926170706 1-  c                   endif
312927170706      * obbligatorio
312928170706 2a  c                   if        v4cpswmag = *blanks
312929170706     c                   eval      v1dmsg = msg(20)
312930170706     c                   seton                                        2858
312931170706     c                   leavesr
312932170706 1-  c                   endif
312933170706      * controllo
312934170706     c                   reset                   tibs02ds
312935170706     c                   eval      t02ke1 = v4cpswmag
312936170706     c                   call      'TIBS02R'
312937170706     c                   parm                    kpjba
312938170706     c                   parm                    tibs02ds
312939170706      * errore: tabella
312940170706 3a  c                   if        t02err <> *blanks
312941170706     c                   eval      v1dmsg = msg(12)
312942170706     c                   seton                                        2858
312943170706     c                   leavesr
312944170706 3-  c                   endif
312945170706      * errore: non per utenti di sede
312946170706     c                   eval      dlat = t02uni
312947170706 4a  c                   if        v1cfil = '046' and
312948170706     c                             §latuus <> 'S'
312949170706     c                   eval      v1dmsg = msg(18)
312950170706     c                   seton                                        2858
312951170706     c                   leavesr
312952170706     c                   endif
312953170706     c                   eval      v4dpswmag = §latdes
312954170706      /free
312955170706        // confronto con lo STD
312956170706         IF  V4Cpswmag <> V4Spswmag and V1Cmopr <> 'S' and wOKuteSTD;
312957170706           ricpswmag = rimm;
312958170706         ENDIF;
312959170706      /end-free
312960170706
312961170706      * ricerca altri utenti
312962170706     c                   clear                   v4dpswr
312963170706     c                   if        %scan('?':v4cpswr) > *zeros
312964170706     c                   clear                   tibs02ds
312965170706     c                   eval      t02mod = 'R'
312966170706     c                   eval      t02sif = knsif
312967170706     c                   eval      t02cod = 'LAT'
312968170706     c                   call      'TIBS02R'
312969170706     c                   parm                    kpjba
312970170706     c                   parm                    tibs02ds
312971170706     c                   eval      v4cpswr = t02ke1
312972170706     c                   eval      v4dpswr = t02uni
312973170711     c                   seton                                        59
312974170711     c                   leavesr
312975170706 1-  c                   endif
312976170706      * obbligatorio
312977170706 2a  c                   if        v4cpswr = *blanks
312978170706     c                   eval      v1dmsg = msg(20)
312979170706     c                   seton                                        2859
312980170706     c                   leavesr
312981170706 1-  c                   endif
312982170706      * controllo
312983170706     c                   reset                   tibs02ds
312984170706     c                   eval      t02ke1 = v4cpswr
312985170706     c                   call      'TIBS02R'
312986170706     c                   parm                    kpjba
312987170706     c                   parm                    tibs02ds
312988170706      * errore: tabella
312989170706 3a  c                   if        t02err <> *blanks
312990170706     c                   eval      v1dmsg = msg(12)
312991170706     c                   seton                                        2859
312992170706     c                   leavesr
312993170706 3-  c                   endif
312994170706      * errore: non per utenti di sede
312995170706     c                   eval      dlat = t02uni
312996170706 4a  c                   if        v1cfil = '046' and
312997170706     c                             §latuus <> 'S'
312998170706     c                   eval      v1dmsg = msg(18)
312999170706     c                   seton                                        2859
313000170706     c                   leavesr
313001170706     c                   endif
313002170706     c                   eval      v4dpswr = §latdes
313003170706      /free
313004170706        // confronto con lo STD
313005170706         IF  V4Cpswr <> V4Spswr and V1Cmopr <> 'S' and wOKuteSTD;
313006170706           ricpswr = rimm;
313007170706         ENDIF;
313032170706
313033170706     c                   ENDSR
313034130927      *---------------------------------------------------------------*
313035130927      * GesD06 - Gestione quarta videata -                            *
313036130927      *---------------------------------------------------------------*
313037130927     c     GesD06        BEGSR
313038130927      *
313039130927      * Emissione testata
313040130927     c                   write     Bs31T01
313041130927     c                   write     Bs31T02
313042130927      *
313043130927      * Se esistono errori sulla videata
313044130927      *   emetto la videata ad indicatori spenti per visualizzare le
313045130927      *   eventuali decodifiche
313046130927if  1c                   if        *in28
313047130927     c                   movea     *in           Win
313048130927     c                   move      *all'0'       in2841
313049130927     c                   movea     in2841        *in(28)
313050130927     c                   write     Bs31D06
313051130927     c                   movea     Win           *in
313052130927e   1c                   endif
313053130927      *
313054131022if  1c*m                 if        I31opz = '3' and primo = *blank
313055131022     c*m                 clear                   bs31d06
313056131022     c*m                 move      'NO'          V6autautor
313057131022     c*m                 move      'X'           primo             1
313058131022     c*m                 endif
313100131002      * Copia non riporto i flag dal profilo copiato
313200130927if  1c                   if        I31opz = '5'
313300130927      * Visualizzazione
313400130927     c                   write     Bs31D06
313500130927     c                   exfmt     PROTECT
313600130927x   1c                   else
313700130927      *
313800130927      * Immissione/Modifica/Copia
313900130927     c                   exfmt     Bs31D06
314000130927e   1c                   endif
314100130927      *
314200130927     c                   setoff                                       28  90
314300130927     c                   clear                   V1Dmsg
314400130927      *
314500130927sel 1c                   SELECT
314600130927      * F3=Fine
314700130927w   1c                   WHEN      *inKC
314800130927     c                   exsr      F03Ges
314900130927     c                   leavesr
315000130927      * F8=Modulo_Base + AS/400 del profilo
315100130927w   1c                   WHEN      *inKH
315200130927     c                   exsr      CallTIBS32
315300130927     c                   leavesr
315400130927      * F12=Ritorno
315500130927w   1c                   WHEN      *inKL
315600130927     c                   exsr      F12Ges
315700130927     c                   leavesr
315800161006      * F18=Note
315900161006w   1c                   WHEN      *inKS
316000161006     c                   exsr      sr_CallTIBS43
316100161006     c                   leavesr
316200130927      *
316300130927e   1c                   ENDSL
316400130927      *
316500130927      * Controlli e decodifiche
316600130927     c                   exsr      CtrD06
316700130927      *
316800130927      * Eseguo conferma
316900130927if  1c                   if             *inKF
317000130927     c                             and  NOT *in28
317100130927      *
317200130927      * Eseguo aggiornamento
317300130927     c                   exsr      AggUte
317400130927      *
317500130927      * eseguo operazioni del dopo-aggiornamento
317600130927if  2c                   if        NOT *in28
317700130927     c                   exsr      GesAGG
317800130927e   2c                   endif
317900130927      *
318000130927e   1c                   endif
318100130927      * Richiamo la gestione "tecnica" (Modulo_Base+AS/400) del profilo
318200130927if  1c                   if             *in28  = *off
318300130927     c                             and  *inKF  = *on
318400130927     c                             and (I31opz = 'I'
318500130927     c                              or  I31opz = '3')
318600130927     c                   exsr      CallTIBS32
318700130927e   1c                   endif
318800130927      *
318900130927     c                   ENDSR
319000130927      *---------------------------------------------------------------*
319100130927      * Controllo e decodifica quarta videata                         *
319200130927      *---------------------------------------------------------------*
319300130927     c     CtrD06        BEGSR
319400130927
319500130927      * Se sono in visualizzazione spengo il 28
319600130927     c                   if        I31opz = '5'
319700130927     c                   eval      *in28  = *off
319800130927     c                   endif
319900130930      * obbligatorio
320000130930 2a  c                   if        V6AUTAUTOR = *blanks
320100130930     c                   eval      v1dmsg = msg(20)
320200130930     c                   seton                                        2859
320300130930     c                   leavesr
320400130930 1-  c                   endif
320500130930      * ricerca livello autorità
320600130930     C     '?'           scan      v6autautor                             60
320700130930     c                   if        *in60
320800130927     c                   clear                   tibs02ds
320900130927     c                   eval      t02mod = 'R'
321000130927     c                   eval      t02sif = knsif
321100130927     c                   eval      t02cod = 'LAT'
321200130927     c                   call      'TIBS02R'
321300130927     c                   parm                    kpjba
321400130927     c                   parm                    tibs02ds
321500130927     c                   movel     t02ke1        V6AUTAUTOR
321600130930      *
321700130930 1-  c                   else
321800130927      * controllo
321900130927     c                   reset                   tibs02ds
322000130927     c                   eval      t02ke1 = V6AUTAUTOR
322100130927     c                   call      'TIBS02R'
322200130927     c                   parm                    kpjba
322300130927     c                   parm                    tibs02ds
322400130927      * errore: tabella
322500130927 3a  c                   if        t02err <> *blanks
322600130927     c                   eval      v1dmsg = msg(12)
322700130927     c                   seton                                        2859
322800130927     c                   leavesr
322900130927 3-  c                   endif
323000130927      * errore: non per utenti di sede
323100130927     c                   eval      dlat = t02uni
323200130927 4a  c                   if        v1cfil = '046' and
323300130927     c                             §latuus <> 'S'
323400130927     c                   eval      v1dmsg = msg(18)
323500130927     c                   seton                                        2859
323600130927     c                   leavesr
323700130930     c                   endif
323800130927     c                   endif
323900131011     c                   setoff                                       72
324000130930 2a  c                   if        V6AUTAUTOR  = 'NO'
324100130930     c     V6AUTMTC      comp      'N'                                7272
324200130930     c                   if        *in72
324300130930     c                   eval      v1dmsg = msg(23)
324400130930     c                   seton                                        2859
324500130930     c                   leavesr
324600130930     c                   endif
324700130930     c     V6AFFMTC      comp      'N'                                7272
324800130930     c                   if        *in72
324900130930     c                   eval      v1dmsg = msg(23)
325000130930     c                   seton                                        2859
325100130930     c                   leavesr
325200130930     c                   endif
325300130930     c     V6AUTRET      comp      'N'                                7272
325400130930     c                   if        *in72
325500130930     c                   eval      v1dmsg = msg(23)
325600130930     c                   seton                                        2859
325700130930     c                   leavesr
325800130930     c                   endif
325900130930     c     V6AFFGVSTD    comp      'N'                                7272
326000130930     c                   if        *in72
326100130930     c                   eval      v1dmsg = msg(23)
326200130930     c                   seton                                        2859
326300130930     c                   leavesr
326400130930     c                   endif
326500130930     c     V6AUTCON      comp      'N'                                7272
326600130930     c                   if        *in72
326700130930     c                   eval      v1dmsg = msg(23)
326800130930     c                   seton                                        2859
326900130930     c                   leavesr
327000130930     c                   endif
327100130930     c     V6AFFAGV      comp      'N'                                7272
327200130930     c                   if        *in72
327300130930     c                   eval      v1dmsg = msg(23)
327400130930     c                   seton                                        2859
327500130930     c                   leavesr
327600130930     c                   endif
327700130930     c     V6AUTCRS      comp      'N'                                7272
327800130930     c                   if        *in72
327900130930     c                   eval      v1dmsg = msg(23)
328000130930     c                   seton                                        2859
328100130930     c                   leavesr
328200130930     c                   endif
328300130930     c     V6AFFCPSTD    comp      'N'                                7272
328400130930     c                   if        *in72
328500130930     c                   eval      v1dmsg = msg(23)
328600130930     c                   seton                                        2859
328700130930     c                   leavesr
328800130930     c                   endif
328900130930     c     V6AUTSTDCC    comp      'N'                                7272
329000130930     c                   if        *in72
329100130930     c                   eval      v1dmsg = msg(23)
329200130930     c                   seton                                        2859
329300130930     c                   leavesr
329400130930     c                   endif
329500130930     c     V6AFFARRM     comp      'N'                                7272
329600130930     c                   if        *in72
329700130930     c                   eval      v1dmsg = msg(23)
329800130930     c                   seton                                        2859
329900130930     c                   leavesr
330000130930     c                   endif
330100130930     c     V6RIEABI      comp      'N'                                7272
330200130930     c                   if        *in72
330300130930     c                   eval      v1dmsg = msg(23)
330400130930     c                   seton                                        2859
330500130930     c                   leavesr
330600130930     c                   endif
330700130930     c     V6AFFCON      comp      'N'                                7272
330800130930     c                   if        *in72
330900130930     c                   eval      v1dmsg = msg(23)
331000130930     c                   seton                                        2859
331100130930     c                   leavesr
331200130930     c                   endif
331300130930     c     V6AUTRFCIT    comp      'N'                                7272
331400130930     c                   if        *in72
331500130930     c                   eval      v1dmsg = msg(23)
331600130930     c                   seton                                        2859
331700130930     c                   leavesr
331800130930     c                   endif
331900130930     c     V6AUTRFCOP    comp      'N'                                7272
332000130930     c                   if        *in72
332100130930     c                   eval      v1dmsg = msg(23)
332200130930     c                   seton                                        2859
332300130930     c                   leavesr
332400130930     c                   endif
332500130930     c     V6AUTSDT      comp      'N'                                7272
332600130930     c                   if        *in72
332700130930     c                   eval      v1dmsg = msg(23)
332800130930     c                   seton                                        2859
332900130930     c                   leavesr
333000130930     c                   endif
333100130930     c     V6AFFCRS      comp      'N'                                7272
333200130930     c                   if        *in72
333300130930     c                   eval      v1dmsg = msg(23)
333400130930     c                   seton                                        2859
333500130930     c                   leavesr
333600130930     c                   endif
333700130930     c     V6AUTMTCC     comp      'N'                                7272
333800130930     c                   if        *in72
333900130930     c                   eval      v1dmsg = msg(23)
334000130930     c                   seton                                        2859
334100130930     c                   leavesr
334200130930     c                   endif
334300130930     c     V6AUTVTCC     comp      'N'                                7272
334400130930     c                   if        *in72
334500130930     c                   eval      v1dmsg = msg(23)
334600130930     c                   seton                                        2859
334700130930     c                   leavesr
334800130930     c                   endif
334900130930     c     V6AUTCPRRE    comp      'N'                                7272
335000130930     c                   if        *in72
335100130930     c                   eval      v1dmsg = msg(23)
335200130930     c                   seton                                        2859
335300130930     c                   leavesr
335400130930     c                   endif
335500130930     c     V6AUTSDTC     comp      'N'                                7272
335600130930     c                   if        *in72
335700130930     c                   eval      v1dmsg = msg(23)
335800130930     c                   seton                                        2859
335900130930     c                   leavesr
336000130930     c                   endif
336100130930     c     V6AUTRFPRE    comp      'N'                                7272
336200130930     c                   if        *in72
336300130930     c                   eval      v1dmsg = msg(23)
336400130930     c                   seton                                        2859
336500130930     c                   leavesr
336600130930     c                   endif
336700130930     c     V6AUTRETC     comp      'N'                                7272
336800130930     c                   if        *in72
336900130930     c                   eval      v1dmsg = msg(23)
337000130930     c                   seton                                        2859
337100130930     c                   leavesr
337200130930     c                   endif
337300130930     c     V6AUTCONC     comp      'N'                                7272
337400130930     c                   if        *in72
337500130930     c                   eval      v1dmsg = msg(23)
337600130930     c                   seton                                        2859
337700130930     c                   leavesr
337800130930     c                   endif
337900130930     c     V6AUTRFAFD    comp      'N'                                7272
338000130930     c                   if        *in72
338100130930     c                   eval      v1dmsg = msg(23)
338200130930     c                   seton                                        2859
338300130930     c                   leavesr
338400130930     c                   endif
338500130930     c     V6AUTCRSC     comp      'N'                                7272
338600130930     c                   if        *in72
338700130930     c                   eval      v1dmsg = msg(23)
338800130930     c                   seton                                        2859
338900130930     c                   leavesr
339000130930     c                   endif
339100130930     c                   else
339200130930     c     V6AUTMTC      comp      'S'                                    72
339300131014     c  n72V6AFFMTC      comp      'S'                                    72
339400130930     c  n72V6AUTRET      comp      'S'                                    72
339500130930     c  n72V6AFFGVSTD    comp      'S'                                    72
339600130930     c  n72V6AUTCON      comp      'S'                                    72
339700130930     c  n72V6AFFAGV      comp      'S'                                    72
339800130930     c  n72V6AUTCRS      comp      'S'                                    72
339900130930     c  n72V6AFFCPSTD    comp      'S'                                    72
340000130930     c  n72V6AUTSTDCC    comp      'S'                                    72
340100130930     c  n72V6AFFARRM     comp      'S'                                    72
340200130930     c  n72V6RIEABI      comp      'S'                                    72
340300130930     c  n72V6AFFCON      comp      'S'                                    72
340400130930     c  n72V6AUTRFCIT    comp      'S'                                    72
340500130930     c  n72V6AUTRFCOP    comp      'S'                                    72
340600130930     c  n72V6AUTSDT      comp      'S'                                    72
340700130930     c  n72V6AFFCRS      comp      'S'                                    72
340800130930     c  n72V6AUTMTCC     comp      'S'                                    72
340900130930     c  n72V6AUTVTCC     comp      'S'                                    72
341000130930     c  n72V6AUTCPRRE    comp      'S'                                    72
341100130930     c  n72V6AUTSDTC     comp      'S'                                    72
341200130930     c  n72V6AUTRFPRE    comp      'S'                                    72
341300130930     c  n72V6AUTRETC     comp      'S'                                    72
341400130930     c  n72V6AUTCONC     comp      'S'                                    72
341500130930     c  n72V6AUTRFAFD    comp      'S'                                    72
341600130930     c  n72V6AUTCRSC     comp      'S'                                    72
341700130930     c                   if        not *in72
341800130930     c                   eval      v1dmsg = msg(24)
341900130930     c                   seton                                        2859
342000130930     c                   leavesr
342100130930     c                   endif
342200130930      *
342300130930     c                   endif
342400131014      *
342500131014     c                   if        v1cfil <> '046' and
342600131014     c                             (v6autSDT ='S' or
342700131014     c                             v6autcrsc ='S' or
342800131014     c                             v6AFFCRS ='S')
342900131014     c                   eval      v1dmsg = msg(25)
343000131014     c                   seton                                        2859
343100131014     c                   leavesr
343200131014     c                   endif
343300131014      *
343400130927
343500130927     c                   ENDSR
343501170707      *--------------------------------------------------------------------------------------------*
343502170707      * GesD06_s videata autorizzazioni speciali: entrata diretta
343503170707      *--------------------------------------------------------------------------------------------*
343504170707     C     GesD06_s      BEGSR
343505170707      *
343506170707      * Inizializzazione videata
343507170707if  1C                   if        $CarD01 <> *off
343508170707     C                   exsr      CarD01
343509170707     C                   move      *off          $CarD01
343514170707e   1C                   endif
343515170707      *
343516170707      * Se non ci sono errori nel caricamento della videta proseguo
343517170707if  1C                   if        O31Err = 'E'
343518170707     C                   leavesr
343519170707e   1C                   endif
343520170707      *
343521170707      * Scrivo la testata
343522170707     C                   write     Bs31T01
343523170707      *
343524170707      * Se esistono errori sulla videata
343525170707      * emetto la write del formato a indicatori spenti per vedere
343526170707      * le eventuali decodifiche
343527170707if  1C                   if        *in28
343528170707     C                   movea     *in           Win
343529170707     C                   move      *ALL'0'       in2849           22
343530170707     C                   movea     in2849        *in(28)
343531170707     C                   write     Bs31D01
343532170707     C                   movea     Win           *in
343533170707e   1C                   endif
343534170707      *
343541170707     C                   exfmt     Bs31D06
343543170707      *
343544170707     c                   setoff                                       28  90
343545170707     c                   clear                   V1Dmsg
343546170707      *
343547170707sel 1C                   SELECT
343548170707      * F3=Fine
343549170707w   1C                   WHEN      *inKC
343550170707     C                   exsr      F03Ges
343551170707     c                   leavesr
343552170707      * F8=Modulo_Base + AS/400 del profilo
343553170707w   1C                   WHEN      *inKH
343554170707     c                   exsr      CallTIBS32
343555170707     c                   leavesr
343556170707      * F12=Ritorno
343557170707w   1C                   WHEN      *inKL
343558170707     C****               exsr      F12Ges
343559170707     C                   exsr      F03Ges
343560170707     c                   leavesr
343561170707      * F18=Note
343562170707w   1c                   WHEN      *inKS
343563170707     c                   exsr      sr_CallTIBS43
343564170707     c                   leavesr
343565170707      *
343566170707e   1C                   ENDSL
343567170707      *
343568170707      * Controlli e decodifiche
343569170707     C                   exsr      CtrD06
343570170707      * Eseguo conferma
343571170707if  1c                   if             *inKF
343572170707     c                             and  NOT *in28
343573170707      *
343574170707      * Eseguo aggiornamento
343575170707     c                   exsr      AggUte
343576170707      *
343577170707      * eseguo operazioni del dopo-aggiornamento
343578170707if  2c                   if        NOT *in28
343579170707     c                   exsr      GesAGG
343580170707e   2c                   endif
343581170707      *
343582170707e   1c                   endif
343589170707      *
343590170707     C                   ENDSR
343600080530      *---------------------------------------------------------------*
343700080530      * GesW01 - Gestione prima window -                              *
343800080530      *---------------------------------------------------------------*
343900080530     c     GesW01        BEGSR
344000080530      *
344100080530      * Emissione window
344200080530     c                   exfmt     BS31W01
344300080530      *
344400080530      * F12=Ritorno
344500080530if  1c                   if        *inKL
344600080530     c                   clear                   V4Cpwdint
344700080530     c                   exsr      F12Ges
344800080530     c                   endif
344900080530      *
345000080530     c                   ENDSR
345100100114      /free
345200100114       //--------------------------------------------------------------
345300100114       //?Gestione videata per la manutenzione del "Modulo Profilo"    ?
345400100114       //--------------------------------------------------------------
345500100114       BEGSR  sr_GesW02;
345600100114
345700100114         //?Inizializzazione videata?
345800100114         if  $InzW02 = *on;
345900100114           exsr  sr_InzW02;
346000100114         endif;
346100100114
346200100114         //?Emissione window?
346300100114         exfmt  BS31W02;
346400100114
346500100114         select;
346600100115
346700100115           //?F3=Fine?
346800100115           when *inKC;
346900100115             unlock  KPPRF01L;
347000100115             unlock  AZUTE01L;
347100100115             clear  O31err;
347200100115             clear  O31msg;
347300100115             O31tfu = 'F03';
347400100115             $Fine  = 'S';
347500100114
347600100114           //?F12=Ritorno?
347700100114           when *inKL;
347800100114             unlock  KPPRF01L;
347900100115             unlock  AZUTE01L;
348000100114             clear  O31err;
348100100114             clear  O31msg;
348200100114             O31tfu = 'F12';
348300100115             $Fine  = 'S';
348400100114
348500100114           //?Enter o F6=Conferma?
348600100114           other;
348700100114             exsr  sr_CtrW02;
348800100114             if  *inKF   and   NOT *in28;
348900100114               exsr  sr_F06W02;
349000100115               if  BS31err = *blank;
349100100114                 clear  O31err;
349200100114                 clear  O31msg;
349300100114                 O31tfu = 'F06';
349400100115                 $Fine  = 'S';
349500100114               endif;
349600100114             endif;
349700100114
349800100114         endsl;
349900100114
350000100114         KPJBU = TIBS31ds;
350100100114
350200100114       ENDSR;
350300100114
350400100114       //--------------------------------------------------------------
350500100114       //?Inizializzazione videata x la manutenz. del "Modulo Profilo"
350600100114       //--------------------------------------------------------------
350700100114       BEGSR  sr_InzW02;
350800100114
350900100114         // - Reperimento dati da KPPRF00F
351000100115         chain  (I31ute)  KPPRF000;
351100100114         if  Not %found(KPPRF01L);
351200100114           clear  PRFtxt;
351300100114         endif;
351400100114
351500100114         // - Reperimento dati da AZUTE00F
351600100114         clear  dUte02;
351700100114         $Ute = I31Ute;
351800100114         $Mod = '1';
351900100114         exsr  ChnUte;
352000100114         if  %found(AZUTE01L);
352100100114           dUte02 = UteFa1;
352200100114         endif;
352300100114
352400100114
352500100115         //clear  BS31W02;        (NO: emessi anche dati della KPJBA)
352600100114
352700100115         W2Cute  = I31ute;
352800100114         W2Cmopr = §UT2mopr;
352900100114         W2Ctxt  = PRFtxt;
353000100114
353100100114         $InzW02 = *off;
353200100114
353300100114       ENDSR;
353400100114
353500100114       //--------------------------------------------------------------
353600100114       //?Controllo dati del "Modulo Profilo"
353700100114       //--------------------------------------------------------------
353800100114       BEGSR  sr_CtrW02;
353900100114
354000100114         *in68 = *off;
354100100114
354200100114         if  W2Ctxt = *blank;
354300100114           *in28 = *on;
354400100114           *in68 = *on;
354500100114           V1Dmsg = Msg(21);
354600100114           leavesr;
354700100114         endif;
354800100114
354900100114       ENDSR;
355000100114
355100100114       //--------------------------------------------------------------
355200100114       //?Gestione conferma per la manutenzione del "Modulo Profilo"
355300100114       //--------------------------------------------------------------
355400100114       BEGSR  sr_F06W02;
355500100114
355600100114         // - Esegue aggiornamento di AZUTE00F
355700100114         clear  dUte02;
355800100114         §UT2mopr = W2Cmopr;
355900100114         UTEfa1   = dUTE02;
356000100114         if  %found(AZUTE01L);
356100100114           //_______________
356200100114           update  AZUTE000;
356300100114           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
356400100114         endif;
356500100114
356600100114         // - Esegue aggiornamento di KNMUS00F
356700100114         PRFtxt = W2Ctxt;
356800100114         if  %found(KPPRF01L);
356900100114           //_______________
357000100114           update  KPPRF000;
357100100114           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
357200100114         endif;
357300100114
357400100114         // - Esegue aggiornamento dei dati AS/400
357500100115         clear BS31err;
357600100115         tibs31c ( I31ute : W2Ctxt : BS31err );
357700100114
357800100115         if  BS31err = 'E';
357900100114           *in28  = *on;
358000100114           V1CMsg = Msg(22);
358100100114           leavesr;
358200100114         endif;
358300100114
358400100114         // - Eseguo operazioni del dopo-aggiornamento
358500100114         exsr  GesAGG;
358600100114
358700100114       ENDSR;
358800100114
358900100114      /end-free
359000011121      *-----------------------------------------------------------
359100011114      * Aggiornamento File Utenti aziendali
359200011121      *-----------------------------------------------------------
359300011114     C     AggUte        BEGSR
359400011114      *
359500011114      * Immissione
359600011203     C     I31Opz        ifeq      'I'
359700011203     C     I31Opz        oreq      '3'
359800011114     C                   EXSR      RieUte
359900011114     C                   WRITE     AzUte000                             22
360000011114     C                   endif
360100011114      *
360200011114      * Modifica
360300011114     C     *in02         Ifeq      *on
360400011114     C                   EXSR      RieUte
360500011114     C                   UPDATE    AzUte000
360600011114     C                   endif
360700011114      *
360800011114     C                   ENDSR
360900011121      *-----------------------------------------------------------
361000011114      * Operazioni dopo aggiornamento
361100011121      *-----------------------------------------------------------
361200011114     C     GesAgg        BEGSR
361300011114      *
361400011115     C                   eval      $Fine ='S'
361500011126      * Segnalo che torno al primo pgm con F6, per impostare
361600011126      * il caricamneto sfl.
361700011203     C                   eval      O31Tfu = 'F06'
361800161006      *
361900161006     c                   exsr      sr_CallTIBS43
362000011115      *
362100011114     C                   ENDSR
362200040729      *-----------------------------------------------------------
362300040729      * Richiamo pgm. TIBS32R
362400040729      *-----------------------------------------------------------
362500040729     c     CallTIBS32    BEGSR
362600040729      *
362700040729     c                   clear                   TIBS32ds
362800040826     c                   if        I31opz = 'I'
362900040729     c                   eval      I32opz = '1'
363000040729     c                   else
363100040729     c                   eval      I32opz = I31opz
363200040729     c                   endif
363300040729     c                   eval      I32ute = V1Cute
363400040826     c                   if        I31opz = '3'
363500040826     c                   eval      I32uco = I31ute
363600040826     c                   endif
363700040729     c                   movel(p)  TIBS32ds      KPJBU
363800040729      *
363900160830if  1c                   If        SDSprm > 2
364000160830     c                   call      'TIBS32R'
364100160830     c                   parm                    KPJBA
364200160830     c                   parm                    TIBS31ds2
364300160830x   1c                   else
364400040729     c                   call      'TIBS32R'
364500040729     c                   parm                    KPJBA
364600160830e   1c                   endif
364700040729      *
364800040729     c                   movel     KPJBU         TIBS32ds
364900040729     c                   if        O32err = 'E'
365000040729     c                   clear                   $Fine
365100040729     c                   eval      *in28  = *on
365200080218     c                   movel     O32msg        V1Dmsg
365300040729     c                   endif
365400040729      *
365500040729     c                   ENDSR
365600161006      *-----------------------------------------------------------
365700161006      * Richiamo pgm. TIBS43R
365800161006      *-----------------------------------------------------------
365900161006     c     sr_CallTIBS43 BEGSR
366000161006      *
366100161006     c                   reset                   TIBS43ds
366200161006     c*//                eval      iBS43tla = 'L'       (già così)?
366300161010     c*//                if        *inKS
366400161010if  1C                   if        I31Opz = '5'
366500161010     c                   eval      iBS43opz = '5'
366600161010     c*//                else
366700161010     c*//                eval      iBS43opz = *blank    (già così)?
366800161010     c                   endif
366900161006     c                   eval      iBS43ute = V1Cute
367000161006     c*//                eval      iBS43trc = *blank    (già così)?
367100161006      *
367200161006     c                   eval      kpjbu    = TIBS43ds
367300161006     c                   call      'TIBS43R'
367400161006     c                   parm                    KPJBA
367500161006     c                   eval      TIBS43ds = kpjbu
367600161018      *
367700161018     c                   if        oBS43err = *on
367800161018     c                   eval      *in28  = *on
367900161018     c                   eval      V1Dmsg = oBS43msg
368000161018     c                   endif
368100161006      *
368200161006     c                   ENDSR
368300011121      *-----------------------------------------------------------
368400011114      * Riempimento file tabella
368500011121      *-----------------------------------------------------------
368600011114     C     RieUte        BEGSR
368700011114      *
368800011114     C                   clear                   azute000
368900011115      * Profilo
369000011120     C                   Eval      UteUte = V1CUte
369100011115      * Data
369200011115     C                   reset                   WLBDA8
369300011115     C                   Z-ADD     *zeros        G08INV
369400020329     C                   Z-ADD     V1Cdsc        G08DAT
369500011115     C                   move      '0'           G08ERR
369600011115     C                   CALL      'XSRDA8'
369700011115     C                   PARM                    WLBDA8
369800020329     C                   Z-ADD     G08INV        Utedsc
369900011115      * Codice utente
370000020329     C                   z-add     V1Ccdu        Utecdu
370100011115      * Filiale
370200011120     C                   move      V1CFil        UteFil
370300011115      * Funzione
370400011120     C                   Eval      UteFun = V1CFun
370500011115      * Pgm iniziale/libreria
370600011120     C                   Eval      UtePgm = V1CPgm
370700011120     C                   Eval      UteLib = V1CLib
370800011115      * Livello autorità
370900011120     C                   Eval      UteAut = V1CAut
371000011115      * Profilo posta AS
371100011120     C                   Eval      UtePas = V1CPas
371200011115      * E-mail
371300011221     C                   clear                   Ema
371400011221     C                   Eval      Ema1   = V1CEmA
371500011221     C                   Eval      Ema2   = V1CEm2
371600011221     C                   Eval      UteEma = Ema
371700011115      * Azienda
371800011120     C                   Eval      UteKdt = V1CKdc
371900011115      * Legame con persona fisica
372000011120     C                   Eval      UteKcd = V1CKcd
372100011121      * Abilitazioni
372200150211     C                   Movel     V2CAuB        §UteMbs
372300020516     C                   Movel     V2CIbs        §UteIbs
372400020402     C                   Movel     V2CIUn        §UteIUn
372500020329     C                   Movel     V2CCUn        §UteCUn
372600020913     C                   Movel     V2CISt        §UteISt
372700080218     C                   Movel     V4CMaz        §UteMaz
372800030709     c                   Movel     V2CIas        §UteIas
372900040603     C                   Movel     V2CDST        §UteDST
373000040603     C                   Movel     V2CDSS        §UteDSS
373100040603     C                   Movel     V2CPRO        §UtePRO
373200050413     C                   Movel     V2CCAP        §UteCAP
373300040922     c                   Movel     v3cgtc        §UteGtc
373400130124     c                   Movel     v3modtar      §UTEmodtar
373500040922     c                   Movel     v3ccar        §UteCar
373600040922     c                   Movel     v3cctc        §UteCtc
373700090423     c                   Movel     v3cdupcli     §Utedupcli
373800041207     c                   Movel     v3cpot        §UtePot
373900090514     c                   Movel     v3cpotst      §UtePotst
374000041207     c                   Movel     v3ccli        §UteCli
374100091006     c                   movel     V3CmntSp      §UteMntSp
374200050609     c                   Movel     v3ceca        §Uteeca
374300060315     c                   Movel     v3ccep        §Utecep
374400060804     c                   Movel     v3cfgia       §Utefgia
374500140403     c                   Movel     v3cabres      §Uteabres
374600070410     c                   movel     v3cflna       §Uteflna
374700100917     c                   movel     V3CPotMsf     §UtePotMsf
374800100917     c                   movel     V3CChgCmm     §UteChgCmm
374900160905     c                   movel     V4CInvPwd     §UteInvPwd
375000100917     c                   Movel     v3cmbf        §Utembf
375100100917     c                   Movel     v3cmtf        §Utemtf
375200100917     c                   Movel     v3cmfcp       §Utemfcp
375300080218     c                   movel     V4Cgorg       §UteGORG
375400080530     c                   movel     V4Cpwd        §UtePWD
375500150212     C                   Movel     V4Cipt        §Uteipt
375600150618     c                   movel     V4Cfuf        §uteFUF
375700080526     c                   movel     V4Cmps        §UteMPS
375800081014     c                   movel     V4Cpswaut     §Utepswaut
375900081014     c                   movel     V4Cpswmag     §Utepswmag
376000081014     c                   movel     V4Cpswr       §Utepswr
376100150210     c                   movel     V4Ccgb        §Utecgb
376200150210     c                   movel     V4Cmgb        §Utemgb
376300160808     c                   movel     V4Ccanc       §Utecanc
376400110429     c                   Movel     v3cstp        §Utestp
376500120613     c                   movel     V3CLPfft      §UteCLPfft
376600160905     c                   movel     V3Cksclog     §UteKscLog
376700170705     c***                movel     V4GDCksc      §UteGDCksc
376800130619     c                   movel     V3Corm        §UTEorm
376900160422     c                   movel     V3Cstmo       §UTEistmo
377000130806     c                   movel     V5Cvrac       §UTEvrac
377100150317     c                   movel     V5Crac        §UTErac
377200130806     c                   movel     V5Cmresca     §UTEmresca
377300170515     c                   movel     V5Cmdppca     §UTEmdppca
377400150810     c                   movel     V5Cvadd       §UTEaddt
377500150810     c                   movel     V5Cadd        §UTEadd
377600130927     c                   movel     V6autautor    §UTEautor
377700130927     c                   movel     v6AUTMTC      §AUTMTC
377800130927     c                   movel     v6AUTSDT      §AUTSDT
377900130927     c                   movel     v6AUTRET      §AUTRET
378000130927     c                   movel     v6AUTCON      §AUTCON
378100130927     c                   movel     v6AUTCRS      §AUTCRS
378200130927     c                   movel     v6AUTVTCC     §AUTVTCC
378300130927     c                   movel     v6AUTMTCC     §AUTMTCC
378400130927     c                   movel     v6AUTSDTC     §AUTSDTC
378500130927     c                   movel     v6AUTRETC     §AUTRETC
378600130927     c                   movel     v6AUTCRSC     §AUTCRSC
378700130927     c                   movel     v6AUTCONC     §AUTCONC
378800130927     c                   movel     v6RIEABI      §RIEABI
378900130927     c                   movel     v6AFFMTC      §AFFMTC
379000130927     c                   movel     v6AFFCON      §AFFCON
379100130927     c                   movel     v6AFFCRS      §AFFCRS
379200130927     c                   movel     v6AFFGVSTD    §AFFGVSTD
379300130927     c                   movel     v6AFFAGV      §AFFAGV
379400130927     c                   movel     v6AFFCPSTD    §AFFCPSTD
379500130927     c                   movel     v6AFFARRM     §AFFARRM
379600130927     c                   movel     v6AUTCPRRE    §AUTCPRRE
379700130927     c                   movel     v6AUTSTDCC    §AUTSTDCC
379800130927     c                   movel     v6AUTRFCIT    §AUTRFCIT
379900130927     c                   movel     v6AUTRFCOP    §AUTRFCOP
380000130927     c                   movel     v6AUTRFAFD    §AUTRFAFD
380100130927     c                   movel     v6AUTRFPRE    §AUTRFPRE
380200131002     c     ' ':'N'       xlate     uteautds      uteautds
380300130927     c                   movel     uteautds      §UTEdsaut
380400020329     C                   Movel     DUte01        UteFaf
380500100115      * Flags
380600100115     c                   movel     V1Cmopr       §UT2mopr
380700100115     c                   movel     dUte02        UteFa1
380800011114      *
380900011114     C                   ENDSR
381000011114      *--------------------------------------------------------------------------------------------*
381100011114      * Aggancio File Utenti aziendali
381200011114      *--------------------------------------------------------------------------------------------*
381300011218     C     ChnUte        BEGSR
381400011218     C                   setoff                                       3031
381500011210      *
381600011218     C     *IN31         doueq     *OFF
381700011114      *
381800011114      * A seconda del modo richiesto aggancio il file
381900011218      *  Modalità 1:utilizzo v.l AzUte01l, gestisco l'allocazione
382000011114     C     $Mod          ifeq      '1'
382100011218     C     I31Opz        Ifeq      '2'
382101170712     C     I31Opz        oreq      'S'
382200100115     c     I31opz        oreq      '9'
382300011210     C     $Ute          chain     AzUte01l                           3031
382400011218      *
382500011218      * non alloco il record
382600011218     C                   else
382700011218     C     $Ute          chain(n)  AzUte01l                           30
382800011114     C                   endif
382900011218     C                   endif
383000011114      *
383100011218      *  Modalità 2:utilizzo v.l AzUte02l, gestisco il riposizionamento
383200011114     C     $Mod          ifeq      '2'
383300011218     C     K02Ute        chain     AzUte02l                           30
383400011115      *
383500011115      * Riaggancio il record di dettaglio se sono in modifica
383600011203     C     I31Opz        Ifeq      '2'
383601170712     C     I31Opz        oreq      'S'
383700011210     C     I31Ute        chain     AzUte01l                           3031
383800011115     C                   endif
383900011218     C                   endif
384000011115      *
384100011210      * Record allocato
384200011210     C     *in31         ifeq      *on
384300011210     C                   clear                   O31Msg
384400011210     C                   clear                   O31Tfu
384500011211      *
384600011210     C                   move      'E'           O31Err
384700020315     C                   movel     msg(6)        O31Msg
384800011210     C                   movel     'S'           $Fine                          *fine pgm
384900011210     C                   endif
385000011210     C                   enddo
385100011210      *
385200011115     C                   ENDSR
385300011114      *--------------------------------------------------------------------------------------------*
385400011119      * Operazioni iniziali
385500011114      *--------------------------------------------------------------------------------------------*
385600011119     C     *inzsr        BEGSR
385700011114      *
385800011114      * Ricevimento parametri
385900011114     C     *ENTRY        PLIST
386000011114     C                   PARM                    kpjba
386100011203     C                   PARM                    Tibs31Ds
386200160830     c                   parm                    Tibs31Ds2
386300020408      *
386400020408     C                   z-add     1             CODUT
386500020408     C                   call      'X§PARUT'
386600020408     C                   parm                    UT§DSE0F
386700020516     C                   movel     RAGUT         RSUT
386800011114      *
386900011114      * Chiavi di lettura
387000011114     C     K02Ute        KLIST                                                  *tntbe01l
387100011114     C                   KFLD                    $Fil              3 0
387200020329     C                   KFLD                    $cdu              3 0
387300021003      * Chiavi di lettura per lettura file con campi a video
387400021003     C     K02UteV       KLIST                                                  *tntbe01l
387500021003     C                   KFLD                    $FilV             3 0
387600021003     C                   KFLD                    V1CCdu            3 0
387700130705      * Key per Tabella AUT
387800130705     c     TBE01L        Klist
387900130705     c                   Kfld                    TBEcod
388000130705     c                   Kfld                    TBEke1
388100011114      *
388200011114      * Imposto tipo opzione in testata a seconda del
388300011114      * campo opzione della DS
388400011114     C                   SELECT
388500011114      * Inserimento
388600011203     C     I31Opz        Wheneq    'I'
388700011120     C                   eval      VTCOpz = Opz(1)
388800011114      * Modifica
388900011203     C     I31Opz        Wheneq    '2'
388901170712     C     I31Opz        oreq      'S'
389000011120     C                   eval      VTCOpz = Opz(2)
389100011114      * Copia
389200011203     C     I31Opz        Wheneq    '3'
389300011120     C                   eval      VTCOpz = Opz(3)
389400130705      * Visualizzazione
389500011203     C     I31Opz        Wheneq    '5'
389600011120     C                   eval      VTCOpz = Opz(4)
389700011114     C                   ENDSL
389800011114      *
389900011114      * Inizializzo campi di work
390000011114     C                   move      *zeros        $Fil
390100020329     C                   move      *zeros        $cdu
390200011114     C                   move      *blanks       $Ute
390300080218     C                   move      *blanks       $Mod
390400130705
390500130705      * carico in sk le abilitazioni dei profili STANDARD
390600130705     c                   exsr      ABISTD
390700011114      *
390800011114     C                   ENDSR
390900130705
391000130705      /free
391100130705       //--------------------------------------------------------------
391200130705       //?Carico abilitazioni per profili standard.                    ?
391300130705       //--------------------------------------------------------------
391400130705       BEGSR  ABISTD;
391500130705
391600130705         clear xx;
391700130705         clear skUTE;
391800130705         clear skABI;
391900130705         clear skAUT;
392000130705         clear skFUN;
392100130705         clear skABI1;
392200130705
392300130705       //?Carico gli utenti standard in SK + abilitazioni
392400130705         exec sql
392500130705         DECLARE AZUTE cursor for
392600130705         SELECT * from AZUTE00F
392700130705         ORDER BY UTEute;
392800130705
392900130705       //?Apertura del cursore
393000130705         exec sql
393100130705         OPEN AZUTE;
393200130705
393300130705         IF  sqlcode < 0;
393400130705           wEnd = *on;
393500130705         ENDIF;
393600130705
393700130705         DOW  not wEnd;
393800130705           exec sql
393900130705           FETCH next from AZUTE into :azuteds;
394000130705           IF  sqlcod = 100 or sqlcod < 0;
394100130705             wEnd = *on;
394200130705             leave;
394300130705           ENDIF;
394400130705
394500130705       //?solo utenti standard
394600130705           dUte02 = UTEfa1;
394700130705           IF  §UT2mopr = 'S';
394800130705             xx += 1;
394900130705             skUTE(xx) = UTEute;
395000130705             skABI(xx) = UTEfaf;
395100130705             skAUT(xx) = UTEaut;
395200130705             skFUN(xx) = UTEfun;
395300130705             skABI1(xx) = UTEfa1;
395400130705           ENDIF;
395500130705
395600130705         ENDDO;
395700130705
395800130705       //?Chiusura del cursore
395900130705         exec sql
396000130705         CLOSE AZUTE;
396100130705
396200130705       ENDSR;
396300130705
396400130705      /end-free
396500130705
396600130705     P*--------------------------------------------------
396700130705     P* Procedure name: GetProfiloUtenteProj
396800130705     P* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
396900130705     P* Returns:        Profilo utente in Proj.
397000130705     P* Parameter:      priProfiloUtente => Profilo Utente
397100130705     P*--------------------------------------------------
397200130705     P GetProfiloUtenteProj...
397300130705     P                 B
397400130705     D GetProfiloUtenteProj...
397500130705     D                 PI             1A
397600130705     D  priProfiloUtente...
397700130705     D                               10A
397800130705
397900130705     D retField        S              1A   STATIC
398000130705
398100130705      /FREE
398200130705
398300130705       CLEAR retField;
398400130705
398500130705       EXEC SQL
398600130705         SELECT CASE
398700130705                WHEN utnAnn = '1' THEN 'A'
398800130705                WHEN utnGrp = 999 THEN 'D'
398900130705                ELSE 'S'
399000130705                END
399100130705           INTO :retField
399200130705           FROM anutn00f
399300130705           WHERE utnPrf = :priProfiloUtente
399400130705       ;
399500130705
399600130705       SELECT;
399700130705         WHEN sqlCode < *ZERO;
399800130705           DUMP(A);
399900130705           retField = '?';
400000130705       ENDSL;
400100130705
400200130705       RETURN retField;
400300130705
400400130705      /END-FREE
400500130705     P GetProfiloUtenteProj...
400600130705     P                 E
400700140522
400800140522      *----------------------------------------------------
400900140523      * Procedure name: IsFilMess
401000140523      * Purpose:        Filiale utente Messaggeria
401100140522      * Returns:        ON - OFF
401200140522      *----------------------------------------------------
401300140523     p IsFilMess       B
401400140523     d IsFilMess       PI              n
401500140522
401600140522     d isErrore        s               n   inz
401700140522
401800140522      /free
401900140522
402000140522       clear OG140;
402100140522       clear OG143;
402200140526       kfil = %int(V1Cfil);
402300140526       chain (kfil) AZORG01L;
402400140522       IF  %found(AZORG01L);
402500140522         OG140 = ORGde0;
402600140522         OG143 = ORGde3;
402700140522       ENDIF;
402800140522
402900140523       IF og143.§OGntw = 'MES' and og140.§OGapo <> %editc(ORGfil:'X');
403000140522         return *on;
403100140522       ELSE;
403200140522         return *off;
403300140522       ENDIF;
403400140522
403500140522      /end-free
403600140523     p IsFilMess       E
403700140522
403800011114     O*--------------------------------------------------------------------------------------------*
403900011114** -OPZ-
404000011114  Inserimento
404100011114   Modifica
404200011114    Copia
404300011114Visualizzazione
404400011114** -MSG-
404500050609TIBS31R- Profilo obbligatorio                                                  1
404600050609TIBS31R- Profilo già esistente                                                 2
404700050609TIBS31R- Codice utente già presente per la filiale                             3
404800050609TIBS31R- Immettere entrambi i campi                                            4
404900050609TIBS31R- Incongruenza utente posta/filiale                                     5
405000050609TIBS31R- Record allocato                                                       6
405100130709TIBS31R- Filiale di appartenenza non corretta                                  7
405200050609TIBS31R- Codice utente obbligatorio                                            8
405300050609TIBS31R- Data scadenza validità obbligatoria                                   9
405400050609TIBS31R- Funzione aziendale obbligatoria                                      10
405500050609TIBS31R- Livello di autorità obbligatorio                                     11
405600050609TIBS31R- Codice autorizzazione non valido                                     12
405700130709TIBS31R- Filiale di appartenenza obbligatoria                                 13
405800050609TIBS31R- Data scadenza validità non corretta                                  14
405900050609TIBS31R- Funzione aziendale non corretta                                      15
406000050609TIBS31R- Livello di autorità non corretto                                     16
406100050609TIBS31R- Profilo posta AS/400 errato                                          17
406200050609TIBS31R- Autorizzazione non utilizzabile da utenti di sede                    18
406300050609TIBS31R- Profilo inesistente nel modulo base                                  19
406400081014TIBS31R- Codice autorizzazione obbligatorio                                   20
406500100114TIBS31R- Descrizione obbligatoria                                             21
406600100114TIBS31R- Modifica del profilo utente sul sistema non riuscita - vedi JOBLOG   22
406700130930TIBS31R- Se impostato AUTORITA AUT uguale a NO i flag devono essere = a N     23
406800131014TIBS31R- Se impostato AUTORITA AUT diversa da NO almeno un flag deve essere S 24
406900131014TIBS31R- Se Profilo di filiale i flag raggruppamento SEDE devono essere = N   25
407000150618TIBS31R- Filiale ufficio errata                                               26
