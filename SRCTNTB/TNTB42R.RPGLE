000001031118     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000002031118     H*PARMS ACTGRP(YFICN51)
000100020212      *---------------------------------------------------------------*
000200031117      * Gestione tabella "CAF" = Causali autofatturazione fornitori   *
000300020212      *---------------------------------------------------------------*
000400020212
000500020212     H DECEDIT('0,') DATEDIT(*DMY.)
000600020212
000700020212      *---------------------------------------------------------------*
000800020212      *   A R C H I V I                                               *
000900020212      *---------------------------------------------------------------*
001000020212      *
001100031127     Fantxt01L  iF   E           K DISK
001101031127     Fansif01L  iF   E           K DISK
001102031127     FTNTBE01L  UF A E           K DISK
001400020212      *
001500031117     FTNTB42D   CF   E             WORKSTN
001600020212
001700020212      *---------------------------------------------------------------*
001800020212      *   S C H I E R E                                               *
001900020212      *---------------------------------------------------------------*
002000020212      *
002100020215     D Opz             s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
002200020212      *
002300021025     D Msg             s             78    dim(16) ctdata perrcd(1)             Messaggi video
002400020212      *
002500020215     D NmFl            s             10    dim(10)                              Campi per ricerca
002600020913      *
002700031117     D imp             s             10  3 dim(12)                              Orari
002701031117     D mes             s              1    dim(12)                              Orari
002800020212
002900020212      *---------------------------------------------------------------*
003000020212      *   S T R U T T U R E   D A T I                                 *
003100020212      *---------------------------------------------------------------*
003200020212      *
003300020212      * Parametri
003400020212     D KPJBA         e ds
003401031118     D xatbds        e DS
003402031118     D soc001        E DS                  EXTNAME(xsoc001ds)
003403031118     D xsocds          DS          1000
003404031118     D tibs02ds      E DS
003500020212      *
003501031117     D Tntbeds       e ds                  extname(tntbe00f) inz
004000020212      *
004100031117      * Tabella CAF
004200031117     D DCAF          e ds                  inz
004201031118     D Ptr             S               *
004202031118     D                                     INZ(%ADDR(§CAFMES1))
004203031118     D CAFMES          S                   LIKE(§CAFMES1)
004204031118     D                                     DIM(12)
004205031118     D                                     BASED(Ptr)
004206031118     D Ptr1            S               *
004207031118     D                                     INZ(%ADDR(§CAFIMP1))
004208031118     D CAFIMP          S                   LIKE(§CAFIMP1)
004209031118     D                                     DIM(12)
004210031118     D                                     BASED(Ptr1)
004800020724
004900020724     D TIBS34DS      e ds                  inz
005000020724     D DDATIUTE      e ds                  inz
005100020724     D AZUTEDS       e ds                  inz extname(AZUTE00F)
005101031118     d angy4Z        e ds                  extname(angy4Zds)
005200020226
005300020226     D WLBDAT          ds                  inz
005400020226     D  G02DAT                 1      8  0
005500020226     D  G02INV                 9     16  0
005600020226     D  G02ERR                17     17
005700020226     D  G02TGI                18     22  0
006500020212
006600020212      *---------------------------------------------------------------*
006700020212      *   V A R I A B I L I                                           *
006800020212      *---------------------------------------------------------------*
006900020212      *
007000020212     D XX              s              2  0 inz
007800020913     D TipVid          s              1a   inz('0')
007900020212     D Win             s             99a   inz(*zeros)
008000020301     D wTasto          s              2a   inz(*zeros)
008100031118     d totimp          s             10  3
008200031118     d mese            s             12
008400020212
008500020212      *---------------------------------------------------------------*
008600020212      *   M A I N   L I N E                                           *
008700020212      *---------------------------------------------------------------*
008800020212      *  Riepilogo indicatori utilizzati:                             *
008900020212      *  --------------------------------                             *
009000020212      *  01 - Record inesistente (inserimento)                        *
009100020212      *  02 - Record esistente   (modifica)                           *
009300020301      *  20 - Comodo                                                  *
009400020212      *  22 - Errori in scrittura record (WRITE)                      *
009500020212      *---------------------------------------------------------------*
010000020212      * Gestione video
010100031117     C                   do        *hival
010101031117     c                   select
010300031117     C                   when      TipVid='1'
010301031117     c                   exsr      gesv1
010302031117     C                   when      TipVid='2'
010303031117     c                   exsr      gesv2
010304031117     C                   when      TipVid='A'
010305031118     c                   exsr      gesw1
010306031117     c                   other
010307031117     c                   exsr      fine
010600031118     C                   endsl
010700020212     C                   enddo
010800020212      *
026600020913      *---------------------------------------------------------------*
026700031117      * Fine   - Tasto funzionale F03 -> Fine programma               *
026800020913      *---------------------------------------------------------------*
026900031117     C     Fine          BEGSR
027000020913      *
027100031117     C                   seton                                        lr        fine pgm
027101031117     c                   return
027200020913      *
027300020913     C                   ENDSR
027400020212      *---------------------------------------------------------------*
027500020212      * GESV1  - Gestione videata selezione codice tabella            *
027600020212      *---------------------------------------------------------------*
027700020212     C     GESV1         BEGSR
027900031117      *
027901031118     C                   clear                   V1cau
027902031118     C                   clear                   V1des
027903031118     C                   move      '?'           V1cau
028400020913      * Scrivo la testata
028500020913     C                   clear                   T1opz
028700020913      *
028701031117     c                   do        *hival
028702031118     C                   write     tb42T1
028800020913      * Se esistono errori sulla videata
028900020913      * emetto la write del formato a indicatori spenti per vedere
029000020913      * le eventuali decodifiche
029100020913     C                   if        *in99
029200020913     C                   movea     *in           Win
029400031117     C                   write     tb42V1
029500020913     C                   movea     Win           *in
029600020913     C                   endif
029700020913      *
029800031117     C                   exfmt     tb42V1
029900020913     C                   eval      *in99 = *off
030000020913     C                   clear                   V1MSG
030100020913      *
030300020913      * F03=Fine
030400031117w   1C                   if        *inKC
030500031117     C                   exsr      fine
030501031117     c                   end
031300020212      * Ricerche
031400020212     C                   clear                   XX
031500020212     C                   clear                   NMFL
031600031117     C     '?'           scan      V1cau                                  20
031700020301if  1C                   if        *in20
031800020212     C                   add       1             XX
031900031117     C                   eval      NMFL(XX) = 'V1CAU     '
032000020212     C                   exsr      SEARCH
032100020212e   1C                   endif
032300020301      * Controllo e decodifiche dati immessi a video
032400020301if  1C                   if        not *in99
032500020212     C                   exsr      CTRV1
032600020301e   1C                   endif
032700020212      *
032800020212      * Passaggio alla videata di dettaglio
032900020212if  1C                   if        not *in99
033100020212     C                   eval      TipVid = '2'
033101031117     c                   leave
033200020212e   1C                   endif
033201031117     c                   enddo
033300020212      *
033400031117     C                   ENDSR
034800020212      *---------------------------------------------------------------*
034900020212      * CTRV1  - Controllo e decodifica prima videata                 *
035000020212      *---------------------------------------------------------------*
035100020212     C     CTRV1         BEGSR
035200020212      *
035400020212     C                   clear                   V1msg
035600031117     C                   clear                   V1des
035700031117      * Causale obbligatoria
035800031117if  1C                   if        V1cau = *blanks
035900031117     C                   seton                                        5199
036000031118     C                   eval      V1msg = Msg(01)
036200031117x   1C                   else
036300031117      * Controllo/Decodifica causale
036301031118     C                   clear                   Tibs02DS
036302031118     C                   eval      t02tla = 'L'
036303031118     C                   movel(p)  v1cau         t02ke1
036304031118     C                   movel     'C'           t02mod
036305031118     C                   movel     'CAF'         t02cod
036307031118     C                   CALL      'TIBS02R'
036308031118     C                   parm                    KPJBA
036309031118     C                   parm                    TIBS02DS
037000020212if  2C                   if        T02err = *blanks
037100031117     C                   movel     T02uni        Dcaf
037200031118     C                   movel     §cafdes       V1des
037300020212x   2C                   else
037400031118     C                   clear                   dcaf
037600020212e   1C                   endif
037601031117e   1C                   endif
037700020212      *
037800031117     C                   ENDSR
038900020212      *---------------------------------------------------------------*
039000020212      * SEARCH - Ricerche                                             *
039100020212      *---------------------------------------------------------------*
039200020212     C     SEARCH        BEGSR
039300020301      *
039400020301     C                   eval      *in99 = *on
039500020212      *
039600020212do  1C                   do        10            XX
039700020212      *
039800020212if  2C                   if        NMFL(XX) = *blanks
039900020212     C                   leave
040000020212e   2C                   endif
040100020212      *
040200020212sel 2C                   select
040300031117      * Causale
040400031117w   2C                   when      NMFL(XX) =  'V1CAU     '
040401031118     C                   clear                   Tibs02DS
040402031118     C                   eval      t02tla = 'L'
040403031118     C                   movel(p)  v1cau         t02ke1
040404031118     C                   movel     'R'           t02mod
040405031118     C                   movel     'CAF'         t02cod
040407031118     C                   CALL      'TIBS02R'
040408031118     C                   parm                    KPJBA
040409031118     C                   parm                    TIBS02DS
040410031118    2C                   IF        T02err = *blanks
040411031118     c                   movel     t02ke1        v1cau
040412031118     c                   movel     t02uni        dcaf
040413031118     c                   movel     §cafdes       v1des
040414031118     c                   else
040415031118     c                   clear                   v1des
040416031118     C                   seton                                        51  99
040417031118     C                   MOVEL     MSG(01)       v1msg
040418031118     C                   ENDIF
041700020212e   2C                   endsl
041900020212e   1C                   enddo
042000020212      *
042100020212     C                   ENDSR
042300020212      *---------------------------------------------------------------*
042400020212      * GESV2  - Gestione videata dettaglio dati                      *
042500020212      *---------------------------------------------------------------*
042600020212     C     GESV2         BEGSR
042700020212      *
042800020212      * Inizializzazione videata
043000020212     C                   exsr      CarV2
043500020212      *
043501031117     c                   do        *hival
043502031118      * Scrivo la testata
043503031118     C                   write     tb42T1
043600020212      * Se esistono errori sulla videata
043700020212      * emetto la write del formato a indicatori spenti per vedere
043800020212      * le eventuali decodifiche
043900020212     C                   if        *in99
044000020212     C                   movea     *in           Win
044200031117     C                   write     tb42V2
044300020212     C                   movea     Win           *in
044400020212     C                   endif
044500020212      *
044501031118if  1C                   if        *in02
044502031118     C                   write     TB42V2
044503031118     C                   exfmt     PROTECT
044504031118x   1C                   else
045000031117     C                   exfmt     tb42V2
045001031118     c                   end
045200020212     C                   eval      *in99 = *off
045300020301     C                   clear                   V2MSG
045500020212      *
045600020212sel 1C                   select
045700020212      * F03=Fine
045800020212w   1C                   when      *inKC
045900031117     C                   exsr      fine
046100020212      * F12=Ritorno
046200020212w   1C                   when      *inKL
046201031117     c                   Unlock    Tntbe01l
046300031117     C                   eval      tipvid = '1'
046400031117     C                   leave
046500020212e   1C                   endsl
046700020212      * Controllo dati immessi a video
046800020212     C                   exsr      CTRV2
047000020212      * Aggiornamento se non ci sono errori
047100020212if  1C                   if        not *in99
047200020212     C                             and (*inKF or *inKE or *inKQ)
048200020226     C                   eval      TipVid = 'A'
048201031117     C   kq              movel     'A'           TBEatb
048202031117     c                   leave
048300020212e   1C                   endif
048301031117     c                   enddo
048400020212      *
048500031117     C                   ENDSR
048700020212      *---------------------------------------------------------------*
048800020212      * CARV2  - Caricamento dati seconda videata                     *
048900020212      *---------------------------------------------------------------*
049000020212     C     CARV2         BEGSR
049100020212      *
049200031127     C                   clear                   Tb42v2
049201031127     C                   clear                   T1opz
049202031117     C                   clear                   tntbeds
049300031117     C                   movea     '00'          *in(01)
049400020212      *
049500020212      * Aggancio la tabella, se trovo il codice sono in modifica
049600020212      * o ripristino (se record annullato), altrimenti in immissione
049701031117     C                   movel     T02cod        TBEcod
049702031117     C                   movel(p)  V1cau         TBEke1
049705031117     C     Ktbe          chain     TNTBE01L
049900020212if  1C                   if        %found(TNTBE01L)
050100020212      * MODIFICA/RIPRISTINO
050200031117     C                   movel     TBEuni        Dcaf
050300020212if  2C                   if        TBEatb = *blanks
050301031118     C                   setoff                                       02
050500020212     C                   eval      T1opz  = Opz(02)
050600020212x   2C                   else
050700031118     C                   seton                                        02
050800031118     C                   eval      T1opz  = Opz(05)
050900020212e   2C                   endif
051100020212x   1C                   else
051300020212      * IMMISSIONE
051400031117     c                   clear                   Dcaf
051500031117     C                   seton                                        01
051600020212     C                   eval      T1opz  = Opz(01)
051800020212e   1C                   endif
051900020212      *
052000031117     C                   move      V1cau         V2cau
052001031118     c                   if        not *in01
052200031127     C                   eval      V2csoc = §cafsoc
052201031127     C                   eval      V2ctes = §cafcdtes
052202031127     c                   clear                   v2dtes
052203031127     c     ktxt          chain     antxt01l
052204031127     c                   if        %found(antxt01l)
052205031127     c                   eval      v2dtes = txttesrig
052206031127     c                   end
052207031127     C                   eval      V2des = §cafdes
052300031118     C                   eval      V2tip  = §caftip
052400031118     C                   eval      V2qta  = §cafqta
052500031118     C                   eval      V2tab  = §caftab
052501031118     c                   exsr      sry4z
052600031118     C                   eval      V2mes1 = §cafmes1
052601031118     C                   eval      V2mes2 = §cafmes2
052602031118     C                   eval      V2mes3 = §cafmes3
052603031118     C                   eval      V2mes4 = §cafmes4
052604031118     C                   eval      V2mes5 = §cafmes5
052605031118     C                   eval      V2mes6 = §cafmes6
052606031118     C                   eval      V2mes7 = §cafmes7
052607031118     C                   eval      V2mes8 = §cafmes8
052608031118     C                   eval      V2mes9 = §cafmes9
052609031118     C                   eval      V2mes10= §cafmes10
052610031118     C                   eval      V2mes11= §cafmes11
052611031118     C                   eval      V2mes12= §cafmes12
052612031118     C                   eval      V2imp1 = §cafimp1
052613031118     C                   eval      V2imp2 = §cafimp2
052614031118     C                   eval      V2imp3 = §cafimp3
052615031118     C                   eval      V2imp4 = §cafimp4
052616031118     C                   eval      V2imp5 = §cafimp5
052617031118     C                   eval      V2imp6 = §cafimp6
052618031118     C                   eval      V2imp7 = §cafimp7
052619031118     C                   eval      V2imp8 = §cafimp8
052620031118     C                   eval      V2imp9 = §cafimp9
052621031118     C                   eval      V2imp10= §cafimp10
052622031118     C                   eval      V2imp11= §cafimp11
052623031118     C                   eval      V2imp12= §cafimp12
054300031118     c                   end
054400020212     C                   ENDSR
055600020212      *---------------------------------------------------------------*
055700020212      * CTRV2  - Controllo e decodifica seconda videata               *
055800020212      *---------------------------------------------------------------*
055900020212     C     CTRV2         BEGSR
056000020212      *
056001031118     c                   movea     '000000'      *in(52)
056200020212     C                   clear                   V2msg
056201031117     c                   do
056400020212      * Non si fanno i controlli se richisto l'annullamento
056500031117     C                   if        not *inkq
058400020212      * Descrizione obbligatoria
058500031117w   1C                   If        V2des =  *blanks
058600020913     C                   seton                                        52  99
058700031117     C                   movel     Msg(02)       V2msg
058800031117     C                   leave
058900020913     c                   endif
059100031117      * tipo anagrafico
059200031117w   1C                   If        V2tip = *blanks
059300020913     C                   seton                                        53  99
059400031117     C                   movel     Msg(03)       V2msg
059500031117     C                   leave
059600020913     c                   endif
059601031117      * Richiesta qta
059602031117w   1C                   If        V2qta = *blanks
059603031117     C                   seton                                        54  99
059604031117     C                   movel     Msg(04)       V2msg
059605031117     C                   leave
059606031117     c                   endif
060800031117      * Codice tabella Y4Z
060900031117w   1C                   If        V2tab  = *blanks
061000031117     C                   seton                                        55  99
061100031117     C                   movel     Msg(05)       V2msg
061200031117     C                   leave
061201031117     c                   else
061202031117     c                   exsr      sry4z
061203031118     C   55              LEAVE
061300020913     c                   endif
061301031127      * Codice società testo
061302031127     c                   clear                   v2dtes
061303031127w   1C                   If        V2csoc = *blanks
061304031127     C                   seton                                        58
061306031127     c                   else
061307031127     c     v2csoc        chain     ansif01l                           58
061312031127     c                   endif
061313031127     c                   if        *in58
061314031127     c                   seton                                        99
061315031127     C                   movel     Msg(08)       V2msg
061316031127     C                   LEAVE
061317031127     c                   endif
061318031127      * Codice testo
061319031127w   1C                   If        V2ctes = *blanks
061320031127     C                   seton                                        59
061321031127     c                   else
061322031127     c     ktxt          chain     antxt01l                           59
061323031127     c                   endif
061324031127     c                   if        *in59
061326031127     c                   seton                                        99
061327031127     C                   movel     Msg(09)       V2msg
061328031127     C                   LEAVE
061329031127     c                   else
061330031127     c                   eval      v2dtes = txttesrig
061331031127     c                   endif
061332031117      * Passo gli importi e i mesi nelle schiere
061333031117     c                   move      v2mes1        mes(1)
061334031117     c                   move      v2mes2        mes(2)
061335031117     c                   move      v2mes3        mes(3)
061336031117     c                   move      v2mes4        mes(4)
061337031117     c                   move      v2mes5        mes(5)
061338031117     c                   move      v2mes6        mes(6)
061339031117     c                   move      v2mes7        mes(7)
061340031117     c                   move      v2mes8        mes(8)
061341031117     c                   move      v2mes9        mes(9)
061342031117     c                   move      v2mes10       mes(10)
061343031117     c                   move      v2mes11       mes(11)
061344031117     c                   move      v2mes12       mes(12)
061345031117     c                   movea     mes           mese
061346031117     c* mesi
061347031117     c                   if        mese = ' '
061348031117     C                   seton                                        56  99
061349031117     C                   movel     Msg(06)       V2msg
061350031117     C                   leave
061351031117     c                   end
061352031117     c                   move      v2imp1        imp(1)
061353031117     c                   move      v2imp2        imp(2)
061354031117     c                   move      v2imp3        imp(3)
061355031117     c                   move      v2imp4        imp(4)
061356031117     c                   move      v2imp5        imp(5)
061357031117     c                   move      v2imp6        imp(6)
061358031117     c                   move      v2imp7        imp(7)
061359031117     c                   move      v2imp8        imp(8)
061360031117     c                   move      v2imp9        imp(9)
061361031117     c                   move      v2imp10       imp(10)
061362031117     c                   move      v2imp11       imp(11)
061363031117     c                   move      v2imp12       imp(12)
061364031117     c                   xfoot     imp           totimp
061365031117     c* importi
061366031204     c                   if        totimp = 0 and v2qta = 'S'
061367031117     C                   seton                                        57  99
061368031117     C                   movel     Msg(07)       V2msg
061369031117     C                   leave
061370031117     c                   end
061371031118     c                   end
061372031117     c                   enddo
061373031203     c* controllo che se inserito un importo ci deve essere anche la scelta
061374031203     c* del mese
061375031203     c                   do        12            zz                2 0
061376031203     c                   if        imp(zz) <> 0 and mes(zz) = ' '
061378031203     C                   movel     'X'           mes(zz)
061380031203     c                   end
061381031203     c                   enddo
069300020212      *
069400031117     C                   ENDSR
069600020226      *---------------------------------------------------------------*
069700020226      * GESW1  - Gestione videata dati relativi alla trasmissione     *
069800020226      *---------------------------------------------------------------*
069900020226     C     GESW1         BEGSR
070100020226      * Inizializzazione videata
070300020226     C                   exsr      CARW1
070301031117     c*
070302031117     c                   do        *hival
071100031117     C                   exfmt     tb42W1
071400020301     C                   clear                   W1MSG
071700020226      * F12=Ritorno
071800031117w   1C                   if        *inKL
071901031117     C                   eval      TipVid = '2'
072000031117     C                   leave
072100031117e   1C                   end
072600020226      * Aggiornamento se non ci sono errori
072700031118if  1C                   if        *inKF
072800020226     C                   exsr      AGGTBE
072801031118     c                   leave
072900020226e   1C                   endif
072901031117e   1C                   enddo
073000020226      *
073100031117     C                   ENDSR
073300020226      *---------------------------------------------------------------*
073400020226      * CARW1  - Caricamento dati window                              *
073500020226      *---------------------------------------------------------------*
073600020226     C     CARW1         BEGSR
073900020226      *
075400020313     C                   eval      W1FTT = TBEftt
076600020226     C                   eval      W1FLT = TBEflt
076700020226     C                   eval      W1FTR = TBEftr
076800020226if  2C                   if        TBEdtr <> 0
076900020226     C                   clear                   WLBDAT
077000020226     C                   z-add     TBEdtr        G02inv
077100020226     C                   movel     '3'           G02err
077200020226     C                   call      'XSRDA8'
077300020226     C                   parm                    WLBDAT
077400020226     C                   z-add     G02dat        W1DTR
077600020226e   1C                   endif
077700020226      *
077800020226     C                   ENDSR
081800020212      *---------------------------------------------------------------*
081900020212      * AGGTBE * Aggiornamento tabella                                *
082000020212      *---------------------------------------------------------------*
082100020212     C     AGGTBE        BEGSR
083400020212      *   Immissione
083500031118w   2C                   if        *in01
083600020212     C                   exsr      RIETBE
083700020226     C                   clear                   TBEflt
083800020226     C                   clear                   TBEdtr
083900020212     C                   WRITE     TNTBE000                             22
084000031117      *   Modifica
084100031118w   2C                   else
084101031117     c                   if        tbeatb = 'A'
084102031117      * F16=Annullamento
084103031117     C                   clear                   TBEftr
084104031117     c                   else
084200020212     C                   exsr      RIETBE
084201031117     c                   end
085000020212     C                   UPDATE    TNTBE000
085200031118e   1C                   end
085400020212      * Torno alla prima videata che carico come da inizio pgm
085500020212     C                   movel     '1'           TipVid
086000020212     C                   ENDSR
086200020212      *---------------------------------------------------------------*
086300020212      * Riempimento file tabella
086400020212      *---------------------------------------------------------------*
086500020212     C     RIETBE        BEGSR
086600020212      *
086700031117     C                   clear                   Tntbeds
087400020212     C                   movel     T02cod        TBEcod
087401031118     C                   movel     T02apl        TBEapl
087500031117     C                   movel     V2cau         TBEke1
087800031117     C                   clear                   Dcaf
087900031127     C                   eval      §cafsoc  =  V2csoc
087901031127     C                   eval      §cafcdtes  =  V2ctes
087902031127     C                   eval      §cafdes  =  V2des
088000031118     C                   eval      §caftip  =  V2tip
088100031118     C                   eval      §cafqta  =  V2qta
088200031118     C                   eval      §caftab  =  V2tab
088300031117     C                   eval      cafmes  =  mes
088400031117     C                   eval      cafimp  =  imp
090100031117     C                   movel(p)  Dcaf          TBEuni
090200020212      *
090300020226     C                   movel     W1FTT         TBEftt
090400031118     C                   z-add     w1flt         TBEflt
090500020212     C                   clear                   TBEftr
090600020212     C
090700020212     C                   ENDSR
090702031118      *---------------------------------------------------------------*
090703031118     C*  Eseguo posizionamento su y4R
090704031118      *---------------------------------------------------------------*
090705031118     C     SRY4Z         BEGSR
090706031118     c                   clear                   xatbds
090707031118     c                   move      '1'           xtbric
090708031118     c                   move      xscsoc        xtbazi
090709031118     c                   move      *blank        xtbkey
090710031118     c                   move      'Y4Z'         xtbcod
090712031118     c                   movel(p)  v2tAB         xtbkey
090713031118     c                   call      'XATB'
090714031118     c                   parm                    xatbds
090715031118     c                   if        xtberr = '0'
090716031118     c                   movel     xtbuni        angy4Z
090717031118     C                   ELSE
090718031118     C                   seton                                        55  99
090719031118     C                   movel     Msg(05)       V2msg
090720031118     c                   end
090721031118     C                   ENDSR
090722031118      *---------------------------------------------------------------*
090723031118      * Operazioni Iniziali                                           *
090724031118      *---------------------------------------------------------------*
090725031118     C     *inzsr        BEGSR
090726031118      *
090727031118      * Ricezione parametri
090728031118     C     *entry        plist
090729031118     C                   parm                    KPJBA
090730031118      *
090731031118      * Definizioni chiavi di accesso
090732031118     C     Ktbe          klist
090733031118     C                   kfld                    TBEcod
090734031118     C                   kfld                    TBEke1
090735031127     C     Ktxt          klist
090736031127     C                   kfld                    v2csoc
090737031127     C                   kfld                    txtunita
090738031127     C                   kfld                    v2ctes
090739031127     C                   kfld                    txtlingua
090740031127     C                   kfld                    txtposrig
090741031127     c                   eval      txtlingua = '1'
090742031127     c                   eval      txtposrig = 'A'
090743031118     c* reperisce la società
090744031118     c                   exsr      repsoc
090745031118      *
090746031118      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
090747031118     C     *dtaara       define    §azute        azuteds
090748031118     C     *dtaara       define    §datiute      ddatiute
090749031118      *
090750031118     C                   clear                   AzUteDs
090751031118     C                   clear                   DDatiUte
090752031118     C                   clear                   Tibs34Ds
090753031118     C                   in(E)     *dtaara
090754031118if  1C                   IF        %Error  or  RSUT = *blanks
090755031118     C                   call      'TIBS34R'
090756031118     C                   parm                    Tibs34Ds
090757031118     C                   in        *dtaara
090758031118e   1C                   ENDIF
090759031118w   1C                   if        DUTERR = 'E'
090760031118     c                   exsr      fine
090761031118     c                   end
090762031118
090769031118     C                   eval      TipVid = '1'
090770031118
090771031118     c                   EndSr
090772031118     C*----------------------------------------------------*
090773031118     C* Reperimento dati società
090774031118     C*----------------------------------------------------*
090775031118     C     REPSOC        BEGSR
090776031118     C*
090777031118     C                   CALLB     'XSOC'
090778031118     C                   PARM                    TIPXSC            6
090779031118     C                   PARM                    SOCXSC            3
090780031118     C                   PARM                    CDSXSC            9 0
090781031118     C                   PARM                    MODXSC            3
090782031118     C                   PARM      *blanks       RTNXSC            1
090783031118     C                   PARM                    XSOCDS
090784031118     C                   PARM                    KPJBA
090785031118     C*
090786031118     C                   ENDSR
090800020212
090900020212** -OPZ-      *
091000020212  Inserimento
091100020212    Modifica
091300020212  Annullamento
091400020212Visualizzazione
091500020212   ANNULLATO
091600020212** -MSG-                                                                     *
091900031117Immettere la causale                                                           01
092000031117Immettere la descrizione                                                       02
092100031117Immettere il tipo anagrafco                                                    03
092101031117Immettere il flag di richiesta quantità                                        04
092300031117Immettere un codice tabella corretto                                           05
092900031117Immettere almeno un mese                                                       06
093000031117Immettere almeno un importo                                                    07
093100031127Società testo errata                                                           08
093200031127Codice testo errato                                                            09
