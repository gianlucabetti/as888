000100151028     /*END
000200020513      *-----------*
000300020513      *  TRUL31R  *  CARICAMENTO P.O. GESTIBILI
000400020513      *-----------*
000500020513      *
000600020513      * INPUT
000700020513      * -----
000800020513      *  I31ABI   Codice Abilitazione
000900020513      *  I31CDI   Codice Distretto Utente
001000040203      *  I31CAR   Codice Area      Utente
001100020513      *  I31CPO   Punto Operativo  Utente
001200020513      *
001300020513      * OUTPUT
001400020513      * ------
001500020513      *  O31POG   Schiera P.O. gestibili (250 elem.)
001600050608      *
001700050608      * 2° OUTPUT
001800050608      * ------
001900050608      *  O31DIG   Schiera Distretti gestibili (20 elem.)
002000050613      *  O31gtd   T=gestibili tutti i distretti del p.o. (esclusa SEDE )
002100050608      *  O31ARG   Schiera Aree      gestibili (50 elem.)
002200020513      *
002300020513      *--------------------------------------------------------------*
002400020513
002500041214     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
002600020513
002700020513     FAZORG01L  IF   E           K DISK
002800020513     FAZORG02L  IF   E           K DISK    rename(AZORG:AZORG02)
002900020513
003000020513      *
003100020513      * === COSTANTI =============================================== *
003200020513      *
003300020513     D MaxPOG          c                   const(250)
003400020513      *
003500020513      * === SCHIERE A TEMPO DI COMPILAZIONE ======================== *
003600020513      *
003700020513      * Messaggi di errore
003800020513     D MSG             s             78    dim(2) ctdata perrcd(1)
003900020513      *
004000020513      * === SCHIERE A TEMPO DI ESECUZIONE ========================== *
004100020513      *
004200050609      * P.O. gestibili di work e di output
004300020513     D $POG            s              3    dim(MaxPOG) inz(*zeros)
004400050609     D OPOG            s              3    dim(MaxPOG) inz(*zeros)
004500050608      * Distretti gestibili
004600050609     D $DIG            s              1    dim(20)
004700050609     D ODIG            s              1    dim(20)
004800050610     D cdiela          s              1    dim(20)
004900050608      * Aree gestibili
005000050608     D $ARG            s              3    dim(50) inz(*zeros)
005100050609     D OARG            s              3    dim(50) inz(*zeros)
005200050610     D carela          s              3  0 dim(50) inz(*zeros)
005300020513      *
005400020513      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
005500020513      *
005600020513      * Passaggio Parametri
005700020513     D KPJBA         e ds
005800020513     D TRUL31DS      e ds
005900050609     D TRUL31DS2     e ds
006000021107     D OG140         e ds
006100050614     D OG143         e ds
006200020513      *
006300020513      * Ridefinizione campi del file AZORG
006400020513      * (per semplificarne la pulizia con una banale CLEAR)
006500020513     D AZORGDS       e ds                  inz extname(AZORG00F)
006600020513      *
006700020513     D TRUL06DS      e ds                  inz
006800020513      *
006900020513      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
007000020513      *
007100020513      * === VARIABILI ============================================== *
007200020513      *
007300020513      * Indice schiere
007400020513     D XX              S              3  0 inz
007500050609     D yy              S              3  0 inz
007600050609     D zz              S              3  0 inz
007700050609     D ww              S              3  0 inz
007800050609     D wcaricaA        S              1    inz
007900050609     D wcaricaD        S              1    inz
008000050609     D noncarica       S              1    inz
008100050609     D wcpo            S                   like(i31cpo)
008200050609     D wcar            S                   like(i31car)
008300050609     D wcdi            S                   like(i31cdi)
008400020513
008500020513      *--------------------------------------------------------------*
008600020513      * === MAIN LINE PROGRAM ====================================== *
008700020513      *--------------------------------------------------------------*
008800020513      *
008900020513     C     *entry        plist
009000020513     C                   parm                    KPJBA
009100020513     C                   parm                    TRUL31DS
009200050608     C                   parm                    TRUL31DS2
009300020513      *
009400020513      * inizializzazione variabili
009500020513     C                   exsr      INZVAR
009600020513      *
009700020513      * Controllo parametri ricevuti
009800020513     C                   exsr      CTRPARM
009900020513      *
010000020513      * Intabellamento P.O. gestibili dal P.O. dell'utente
010100020513     C                   exsr      CARPOG
010200020513      *
010300020513      * Fine
010400020513     C                   EXSR      ENDPGM
010500020513
010600020513      *--------------------------------------------------------------*
010700020513      * === INIZIALIZZAZIONE VARIABILI ============================= *
010800020513      *--------------------------------------------------------------*
010900020513     C     INZVAR        BEGSR
011000020513      *
011100020513      * Impostazione iniziale dati di output
011200020513     C                   clear                   O31pog
011300050609     c                   if        %parms>2
011400050613     c                   clear                   o31dig
011500050613     c                   clear                   o31gtd
011600050613     c                   clear                   o31arg
011700050609     c                   endif
011800050609     c
011900020513     C                   reset                   $POG
012000050609     C                   reset                   $arG
012100050609     C                   reset                   $diG
012200050609     C                   reset                   OPOG
012300050609     C                   reset                   OarG
012400050609     C                   reset                   OdiG
012500020513     C                   reset                   XX
012600020513      *
012700020513     C                   reset                   AZORGDS
012800020513      *
012900020513     C     EndInzVar     ENDSR
013000020513
013100020513      *--------------------------------------------------------------*
013200020513      * === CONTROLLO PARAMETRI RICEVUTI =========================== *
013300020513      *--------------------------------------------------------------*
013400020513     C     CTRPARM       BEGSR
013500020513      *
013600020513      * - Codice Abilitazione non valido
013700020513if  1C                   if            I31abi <> *blanks
013800020513     C                             and I31abi <> 'NO'
013900020513     C                             and I31abi <> 'AZ'
014000160913     C                             and I31abi <> 'AL'
014100020513     C                             and I31abi <> 'DI'
014200021113     C                             and I31abi <> 'AO'
014300151029     C                             and I31abi <> 'AR'
014400151029     C                             and I31abi <> 'AP'
014500131010     C                             and I31abi <> 'AC'
014600160914     C                             and I31abi <> 'CL'
014700151029     C                             and I31abi <> 'RA'
014800151029     C                             and I31abi <> 'RC'
014900160913     C                             and I31abi <> 'RL'
015000020513     C                             and I31abi <> 'TP'
015100020513     C                             and I31abi <> 'PO'
015200050614     C                             and I31abi <> 'EE'
015300020513     C                   exsr      ENDPGM
015400020513e   1C                   endif
015500020513      *
015600020513     C                   ENDSR
015700020513
015800020513      *--------------------------------------------------------------*
015900020513      * === CARICAMENTO P.O. GESTIBILI DAL P.O. UTENTE ============= *
016000020513      *--------------------------------------------------------------*
016100020513     C     CARPOG        BEGSR
016200020513      *
016300020513sel 1C                   select
016400020513      * Utente senza alcuna abilitazione
016500020513w   1C                   when      I31abi = *blanks   or
016600020513     C                             I31abi = 'NO'
016700020513      * Utente con abilitazione a tutti i P.O.
016800020513w   1C                   when      I31abi = 'AZ'
016900020513     C                   exsr      PO_AZIENDA
017000160913w   1C                   when      I31abi = 'AL'
017100160913     C                   exsr      PO_AZIENDAlog
017200020513      * Utente con abilitazione ai P.O. del suo distretto
017300020513w   1C                   when      I31abi = 'DI'
017400020513     C                   exsr      PO_DISTRETTO
017500020513      * Utente con abilitazione ai P.O. della sua area
017600021113w   1C                   when      I31abi = 'AO'
017700020513     C                   exsr      PO_AREA
017800151029      * Utente con abilitazione ai P.O. del suo raggruppamento area EX AS400
017900151029w   1C                   when      I31abi = 'AR'
018000151029     C                   exsr      PO_AR
018100151029      * Utente con abilitazione ai P.O. del suo raggruppamento area POC
018200151029w   1C                   when      I31abi = 'AP'
018300151029     C                   exsr      PO_AP
018400160913      * Utente con abilitazione ai P.O. del suo raggruppamento area + filiali logistica
018500160913w   1C                   when      I31abi = 'RL'
018600160913     C                   exsr      PO_RL
018700151029      * Utente con abilitazione ai P.O. del suo raggruppamento area contabile
018800151029w   1C                   when      I31abi = 'AC'
018900151029     C                   exsr      PO_AC
019000160914      * Utente con abilitazione ai P.O. del suo raggruppamento area contabile + logistiche
019100160914w   1C                   when      I31abi = 'CL'
019200160914     C                   exsr      PO_CL
019300151029      * Utente con abilitazione ai P.O. del suo raggruppamento area
019400151029w   1C                   when      I31abi = 'RA'
019500151029     C                   exsr      PO_RA
019600151029      * Utente con abilitazione ai P.O. del suo raggruppamento area centralino
019700151029w   1C                   when      I31abi = 'RC'
019800151029     C                   exsr      PO_RC
019900020513      * Utente con abilitazione ai P.O. del suo terminal di partenza
020000020513w   1C                   when      I31abi = 'TP'
020100020513     C                   exsr      PO_TERMINALP
020200020513      * Utente con abilitazione al suo solo P.O.
020300020513w   1C                   when      I31abi = 'PO'
020400020513     C                   exsr      PO_PO
020500050614      * Utente con abilitazione ai P.O. esteri
020600050614w   1C                   when      I31abi = 'EE'
020700050614     C                   exsr      PO_ESTERI
020800020513      *
020900020513e   1C                   endsl
021000020513      *
021100020513     C                   ENDSR
021200020513
021300020513      *--------------------------------------------------------------*
021400020513      * === GESTIONE ABILITAZIONI PER P.O. ========================= *
021500020513      *--------------------------------------------------------------*
021600020513      *
021700020513      * === UTENTE ABILITATO A TUTTI I P.O. ======================== *
021800020513      *
021900020513     C     PO_AZIENDA    BEGSR
022000020513      *
022100050609     c                   eval      wcaricaA='S'
022200050609     c                   eval      wcaricaD='S'
022300020513     C     *loval        setll     AZORG
022400020513     C                   read      AZORG
022500020513do  1C                   dow       not  %eof(AZORG01L)
022600020513     C                   exsr      MEMOPO
022700020513     C                   read      AZORG
022800020513e   1C                   enddo
022900020513      *
023000050609     c                   movea     $pog          Opog
023100050609     c                   movea     $arg          Oarg
023200050609     c                   movea     $dig          Odig
023300020513     C                   ENDSR
023400160913      *______________________________________________________________
023500160913      * === UTENTE ABILITATO A TUTTI I P.O. + logistica              *
023600160913      *______________________________________________________________
023700160913     C     PO_AZIENDAlog BEGSR
023800160913      *
023900160913     C     *loval        setll     AZORG
024000160913     C                   read      AZORG
024100160913do  1C                   dow       not  %eof(AZORG01L)
024200160913     c                   eval      og143 = orgde3
024300160913     C                   exsr      MEMOPOlog
024400160913     C                   read      AZORG
024500160913e   1C                   enddo
024600160913      *
024700160913     c                   movea     $pog          Opog
024800160913     c*                  movea     $arg          Oarg
024900160913     c*                  movea     $dig          Odig
025000160913     C                   ENDSR
025100160913      *________________________________________________________________
025200020513      * === UTENTE ABILITATO AI P.O. DEL DISTRETTO ================= *
025300020513      *
025400020513     C     PO_DISTRETTO  BEGSR
025500160913      *________________________________________________________________
025600020513      *
025700020513      * Accedo al P.O. utente per recuperare il distretto
025800020513if  1C                   if        I31cdi = *blanks
025900050609     c                   eval      wcpo=i31cpo
026000020513     C                   exsr      REPDPO
026100050609     c                   eval      i31car=wcar
026200050609     c                   eval      i31cdi=wcdi
026300020513e   1C                   endif
026400020513      *
026500020513      * Ciclo di lettura P.O. del distretto
026600050609     c                   eval      wcaricaA='S'
026700050609     c                   eval      wcaricaD='S'
026800050609     c                   eval      wcdi=i31cdi
026900050609     c
027000050609     c                   exsr      DISTRETTO
027100050609     c
027200020513      *
027300050609     c                   movea     $pog          Opog
027400050609     c                   movea     $arg          Oarg
027500050609     c                   movea     $dig          Odig
027600050609     c
027700020513     C                   ENDSR
027800160913      *________________________________________________________________
027900050609      * === Carica da azorg02 tutti i p.o. del distretto =========== *
028000050609     c     DISTRETTO     begsr
028100160913      *________________________________________________________________
028200050609     C     wcdi          chain     AZORG02
028300050609if  1C                   if        %found(AZORG02L)
028400050609do  2C                   dou       %eof(AZORG02L)
028500050609     C                   exsr      MEMOPO
028600050609     C     wcdi          reade     AZORG02
028700050609e   2C                   enddo
028800050609e   1C                   endif
028900050609     c                   endsr
029000020513      *
029100160913      *________________________________________________________________
029200020513      * === UTENTE ABILITATO AI P.O. DELL'AREA ===================== *
029300020513     C     PO_AREA       BEGSR
029400160913      *________________________________________________________________
029500021113      *
029600021113      * Accedo al P.O. utente per recuperare il distretto/area
029700040203if  1C                   if        I31cpo <> *zeros
029800050609     c                   eval      wcpo=i31cpo
029900021113     C                   exsr      REPDPO
030000050609     c                   eval      i31car=wcar
030100050609     c                   eval      i31cdi=wcdi
030200040203e   1C                   endif
030300020513      *
030400040203if  1C                   if            I31cdi <> *blanks
030500040203     C                             and I31car <> *zeros
030600040203      *
030700050609     c                   eval      wcar=i31car
030800050609     c                   eval      wcdi=i31cdi
030900050609     c                   eval      wcaricaD='N'
031000050609     c                   eval      wcaricaA='S'
031100050609     c
031200050609     c                   EXSR      AREA
031300040203      *
031400040203x   1C                   else
031500040203      *
031600040203      * Ciclo di lettura P.O. e selezione per area
031700040203     C     *loval        setll     AZORG
031800040203     C                   read      AZORG
031900040203do  2C                   dow       not  %eof(AZORG01L)
032000040203if  3C                   if             ORGfva = *blanks
032100040203     C                             and (ORGfag = 'A'
032200040203     C                              or  ORGfag = 'F')
032300040203     C                             and  ORGcar = I31car
032400040203     C                   add       1             XX
032500040203     C                   move      ORGfil        $POG(XX)
032600040203e   3C                   endif
032700040203     C                   read      AZORG
032800040203e   2C                   enddo
032900050609     c
033000050609     c* Carico anche l'area
033100050609     c                   movel     i31car        $arg(1)
033200040203e   1C                   endif
033300020513      *
033400050609     c                   movea     $pog          Opog
033500050609     c                   movea     $arg          Oarg
033600020513     C                   ENDSR
033700160913      *________________________________________________________________
033800160913      * === Carica p.o. area da azorg02l =========================== *
033900050609     c     AREA          begsr
034000160913      *________________________________________________________________
034100050609      * Ciclo di lettura P.O. del distretto e dell'area
034200050609     C     Korg2         chain     AZORG02
034300050609if  2C                   if        %found(AZORG02L)
034400050609do  3C                   dou       %eof(AZORG02L)
034500050609     C                   exsr      MEMOPO
034600050609     C     Korg2         reade     AZORG02
034700050609e   3C                   enddo
034800050609e   2C                   endif
034900050609     c                   ENDSR
035000050609     c
035100160913      *________________________________________________________________
035200160913      * === UTENTE ABILITATO AI P.O. DEL TERMINAL DI PARTENZA ====== *
035300020513     C     PO_TERMINALP  BEGSR
035400160913      *________________________________________________________________
035500020513      *
035600020513     C                   reset                   TRUL06DS
035700021115     C                   movel     'L'           D06tla
035800021115     C                   movel     'PP'          D06cod
035900021115     C                   movel     I31cpo        D06key
036000020513     C                   movel(p)  TRUL06DS      KPJBU
036100020513     C                   call      'TRUL06R'
036200020513     C                   parm                    KPJBA
036300020513     C                   movel     KPJBU         TRUL06DS
036400020513if  1C                   if        D06err = *blanks
036500050609     C                   movea     D06lia        OPOG
036600020513e   1C                   endif
036700050609     c
036800050609     c* Carica se richeisto anche le aree e distretti a cui è
036900050609     c*  abilitato
037000050609     c                   exsr      CTR3parms
037100020513      *
037200020513     C                   ENDSR
037300050609     c*
037400050609     c* Vedo se caricare 3 parametro di output:aree e distretti ======
037500050609     c*
037600160913      *________________________________________________________________
037700050609     c     CTR3PARMS     begsr
037800160913      *________________________________________________________________
037900050609     c* Se richiesta in output anche schiera delle aree e distretti
038000050609    1c                   if        %parms>2
038100050610     c                   reset                   $arg
038200050610     c                   reset                   $dig
038300050609     c
038400050609     c* Leggo la schiera per vedere dei p.o. le aree e distretto:
038500050609     c*  se ne ho caricati tutti i p.o. --> passo indietro anche le aree/dis
038600050609     c                   eval      zz=1
038700050609    2c                   dow       Opog(zz)>*zeros
038800050609     c                   movel     Opog(zz)      wcpo
038900050609     c                   exsr      REPDpo
039000050609     c
039100050609     c* Carico tutti i p.o. dell'area (area e distetto non mi interess)
039200050610     c*   se non l'ho già elaborata
039300050610     c     wcar          lookup    carela                                 99
039400050610   2ac                   if        not *in99
039500050610     c                   z-add     1             ww
039600050610     c     000           lookup    carela(ww)                             99
039700050610     c   99              movel     wcar          carela(WW)
039800050610     c
039900050609     c                   clear                   $pog
040000050610     c                   clear                   xx
040100050609     c                   eval      wcaricaA='N'
040200050609     c                   eval      wcaricaD='N'
040300050609     c                   exsr      AREA
040400050609     c
040500050609     c* se tutti i p.o. dell'area sono anche nella schiera OPOG
040600050609     c*  in output do anche l'area
040700050609     c                   eval      ww=1
040800050609     c                   clear                   Noncarica
040900050609    3c                   dow       ww<=250  and $pog(WW)>*zeros
041000050609     c     $pog(WW)      lookup    Opog                                   99
041100050609     c                   if        not *in99
041200050609     c                   eval      Noncarica='N'
041300050609     c                   eval      ww=251
041400050609     c                   else
041500050609     c                   add       1             ww
041600050609     c                   endif
041700050609    3c                   enddo
041800050609     c
041900050609     c                   if        Noncarica=' '
042000050609     c                   eval      wcaricaA='S'
042100050609     c                   eval      wcaricaD='N'
042200050609     c                   exsr      CarAREADISTR
042300050609     c                   endif
042400050610   2ac                   endif
042500050609     c
042600050609     c* Carico tutti i p.o. del distre(area e distetto non mi interess)
042700050610     c*   se non l'ho già elaborato
042800050610     c     wcdi          lookup    cdiela                                 99
042900050610   2ac                   if        not *in99
043000050610     c                   z-add     1             ww
043100050610     c     ' '           lookup    cdiela(ww)                             99
043200050610     c   99              movel     wcdi          cdiela(WW)
043300050610     c
043400050609     c                   clear                   $pog
043500050610     c                   clear                   xx
043600050609     c                   eval      wcaricaA='N'
043700050609     c                   eval      wcaricaD='N'
043800050609     c                   exsr      DISTRETTO
043900050609     c
044000050609     c* se tutti i p.o. del Distretto sono anche nella schiera OPOG
044100050609     c*  in output do anche ildistretto
044200050609     c                   eval      ww=1
044300050609     c                   clear                   noncarica
044400050609    3c                   dow       ww<=250  and $pog(WW)>*zeros
044500050609     c     $pog(WW)      lookup    Opog                                   99
044600050609     c                   if        not *in99
044700050609     c                   eval      Noncarica='N'
044800050609     c                   eval      ww=251
044900050609     c                   else
045000050609     c                   add       1             ww
045100050609     c                   endif
045200050609    3c                   enddo
045300050609     c
045400050609     c                   if        noncarica=' '
045500050609     c                   eval      wcaricaA='n'
045600050609     c                   eval      wcaricaD='S'
045700050609     c                   exsr      CarAREADISTR
045800050609     c                   endif
045900050610   2ac                   endif
046000050609     c
046100050609     c
046200050609     c                   add       1             zz
046300050609    2c                   enddo
046400050609     c
046500050609     c                   movea     $arg          Oarg
046600050609     c                   movea     $dig          Odig
046700050609    1c                   endif
046800050609     c                   ENDSR
046900020513      *
047000021115      * === UTENTE ABILITATO AL SUO SOLO P.O. + £6 ================= *
047100020513      *
047200020513     C     PO_PO         BEGSR
047300020513      *
047400021115     C                   reset                   TRUL06DS
047500021115     C                   movel     'L'           D06tla
047600021115     C                   movel     '£6'          D06cod
047700021115     C                   movel     I31cpo        D06key
047800021115     C                   movel(p)  TRUL06DS      KPJBU
047900021115     C                   call      'TRUL06R'
048000021115     C                   parm                    KPJBA
048100021115     C                   movel     KPJBU         TRUL06DS
048200021115if  1C                   if        D06err = *blanks
048300050609     C                   movea     D06lia        OPOG
048400021115e   1C                   else
048500020513     C                   add       1             XX
048600050609     C                   move      I31cpo        OPOG(XX)
048700021115     c                   endif
048800020513      *
048900050609     c* qui sicuramente non c'e' nulla di aree e distretto per
049000050609     c*  cui non faccio nemmeno la fatica
049100020513     C                   ENDSR
049200021107      *
049300151029      * === RAGGRUPPAMENTO AREA P.O DI UTENTE ====================== *
049400021107      *
049500021111     C     PO_RA         BEGSR
049600021107      *
049700040203if  1C                   if        I31cpo <> *zeros
049800040203      *
049900021107      * Accedo al P.O. utente per recuperare raggruppamento area
050000021107     C     I31cpo        chain     AZORG
050100040203if  2C                   if            %found(AZORG01L)
050200021107     C                             and  ORGfva = *blanks
050300021107     C                   eval      Og140 = OrgDe0
050400041214     C                   movel     §ogara        Wrar              3
050500040203e   2C                   endif
050600040203      *
050700040203x   1C                   else
050800040203      *
050900040203      * Imposto il codice area ricevuto come raggruppamento area
051000040203     C                   movel     I31car        Wrar
051100040203      *
051200040203e   1C                   endif
051300021107      *
051400021107      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
051500131010     C     *loval        setll     AZORG
051600021107     C                   read      AZORG
051700040203do  1C                   dow       not  %eof(AZORG01L)
051800021107     C                   eval      Og140 = OrgDe0
051900040203if  2C                   if             ORGfva = *blanks
052000021107     C                             and (ORGfag = 'A'
052100021107     C                              or  ORGfag = 'F')
052200021107     C                             and Wrar = §ogara
052300021107     C                   add       1             XX
052400021107     C                   move      ORGfil        $POG(XX)
052500040203e   2C                   endif
052600021107     C                   read      AZORG
052700040203e   1C                   enddo
052800021107      *
052900050609     c                   movea     $pog          Opog
053000151028     c*
053100050609     c* Carica se richeisto anche le aree e distretti a cui è
053200050609     c*  abilitato
053300050609     c                   exsr      CTR3parms
053400151028      *
053500021107     C                   ENDSR
053600151028      *
053700151029      * === RAGGRUPPAMENTO AREA CENTRALINO ========================= *
053800151028      *
053900151028     C     PO_RC         BEGSR
054000151028      *
054100151028if  1C                   if        I31cpo <> *zeros
054200151028      *
054300151028      * Accedo al P.O. utente per recuperare raggruppamento area
054400151028     C     I31cpo        chain     AZORG
054500151028if  2C                   if            %found(AZORG01L)
054600151028     C                             and  ORGfva = *blanks
054700151028     C                   eval      Og140 = OrgDe0
054800151028     C                   movel     §ogARC        Wrac              3
054900151028e   2C                   endif
055000151028      *
055100151028x   1C                   else
055200151028      *
055300151028      * Imposto il codice area ricevuto come raggruppamento area
055400151028     C                   movel     I31car        Wrac
055500151028      *
055600151028e   1C                   endif
055700151028      *
055800151028      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
055900151028     C     *loval        setll     AZORG
056000151028     C                   read      AZORG
056100151028do  1C                   dow       not  %eof(AZORG01L)
056200151028     C                   eval      Og140 = OrgDe0
056300151028if  2C                   if             ORGfva = *blanks
056400151028     C                             and (ORGfag = 'A'
056500151028     C                              or  ORGfag = 'F')
056600151028     C                             and Wrac = §ogARC
056700151028     C                   add       1             XX
056800151028     C                   move      ORGfil        $POG(XX)
056900151028e   2C                   endif
057000151028     C                   read      AZORG
057100151028e   1C                   enddo
057200151028      *
057300151028     c                   movea     $pog          Opog
057400151028     c*
057500151028     c* Carica se richeisto anche le aree e distretti a cui è
057600151028     c*  abilitato
057700151028     c                   exsr      CTR3parms
057800151028      *
057900151028     C                   ENDSR
058000021113      *
058100151029      * === RAGGRUPPAMENTO AREA POC P.O DI UTENTE ================== *
058200021113      *
058300021113     C     PO_AP         BEGSR
058400021113      *
058500040203if  1C                   if        I31cpo <> *zeros
058600040203      *
058700021113      * Accedo al P.O. utente per recuperare raggruppamento area
058800021113     C     I31cpo        chain     AZORG
058900040203if  2C                   if            %found(AZORG01L)
059000021113     C                             and  ORGfva = *blanks
059100021113     C                   eval      Og140 = OrgDe0
059200021113     C                   movel     §ogapo        Wapo              3
059300040203e   2C                   endif
059400040203      *
059500040203x   1C                   else
059600040203      *
059700040203      * Imposto il codice area ricevuto come raggruppamento area
059800040203     C                   movel     I31car        Wapo
059900040203      *
060000040203e   1C                   endif
060100021113      *
060200021113      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
060300021113     C     *loval        setll     AZORG
060400021113     C                   read      AZORG
060500021113do  1C                   dow       not  %eof(AZORG01L)
060600021113     C                   eval      Og140 = OrgDe0
060700040203if  2C                   if             ORGfva = *blanks
060800021113     C                             and (ORGfag = 'A'
060900021113     C                              or  ORGfag = 'F')
061000021113     C                             and Wapo = §ogapo
061100021113     C                   add       1             XX
061200021113     C                   move      ORGfil        $POG(XX)
061300040203e   2C                   endif
061400021113     C                   read      AZORG
061500021113e   1C                   enddo
061600050609     c
061700050609     c                   movea     $pog          Opog
061800050609     c* Carica se richeisto anche le aree e distretti a cui è
061900050609     c*  abilitato
062000050609     c                   exsr      CTR3parms
062100021113      *
062200021113     C                   ENDSR
062300160913      *__________________________________________________________________
062400160913      * === RAGGRUPPAMENTO AREA + filiali logistiche  ============== *
062500160913     C     PO_RL         BEGSR
062600160913      *___________________________________________________________________
062700160913if  1C                   if        I31cpo <> *zeros
062800160913      *
062900160913      * Accedo al P.O. utente per recuperare raggruppamento area
063000160913     C     I31cpo        chain     AZORG
063100160913if  2C                   if            %found(AZORG01L)
063200160913     C                             and  ORGfva = *blanks
063300160913     C                   eval      Og140 = OrgDe0
063400160913     C                   movel     §ogara        Wapo              3
063500160913e   2C                   endif
063600160913      *
063700160913x   1C                   else
063800160913      *
063900160913      * Imposto il codice area ricevuto come raggruppamento area
064000160913     C                   movel     I31car        Wapo
064100160913      *
064200160913e   1C                   endif
064300160913      *
064400160913      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
064500160913     C     *loval        setll     AZORG
064600160913     C                   read      AZORG
064700160913do  1C                   dow       not  %eof(AZORG01L)
064800160913     C                   eval      Og140 = OrgDe0
064900160913     C                   eval      Og143 = OrgDe3
065000160913if  2C                   if        ORGfva = *blanks
065100160913     C                             and (ORGfag = 'A'
065200160913     C                              or  ORGfag = 'F')
065300160913     C                             and Wapo = §ogara
065400160913     c                             or
065500160927if  2C                             ORGfva = *blanks and
065600160913if  2C                             Wapo = §ogara and
065700160913     c                             §ogntw = 'LOG'
065800160913     C                   add       1             XX
065900160913     C                   move      ORGfil        $POG(XX)
066000160913e   2C                   endif
066100160913     C                   read      AZORG
066200160913e   1C                   enddo
066300160913     c
066400160913     c                   movea     $pog          Opog
066500160913     c                   exsr      CTR3parms
066600160913      *
066700160913     C                   ENDSR
066800131010      *____________________________________________________________
066900160914      * filiali contabili
067000131010     C     PO_AC         BEGSR
067100131010      *____________________________________________________________
067200131010      *
067300131010     C                   z-add     0             XX
067400131010if  1C                   if        I31cpo <> *zeros
067500131010      *
067600131010      * Accedo al P.O. utente per recuperare raggruppamento area
067700131010     C     I31cpo        chain     AZORG
067800131010if  2C                   if        %found(AZORG01L)
067900131010     C                   movel     orgfc0        Wapo              3
068000131010e   2C                   endif
068100131010      *
068200131010      *
068300131010      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
068400131010     C     *loval        setll     AZORG
068500131010     c                   do        *hival
068600131010     C                   read      AZORG
068700131010do  1C                   if        %eof(AZORG01L)
068800131010     C                   leave
068900131010     c                   endif
069000131010     C                   movel     orgfc0        Wapo2             3
069100131010if  2C                   if        wapo2 <> wapo
069200131010     C                   iter
069300131010e   2C                   endif
069400160914if  3C                   if        ORGfva = *blanks
069500160914     C                             and (ORGfag = 'A'
069600160914     C                             or  ORGfag = 'F')
069700131010     C                   add       1             XX
069800131010     C                   move      ORGfil        $POG(XX)
069900160914e   1C                   endif
070000131010e   1C                   enddo
070100131010     c
070200131010     c                   movea     $pog          Opog
070300131010e   1C                   endif
070400131010      *
070500131010     C                   ENDSR
070600160914      *____________________________________________________________
070700160914      * filiali contabili + logistica
070800160914     C     PO_CL         BEGSR
070900160914      *____________________________________________________________
071000160914      *
071100160914     C                   z-add     0             XX
071200160914if  1C                   if        I31cpo <> *zeros
071300160914      *
071400160914      * Accedo al P.O. utente per recuperare raggruppamento area
071500160914     C     I31cpo        chain     AZORG
071600160914if  2C                   if        %found(AZORG01L)
071700160914     C                   movel     orgfc0        Wapo              3
071800160914e   2C                   endif
071900160914      *
072000160914      *
072100160914      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
072200160914     C     *loval        setll     AZORG
072300160914     c                   do        *hival
072400160914     C                   read      AZORG
072500160914do  1C                   if        %eof(AZORG01L)
072600160914     C                   leave
072700160914     c                   endif
072800160914     C                   movel     orgfc0        Wapo2             3
072900160914     C                   movel     orgde3        og143
073000160914if  2C                   if        wapo2 <> wapo
073100160914     C                   iter
073200160914e   2C                   endif
073300160914if  3C                   if        ORGfva = *blanks
073400160914     C                             and (ORGfag = 'A' or  ORGfag = 'F')
073500160914     C                             or
073600160914if  3C                             §ogntw = 'LOG'
073700160914     C                   add       1             XX
073800160914     C                   move      ORGfil        $POG(XX)
073900160914e   1C                   endif
074000160914e   1C                   enddo
074100160914     c
074200160914     c                   movea     $pog          Opog
074300160914e   1C                   endif
074400160914      *
074500160914     C                   ENDSR
074600131010      *____________________________________________________________
074700021113      *
074800151029      * === RAGGRUPPAMENTO AREA EX AS400 P.O DI UTENTE ============= *
074900021113      *
075000021113     C     PO_AR         BEGSR
075100021113      *
075200040203if  1C                   if        I31cpo <> *zeros
075300040203      *
075400021113      * Accedo al P.O. utente per recuperare raggruppamento area
075500021113     C     I31cpo        chain     AZORG
075600040203if  2C                   if            %found(AZORG01L)
075700021113     C                             and  ORGfva = *blanks
075800021113     C                   eval      Og140 = OrgDe0
075900021113     C                   movel     §ogaex        Waex              3
076000040203e   2C                   endif
076100040203      *
076200040203x   1C                   else
076300040203      *
076400040203      * Imposto il codice area ricevuto come raggruppamento area
076500040203     C                   movel     I31car        Waex
076600040203      *
076700040203e   1C                   endif
076800021113      *
076900021113      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
077000021113     C     *loval        setll     AZORG
077100021113     C                   read      AZORG
077200021113do  1C                   dow       not  %eof(AZORG01L)
077300021113     C                   eval      Og140 = OrgDe0
077400040203if  2C                   if             ORGfva = *blanks
077500021113     C                             and (ORGfag = 'A'
077600021113     C                              or  ORGfag = 'F')
077700021113     C                             and Waex = §ogaex
077800021113     C                   add       1             XX
077900021113     C                   move      ORGfil        $POG(XX)
078000040203e   2C                   endif
078100021113     C                   read      AZORG
078200021113e   1C                   enddo
078300050609     c
078400050609     c                   movea     $pog          Opog
078500050609     c* Carica se richeisto anche le aree e distretti a cui è
078600050609     c*  abilitato
078700050609     c                   exsr      CTR3parms
078800021113      *
078900021113     C                   ENDSR
079000020513      *--------------------------------------------------------------*
079100020513      * === REPERIMENTO DATI DEL P.O. ============================== *
079200020513      *--------------------------------------------------------------*
079300020513     C     REPDPO        BEGSR
079400020513      *
079500050609     C     wcpo          chain     AZORG
079600020513if  1C                   if            %found(AZORG01L)
079700020513     C                             and  ORGfva = *blanks
079800020513     C                             and (ORGfag = 'A'
079900020513     C                              or  ORGfag = 'F')
080000050609     C                   eval      wcdi = ORGfl3
080100050609     C                   eval      wcar = ORGcar
080200020513e   1C                   endif
080300020513      *
080400020513     C                   ENDSR
080500020513
080600020513      *--------------------------------------------------------------*
080700020513      * === MEMORIZZAZIONE P.O. IN SCHIERA ========================= *
080800020513      *--------------------------------------------------------------*
080900020513     C     MEMOPO        BEGSR
081000020513      *
081100020513if  1C                   if             ORGfva = *blanks
081200020513     C                             and (ORGfag = 'A'
081300020513     C                              or  ORGfag = 'F')
081400020513     C                   add       1             XX
081500020513     C                   move      ORGfil        $POG(XX)
081600050608     c
081700050609     c                   exsr      carAREADISTR
081800050609     c
081900050609e   1C                   endif
082000020513      *
082100020513     C                   ENDSR
082200160913      *--------------------------------------------------------------*
082300160913      * === MEMORIZZAZIONE P.O. + logistica                          *
082400160913      *--------------------------------------------------------------*
082500160913     C     MEMOPOlog     BEGSR
082600160913      *
082700160913if  1C                   if        ORGfva = *blanks
082800160913     C                             and (ORGfag = 'A'
082900160913     C                              or  ORGfag = 'F')
083000160913     C                              or  §ogntw = 'LOG'
083100160913     C                   add       1             XX
083200160913     C                   move      ORGfil        $POG(XX)
083300160913e   1C                   endif
083400160913      *
083500160913     C                   ENDSR
083600160913      *__________________________________________________________________
083700050614      * === UTENTE ABILITATO AI P.O. ESTERI ======================== *
083800050614      *
083900050614     C     PO_ESTERI     BEGSR
084000050614      *
084100050614     c                   eval      wcaricaA='N'
084200050614     c                   eval      wcaricaD='N'
084300050614     c
084400050614     C     *loval        setll     AZORG
084500050614     C                   read      AZORG
084600050614do  1C                   dow       not  %eof(AZORG01L)
084700050614     c* vedo dal ntw se estero
084800050614     c                   movel     orgde3        og143
084900050614     c                   if        §ogntw='DPD' or §ogntw='FED' or
085000050614     c                             §ogntw='EEX' or §ogntw='EUP'
085100050614     C                   exsr      MEMOPO
085200050614     c                   endif
085300050614     C                   read      AZORG
085400050614e   1C                   enddo
085500050614     c
085600050614     c                   movea     $pog          Opog
085700050614     c
085800050614     c* Carica se richeisto anche le aree e distretti a cui è
085900050614     c*  abilitato
086000050614     c                   exsr      CTR3parms
086100050614      *
086200050614     C                   ENDSR
086300050609      *--------------------------------------------------------------*
086400050609      * === Carico area e distretto ================================ *
086500050609      *--------------------------------------------------------------*
086600050609     c     carAREADISTR  begsr
086700151029    1c                   if        wcaricaA<>'N'
086800050609     c                   movel     orgcar        w003a             3
086900050609     c* Carico anche area : tutti
087000050609     c     w003a         lookup    $arg                                   90
087100050609    2c                   if        not *in90
087200050609     c                   z-add     1             yy
087300050609     c     '000'         lookup    $arg(YY)                               90
087400050609    3c                   if        *in90
087500050609     c                   move      orgcar        $arg(YY)
087600050609e   3C                   endif
087700050609e   2C                   endif
087800151029e   1C                   endif
087900050609
088000050609     c* Carico anche distretto
088100151029if  2c                   if        wcaricaD<>'N'
088200050609     c     orgfl3        lookup    $dig                                   90
088300151029if  3c                   if        not *in90
088400050609     c                   z-add     1             yy
088500050609     c     ' '           lookup    $dig(YY)                               90
088600050609    3c                   if        *in90
088700050609     c                   move      orgfl3        $dig(YY)
088800050609e   3C                   endif
088900050609e   2C                   endif
089000050609e   1C                   endif
089100050609     c
089200050609     c                   ENDSR
089300020513
089400020513      *--------------------------------------------------------------*
089500020513      * === FINE PROGRAMMA ========================================= *
089600020513      *--------------------------------------------------------------*
089700020513     C     ENDPGM        BEGSR
089800020513      *
089900020513      * Restituzione dei dati reperiti
090000050609     C                   movea     OPOG          O31POG
090100050609     c                   if        %parms>2
090200050609     C                   movea     OarG          O31arG
090300050609     C                   movea     OdiG          O31diG
090400050613     c                   if        i31abi='AZ'
090500050613     c                   eval      o31gtd='T'
090600050609     c                   endif
090700050613     c                   endif
090800020513     C                   clear                   KPJBU
090900020513      *
091000020513     C                   eval      *inLR  = *on
091100020513     C                   return
091200020513      *
091300020513     C                   ENDSR
091400020513
091500020513      *--------------------------------------------------------------*
091600020513      * === DEFINIZIONE KEY-LIST =================================== *
091700020513      *--------------------------------------------------------------*
091800020513     C     DEFKLIS       BEGSR
091900020513      *
092000020513      * AZORG02L
092100050609     C     Korg2         klist
092200050609     C                   kfld                    wcdi
092300050609     C                   kfld                    wcar
092400020513      *
092500020513     C                   ENDSR
092600020513** MSG                                                                       *
092700020513TRUL31R - Codice abilitazione errato                                           01
092800020513TRUL31R - Punto Operativo errato                                               02
