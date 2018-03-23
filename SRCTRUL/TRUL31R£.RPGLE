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
014000020513     C                             and I31abi <> 'DI'
014100021113     C                             and I31abi <> 'AO'
014200151029     C                             and I31abi <> 'AR'
014300151029     C                             and I31abi <> 'AP'
014400131010     C                             and I31abi <> 'AC'
014500151029     C                             and I31abi <> 'RA'
014600151029     C                             and I31abi <> 'RC'
014700020513     C                             and I31abi <> 'TP'
014800020513     C                             and I31abi <> 'PO'
014900050614     C                             and I31abi <> 'EE'
015000020513     C                   exsr      ENDPGM
015100020513e   1C                   endif
015200020513      *
015300020513     C                   ENDSR
015400020513
015500020513      *--------------------------------------------------------------*
015600020513      * === CARICAMENTO P.O. GESTIBILI DAL P.O. UTENTE ============= *
015700020513      *--------------------------------------------------------------*
015800020513     C     CARPOG        BEGSR
015900020513      *
016000020513sel 1C                   select
016100020513      * Utente senza alcuna abilitazione
016200020513w   1C                   when      I31abi = *blanks   or
016300020513     C                             I31abi = 'NO'
016400020513      * Utente con abilitazione a tutti i P.O.
016500020513w   1C                   when      I31abi = 'AZ'
016600020513     C                   exsr      PO_AZIENDA
016700020513      * Utente con abilitazione ai P.O. del suo distretto
016800020513w   1C                   when      I31abi = 'DI'
016900020513     C                   exsr      PO_DISTRETTO
017000020513      * Utente con abilitazione ai P.O. della sua area
017100021113w   1C                   when      I31abi = 'AO'
017200020513     C                   exsr      PO_AREA
017300151029      * Utente con abilitazione ai P.O. del suo raggruppamento area EX AS400
017400151029w   1C                   when      I31abi = 'AR'
017500151029     C                   exsr      PO_AR
017600151029      * Utente con abilitazione ai P.O. del suo raggruppamento area POC
017700151029w   1C                   when      I31abi = 'AP'
017800151029     C                   exsr      PO_AP
017900151029      * Utente con abilitazione ai P.O. del suo raggruppamento area contabile
018000151029w   1C                   when      I31abi = 'AC'
018100151029     C                   exsr      PO_AC
018200151029      * Utente con abilitazione ai P.O. del suo raggruppamento area
018300151029w   1C                   when      I31abi = 'RA'
018400151029     C                   exsr      PO_RA
018500151029      * Utente con abilitazione ai P.O. del suo raggruppamento area centralino
018600151029w   1C                   when      I31abi = 'RC'
018700151029     C                   exsr      PO_RC
018800020513      * Utente con abilitazione ai P.O. del suo terminal di partenza
018900020513w   1C                   when      I31abi = 'TP'
019000020513     C                   exsr      PO_TERMINALP
019100020513      * Utente con abilitazione al suo solo P.O.
019200020513w   1C                   when      I31abi = 'PO'
019300020513     C                   exsr      PO_PO
019400050614      * Utente con abilitazione ai P.O. esteri
019500050614w   1C                   when      I31abi = 'EE'
019600050614     C                   exsr      PO_ESTERI
019700020513      *
019800020513e   1C                   endsl
019900020513      *
020000020513     C                   ENDSR
020100020513
020200020513      *--------------------------------------------------------------*
020300020513      * === GESTIONE ABILITAZIONI PER P.O. ========================= *
020400020513      *--------------------------------------------------------------*
020500020513      *
020600020513      * === UTENTE ABILITATO A TUTTI I P.O. ======================== *
020700020513      *
020800020513     C     PO_AZIENDA    BEGSR
020900020513      *
021000050609     c                   eval      wcaricaA='S'
021100050609     c                   eval      wcaricaD='S'
021200020513     C     *loval        setll     AZORG
021300020513     C                   read      AZORG
021400020513do  1C                   dow       not  %eof(AZORG01L)
021500020513     C                   exsr      MEMOPO
021600020513     C                   read      AZORG
021700020513e   1C                   enddo
021800020513      *
021900050609     c                   movea     $pog          Opog
022000050609     c                   movea     $arg          Oarg
022100050609     c                   movea     $dig          Odig
022200020513     C                   ENDSR
022300020513      *
022400020513      * === UTENTE ABILITATO AI P.O. DEL DISTRETTO ================= *
022500020513      *
022600020513     C     PO_DISTRETTO  BEGSR
022700020513      *
022800020513      * Accedo al P.O. utente per recuperare il distretto
022900020513if  1C                   if        I31cdi = *blanks
023000050609     c                   eval      wcpo=i31cpo
023100020513     C                   exsr      REPDPO
023200050609     c                   eval      i31car=wcar
023300050609     c                   eval      i31cdi=wcdi
023400020513e   1C                   endif
023500020513      *
023600020513      * Ciclo di lettura P.O. del distretto
023700050609     c                   eval      wcaricaA='S'
023800050609     c                   eval      wcaricaD='S'
023900050609     c                   eval      wcdi=i31cdi
024000050609     c
024100050609     c                   exsr      DISTRETTO
024200050609     c
024300020513      *
024400050609     c                   movea     $pog          Opog
024500050609     c                   movea     $arg          Oarg
024600050609     c                   movea     $dig          Odig
024700050609     c
024800020513     C                   ENDSR
024900050609     c
025000151029      *
025100050609      * === Carica da azorg02 tutti i p.o. del distretto =========== *
025200050609      *
025300050609     c     DISTRETTO     begsr
025400050609     C     wcdi          chain     AZORG02
025500050609if  1C                   if        %found(AZORG02L)
025600050609do  2C                   dou       %eof(AZORG02L)
025700050609     C                   exsr      MEMOPO
025800050609     C     wcdi          reade     AZORG02
025900050609e   2C                   enddo
026000050609e   1C                   endif
026100050609     c                   endsr
026200020513      *
026300020513      * === UTENTE ABILITATO AI P.O. DELL'AREA ===================== *
026400020513      *
026500020513     C     PO_AREA       BEGSR
026600021113      *
026700021113      * Accedo al P.O. utente per recuperare il distretto/area
026800040203if  1C                   if        I31cpo <> *zeros
026900050609     c                   eval      wcpo=i31cpo
027000021113     C                   exsr      REPDPO
027100050609     c                   eval      i31car=wcar
027200050609     c                   eval      i31cdi=wcdi
027300040203e   1C                   endif
027400020513      *
027500040203if  1C                   if            I31cdi <> *blanks
027600040203     C                             and I31car <> *zeros
027700040203      *
027800050609     c                   eval      wcar=i31car
027900050609     c                   eval      wcdi=i31cdi
028000050609     c                   eval      wcaricaD='N'
028100050609     c                   eval      wcaricaA='S'
028200050609     c
028300050609     c                   EXSR      AREA
028400040203      *
028500040203x   1C                   else
028600040203      *
028700040203      * Ciclo di lettura P.O. e selezione per area
028800040203     C     *loval        setll     AZORG
028900040203     C                   read      AZORG
029000040203do  2C                   dow       not  %eof(AZORG01L)
029100040203if  3C                   if             ORGfva = *blanks
029200040203     C                             and (ORGfag = 'A'
029300040203     C                              or  ORGfag = 'F')
029400040203     C                             and  ORGcar = I31car
029500040203     C                   add       1             XX
029600040203     C                   move      ORGfil        $POG(XX)
029700040203e   3C                   endif
029800040203     C                   read      AZORG
029900040203e   2C                   enddo
030000050609     c
030100050609     c* Carico anche l'area
030200050609     c                   movel     i31car        $arg(1)
030300040203e   1C                   endif
030400020513      *
030500050609     c                   movea     $pog          Opog
030600050609     c                   movea     $arg          Oarg
030700020513     C                   ENDSR
030800020513      *
030900050609      * === Carica p.o. area da azorg02l =========================== *
031000050609      *
031100050609     c     AREA          begsr
031200050609      * Ciclo di lettura P.O. del distretto e dell'area
031300050609     C     Korg2         chain     AZORG02
031400050609if  2C                   if        %found(AZORG02L)
031500050609do  3C                   dou       %eof(AZORG02L)
031600050609     C                   exsr      MEMOPO
031700050609     C     Korg2         reade     AZORG02
031800050609e   3C                   enddo
031900050609e   2C                   endif
032000050609     c                   ENDSR
032100050609     c
032200151029      *
032300020513      * === UTENTE ABILITATO AI P.O. DEL TERMINAL DI PARTENZA ====== *
032400020513      *
032500020513     C     PO_TERMINALP  BEGSR
032600020513      *
032700020513     C                   reset                   TRUL06DS
032800021115     C                   movel     'L'           D06tla
032900021115     C                   movel     'PP'          D06cod
033000021115     C                   movel     I31cpo        D06key
033100020513     C                   movel(p)  TRUL06DS      KPJBU
033200020513     C                   call      'TRUL06R'
033300020513     C                   parm                    KPJBA
033400020513     C                   movel     KPJBU         TRUL06DS
033500020513if  1C                   if        D06err = *blanks
033600050609     C                   movea     D06lia        OPOG
033700020513e   1C                   endif
033800050609     c
033900050609     c* Carica se richeisto anche le aree e distretti a cui è
034000050609     c*  abilitato
034100050609     c                   exsr      CTR3parms
034200020513      *
034300020513     C                   ENDSR
034400050609     c*
034500050609     c* Vedo se caricare 3 parametro di output:aree e distretti ======
034600050609     c*
034700050609     c     CTR3PARMS     begsr
034800050609     c* Se richiesta in output anche schiera delle aree e distretti
034900050609    1c                   if        %parms>2
035000050610     c                   reset                   $arg
035100050610     c                   reset                   $dig
035200050609     c
035300050609     c* Leggo la schiera per vedere dei p.o. le aree e distretto:
035400050609     c*  se ne ho caricati tutti i p.o. --> passo indietro anche le aree/dis
035500050609     c                   eval      zz=1
035600050609    2c                   dow       Opog(zz)>*zeros
035700050609     c                   movel     Opog(zz)      wcpo
035800050609     c                   exsr      REPDpo
035900050609     c
036000050609     c* Carico tutti i p.o. dell'area (area e distetto non mi interess)
036100050610     c*   se non l'ho già elaborata
036200050610     c     wcar          lookup    carela                                 99
036300050610   2ac                   if        not *in99
036400050610     c                   z-add     1             ww
036500050610     c     000           lookup    carela(ww)                             99
036600050610     c   99              movel     wcar          carela(WW)
036700050610     c
036800050609     c                   clear                   $pog
036900050610     c                   clear                   xx
037000050609     c                   eval      wcaricaA='N'
037100050609     c                   eval      wcaricaD='N'
037200050609     c                   exsr      AREA
037300050609     c
037400050609     c* se tutti i p.o. dell'area sono anche nella schiera OPOG
037500050609     c*  in output do anche l'area
037600050609     c                   eval      ww=1
037700050609     c                   clear                   Noncarica
037800050609    3c                   dow       ww<=250  and $pog(WW)>*zeros
037900050609     c     $pog(WW)      lookup    Opog                                   99
038000050609     c                   if        not *in99
038100050609     c                   eval      Noncarica='N'
038200050609     c                   eval      ww=251
038300050609     c                   else
038400050609     c                   add       1             ww
038500050609     c                   endif
038600050609    3c                   enddo
038700050609     c
038800050609     c                   if        Noncarica=' '
038900050609     c                   eval      wcaricaA='S'
039000050609     c                   eval      wcaricaD='N'
039100050609     c                   exsr      CarAREADISTR
039200050609     c                   endif
039300050610   2ac                   endif
039400050609     c
039500050609     c* Carico tutti i p.o. del distre(area e distetto non mi interess)
039600050610     c*   se non l'ho già elaborato
039700050610     c     wcdi          lookup    cdiela                                 99
039800050610   2ac                   if        not *in99
039900050610     c                   z-add     1             ww
040000050610     c     ' '           lookup    cdiela(ww)                             99
040100050610     c   99              movel     wcdi          cdiela(WW)
040200050610     c
040300050609     c                   clear                   $pog
040400050610     c                   clear                   xx
040500050609     c                   eval      wcaricaA='N'
040600050609     c                   eval      wcaricaD='N'
040700050609     c                   exsr      DISTRETTO
040800050609     c
040900050609     c* se tutti i p.o. del Distretto sono anche nella schiera OPOG
041000050609     c*  in output do anche ildistretto
041100050609     c                   eval      ww=1
041200050609     c                   clear                   noncarica
041300050609    3c                   dow       ww<=250  and $pog(WW)>*zeros
041400050609     c     $pog(WW)      lookup    Opog                                   99
041500050609     c                   if        not *in99
041600050609     c                   eval      Noncarica='N'
041700050609     c                   eval      ww=251
041800050609     c                   else
041900050609     c                   add       1             ww
042000050609     c                   endif
042100050609    3c                   enddo
042200050609     c
042300050609     c                   if        noncarica=' '
042400050609     c                   eval      wcaricaA='n'
042500050609     c                   eval      wcaricaD='S'
042600050609     c                   exsr      CarAREADISTR
042700050609     c                   endif
042800050610   2ac                   endif
042900050609     c
043000050609     c
043100050609     c                   add       1             zz
043200050609    2c                   enddo
043300050609     c
043400050609     c                   movea     $arg          Oarg
043500050609     c                   movea     $dig          Odig
043600050609    1c                   endif
043700050609     c                   ENDSR
043800020513      *
043900021115      * === UTENTE ABILITATO AL SUO SOLO P.O. + £6 ================= *
044000020513      *
044100020513     C     PO_PO         BEGSR
044200020513      *
044300021115     C                   reset                   TRUL06DS
044400021115     C                   movel     'L'           D06tla
044500021115     C                   movel     '£6'          D06cod
044600021115     C                   movel     I31cpo        D06key
044700021115     C                   movel(p)  TRUL06DS      KPJBU
044800021115     C                   call      'TRUL06R'
044900021115     C                   parm                    KPJBA
045000021115     C                   movel     KPJBU         TRUL06DS
045100021115if  1C                   if        D06err = *blanks
045200050609     C                   movea     D06lia        OPOG
045300021115e   1C                   else
045400020513     C                   add       1             XX
045500050609     C                   move      I31cpo        OPOG(XX)
045600021115     c                   endif
045700020513      *
045800050609     c* qui sicuramente non c'e' nulla di aree e distretto per
045900050609     c*  cui non faccio nemmeno la fatica
046000020513     C                   ENDSR
046100021107      *
046200151029      * === RAGGRUPPAMENTO AREA P.O DI UTENTE ====================== *
046300021107      *
046400021111     C     PO_RA         BEGSR
046500021107      *
046600040203if  1C                   if        I31cpo <> *zeros
046700040203      *
046800021107      * Accedo al P.O. utente per recuperare raggruppamento area
046900021107     C     I31cpo        chain     AZORG
047000040203if  2C                   if            %found(AZORG01L)
047100021107     C                             and  ORGfva = *blanks
047200021107     C                   eval      Og140 = OrgDe0
047300041214     C                   movel     §ogara        Wrar              3
047400040203e   2C                   endif
047500040203      *
047600040203x   1C                   else
047700040203      *
047800040203      * Imposto il codice area ricevuto come raggruppamento area
047900040203     C                   movel     I31car        Wrar
048000040203      *
048100040203e   1C                   endif
048200021107      *
048300021107      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
048400131010     C     *loval        setll     AZORG
048500021107     C                   read      AZORG
048600040203do  1C                   dow       not  %eof(AZORG01L)
048700021107     C                   eval      Og140 = OrgDe0
048800040203if  2C                   if             ORGfva = *blanks
048900021107     C                             and (ORGfag = 'A'
049000021107     C                              or  ORGfag = 'F')
049100021107     C                             and Wrar = §ogara
049200021107     C                   add       1             XX
049300021107     C                   move      ORGfil        $POG(XX)
049400040203e   2C                   endif
049500021107     C                   read      AZORG
049600040203e   1C                   enddo
049700021107      *
049800050609     c                   movea     $pog          Opog
049900151028     c*
050000050609     c* Carica se richeisto anche le aree e distretti a cui è
050100050609     c*  abilitato
050200050609     c                   exsr      CTR3parms
050300151028      *
050400021107     C                   ENDSR
050500151028      *
050600151029      * === RAGGRUPPAMENTO AREA CENTRALINO ========================= *
050700151028      *
050800151028     C     PO_RC         BEGSR
050900151028      *
051000151028if  1C                   if        I31cpo <> *zeros
051100151028      *
051200151028      * Accedo al P.O. utente per recuperare raggruppamento area
051300151028     C     I31cpo        chain     AZORG
051400151028if  2C                   if            %found(AZORG01L)
051500151028     C                             and  ORGfva = *blanks
051600151028     C                   eval      Og140 = OrgDe0
051700151028     C                   movel     §ogARC        Wrac              3
051800151028e   2C                   endif
051900151028      *
052000151028x   1C                   else
052100151028      *
052200151028      * Imposto il codice area ricevuto come raggruppamento area
052300151028     C                   movel     I31car        Wrac
052400151028      *
052500151028e   1C                   endif
052600151028      *
052700151028      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
052800151028     C     *loval        setll     AZORG
052900151028     C                   read      AZORG
053000151028do  1C                   dow       not  %eof(AZORG01L)
053100151028     C                   eval      Og140 = OrgDe0
053200151028if  2C                   if             ORGfva = *blanks
053300151028     C                             and (ORGfag = 'A'
053400151028     C                              or  ORGfag = 'F')
053500151028     C                             and Wrac = §ogARC
053600151028     C                   add       1             XX
053700151028     C                   move      ORGfil        $POG(XX)
053800151028e   2C                   endif
053900151028     C                   read      AZORG
054000151028e   1C                   enddo
054100151028      *
054200151028     c                   movea     $pog          Opog
054300151028     c*
054400151028     c* Carica se richeisto anche le aree e distretti a cui è
054500151028     c*  abilitato
054600151028     c                   exsr      CTR3parms
054700151028      *
054800151028     C                   ENDSR
054900021113      *
055000151029      * === RAGGRUPPAMENTO AREA POC P.O DI UTENTE ================== *
055100021113      *
055200021113     C     PO_AP         BEGSR
055300021113      *
055400040203if  1C                   if        I31cpo <> *zeros
055500040203      *
055600021113      * Accedo al P.O. utente per recuperare raggruppamento area
055700021113     C     I31cpo        chain     AZORG
055800040203if  2C                   if            %found(AZORG01L)
055900021113     C                             and  ORGfva = *blanks
056000021113     C                   eval      Og140 = OrgDe0
056100021113     C                   movel     §ogapo        Wapo              3
056200040203e   2C                   endif
056300040203      *
056400040203x   1C                   else
056500040203      *
056600040203      * Imposto il codice area ricevuto come raggruppamento area
056700040203     C                   movel     I31car        Wapo
056800040203      *
056900040203e   1C                   endif
057000021113      *
057100021113      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
057200021113     C     *loval        setll     AZORG
057300021113     C                   read      AZORG
057400021113do  1C                   dow       not  %eof(AZORG01L)
057500021113     C                   eval      Og140 = OrgDe0
057600040203if  2C                   if             ORGfva = *blanks
057700021113     C                             and (ORGfag = 'A'
057800021113     C                              or  ORGfag = 'F')
057900021113     C                             and Wapo = §ogapo
058000021113     C                   add       1             XX
058100021113     C                   move      ORGfil        $POG(XX)
058200040203e   2C                   endif
058300021113     C                   read      AZORG
058400021113e   1C                   enddo
058500050609     c
058600050609     c                   movea     $pog          Opog
058700050609     c* Carica se richeisto anche le aree e distretti a cui è
058800050609     c*  abilitato
058900050609     c                   exsr      CTR3parms
059000021113      *
059100021113     C                   ENDSR
059200131010      *____________________________________________________________
059300131010     C     PO_AC         BEGSR
059400131010      *____________________________________________________________
059500131010      *
059600131010     C                   z-add     0             XX
059700131010if  1C                   if        I31cpo <> *zeros
059800131010      *
059900131010      * Accedo al P.O. utente per recuperare raggruppamento area
060000131010     C     I31cpo        chain     AZORG
060100131010if  2C                   if        %found(AZORG01L)
060200131010     C                   movel     orgfc0        Wapo              3
060300131010e   2C                   endif
060400131010      *
060500131010      *
060600131010      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
060700131010     C     *loval        setll     AZORG
060800131010     c                   do        *hival
060900131010     C                   read      AZORG
061000131010do  1C                   if        %eof(AZORG01L)
061100131010     C                   leave
061200131010     c                   endif
061300131010     C                   movel     orgfc0        Wapo2             3
061400131010if  2C                   if        wapo2 <> wapo
061500131010     C                   iter
061600131010e   2C                   endif
061700131010     C                   add       1             XX
061800131010     C                   move      ORGfil        $POG(XX)
061900131010e   1C                   enddo
062000131010     c
062100131010     c                   movea     $pog          Opog
062200131010e   1C                   endif
062300131010      *
062400131010     C                   ENDSR
062500131010      *____________________________________________________________
062600021113      *
062700151029      * === RAGGRUPPAMENTO AREA EX AS400 P.O DI UTENTE ============= *
062800021113      *
062900021113     C     PO_AR         BEGSR
063000021113      *
063100040203if  1C                   if        I31cpo <> *zeros
063200040203      *
063300021113      * Accedo al P.O. utente per recuperare raggruppamento area
063400021113     C     I31cpo        chain     AZORG
063500040203if  2C                   if            %found(AZORG01L)
063600021113     C                             and  ORGfva = *blanks
063700021113     C                   eval      Og140 = OrgDe0
063800021113     C                   movel     §ogaex        Waex              3
063900040203e   2C                   endif
064000040203      *
064100040203x   1C                   else
064200040203      *
064300040203      * Imposto il codice area ricevuto come raggruppamento area
064400040203     C                   movel     I31car        Waex
064500040203      *
064600040203e   1C                   endif
064700021113      *
064800021113      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
064900021113     C     *loval        setll     AZORG
065000021113     C                   read      AZORG
065100021113do  1C                   dow       not  %eof(AZORG01L)
065200021113     C                   eval      Og140 = OrgDe0
065300040203if  2C                   if             ORGfva = *blanks
065400021113     C                             and (ORGfag = 'A'
065500021113     C                              or  ORGfag = 'F')
065600021113     C                             and Waex = §ogaex
065700021113     C                   add       1             XX
065800021113     C                   move      ORGfil        $POG(XX)
065900040203e   2C                   endif
066000021113     C                   read      AZORG
066100021113e   1C                   enddo
066200050609     c
066300050609     c                   movea     $pog          Opog
066400050609     c* Carica se richeisto anche le aree e distretti a cui è
066500050609     c*  abilitato
066600050609     c                   exsr      CTR3parms
066700021113      *
066800021113     C                   ENDSR
066900020513      *--------------------------------------------------------------*
067000020513      * === REPERIMENTO DATI DEL P.O. ============================== *
067100020513      *--------------------------------------------------------------*
067200020513     C     REPDPO        BEGSR
067300020513      *
067400050609     C     wcpo          chain     AZORG
067500020513if  1C                   if            %found(AZORG01L)
067600020513     C                             and  ORGfva = *blanks
067700020513     C                             and (ORGfag = 'A'
067800020513     C                              or  ORGfag = 'F')
067900050609     C                   eval      wcdi = ORGfl3
068000050609     C                   eval      wcar = ORGcar
068100020513e   1C                   endif
068200020513      *
068300020513     C                   ENDSR
068400020513
068500020513      *--------------------------------------------------------------*
068600020513      * === MEMORIZZAZIONE P.O. IN SCHIERA ========================= *
068700020513      *--------------------------------------------------------------*
068800020513     C     MEMOPO        BEGSR
068900020513      *
069000020513if  1C                   if             ORGfva = *blanks
069100020513     C                             and (ORGfag = 'A'
069200020513     C                              or  ORGfag = 'F')
069300020513     C                   add       1             XX
069400020513     C                   move      ORGfil        $POG(XX)
069500050608     c
069600050609     c                   exsr      carAREADISTR
069700050609     c
069800050609e   1C                   endif
069900020513      *
070000020513     C                   ENDSR
070100050614      *
070200050614      * === UTENTE ABILITATO AI P.O. ESTERI ======================== *
070300050614      *
070400050614     C     PO_ESTERI     BEGSR
070500050614      *
070600050614     c                   eval      wcaricaA='N'
070700050614     c                   eval      wcaricaD='N'
070800050614     c
070900050614     C     *loval        setll     AZORG
071000050614     C                   read      AZORG
071100050614do  1C                   dow       not  %eof(AZORG01L)
071200050614     c* vedo dal ntw se estero
071300050614     c                   movel     orgde3        og143
071400050614     c                   if        §ogntw='DPD' or §ogntw='FED' or
071500050614     c                             §ogntw='EEX' or §ogntw='EUP'
071600050614     C                   exsr      MEMOPO
071700050614     c                   endif
071800050614     C                   read      AZORG
071900050614e   1C                   enddo
072000050614     c
072100050614     c                   movea     $pog          Opog
072200050614     c
072300050614     c* Carica se richeisto anche le aree e distretti a cui è
072400050614     c*  abilitato
072500050614     c                   exsr      CTR3parms
072600050614      *
072700050614     C                   ENDSR
072800050609      *--------------------------------------------------------------*
072900050609      * === Carico area e distretto ================================ *
073000050609      *--------------------------------------------------------------*
073100050609     c     carAREADISTR  begsr
073200151029    1c                   if        wcaricaA<>'N'
073300050609     c                   movel     orgcar        w003a             3
073400050609     c* Carico anche area : tutti
073500050609     c     w003a         lookup    $arg                                   90
073600050609    2c                   if        not *in90
073700050609     c                   z-add     1             yy
073800050609     c     '000'         lookup    $arg(YY)                               90
073900050609    3c                   if        *in90
074000050609     c                   move      orgcar        $arg(YY)
074100050609e   3C                   endif
074200050609e   2C                   endif
074300151029e   1C                   endif
074400050609
074500050609     c* Carico anche distretto
074600151029if  2c                   if        wcaricaD<>'N'
074700050609     c     orgfl3        lookup    $dig                                   90
074800151029if  3c                   if        not *in90
074900050609     c                   z-add     1             yy
075000050609     c     ' '           lookup    $dig(YY)                               90
075100050609    3c                   if        *in90
075200050609     c                   move      orgfl3        $dig(YY)
075300050609e   3C                   endif
075400050609e   2C                   endif
075500050609e   1C                   endif
075600050609     c
075700050609     c                   ENDSR
075800020513
075900020513      *--------------------------------------------------------------*
076000020513      * === FINE PROGRAMMA ========================================= *
076100020513      *--------------------------------------------------------------*
076200020513     C     ENDPGM        BEGSR
076300020513      *
076400020513      * Restituzione dei dati reperiti
076500050609     C                   movea     OPOG          O31POG
076600050609     c                   if        %parms>2
076700050609     C                   movea     OarG          O31arG
076800050609     C                   movea     OdiG          O31diG
076900050613     c                   if        i31abi='AZ'
077000050613     c                   eval      o31gtd='T'
077100050609     c                   endif
077200050613     c                   endif
077300020513     C                   clear                   KPJBU
077400020513      *
077500020513     C                   eval      *inLR  = *on
077600020513     C                   return
077700020513      *
077800020513     C                   ENDSR
077900020513
078000020513      *--------------------------------------------------------------*
078100020513      * === DEFINIZIONE KEY-LIST =================================== *
078200020513      *--------------------------------------------------------------*
078300020513     C     DEFKLIS       BEGSR
078400020513      *
078500020513      * AZORG02L
078600050609     C     Korg2         klist
078700050609     C                   kfld                    wcdi
078800050609     C                   kfld                    wcar
078900020513      *
079000020513     C                   ENDSR
079100020513** MSG                                                                       *
079200020513TRUL31R - Codice abilitazione errato                                           01
079300020513TRUL31R - Punto Operativo errato                                               02
