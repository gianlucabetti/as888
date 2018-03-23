000100020513      *-----------*
000200020513      *  TRUL31R  *  CARICAMENTO P.O. GESTIBILI
000300020513      *-----------*
000400020513      *
000500020513      * INPUT
000600020513      * -----
000700020513      *  I31ABI   Codice Abilitazione
000800020513      *  I31CDI   Codice Distretto Utente
000900040203      *  I31CAR   Codice Area      Utente
001000020513      *  I31CPO   Punto Operativo  Utente
001100020513      *
001200020513      * OUTPUT
001300020513      * ------
001400020513      *  O31POG   Schiera P.O. gestibili (250 elem.)
001500050608      *
001600050608      * 2° OUTPUT
001700050608      * ------
001800050608      *  O31DIG   Schiera Distretti gestibili (20 elem.)
001900050613      *  O31gtd   T=gestibili tutti i distretti del p.o. (esclusa SEDE )
002000050608      *  O31ARG   Schiera Aree      gestibili (50 elem.)
002100020513      *
002200020513      *--------------------------------------------------------------*
002300020513
002400041214     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
002500020513
002600020513     FAZORG01L  IF   E           K DISK
002700020513     FAZORG02L  IF   E           K DISK    rename(AZORG:AZORG02)
002800020513
002900020513      *
003000020513      * === COSTANTI =============================================== *
003100020513      *
003200020513     D MaxPOG          c                   const(250)
003300020513      *
003400020513      * === SCHIERE A TEMPO DI COMPILAZIONE ======================== *
003500020513      *
003600020513      * Messaggi di errore
003700020513     D MSG             s             78    dim(2) ctdata perrcd(1)
003800020513      *
003900020513      * === SCHIERE A TEMPO DI ESECUZIONE ========================== *
004000020513      *
004100050609      * P.O. gestibili di work e di output
004200020513     D $POG            s              3    dim(MaxPOG) inz(*zeros)
004300050609     D OPOG            s              3    dim(MaxPOG) inz(*zeros)
004400050608      * Distretti gestibili
004500050609     D $DIG            s              1    dim(20)
004600050609     D ODIG            s              1    dim(20)
004700050610     D cdiela          s              1    dim(20)
004800050608      * Aree gestibili
004900050608     D $ARG            s              3    dim(50) inz(*zeros)
005000050609     D OARG            s              3    dim(50) inz(*zeros)
005100050610     D carela          s              3  0 dim(50) inz(*zeros)
005200020513      *
005300020513      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
005400020513      *
005500020513      * Passaggio Parametri
005600020513     D KPJBA         e ds
005700020513     D TRUL31DS      e ds
005800050609     D TRUL31DS2     e ds
005900021107     D OG140         e ds
006000050614     D OG143         e ds
006100020513      *
006200020513      * Ridefinizione campi del file AZORG
006300020513      * (per semplificarne la pulizia con una banale CLEAR)
006400020513     D AZORGDS       e ds                  inz extname(AZORG00F)
006500020513      *
006600020513     D TRUL06DS      e ds                  inz
006700020513      *
006800020513      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
006900020513      *
007000020513      * === VARIABILI ============================================== *
007100020513      *
007200020513      * Indice schiere
007300020513     D XX              S              3  0 inz
007400050609     D yy              S              3  0 inz
007500050609     D zz              S              3  0 inz
007600050609     D ww              S              3  0 inz
007700050609     D wcaricaA        S              1    inz
007800050609     D wcaricaD        S              1    inz
007900050609     D noncarica       S              1    inz
008000050609     D wcpo            S                   like(i31cpo)
008100050609     D wcar            S                   like(i31car)
008200050609     D wcdi            S                   like(i31cdi)
008300020513
008400020513      *--------------------------------------------------------------*
008500020513      * === MAIN LINE PROGRAM ====================================== *
008600020513      *--------------------------------------------------------------*
008700020513      *
008800020513     C     *entry        plist
008900020513     C                   parm                    KPJBA
009000020513     C                   parm                    TRUL31DS
009100050608     C                   parm                    TRUL31DS2
009200020513      *
009300020513      * inizializzazione variabili
009400020513     C                   exsr      INZVAR
009500020513      *
009600020513      * Controllo parametri ricevuti
009700020513     C                   exsr      CTRPARM
009800020513      *
009900020513      * Intabellamento P.O. gestibili dal P.O. dell'utente
010000020513     C                   exsr      CARPOG
010100020513      *
010200020513      * Fine
010300020513     C                   EXSR      ENDPGM
010400020513
010500020513      *--------------------------------------------------------------*
010600020513      * === INIZIALIZZAZIONE VARIABILI ============================= *
010700020513      *--------------------------------------------------------------*
010800020513     C     INZVAR        BEGSR
010900020513      *
011000020513      * Impostazione iniziale dati di output
011100020513     C                   clear                   O31pog
011200050609     c                   if        %parms>2
011300050613     c                   clear                   o31dig
011400050613     c                   clear                   o31gtd
011500050613     c                   clear                   o31arg
011600050609     c                   endif
011700050609     c
011800020513     C                   reset                   $POG
011900050609     C                   reset                   $arG
012000050609     C                   reset                   $diG
012100050609     C                   reset                   OPOG
012200050609     C                   reset                   OarG
012300050609     C                   reset                   OdiG
012400020513     C                   reset                   XX
012500020513      *
012600020513     C                   reset                   AZORGDS
012700020513      *
012800020513     C     EndInzVar     ENDSR
012900020513
013000020513      *--------------------------------------------------------------*
013100020513      * === CONTROLLO PARAMETRI RICEVUTI =========================== *
013200020513      *--------------------------------------------------------------*
013300020513     C     CTRPARM       BEGSR
013400020513      *
013500020513      * - Codice Abilitazione non valido
013600020513if  1C                   if            I31abi <> *blanks
013700020513     C                             and I31abi <> 'NO'
013800020513     C                             and I31abi <> 'AZ'
013900020513     C                             and I31abi <> 'DI'
014000021113     C                             and I31abi <> 'AO'
014001131010     C                             and I31abi <> 'AC'
014100020513     C                             and I31abi <> 'TP'
014200020513     C                             and I31abi <> 'PO'
014300021113     C                             and I31abi <> 'RA'
014400041214     C**!!!                        and I31abi <> 'XX'
014500021113     C                             and I31abi <> 'AP'
014600021113     C                             and I31abi <> 'AR'
014700050614     C                             and I31abi <> 'EE'
014800020513     C                   exsr      ENDPGM
014900020513e   1C                   endif
015000020513      *
015100020513     C                   ENDSR
015200020513
015300020513      *--------------------------------------------------------------*
015400020513      * === CARICAMENTO P.O. GESTIBILI DAL P.O. UTENTE ============= *
015500020513      *--------------------------------------------------------------*
015600020513     C     CARPOG        BEGSR
015700020513      *
015800020513sel 1C                   select
015900020513      * Utente senza alcuna abilitazione
016000020513w   1C                   when      I31abi = *blanks   or
016100020513     C                             I31abi = 'NO'
016200020513      * Utente con abilitazione a tutti i P.O.
016300020513w   1C                   when      I31abi = 'AZ'
016400020513     C                   exsr      PO_AZIENDA
016500020513      * Utente con abilitazione ai P.O. del suo distretto
016600020513w   1C                   when      I31abi = 'DI'
016700020513     C                   exsr      PO_DISTRETTO
016800020513      * Utente con abilitazione ai P.O. della sua area
016900021113w   1C                   when      I31abi = 'AO'
017000020513     C                   exsr      PO_AREA
017100020513      * Utente con abilitazione ai P.O. del suo terminal di partenza
017200020513w   1C                   when      I31abi = 'TP'
017300020513     C                   exsr      PO_TERMINALP
017400020513      * Utente con abilitazione al suo solo P.O.
017500020513w   1C                   when      I31abi = 'PO'
017600020513     C                   exsr      PO_PO
017700021114      * Utente con abilitazione ai P.O. del suo raggruppamento area
017800021111w   1C                   when      I31abi = 'RA'
017900021111     C                   exsr      PO_RA
017901131010      * Utente con abilitazione ai P.O. del suo raggruppamento area contabile
017902131010w   1C                   when      I31abi = 'AC'
017903131010     C                   exsr      PO_AC
018000021114      * Utente con abilitazione ai P.O. del suo raggruppamento area e stesso AS
018100041214w   1C**!!!              when      I31abi = 'XX'
018200041214     C**!!!              exsr      PO_XX
018300021114      * Utente con abilitazione ai P.O. del suo raggruppamento area POC
018400021113w   1C                   when      I31abi = 'AP'
018500021113     C                   exsr      PO_AP
018600021114      * Utente con abilitazione ai P.O. del suo raggruppamento area EX AS400
018700021113w   1C                   when      I31abi = 'AR'
018800021113     C                   exsr      PO_AR
018900050614      * Utente con abilitazione ai P.O. esteri
019000050614w   1C                   when      I31abi = 'EE'
019100050614     C                   exsr      PO_ESTERI
019200020513      *
019300020513e   1C                   endsl
019400020513      *
019500020513     C                   ENDSR
019600020513
019700020513      *--------------------------------------------------------------*
019800020513      * === GESTIONE ABILITAZIONI PER P.O. ========================= *
019900020513      *--------------------------------------------------------------*
020000020513      *
020100020513      * === UTENTE ABILITATO A TUTTI I P.O. ======================== *
020200020513      *
020300020513     C     PO_AZIENDA    BEGSR
020400020513      *
020500050609     c                   eval      wcaricaA='S'
020600050609     c                   eval      wcaricaD='S'
020700020513     C     *loval        setll     AZORG
020800020513     C                   read      AZORG
020900020513do  1C                   dow       not  %eof(AZORG01L)
021000020513     C                   exsr      MEMOPO
021100020513     C                   read      AZORG
021200020513e   1C                   enddo
021300020513      *
021400050609     c                   movea     $pog          Opog
021500050609     c                   movea     $arg          Oarg
021600050609     c                   movea     $dig          Odig
021700020513     C                   ENDSR
021800020513      *
021900020513      * === UTENTE ABILITATO AI P.O. DEL DISTRETTO ================= *
022000020513      *
022100020513     C     PO_DISTRETTO  BEGSR
022200020513      *
022300020513      * Accedo al P.O. utente per recuperare il distretto
022400020513if  1C                   if        I31cdi = *blanks
022500050609     c                   eval      wcpo=i31cpo
022600020513     C                   exsr      REPDPO
022700050609     c                   eval      i31car=wcar
022800050609     c                   eval      i31cdi=wcdi
022900020513e   1C                   endif
023000020513      *
023100020513      * Ciclo di lettura P.O. del distretto
023200050609     c                   eval      wcaricaA='S'
023300050609     c                   eval      wcaricaD='S'
023400050609     c                   eval      wcdi=i31cdi
023500050609     c
023600050609     c                   exsr      DISTRETTO
023700050609     c
023800020513      *
023900050609     c                   movea     $pog          Opog
024000050609     c                   movea     $arg          Oarg
024100050609     c                   movea     $dig          Odig
024200050609     c
024300020513     C                   ENDSR
024400050609     c
024500050609      * === Carica da azorg02 tutti i p.o. del distretto =========== *
024600050609      *
024700050609     c     DISTRETTO     begsr
024800050609     C     wcdi          chain     AZORG02
024900050609if  1C                   if        %found(AZORG02L)
025000050609do  2C                   dou       %eof(AZORG02L)
025100050609     C                   exsr      MEMOPO
025200050609     C     wcdi          reade     AZORG02
025300050609e   2C                   enddo
025400050609e   1C                   endif
025500050609     c                   endsr
025600020513      *
025700020513      * === UTENTE ABILITATO AI P.O. DELL'AREA ===================== *
025800020513      *
025900020513     C     PO_AREA       BEGSR
026000021113      *
026100021113      * Accedo al P.O. utente per recuperare il distretto/area
026200040203if  1C                   if        I31cpo <> *zeros
026300050609     c                   eval      wcpo=i31cpo
026400021113     C                   exsr      REPDPO
026500050609     c                   eval      i31car=wcar
026600050609     c                   eval      i31cdi=wcdi
026700040203e   1C                   endif
026800020513      *
026900040203if  1C                   if            I31cdi <> *blanks
027000040203     C                             and I31car <> *zeros
027100040203      *
027200050609     c                   eval      wcar=i31car
027300050609     c                   eval      wcdi=i31cdi
027400050609     c                   eval      wcaricaD='N'
027500050609     c                   eval      wcaricaA='S'
027600050609     c
027700050609     c                   EXSR      AREA
027800040203      *
027900040203x   1C                   else
028000040203      *
028100040203      * Ciclo di lettura P.O. e selezione per area
028200040203     C     *loval        setll     AZORG
028300040203     C                   read      AZORG
028400040203do  2C                   dow       not  %eof(AZORG01L)
028500040203if  3C                   if             ORGfva = *blanks
028600040203     C                             and (ORGfag = 'A'
028700040203     C                              or  ORGfag = 'F')
028800040203     C                             and  ORGcar = I31car
028900040203     C                   add       1             XX
029000040203     C                   move      ORGfil        $POG(XX)
029100040203e   3C                   endif
029200040203     C                   read      AZORG
029300040203e   2C                   enddo
029400050609     c
029500050609     c* Carico anche l'area
029600050609     c                   movel     i31car        $arg(1)
029700040203e   1C                   endif
029800020513      *
029900050609     c                   movea     $pog          Opog
030000050609     c                   movea     $arg          Oarg
030100020513     C                   ENDSR
030200020513      *
030300050609      * === Carica p.o. area da azorg02l =========================== *
030400050609      *
030500050609     c     AREA          begsr
030600050609      * Ciclo di lettura P.O. del distretto e dell'area
030700050609     C     Korg2         chain     AZORG02
030800050609if  2C                   if        %found(AZORG02L)
030900050609do  3C                   dou       %eof(AZORG02L)
031000050609     C                   exsr      MEMOPO
031100050609     C     Korg2         reade     AZORG02
031200050609e   3C                   enddo
031300050609e   2C                   endif
031400050609     c                   ENDSR
031500050609     c
031600020513      * === UTENTE ABILITATO AI P.O. DEL TERMINAL DI PARTENZA ====== *
031700020513      *
031800020513     C     PO_TERMINALP  BEGSR
031900020513      *
032000020513     C                   reset                   TRUL06DS
032100021115     C                   movel     'L'           D06tla
032200021115     C                   movel     'PP'          D06cod
032300021115     C                   movel     I31cpo        D06key
032400020513     C                   movel(p)  TRUL06DS      KPJBU
032500020513     C                   call      'TRUL06R'
032600020513     C                   parm                    KPJBA
032700020513     C                   movel     KPJBU         TRUL06DS
032800020513if  1C                   if        D06err = *blanks
032900050609     C                   movea     D06lia        OPOG
033000020513e   1C                   endif
033100050609     c
033200050609     c* Carica se richeisto anche le aree e distretti a cui è
033300050609     c*  abilitato
033400050609     c                   exsr      CTR3parms
033500020513      *
033600020513     C                   ENDSR
033700050609     c*
033800050609     c* Vedo se caricare 3 parametro di output:aree e distretti ======
033900050609     c*
034000050609     c     CTR3PARMS     begsr
034100050609     c* Se richiesta in output anche schiera delle aree e distretti
034200050609    1c                   if        %parms>2
034300050610     c                   reset                   $arg
034400050610     c                   reset                   $dig
034500050609     c
034600050609     c* Leggo la schiera per vedere dei p.o. le aree e distretto:
034700050609     c*  se ne ho caricati tutti i p.o. --> passo indietro anche le aree/dis
034800050609     c                   eval      zz=1
034900050609    2c                   dow       Opog(zz)>*zeros
035000050609     c                   movel     Opog(zz)      wcpo
035100050609     c                   exsr      REPDpo
035200050609     c
035300050609     c* Carico tutti i p.o. dell'area (area e distetto non mi interess)
035400050610     c*   se non l'ho già elaborata
035500050610     c     wcar          lookup    carela                                 99
035600050610   2ac                   if        not *in99
035700050610     c                   z-add     1             ww
035800050610     c     000           lookup    carela(ww)                             99
035900050610     c   99              movel     wcar          carela(WW)
036000050610     c
036100050609     c                   clear                   $pog
036200050610     c                   clear                   xx
036300050609     c                   eval      wcaricaA='N'
036400050609     c                   eval      wcaricaD='N'
036500050609     c                   exsr      AREA
036600050609     c
036700050609     c* se tutti i p.o. dell'area sono anche nella schiera OPOG
036800050609     c*  in output do anche l'area
036900050609     c                   eval      ww=1
037000050609     c                   clear                   Noncarica
037100050609    3c                   dow       ww<=250  and $pog(WW)>*zeros
037200050609     c     $pog(WW)      lookup    Opog                                   99
037300050609     c                   if        not *in99
037400050609     c                   eval      Noncarica='N'
037500050609     c                   eval      ww=251
037600050609     c                   else
037700050609     c                   add       1             ww
037800050609     c                   endif
037900050609    3c                   enddo
038000050609     c
038100050609     c                   if        Noncarica=' '
038200050609     c                   eval      wcaricaA='S'
038300050609     c                   eval      wcaricaD='N'
038400050609     c                   exsr      CarAREADISTR
038500050609     c                   endif
038600050610   2ac                   endif
038700050609     c
038800050609     c* Carico tutti i p.o. del distre(area e distetto non mi interess)
038900050610     c*   se non l'ho già elaborato
039000050610     c     wcdi          lookup    cdiela                                 99
039100050610   2ac                   if        not *in99
039200050610     c                   z-add     1             ww
039300050610     c     ' '           lookup    cdiela(ww)                             99
039400050610     c   99              movel     wcdi          cdiela(WW)
039500050610     c
039600050609     c                   clear                   $pog
039700050610     c                   clear                   xx
039800050609     c                   eval      wcaricaA='N'
039900050609     c                   eval      wcaricaD='N'
040000050609     c                   exsr      DISTRETTO
040100050609     c
040200050609     c* se tutti i p.o. del Distretto sono anche nella schiera OPOG
040300050609     c*  in output do anche ildistretto
040400050609     c                   eval      ww=1
040500050609     c                   clear                   noncarica
040600050609    3c                   dow       ww<=250  and $pog(WW)>*zeros
040700050609     c     $pog(WW)      lookup    Opog                                   99
040800050609     c                   if        not *in99
040900050609     c                   eval      Noncarica='N'
041000050609     c                   eval      ww=251
041100050609     c                   else
041200050609     c                   add       1             ww
041300050609     c                   endif
041400050609    3c                   enddo
041500050609     c
041600050609     c                   if        noncarica=' '
041700050609     c                   eval      wcaricaA='n'
041800050609     c                   eval      wcaricaD='S'
041900050609     c                   exsr      CarAREADISTR
042000050609     c                   endif
042100050610   2ac                   endif
042200050609     c
042300050609     c
042400050609     c                   add       1             zz
042500050609    2c                   enddo
042600050609     c
042700050609     c                   movea     $arg          Oarg
042800050609     c                   movea     $dig          Odig
042900050609    1c                   endif
043000050609     c                   ENDSR
043100020513      *
043200021115      * === UTENTE ABILITATO AL SUO SOLO P.O. + £6 ================= *
043300020513      *
043400020513     C     PO_PO         BEGSR
043500020513      *
043600021115     C                   reset                   TRUL06DS
043700021115     C                   movel     'L'           D06tla
043800021115     C                   movel     '£6'          D06cod
043900021115     C                   movel     I31cpo        D06key
044000021115     C                   movel(p)  TRUL06DS      KPJBU
044100021115     C                   call      'TRUL06R'
044200021115     C                   parm                    KPJBA
044300021115     C                   movel     KPJBU         TRUL06DS
044400021115if  1C                   if        D06err = *blanks
044500050609     C                   movea     D06lia        OPOG
044600021115e   1C                   else
044700020513     C                   add       1             XX
044800050609     C                   move      I31cpo        OPOG(XX)
044900021115     c                   endif
045000020513      *
045100050609     c* qui sicuramente non c'e' nulla di aree e distretto per
045200050609     c*  cui non faccio nemmeno la fatica
045300020513     C                   ENDSR
045400021107      *
045500021107      * =  RAGGRUPPAMENTO AREA P.O DI UTENTE ============================= *
045600021107      *
045700021111     C     PO_RA         BEGSR
045800021107      *
045900040203if  1C                   if        I31cpo <> *zeros
046000040203      *
046100021107      * Accedo al P.O. utente per recuperare raggruppamento area
046200021107     C     I31cpo        chain     AZORG
046300040203if  2C                   if            %found(AZORG01L)
046400021107     C                             and  ORGfva = *blanks
046500021107     C                   eval      Og140 = OrgDe0
046600041214     C                   movel     §ogara        Wrar              3
046700040203e   2C                   endif
046800040203      *
046900040203x   1C                   else
047000040203      *
047100040203      * Imposto il codice area ricevuto come raggruppamento area
047200040203     C                   movel     I31car        Wrar
047300040203      *
047400040203e   1C                   endif
047500021107      *
047600021107      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
047700131010     C     *loval        setll     AZORG
047800021107     C                   read      AZORG
047900040203do  1C                   dow       not  %eof(AZORG01L)
048000021107     C                   eval      Og140 = OrgDe0
048100040203if  2C                   if             ORGfva = *blanks
048200021107     C                             and (ORGfag = 'A'
048300021107     C                              or  ORGfag = 'F')
048400021107     C                             and Wrar = §ogara
048500021107     C                   add       1             XX
048600021107     C                   move      ORGfil        $POG(XX)
048700040203e   2C                   endif
048800021107     C                   read      AZORG
048900040203e   1C                   enddo
049000021107      *
049100050609     c                   movea     $pog          Opog
049200050609     c
049300050609     c* Carica se richeisto anche le aree e distretti a cui è
049400050609     c*  abilitato
049500050609     c                   exsr      CTR3parms
049600021107     C                   ENDSR
049700021107      *
049800021107      * = RAGGRUPPAMENTO AREA P.O UTENTE E STESSO AS ====================*
049900021107      *
050000041214     C**!!!PO_XX         BEGSR
050100021107      *
050200021107      * Accedo al P.O. utente per recuperare raggruppamento area
050300041214     C**!!!I31cpo        chain     AZORG
050400041214if  1C**!!!              IF            %found(AZORG01L)
050500041214     C**!!!                        and  ORGfva = *blanks
050600041214     C**!!!              eval      Og140 = OrgDe0
050700041214     C**!!!              movel     §ogara        Wrar              3
050800041214     C**!!!              move      OrgFel        WFel              3 0
050900021107      *
051000021107      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
051100041214     C**!!!*loval        setll     AZORG
051200041214     C**!!!              read      AZORG
051300041214do  2C**!!!              dow       not  %eof(AZORG01L)
051400041214     C**!!!              eval      Og140 = OrgDe0
051500041214if  3C**!!!              if             ORGfva = *blanks
051600041214     C**!!!                        and (ORGfag = 'A'
051700041214     C**!!!                         or  ORGfag = 'F')
051800041214     C**!!!                        and Wrar = §ogara
051900041214     C**!!!                        and WFel = OrgFel
052000041214     C**!!!              add       1             XX
052100041214     C**!!!              move      ORGfil        $POG(XX)
052200041214e   3C**!!!              endif
052300041214     C**!!!              read      AZORG
052400041214e   2C**!!!              enddo
052500041214e   1C**!!!              ENDIF
052600021107      *
052700041214     C**!!!              ENDSR
052800021113      *
052900021113      * =  RAGGRUPPAMENTO AREA POC P.O DI UTENTE ======================== *
053000021113      *
053100021113     C     PO_AP         BEGSR
053200021113      *
053300040203if  1C                   if        I31cpo <> *zeros
053400040203      *
053500021113      * Accedo al P.O. utente per recuperare raggruppamento area
053600021113     C     I31cpo        chain     AZORG
053700040203if  2C                   if            %found(AZORG01L)
053800021113     C                             and  ORGfva = *blanks
053900021113     C                   eval      Og140 = OrgDe0
054000021113     C                   movel     §ogapo        Wapo              3
054100040203e   2C                   endif
054200040203      *
054300040203x   1C                   else
054400040203      *
054500040203      * Imposto il codice area ricevuto come raggruppamento area
054600040203     C                   movel     I31car        Wapo
054700040203      *
054800040203e   1C                   endif
054900021113      *
055000021113      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
055100021113     C     *loval        setll     AZORG
055200021113     C                   read      AZORG
055300021113do  1C                   dow       not  %eof(AZORG01L)
055400021113     C                   eval      Og140 = OrgDe0
055500040203if  2C                   if             ORGfva = *blanks
055600021113     C                             and (ORGfag = 'A'
055700021113     C                              or  ORGfag = 'F')
055800021113     C                             and Wapo = §ogapo
055900021113     C                   add       1             XX
056000021113     C                   move      ORGfil        $POG(XX)
056100040203e   2C                   endif
056200021113     C                   read      AZORG
056300021113e   1C                   enddo
056400050609     c
056500050609     c                   movea     $pog          Opog
056600050609     c* Carica se richeisto anche le aree e distretti a cui è
056700050609     c*  abilitato
056800050609     c                   exsr      CTR3parms
056900021113      *
057000021113     C                   ENDSR
057001131010      *____________________________________________________________
057002131010     C     PO_AC         BEGSR
057003131010      *____________________________________________________________
057004131010      *
057005131010     C                   z-add     0             XX
057006131010if  1C                   if        I31cpo <> *zeros
057007131010      *
057008131010      * Accedo al P.O. utente per recuperare raggruppamento area
057009131010     C     I31cpo        chain     AZORG
057010131010if  2C                   if        %found(AZORG01L)
057012131010     C                   movel     orgfc0        Wapo              3
057013131010e   2C                   endif
057019131010      *
057021131010      *
057022131010      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
057023131010     C     *loval        setll     AZORG
057024131010     c                   do        *hival
057025131010     C                   read      AZORG
057026131010do  1C                   if        %eof(AZORG01L)
057027131010     C                   leave
057028131010     c                   endif
057029131010     C                   movel     orgfc0        Wapo2             3
057030131010if  2C                   if        wapo2 <> wapo
057031131010     C                   iter
057035131010e   2C                   endif
057036131010     C                   add       1             XX
057037131010     C                   move      ORGfil        $POG(XX)
057038131010e   1C                   enddo
057039131010     c
057040131010     c                   movea     $pog          Opog
057041131010e   1C                   endif
057043131010      *
057044131010     C                   ENDSR
057045131010      *____________________________________________________________
057100021113      *
057200021113      * =  RAGGRUPPAMENTO AREA EX AS400 P.O DI UTENTE ============== *
057300021113      *
057400021113     C     PO_AR         BEGSR
057500021113      *
057600040203if  1C                   if        I31cpo <> *zeros
057700040203      *
057800021113      * Accedo al P.O. utente per recuperare raggruppamento area
057900021113     C     I31cpo        chain     AZORG
058000040203if  2C                   if            %found(AZORG01L)
058100021113     C                             and  ORGfva = *blanks
058200021113     C                   eval      Og140 = OrgDe0
058300021113     C                   movel     §ogaex        Waex              3
058400040203e   2C                   endif
058500040203      *
058600040203x   1C                   else
058700040203      *
058800040203      * Imposto il codice area ricevuto come raggruppamento area
058900040203     C                   movel     I31car        Waex
059000040203      *
059100040203e   1C                   endif
059200021113      *
059300021113      * Carico in schiera tutti i p.o F/A con corretto raggruppamento area
059400021113     C     *loval        setll     AZORG
059500021113     C                   read      AZORG
059600021113do  1C                   dow       not  %eof(AZORG01L)
059700021113     C                   eval      Og140 = OrgDe0
059800040203if  2C                   if             ORGfva = *blanks
059900021113     C                             and (ORGfag = 'A'
060000021113     C                              or  ORGfag = 'F')
060100021113     C                             and Waex = §ogaex
060200021113     C                   add       1             XX
060300021113     C                   move      ORGfil        $POG(XX)
060400040203e   2C                   endif
060500021113     C                   read      AZORG
060600021113e   1C                   enddo
060700050609     c
060800050609     c                   movea     $pog          Opog
060900050609     c* Carica se richeisto anche le aree e distretti a cui è
061000050609     c*  abilitato
061100050609     c                   exsr      CTR3parms
061200021113      *
061300021113     C                   ENDSR
061400020513      *--------------------------------------------------------------*
061500020513      * === REPERIMENTO DATI DEL P.O. ============================== *
061600020513      *--------------------------------------------------------------*
061700020513     C     REPDPO        BEGSR
061800020513      *
061900050609     C     wcpo          chain     AZORG
062000020513if  1C                   if            %found(AZORG01L)
062100020513     C                             and  ORGfva = *blanks
062200020513     C                             and (ORGfag = 'A'
062300020513     C                              or  ORGfag = 'F')
062400050609     C                   eval      wcdi = ORGfl3
062500050609     C                   eval      wcar = ORGcar
062600020513e   1C                   endif
062700020513      *
062800020513     C                   ENDSR
062900020513
063000020513      *--------------------------------------------------------------*
063100020513      * === MEMORIZZAZIONE P.O. IN SCHIERA ========================= *
063200020513      *--------------------------------------------------------------*
063300020513     C     MEMOPO        BEGSR
063400020513      *
063500020513if  1C                   if             ORGfva = *blanks
063600020513     C                             and (ORGfag = 'A'
063700020513     C                              or  ORGfag = 'F')
063800020513     C                   add       1             XX
063900020513     C                   move      ORGfil        $POG(XX)
064000050608     c
064100050609     c                   exsr      carAREADISTR
064200050609     c
064300050609e   1C                   endif
064400020513      *
064500020513     C                   ENDSR
064600050614      *
064700050614      * === UTENTE ABILITATO AI P.O. ESTERI ======================== *
064800050614      *
064900050614     C     PO_ESTERI     BEGSR
065000050614      *
065100050614     c                   eval      wcaricaA='N'
065200050614     c                   eval      wcaricaD='N'
065300050614     c
065400050614     C     *loval        setll     AZORG
065500050614     C                   read      AZORG
065600050614do  1C                   dow       not  %eof(AZORG01L)
065700050614     c* vedo dal ntw se estero
065800050614     c                   movel     orgde3        og143
065900050614     c                   if        §ogntw='DPD' or §ogntw='FED' or
066000050614     c                             §ogntw='EEX' or §ogntw='EUP'
066100050614     C                   exsr      MEMOPO
066200050614     c                   endif
066300050614     C                   read      AZORG
066400050614e   1C                   enddo
066500050614     c
066600050614     c                   movea     $pog          Opog
066700050614     c
066800050614     c* Carica se richeisto anche le aree e distretti a cui è
066900050614     c*  abilitato
067000050614     c                   exsr      CTR3parms
067100050614      *
067200050614     C                   ENDSR
067300050609      *--------------------------------------------------------------*
067400050609      * === Carico area e distretto ================================ *
067500050609      *--------------------------------------------------------------*
067600050609     c     carAREADISTR  begsr
067700050609     c                   if        wcaricaA<>'N'
067800050609     c                   movel     orgcar        w003a             3
067900050609     c* Carico anche area : tutti
068000050609     c     w003a         lookup    $arg                                   90
068100050609    2c                   if        not *in90
068200050609     c                   z-add     1             yy
068300050609     c     '000'         lookup    $arg(YY)                               90
068400050609    3c                   if        *in90
068500050609     c                   move      orgcar        $arg(YY)
068600050609e   3C                   endif
068700050609e   2C                   endif
068800050609e   2C                   endif
068900050609
069000050609     c* Carico anche distretto
069100050609     c                   if        wcaricaD<>'N'
069200050609     c     orgfl3        lookup    $dig                                   90
069300050609    2c                   if        not *in90
069400050609     c                   z-add     1             yy
069500050609     c     ' '           lookup    $dig(YY)                               90
069600050609    3c                   if        *in90
069700050609     c                   move      orgfl3        $dig(YY)
069800050609e   3C                   endif
069900050609e   2C                   endif
070000050609e   1C                   endif
070100050609     c
070200050609     c                   ENDSR
070300020513
070400020513      *--------------------------------------------------------------*
070500020513      * === FINE PROGRAMMA ========================================= *
070600020513      *--------------------------------------------------------------*
070700020513     C     ENDPGM        BEGSR
070800020513      *
070900020513      * Restituzione dei dati reperiti
071000050609     C                   movea     OPOG          O31POG
071100050609     c                   if        %parms>2
071200050609     C                   movea     OarG          O31arG
071300050609     C                   movea     OdiG          O31diG
071400050613     c                   if        i31abi='AZ'
071500050613     c                   eval      o31gtd='T'
071600050609     c                   endif
071700050613     c                   endif
071800020513     C                   clear                   KPJBU
071900020513      *
072000020513     C                   eval      *inLR  = *on
072100020513     C                   return
072200020513      *
072300020513     C                   ENDSR
072400020513
072500020513      *--------------------------------------------------------------*
072600020513      * === DEFINIZIONE KEY-LIST =================================== *
072700020513      *--------------------------------------------------------------*
072800020513     C     DEFKLIS       BEGSR
072900020513      *
073000020513      * AZORG02L
073100050609     C     Korg2         klist
073200050609     C                   kfld                    wcdi
073300050609     C                   kfld                    wcar
073400020513      *
073500020513     C                   ENDSR
073600020513** MSG                                                                       *
073700020513TRUL31R - Codice abilitazione errato                                           01
073800020513TRUL31R - Punto Operativo errato                                               02
