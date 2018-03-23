000100020212      *----------*
000200020213      * TRUL33R  *
000300020212      *----------*
000400020212      *
000500020212      *            REPERIMENTO/AGGIORNAMENTO NUMERATORI
000600020212      *
000700020212      * INPUT
000800020212      * -----
000900020306      *  I33TLA   Tipo di lancio
001000020306      *  I33OPE   Operazione richiesta
001100020213      *  I33SOC   Società
001200020213      *  I33AAA   Anno              (facoltativo)
001300020213      *  I33CNU   Codice numeratore
001400020213      *  I33PO1   P.O. 1
001500020213      *  I33PO2   P.O. 2
001600020213      *  I33DAC   Data Creazione    (facoltativa)
001700020213      *  I33NUM   Numero di incrementi da operare
001800020212      *
001900020212      * OUTPUT
002000020212      * ------
002100020213      *  O33ERR   Codice di errore:
002200020212      *           00 = Nessun errore
002300020306      *           01 = Numeratore non trovato in AZNUM00F
002400020306      *           02 = Record del file AZNUM00F allocato
002500020306      *           03 = Numeratore oltre il massimo del range
002600020220      *  O33MSG   Messaggio di errore
002700020213      *  O33SOC   Società
002800020213      *  O33AAA   Anno
002900020213      *  O33PO1   P.O. 1
003000020213      *  O33PO2   P.O. 2
003100020214      *  O33NRI   Primo  numero prelevato
003200020214      *  O33NRF   Ultimo numero prelevato
003300020304      *  O33INC   Incremento utilizzato o previsto
003400020213      *  O33RIN   Valore inizio range
003500020213      *  O33RFI   Valore fine range
003600020213      *  O33DAC   Data creazione
003700020213      *  O33FLO   Flag operativi
003800020212      *
003900020212      *--------------------------------------------------------------*
004000160301     /*PRM  dbgview(*source)
004100160301     /*END
004200020212
004300020212     H DECEDIT('0,') DATEDIT(*DMY.)
004400020212
004500020212     FAZORG01L  IF   E           K DISK
004600020212     FAZNUM01L  UF A E           K DISK    USROPN
004700020212     F                                     RENAME(AZNUM000:AZNUM01)
004800020212     FAZNUM02L  UF A E           K DISK    USROPN
004900020212     F                                     RENAME(AZNUM000:AZNUM02)
005000020212
005100020212      *
005200020212      * === SCHIERE ================================================ *
005300020304      *
005400020214      * Messaggi di errore
005500020212     D MSG             s             78    dim(20) ctdata perrcd(1)
005600020213      *
005700020214      * Tabella numeratori
005800020214     D $CNU            s                   like(NUMCNU)  dim(100) inz
005900020214     D $CNUD           s                   like(DCNU)    dim(100) inz
006000020212      *
006100020212      * === STRUTTURE DATI DI SISTEMA ============================== *
006200020212      *
006300020212     D STATUS         SDS           333
006400020212     D  DSPGM            *PROC
006500020212     D  PARMS            *PARMS
006600020212      *
006700020212      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
006800020212      *
006900020212      * Passaggio Parametri
007000020212     D KPJBA         e ds
007100020214     D TRUL33DS      e ds                  inz
007200020225     D  I33tla       e                     inz('L')
007300020723      *
007400020723      * Passaggio Parametri al pgm FNLV55R
007500020723     D FNLV55DS      e ds                  inz
007600020723     D  D55tpt       e                     inz('P')
007700020212      *
007800020212      * Passaggio Parametri al pgm TIBS02R
007900020212     D TIBS02DS      e ds                  inz
008000020212     D  T02mod       e                     inz('C')
008100020212     D  T02cod       e                     inz('CNU')
008200020212      *
008300020212      * Tabella CNU = Codici Numeratori di Gruppo
008400020212     D DCNU          e ds                  inz
008500020212      *
008600020212      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
008700020214      *
008800020214      * Ridefinizione campi del file AZORG
008900020214      * (per semplificarne la pulizia con un semplice CLEAR)
009000020214     D AZORGDS         ds                  inz
009100020214     D  ORGdit
009200020214     D  ORGfil
009300020214     D  ORGfva
009400020214     D  ORGfag
009500020214     D  ORGdes
009600020214     D  ORGfel
009700020214      *
009800020214      * Salvataggio dati file AZORG per P.O.1
009900020214     D AZORG1DS        ds                  inz
010000020214     D  PO1ORGdit                          like(ORGdit)
010100020214     D  PO1ORGfil                          like(ORGfil)
010200020214     D  PO1ORGfva                          like(ORGfva)
010300020214     D  PO1ORGfag                          like(ORGfag)
010400020214     D  PO1ORGdes                          like(ORGdes)
010500020214     D  PO1ORGfel                          like(ORGfel)
010600020214      *
010700020214      * Salvataggio dati file AZORG per P.O.2
010800020214     D AZORG2DS        ds                  inz
010900020214     D  PO2ORGdit                          like(ORGdit)
011000020214     D  PO2ORGfil                          like(ORGfil)
011100020214     D  PO2ORGfva                          like(ORGfva)
011200020214     D  PO2ORGfag                          like(ORGfag)
011300020214     D  PO2ORGdes                          like(ORGdes)
011400020214     D  PO2ORGfel                          like(ORGfel)
011500020212      *
011600020212      * DS per inversione data
011700020212     D WLBDAT          DS                  INZ
011800020212     D  G02DAT                 1      8  0
011900020212     D  G02INV                 9     16  0
012000020212     D  G02ERR                17     17
012100020212     D  G02TGI                18     22  0
012200020212      *
012300020212      * === VARIABILI ============================================== *
012400020212      *
012500020215     D SAVcnu          S                   like(NUMcnu)  inz(*loval)
012600020215     D SAVunp          S                   like(NUMunp)  inz(*loval)
012700020215     D wMAXnfi         S                   like(NUMunp)  inz(*hival)
012800020215     D PRIunp          S             30  0 inz
012900020215     D ULTunp          S             30  0 inz
013000020212     D wPO             S              3    inz(*zeros)
013100020212     D wDATE           S              8  0 inz
013200020213     D XX              S              3  0 inz
013300020212     D wVL             S              2    inz
013400020212     D $Write          S              1    inz(*off)
013500020212      *
013600020212      * === COSTANTI =============================================== *
013700020212      *
013800020212
013900020212      *--------------------------------------------------------------*
014000020212      * === MAIN LINE PROGRAM ====================================== *
014100020212      *--------------------------------------------------------------*
014200020212      *                                                              *
014300020212      * RIEPILOGO INDICATORI                                         *
014400020212      * --------------------                                         *
014500020213      *  10 - Comodo                                                 *
014600020212      *  21 - Record non trovato                                     *
014700020212      *  22 - Record allocato oppure già esistente in fase di write  *
014800020212      *                                                              *
014900020212      *--------------------------------------------------------------*
015000020212      *
015100020212     C     *entry        plist
015200020212     C                   parm                    KPJBA
015300020212      *
015400020212      * inizializzazione variabili
015500020212     C                   exsr      INZVAR
015600020212      *
015700020212sel 1C                   select
015800020212      *
015900020212      * Chiusura archivi
016000020213w   1C                   when      I33tla = 'C'
016100020212     C                   exsr      CLONUM
016200020212      *
016300020212      * Lettura ultimo progressivo usato + incremento + aggiornamento
016400020306w   1C                   when      I33ope = 0
016500020212     C                   exsr      CHANUM
016600020212     C                   exsr      INCNUM
016700020215if  2C                   if        ULTunp <> SAVunp
016800020215     C                   z-add     ULTunp        NUMunp
016900020212     C                   exsr      UPDNUM
017000020212e   2C                   endif
017100020212      *
017200020212      * Lettura ultimo progressivo usato
017300020306w   1C                   when      I33ope = 1
017400020212     C                   exsr      CHANUM
017500020305     C                   exsr      INCNUM
017600020212      *
017700020307      * Aggiornamento numero (ultimo prelevato) con quello ricevuto
017800020214w   1C                   when      I33ope = 2
017900020212     C                   exsr      CHANUM
018000020212     C                   eval      SAVunp = NUMunp
018100020215     C                   eval      PRIunp = I33unp
018200020215     C                   eval      ULTunp = I33unp
018300020212     C                   exsr      CTRNUM
018400020408     C                   eval      NUMunp = I33unp
018500020212     C                   exsr      UPDNUM
018600020212      *
018700020307      * Forzatura numero (ultimo prelevato) con quello ricevuto
018800020212      * (nessun controllo)
018900020214w   1C                   when      I33ope = 3
019000020212     C                   exsr      CHANUM
019100020213     C                   eval      NUMunp = I33unp
019200020306     C                   eval      PRIunp = I33unp
019300020306     C                   eval      ULTunp = I33unp
019400020212     C                   exsr      UPDNUM
019500020212      *
019600020212e   1C                   endsl
019700020212      *
019800020212      * Fine
019900020212     C                   EXSR      ENDPGM
020000020212
020100020212      *--------------------------------------------------------------*
020200020212      * === OPERAZIONI INIZIALI ==================================== *
020300020212      *--------------------------------------------------------------*
020400020212     C     *INZSR        BEGSR
020500020212      *
020600020212      * Data odierna in formato AAAAMMGG
020700020212     C                   clear                   WLBDAT
020800020212     C                   move      *date         G02dat
020900020212     C                   call      'XSRDA8'
021000020212     C                   parm                    WLBDAT
021100020212     C                   move      G02inv        wDATE
021200020212      *
021300020212     C                   ENDSR
021400020212
021500020212      *--------------------------------------------------------------*
021600020212      * === INIZIALIZZAZIONE VARIABILI ============================= *
021700020212      *--------------------------------------------------------------*
021800020212     C     INZVAR        BEGSR
021900020212      *
022000020212      * Reperimento parametri
022100020212if  1C                   if        KPJBU <> *blanks
022200020214     C                   movel     KPJBU         TRUL33DS
022300020212x   1C                   else
022400020225     C                   reset                   TRUL33DS
022500020212e   1C                   endif
022600020212      *
022700020212      * Impostazione dati di output
022800020215     C                   clear                   O33err
022900020213     C                   clear                   O33msg
023000020213     C                   clear                   O33soc
023100020213     C                   clear                   O33aaa
023200020213     C                   clear                   O33po1
023300020213     C                   clear                   O33po2
023400020213     C                   clear                   O33nri
023500020213     C                   clear                   O33nrf
023600020213     C                   clear                   O33inc
023700020213     C                   clear                   O33rin
023800020213     C                   clear                   O33rfi
023900020213     C                   clear                   O33dac
024000020213     C                   clear                   O33flo
024100020212     C                   reset                   SAVunp
024200020212     C                   reset                   PRIunp
024300020215     C                   reset                   ULTunp
024400020212     C                   reset                   $Write
024500020212      *
024600020212      * Se chiesta solo la chiusura degli archivi: basta così !
024700020213     C     I33tla        cabeq     'C'           EndInzVar
024800020306      *
024900020306      * Controllo parametri ricevuti
025000020306     C                   exsr      srCtrParm
025100020212      *
025200020214      * Reperimento dati tabella 'CNU'
025300020213if  1C                   if        SAVcnu <> I33cnu
025400020214     C                   exsr      srRepTbCNU
025500020214     C                   eval      SAVcnu =  I33cnu
025600020212e   1C                   endif
025700020212      *
025800020212      * Verifica della vista logica da elaborare
025900020212sel 1C                   select
026000020212w   1C                   when      §CNUfaa = 'S'
026100020212     C                   eval      wVL     = '01'
026200020212w   1C                   when      §CNUkda = 'S'
026300020212     C                   eval      wVL     = '02'
026400020212x   1C                   other
026500020212     C                   eval      wVL     = '01'
026600020212e   1C                   endsl
026700020212      *
026800020212      * Apertura archivio da elaborare
026900020212sel 1C                   select
027000020212w   1C                   when          wVL     = '01'
027100020306     C                             and not %open(AZNUM01L)
027200020212     C                   open      AZNUM01L
027300020212w   1C                   when          wVL     = '02'
027400020306     C                             and not %open(AZNUM02L)
027500020212     C                   open      AZNUM02L
027600020212e   1C                   endsl
027700020212      *
027800020212      * Preparazione klist secondo tabella "CNU"
027900020212      * - codice società
028000020213     C                   eval      NUMsoc  = I33soc
028100020212      * - anno
028200020306      *   (impostato automaticamente se non ricevuto come parametro)
028300020212if  1C                   if        §CNUfaa = 'S'
028400020213if  2C                   if        I33aaa  = *zeros
028500020212     C                   eval      NUMaaa  = *year
028600020212x   2C                   else
028700020213     C                   eval      NUMaaa  = I33aaa
028800020212e   2C                   endif
028900020212x   1C                   else
029000020212     C                   clear                   NUMaaa
029100020212e   1C                   endif
029200020212      * - codice numeratore
029300020213     C                   eval      NUMcnu  = I33cnu
029400020212      * - primo punto operativo
029500020306      *   (non prevista l'impostazione automatica)
029600020306sel 1C***                select
029700020306w   1C***                when      §CNUpo1 = *blanks
029800020306     C***                clear                   NUMpo1
029900020306w   1C***                when      §CNUpo1 > *zeros
030000020306     C***                move      §CNUpo1       NUMpo1
030100020306x   1C***                other
030200020213     C                   eval      NUMpo1  = I33po1
030300020306e   1C***                endsl
030400020212      * - secondo punto operativo
030500020306      *   (non prevista l'impostazione automatica)
030600020306sel 1C***                select
030700020306w   1C***                when      §CNUpo2 = *blanks
030800020306     C***                clear                   NUMpo2
030900020306w   1C***                when      §CNUpo2 > *zeros
031000020306     C***                move      §CNUpo2       NUMpo2
031100020306x   1C***                other
031200020213     C                   eval      NUMpo2  = I33po2
031300020306e   1C***                endsl
031400020212      * - data in chiave
031500020306      *   (impostata automaticamente se non ricevuta come parametro)
031600020306if  1C                   if        §CNUkda = 'S'
031700020306if  2C                   if        I33dac  = *zeros
031800020306     C                   eval      NUMdac  = wDATE
031900020306x   2C                   else
032000020213     C                   eval      NUMdac  = I33dac
032100020306e   2C                   endif
032200020306x   1C                   else
032300020306     C                   clear                   NUMdac
032400020306e   1C                   endif
032500020212      *
032600020212     C     EndInzVar     ENDSR
032700020212
032800020212      *--------------------------------------------------------------*
032900020212      * === CONTROLLO PARAMETRI RICEVUTI =========================== *
033000020212      *--------------------------------------------------------------*
033100020212     C     srCtrParm     BEGSR
033200020212      *
033300020212sel 1C                   select
033400020213      * - tipo di lancio non valido
033500020306w   1C                   when           I33tla <> *blank
033600020306     C                              and I33tla <> 'C'
033700020306     C                              and I33tla <> 'L'
033800020306     C                   reset                   SAVcnu
033900020306     C                   reset                   DCNU
034000020306     C                   exsr      ENDPGM
034100020212      * - operazione richiesta non valida
034200020213w   1C                   when           I33ope <  0
034300020213     C                              or  I33ope >  3
034400020306     C                   reset                   SAVcnu
034500020306     C                   reset                   DCNU
034600020306     C                   exsr      ENDPGM
034700020212      * - anno obbligatorio
034800020306      *   => impostato automaticamente se non ricevuto e richiesto
034900020213      * - data creazione obbligatoria
035000020306      *   => impostata automaticamente se non ricevuta e richiesta
035100020212e   1C                   endsl
035200020212      *
035300020213cas 1C     O33err        casne     *zeros        ENDPGM
035400020212e   1C                   endcs
035500020212      *
035600020212     C                   ENDSR
035700020213
035800020214      *--------------------------------------------------------------*
035900020214      * === REPERIMENTO DATI TAB. CNU ============================== *
036000020214      *--------------------------------------------------------------*
036100020214     C     srRepTbCNU    BEGSR
036200020306      *
036300020306     C                   clear                   DCNU
036400020214      *
036500020214      * Controllo se già memorizzato in schiera
036600020214     C                   eval      *in10     = *off
036700020214     C                   eval      XX        = 1
036800020214     C     I33cnu        lookup    $CNU(xx)                               10
036900020214      *
037000020214if  1C                   if        *in10
037100020306     C                             and   $CNUD(xx) <> *blanks
037200020214      * - già memorizzato -> reperisco i dati
037300020214      *                      e segnalo errore se codice errato
037400020214     C                   movel     $CNUD(xx)     DCNU
037500020214      *
037600020214x   1C                   else
037700020214      * - non ancora memorizzato -> memorizzo codice e dati tabella
037800020214      *                             e segnalo errore se codice errato
037900020214     C                   reset                   TIBS02DS
038000020214     C                   movel     KNSIF         T02sif
038100020214     C                   movel     I33cnu        T02ke1
038200020214     C                   call      'TIBS02R'
038300020214     C                   parm                    KPJBA
038400020214     C                   parm                    TIBS02DS
038500020214if  2C                   if        T02err    = *blanks
038600020214     C                   movel     T02uni        DCNU
038700020214     C     *zeros        lookup    $CNU(xx)                               10
038800020214if  3C                   if        *in10
038900020214     C                   eval      $CNU(xx)  = I33cnu
039000020214     C                   movel     DCNU          $CNUD(xx)
039100020214e   3C                   endif
039200020214x   2C                   else
039300020214     C                   eval      O33err =  01
039400020214     C                   exsr      ENDPGM
039500020214e   2C                   endif
039600020214      *
039700020214e   1C                   endif
039800020214      *
039900020214     C                   ENDSR
040000020214
040100020214      *--------------------------------------------------------------*
040200020214      * === CONTROLLO P.O. PER CREAZIONE =========================== *
040300020214      *--------------------------------------------------------------*
040400020214     C     srCtrPO       BEGSR
040500020214      *
040600020214      * Controllo P.O.1
040700020214if  1C                   if        I33po1  =  *zeros
040800020214     C                   clear                   AZORG1DS
040900020214e   1C                   endif
041000020214if  1C                   if        I33po1  <> PO1ORGfil
041100020214if  2C                   if        I33po1  <> ORGfil
041200020214     C                   clear                   AZORG1DS
041300020214     C                   clear                   AZORGDS
041400020214     C                   z-add     I33po1        ORGfil
041500020214     C     K01ORG01      chain     AZORG01L
041600020214if  3C                   if        not %found(AZORG01L)
041700020214     C                             and §CNUp1n <> 'S'
041800020214     C                   move      *loval        PO1ORGfil
041900020306     C                   eval      O33err  =  01
042000020214     C                   exsr      ENDPGM
042100020214e   3C                   endif
042200020214e   2C                   endif
042300020214     C                   movel     AZORGDS       AZORG1DS
042400020214e   1C                   endif
042500020214if  1C                   if            §CNUp1n    <> 'S'
042600020304     C                             and §CNUpo1    <> *blanks
042700020214     C                             and (PO1ORGfag <> 'A'
042800020214     C                             and  PO1ORGfag <> 'F'
042900020214     C                              or  PO1ORGfva <> *blanks)
043000020306     C                   eval      O33err  =  01
043100020214     C                   exsr      ENDPGM
043200020214e   1C                   endif
043300020214      *
043400020214      * Controllo P.O.2
043500020214if  1C                   if        I33po2  =  *zeros
043600020214     C                   clear                   AZORG2DS
043700020214e   1C                   endif
043800020214if  1C                   if        I33po2  <> PO2ORGfil
043900020214if  2C                   if        I33po2  <> ORGfil
044000020214     C                   clear                   AZORG2DS
044100020214     C                   clear                   AZORGDS
044200020214     C                   z-add     I33po2        ORGfil
044300020214     C     K01ORG01      chain     AZORG01L
044400020214if  3C                   if        not %found(AZORG01L)
044500020214     C                             and §CNUp2n <> 'S'
044600020214     C                   move      *loval        PO2ORGfil
044700020306     C                   eval      O33err  =  01
044800020214     C                   exsr      ENDPGM
044900020214e   3C                   endif
045000020214e   2C                   endif
045100020214     C                   movel     AZORGDS       AZORG2DS
045200020214e   1C                   endif
045300020214if  1C                   if            §CNUp2n    <> 'S'
045400020304     C                             and §CNUpo2    <> *blanks
045500020214     C                             and (PO2ORGfag <> 'A'
045600020214     C                             and  PO2ORGfag <> 'F'
045700020214     C                              or  PO2ORGfva <> *blanks)
045800020306     C                   eval      O33err  =  01
045900020214     C                   exsr      ENDPGM
046000020214e   1C                   endif
046100020723      *
046200020723      * reperisco il 1° livello del p.o.1
046300050708      * (se NON ammessi P.O. non validi   &
046400050708      *  se richiesto P.O.)
046500050708if  1***C                   if        I33po1 <> D55lin
046600050708if  1C                   if            §CNUp1n    <> 'S'
046700050708     C                             and §CNUpo1    <> *blanks
046800050708     C                             and I33po1     <> D55lin
046900020723     C                   reset                   FNLV55DS
047000020723     C                   eval      D55lin  = I33po1
047100020723     C                   eval      D55drf  = wDATE
047200020723     C                   call      'FNLV55R'
047300020723     C                   parm                    FNLV55DS
047400020723e   1C                   endif
047500020723if  1C                   if        D55err <> *blanks
047600020723     C                   eval      O33err  =  01
047700020723     C                   exsr      ENDPGM
047800020723e   1C                   endif
047900020214      *
048000020214      * altri controlli sui P.O.
048100020214sel 1C                   select
048200050708      * - ammesso p.o.1 non valido   o   previsto nessun p.o.1
048300050708w   1C                   when          §CNUp1n    =  'S'
048400050708     C                             or  §CNUpo1    =  *blanks
048500020214      * - p.o.1 non del livello richiesto
048600020214w   1C                   when           I33po1 <> *zeros
048700020723     C                             and (I33po1 <> D55tfp
048800020214     C                             and  §CNUfnu = 'S')
048900020723     C                             or  (I33po1  = D55tfp
049000020214     C                             and  §CNUfnu = 'L')
049100020306     C                   eval      O33err  =  01
049200050708e   1C                   endsl
049300050708sel 1C***                select
049400050708      * - ammesso p.o.2 non valido   o   previsto nessun p.o.2
049500050708w   1C***                when          §CNUp2n    =  'S'
049600050708     C***                          or  §CNUpo2    =  *blanks
049700020214      * - p.o.2 non del livello richiesto
049800020214w   1C***                when           I33po2 <> *zeros
049900020214     C***                          and (I33po2 <> PO2ORGfel
050000020214     C***                          and  §CNUfnu = 'S')
050100020214     C***                          or  (I33po2  = PO2ORGfel
050200020214     C***                          and  §CNUfnu = 'L')
050300020306     C***                eval      O33err  =  01
050400050708e   1C***                endsl
050500020214      *
050600020214cas 1C     O33err        casne     *zeros        ENDPGM
050700020214e   1C                   endcs
050800020214      *
050900020214      * p.o.1 non previsto per questo numeratore
051000020304if  1C                   if            §CNUpo1 <> 'TUT'
051100020304     C                             and §CNUpo1 <> 'FIL'
051200020214     C                   move      I33po1        wPO
051300020305if  2C                   if             §CNUpo1 <> wPO
051400020305     C                             and (§CNUpo1 <> *blanks
051500020305     C                              or  wPO     <> *zeros)
051600020306     C                   eval      O33err  =  01
051700020214     C                   exsr      ENDPGM
051800020214e   2C                   endif
051900020214e   1C                   endif
052000020305      *
052100020214      * p.o.2 non previsto per questo numeratore
052200020304if  1C                   if            §CNUpo2 <> 'TUT'
052300020304     C                             and §CNUpo2 <> 'FIL'
052400020214     C                   move      I33po2        wPO
052500020305if  2C                   if             §CNUpo2 <> wPO
052600020305     C                             and (§CNUpo2 <> *blanks
052700020305     C                              or  wPO     <> *zeros)
052800020306     C                   eval      O33err  =  01
052900020214     C                   exsr      ENDPGM
053000020214e   2C                   endif
053100020214e   1C                   endif
053200020214      *
053300020214     C                   ENDSR
053400020212
053500020212      *--------------------------------------------------------------*
053600020212      * === AGGANCIO FILE AZNUM ==================================== *
053700020212      *--------------------------------------------------------------*
053800020212     C     CHANUM        BEGSR
053900020212      *
054000020212      * se richiesta solo lettura, non alloco
054100020213if  1C                   if        I33ope  =  1
054200020212      *
054300020212sel 2C                   select
054400020212w   2C                   when      wVL = '01'
054500020212     C     K05NUM01      chain(n)  AZNUM01                            2122
054600020212w   2C                   when      wVL = '02'
054700020212     C     K05NUM02      chain(n)  AZNUM02                            2122
054800020212e   2C                   endsl
054900020212      *
055000020212      * se richiesto anche aggiornamento, alloco
055100020212x   1C                   else
055200020212      *
055300020212sel 2C                   select
055400020212w   2C                   when      wVL = '01'
055500020212     C     K05NUM01      chain     AZNUM01                            2122
055600020212w   2C                   when      wVL = '02'
055700020212     C     K05NUM02      chain     AZNUM02                            2122
055800020212e   2C                   endsl
055900020212      *
056000020212e   1C                   endif
056100020212      *
056200020212     C                   movel     *in21         $Write
056300160301      *
056400160301      * Gestione RCD allocato:
056500160301      *  se il rcd da gestire è allocato da un altro lavoro,
056600160301      *  ritorno al pgm chiamante avvertendolo
056700160301if  1C                   if        *in22
056800160301     C                   eval      O33err  =  02
056900160301     C                   exsr      ENDPGM
057000160301e   1C                   endif
057100020212      *
057200020212      * Gestione RCD non trovato:
057300020214if  1C                   if        $Write  =  *on
057400020214      *
057500020213if  2C                   if        I33ope  <> 1
057600020212      *  se è richiesta la creazione se inesistente,
057700020306      *  ma il progressivo non è creabile,
057800020212      *  ritorno al pgm chiamante avvertendolo
057900020308if  3C                   if        §CNUsca <> 'S'
058000020306     C                   eval      O33err  =  01
058100020212     C                   exsr      ENDPGM
058200020215e   3C                   endif
058300020215      *  ripulisco i dati del numeratore per crearlo
058400020212     C                   clear                   NUMunp
058500020212     C                   clear                   NUMrin
058600020212     C                   clear                   NUMrfi
058700020212     C                   clear                   NUMflo
058800020214      *  controllo P.O.1 e P.O.2 se richiesta la creazione
058900020214     C                   exsr      srCtrPO
059000020212x   2C                   else
059100020215      *  se è richiesto solo il reperimento del numeratore,
059200020212      *  ritorno al pgm chiamante avvertendolo
059300020306     C                   eval      O33err  =  01
059400020212     C                   exsr      ENDPGM
059500020212e   2C                   endif
059600020214      *
059700020212e   1C                   endif
059800020212      *
059900020212     C                   ENDSR
060000020212
060100020212      *--------------------------------------------------------------*
060200020212      * === INCREMENTO PROGRESSIVO ================================= *
060300020212      *--------------------------------------------------------------*
060400020212     C     INCNUM        BEGSR
060500020212      *
060600020215     C                   eval      SAVunp =  NUMunp
060700020212      *
060800020215if  1C                   if        I33num <> 0
060900020212      *
061000020408if  2C                   if        I33num >= 0
061100020304     C                   eval      PRIunp =  NUMunp + §CNUinc
061200020408x   2C                   else
061300020304     C                   eval      PRIunp =  NUMunp - §CNUinc
061400020408e   2C                   endif
061500020215     C                   eval      ULTunp =  NUMunp + §CNUinc * I33num
061600020212      *
061700020212     C                   exsr      CTRNUM
061800020305      *
061900020305x   1C                   else
062000020305      *
062100020305     C                   eval      PRIunp =  NUMunp
062200020305     C                   eval      ULTunp =  NUMunp
062300020212      *
062400020212e   1C                   endif
062500020212      *
062600020212     C                   ENDSR
062700020212
062800020212      *--------------------------------------------------------------*
062900020212      * === CONTROLLO PROGRESSIVO ================================== *
063000020212      *--------------------------------------------------------------*
063100020212     C     CTRNUM        BEGSR
063200020212      *
063300020306      * se superato il valore finale del range di validità
063400020212      * se previsto il controllo con il numero esistente e
063500020212      *    nuovo valore inferiore al precedente
063600020212      * se bypassato il massimo consentito dalla lunghezza del campo
063700020306      *    (ovvero: se ultimo numero:    999999999999999
063800020306      *             e  incremento   :    1
063900020306      *             => risultato... : (1)000000000000000 - NO BUONO !!!
064000020306      *                sistemazione :    000000000000001 - OK)
064100020304if  1C                   if        NUMrin = 0
064200020212     C                   eval      NUMrin = §CNUrin
064300020212e   1C                   endif
064400020304if  1C                   if        NUMrfi = 0
064500020212     C                   eval      NUMrfi = §CNUrfi
064600020212e   1C                   endif
064700020215      *
064800020215      * controllo possibili casi "indesiderati"
064900020215      * - questa IF iniziale può velocizzare l'elaborazione se non
065000020215      *   risulta verificato nessun caso tra quelli previsti nelle
065100020215      *   varie WHEN - che non vengono, quindi, testate); nel caso
065200020215      *   in cui, invece, risulta verificato uno dei casi previsti,
065300020215      *   l'elaborazione si rallenta del tempo necessario all'ese-
065400020215      *   cuzione di questo confronto iniziale.
065500020215      *   Ma, tutto sommato, non si dovrebbe verificare spesso...
065600020304if  1C                   if             (PRIunp  >  NUMrfi
065700020304     C                              or   PRIunp  <  NUMrin
065800020304     C                              or   ULTunp  >  NUMrfi
065900020304     C                              or   ULTunp  <  NUMrin)
066000020215     C                             or   (§CNUctn =  'S'
066100020215     C                              and (PRIunp  <  SAVunp
066200020215     C                               or  ULTunp  <  SAVunp))
066300020212      *
066400020212      * reimposto il progressivo
066500020212sel 2C                   select
066600020215      * - ultimo numero prelevato da forzare errato
066700020215w   2C                   when           I33ope  =  2
066800020215     C                             and (I33unp  <  NUMrin
066900020215     C                              or  I33unp  >  NUMrfi)
067000020306     C                   eval      O33err  =  03
067100020215     C                   exsr      ENDPGM
067200020305      * - previsto il blocco dell'incremento se oltre il massimo previsto
067300020304w   2C                   when           §CNUbio =  'S'
067400020213     C                             and (PRIunp  >  NUMrfi
067500020215     C                              or  ULTunp  >  NUMrfi)
067600020306     C                   eval      O33err  =  03
067700020212     C                   exsr      ENDPGM
067800020305      * - previsto controllo con progressivo esistente
067900020213w   2C                   when           §CNUctn =  'S'
068000020213     C                             and (PRIunp  <  SAVunp
068100020215     C                              or  ULTunp  <  SAVunp)
068200020213     C                   eval      PRIunp  =  SAVunp
068300020215     C                   eval      ULTunp  =  SAVunp
068400020306      * - progressivo calcolato non compreso nel range
068500020304w   2C                   when          (PRIunp >  NUMrfi
068600020213     C                              or  PRIunp <  NUMrin
068700020215     C                              or  ULTunp >  NUMrfi
068800020215     C                              or  ULTunp <  NUMrin)
068900020215if  3C                   if        I33num  >= 0
069000020305     C                   eval      PRIunp  =  NUMrin
069100020305     C                   eval      ULTunp  =  NUMrin + §CNUinc * (I33num - 1)
069200020215if  4C                   if        ULTunp  >  NUMrfi
069300020306     C                   eval      O33err  =  03
069400020212     C                   exsr      ENDPGM
069500020215e   4C                   endif
069600020215x   3C                   else
069700020305     C                   eval      PRIunp  =  NUMrfi
069800020305     C                   eval      ULTunp  =  NUMrfi + §CNUinc * (I33num + 1)
069900020215if  4C                   if        ULTunp  <  NUMrin
070000020306     C                   eval      O33err  =  03
070100020215     C                   exsr      ENDPGM
070200020215e   4C                   endif
070300020215e   3C                   endif
070400020215      *
070500020212e   2C                   endsl
070600020212      *
070700020215e   1C                   endif
070800020212      *
070900020212     C                   ENDSR
071000020212
071100020212      *--------------------------------------------------------------*
071200020212      * === AGGIORNAMENTO/SCRITTURA RECORD ========================= *
071300020212      *--------------------------------------------------------------*
071400020212     C     UPDNUM        BEGSR
071500020212      *
071600020212if  1C                   if        $Write = *on
071700020212      *
071800020212     C                   exsr      RIENUM
071900020212sel 2C                   select
072000020212w   2C                   when      wVL = '01'
072100020306     C                   write     AZNUM01
072200020212w   2C                   when      wVL = '02'
072300020306     C                   write     AZNUM02
072400020212e   2C                   endsl
072500020212      *
072600020212x   1C                   else
072700020212      *
072800020212sel 2C                   select
072900020212w   2C                   when      wVL = '01'
073000020212     C                   update    AZNUM01
073100020212w   2C                   when      wVL = '02'
073200020212     C                   update    AZNUM02
073300020212e   2C                   endsl
073400020212      *
073500020212e   1C                   endif
073600020212      *
073700020212     C                   ENDSR
073800020212
073900020212      *--------------------------------------------------------------*
074000020212      * === IMPOSTAZIONE ALTRI DATI ================================ *
074100020212      *--------------------------------------------------------------*
074200020212     C     RIENUM        BEGSR
074300020212      *
074400020212      * Codice Società
074500020213     C                   eval      NUMsoc  = I33soc
074600020212      * Anno
074700020212if  1C                   if        §CNUfaa = 'S'
074800020213if  2C                   if        I33aaa  = *zeros
074900020212     C                   eval      NUMaaa  = *year
075000020212x   2C                   else
075100020213     C                   eval      NUMaaa  = I33aaa
075200020212e   2C                   endif
075300020212x   1C                   else
075400020212     C                   clear                   NUMaaa
075500020212e   1C                   endif
075600020212      * Codice Numeratore
075700020213     C                   eval      NUMcnu  = I33cnu
075800020212      * Primo Punto Operativo
075900020304if  1C                   if        §CNUpo1 = *blanks
076000020304     C                   clear                   NUMpo1
076100020304x   1C                   else
076200020213     C                   eval      NUMpo1  = I33po1
076300020304e   1C                   endif
076400020212      * Secondo Punto Operativo
076500020304if  1C                   if        §CNUpo2 = *blanks
076600020304     C                   clear                   NUMpo2
076700020304x   1C                   else
076800020213     C                   eval      NUMpo2  = I33po2
076900020304e   1C                   endif
077000020212      * Ultimo Numero prelevato
077100020213if  1C                   if        I33ope  = 2   or
077200020213     C                             I33ope  = 3
077300020213     C                   eval      NUMunp  = I33unp
077400020215x   1C                   else
077500020215     C                   eval      NUMunp  = ULTunp
077600020212e   1C                   endif
077700020212      * Range (inizio & fine)
077800020212     C                   eval      NUMrin  = §CNUrin
077900020212     C                   eval      NUMrfi  = §CNUrfi
078000020212      * Data Creazione
078100020307if  1C                   if        I33dac  = *zeros
078200020307     C                   eval      NUMdac  = wDATE
078300020307x   1C                   else
078400020307     C                   eval      NUMdac  = I33dac
078500020307e   1C                   endif
078600020212      * Flag Operativi
078700020212     C                   eval      NUMflo  = *blanks
078800020212      *
078900020212     C                   ENDSR
079000020212
079100020212      *--------------------------------------------------------------*
079200020212      * === CHIUSURA ARCHIVI ======================================= *
079300020212      *--------------------------------------------------------------*
079400020212     C     CLONUM        BEGSR
079500020212      *
079600020212     C                   clear                   TIBS02DS
079700020212     C                   movel     'C'           T02tla
079800020212     C                   movel     KNSIF         T02sif
079900020212     C                   call      'TIBS02R'
080000020212     C                   parm                    KPJBA
080100020212     C                   parm                    TIBS02DS
080200020212      *
080300020306if  1C                   if        %open(AZNUM01L)
080400020212     C                   close     AZNUM01L
080500020212e   1C                   endif
080600020306if  1C                   if        %open(AZNUM02L)
080700020212     C                   close     AZNUM02L
080800020212e   1C                   endif
080900020212      *
081000020212     C                   ENDSR
081100020212
081200020212      *--------------------------------------------------------------*
081300020212      * === FINE PROGRAMMA ========================================= *
081400020212      *--------------------------------------------------------------*
081500020212     C     ENDPGM        BEGSR
081600020212      *
081700020212      * restituzione dei dati reperiti e calcolati
081800020212sel 1C                   select
081900020212      *
082000020213w   1C                   when          O33err =  *zeros
082100020213     C                             and I33tla <> 'C'
082200020213     C                   eval      O33soc = NUMsoc
082300020213     C                   eval      O33aaa = NUMaaa
082400020213     C                   eval      O33po1 = NUMpo1
082500020213     C                   eval      O33po2 = NUMpo2
082600020213     C                   eval      O33nri = PRIunp
082700020215     C                   eval      O33nrf = ULTunp
082800020213     C                   eval      O33inc = §CNUinc
082900020213     C                   eval      O33rin = NUMrin
083000020213     C                   eval      O33rfi = NUMrfi
083100020213     C                   eval      O33dac = NUMdac
083200020213     C                   eval      O33flo = NUMflo
083300020212      *
083400020213w   1C                   when      O33err > 0
083500020213     C                   eval      O33msg = MSG(O33err)
083600020212      *
083700020212e   1C                   endsl
083800020212      *
083900020214     C                   movel(p)  TRUL33DS      KPJBU
084000020212      *
084100020212      * disallocazione record (se allocato per aggiornamento)
084200020214if  1C                   if        I33ope <> 1
084300020212     C                   unlock    AZNUM01L                             22
084400020212e   1C                   endif
084500020212      *
084600020214if  1C                   if        I33tla =  *blanks
084700020212     C                   movel     *on           *inRT
084800020212x   1C                   else
084900020212     C                   movel     *on           *inLR
085000020212e   1C                   endif
085100020212     C                   return
085200020212      *
085300020212     C                   ENDSR
085400020212
085500020212      *--------------------------------------------------------------*
085600020212      * === DEFINIZIONE KEY-LIST =================================== *
085700020212      *--------------------------------------------------------------*
085800020212     C     DEFKLIS       BEGSR
085900020212      *
086000020212      * AZORG01L
086100020212     C     K01ORG01      klist
086200020212     C                   kfld                    ORGfil
086300020212      *
086400020212      * AZNUM01L
086500020212     C     K05NUM01      klist
086600020212     C                   kfld                    NUMsoc
086700020212     C                   kfld                    NUMaaa
086800020212     C                   kfld                    NUMcnu
086900020212     C                   kfld                    NUMpo1
087000020212     C                   kfld                    NUMpo2
087100020212      *
087200020212      * AZNUM02L
087300020212     C     K05NUM02      klist
087400020212     C                   kfld                    NUMsoc
087500020212     C                   kfld                    NUMcnu
087600020212     C                   kfld                    NUMdac
087700020212     C                   kfld                    NUMpo1
087800020212     C                   kfld                    NUMpo2
087900020212      *
088000020212     C                   ENDSR
088100020212** MSG                                                                       *
088200020306TRUL33R - Numeratore non trovato in AZNUM00F                                   01
088300020306TRUL33R - Record del file AZNUM00F allocato                                    02
088400020306TRUL33R - Numeratore oltre il massimo del range previsto                       03
