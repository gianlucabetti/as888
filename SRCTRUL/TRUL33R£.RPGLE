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
004000020212
004100020212     H DECEDIT('0,') DATEDIT(*DMY.)
004200020212
004300020212     FAZORG01L  IF   E           K DISK
004400020212     FAZNUM01L  UF A E           K DISK    USROPN
004500020212     F                                     RENAME(AZNUM000:AZNUM01)
004600020212     FAZNUM02L  UF A E           K DISK    USROPN
004700020212     F                                     RENAME(AZNUM000:AZNUM02)
004800020212
004900020212      *
005000020212      * === SCHIERE ================================================ *
005100020304      *
005200020214      * Messaggi di errore
005300020212     D MSG             s             78    dim(20) ctdata perrcd(1)
005400020213      *
005500020214      * Tabella numeratori
005600020214     D $CNU            s                   like(NUMCNU)  dim(100) inz
005700020214     D $CNUD           s                   like(DCNU)    dim(100) inz
005800020212      *
005900020212      * === STRUTTURE DATI DI SISTEMA ============================== *
006000020212      *
006100020212     D STATUS         SDS           333
006200020212     D  DSPGM            *PROC
006300020212     D  PARMS            *PARMS
006400020212      *
006500020212      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
006600020212      *
006700020212      * Passaggio Parametri
006800020212     D KPJBA         e ds
006900020214     D TRUL33DS      e ds                  inz
007000020225     D  I33tla       e                     inz('L')
007100020723      *
007200020723      * Passaggio Parametri al pgm FNLV55R
007300020723     D FNLV55DS      e ds                  inz
007400020723     D  D55tpt       e                     inz('P')
007500020212      *
007600020212      * Passaggio Parametri al pgm TIBS02R
007700020212     D TIBS02DS      e ds                  inz
007800020212     D  T02mod       e                     inz('C')
007900020212     D  T02cod       e                     inz('CNU')
008000020212      *
008100020212      * Tabella CNU = Codici Numeratori di Gruppo
008200020212     D DCNU          e ds                  inz
008300020212      *
008400020212      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
008500020214      *
008600020214      * Ridefinizione campi del file AZORG
008700020214      * (per semplificarne la pulizia con un semplice CLEAR)
008800020214     D AZORGDS         ds                  inz
008900020214     D  ORGdit
009000020214     D  ORGfil
009100020214     D  ORGfva
009200020214     D  ORGfag
009300020214     D  ORGdes
009400020214     D  ORGfel
009500020214      *
009600020214      * Salvataggio dati file AZORG per P.O.1
009700020214     D AZORG1DS        ds                  inz
009800020214     D  PO1ORGdit                          like(ORGdit)
009900020214     D  PO1ORGfil                          like(ORGfil)
010000020214     D  PO1ORGfva                          like(ORGfva)
010100020214     D  PO1ORGfag                          like(ORGfag)
010200020214     D  PO1ORGdes                          like(ORGdes)
010300020214     D  PO1ORGfel                          like(ORGfel)
010400020214      *
010500020214      * Salvataggio dati file AZORG per P.O.2
010600020214     D AZORG2DS        ds                  inz
010700020214     D  PO2ORGdit                          like(ORGdit)
010800020214     D  PO2ORGfil                          like(ORGfil)
010900020214     D  PO2ORGfva                          like(ORGfva)
011000020214     D  PO2ORGfag                          like(ORGfag)
011100020214     D  PO2ORGdes                          like(ORGdes)
011200020214     D  PO2ORGfel                          like(ORGfel)
011300020212      *
011400020212      * DS per inversione data
011500020212     D WLBDAT          DS                  INZ
011600020212     D  G02DAT                 1      8  0
011700020212     D  G02INV                 9     16  0
011800020212     D  G02ERR                17     17
011900020212     D  G02TGI                18     22  0
012000020212      *
012100020212      * === VARIABILI ============================================== *
012200020212      *
012300020215     D SAVcnu          S                   like(NUMcnu)  inz(*loval)
012400020215     D SAVunp          S                   like(NUMunp)  inz(*loval)
012500020215     D wMAXnfi         S                   like(NUMunp)  inz(*hival)
012600020215     D PRIunp          S             30  0 inz
012700020215     D ULTunp          S             30  0 inz
012800020212     D wPO             S              3    inz(*zeros)
012900020212     D wDATE           S              8  0 inz
013000020213     D XX              S              3  0 inz
013100020212     D wVL             S              2    inz
013200020212     D $Write          S              1    inz(*off)
013300020212      *
013400020212      * === COSTANTI =============================================== *
013500020212      *
013600020212
013700020212      *--------------------------------------------------------------*
013800020212      * === MAIN LINE PROGRAM ====================================== *
013900020212      *--------------------------------------------------------------*
014000020212      *                                                              *
014100020212      * RIEPILOGO INDICATORI                                         *
014200020212      * --------------------                                         *
014300020213      *  10 - Comodo                                                 *
014400020212      *  21 - Record non trovato                                     *
014500020212      *  22 - Record allocato oppure già esistente in fase di write  *
014600020212      *                                                              *
014700020212      *--------------------------------------------------------------*
014800020212      *
014900020212     C     *entry        plist
015000020212     C                   parm                    KPJBA
015100020212      *
015200020212      * inizializzazione variabili
015300020212     C                   exsr      INZVAR
015400020212      *
015500020212sel 1C                   select
015600020212      *
015700020212      * Chiusura archivi
015800020213w   1C                   when      I33tla = 'C'
015900020212     C                   exsr      CLONUM
016000020212      *
016100020212      * Lettura ultimo progressivo usato + incremento + aggiornamento
016200020306w   1C                   when      I33ope = 0
016300020212     C                   exsr      CHANUM
016400020212     C                   exsr      INCNUM
016500020215if  2C                   if        ULTunp <> SAVunp
016600020215     C                   z-add     ULTunp        NUMunp
016700020212     C                   exsr      UPDNUM
016800020212e   2C                   endif
016900020212      *
017000020212      * Lettura ultimo progressivo usato
017100020306w   1C                   when      I33ope = 1
017200020212     C                   exsr      CHANUM
017300020305     C                   exsr      INCNUM
017400020212      *
017500020307      * Aggiornamento numero (ultimo prelevato) con quello ricevuto
017600020214w   1C                   when      I33ope = 2
017700020212     C                   exsr      CHANUM
017800020212     C                   eval      SAVunp = NUMunp
017900020215     C                   eval      PRIunp = I33unp
018000020215     C                   eval      ULTunp = I33unp
018100020212     C                   exsr      CTRNUM
018200020408     C                   eval      NUMunp = I33unp
018300020212     C                   exsr      UPDNUM
018400020212      *
018500020307      * Forzatura numero (ultimo prelevato) con quello ricevuto
018600020212      * (nessun controllo)
018700020214w   1C                   when      I33ope = 3
018800020212     C                   exsr      CHANUM
018900020213     C                   eval      NUMunp = I33unp
019000020306     C                   eval      PRIunp = I33unp
019100020306     C                   eval      ULTunp = I33unp
019200020212     C                   exsr      UPDNUM
019300020212      *
019400020212e   1C                   endsl
019500020212      *
019600020212      * Fine
019700020212     C                   EXSR      ENDPGM
019800020212
019900020212      *--------------------------------------------------------------*
020000020212      * === OPERAZIONI INIZIALI ==================================== *
020100020212      *--------------------------------------------------------------*
020200020212     C     *INZSR        BEGSR
020300020212      *
020400020212      * Data odierna in formato AAAAMMGG
020500020212     C                   clear                   WLBDAT
020600020212     C                   move      *date         G02dat
020700020212     C                   call      'XSRDA8'
020800020212     C                   parm                    WLBDAT
020900020212     C                   move      G02inv        wDATE
021000020212      *
021100020212     C                   ENDSR
021200020212
021300020212      *--------------------------------------------------------------*
021400020212      * === INIZIALIZZAZIONE VARIABILI ============================= *
021500020212      *--------------------------------------------------------------*
021600020212     C     INZVAR        BEGSR
021700020212      *
021800020212      * Reperimento parametri
021900020212if  1C                   if        KPJBU <> *blanks
022000020214     C                   movel     KPJBU         TRUL33DS
022100020212x   1C                   else
022200020225     C                   reset                   TRUL33DS
022300020212e   1C                   endif
022400020212      *
022500020212      * Impostazione dati di output
022600020215     C                   clear                   O33err
022700020213     C                   clear                   O33msg
022800020213     C                   clear                   O33soc
022900020213     C                   clear                   O33aaa
023000020213     C                   clear                   O33po1
023100020213     C                   clear                   O33po2
023200020213     C                   clear                   O33nri
023300020213     C                   clear                   O33nrf
023400020213     C                   clear                   O33inc
023500020213     C                   clear                   O33rin
023600020213     C                   clear                   O33rfi
023700020213     C                   clear                   O33dac
023800020213     C                   clear                   O33flo
023900020212     C                   reset                   SAVunp
024000020212     C                   reset                   PRIunp
024100020215     C                   reset                   ULTunp
024200020212     C                   reset                   $Write
024300020212      *
024400020212      * Se chiesta solo la chiusura degli archivi: basta così !
024500020213     C     I33tla        cabeq     'C'           EndInzVar
024600020306      *
024700020306      * Controllo parametri ricevuti
024800020306     C                   exsr      srCtrParm
024900020212      *
025000020214      * Reperimento dati tabella 'CNU'
025100020213if  1C                   if        SAVcnu <> I33cnu
025200020214     C                   exsr      srRepTbCNU
025300020214     C                   eval      SAVcnu =  I33cnu
025400020212e   1C                   endif
025500020212      *
025600020212      * Verifica della vista logica da elaborare
025700020212sel 1C                   select
025800020212w   1C                   when      §CNUfaa = 'S'
025900020212     C                   eval      wVL     = '01'
026000020212w   1C                   when      §CNUkda = 'S'
026100020212     C                   eval      wVL     = '02'
026200020212x   1C                   other
026300020212     C                   eval      wVL     = '01'
026400020212e   1C                   endsl
026500020212      *
026600020212      * Apertura archivio da elaborare
026700020212sel 1C                   select
026800020212w   1C                   when          wVL     = '01'
026900020306     C                             and not %open(AZNUM01L)
027000020212     C                   open      AZNUM01L
027100020212w   1C                   when          wVL     = '02'
027200020306     C                             and not %open(AZNUM02L)
027300020212     C                   open      AZNUM02L
027400020212e   1C                   endsl
027500020212      *
027600020212      * Preparazione klist secondo tabella "CNU"
027700020212      * - codice società
027800020213     C                   eval      NUMsoc  = I33soc
027900020212      * - anno
028000020306      *   (impostato automaticamente se non ricevuto come parametro)
028100020212if  1C                   if        §CNUfaa = 'S'
028200020213if  2C                   if        I33aaa  = *zeros
028300020212     C                   eval      NUMaaa  = *year
028400020212x   2C                   else
028500020213     C                   eval      NUMaaa  = I33aaa
028600020212e   2C                   endif
028700020212x   1C                   else
028800020212     C                   clear                   NUMaaa
028900020212e   1C                   endif
029000020212      * - codice numeratore
029100020213     C                   eval      NUMcnu  = I33cnu
029200020212      * - primo punto operativo
029300020306      *   (non prevista l'impostazione automatica)
029400020306sel 1C***                select
029500020306w   1C***                when      §CNUpo1 = *blanks
029600020306     C***                clear                   NUMpo1
029700020306w   1C***                when      §CNUpo1 > *zeros
029800020306     C***                move      §CNUpo1       NUMpo1
029900020306x   1C***                other
030000020213     C                   eval      NUMpo1  = I33po1
030100020306e   1C***                endsl
030200020212      * - secondo punto operativo
030300020306      *   (non prevista l'impostazione automatica)
030400020306sel 1C***                select
030500020306w   1C***                when      §CNUpo2 = *blanks
030600020306     C***                clear                   NUMpo2
030700020306w   1C***                when      §CNUpo2 > *zeros
030800020306     C***                move      §CNUpo2       NUMpo2
030900020306x   1C***                other
031000020213     C                   eval      NUMpo2  = I33po2
031100020306e   1C***                endsl
031200020212      * - data in chiave
031300020306      *   (impostata automaticamente se non ricevuta come parametro)
031400020306if  1C                   if        §CNUkda = 'S'
031500020306if  2C                   if        I33dac  = *zeros
031600020306     C                   eval      NUMdac  = wDATE
031700020306x   2C                   else
031800020213     C                   eval      NUMdac  = I33dac
031900020306e   2C                   endif
032000020306x   1C                   else
032100020306     C                   clear                   NUMdac
032200020306e   1C                   endif
032300020212      *
032400020212     C     EndInzVar     ENDSR
032500020212
032600020212      *--------------------------------------------------------------*
032700020212      * === CONTROLLO PARAMETRI RICEVUTI =========================== *
032800020212      *--------------------------------------------------------------*
032900020212     C     srCtrParm     BEGSR
033000020212      *
033100020212sel 1C                   select
033200020213      * - tipo di lancio non valido
033300020306w   1C                   when           I33tla <> *blank
033400020306     C                              and I33tla <> 'C'
033500020306     C                              and I33tla <> 'L'
033600020306     C                   reset                   SAVcnu
033700020306     C                   reset                   DCNU
033800020306     C                   exsr      ENDPGM
033900020212      * - operazione richiesta non valida
034000020213w   1C                   when           I33ope <  0
034100020213     C                              or  I33ope >  3
034200020306     C                   reset                   SAVcnu
034300020306     C                   reset                   DCNU
034400020306     C                   exsr      ENDPGM
034500020212      * - anno obbligatorio
034600020306      *   => impostato automaticamente se non ricevuto e richiesto
034700020213      * - data creazione obbligatoria
034800020306      *   => impostata automaticamente se non ricevuta e richiesta
034900020212e   1C                   endsl
035000020212      *
035100020213cas 1C     O33err        casne     *zeros        ENDPGM
035200020212e   1C                   endcs
035300020212      *
035400020212     C                   ENDSR
035500020213
035600020214      *--------------------------------------------------------------*
035700020214      * === REPERIMENTO DATI TAB. CNU ============================== *
035800020214      *--------------------------------------------------------------*
035900020214     C     srRepTbCNU    BEGSR
036000020306      *
036100020306     C                   clear                   DCNU
036200020214      *
036300020214      * Controllo se già memorizzato in schiera
036400020214     C                   eval      *in10     = *off
036500020214     C                   eval      XX        = 1
036600020214     C     I33cnu        lookup    $CNU(xx)                               10
036700020214      *
036800020214if  1C                   if        *in10
036900020306     C                             and   $CNUD(xx) <> *blanks
037000020214      * - già memorizzato -> reperisco i dati
037100020214      *                      e segnalo errore se codice errato
037200020214     C                   movel     $CNUD(xx)     DCNU
037300020214      *
037400020214x   1C                   else
037500020214      * - non ancora memorizzato -> memorizzo codice e dati tabella
037600020214      *                             e segnalo errore se codice errato
037700020214     C                   reset                   TIBS02DS
037800020214     C                   movel     KNSIF         T02sif
037900020214     C                   movel     I33cnu        T02ke1
038000020214     C                   call      'TIBS02R'
038100020214     C                   parm                    KPJBA
038200020214     C                   parm                    TIBS02DS
038300020214if  2C                   if        T02err    = *blanks
038400020214     C                   movel     T02uni        DCNU
038500020214     C     *zeros        lookup    $CNU(xx)                               10
038600020214if  3C                   if        *in10
038700020214     C                   eval      $CNU(xx)  = I33cnu
038800020214     C                   movel     DCNU          $CNUD(xx)
038900020214e   3C                   endif
039000020214x   2C                   else
039100020214     C                   eval      O33err =  01
039200020214     C                   exsr      ENDPGM
039300020214e   2C                   endif
039400020214      *
039500020214e   1C                   endif
039600020214      *
039700020214     C                   ENDSR
039800020214
039900020214      *--------------------------------------------------------------*
040000020214      * === CONTROLLO P.O. PER CREAZIONE =========================== *
040100020214      *--------------------------------------------------------------*
040200020214     C     srCtrPO       BEGSR
040300020214      *
040400020214      * Controllo P.O.1
040500020214if  1C                   if        I33po1  =  *zeros
040600020214     C                   clear                   AZORG1DS
040700020214e   1C                   endif
040800020214if  1C                   if        I33po1  <> PO1ORGfil
040900020214if  2C                   if        I33po1  <> ORGfil
041000020214     C                   clear                   AZORG1DS
041100020214     C                   clear                   AZORGDS
041200020214     C                   z-add     I33po1        ORGfil
041300020214     C     K01ORG01      chain     AZORG01L
041400020214if  3C                   if        not %found(AZORG01L)
041500020214     C                             and §CNUp1n <> 'S'
041600020214     C                   move      *loval        PO1ORGfil
041700020306     C                   eval      O33err  =  01
041800020214     C                   exsr      ENDPGM
041900020214e   3C                   endif
042000020214e   2C                   endif
042100020214     C                   movel     AZORGDS       AZORG1DS
042200020214e   1C                   endif
042300020214if  1C                   if            §CNUp1n    <> 'S'
042400020304     C                             and §CNUpo1    <> *blanks
042500020214     C                             and (PO1ORGfag <> 'A'
042600020214     C                             and  PO1ORGfag <> 'F'
042700020214     C                              or  PO1ORGfva <> *blanks)
042800020306     C                   eval      O33err  =  01
042900020214     C                   exsr      ENDPGM
043000020214e   1C                   endif
043100020214      *
043200020214      * Controllo P.O.2
043300020214if  1C                   if        I33po2  =  *zeros
043400020214     C                   clear                   AZORG2DS
043500020214e   1C                   endif
043600020214if  1C                   if        I33po2  <> PO2ORGfil
043700020214if  2C                   if        I33po2  <> ORGfil
043800020214     C                   clear                   AZORG2DS
043900020214     C                   clear                   AZORGDS
044000020214     C                   z-add     I33po2        ORGfil
044100020214     C     K01ORG01      chain     AZORG01L
044200020214if  3C                   if        not %found(AZORG01L)
044300020214     C                             and §CNUp2n <> 'S'
044400020214     C                   move      *loval        PO2ORGfil
044500020306     C                   eval      O33err  =  01
044600020214     C                   exsr      ENDPGM
044700020214e   3C                   endif
044800020214e   2C                   endif
044900020214     C                   movel     AZORGDS       AZORG2DS
045000020214e   1C                   endif
045100020214if  1C                   if            §CNUp2n    <> 'S'
045200020304     C                             and §CNUpo2    <> *blanks
045300020214     C                             and (PO2ORGfag <> 'A'
045400020214     C                             and  PO2ORGfag <> 'F'
045500020214     C                              or  PO2ORGfva <> *blanks)
045600020306     C                   eval      O33err  =  01
045700020214     C                   exsr      ENDPGM
045800020214e   1C                   endif
045900020723      *
046000020723      * reperisco il 1° livello del p.o.1
046100050708      * (se NON ammessi P.O. non validi   &
046200050708      *  se richiesto P.O.)
046300050708if  1***C                   if        I33po1 <> D55lin
046400050708if  1C                   if            §CNUp1n    <> 'S'
046500050708     C                             and §CNUpo1    <> *blanks
046600050708     C                             and I33po1     <> D55lin
046700020723     C                   reset                   FNLV55DS
046800020723     C                   eval      D55lin  = I33po1
046900020723     C                   eval      D55drf  = wDATE
047000020723     C                   call      'FNLV55R'
047100020723     C                   parm                    FNLV55DS
047200020723e   1C                   endif
047300020723if  1C                   if        D55err <> *blanks
047400020723     C                   eval      O33err  =  01
047500020723     C                   exsr      ENDPGM
047600020723e   1C                   endif
047700020214      *
047800020214      * altri controlli sui P.O.
047900020214sel 1C                   select
048000050708      * - ammesso p.o.1 non valido   o   previsto nessun p.o.1
048100050708w   1C                   when          §CNUp1n    =  'S'
048200050708     C                             or  §CNUpo1    =  *blanks
048300020214      * - p.o.1 non del livello richiesto
048400020214w   1C                   when           I33po1 <> *zeros
048500020723     C                             and (I33po1 <> D55tfp
048600020214     C                             and  §CNUfnu = 'S')
048700020723     C                             or  (I33po1  = D55tfp
048800020214     C                             and  §CNUfnu = 'L')
048900020306     C                   eval      O33err  =  01
049000050708e   1C                   endsl
049100050708sel 1C***                select
049200050708      * - ammesso p.o.2 non valido   o   previsto nessun p.o.2
049300050708w   1C***                when          §CNUp2n    =  'S'
049400050708     C***                          or  §CNUpo2    =  *blanks
049500020214      * - p.o.2 non del livello richiesto
049600020214w   1C***                when           I33po2 <> *zeros
049700020214     C***                          and (I33po2 <> PO2ORGfel
049800020214     C***                          and  §CNUfnu = 'S')
049900020214     C***                          or  (I33po2  = PO2ORGfel
050000020214     C***                          and  §CNUfnu = 'L')
050100020306     C***                eval      O33err  =  01
050200050708e   1C***                endsl
050300020214      *
050400020214cas 1C     O33err        casne     *zeros        ENDPGM
050500020214e   1C                   endcs
050600020214      *
050700020214      * p.o.1 non previsto per questo numeratore
050800020304if  1C                   if            §CNUpo1 <> 'TUT'
050900020304     C                             and §CNUpo1 <> 'FIL'
051000020214     C                   move      I33po1        wPO
051100020305if  2C                   if             §CNUpo1 <> wPO
051200020305     C                             and (§CNUpo1 <> *blanks
051300020305     C                              or  wPO     <> *zeros)
051400020306     C                   eval      O33err  =  01
051500020214     C                   exsr      ENDPGM
051600020214e   2C                   endif
051700020214e   1C                   endif
051800020305      *
051900020214      * p.o.2 non previsto per questo numeratore
052000020304if  1C                   if            §CNUpo2 <> 'TUT'
052100020304     C                             and §CNUpo2 <> 'FIL'
052200020214     C                   move      I33po2        wPO
052300020305if  2C                   if             §CNUpo2 <> wPO
052400020305     C                             and (§CNUpo2 <> *blanks
052500020305     C                              or  wPO     <> *zeros)
052600020306     C                   eval      O33err  =  01
052700020214     C                   exsr      ENDPGM
052800020214e   2C                   endif
052900020214e   1C                   endif
053000020214      *
053100020214     C                   ENDSR
053200020212
053300020212      *--------------------------------------------------------------*
053400020212      * === AGGANCIO FILE AZNUM ==================================== *
053500020212      *--------------------------------------------------------------*
053600020212     C     CHANUM        BEGSR
053700020212      *
053800020212      * se richiesta solo lettura, non alloco
053900020213if  1C                   if        I33ope  =  1
054000020212      *
054100020212sel 2C                   select
054200020212w   2C                   when      wVL = '01'
054300020212     C     K05NUM01      chain(n)  AZNUM01                            2122
054400020212w   2C                   when      wVL = '02'
054500020212     C     K05NUM02      chain(n)  AZNUM02                            2122
054600020212e   2C                   endsl
054700020212      *
054800020212      * se richiesto anche aggiornamento, alloco
054900020212x   1C                   else
055000020212      *
055100020212sel 2C                   select
055200020212w   2C                   when      wVL = '01'
055300020212     C     K05NUM01      chain     AZNUM01                            2122
055400020212w   2C                   when      wVL = '02'
055500020212     C     K05NUM02      chain     AZNUM02                            2122
055600020212e   2C                   endsl
055700020212      *
055800020212e   1C                   endif
055900020212      *
056000020212     C                   movel     *in21         $Write
056100020212      *
056200020212      * Gestione RCD non trovato:
056300020214if  1C                   if        $Write  =  *on
056400020214      *
056500020213if  2C                   if        I33ope  <> 1
056600020212      *  se è richiesta la creazione se inesistente,
056700020306      *  ma il progressivo non è creabile,
056800020212      *  ritorno al pgm chiamante avvertendolo
056900020308if  3C                   if        §CNUsca <> 'S'
057000020306     C                   eval      O33err  =  01
057100020212     C                   exsr      ENDPGM
057200020215e   3C                   endif
057300020215      *  ripulisco i dati del numeratore per crearlo
057400020212     C                   clear                   NUMunp
057500020212     C                   clear                   NUMrin
057600020212     C                   clear                   NUMrfi
057700020212     C                   clear                   NUMflo
057800020214      *  controllo P.O.1 e P.O.2 se richiesta la creazione
057900020214     C                   exsr      srCtrPO
058000020212x   2C                   else
058100020215      *  se è richiesto solo il reperimento del numeratore,
058200020212      *  ritorno al pgm chiamante avvertendolo
058300020306     C                   eval      O33err  =  01
058400020212     C                   exsr      ENDPGM
058500020212e   2C                   endif
058600020214      *
058700020212e   1C                   endif
058800020212      *
058900020212      * Gestione RCD allocato:
059000020212      *  se il rcd da gestire è allocato da un altro lavoro,
059100020212      *  ritorno al pgm chiamante avvertendolo
059200020212if  1C                   if        *in22
059300020306     C                   eval      O33err  =  02
059400020212     C                   exsr      ENDPGM
059500020212e   1C                   endif
059600020212      *
059700020212     C                   ENDSR
059800020212
059900020212      *--------------------------------------------------------------*
060000020212      * === INCREMENTO PROGRESSIVO ================================= *
060100020212      *--------------------------------------------------------------*
060200020212     C     INCNUM        BEGSR
060300020212      *
060400020215     C                   eval      SAVunp =  NUMunp
060500020212      *
060600020215if  1C                   if        I33num <> 0
060700020212      *
060800020408if  2C                   if        I33num >= 0
060900020304     C                   eval      PRIunp =  NUMunp + §CNUinc
061000020408x   2C                   else
061100020304     C                   eval      PRIunp =  NUMunp - §CNUinc
061200020408e   2C                   endif
061300020215     C                   eval      ULTunp =  NUMunp + §CNUinc * I33num
061400020212      *
061500020212     C                   exsr      CTRNUM
061600020305      *
061700020305x   1C                   else
061800020305      *
061900020305     C                   eval      PRIunp =  NUMunp
062000020305     C                   eval      ULTunp =  NUMunp
062100020212      *
062200020212e   1C                   endif
062300020212      *
062400020212     C                   ENDSR
062500020212
062600020212      *--------------------------------------------------------------*
062700020212      * === CONTROLLO PROGRESSIVO ================================== *
062800020212      *--------------------------------------------------------------*
062900020212     C     CTRNUM        BEGSR
063000020212      *
063100020306      * se superato il valore finale del range di validità
063200020212      * se previsto il controllo con il numero esistente e
063300020212      *    nuovo valore inferiore al precedente
063400020212      * se bypassato il massimo consentito dalla lunghezza del campo
063500020306      *    (ovvero: se ultimo numero:    999999999999999
063600020306      *             e  incremento   :    1
063700020306      *             => risultato... : (1)000000000000000 - NO BUONO !!!
063800020306      *                sistemazione :    000000000000001 - OK)
063900020304if  1C                   if        NUMrin = 0
064000020212     C                   eval      NUMrin = §CNUrin
064100020212e   1C                   endif
064200020304if  1C                   if        NUMrfi = 0
064300020212     C                   eval      NUMrfi = §CNUrfi
064400020212e   1C                   endif
064500020215      *
064600020215      * controllo possibili casi "indesiderati"
064700020215      * - questa IF iniziale può velocizzare l'elaborazione se non
064800020215      *   risulta verificato nessun caso tra quelli previsti nelle
064900020215      *   varie WHEN - che non vengono, quindi, testate); nel caso
065000020215      *   in cui, invece, risulta verificato uno dei casi previsti,
065100020215      *   l'elaborazione si rallenta del tempo necessario all'ese-
065200020215      *   cuzione di questo confronto iniziale.
065300020215      *   Ma, tutto sommato, non si dovrebbe verificare spesso...
065400020304if  1C                   if             (PRIunp  >  NUMrfi
065500020304     C                              or   PRIunp  <  NUMrin
065600020304     C                              or   ULTunp  >  NUMrfi
065700020304     C                              or   ULTunp  <  NUMrin)
065800020215     C                             or   (§CNUctn =  'S'
065900020215     C                              and (PRIunp  <  SAVunp
066000020215     C                               or  ULTunp  <  SAVunp))
066100020212      *
066200020212      * reimposto il progressivo
066300020212sel 2C                   select
066400020215      * - ultimo numero prelevato da forzare errato
066500020215w   2C                   when           I33ope  =  2
066600020215     C                             and (I33unp  <  NUMrin
066700020215     C                              or  I33unp  >  NUMrfi)
066800020306     C                   eval      O33err  =  03
066900020215     C                   exsr      ENDPGM
067000020305      * - previsto il blocco dell'incremento se oltre il massimo previsto
067100020304w   2C                   when           §CNUbio =  'S'
067200020213     C                             and (PRIunp  >  NUMrfi
067300020215     C                              or  ULTunp  >  NUMrfi)
067400020306     C                   eval      O33err  =  03
067500020212     C                   exsr      ENDPGM
067600020305      * - previsto controllo con progressivo esistente
067700020213w   2C                   when           §CNUctn =  'S'
067800020213     C                             and (PRIunp  <  SAVunp
067900020215     C                              or  ULTunp  <  SAVunp)
068000020213     C                   eval      PRIunp  =  SAVunp
068100020215     C                   eval      ULTunp  =  SAVunp
068200020306      * - progressivo calcolato non compreso nel range
068300020304w   2C                   when          (PRIunp >  NUMrfi
068400020213     C                              or  PRIunp <  NUMrin
068500020215     C                              or  ULTunp >  NUMrfi
068600020215     C                              or  ULTunp <  NUMrin)
068700020215if  3C                   if        I33num  >= 0
068800020305     C                   eval      PRIunp  =  NUMrin
068900020305     C                   eval      ULTunp  =  NUMrin + §CNUinc * (I33num - 1)
069000020215if  4C                   if        ULTunp  >  NUMrfi
069100020306     C                   eval      O33err  =  03
069200020212     C                   exsr      ENDPGM
069300020215e   4C                   endif
069400020215x   3C                   else
069500020305     C                   eval      PRIunp  =  NUMrfi
069600020305     C                   eval      ULTunp  =  NUMrfi + §CNUinc * (I33num + 1)
069700020215if  4C                   if        ULTunp  <  NUMrin
069800020306     C                   eval      O33err  =  03
069900020215     C                   exsr      ENDPGM
070000020215e   4C                   endif
070100020215e   3C                   endif
070200020215      *
070300020212e   2C                   endsl
070400020212      *
070500020215e   1C                   endif
070600020212      *
070700020212     C                   ENDSR
070800020212
070900020212      *--------------------------------------------------------------*
071000020212      * === AGGIORNAMENTO/SCRITTURA RECORD ========================= *
071100020212      *--------------------------------------------------------------*
071200020212     C     UPDNUM        BEGSR
071300020212      *
071400020212if  1C                   if        $Write = *on
071500020212      *
071600020212     C                   exsr      RIENUM
071700020212sel 2C                   select
071800020212w   2C                   when      wVL = '01'
071900020306     C                   write     AZNUM01
072000020212w   2C                   when      wVL = '02'
072100020306     C                   write     AZNUM02
072200020212e   2C                   endsl
072300020212      *
072400020212x   1C                   else
072500020212      *
072600020212sel 2C                   select
072700020212w   2C                   when      wVL = '01'
072800020212     C                   update    AZNUM01
072900020212w   2C                   when      wVL = '02'
073000020212     C                   update    AZNUM02
073100020212e   2C                   endsl
073200020212      *
073300020212e   1C                   endif
073400020212      *
073500020212     C                   ENDSR
073600020212
073700020212      *--------------------------------------------------------------*
073800020212      * === IMPOSTAZIONE ALTRI DATI ================================ *
073900020212      *--------------------------------------------------------------*
074000020212     C     RIENUM        BEGSR
074100020212      *
074200020212      * Codice Società
074300020213     C                   eval      NUMsoc  = I33soc
074400020212      * Anno
074500020212if  1C                   if        §CNUfaa = 'S'
074600020213if  2C                   if        I33aaa  = *zeros
074700020212     C                   eval      NUMaaa  = *year
074800020212x   2C                   else
074900020213     C                   eval      NUMaaa  = I33aaa
075000020212e   2C                   endif
075100020212x   1C                   else
075200020212     C                   clear                   NUMaaa
075300020212e   1C                   endif
075400020212      * Codice Numeratore
075500020213     C                   eval      NUMcnu  = I33cnu
075600020212      * Primo Punto Operativo
075700020304if  1C                   if        §CNUpo1 = *blanks
075800020304     C                   clear                   NUMpo1
075900020304x   1C                   else
076000020213     C                   eval      NUMpo1  = I33po1
076100020304e   1C                   endif
076200020212      * Secondo Punto Operativo
076300020304if  1C                   if        §CNUpo2 = *blanks
076400020304     C                   clear                   NUMpo2
076500020304x   1C                   else
076600020213     C                   eval      NUMpo2  = I33po2
076700020304e   1C                   endif
076800020212      * Ultimo Numero prelevato
076900020213if  1C                   if        I33ope  = 2   or
077000020213     C                             I33ope  = 3
077100020213     C                   eval      NUMunp  = I33unp
077200020215x   1C                   else
077300020215     C                   eval      NUMunp  = ULTunp
077400020212e   1C                   endif
077500020212      * Range (inizio & fine)
077600020212     C                   eval      NUMrin  = §CNUrin
077700020212     C                   eval      NUMrfi  = §CNUrfi
077800020212      * Data Creazione
077900020307if  1C                   if        I33dac  = *zeros
078000020307     C                   eval      NUMdac  = wDATE
078100020307x   1C                   else
078200020307     C                   eval      NUMdac  = I33dac
078300020307e   1C                   endif
078400020212      * Flag Operativi
078500020212     C                   eval      NUMflo  = *blanks
078600020212      *
078700020212     C                   ENDSR
078800020212
078900020212      *--------------------------------------------------------------*
079000020212      * === CHIUSURA ARCHIVI ======================================= *
079100020212      *--------------------------------------------------------------*
079200020212     C     CLONUM        BEGSR
079300020212      *
079400020212     C                   clear                   TIBS02DS
079500020212     C                   movel     'C'           T02tla
079600020212     C                   movel     KNSIF         T02sif
079700020212     C                   call      'TIBS02R'
079800020212     C                   parm                    KPJBA
079900020212     C                   parm                    TIBS02DS
080000020212      *
080100020306if  1C                   if        %open(AZNUM01L)
080200020212     C                   close     AZNUM01L
080300020212e   1C                   endif
080400020306if  1C                   if        %open(AZNUM02L)
080500020212     C                   close     AZNUM02L
080600020212e   1C                   endif
080700020212      *
080800020212     C                   ENDSR
080900020212
081000020212      *--------------------------------------------------------------*
081100020212      * === FINE PROGRAMMA ========================================= *
081200020212      *--------------------------------------------------------------*
081300020212     C     ENDPGM        BEGSR
081400020212      *
081500020212      * restituzione dei dati reperiti e calcolati
081600020212sel 1C                   select
081700020212      *
081800020213w   1C                   when          O33err =  *zeros
081900020213     C                             and I33tla <> 'C'
082000020213     C                   eval      O33soc = NUMsoc
082100020213     C                   eval      O33aaa = NUMaaa
082200020213     C                   eval      O33po1 = NUMpo1
082300020213     C                   eval      O33po2 = NUMpo2
082400020213     C                   eval      O33nri = PRIunp
082500020215     C                   eval      O33nrf = ULTunp
082600020213     C                   eval      O33inc = §CNUinc
082700020213     C                   eval      O33rin = NUMrin
082800020213     C                   eval      O33rfi = NUMrfi
082900020213     C                   eval      O33dac = NUMdac
083000020213     C                   eval      O33flo = NUMflo
083100020212      *
083200020213w   1C                   when      O33err > 0
083300020213     C                   eval      O33msg = MSG(O33err)
083400020212      *
083500020212e   1C                   endsl
083600020212      *
083700020214     C                   movel(p)  TRUL33DS      KPJBU
083800020212      *
083900020212      * disallocazione record (se allocato per aggiornamento)
084000020214if  1C                   if        I33ope <> 1
084100020212     C                   unlock    AZNUM01L                             22
084200020212e   1C                   endif
084300020212      *
084400020214if  1C                   if        I33tla =  *blanks
084500020212     C                   movel     *on           *inRT
084600020212x   1C                   else
084700020212     C                   movel     *on           *inLR
084800020212e   1C                   endif
084900020212     C                   return
085000020212      *
085100020212     C                   ENDSR
085200020212
085300020212      *--------------------------------------------------------------*
085400020212      * === DEFINIZIONE KEY-LIST =================================== *
085500020212      *--------------------------------------------------------------*
085600020212     C     DEFKLIS       BEGSR
085700020212      *
085800020212      * AZORG01L
085900020212     C     K01ORG01      klist
086000020212     C                   kfld                    ORGfil
086100020212      *
086200020212      * AZNUM01L
086300020212     C     K05NUM01      klist
086400020212     C                   kfld                    NUMsoc
086500020212     C                   kfld                    NUMaaa
086600020212     C                   kfld                    NUMcnu
086700020212     C                   kfld                    NUMpo1
086800020212     C                   kfld                    NUMpo2
086900020212      *
087000020212      * AZNUM02L
087100020212     C     K05NUM02      klist
087200020212     C                   kfld                    NUMsoc
087300020212     C                   kfld                    NUMcnu
087400020212     C                   kfld                    NUMdac
087500020212     C                   kfld                    NUMpo1
087600020212     C                   kfld                    NUMpo2
087700020212      *
087800020212     C                   ENDSR
087900020212** MSG                                                                       *
088000020306TRUL33R - Numeratore non trovato in AZNUM00F                                   01
088100020306TRUL33R - Record del file AZNUM00F allocato                                    02
088200020306TRUL33R - Numeratore oltre il massimo del range previsto                       03
