000100950920     H*BIND
000200950601     H*------------------------------------------------------------
000300950601    >H*  Procedura: AggiornaIVA
000400950601     H*                                                  Reffld/
000500950601     H*  Campo       Descrizione                         Lungh Tipo  I/O Opz
000600950601    >H*  <-------->  <-------------------------------->  <-------->  <->  X
000700950601     H*
000800950601     H*$ Operazione  Operazione                              2  A     I
000900950601     H*$             '01' = Controllo
001000950601     H*$             '02' = Controllo e aggiorna
001100950601     H*$             '03' = Controllo e aggiorna anche
001200950601     H*$                    se ci sono errori
001300950601     H*$             '04' = Legge record,lo controlla
001400950601     H*$                    e lo aggiorna se non vi
001500950601     H*$                    sono errori
001600950601     H*$             '05' = Annulla record
001700971205     H*$             '06' = Come '02' ma con forzatura
001800971205     H*$                    per data liquidazione IVA
001900950601     H*$ LenInp      Lunghezza in input                      9  B     I
002000950601     H*$ Input       Contiene dati in input               4000  A    I/O
002100950601     H*$ NrRigaM     Numero riga movimento               IvaNrRigaM  I/O
002200950601     H*$ NrRiga      Numero riga                         IvaNrRiga   I/O
002300950601     H*$ Gesmsg      '0' Non invia messaggi di errore        1  A     I
002400950601     H*$             '1' Invia messaggi di errore
002500950601     H*$ Esito       '0' OK                                  1  A     O
002600950601     H*$             '1' errori
002700950601     H*$ StrutturaO  Nome DS in cui passare i dati;         10  A     I  S
002800950601     H*$             accetta i seguenti valori :
002900950601     H*$             ND010DS
003000950601     H*$ LenOut      Lunghezza max dei dati di ritorno       9  B     I  S
003100950601     H*$ Output      Conterrà i dati di ritorno           4000  A     O  S
003200010112D0808H*$ Fun8Mvc099  Impostazione bottone Fun(8) di NDMVC099 1  A     I  S
003300950601     F*/EJECT
003400950601     F*------------------------------------------------------------
003500950601     F* Definizione file
003600950601     F*------------------------------------------------------------
003700950601     F*------------
003800950601     FANDLI01L  IF   E           K DISK
003900950601     FANALR02L  IF   E           K DISK
004000020321C1550FANSLI01L  IF   E           K DISK    usropn
004100080213     FNDIVA01L  iF   E           K DISK    rename(ndiva000:ndiva01)
004200950807     FNDIVA00F  UF A E           K DISK    commit
004300080213C2072F*------------
004400080213  "  F* Key: soc/associazione/causale
004500080213C2072FNDELT02L  IF   E           K DISK
004600980922     D*------------------------------------------------------------
004700980922     D* Prototipazione procedure EURO
004800981103     D/COPY *LIBL/SRCCPY,PJX002PR
004900950601     D*------------------------------------------------------------
005000950601     D* Definizioni dati
005100950601     D*------------------------------------------------------------
005200951024     D* BOTTONE IVA NDMVC299
005300951024     D ND024DS       E DS
005400980511     D*-------------
005500950601     D* ------ Definizioni parametri di ingresso
005600950601    >D Operazione      S              2A
005700950601    >D LenInp          S              9B 0
005800950601    >D Input           S           4000A
005900950601    >D NrRigaM         S                    like(IvaNrRigaM)
006000950601    >D NrRiga          S                    like(IvaNrRiga)
006100950601     D GesMsg          S              1
006200950601     D Esito           S              1
006300951004     D Refresh         S              1
006400950601     D StrutturaO      S             10
006500950601     D LenOut          S              9B 0
006600950601     D Output          S           4000
006700010112D0808D Fun8Mvc099      S              1A
006800950601     D*
006900950601     D* ------ Definizioni varie
007000950601    >D PROMSG          C                    Const('PROMSG    *LIBL     ')
007100950601     D I               S              3  0
007200950725     D Ctr             S              1A    INZ(*OFF)
007300980609     D TotImpo         S                    Like(IVAImponib)
007400980609     D TotImpD         S                    Like(IVAImponD )
007500980609     D TotIVA          S                    Like(IVAImporto)
007600980609     D TotIVAD         S                    Like(IVAImportD)
007700950601     D Lock            S              1
007800950601     D Fun             S              1     dim(100)
007900950601     D $Iva            S              1
008000950803     D Codoper         S              1
008100980520     D ChgCausale      S              1  0 inz(0)
008200010112D0808D WFun8099        S              1A
008300080213C2072D $NotaVar        S              1    inz(*Off)
008400950601     D* variabili di salvataggio ultimo stato registrazione
008500950601     D*
008600950612     D $NrAsReg        S                    like(NrAsReg003)
008700950601     D $NrRigaM        S                    like(IvaNrRigaM)
008800950601     D $NrRiga         S                    like(IvaNrRiga)
008900080327D2368D $UltNrRiga      S                    like(IvaNrRiga)
009000950601     D*
009100951009     D* ------ Definizioni parametri standard
009200950601     D $Esito          S              1
009300950601     D $Gesmsg         S              1
009400950601     D $Struttura      S             10
009500950601     D $LenOut         S              9B 0
009600950927     D $LenIn          S              9B 0
009700950601     D EsiCal          S              1A   INZ(*OFF)
009800981009     D $NoCtrlSco      S              1
009900971209     D*-------------
010000971209     D* Plist per  XDATR
010100971209     D XDTRIC          S              1
010200971209     D XDTSOC          S              3
010300971209     D XDTUNI          S              8
010400971209     D XDTCTB          S              2
010500971209     D XDTKEY          S              8
010600971209     D XDTALC          S              1
010700971209     D XDTCMT          S              1
010800971209     D XDTDAT          S               D
010900971209     D XDTTIP          S              6
011000971209     D XDTERR          S              1
011100971211     D XDTTST          S              1
011200971211     D*-------------
011300971211     D* Controlli validità legende
011400971211     D X07VALDS      E DS                  INZ
011500950601     D* ------ Strutture dati di output da driver
011600950804    >D ND003DS       E DS                                    inz
011700950804    >D ND006DS       E DS                                    inz
011800950804    >D ND009DS       E DS                                    inz
011900950804    >D ND010DS       E DS                                    inz
012000950804    >D ANOPEDS       E DS                  ExtName(ANOPE00F) inz
012100950804    >D NDMOVDS       E DS                  ExtName(NDMOV00F) inz
012200950601     D* ------ Campi esterni
012300950920    >D £RigaM          S                   like(IvaNrRigaM)   Import
012400950920    >D £RigaMInd       S                   like(IvaNrRigaM)   Import
012500950920    >D £ANOPETES       S                   like(ANOPEDS)   Import
012600950920    >D £NDREG        E DS                  extName(NDREG00F)  import
012700950818     D* nr.decimali previsti per la moneta di conto
012800950928     D £NrDec          s              1P 0 import
012900950928     D £NrDecDiv       s              1P 0 import
013000980925     D £DtRfCam        s               D   import
013100960621     D  £XSCDIV        s                   Like(Regdivisa) import
013200020321C1550D £XSoc001DS    E DS                  extname(XSOC001DS)  import
013300020321C1550D   £RegimeIva                   1    overlay(XscFgo:9)
013400980511     D*
013500980922     D WImposta        s             30S15
013600980515    >D WANOPE          S                   like(ANOPEDS)
013700950601     D*
013800950601     D* ------ Definizioni per Gestione messaggi
013900950601     D LenDta          S              9B 0
014000950601     D CallStack       S              9B 0
014100950601     D ErrCod          DS
014200950601     D  ErrLen                        9B 0 INZ(16)
014300950601     D  LenRet                        9B 0 INZ(16)
014400950601     D  ErrId                         7
014500950601     D ErrMsg          S              7A   INZ(*blank)
014600950601     D DtaMsg          S            256A   INZ(*blank)
014700980529     D*
014800980529     D STATUS         SDS
014900980529     D  PARMS            *PARMS
015000020321C1550 *-------------
015100020322  "   * mese della data annotazione/liquidazione da far slittare
015200020322  "  D $MeseOrig       S              2S 0 inz
015300020322  "   * data usata da XDATCD per reperire l'ultimo g del mese
015400020322  "   * della data annotazione/liquidazione slittata
015500020322  "  D $DataDS         DS            10    inz
015600020322  "  D  $DSAnno                1      4S 0
015700020322  "  D  $DSLin1                5      5
015800020322  "  D  $DSMese                6      7S 0
015900020322  "  D  $DSLin2                8      8
016000020322  "  D  $DSGiorno              9     10S 0
016100020321C1550D
016200020927D1208 *---------------------------------------------------------
016300020927  "   * il controllo scostamento dipende dal nr decimali divisa
016400020927D1208D $Scostam        s              4s 3 inz
016500060613C1939 *-------------
016600060613  "   * reperimento paramentro modulo Iva (solo 1 volta)
016700060613  "  D XPaOut          ds          2000
016800060807C1939D Anp045ds      e ds                  inz
016900060807D2033D $Anp045ds       s                   like(anp045ds)
017000060807C1939D $RepParModIVA   s              1a   inz('0')
017100060613  "   * importo di tolleranza stabilito nel parametro mod. Iva
017200060613  "  D $Tolleranza     s                   like(TollIvA045) inz
017300060613  "   * dati msg MVC0095
017400060613  "  D                 DS           512
017500060613  "  D  P1MVC0095              1     61
017600060613  "   *  registro iva (=1a)
017700060613  "  D  Char09501              1      1
017800060613  "   *  tolleranza (14,3 editato=15)
017900060613  "  D  Char09502              2     16
018000060613  "   *  imposta esistente (=14,3 editato=15)
018100060613  "  D  Char09503             17     31
018200060613  "   *  limite da  (=14,3 editato=15)
018300060613  "  D  Char09504             32     46
018400060613  "   *  limite a   (=14,3 editato=15)
018500060613  "  D  Char09505             47     61
018600060613  "   *
018700060613  "  D $WImposta       s             14s 3 inz
018800060613C1939 *-------------
018900950601     D*---------------------------------------------------------
019000950601     C/EJECT
019100950601     C     *entry        PLIST
019200950601    >C                   PARM                    Operazione
019300950601    >C                   PARM                    LenInp
019400950601    >C                   PARM                    Input
019500950601    >C                   PARM                    NrRigaM
019600950601    >C                   PARM                    NrRiga
019700950601     C                   PARM                    GesMsg
019800950601     C                   PARM                    Esito
019900950601     C                   PARM                    StrutturaO
020000950601     C                   PARM                    LenOut
020100950601     C                   PARM                    Output
020200010112D0808C                   PARM                    Fun8Mvc099
020300950601     C*
020400950601     C* Resetta variabili
020500950601     C*
020600950601     C                   EXSR      InzVar
020700950601     C*
020800950601     C* Esegue attività richiesta
020900950601     C*
021000950601     C                   EXSR      Esegui
021100950601     C*
021200950601     C* Costruisce strutture dati da rendere se richiesto
021300950601     C*
021400950601     C                   EXSR      PreOut
021500950601     C*
021600950601     C* Rende i messaggi al chiamante se ho eseguito almeno una callb
021700950601     C* durante l'elaborazione che ha terminato con errore (var. EsiCal)
021800950601     C*
021900950601     C                   EXSR      Return
022000950601     C*
022100950601     C                   RETURN
022200950601     C/EJECT
022300950601     C*----------------------------------------------------------
022400950601     C* Resetta parametri standard
022500950601     C*----------------------------------------------------------
022600950601     C     InzVar        BEGSR
022700950601     C*
022800950615     C                   RESET                   AnopeDS
022900950803     C                   Reset                   ND010DS
023000950615     C                   RESET                   EsiCal
023100950601     C                   EVAL      Esito = *OFF
023200010112D0808C*
023300010112  "  C* Imposto flag modalità impostazione bottone Fun(8) di NDMVC099
023400010112  "  C                   If        PARMS >= 11
023500010112  "  C                   Eval      WFun8099 = Fun8Mvc099
023600010112  "  C                   Else
023700010112  "  C                   Eval      WFun8099 = '4'
023800010112D0808C                   EndIf
023900980529     C*
024000950612     C                   CLEAR                   Fun
024100950612     C*  bottone(2) stato registrazione
024200950612     C                   EVAL      Fun(2) = '2'
024300950612     C                   EXSR      CallBot199
024400950920     C* Se la contabilità risulta essere 'MD' cioè quella che definisce
024500950920     C* un modello di prima nota non invio i messaggi al programma chiamante
024600950920     C                   if        RegCtb = 'MD'
024700950920     C                   movel     *off          GesMsg
024800950920     C                   endif
024900950612     c*
025000950601     C     Operazione    IfEq      '04'
025100950601     C*
025200950601     C* se richiesta la lettura
025300950601     C* reperisco il rcd mediante le chiavi passate
025400950612     C                   EVAL      IvaSys = Sys003
025500950612     C                   EVAL      IvaNrAsReg = NrAsReg003
025600950601     C                   EVAL      IvaNrRigaM= NrRigaM
025700950601     C                   EVAL      IvaNrRiga = NrRiga
025800950807     C     K04IVA01      CHAIN     NDIVA01                            21
025900980515     C* imposto la DS ND009DS come se mi fosse stata passata
026000950601     C                   EXSR      Rie009
026100951004     C                   EXSR      RieTestata
026200950601     C*
026300950601     C                   Else
026400950601     C* se i dati da usare sono stati passati
026500950601     C* li sposto nella DS su cui operare
026600950811    >C                   reset                   ND009DS
026700950811    >C                   EVAL      %subst(ND009DS:1:LenInp)
026800950811    >C                             = %subst(Input:1:LenInp)
026900950601     C                   IF        Operazione <> '05'
027000950601     C                   EXSR      RieTestata
027100950601     C                   EndIf
027200950601     C*
027300950601     C                   ENDIF
027400950601     C                   ENDSR
027500950601     C/EJECT
027600950601     C*----------------------------------------------------------
027700950601     C* Chimama il driver della testata registrazione
027800950601     C*----------------------------------------------------------
027900950601     C     RieTestata    BEGSR
028000950728     c                   eval      anopeds = £anopetes
028100950601     C*
028200950601     C                   ENDSR
028300950601     C/EJECT
028400950601     C*----------------------------------------------------------
028500950601     C* Riempie campi della ND009DS dal rcd letto per
028600950601     C* l'operazione 04
028700950601     C*----------------------------------------------------------
028800950601     C     Rie009        BEGSR
028900950601     C*
029000950920     C                   RESET                   ND009DS
029100950601     C                   Eval      CAUSRIG009 = IvaCausRig
029200950601     C                   Eval      DTRIF009   = IvaDtRif
029300020322C1550C                   Eval      DTAnnot009 = IVADtAnnot
029400950601     C                   Eval      IMPORTO009 = IvaImporto
029500950601     C                   Eval      IMPORTD009 = IvaImportD
029600950601     C                   Eval      IMPON009   = IvaImponib
029700950601     C                   Eval      IMPOND009  = IvaImpoND
029800950601     C                   Eval      DTCOMP009  = IvaDtComp
029900950601     C                   Eval      DESBREV009 = IvaDesBrev
030000950601     C                   Eval      TPREG009   = IvaTpReg
030100950601     C                   Eval      LIBRO009   = IvaLibro
030200950601     C                   Eval      ALI009     = IvaAliq
030300950601     C                   Eval      RIFIVA009  = IvaRifIva
030400950601     C                   Eval      SOSP009    = IvaSosp
030500950601     C                   If        IvaAvere = 1 and IvaDare = 0
030600950601     C                   Eval      SEGNO009  ='A'
030700950601     C                   Else
030800950601     C                   Eval      SEGNO009  ='D'
030900950601     C                   EndIF
031000080107C2072C                   Eval      Elenchi009 = %SubSt(IVAFlag5A:1:1)
031100080121  "  C                   Eval      Flag01009  = %SubSt(IVAFlag5A:2:1)
031200080121  "  C                   Eval      Flag02009  = %SubSt(IVAFlag5A:3:1)
031300080121  "  C                   Eval      Flag03009  = %SubSt(IVAFlag5A:4:1)
031400080121C2072C                   Eval      Flag04009  = %SubSt(IVAFlag5A:5:1)
031500950601     C*
031600950601     C                   ENDSR
031700950601     C/EJECT
031800950601     C*----------------------------------------------------------
031900950601     C* Esegue la attività richiesta
032000950601     C*----------------------------------------------------------
032100950601     C     Esegui        BEGSR
032200950601     C*
032300950601     C                   IF        Operazione = '05'
032400950807     C                   EXSR      Annulla
032500950601     C*
032600950601     C                   Else
032700950601     C                   EXSR      Controlla
032800950601     C* aggiorno solo se non ho avuto errori
032900950601     C* oppure se è richiesto aggiornamento anche con errori
033000950920     C* o ancora se la contabilità è di tipo 'MD'
033100950920     C                   if        RegCtb = 'MD'
033200950920     C                   Movel     *off          Esito
033300950920     C                   endif
033400950601     C                   IF        Esito = *off And
033500950601     C                             Operazione<> '01' or
033600950601     C                             Operazione = '03'
033700950601     C                   EXSR      Aggiorna
033800950601     C                   ENDIF
033900950601     C                   ENDIF
034000950601     C*
034100950601     C                   ENDSR
034200950601     C/EJECT
034300950601     C*----------------------------------------------------------
034400950601     C* Controlla registrazione Iva
034500950601     C*----------------------------------------------------------
034600950601     C     Controlla     BEGSR
034700950601     C*
034800950601     C                   EXSR      CtrDarAver
034900950601     C*
035000950601     C                   EXSR      CtrCausale
035100950601     C*
035200950601     C                   EXSR      CtrDate
035300950601     C*
035400950601     C                   EXSR      CtrLibro
035500950601     C*
035600950601     C                   EXSR      CtrRifAliq
035700950601     C*
035800950601     C                   EXSR      CtrImporto
035900971211     C*
036000971211     C                   EXSR      CtrIVASosp
036100080107C2072C*
036200080107C2072C                   EXSR      CtrElenchi
036300950601     C*
036400950601     C                   ENDSR
036500950601     C*----------------------------------------------------------
036600950601     C* Controlla importo
036700950601     C*----------------------------------------------------------
036800950601     C     CtrImporto    BEGSR
036900950904     C*
037000060613C1939 *---------------------------------------------------------------
037100980922     c* Se la divisa è la m.c. forzo l'importo in divisa nell'importo m.c.
037200960621     c                   if        Regdivisa = £XscDiv and
037300960621     c                             ImportD009 <> 0
037400960621     c                   eval      Importo009 = ImportD009
037500960621     c                   endif
037600960621     c                   if        Regdivisa = £XscDiv and
037700960621     c                             ImponD009 <> 0
037800960621     c                   eval      Impon009 = ImponD009
037900960621     c                   endif
038000980922     C*
038100060613C1939 *---------------------------------------------------------------
038200980922     C* Controllo imposta
038300980515     C*
038400980515     C* se causale di testata di chiusura/apertura non eseguo il controllo
038500980515     C* reperisco i dati della causale di testata salvando quelli della
038600980515     C* causale di dettaglio righe IVA eventualmente digitata
038700980515     C                   MOVEL     ANOPEDS       WANOPE
038800980515     C                   MOVEL     £ANOPETes     ANOPEDS
038900981009     C                   EVAL      $NoCtrlSco = *off
039000981009     C* se causale di chiusura/apertura
039100961119     C                   If        OpeTpDocNi = '7'  or
039200981009     C                             OpeTpDocNi = '8'  or
039300981009     C* oppure siamo nella gestione insoluti
039400981012     C                             REGFunGen = 'I'   or
039500981012     C* oppure non esiste una data di riferimento cambio
039600981012     C                             £DtRfCam = *loval
039700981009     C* non eseguo il controllo
039800981009     C                   EVAL      $NoCtrlSco = *on
039900961119     C                   ENDIF
040000980922     C*
040100980922     C                   Reset                   PJX00202DS
040200980922     C                   MOVE      REGDivisa     Divisa0202
040300980922     C                   Move      £XScDiv       DivMC0202
040400980922     C                   Z-Add     ImportD009    ImporD0202
040500980922     C                   Z-Add     Importo009    ImporM0202
040600980922     C                   Z-Add     REGCambio     Cambio0202
040700980922     C                   Move      £DtRfCam      DtRif0202
040800980922     C*
040900980922     C                   CallP     X0202TestC(PJX00202DS: %Size(PJX00202DS):
041000981009     C                                        GesMsg: $Esito: $NoCtrlSco)
041100980922     C*
041200980922     c                   Eval      IMPORTO009 = ImporM0202
041300980922     c                   Eval      IMPORTD009 = imporD0202
041400950927     C*
041500950927     C                   IF        $Esito = *On
041600950927     C                   Eval      EsiCal= *On
041700950927     C                   Eval      Esito =*on
041800980515     C                   EndIf
041900980515     C* reimposto i dati della causale di dettaglio righe IVA
042000980515     C                   MOVEL     WANOPE        ANOPEDS
042100980922     C*
042200060613C1939 *---------------------------------------------------------------
042300980922     C* controllo imponibile
042400980922     C*
042500980922     C                   Reset                   PJX00202DS
042600980922     C                   MOVE      REGDivisa     Divisa0202
042700980922     C                   Move      £XScDiv       DivMC0202
042800980922     C                   Z-Add     ImponD009     ImporD0202
042900980922     C                   Z-Add     Impon009      ImporM0202
043000980922     C                   Z-Add     REGCambio     Cambio0202
043100980922     C                   Move      £DtRfCam      DtRif0202
043200980922     C*
043300980922     C                   CallP     X0202TestC(PJX00202DS: %Size(PJX00202DS):
043400981009     C                                        GesMsg: $Esito: $NoCtrlSco)
043500980922     C*
043600980922     c                   Eval      Impon009 = ImporM0202
043700980922     c                   Eval      ImponD009 = ImporD0202
043800950927     C*
043900980922     C                   If        $Esito = *On
044000950927     C                   Eval      EsiCal= *On
044100950927     C                   Eval      Esito =*on
044200950927     C                   EndIf
044300950725     C*
044400980511     C* imposta non deve essere valorizzata
044500950601     C                   If        Importo009 <> 0
044600980511     C* se presente codice di esenzione
044700950601     C                             and RifIva009 <> *blank
044800950601     C                   Eval      Esito = '1'
044900950601     C                   Eval      ErrMsg = 'MVC0034'
045000950601     C                   EXSR      MsgErr
045100950601     C                   EndIf
045200980511     C*
045300060613C1939 *---------------------------------------------------------------
045400980511     C* controllo congruenza imponibile-aliquota-imposta
045500980511     C*
045600980511     C* se valorizzati imposta e imponibile in divisa
045700980511     C                   If        ImponD009 <> 0 and ImportD009 <> 0
045800980529     C* ed è presente una aliquota
045900980529     C                             and Ali009 <> 0 and
046000980529     C* e non è richiesta la forzatura
046100980529     C                             Operazione <> '06'
046200980511     C* allora imponibile*aliquota/100 (arrotondato all'unità superiore)
046300980511     C* deve essere = all'imposta in ingresso o comunque compresa entro 5
046400980511     C* unità +/-
046500980511     C                   EVAL      WImposta = (ImponD009 * Ali009)/100
046600020927D1208X**l'arrotondo per eccesso al nr. di decimali giusti per la divisa
046700020927D1208X****************** CallP     X0201Arrot (WImposta: '+': £NrDecDiv)
046800020930D1208 * l'imposta viene arrotondata per eccesso solo per le Lire
046900020930  "   * che oggi non viene + usata
047000020930D1208C                   CallP     X0201Arrot (WImposta: 'H': £NrDecDiv)
047100060613C1939 *
047200060613  "   * reperisco i dati della causale di testata salvando quelli della
047300060613  "   * causale di dettaglio righe IVA eventualmente digitata
047400060613  "  C                   MOVEL     ANOPEDS       WANOPE
047500060613  "  C                   MOVEL     £ANOPETes     ANOPEDS
047600060613  "   *
047700060613  "   *
047800060613  "   * se la causale prevede il controllo con tolleranza
047900060613  "   * e la reg è in m.c.
048000060613  "  C                   if        (OPEQuaIva='3' or OPEQuaIva='4')
048100060613  "  C                             and REGDivisa = £XScDiv
048200060613  "   *
048300060613  "   *  CONTROLLO IMPOSTA CON TOLLERANZA
048400060613  "  C                   exsr      CtrTolleranza
048500060613  "   *
048600060613  "   *  CONTROLLO IMPOSTA CON SCOSTAMENTO
048700060613  "   *  se la causale non prevede il controllo con tolleranza
048800060613  "   *  oppure la reg non è in m.c.
048900060613  "  C                   else
049000060613C1939 *
049100020930D1208X**se l'imposta ricevuta discosta di più o meno 5 unità
049200020927D1208X****************** IF        (WImposta > (ImportD009 + 5) or
049300020927D1208X******************            WImposta < (ImportD009 - 5))
049400020927D1208 * quando si tratta di euro o comunque di divise che prevedono
049500020927  "   * dei decimali, 5 unità sono troppe (erano adatte x le lire)
049600020927  "   * quindi:
049700020927  "   * ____________________________________________________________
049800020927  "   * |divisa con nr decimali   | controllo su uno scostamento di|
049900020927  "   * | 3                       | 0,001                          |
050000020927  "   * | 2                       | 0,01                           |
050100020930  "   * | 1                       | 0,1                            |
050200020927  "   * | 0                       | 5                              |
050300020927  "   * ____________________________________________________________
050400020927  "  C                   select
050500020927  "  C                   when      £NrDecDiv = 3
050600020927  "  C                   eval      $Scostam  = 0,001
050700020927  "  C                   when      £NrDecDiv = 2
050800020927  "  C                   eval      $Scostam  = 0,01
050900020927  "  C                   when      £NrDecDiv = 1
051000020930  "  C                   eval      $Scostam  = 0,1
051100020927  "  C                   when      £NrDecDiv = 0
051200020927  "  C                   eval      $Scostam  = 5
051300020927  "  C                   endsl
051400020927  "   * se l'imposta ricevuta discosta di più o meno di  $scostam
051500020927  "  C                   if        (WImposta > (ImportD009 + $Scostam)
051600020927  "  C                             or
051700020927  "  C                              WImposta < (ImportD009 - $Scostam))
051800020927D1208 *
051900980515     C* errore
052000980511     C                   Eval      Esito = '1'
052100980511     C                   Eval      ErrMsg = 'MVC0088'
052200980511     C                   EXSR      MsgErr
052300980511     C                   ENDIF
052400120330C1939 *
052500120330D2730x**reimposto i dati della causale di dettaglio righe IVA
052600120330D2730x**                 MOVEL     WANOPE        ANOPEDS
052700120330C1939C                   endif
052800060613C1939 *
052900120330D2730 * reimposto i dati della causale di dettaglio righe IVA
053000120330D2730C                   MOVEL     WANOPE        ANOPEDS
053100120330      *
053200980511     C                   ENDIF
053300990512     C*
053400990512C1176C****************************************************************
053500990512  "  C* questo test non è più vero in quanto gli importi in m.c. di
053600990512  "  C* una registrazione in divisa devono essere un semplice
053700990512  "  C* controvalore e non serve che mantengano la relazione tra
053800990512  "  C* imponibile, imposta e aliquota
053900990512  "  C****************************************************************
054000990512  "  C***
054100990512  "  C**se la divisa non è la m.c.
054200990512  "  C***                IF        REGDivisa <> £xscDiv
054300990512  "  C**se valorizzati imposta e imponibile in m.c.
054400990512  "  C***                If        Impon009 <> 0 and Importo009 <> 0
054500990512  "  C**ed è presente una aliquota
054600990512  "  C***                          and Ali009 <> 0 and
054700990512  "  C**e non è richiesta la forzatura
054800990512  "  C***                          Operazione <> '06'
054900990512  "  C**allora imponibile*aliquota/100 (arrotondato all'unità superiore)
055000990512  "  C**deve essere = all'imposta in ingresso o comunque compresa entro 5
055100990512  "  C**unità +/-
055200990512  "  C***                EVAL      WImposta = (Impon009 * Ali009)/100
055300990512  "  C**l'arrotondo per eccesso al nr. di decimali giusti per la divisa
055400990512  "  C***                CallP     X0201Arrot (WImposta: '+': £NrDecDiv)
055500990512  "  C**se l'imposta ricevuta discosta di più o meno 5 unità
055600990512  "  C***                IF        WImposta > (Importo009 + 5) or
055700990512  "  C***                          WImposta < (Importo009 - 5)
055800990512  "  C**errore
055900990512  "  C***                Eval      Esito = '1'
056000990512  "  C***                Eval      ErrMsg = 'MVC0088'
056100990512  "  C***                EXSR      MsgErr
056200990512  "  C***                ENDIF
056300990512  "  C***                ENDIF
056400990512  "  C***                ENDIF
056500990512C1176C****************************************************************
056600980515     C*
056700980515     C* reperisco i dati della causale di testata salvando quelli della
056800980515     C* causale di dettaglio righe IVA eventualmente digitata
056900980515     C                   MOVEL     ANOPEDS       WANOPE
057000980515     C                   MOVEL     £ANOPETes     ANOPEDS
057100980515     C* se la causale di testata prevede l'obbligo imponibile
057200980515     C                   IF        OPEObbImp = '0' and
057300980515     C* ma l'imponibile non è stato passato
057400010723D0965x***                          (Impon009 = 0 or ImponD009 = 0)
057500010723D0965C                             (Impon009 = 0 and ImponD009 = 0)
057600980515     C* errore
057700980515     C                   Eval      Esito = '1'
057800010723D0965x***                Eval      ErrMsg = 'MVC0088'
057900010723D0965C                   Eval      ErrMsg = 'MVC0089'
058000980515     C                   EXSR      MsgErr
058100980515     C                   ENDIF
058200980515     C* reimposto i dati della causale di dettaglio righe IVA
058300980515     C                   MOVEL     WANOPE        ANOPEDS
058400971211     C*
058500971211     C                   ENDSR
058600060613C1939 *****************************************************************
058700060613  "   * Controlla quadratura iva-controllo tolleranza                 *
058800060613  "   *****************************************************************
058900060613  "  C     CtrTolleranza Begsr
059000060613  "   *
059100060613  "   * Reperimento parametro modulo iVA
059200060613  "  C                   if        $RepParModIVA <> *on
059300060613  "  C                   callb     'XPAR'
059400060613  "  C                   parm      REGSocieta    XPaSoc            3
059500060613  "  C                   parm      'IVA     '    XPaPar            8
059600060613  "  C                   parm      *blank        XPaOut
059700060613  "  C                   parm      *off          XPaErr            1
059800060613  "  C                   parm      *blank        XPaRic            1
059900060613  "   *          XPar non emette win se non trova parametro
060000060613  "  C                   parm      *off          XPaMsg            1
060100060613  "   *
060200060807C1939C                   if        XPaErr = '0'
060300060807D2033X* ex C1939 ******* movel     XPaOut        Anp045ds
060400060807D2033 * aggiunto campi numerici in anp045ds dalla pos 10 alla pos 66:
060500060807  "   * se il par.mod. esisteva già, la varibile Xpaout
060600060807  "   * passa dei *blank al posto dei campi numerici aggiunti
060700060807  "   * e quando il pgm usa campi numerici = *blank, si spacca.
060800060807  "  C                   movel     XPaout        $anp045ds
060900060807  "   *       TOLLIVA045
061000060807  "  C                   if        %subst($anp045ds:10:19) = *blank
061100060807  "  C                   eval      %subst($anp045ds:10:19) = *all'0'
061200060807  "  C                   endif
061300060807  "   *       TOLLIVV045
061400060807  "  C                   if        %subst($anp045ds:29:19) = *blank
061500060807  "  C                   eval      %subst($anp045ds:29:19) = *all'0'
061600060807  "  C                   endif
061700060807  "   *       TOLLIVC045
061800060807  "  C                   if        %subst($anp045ds:48:19) = *blank
061900060807  "  C                   eval      %subst($anp045ds:48:19) = *all'0'
062000060807  "  C                   endif
062100060807D2033C                   movel     $anp045ds     Anp045ds
062200060807C1939C                   else
062300060613  "  C                   clear                   Anp045ds
062400060613  "  C                   endif
062500060613  "   *
062600060613  "  C                   eval      $RepParModIVA = *on
062700060613  "  C                   endif
062800060613  "   *
062900060613  "   * Reperisco importo di tolleranza del reg. iva della registraz...
063000060613  "  C                   select
063100060613  "   * ... del registro iva acquisti
063200060613  "  C                   when      TpReg009 = '1'
063300060613  "  C                   z-add     TollIvA045    $Tolleranza
063400060613  "   * ... del registro iva vendite
063500060613  "  C                   when      TpReg009 = '2'
063600060613  "  C                   z-add     TollIvV045    $Tolleranza
063700060613  "   * ... del registro iva corrispettivi
063800060613  "  C                   when      TpReg009 = '3'
063900060613  "  C                   z-add     TollIvC045    $Tolleranza
064000060613  "  C                   endsl
064100060613  "   *
064200060613  "   * Imposta ricevuta discosta di più o meno della tolleranza
064300060613  "  C                   if        (WImposta>(ImportD009+$Tolleranza)
064400060613  "  C                             or
064500060613  "  C                              WImposta<(ImportD009-$Tolleranza))
064600060613  "   *   errore
064700060613  "  C                   eval      Esito = '1'
064800060613  "   *   MVC0095-(F1) Imposta oltre i limiti di (imponibile
064900060613  "   *           * aliquota + o - tolleranza)
065000060613  "  C                   eval      ErrMsg = 'MVC0095'
065100060613  "   *   dati del msg
065200060613  "   *   registro iva
065300060613  "  C                   eval      Char09501 = TpReg009
065400060613  "   *   tolleranza
065500060613  "  C                   z-add     $Tolleranza   $WImposta
065600060613  "  C                   if        $Tolleranza = 0
065700060613  "  C                   eval      Char09502 = '0'
065800060613  "  C                   else
065900060613  "  C                   eval      Char09502 = %triml(%editc($WImposta: 'M'))
066000060613  "  C                   endif
066100060613  "   *   imposta esistente
066200060613  "  C                   z-add     ImportD009    $WImposta
066300060613  "  C                   eval      Char09503 = %editc($WImposta: 'M')
066400060613  "   *   limite da
066500060613  "  C     WImposta      sub       $Tolleranza   $WImposta
066600060613  "  C                   eval      Char09504 = %editc($WImposta: 'M')
066700060613  "  C     WImposta      add       $Tolleranza   $WImposta
066800060613  "  C                   eval      Char09505 = %editc($WImposta: 'M')
066900060613  "   *
067000060613  "  C                   movel     P1MVC0095     DtaMsg
067100060613  "   *
067200060613  "  C                   exsr      MsgErr
067300060613  "   *
067400060613  "  C                   endif
067500060613  "   *
067600060613C1939C     XCtrTolleranzaEndsr
067700971211     C*----------------------------------------------------------
067800971211     C* Controlla flag IVA sospensa
067900971211     C*----------------------------------------------------------
068000971211     C     CtrIVASosp    BEGSR
068100971211     C*
068200971211     C                   RESET                   X07VALDS
068300971211     C                   MOVE      '1'           X07RIC
068400971211     C                   MOVEL     'NDIVA000'    X07TRC
068500971211     C                   MOVEL     'IVASOSP'     X07CAM
068600971211     C                   MOVEL     Sosp009       X07VAL
068700971211     C                   EVAL      X07All = *on
068800971211     C                   CALLB     'X07VALR'
068900971211     C                   PARM                    X07VALDS
069000971211   B3C     X07ERR        IFEQ      '0'
069100971211     C                   MOVEL     X07DES        SospD010
069200971211   X3C                   ELSE
069300971211     C                   Eval      Esito = '1'
069400971216     C                   Eval      ErrMsg = 'MVC0084'
069500971211     C                   EXSR      MsgErr
069600971211   E3C                   ENDIF
069700950601     C*
069800950601     C                   ENDSR
069900950927     C/EJECT
070000080107C2072C*----------------------------------------------------------
070100080107  "  C* Controlla trattamento in elenchi
070200080107  "  C*----------------------------------------------------------
070300080107C2072C     CtrElenchi    BEGSR
070400080213     C*
070500080213     C                   EXSR      CtrNotaVar
070600080107     C*
070700080107     C                   RESET                   X07VALDS
070800080107     C                   MOVE      '1'           X07RIC
070900080107     C                   MOVEL     'ANDIZ000'    X07TRC
071000080107     C                   MOVEL     'DSPELENCHI'  X07CAM
071100080107     C                   MOVEL     Elenchi009    X07VAL
071200080107     C                   EVAL      X07All = *on
071300080107     C                   CALLB     'X07VALR'
071400080107     C                   PARM                    X07VALDS
071500080107     C     X07ERR        IFEQ      '0'
071600080107     C                   MOVEL     X07DES        EleD010
071700080121     C*
071800080121     C* solo se si tratta di nota di accredito, gestisco il campo
071900080121     C* trattamento in Elenchi diverso da *blank
072000080121     C                   if        Elenchi009  <> *Blank     AND
072100080213     C                             $NotaVar    =  *off
072200080121     C                   Eval      Esito = '1'
072300080121     C                   Eval      ErrMsg = 'MVC0097'
072400080121     C                   EXSR      MsgErr
072500080121     C                   endif
072600080121     C*
072700080107     C                   ELSE
072800080107     C                   Eval      Esito = '1'
072900080107     C                   Eval      ErrMsg = 'MVC0096'
073000080107     C                   EXSR      MsgErr
073100080107     C                   ENDIF
073200080107     C*
073300080107C2072C                   ENDSR
073400080107C2072C/EJECT
073500080213C2072C************************************************************
073600080213  "  C* Verifico se trattasi di nota di variazione
073700080213  "  C************************************************************
073800080213C2072C     CtrNotaVar    BEGSR
073900080213     C*
074000080213     C                   Eval      $NotaVar    = *Off
074100080213     C*
074200080213     C                   move      RegSocieta    ELTSocieta
074300080213     C*
074400080213     C* solo causale
074500080213     C                   move      *Blank        ELTTpReg
074600080213     C                   move      *Blank        ELTLibro
074700080213     C                   z-add     0             ELTAliq
074800080213     C                   move      *Blank        ELTRifIva
074900080213     C                   move      RegCausTes    ELTCausale
075000080213     C     K06ELT02      CHAIN     NDELT02L                           21
075100080213     C*
075200080213     C* se non ho trovato una nota di variazione, non la considero e
075300080213     C* faccio una nuova lettura con la stessa chiave
075400080213     C                   if        *In21       = *Off   and
075500080213     C                             ELTCodice  <> 'NV'
075600080213     C     K06ELT02      READE     NDELT02L                               21
075700080213     C                   endif
075800080213     C*
075900080213     C                   Eval      $NotaVar      = Not *In21
076000080213     C*
076100080213C2072C                   ENDSR
076200080213C2072C/EJECT
076300950601     C*----------------------------------------------------------
076400950601     C* Controlla riferimento e aliquota con libro
076500950601     C*----------------------------------------------------------
076600950601     C     CtrRifAliq    BEGSR
076700950601     C*
076800971211     C* Controllo riferimento e aliquota
076900950601     C* devono essere alternati e non blank ed esitere
077000950601     C                   If        (Ali009 <> *zero
077100950601     C                             and RifIva009 <> *blank) or
077200950601     C                             (Ali009 = *zero
077300950601     C                             and RifIva009 = *blank)
077400950601     C                   Eval      Esito = '1'
077500950601     C                   Eval      ErrMsg = 'MVC0033'
077600950601     C                   EXSR      MsgErr
077700950601     C*
077800950601     C                   Else
077900950601     C                   Eval      AlrSocieta = RegSocieta
078000950601     C                   Eval      AlrLibro = Libro009
078100950601     C                   Eval      AlrTpReg = TpReg009
078200950601     C                   Eval      AlrAliq  = Ali009
078300950601     C                   Eval      AlrRifIva= RifIva009
078400950601     C     K05ALR02      CHAIN     ANALR02L                           21
078500950811     C                   If        *in21 =  *on
078600950601     C                   Eval      Esito = '1'
078700950601     C                   Eval      ErrMsg = 'MVC0033'
078800950601     C                   EXSR      MsgErr
078900950601     C                   EndIf
079000950601     C                   EndIf
079100950601     C*
079200950601     C                   ENDSR
079300950601     C/EJECT
079400950601     C*----------------------------------------------------------
079500950601     C* Controlla Libro
079600950601     C*----------------------------------------------------------
079700950601     C     CtrLibro      BEGSR
079800950601     C*
079900971211     C* Controllo libro iva
080000950601     C                   Eval      DliLibro = Libro009
080100950601     C                   Eval      DliTpReg = TpReg009
080200950601     C                   Eval      DliSocieta = RegSocieta
080300950601     C     K03DLI01      CHAIN     ANDLI01L                           21
080400950725     C                   If        *in21 =  *on
080500950601     C                   Eval      Esito = '1'
080600950601     C                   Eval      ErrMsg = 'PRO0306'
080700950601     C                   EXSR      MsgErr
080800950601     C                   EndIf
080900950601     C*
081000950601     C                   ENDSR
081100950601     C/EJECT
081200950601     C*----------------------------------------------------------
081300950601     C* Controlla Date
081400950601     C*----------------------------------------------------------
081500950601     C     CtrDate       BEGSR
081600020321C1550 *
081700020322  "   * Se regime iva=trasportatori mista imposto
081800020322  "   * data annotazione e liquidazione in base allo slittamento
081900020322  "   * eventualmente definito sul libro iva (ANSLI00F)
082000020322  "  C                   if        £RegimeIva = '2'
082100020322  "  C                   exsr      SlittaData
082200020322  "  C                   endif
082300020322  "   *
082400020322  "   * Data annotazione se non impostata:
082500020322  "  C     DtAnnot009    ifeq      *loval
082600020322  "  C                   move      REGDtReg      DtAnnot009
082700020322  "  C                   endif
082800020321C1550 *
082900950601     C*
083000971204     C* DATA LIQUIDAZIONE IVA
083100971204     C*
083200971209     C     dtrif009      ifeq      *loval
083300950808     c                   eval      Dtrif009 = RegDtreg
083400950808     c                   endif
083500971204     C*
083600971204     C* se data liquidazione IVA < data registrazione
083700971205     C                   IF        DtRif009 < RegDtReg and
083800971205     C* e non è richiesta la forzatura data liquidazione IVA
083900971209     C                             Operazione <> '06'  and
084000971209     C* e la registrazione è Definitiva
084100971209     C                             REGTpRegz = 'D'
084200980128     C* controllo che la data sia maggiore del datario corrispettivo
084300971204     C                   MOVE      '1'           XDTRIC
084400971204     C                   MOVE      REGSocieta    XDTSOC
084500971204     C                   MOVE      *blanks       XDTUNI
084600971204     C                   MOVE      'CG'          XDTCTB
084700980128     C                   MOVE      *BLANKS       XDTKEY
084800980128     C                   SELECT
084900980525     C                   WHEN      TpReg009 = '1'
085000980128     C                   MOVE      'IVAACQ'      XDTTIP
085100980525     C                   WHEN      TpReg009 = '2'
085200980128     C                   MOVE      'IVAVEN'      XDTTIP
085300980525     C                   WHEN      TpReg009 = '3'
085400980128     C                   MOVE      'IVACOR'      XDTTIP
085500980128     C                   ENDSL
085600971204     C                   MOVE      *OFF          XDTCMT
085700971204     C                   MOVE      *OFF          XDTALC
085800971204     C*
085900971204     C                   CALLB     'XDATR'       PDAT
086000971204     C*
086100971204     C* se non esiste il datario
086200971210if02 C                   IF        XdtErr = *on
086300971210     C* emetto errore
086400971210     C                   Eval      Esito = '1'
086500971210     C                   Eval      ErrMsg = 'MVC0085'
086600971210     C                   EXSR      MsgErr
086700971210     C*
086800971210     C* se esiste il datario
086900971210     C                   ELSE
087000971210     C* e la data liquidazione IVA non è > di quella del datario
087100971210     C                   IF        DtRif009 <= XdtDat
087200971204     C* emetto errore
087300971204     C                   Eval      Esito = '1'
087400971204     C                   Eval      ErrMsg = 'MVC0032'
087500971204     C                   EXSR      MsgErr
087600971204     C                   ENDIF
087700971209     C                   ENDIF
087800971210     C                   ENDIF
087900971204     C*
088000971209     C     dtcomp009     ifeq      *loval
088100950808     c                   eval      DtComp009 = RegDtreg
088200950808     c                   endif
088300950601     C*
088400950601     C                   ENDSR
088500950601     C/EJECT
088600020321C1550 *----------------------------------------------------------
088700020322  "   * Imposto data annotaz. e liquidaz. in base allo slittamento
088800020321  "   * eventualmente definito sul libro iva (ANSLI00F)
088900020321  "   *----------------------------------------------------------
089000020321  "  C     SlittaData    Begsr
089100020321  "   *
089200020321  "   * verifico  se esistono gli slittamenti sul libro/registro
089300020322  "  C                   if        not %open(ANSLI01L)
089400020322  "  C                   open      ANSLI01L
089500020322  "  C                   endif
089600020322  "  C                   eval      SLISocieta = RegSocieta
089700020322  "  C                   eval      SLITpReg = TpReg009
089800020321  "  C                   eval      SLILibro = Libro009
089900020321  "  C     K03SLI01      chain     ANSLI01L                           21
090000020321  "  C                   if        *in21 =  *off
090100020322  "   *
090200020322  "   * slitta data Annotazione se non impostata
090300020322  "  C     DtAnnot009    ifeq      *loval
090400020322  "  C                   exsr      SlittaDt_A
090500020322  "  C                   endif
090600020322  "   *
090700020322  "   * slitta data Liquidazione se non impostata
090800020322  "  C     DtRif009      ifeq      *loval
090900020322  "  C                   exsr      SlittaDt_L
091000020322  "  C                   endif
091100020321  "   *
091200020321  "  C                   endif
091300020321  "   *
091400020321C1550C     XSlittaDat    Endsr
091500020322C1550 *----------------------------------------------------------
091600020322  "   * Calcolo slittamento data annotazione
091700020322  "   *----------------------------------------------------------
091800020322  "  C     SlittaDt_A    Begsr
091900020322  "   *
092000020322  "   * lo slittamento parte dalla data registrazione
092100020322  "  C                   move      REGDtReg      DtAnnot009
092200020322  "   *
092300020322  "   * reperisco il mese della data, per applicargli lo slittamento
092400020322  "  C                   extrct    DtAnnot009:*M $MeseOrig
092500020322  "   *
092600020322  "   * slitto il mese:
092700020322  "   * il mese originario della data, slitta al mese definito
092800020322  "   * sull'anagrafica slittamenti
092900020322  "  C                   select
093000020322  "  C                   when      $MeseOrig         = 1
093100020322  "  C                   eval      $DSMese  = SLIAnnot01
093200020322  "  C                   when      $MeseOrig         = 2
093300020322  "  C                   eval      $DSMese  = SLIAnnot02
093400020322  "  C                   when      $MeseOrig         = 3
093500020322  "  C                   eval      $DSMese  = SLIAnnot03
093600020322  "  C                   when      $MeseOrig         = 4
093700020322  "  C                   eval      $DSMese  = SLIAnnot04
093800020322  "  C                   when      $MeseOrig         = 5
093900020322  "  C                   eval      $DSMese  = SLIAnnot05
094000020322  "  C                   when      $MeseOrig         = 6
094100020322  "  C                   eval      $DSMese  = SLIAnnot06
094200020322  "  C                   when      $MeseOrig         = 7
094300020322  "  C                   eval      $DSMese  = SLIAnnot07
094400020322  "  C                   when      $MeseOrig         = 8
094500020322  "  C                   eval      $DSMese  = SLIAnnot08
094600020322  "  C                   when      $MeseOrig         = 9
094700020322  "  C                   eval      $DSMese  = SLIAnnot09
094800020322  "  C                   when      $MeseOrig         =10
094900020322  "  C                   eval      $DSMese  = SLIAnnot10
095000020322  "  C                   when      $MeseOrig         =11
095100020322  "  C                   eval      $DSMese  = SLIAnnot11
095200020322  "  C                   when      $MeseOrig         =12
095300020322  "  C                   eval      $DSMese  = SLIAnnot12
095400030714C1550C                   endsl
095500030714C1677 *
095600030714  "   * se in anagrafica slittamento si è definito che il mese x
095700030714  "   * slitta allo stesso mese, la data non slitta a fine mese
095800030714  "   * ( situazione possibile se il libro ha tipo liquidazione
095900030714  "   *   diversa da quella dichiarata in anagrafica società, non
096000030714  "   *   deve slittare e deve essere stampato nei libri iva,
096100030714  "   *   in ordine data registrazione - la stampa libri iva
096200030714  "   *   è in ordine di data annotazione per qualsiasi regime iva-)
096300030714C1677C                   if        $MeseOrig <> $DSMese
096400030714C1550 *
096500020322  "   * slitto l'anno:
096600020322  "   * se il mese originario  supera il mese slittato,
096700020322  "   * anche l'anno deve slittare a quello successivo
096800020322  "  C                   extrct    DtAnnot009:*Y $DSAnno
096900020322  "  C                   if        $MeseOrig  > $DSMese
097000020322  "  C                   eval      $DSAnno  = $DSAnno  + 1
097100020322  "  C                   endif
097200020322  "   *
097300020322  "   * slitto il giorno:
097400020322  "   * per reperisco l'ultimo giorno del mese data slittata
097500020322  "   * passo a XDATCD la data già slittata nel mese e nell'anno
097600020322  "   * ma con giorno=1
097700020322  "  C                   eval      $DSGiorno  = 01
097800020322  "  C                   eval      $DSLin1  = '-'
097900020322  "  C                   eval      $DSLin2  = '-'
098000020322  "   *
098100020322  "  C                   call      'XDATCD'
098200020322  "   *  (codice=4 impostato in tab.G92)
098300020322  "  C                   parm      4             Codice            2 0
098400020322  "  C                   parm                    $DataDS
098500020322  "   *
098600030714C1550C                   move      $DataDS       DtAnnot009
098700030714C1677 *
098800030714C1677C                   endif
098900030714C1550 *
099000020322C1550C     XSlittaDtA    Endsr
099100020322C1550 *----------------------------------------------------------
099200020322  "   * Calcolo slittamento data liquidazione
099300020322  "   *----------------------------------------------------------
099400020322  "  C     SlittaDt_L    Begsr
099500020322  "   *
099600020322  "   * lo slittamento parte dalla data registrazione
099700020322  "  C                   move      REGDtReg      DtRif009
099800020322  "   *
099900020322  "   * reperisco il mese della data, per applicargli lo slittamento
100000020322  "  C                   extrct    DtRif009:*M   $MeseOrig
100100020322  "   *
100200020322  "   * slitto il mese:
100300020322  "   * il mese originario della data, slitta al mese definito
100400020322  "   * sull'anagrafica slittamenti
100500020322  "  C                   select
100600020322  "  C                   when      $MeseOrig         = 1
100700020322  "  C                   eval      $DSMese  = SLILiqui01
100800020322  "  C                   when      $MeseOrig         = 2
100900020322  "  C                   eval      $DSMese  = SLILiqui02
101000020322  "  C                   when      $MeseOrig         = 3
101100020322  "  C                   eval      $DSMese  = SLILiqui03
101200020322  "  C                   when      $MeseOrig         = 4
101300020322  "  C                   eval      $DSMese  = SLILiqui04
101400020322  "  C                   when      $MeseOrig         = 5
101500020322  "  C                   eval      $DSMese  = SLILiqui05
101600020322  "  C                   when      $MeseOrig         = 6
101700020322  "  C                   eval      $DSMese  = SLILiqui06
101800020322  "  C                   when      $MeseOrig         = 7
101900020322  "  C                   eval      $DSMese  = SLILiqui07
102000020322  "  C                   when      $MeseOrig         = 8
102100020322  "  C                   eval      $DSMese  = SLILiqui08
102200020322  "  C                   when      $MeseOrig         = 9
102300020322  "  C                   eval      $DSMese  = SLILiqui09
102400020322  "  C                   when      $MeseOrig         =10
102500020322  "  C                   eval      $DSMese  = SLILiqui10
102600020322  "  C                   when      $MeseOrig         =11
102700020322  "  C                   eval      $DSMese  = SLILiqui11
102800020322  "  C                   when      $MeseOrig         =12
102900020322  "  C                   eval      $DSMese  = SLILiqui12
103000030714C1550C                   endsl
103100030714C1677 *
103200030714  "   * se in anagrafica slittamento si è definito che il mese x
103300030714  "   * slitta allo stesso mese, la data NON slitta a fine mese
103400030714  "   * ( anche se la motivazione principale di ciò è legata alla
103500030714  "   *   data annotazione, la stessa regole viene applicata
103600030714  "   *   alla data liquidazioen per coerenza)
103700030714C1677C                   if        $MeseOrig <> $DSMese
103800030714C1550 *
103900020322  "   * slitto l'anno:
104000020322  "   * se il mese originario  supera il mese slittato,
104100020322  "   * anche l'anno deve slittare a quello successivo
104200020322  "  C                   extrct    DtRif009:*Y   $DSAnno
104300020322  "  C                   if        $MeseOrig  > $DSMese
104400020322  "  C                   eval      $DSAnno  = $DSAnno  + 1
104500020322  "  C                   endif
104600020322  "   *
104700020322  "   * slitto il giorno:
104800020322  "   * per reperisco l'ultimo giorno del mese data slittata
104900020322  "   * passo a XDATCD la data già slittata nel mese e nell'anno
105000020322  "   * ma con giorno=1
105100020322  "  C                   eval      $DSGiorno  = 01
105200020322  "  C                   eval      $DSLin1  = '-'
105300020322  "  C                   eval      $DSLin2  = '-'
105400020322  "   *
105500020322  "  C                   call      'XDATCD'
105600020322  "   *  (codice=4 impostato in tab.G92)
105700020322  "  C                   parm      4             Codice            2 0
105800020322  "  C                   parm                    $DataDS
105900020322  "   *
106000030714C1550C                   move      $DataDS       DtRif009
106100030714C1677 *
106200030714C1677C                   endif
106300030714C1550 *
106400020322C1550C     XSlittaDtL    Endsr
106500950601     C*----------------------------------------------------------
106600950601     C* Controlla Dare,Avere
106700950601     C*----------------------------------------------------------
106800950601     C     CtrDarAver    BEGSR
106900950601     C*
107000950601     C* Il segno deve essere Dare o Avere
107100950724     C                   IF        Segno009 <> 'D' and Segno009 <> 'A'
107200950601     C                   Eval      Esito = '1'
107300950601     C                   Eval      ErrMsg = 'MVC0028'
107400950601     C                   EXSR      MsgErr
107500950601     C                   EndIf
107600950601     C*
107700950601     C                   ENDSR
107800950601     C/EJECT
107900950601     C*----------------------------------------------------------
108000950601     C* Controlla causale riga
108100950601     C*----------------------------------------------------------
108200950601     C     CtrCausale    BEGSR
108300950601     C*
108400950601     C* Se causale Blank nessun controllo
108500950601     C                   IF        CausRig009 <> *Blank
108600950601     C*
108700950601     C                   EVAL      $LenOut = %Size(ANOPEDS)
108800950601     C* Controllo causale riga
108900950601     C                   CallB     'NDMVC003'
109000950601     C                   PARM                    RegSocieta
109100950601     C                   PARM                    CausRig009
109200950601     C                   PARM                    RegDtReg
109300950601     C                   PARM                    *omit
109400950724     C                   PARM                    GesMsg
109500950601     C                   PARM                    $Esito
109600950601     C                   PARM      'ANOPE'       $Struttura
109700950601     C                   PARM                    ANOPEDS
109800950601     C                   PARM                    $LenOut
109900951004     C                   PARM      *on           Refresh
110000950601     C*
110100950601     C                   IF        $Esito = *On
110200950601     C                   Eval      EsiCal= *On
110300950601     C                   Eval      Esito =*on
110400950601     C*
110500950601     C                   Else
110600950601     C                   IF        OPETPC <> '2'
110700950601     C                   Eval      Esito =*on
110800950601     C                   Eval      ErrMsg = 'MVC0001'
110900950601     C                   EXSR      MsgErr
111000950601     C                   EndIf
111100950601     C                   EndIf
111200950601     C*
111300950601     C                   EndIF
111400950601     C*
111500950601     C                   ENDSR
111600950601     C/EJECT
111700950601     C*----------------------------------------------------------
111800950601     C* Aggiorna il movimento iva
111900950601     C*----------------------------------------------------------
112000950601     C     Aggiorna      BEGSR
112100950601     C*
112200950601     C                   EVAL      $iva=*Off
112300950920     c                   RESET                   nd006ds
112400950601     C*
112500950601     C* Se Numero riga e riga movimento sono diversi da zero sono in
112600950601     C* modifica
112700980609     C                   Eval      TotImpo = 0
112800980609     C                   Eval      TotIva  = 0
112900980609     C                   Eval      TotImpD = 0
113000980609     C                   Eval      TotIVAD = 0
113100980520     C                   EVAL      ChgCausale = 0
113200950728     C                   IF        NrRigaM <> *zero And NrRiga <> *Zero
113300950601     C*
113400970123     c                   clear                   ndiva000
113500950601     C* reperisco il rcd mediante le chiavi passate
113600950612     C                   EVAL      IvaSys = Sys003
113700950612     C                   EVAL      IvaNrAsReg = NrAsReg003
113800950601     C                   EVAL      IvaNrRigaM = NrRigaM
113900950601     C                   EVAL      IvaNrRiga = NrRiga
114000950601     C     K04IVA01      CHAIN     NDIVA000                           21
114100950601     C*
114200951201     c* se la causale iva è diversa da quella del movimento riprendo i dati
114300951201     c* per prendere opequaimp
114400951201     c*
114500951201     c                   if        ivacausrig <> causrig009
114600980520     C* memorizzo che è cambiata la causale di riga
114700980520     C                   EVAL      ChgCausale = 1
114800951201     c                   if        Ivacausrig <> *blank
114900951201     C                   EVAL      $LenOut = %Size(ANOPEDS)
115000951201     C                   CallB     'NDMVC003'
115100951201     C                   PARM                    RegSocieta
115200951201     C                   PARM                    IvaCausRig
115300951201     C                   PARM                    RegDtReg
115400951201     C                   PARM                    *omit
115500951201     C                   PARM                    GesMsg
115600951201     C                   PARM                    $Esito
115700951201     C                   PARM      'ANOPE'       $Struttura
115800951201     C                   PARM                    ANOPEDS
115900951201     C                   PARM                    $LenOut
116000951201     C                   PARM      *on           Refresh
116100951201     c                   else
116200980515     c                   exsr      rietestata
116300951201     c                   endif
116400951201     c*
116500951201     c                   endif
116600951201     c                   eval      totimpo = 0
116700980609     c                   eval      TotImpD = 0
116800951201     c                   if        opequaimp = ' ' or opequaimp = '0'
116900950725     C                   Eval      TotImpo = IvaDare * IvaImponib -
117000950725     C                                       IvaAvere * IvaImponib
117100980609     C                   Eval      TotImpD = IvaDare * IvaImponD  -
117200980609     C                                       IvaAvere * IvaImponD
117300951201     c                   endif
117400951201     c* ripristino i dati originali
117500951201     c*
117600951201     c                   if        causrig009 <> *blank
117700951201     c                   exsr      ctrcausale
117800951201     c                   else
117900951201     c                   exsr      rietestata
118000951201     c                   endif
118100950725     C                   Eval      TotIva  = IvaDare * Ivaimporto -
118200950725     C                                       IvaAvere * IvaImporto
118300980609     C                   Eval      TotIvaD = IvaDare * IvaimportD -
118400980609     C                                       IvaAvere * IvaImportD
118500950601     C*
118600951010     c                   if        *in21 = *OFF
118700950601     C                   Eval      $Iva=*On
118800951010     c                   endif
118900950601     C                   ENDIF
119000950601     c*
119100950601     C* Reperimento nuovi numeri
119200950601     C                   EXSR      RepNum
119300950601     C*
119400950601     C* riempio i campi del file con i valori della ND009DS
119500950601     C                   EXSR      RieIva
119600950601     C*
119700950601     C                   IF        $Iva = *on
119800950601     C                   UPDATE    NDIVA000
119900950601     C                   ELSE
120000950601     C                   WRITE     NDIVA000
120100950601     C                   ENDIF
120200951024     C                   CLEAR                   Fun
120300951024     C*  bottone(1) Movimento controllato *ON (nessun errore)
120400951024     C                   EVAL      Fun(1) = '3'
120500951024     C                   If        (Operazione ='02'Or
120600951024     C                             Operazione ='03'  Or
120700971205     C                             Operazione ='04'  Or
120800971205     C                             Operazione ='06') And
120900951024     C                             Esito = *off
121000951024     C                   EVAL      MovCtr024 = *ON
121100951024     C                   EXSR      CallBot299
121200951024     C                   Endif
121300951024     C                   If        (Operazione ='03'Or
121400951024     C                             Operazione ='04') And
121500951024     C                             Esito = *on
121600951024     C                   EVAL      Fun(1) = '1'
121700951024     C                   EXSR      CallBot299
121800951024     C                   Endif
121900950601     C*
122000950601     C*
122100950601     C*
122200950601     C*Scrivo bottone (5) Totali imposta e imponibile
122300951201     C                   Eval      TotImpo003 =            - TotImpo
122400980609     C                   Eval      TotImpD003 =            - TotImpD
122500950908     c                   if        opequaimp = ' ' or opequaimp = '0'
122600980609     C                   Eval      TotImpo003 =            - TotImpo +
122700980609     C                                          (IvaDare * Impon009 -
122800980609     C                                           IvaAvere * Impon009)
122900980609     C                   Eval      TotImpD003 =            - TotImpD +
123000980609     C                                          (IvaDare * ImponD009 -
123100980609     C                                           IvaAvere * ImponD009)
123200950612     c                   endif
123300980609     C                   Eval      TotIva003  =           - TotIva +
123400950725     C                                          (IvaDare * Importo009 -
123500950725     C                                           IvaAvere * Importo009)
123600980609     C                   Eval      TotIvaD003 =           - TotIvaD +
123700980609     C                                          (IvaDare * ImportD009 -
123800980609     C                                           IvaAvere * ImportD009)
123900980520     C* se è variato l'imponibile o l'imposta o la causale di riga
124000980520     c                   if        totImpo003 <> 0 or Totiva003 <> 0 or
124100980609     c                             TotImpD003 <> 0 or TotIVAD003 <> 0 or
124200980520     C                             ChgCausale = 1
124300980520     C                   CLEAR                   Fun
124400980520     C* avverto il gestore dei bottoni che la quadratura IVA deve essere
124500980520     C* ricalcolata :
124600980520     C*  bottone(7) quadratura iva = 1
124700950725     C                   EVAL      Fun(7) = '1'
124800980520     C* avverto il gestore dei bottoni di incrementare i totali imponibile
124900980520     C* e imposte :
125000980520     C*  bottone(5) totali imponibile/imposta = 4
125100950731     C                   Eval      Fun(5) = '4'
125200950601     C                   EXSR      CallBot199
125300950908     c                   endif
125400950721     C* totalizzo importi IVA su 099
125500010112D0808C                   If        WFun8099 = '4'
125600950803     C                   Eval      IvaImp006  =           - TotIVA  +
125700950726     C                                          (IvaDare * Importo009 -
125800950726     C                                           IvaAvere * Importo009)
125900980609     C                   Eval      IvaImpD006=           - TotIVAD +
126000950726     C                                          (IvaDare * ImportD009 -
126100950726     C                                           IvaAvere * ImportD009)
126200010112D0808C                   Else
126300010112  "  C                   Eval      IvaImp006  = (IvaDare * Importo009 -
126400010112  "  C                                           IvaAvere * Importo009)
126500010112  "  C                   Eval      IvaImpD006 = (IvaDare * ImportD009 -
126600010112  "  C                                           IvaAvere * ImportD009)
126700010112D0808C                   EndIf
126800950726     C                   CLEAR                   Fun
126900950908     c                   if        IvaImp006 <> 0 or IvaImpD006 <> 0
127000010112D0808X*****              Eval      Fun(8) = '4'
127100010112D0808C                   Eval      Fun(8) = WFun8099
127200950726     C                   EXSR      CallBot099
127300950908     c                   endif
127400950601     C*
127500950601     C                   ENDSR
127600950807     C/EJECT
127700950807     C*----------------------------------------------------------
127800950807     C* Annulla  il movimento iva
127900950807     C*----------------------------------------------------------
128000950807     C     Annulla       BEGSR
128100950807     C*
128200950807     C                   EVAL      $iva=*Off
128300950807     C* reperisco il rcd mediante le chiavi passate
128400950808     C                   EVAL      IvaSys =  sys003
128500950808     C                   EVAL      IvaNrAsReg =  NrAsReg003
128600950807     C                   EVAL      IvaNrRigaM = NrRigaM
128700950807     C                   EVAL      IvaNrRiga = NrRiga
128800950807     C     K04IVA01      CHAIN     NDIVA000                           21
128900950807     C*
129000950807     C                   Eval      TotImpo = IvaDare * IvaImponib -
129100950807     C                                       IvaAvere * IvaImponib
129200950807     C                   Eval      TotIva  = IvaDare * Ivaimporto -
129300950807     C                                       IvaAvere * IvaImporto
129400980609     C                   Eval      TotImpD = IvaDare * IvaImponD  -
129500980609     C                                       IvaAvere * IvaImponD
129600980609     C                   Eval      TotIvaD = IvaDare * IvaimportD -
129700980609     C                                       IvaAvere * IvaImportD
129800950807     C*
129900950807     c*
130000970415     C                   delete    NDIVA000
130100950807     C*
130200950807     C*
130300950807     C*
130400950807     C*Scrivo bottone (5) Totali imposta e imponibile
130500950807     c                   if        opequaimp = ' ' or opequaimp = '1'
130600950808     C                   Eval      TotImpo003 =            - TotImpo
130700980609     C                   Eval      TotImpD003 =            - TotImpD
130800950807     c                   endif
130900980609     C                   Eval      TotIva003  =           - TotIva
131000980609     C                   Eval      TotIVAD003 =           - TotIvaD
131100950908     c*
131200980609     c                   if        TotImpo003 <> 0 or TotIva003 <> 0 or
131300980609     c                             TotImpD003 <> 0 or TotIVAD003 <> 0
131400950807     C                   CLEAR                   Fun
131500950807     C*  bottone(7) quadratura iva (iniz)
131600950807     C                   EVAL      Fun(7) = '1'
131700950807     C                   Eval      Fun(5) = '4'
131800950807     C                   EXSR      CallBot199
131900950908     c                   endif
132000950908     c*
132100950807     C* totalizzo importi IVA su 099
132200010112D0808C                   If        WFun8099 = '4'
132300950808     C                   Eval      IvaImp006  =           - TotIVA
132400980609     C                   Eval      IvaImpD006=            - TotIVAD
132500950908     c                   if        IvaImp006 <> 0 or IvaImpD006 <> 0
132600950807     C                   CLEAR                   Fun
132700950807     C                   Eval      Fun(8) = '4'
132800950807     C                   EXSR      CallBot099
132900950908     c                   endif
133000010112D0808C                   EndIf
133100950807     C*
133200951024     C* inizializzo tutti i bottoni 499
133300951024     C                   Movel     *all'1'       Fun
133400951024     C                   EXSR      CallBot299
133500951024     C*
133600950807     C                   ENDSR
133700950601     C/EJECT
133800950601     C*----------------------------------------------------------
133900950601     C* Reperimento numero riga movimento e riga
134000950601     C*----------------------------------------------------------
134100950601     C     RepNum        BEGSR
134200950601     c*
134300950601     C* £RigaMInd variavile esterne
134400950601     C* £RigaM    variavile esterne
134500950601     c* assegna il numero movimento a seconda che sia un 'iva indetraibile
134600950601     c* o meno.
134700950615     c                   if        Opeinded  = *ON
134800950601     C                   EVAL      NrRigaM = £Rigamind
134900950601     c                   else
135000950601     C                   EVAL      NrRigaM = £Rigam
135100950601     c                   endif
135200950601     c*
135300950601     C* se uguale a zero reperisco l'ultimo numero
135400950601     C                   If        NrRigaM = 0
135500950807     c                   exsr      RepNumMov
135600950601     c*
135700950601     c* Salvo il numero reperito nelle variabili esterne
135800950810     c* Se Indetraibile tengo un numero vuoto per l' eventuale storno
135900951004     c* e uno per l'eventuale autofattura
136000951004     c*
136100950615     c                   if        OpeInded = *ON
136200951004     C                   EVAL      £Rigamind = NrRigaM + 2
136300951004     C                   EVAL      NrRigam = NrRigaM + 2
136400950807     c                   exsr      RepNumMov
136500951004     c                   exsr      RepNumMov
136600950601     c                   else
136700950810     c* Se non Indetraibile tengo un numero vuoto per l' ev. autofattura
136800950810     C                   EVAL      £Rigam = NrRigaM + 1
136900950810     C                   EVAL      NrRigam = NrRigaM + 1
137000950810     c                   exsr      RepNumMov
137100950601     c                   endif
137200950601     c                   endif
137300950601     C*
137400950601     C* Se ho modificato il numero movimento (NrRigaM) azzero numero
137500950601     C* riga (NrRiga)
137600950601     C                   If        IvaNrRigaM <> NrRigaM
137700950601     C                   Eval      NrRiga = 0
137800950601     C                   Endif
137900950601     C* Se numero riga = 0 allora reperisco un nuovo numero per la riga
138000950601     C                   If        NrRiga = 0
138100950601     C                   Exsr      RepNumRig
138200950601     C                   Endif
138300950601     C*
138400950601     C* Salvo chiave registrazione e numeratori
138500950612     C                   Eval      $NrAsReg = NrAsReg003
138600950601     C                   Eval      $NrRigaM = NrRigaM
138700950804     C                   Eval      $NrRiga  = NrRiga
138800950601     C                   EndSr
138900950601     C/EJECT
139000950601     C*----------------------------------------------------------
139100950601     C* Reperimento numero riga
139200950601     C*----------------------------------------------------------
139300950601     C     RepNumRig     BEGSR
139400980611     C*
139500980611     C* se variata la registrazione
139600950807     C                   IF        $NrAsReg  <> NrAsReg003 or
139700980611     C* oppure il nr.movimento attualmente in esame è diverso dall'ultimo
139800980611     C* elaborato
139900980611     C                             $NrRigaM  <> NrRigaM  or
140000080805D2422C* oppure, se ultimo numero di riga = 0
140100080805D2422C                             $UltNrRiga = 0 or
140200980612     C* oppure, se la riga di NDIVA esiste
140300980612     C                             $IVA = *on  and
140400980612     C* ma è variato il movimento a cui è collegato rispetto alla
140500980611     C* precedente scrittura (da indeducibile a deducibile o viceversa)
140600980611     C                             IvaNrRigaM <> NrRigaM
140700980611     C* reperisco l'ultimo numero di riga
140800950807     C                   EVAL      IvaSys = Sys003
140900950807     C                   EVAL      IvaNrAsReg = NrAsReg003
141000980611     C                   EVAL      IvaNrRigaM = NrRigaM
141100950601     C* Ultimo movimento
141200950807     C     K03IVA01      SETGT     NDIVA01
141300950807     C     K03IVA01      READPE    NDIVA01                                21
141400950601     C* Se non trovato è il primo movimento
141500950601     C                   If        *IN21 =  *ON
141600950601     C                   Eval      NrRiga = 1
141700950601     C                   Else
141800950601     C* Altrimenti Aggiungo 1
141900950601     C                   Eval      NrRiga = IvaNrRiga + 1
142000950601     C                   EndIf
142100950727     C* Salvo registrazione
142200950727     C                   Eval      $NrAsReg = NrAsReg003
142300950727     C                   Eval      $NrRigaM = NrRigaM
142400950727     C                   Eval      $NrRiga  = NrRiga
142500080327D2368C                   Eval      $UltNrRiga  = NrRiga
142600950601     C*
142700950601     C                   Else
142800980611     C* se sono nell'ambito della stessa registrazione e dello stesso
142900980609     C* movimento, il nuovo nr.riga è
143000080805D2368C* la D2368 è stata effettuata perchè in caso di
143100080805  "  c* immissione di una riga indeducibile + F20, poi invio e scrittur
143200080805  "  c* di un'altra riga iva, si spaccava con scrittura nr. riga doppio
143300080805  "  x****************   Eval      NrRiga = $NrRiga + 1
143400080519  "  c                   Eval      NrRiga = $UltNrRiga + 1
143500080519D2368c                   Eval      $UltNrRiga =NrRiga
143600950601     C                   EndIf
143700950601     C*
143800950601     C                   EndSr
143900950601     C/EJECT
144000950601     C*----------------------------------------------------------
144100950601     C* Reperimento numero riga movimento
144200950601     C*----------------------------------------------------------
144300950601     C     RepNumMov     BEGSR
144400950601     C*
144500950727     C                   Callb     'NDMVC018'
144600950727     C                   Parm                    NrRigaM
144700950727     C*
144800950601     C                   EndSr
144900950721     C/EJECT
145000950721     C*----------------------------------------------------------
145100950721     C* Chimata progamma bottoni 099
145200950721     C*----------------------------------------------------------
145300950726     C     CallBot099    BEGSR
145400950726     C
145500951026     c                   If        operazione <> '04'
145600950726     C                   EVAL      $LenOut = %Size(ND006DS)
145700950726     C                   CALLB     'NDMVC099'
145800950803     C                   parm                    IvaNrrigam
145900950726     C                   parm      '0'           CTR
146000950726     C                   PARM                    Fun
146100950726     C                   PARM                    ND006DS
146200950726     C                   PARM                    $LenOut
146300951026     c                   endif
146400950725     C*
146500950726     C                   ENDSR
146600950601     C/EJECT
146700950601     C*----------------------------------------------------------
146800950601     C* Chimata progamma bottoni registrazione testata
146900950601     C*----------------------------------------------------------
147000950601     C     CallBot199    BEGSR
147100950601     C*
147200951026     c                   If        operazione <> '04'
147300950601     C                   EVAL      $LenOut = %Size(ND003DS)
147400950601     C                   CALLB     'NDMVC199'
147500950601     C                   PARM                    Fun
147600950601     C                   PARM                    ND003DS
147700950601     C                   PARM                    $LenOut
147800951026     c                   endif
147900950601     C*
148000950601     C                   ENDSR
148100950601     C/EJECT
148200950601     C*----------------------------------------------------------
148300950601     C* Riempie campi dsoutput ND010DS
148400950601     C*----------------------------------------------------------
148500950601     C     Rie010        BEGSR
148600950601     C*
148700950601     C* Riempio i campi della DS esterna
148800950601     C                   Eval      OpeDesB010= OpeDesBrev
148900950601     C                   Eval      OpeDes010 = OpeDescr
149000950601     C                   Eval      DliDesB010= DliDesBrev
149100950804     C                   Eval      IvaInd010 = DliIvaInd
149200950601     C                   Eval      DliDes010 = DliDescr
149300950601     C                   Eval      AlrDesB010= AlrDesBrev
149400950601     C                   Eval      AlrDes010 = AlrDescr
149500971211     C* la descrizione del flag IVA sospesa è già stata impostata
149600080107C2072C* la descrizione del trattamento in elenchi è già stata impostata
149700971211     C*
149800950601     C                   ENDSR
149900950601     C/EJECT
150000950601     C*----------------------------------------------------------
150100950601     C* Riempie campi del file dalla ND009DS
150200950601     C*----------------------------------------------------------
150300950601     C     RieIva        BEGSR
150400950601     C*
150500950920     C                   RESET                   NDIVA000
150600971209     C                   EVAL      IVADTREG = *loval
150700971209     C                   EVAL      IVADTDOC = *loval
150800971209     C                   EVAL      IVADTRIF = *loval
150900020322C1550C                   EVAL      IVADtAnnot = *loval
151000971209     C                   EVAL      IVADTCOMP = *loval
151100950601     C* Campi chiavi
151200950612     C                   Eval      IVASYS    = Sys003
151300950612     C                   Eval      IVANRASREG= NraSreg003
151400950601     C                   Eval      IVANRRIGAM= NrRigaM
151500950601     C                   Eval      IVANRRIGA = NrRiga
151600950601     C* Campi
151700950601     C                   Eval      IVACAUSRIG = CausRig009
151800950601     C                   Eval      IVADTRIF   = DtRif009
151900020322C1550C                   Eval      IVADtAnnot = DtAnnot009
152000950601     C                   Eval      IVAIMPORTO = Importo009
152100950601     C                   Eval      IVAIMPORTD = ImportD009
152200950601     C                   Eval      IVAIMPONIB = ImpoN009
152300950601     C                   Eval      IVAIMPOND  = ImpoND009
152400950601     C                   Eval      IVADTCOMP  = DtComp009
152500950601     C                   Eval      IVADESBREV = DesBrev009
152600950601     C                   Eval      IVATPREG   = TpReg009
152700950601     C                   Eval      IVALIBRO   = Libro009
152800950601     C                   Eval      IVAALIQ    = Ali009
152900950601     C                   Eval      IVARIFIVA  = RifIva009
153000950601     C                   Eval      IVASOSP    = Sosp009
153100950601     C                   If        Segno009  = 'A'
153200950601     C                   Eval      IVADARE   = 0
153300950601     C                   Eval      IVAAVERE  = 1
153400950601     C                   Else
153500950601     C                   Eval      IVADARE   = 1
153600950601     C                   Eval      IVAAVERE  = 0
153700950601     C                   EndIF
153800080107C2072C                   Eval      %SubSt(IVAFlag5A:1:1) = Elenchi009
153900080121  "  C                   Eval      %SubSt(IVAFlag5A:2:1) = Flag01009
154000080121  "  C                   Eval      %SubSt(IVAFlag5A:3:1) = Flag02009
154100080121  "  C                   Eval      %SubSt(IVAFlag5A:4:1) = Flag03009
154200080121C2072C                   Eval      %SubSt(IVAFlag5A:5:1) = Flag04009
154300950601     C* da testata
154400950601     C                   Eval      IVASOCIETA = RegSocieta
154500950601     C                   Eval      IVAUNITA  = RegUnita
154600950601     C                   Eval      IVANRREG = RegNrReg
154700950601     C                   Eval      IVADTREG = RegDtReg
154800950601     C                   Eval      IVASERREG= RegSerReg
154900950601     C                   Eval      IVACAUSTES= RegCausTes
155000950601     C                   Eval      IVADTDOC = RegDtDoc
155100950601     C                   Eval      IVANRDOC = RegNrDoc
155200950601     C                   Eval      IVASERDOC= RegSerDoc
155300950615     C* da ANDLI00f se  indetraibile Opeinded = *ON
155400950601     C*
155500951009     C                   IF        Opeinded <> *ON
155600950601     C                   Eval      IVAKCC   = DliKccIvaN
155700950601     C                   Eval      IVAKSC   = DliKscIvaN
155800950601     C                   Eval      IVAINDETR= *Off
155900950601     C                   Else
156000950601     C                   Eval      IVAKCC   = DliKccIvaI
156100950601     C                   Eval      IVAKSC   = DliKscIvaI
156200950601     C                   Eval      IVAINDETR= *On
156300950601     C                   EndIf
156400950601     C*
156500950601     C                   EXSR      Rep1rMov
156600950601     C                   Eval      IVAKCCCP = MovKcc
156700950601     C                   Eval      IVAKSCCP = MovKsc
156800950601     C*
156900950601     C                   EndSr
157000950601     C/EJECT
157100950601     C*----------------------------------------------------------
157200950601     C* Reperisce i dati da NDMOV00F
157300950601     C*----------------------------------------------------------
157400950601     C     Rep1RMov      BEGSR
157500950601     C*
157600950601     C                   Eval      $LenOut = %Size(NDMOVDS)
157700950601     C                   CallB     'NDDRVMOV'
157800950612    >C                   PARM                    Sys003
157900950612    >C                   PARM                    NrAsReg003
158000950803    >C                   PARM      1             MovNrrigam
158100950601     C                   PARM      'NDMOV'       $Struttura
158200950601     C                   PARM                    $LenOut
158300950803     C                   PARM      '1'           Codoper
158400950601     C                   PARM                    NDMOVDS
158500950601     C*
158600950601     C                   ENDSR
158700950601     C/EJECT
158800950601     C*----------------------------------------------------------
158900950601     C* Prepara strutture di output richieste
159000950601     C* Input: Nome struttura richiesta
159100950601     C*----------------------------------------------------------
159200950601     C     PreOut        BEGSR
159300950601     C*
159400950601     C* se richiesto il passaggio di una struttura di output
159500950601     C*
159600950601     C                   IF        %ADDR(StrutturaO) <> *NULL
159700950601     C*
159800950601     C                   SELECT
159900950601    >C                   WHEN      StrutturaO = 'ND010DS'
160000950601     C* riempio la DS di passaggio (se differisce da una tabella o da
160100950601     C* un rcd)
160200950601    >C                   EXSR      Rie010
160300950601     C* passo al pgm chiamante solo il numero di caratteri richiesto
160400950601     C                   IF        %ADDR(Output) <> *NULL
160500950601    >C                   EVAL      %subst(Output:1:LenOut)
160600950601    >C                             = %subst(ND010DS:1:LenOut)
160700950601     C                   ENDIF
160800950601     C*
160900950601     C                   ENDSL
161000950601     C*
161100950601     C                   ENDIF
161200950804
161300950804    >C                   EVAL      %subst(Input:1:LenInp)
161400950804    >C                             = %subst(ND009DS:1:LenInp)
161500950601     C*
161600950601     C                   ENDSR
161700950601     C/EJECT
161800950601     C*----------------------------------------------------------
161900950601     C* Invia messaggio di errore
162000950601     C*----------------------------------------------------------
162100950601     C     MsgErr        BEGSR
162200950601     C*
162300950601     C* Se richiesto l'invio messaggi
162400950601     C*
162500950601     C                   IF        GesMsg = *ON
162600950601     C                   EXSR      InvMsg
162700950601     C                   ENDIF
162800950601     C*
162900950601     C                   ENDSR
163000950601     C/EJECT
163100950601     C*-----------------------------------------------------------
163200950601     C* DEFINIZIONE KLIST
163300950601     C*-----------------------------------------------------------
163400950601     C     DefKlist      BEGSR
163500950601     C*
163600950601     C* NDIVA01L
163700950601     C     K04IVA01      KLIST
163800950601     C                   KFLD                    IvaSys
163900950601     C                   KFLD                    IvaNrAsReg
164000950601     C                   KFLD                    IvaNrRigaM
164100950601     C                   KFLD                    IvaNrRiga
164200950601     C     K03IVA01      KLIST
164300950601     C                   KFLD                    IvaSys
164400950601     C                   KFLD                    IvaNrAsReg
164500950601     C                   KFLD                    IvaNrRigaM
164600950601     C* ANDLI01L
164700950601     C     K03DLI01      KLIST
164800950601     C                   KFLD                    DliSocieta
164900950601     C                   KFLD                    DliTpReg
165000950601     C                   KFLD                    DliLibro
165100950601     C* ANALR02L
165200950601     C     K05ALR02      KLIST
165300950601     C                   KFLD                    AlrSocieta
165400950601     C                   KFLD                    AlrTpReg
165500950601     C                   KFLD                    AlrLibro
165600950601     C                   KFLD                    AlrAliq
165700950601     C                   KFLD                    AlrRifIva
165800020321C1550C* ANSLI01L
165900020321  "  C     K03SLI01      KLIST
166000020321  "  C                   KFLD                    SLISocieta
166100020321  "  C                   KFLD                    SLITpReg
166200020321C1550C                   KFLD                    SLILibro
166300080213C2072C*
166400080213  "  C* NDELT02L
166500080213  "  C     K06ELT02      KLIST
166600080213  "  C                   KFLD                    ELTSocieta
166700080213  "  C                   KFLD                    ELTTpReg
166800080213  "  C                   KFLD                    ELTLibro
166900080213  "  C                   KFLD                    ELTAliq
167000080213  "  C                   KFLD                    ELTRifIVA
167100080213C2072C                   KFLD                    ELTCausale
167200971209     C*
167300971209     C* Plist
167400971209     C     PDAT          PLIST
167500971209     C                   PARM                    XDTRIC
167600971209     C                   PARM                    XDTSOC
167700971209     C                   PARM                    XDTUNI
167800971209     C                   PARM                    XDTCTB
167900971209     C                   PARM                    XDTKEY
168000971209     C                   PARM                    XDTALC
168100971209     C                   PARM                    XDTCMT
168200971209     C                   PARM                    XDTDAT
168300971209     C                   PARM                    XDTTIP
168400971209     C                   PARM                    XDTERR
168500971209     C                   PARM                    XDTTST
168600971209     C*
168700950601     C                   ENDSR
168800950601     C/EJECT
168900950601     C*----------------------------------------------------------
169000950601     C* Rende messaggi al chiamante
169100950601     C*----------------------------------------------------------
169200950601     C     RetMsg        BEGSR
169300950601     C*
169400950601     C                   CALL      'QMHMOVPM'
169500950601     C                   PARM                    MsgKey            4
169600950601     C                   PARM      '*DIAG'       MsgType          10
169700950601     C                   PARM      1             LenDta
169800950601     C                   PARM      '*'           CallMsg          10
169900950601     C                   PARM      1             CallStack
170000950601     C                   PARM                    ErrCod
170100950601     C*
170200950601     C                   ENDSR
170300950601     C/EJECT
170400950601     C*----------------------------------------------------------
170500950601     C* Invia messaggi di errore
170600950601     C* Input: ID messaggio, Tipo messaggio
170700950601     C*----------------------------------------------------------
170800950601     C     InvMsg        BEGSR
170900950601     C*
171000950601     C                   CALL      'QMHSNDPM'
171100950601     C                   PARM      ErrMsg        MsgId             7
171200950601     C                   PARM      PROMSG        MsgFil           20
171300950601     C                   PARM      DtaMsg        MsgDta          256
171400950601     C                   PARM      256           LenDta
171500950601     C                   PARM      '*DIAG'       Type             10
171600950601     C                   PARM      '*'           MsgQ             10
171700950601     C                   PARM      1             CallStack
171800950601     C                   PARM                    MsgK              4
171900950601     C                   PARM                    ErrCod
172000950601     C*
172100950601     C                   ENDSR
172200950601     C/EJECT
172300950601     C*----------------------------------------------------------
172400950601     C* Restituisce il controllo al chiamante
172500950601     C*----------------------------------------------------------
172600950601     C     Return        BEGSR
172700950601     C*
172800950601     C* Se ritorno con esito diverso da 0 e ha fatto chiamate a
172900950601     C* ad altri moduli che hanno generato messaggi allora.....
173000950601     C                   IF        Esito<>'0' AND EsiCal=*ON
173100950601     C                   EXSR      RetMsg
173200950601     C                   ENDIF
173300950601     C*
173400950601     C                   RETURN
173500950601     C*
173600950601     C                   ENDSR
173700951024     C/EJECT
173800951024     C*----------------------------------------------------------
173900951024     C* Chimata progamma bottoni movimento IVA
174000951024     C*----------------------------------------------------------
174100951024     C     CallBot299    BEGSR
174200951024     C*
174300951024     c                   if        Operazione <> '04'
174400951024     C                   EVAL      $LenOut = %Size(ND024DS)
174500951025     C                   CALLB     'NDMVC299'
174600951024     C                   parm                    NrRigaM
174700951024     C                   parm                    NrRiga
174800951024     C                   parm      '0'           CTR
174900951024     C                   PARM                    Fun
175000951024     C                   PARM                    ND024DS
175100951024     C                   PARM                    $LenOut
175200951024     C*
175300951024     C                   Endif
175400951024     C                   ENDSR
