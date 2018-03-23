000100081223     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200090909     H* ficn14R2 *---------------------------------------------------*
000300000000     H*-----------------*                                            *
000400090908     H*   Routine di Controllo TARIFFE PADRONCINI particolari        *
000500090908     H*   da applicare al posto della relativa tariffa standard.     *
000600000000     H*--------------------------------------------------------------*
000700011107     Ftntbe01L  IF   E           K DISK
000800090908     Ffifpt01L  IF   E           K DISK
000900090909$001 FFICN14D2  CF   E             WORKSTN
001000090908     F                                     INFDS(DSFMT)
001100090908     H*--------------------------------------------------------------*
001200090908     D* Reperimento nome PGM
001300090908     D STATUS         SDS           333
001400090908     D  DSPGM            *PROC
001500090908     D*-------------
001600090908     D DSFMT           DS
001700090908     D  $TASTO               369    369
001800090908     D  NRG                  370    370
001900090908     D  NCL                  371    371
002000090908     D*-------------
002100090908     D* posizione cursore
002200090908     D CURSOR          DS
002300090908     D  RIRI                   1      2B 0 INZ
002400090908     D  R$$                    2      2
002500090908     D  COCO                   3      4B 0 INZ
002600090908     D  C$$                    4      4
002700090908     D*-------------
002800090908      * tariffe da sostituire + Sostituente
002900090908     D TarSTDNEW       S              7a   DIM(999)                             tsr + std + new
003000090908      *
003100090908      * tariffa da sostituire
003200090908     D TarTSR          S              1a   DIM(999)                             Tipo Servizio
003300090908      *
003400090908      * tariffa da sostituire
003500090908     D TarSTD          S              3a   DIM(999)                             COD.TAR da sostituir
003600090908      *
003700090908      * tariffa da sostituire
003800090908     D TarTSRSTD       S              4a   DIM(999)                             tsr + std
003900090908      *
004000090908      * tariffa Sostituente
004100090908     D TarNEW          S              3a   DIM(999)                             COD.TAR sostituente
004200090908      *
004300090908      * tariffa Sostituente
004400090908     D TarTSRNEW       S              4a   DIM(999)                             tsr + new
004500090908      *
004600090908      * se obbligatoria
004700090908     D TarOBB          S              1a   DIM(999)                             TAR obbligatoria
004800090908      *
004900090908      * se le definizioni delle 2 tariffe devono essere uguali
005000090908     D TarUGU          S              1a   DIM(999)                             se devono essere =
005100090908      *
005200090908      *-----
005300090908     D   DCTP        E DS
005400090908      *
005500090908     D*--------------------------------------------------------------*
005600090908     D* Parametri Input/Output
005700090908     D*--------------------------------------------------------------*
005800090908     D KPJBA         E DS
005900090908     D*
006000090909     D FICN14DS2     E DS
006100930825     D*
006200921102     D WLBDAT          DS
006300940906     D  G02DAT                 1      8  0
006400940906     D  G02INV                 9     16  0
006500940906     D  G02ERR                17     17
006600940906     D  G02TGI                18     22  0
006700090908      *
006800020129     d dataiso         s               d   datfmt(*iso)
006900020129     d dataeur         s               d   datfmt(*eur)
007000090908     D*-------------
007100090908     D* COSTANTI
007200090908     D*-------------
007300090908     D* Tasti di funzione
007400090908     D F01             C                   CONST(X'31')
007500090908     D F02             C                   CONST(X'32')
007600090908     D F03             C                   CONST(X'33')
007700090908     D F04             C                   CONST(X'34')
007800090908     D F05             C                   CONST(X'35')
007900090908     D F06             C                   CONST(X'36')
008000090908     D F07             C                   CONST(X'37')
008100090908     D F08             C                   CONST(X'38')
008200090908     D F09             C                   CONST(X'39')
008300090908     D F10             C                   CONST(X'3A')
008400090908     D F11             C                   CONST(X'3B')
008500090908     D F12             C                   CONST(X'3C')
008600090908     D F13             C                   CONST(X'B1')
008700090908     D F14             C                   CONST(X'B2')
008800090908     D F15             C                   CONST(X'B3')
008900090908     D F16             C                   CONST(X'B4')
009000090908     D F17             C                   CONST(X'B5')
009100090908     D F18             C                   CONST(X'B6')
009200090908     D F19             C                   CONST(X'B7')
009300090908     D F20             C                   CONST(X'B8')
009400090908     D F21             C                   CONST(X'B9')
009500090908     D F22             C                   CONST(X'BA')
009600090908     D F23             C                   CONST(X'BB')
009700090908     D F24             C                   CONST(X'BC')
009800090908     D ENTER           C                   CONST(X'F1')
009900090908     D ROLDWN          C                   CONST(X'F4')
010000090908     D ROLLUP          C                   CONST(X'F5')
010100090908     D*-------------
010200930811     C*---------------------------------------------------------------*
010300090908     C     *ENTRY        PLIST
010400090908     C                   PARM                    KPJBA
010500090909     C                   MOVEL     KPJBU         FICN14DS2
010600090908     C*
010700090908     C*   carica le coppie di  Tariffe da controllare
010800090908     c                   exsr      CARTAB
010900090908     C*
011000090908     C*  Esegue i controlli fra le coppie di tariffe
011100090908     C                   exsr      Controlli
011200090908     C*
011300090908     C*   Emissione finestra x messaggio di Errore
011400090909     c                   if        I14VISUALI    = 'S' and O14ERRORE <> *blank
011500090909     c                   eval      d1msg1 = O14ErrMSG
011600090908     C                   exfmt     MSGWIND
011700090908     C                   end
011800090908     C*
011900090909     C                   eval      KPJBU = FICN14DS2
012000930820     C                   SETON                                        LR
012100011109     C*---------------------------------------------------------------*
012200020125     C*--- CARICAMENTO TABELLE ---------------------------------------*
012300090908     C*---------------------------------------------------------------*
012400020125     C     CARTAB        BEGSR
012500090908     C*
012600090908     C                   Z-ADD     1             C                 5 0
012700090908     C                   movel(P)  'CTP'         tbeCOD
012800090908      *
012900090908     c                   clear                   TarSTDNEW
013000090908     c                   clear                   TarTSR
013100090908     c                   clear                   TarTSRSTD
013200090908     c                   clear                   TarTSRNEW
013300090908     c                   clear                   TarSTD
013400090908     c                   clear                   TarNEW
013500090908     c                   clear                   TarOBB
013600090908     c                   clear                   TarUGU
013700090908      *
013800090908      * carica le coppie di tariffe
013900090908      *   per i controlli incrociati
014000090908     C     tbeCOD        setll     tntbe01l
014100090908     C     tbeCOD        reade     tntbe01l
014200090908     c                   DOw       not %eof(tntbe01l)
014300090908      *
014400090908     C                   movel     tbeuni        dCTP
014500090908     C                   MOVEL     TBeKE2        Standnew          7
014600090908     C                   MOVE      §ctpCTD       Standnew
014700090908     c     Standnew      lookup    tarSTDNEW                              33
014800090908      * non trovato
014900090908     c                   if        not *in33
015000090908     C                   Z-ADD     1             Z                 5 0
015100090908     c     *blank        lookup    tarSTDNEW(Z)                           33
015200090908      *
015300090908     C                   MOVEL     Standnew      tarSTDNEW(Z)
015400090908     C                   MOVEL     TBeKE2        TSR_CTD           4
015500090908     C                   MOVEL     TSR_CTD       tarTSR(Z)
015600090908     C                   MOVE      TSR_CTD       tarSTD(Z)
015700090908     C                   MOVEL     TSR_CTD       tarTSRSTD(Z)
015800090908     C                   MOVE      §ctpCTD       tarNEW(Z)
015900090908     C                   MOVEL     tarTSRSTD(Z)  tarTSRNEW(Z)
016000090908     C                   MOVE      tarNEW(Z)     tarTSRNEW(Z)
016100090908     c                   MOVE      §CTPOBB       TarOBB(Z)
016200090908     c                   MOVE      §CTPUGU       TarUGU(Z)
016300090908      *
016400090908     c                   end
016500090908     C     tbeCOD        reade     tntbe01l
016600090908     c                   endDO
016700020125     C*
016800020125     C                   ENDSR
016900020125      *--------------------------------------------------
017000090908      *  Esegue i controlli sulle coppie di tariffe
017100090908      *--------------------------------------------------
017200090908     c     Controlli     begsr
017300090908     C*
017400090908     c                   DO        *hival        Z
017500090908     C*
017600090908      * Se supera il limite max degli elementi appartenenti le schiere
017700090908      *   oppure
017800090908      * se non ci sono + coppie di tariffe sulle schiere
017900090908     c                   if        Z > %elem(tarSTD) or
018000090908     c                             tarSTD(Z) = *blank
018100090908     c                   leave
018200090908     c                   end
018300090908      *
018400090908      * Solo quelle del Tipo servizio
018500090909     c                   if        tarTSR(Z) = I14TipoSer
018600090908      *
018700090908      * controlla la coppia di tariffe
018800091016      *   Solo se NON richiamato dalla Stampa
018900091016     c                   if        I14CHIAMAN <> 'FICN14R'
019000090908     c                   exsr      CHK_tariffe
019100091016     c                   END
019200090908      *
019300090908      *  per Errore esce ed invia l'errore al chiamante
019400090909     c                   if        O14ERRORE <> *blank
019500090908     c                   leave
019600090908     c                   end
019700090908      *
019800090908     c                   end
019900090908      *
020000090908     c                   endDO
020100090908     C*
020200090908     C                   ENDSR
020300090908      *--------------------------------------------------
020400090908     C*  Mette a confronto per verificare errori o mancanze
020500090908      *--------------------------------------------------
020600090908     c     CHK_tariffe   begsr
020700090908     C*
020800090908     C                   clear                   STD_trovata       1
020900090908     C                   clear                   STD_TPG           1
021000090908     C                   clear                   STD_TSM           1
021100090908     C*
021200090908     C                   clear                   NEW_trovata       1
021300090908     C                   clear                   NEW_TPG           1
021400090908     C                   clear                   NEW_TSM           1
021500090908     C*
021600090909     C                   eval       fptPDR =  I14AUTISTA
021700090909     C                   eval       fptSML =  I14SIMULA
021800090909     C                   eval       fptTSR =  I14TipoSer
021900090909     C                   eval       fptCTR =  I14CODTARI
022000090909     C                   eval       fptPRG =  I14PROGRES
022100090908     C*
022200090908     c                   exsr      Cerca_TAR_STD
022300090908     c                   exsr      Cerca_TAR_NEW
022400090909      *-
022500090909      *- Non può  mai essere che c'è la particolare e NON la standard
022600090909      * Se manca lo standard mentre la particolare c'è....
022700090909     c                   if        STD_trovata = ' ' and
022800090909     c                             NEW_trovata = 'S'
022900090909     c                   eval      O14ERRORE = 'S'
023000090909     c                   eval      O14TARSTD     = tarSTD(Z)
023100090909     c                   eval      O14TARNEW     = tarNEW(Z)
023200090909     c                   eval      O14MANCTAR    = tarSTD(Z)
023300090909     c                   eval      O14ErrMSG = 'La tariffa ' + tarSTD(Z) + ' è -
023400090909     c                             obbligatoria e non è presente x l''autista'
023500090918     c                   eval      O14ErrM35 = 'TAR: Non si può inserire la Tar-
023600090918     c                             . ' + tarNEW(Z)
023700090909     c                   leavesr
023800090909     c                   end
023900090908     C*
024000090908     C* Se Obbligatoria e manca segnala l'errore
024100090908     c                   if        TarOBB(Z) = 'S'
024200090908      *-
024300090908     c                   if        STD_trovata = 'S' and
024400090908     c                             NEW_trovata = ' '
024500090909     c                   eval      O14ERRORE = 'S'
024600090909     c                   eval      O14TARSTD     = tarSTD(Z)
024700090909     c                   eval      O14TARNEW     = tarNEW(Z)
024800090909     c                   eval      O14MANCTAR    = tarNEW(Z)
024900090909     c                   eval      O14ErrMSG = 'La tariffa ' + tarNEW(Z) + ' è -
025000090908     c                             obbligatoria e non è presente x l''autista'
025100090909     c                   eval      O14ErrM35 = 'TAR: Manca la ' + tarNEW(Z) +
025200090909     c                             ' InserirLa'
025300090908     c                   leavesr
025400090908     c                   end
025500090909      *-
025600090908     c                   end
025700090908     C*
025800090908     C* Se Devono essere Uguali e non lo sono segnala l'errore
025900091015     C*  CONTROLLO NON ESEGUITA e non lo sono segnala l'errore
026000090918     c                   if        TarUGU(Z)   = 'S' and
026100090918     c                             TarOBB(Z)   = 'S' and
026200090918     c                             STD_trovata = 'S' and
026300090918     c                             NEW_trovata = 'S'
026400090908      *-
026500090908     c                   if        STD_TPG <> NEW_TPG or
026600090908     c                             STD_TSM <> NEW_TSM
026700090909     c                   eval      O14ERRORE = 'S'
026800090909     c                   eval      O14TARSTD     = tarSTD(Z)
026900090909     c                   eval      O14TARNEW     = tarNEW(Z)
027000090909     c                   eval      O14Diverse    = 'S'
027100090909     c                   eval      O14ErrMSG = 'Il Tipo Somma o il tipo Pagamen-
027200090908     c                             to sono diversi x le tar.: ' + tarSTD(Z) +
027300090908     c                             '/' + tarNEW(Z) + '.'
027400090909     c                   eval      O14ErrM35 = 'TAR: ' + tarSTD(Z) + '/' +
027500090909     c                             tarNEW(Z) + ' con Tipologie DIVERSE.'
027600090908     c                   leavesr
027700090908     c                   end
027800090908      *-
027900090908     c                   end
028000090908     C*
028100090908     C                   ENDSR
028200090908      *--------------------------------------------------
028300090908     C*  cerca di agganciare tariffa STD
028400090908      *--------------------------------------------------
028500090908     c     Cerca_TAR_STD begsr
028600090908     C*
028700090908     C                   eval       fptCTD =  tarSTD(Z)
028800090908     C     KFpt          chain     fifpt01l
028900090918     c                   IF        %Found(fifpt01l) and fptatb=' '
029000090908     C                   eval        STD_trovata = 'S'
029100090908     C                   eval        STD_TPG = FPTTPG
029200090908     C                   eval        STD_TSM = FPTTSM
029300090908     c                   end
029400090908     C*
029500090908     C                   ENDSR
029600090908      *--------------------------------------------------
029700090908     C*  cerca di agganciare tariffa NEW
029800090908      *--------------------------------------------------
029900090908     c     Cerca_TAR_NEW begsr
030000090908     C*
030100090908     C                   eval       fptCTD =  tarNEW(Z)
030200090908     C     KFpt          chain     fifpt01l
030300090918     c                   IF        %Found(fifpt01l) and fptatb=' '
030400090908     C                   eval        NEW_trovata = 'S'
030500090908     C                   eval        NEW_TPG = FPTTPG
030600090908     C                   eval        NEW_TSM = FPTTSM
030700090908     c                   end
030800090908     C*
030900090908     C                   ENDSR
031000090908      *--------------------------------------------------
031100090908     c     *inzsr        begsr
031200090908      *--------------------------------------------------
031300090908     C* Elenco Chiavi Files
031400090908      *
031500020125     C     KFpt          KLIST
031600090908     C                   KFLD                    FPTPDR
031700090908     C                   KFLD                    FPTSML
031800090908     C                   KFLD                    FPTTSR
031900090908     C                   KFLD                    FPTCTR
032000090908     C                   KFLD                    FPTPRG
032100090908     C                   KFLD                    FPTCTD
032200090908     C*
032300020125     c                   endsr
032400050411     C*--------------------------------------------------------------------
