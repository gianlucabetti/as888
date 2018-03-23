000100050422     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(*CALLER)
000200050422     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300050422     H DECEDIT('0,') DATEDIT(*DMY.)
000400050422      ********************************************************************
000500050422      *
000600141222      *         ------------------------------------------------
000700141222      *                Comunicazione Dichiarazione d'intento
000800141222      *                    Creazione file telematico
000900141222      *         ------------------------------------------------
001000050422      *
001100150212R168  * Il formato dei campi J61BIMPOPE e J61BIMPCOR viene modificato
001200150212R168  * da NU a VP. Il formato VP è un campo numerico positivo con due
001300150212R168  * cifre decimali allineato a destra con spazi non significativi
001400150212R168  * a sinistra.
001500050422     F*----------------------------------------------------------------
001600050422     F* file di lavoro delle Dichiarazioni di intento estratte
001700141222     FSDGDIE01L IF   E           K DISK
001800050427     F*----------------
001900141222     F* file di lavoro delle Dichiarazioni di intento - estensione
002000150115     FSDGDIF01L UF   E           K DISK    commit
002100150115     F                                     usropn
002200141222     F*----------------
002300050422     F* file contenente i dati da trasmettere
002400141222     FSDGDIW00F UF A E             DISK    UsrOpn
002500050504     F                                     INFDS(InfDIWDS)
002600141222     F*-------------
002700141222     F* Dati anagrafici aggiuntivi
002801160530R224 X**** Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
002802160530R224 F* Key: Sogg/DtFiVl/Sys/NrAsInt/Dest      UNIQUE
002900141222     FSDGSO301L IF   E           K DISK
003000141222     F*-------------
003100141222     FANSIF01L  IF   E           K DISK
003200050504     D*-------------
003300141229     D* Numero relativo record per SDGDIW00F
003400050504     D InfDIWDS        DS           512
003500050504     D  NRDIW                397    400B 0
003600050427     D*----------------
003700050422     D* Parametro di base
003800050422     D XPAOUT          DS          2000
003900050427     D*----------------
004000050422     D* PARAMETRI ESTERNI
004100050422     D KPJBA         E DS
004200150115     D*----------------
004300150115     D* DS Esterna gestione messaggi
004400150115     D SDGMSGDS      E DS                  INZ
004500150115     D  STS          E                     EXTFLD(MSGSTS)
004600150115     D                                     DIM(333)
004700150115     D  JBA          E                     EXTFLD(MSGJBA)
004800150115     D                                     DIM(502)
004900141222     D*----------------
005000141222     D* Valori assunti
005100141222     D XMPEDS        E DS                  INZ
005200141222     D XMPEDS1       E DS                  INZ
005300141222     D DSInterm        DS                  INZ
005400141222     D $FisInt                             Like(J61BCFINT  )
005500141222     D $ImpDta                       10A
005600141222     D $FirInt                       30A
005700141222     D $FISSOG                       16A
005800150113     D $FISDIC                       11A
005900141222     D $CARSOG                        2A
006000141222     D $COGSOG                       24A
006100141222     D $NOMSOG                       20A
006200141222     D $SEXRSOG                       1A
006300141222     D $DTNSOG                       10A
006400141222     D $COMSOG                       40A
006500141222     D $PRVSOG                        2A
006600141229     D $TELEFON                      12A
006700141222     D $FIRDIC                       30A
006800141230     D*-------------
006900141230     D DSInterm2       DS                  INZ
007000141230     D $IdProd                       16A
007100141230     D $EMAIL                        69A
007200150119     D*-------------
007300150119     D DSContrib       DS                  INZ
007400150119     D $COGCon                       24A
007500150119     D $NOMCon                       20A
007600150119     D $DENCon                       50A
007700150119     D $PIVACon                      11A
007800150119     D $COMCon                       40A
007900150119     D $PRVCon                        2A
008000150119     D $DTNCon                       10A
008100150119     D $SEXCon                        1A
008101160530R224 D $Dati1                  1    158
008102160530R224 D $DestCon                       2A
008200141222     D*----------------
008300141222     D* Ds flag AnSif
008400141222     D                 DS
008500141222     D  SocSogg               31     38
008600141222     D  WrkFlgOpe              1     50
008700050427     D*----------------
008800050422     D* Parametri ricevuti
008900141222     D SDGSJ61DS     E DS
008901170825R246 D*-------------
008902170825 "   D* Passaggio Parametri al PGM di controllo dati
008903170825 "   D SDGSJ71DS     E DS
008904170825R246 D  ERRJ71       E                     INZ('0')
009000141222     D*-------------
009100141222     D* Parametri ricevuti
009200141222     D SDGDIWDS      E DS                  ExtName(SDGDIW00F)
009300050427     D*----------------
009400050422     D* DS Interna per dati di output di XSOC
009500050422     D XSOCDS          S           1000
009600050427     D*----------------
009700050422     D* Dati di ambiente ottenuti da XSOC
009800050422     D SOC001        E DS                  EXTNAME(XSOC001DS)
009900050427     D*----------------
010000050422     D* Reperimento nome PGM
010100050422     D STATUS         SDS           333
010200050422     D  DSPGM            *PROC
010300050427     D*----------------
010400050426     D* DS record A
010500141222     D SDGSJ61DSA    E DS
010600050427     D*----------------
010700050503     D* DS record B
010800141222     D SDGSJ61DSB    E DS
010900050427     D*----------------
011000050426     D* DS record Z
011100141222     D SDGSJ61DSZ    E DS
011200141222     D*------------
011300141222     D* Richiamo a controllo tabelle
011400141222     D XATBDS        E DS
011500141222     D*------------
011600141222     D* tabelle delle nature giuridiche
011700141222     D ANGG16DS      E DS                  INZ
011800050422     D*----------------
011900050422     D* campi di lavoro
012000141222     D WrkDt           S              8A
012100050426     D $FineDIE        S              1
012200050503     D $NrRecB         S              9S 0 Inz(*Zero)
012300050426     D UDateISO        S               D
012400050426     D $TpErrore       S              2A
012500050426     D PrimaVolta      S              1A   Inz('1')
012600141229     D $Errori         S              1A   Inz('0')
012601170825R246 D DataNewMod      s               D   datfmt(*iso) inz(d'2017-03-01')
012700050428     D*-------------
012800050422     D* campi per composizione nome file
012900141229     D $Numero6        S              6
013000141229     D $Numero4        S              4
013100141229     D*-------------
013200141229     D DtAMG           DS
013300141229     D  AMGAnno                1      4
013400141229     D  T1                     5      5
013500141229     D  AMGMese                6      7
013600141229     D  T2                     8      8
013700141229     D  AMGGiorno              9     10
013800141229     D*-------------
013900141229     D DtGMA           DS
014000141229     D  GMAGiorno              1      2  0
014100141229     D  GMAMese                3      4  0
014200141229     D  GMAAnno                5      8  0
014300141222     D*-------------
014400141222     D $CdFiscSog      S                   Like(SO3CdFisc  )
014500141222     D $CdIVASog       S                   Like(SO3PartIva )
014600141222     D $CogRagSog      S                   Like(SO3CogRag  )
014700141222     D $NomeSog        S                   Like(SO3Nome    )
014800141222     D $DtNascSog      S                   Like(SO3DtNasc  )
014900141222     D $LocNascSog     S                   Like(SO3LocNasc )
015000141222     D $PrvNascSog     S                   Like(SO3PrvNasc )
015100141222     D $StatoSog       S                   Like(SO3Stato   )
015200141222     D $StaFedSog      S                   Like(SO3StaFed  )
015300141222     D $LocalitSog     S                   Like(SO3Localit )
015400141222     D $IndirESog      S                   Like(SO3IndirE  )
015500141222     D $SexSog         S                   Like(SO3Sex     )
015600150212R168 D*-------------
015700150212 "   D WImporto        S             19S 3
015800150212 "   D Importo16_2     S             16S 2
015900150212 "   D Importo16A      S             16A
016000150212 "   D*-------------
016100150212 "   D Max$Z           C                   CONST(16)
016200150212 "   D $Z              S              3S 0
016300150212 "   D Sk_Importo      S              1    DIM(Max$Z)
016400150212 "   D*-------------
016500150212 "   D CampoWrk        DS
016600150212 "   D CampoInt                1     14
016700150212 "   D CampoDec               15     16
016800150212 "   D*
016900150212 "   D CampoIntero     DS
017000150212 "   D  Int                    1     13
017100150212 "   D  virgola               14     14
017200150212R168 D  Dec                   15     16
017300050422     C*---------------------------------------------------------------*
017400050422     C*  CICLO PRINCIPALE DEL PROGRAMMA                               *
017500050422     C*---------------------------------------------------------------*
017600050422     C*
017700141222     C                   exsr      InzVar
017701170825R246 C*
017702170825 "   C* dal 1/03/17 non potranno più essere comunicate le
017703170825 "   C* dichiarazioni di intento di tipo 3, ovvero per
017704170825 "   C* periodo
017705170825 "   C* Se dal 01/03/17 ne trovo ancora, le stampo, emetto avviso
017706170825 "   C* e non creo il file telematico
017707170825 "   C                   if        UDateIso   >= DataNewMod
017708170825 "   C                   EXSR      CtrDati
017709170825R246 C                   endif
017800141222     C*
017900141229     C                   Eval      DieSocieta  = SocietaJ61
018000141229     C     K01Die01      chain     SdgDie01L
018100141229     C                   if        %found(SdgDie01l)
018200141229     C*
018300141222     C* Dati Rappresentante
018400141222     C                   Exsr      RepDatInter
018500141230     C                   Exsr      RepDatInter2
018600150119     C* Dati Contribuente
018700150119     C                   Exsr      RepDatCon
018800050422     C*
018900050422     C* Reperimento nome file per scarico
019000050427     C                   EXSR      RepNomeF
019100050422     C*
019200050422     C* Cancellazione e creazione file per scarico
019300050427     C                   EXSR      GestFile
019400141222     C                   open      SdgDiw00F
019500050422     C*
019600050422     C* Scrittura file di scarico
019700141222     C                   EXSR      WrtSdgDIW
019800050422     C*
019900050422     C* Iscrizione a catalogo del file di scarico
020000050427     C                   EXSR      IscrCatF
020100150115     C*
020200150115     C* confermo le modifiche
020300150115     C                   commit
020400141229     C*
020500141229     C                   endif
020600050422     C*
020700050427     C                   EXSR      ENDPGM
020800050422     C*
020900050422     C************************************************************
021000050422     C* Inizializzazioni variabili
021100050422     C************************************************************
021200050422     C     INZVAR        BEGSR
021300050422     C*
021400141222     C                   MoveL     KPJBU         SDGSJ61DS
021500050422     C*
021600141222     C                   Eval      ERRJ61      = *Off
021700050422     C*
021800050427     C                   MoveL     'SOC001'      TIPXSC
021900141222     C                   MoveL     SOCIETAJ61    SOCXSC
022000050427     C                   EXSR      RepSoc
022100050422     C                   if        RTNXSC     <> '1'
022200050427     C                   MoveL     XSOCDS        SOC001
022300050422     C                   else
022400141222     C                   Eval      $Errori     = *On
022500050427     C                   EXSR      ENDPGM
022600050422     C                   endif
022700141222     C*
022800141222     C                   Move      XscSoc        SIFSocieta
022900141222     C     SifSocieta    Chain     AnSif01L                           21
023000141222     C  N21              Move      SifFlgOpe     WrkFlgOpe
023100141222     C   21              Move      *Blanks       SocSogg
023200050422     C*
023300141222     C                   ENDSR
023400050422      /EJECT
023401170825R246 C************************************************************
023402170825 "   C* Controllo i dati prima di creare il file telematico
023403170825 "   C************************************************************
023404170825R246 C     CtrDati       BEGSR
023405170825     C*
023406170825     C                   RESET                   SDGSJ71DS
023407170825     C*
023408170825    >C                   MOVE      XScSoc        SocietaJ71
023409170825     C*
023410170825     C                   MOVEL     SDGSJ71DS     KPJBU
023411170825     C*
023412170825    >C                   CALL      'SDGSJ71R'
023413170825     C                   PARM                    KPJBA
023414170825     C*
023415170825     C                   MOVEL     KPJBU         SDGSJ71DS
023416170825     C*
023417170825     C* ritorno da PGM gestione
023418170825     C                   EXSR      RTNJ71
023419170825     C*
023420170825R246 C                   ENDSR
023421170825R246  /EJECT
023422170825R246 C************************************************************
023423170825 "   C* GESTIONE RITORNO DA PGM DI GESTIONE
023424170825 "   C************************************************************
023425170825R246 C     RTNJ71        BEGSR
023426170825     C*
023427170825     C* funzione non eseguibile per errore :
023428170825     C*
023429170825     C                   SELECT
023430170825     C* 0 = funzione eseguita correttamente
023431170825    >C     ERRJ71        WHENEQ    '0'
023432170825     C*
023433170825     C*
023434170825     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
023435170825    >C     ERRJ71        WHENEQ    '1'
023436170825     C                   EXSR      MsgPgm2
023437170825     C                   EXSR      ENDPGM
023438170825     C*
023439170825     C                   ENDSL
023440170825     C*
023441170825R246 C                   ENDSR
023442170825R246 C/EJECT
023443170825R246 C************************************************************
023444170825 "   C* Messaggio errore da controllo dati
023445170825 "   C************************************************************
023446170825R246 C     MsgPgm2       begsr
023447170825      *
023448170825     C                   reset                   SDGMsgDS
023449170825     C*
023450170825     C                   MOVEL     *Blank        XMSGTXT
023451170825     C                   MOVEL     'S'           MSGEMV
023452170825     C                   Z-ADD     10            MSGRGP
023453170825     C                   MOVEL     'N'           MSGRSP
023454170825     C                   MOVEL     *Blank        MSGTPR
023455170825      *
023456170825     C                   call      'SDGMSGR'
023457170825     C                   parm                    SDGMsgDS
023458170825     C                   parm                    XMsgTxt         135
023459170825     C*
023460170825     C* Modo di inizializzazione della window:
023461170825     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
023462170825     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
023463170825     C*                       per mantenere il sottofondo del video princip.)
023464170825     C                   parm      '1'           XMsgInz           1
023465170825     C*
023466170825     C* COS0599-Si sono verificati degli errori: vedi stampa di controllo.
023467170825     C* PRO0860 Fallita creazione file
023468170825     C                   parm      'COS0599'     XMsgCd1           7
023469170825     C                   parm      'PRO0860'     XMsgCd2           7
023470170825     C                   parm                    XMsgCd3           7
023471170825     C                   parm                    XMsgSn1           7
023472170825     C                   parm                    XMsgCdTxT        63
023473170825     C                   parm                    MsgCdTit         14
023474170825     C                   parm                    MsgClPrtf         1
023475170825     C                   parm                    VarMsg1         100
023476170825     C                   parm                    VarMsg2         100
023477170825     C                   parm                    VarMsg3         100
023478170825     C*
023479170825R246 C                   ENDSR
023480170825R246  /EJECT
023500141222     C************************************************************
023600141222     C* Reperimento dati Intermediario
023700141222     C************************************************************
023800141222     C     RepDatInter   BEGSR
023900141222     C*
024000141222     C* preparo i parametri di chiamata al driver
024100141222     C                   Clear                   XMPEDS
024200141222     C                   EVAL      TpChiamata = '01'
024300141222     C                   EVAL      NChiavi = 5
024400141222     C                   EVAL      VLogica = 2
024500141222     C*
024600141222     C* preparo le chiavi per la chiamata al driver
024700141222     C                   Clear                   XMPEDS1
024800141222     C                   MoveL     XSCSOC        DSMPESOC
024900141222     C                   MoveL     'SDGSJ60R R'  DSMPEPGM
025000141222     C                   Move      'R'           DSMPEPGM
025100141222     C                   MoveL     *blanks       DSMPEPRF
025200141222     C* Si reperiscono i dati di default a seconda delle modalità con
025300141222     C* cui il pgm è usato
025400141222     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
025500141222     C* MPETIP = 'PV'  pgm richiamato da menù
025600141222     C* MPETIP = 'PB'  x lancio Batch
025700141222     C                   MoveL     'PB'          DSMPETIP
025800141222     C*
025900141222     C                   CALLB     'XMPE'
026000141222     C                   PARM                    XMPEDS
026100141222     C                   PARM                    XMPEDS1
026200141222     C*
026300141222     C     Risultato     IfEq      '0'
026400141222     C                   MoveL     DSMPEPAR      DSInterm
026500141222     C                   EndIf
026600141222     C*
026700141222     C                   EndSR
026800141230     C************************************************************
026900141230     C* Reperimento dati Intermediario
027000141230     C************************************************************
027100141230     C     RepDatInter2  BEGSR
027200141230     C*
027300141230     C* preparo i parametri di chiamata al driver
027400141230     C                   Clear                   XMPEDS
027500141230     C                   EVAL      TpChiamata = '01'
027600141230     C                   EVAL      NChiavi = 5
027700141230     C                   EVAL      VLogica = 2
027800141230     C*
027900141230     C* preparo le chiavi per la chiamata al driver
028000141230     C                   Clear                   XMPEDS1
028100141230     C                   MoveL     XSCSOC        DSMPESOC
028200141230     C                   MoveL     'SDGSJ60R S'  DSMPEPGM
028300141230     C                   Move      'S'           DSMPEPGM
028400141230     C                   MoveL     *blanks       DSMPEPRF
028500141230     C* Si reperiscono i dati di default a seconda delle modalità con
028600141230     C* cui il pgm è usato
028700141230     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
028800141230     C* MPETIP = 'PV'  pgm richiamato da menù
028900141230     C* MPETIP = 'PB'  x lancio Batch
029000141230     C                   MoveL     'PB'          DSMPETIP
029100141230     C*
029200141230     C                   CALLB     'XMPE'
029300141230     C                   PARM                    XMPEDS
029400141230     C                   PARM                    XMPEDS1
029500141230     C*
029600141230     C     Risultato     IfEq      '0'
029700141230     C                   MoveL     DSMPEPAR      DSInterm2
029800141230     C                   EndIf
029900141230     C*
030000141230     C                   EndSR
030100150119     C************************************************************
030200150119     C* REPERIMENTO dati contribuente
030300150119     C************************************************************
030400150119     C     RepDatCon     BegSr
030500150119     C*
030600150119     C* preparo i parametri di chiamata al driver
030700150119     C                   Clear                   XMPEDS
030800150119     C                   EVAL      TpChiamata = '01'
030900150119     C                   EVAL      NChiavi = 5
031000150119     C                   EVAL      VLogica = 2
031100150119     C*
031200150119     C* preparo le chiavi per la chiamata al driver
031300150119     C                   Clear                   XMPEDS1
031400150119     C                   MoveL     XSCSOC        DSMPESOC
031500150119     C                   MoveL     'SDGSJ60R C'  DSMPEPGM
031600150119     C                   MoveL     *blanks       DSMPEPRF
031700150119     C* Si reperiscono i dati di default a seconda delle modalita con
031800150119     C* cui il pgm e usato
031900150119     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
032000150119     C* MPETIP = 'PV'  pgm richiamato da menu
032100150119     C* MPETIP = 'PB'  x lancio Batch
032200150119     C                   MoveL     'PB'          DSMPETIP
032300150119     C*
032400150119     C                   CALLB     'XMPE'
032500150119     C                   PARM                    XMPEDS
032600150119     C                   PARM                    XMPEDS1
032700150119     C*
032800150119     C     Risultato     IfEq      '0'
032900150119     C                   MoveL     DSMPEPAR      DSContrib
033000150119     C                   EndIf
033100150119     C*
033200150119     C                   EndSr
033300050422     C************************************************************
033400050422     C* Reperimento nome file per invio dichiarazioni d'intento
033500050422     C************************************************************
033600050422     C     RepNomeF      BEGSR
033700050422     C*
033800141229     C* XX + DF + Anno + Progressivo 4
033900141229     C* Es: CADF150001
034000141229     C*
034100141229     C* reperisco il numero di elaborazione
034200141229     C                   exsr      TakeNrElab
034300141229     C                   if        *In99      = *On
034400141229     C                   eval      $Errori    = *On
034500141229     C                   exsr      Endpgm
034600141229     C                   endif
034700141229     C*
034701170825R246 X****               Move      DieAnno       AnnoAlfa          2
034702170825R246 C                   Move      *Year         AnnoAlfa          2
035000050422     C*
035100050422     C                   Eval      $Numero6    = AnnoAlfa    +
035200141229     C                                           $Numero4
035300050422     C*
035400050422     C* valorizzo a destra
035500141229     C                   Move(P)   $Numero6      $Numero
035600050422     C*
035700050422     C                   CALL      'X35NOMFR'
035800141222     C                   PARM                    SocietaJ61
035900050427     C                   PARM      *Blank        $Unita            8
036000141229     C                   PARM      'DF'          $CdScarico        2
036100050422     C                   PARM                    $Numero           9
036200050422     C                   PARM                    $File            10
036300050422     C                   PARM                    $Libreria        10
036400050422     C                   PARM                    $Errore           1
036500050422     C*
036600050422     C                   if        $Errore    <> '0'
036700050502     C                   Eval      $TpErrore   = '10'
036800050427     C                   EXSR      ENDPGM
036900050422     C                   endif
037000050422     C*
037100050422     C                   ENDSR
037200050422      /EJECT
037300050422     C************************************************************
037400050422     C* Creazione/pulizia file per scarico
037500050422     C************************************************************
037600050422     C     GestFile      BEGSR
037700141229     C*
037800141229     C                   Move      UDateIso      $DtAlfa
037900050422     C*
038000141229     C                   Eval      $DesFileI   = AnnoAlfa
038100141229     C*
038200141229     C                   Eval      $DesFileO   = 'Dich. d''intento del ' +
038300141229     C                                           $DtAlfa
038400141222     C*
038500141222     C                   Eval      $Libreria   = LibJ61
038600141222     C                   Eval      $Errore     = *Off
038700050422     C*
038800141229     C                   CALL      'SDGSJ61C1'
038900050422     C                   PARM                    $File
039000050422     C                   PARM                    $Libreria
039100050422     C                   PARM                    WTpDist           2
039200050422     C                   PARM                    $DtAlfa          10
039300050422     C                   PARM                    $DesFileI         8
039400050422     C                   PARM                    $DesFileO        50
039500050422     C                   PARM                    $Errore           1
039600050422     C*
039700050422     C                   if        $Errore    <> '0'
039800050427     C                   EXSR      ENDPGM
039900050422     C                   endif
040000050422     C*
040100050422     C                   ENDSR
040200050422      /EJECT
040300141222     C************************************************************
040400141222     C* Reperisci dati persona fisica attraverso soggetto
040500141222     C************************************************************
040600141222     C     RepPersFis    BegSr
040700141222     C*
040800141222     C                   Clear                   $CdFiscSog
040900141222     C                   Clear                   $CdIVASog
041000141222     C                   Clear                   $CogRagSog
041100141222     C                   Clear                   $NomeSog
041200141222     C                   Clear                   $DtNascSog
041300141222     C                   Clear                   $LocNascSog
041400141222     C                   Clear                   $PrvNascSog
041500141222     C                   Clear                   $StatoSog
041600141222     C                   Clear                   $StaFedSog
041700141222     C                   Clear                   $LocalitSog
041800141222     C                   Clear                   $IndirESog
041900141222     C                   Clear                   $SexSog
042000141222     C*
042100141222     C                   Eval      SO3Sogg     = SocSogg
042200141222     C                   Move      *Loval        SO3DtFiVl
042300141222     C                   Eval      SO3Sys      = 0
042400141222     C                   Eval      SO3NrAsInt  = 0
042500141222     C*
042600160530R224 X**** K04SO301      CHAIN     SDGSO301L
042601160530R224 C                   EXSR      ChainSDGSO3
042602160530R224 C*
042700141222     C                   If        %found(sdgso301l)
042800141222     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
042900141222     C                   MoveL(P)  SO3PartIva    $CdIVASog
043000141222     C                   MoveL(P)  SO3CogRag     $CogRagSog
043100141222     C                   MoveL(P)  SO3Nome       $NomeSog
043200141222     C                   Move      SO3DtNasc     $DtNascSog
043300141222     C                   MoveL(P)  SO3LocNasc    $LocNascSog
043400141222     C                   MoveL(P)  SO3PrvNasc    $PrvNascSog
043500141222     C                   MoveL(P)  SO3Stato      $StatoSog
043600141222     C                   MoveL(P)  SO3StaFed     $StaFedSog
043700141222     C                   MoveL(P)  SO3Localit    $LocalitSog
043800141222     C                   MoveL(P)  SO3IndirE     $IndirESog
043900141222     C                   MoveL     SO3Sex        $SexSog
044000141222     C                   EndIf
044100141222     C*
044200141222     C                   EndSR
044201160530R224 C************************************************************
044202160530 "   C* Lettura dati aggiuntivi anagrafici
044203160530 "   C************************************************************
044204160530R224 C     ChainSDGSO3   BEGSR
044205160530     C*
044211160530     C                   Eval      SO3Dest     = $DestCon
044218160530     C*
044219160530     C     K05SO301      CHAIN     SDGSO301L
044220160530     C*
044221160530     C* se non trovato, provo con la destinazione Generica
044222160530     C                   If        Not(%found(sdgso301l) ) and
044224160530     C                             SO3Dest    <> *Blank
044225160530     C                   Eval      SO3Dest     = *Blank
044226160530     C     K05SO301      CHAIN     SDGSO301L
044227160530     C                   endif
044228160530     C*
044229160530R224 C                   ENDSR
044230160530R224 C/EJECT
044300050422     C************************************************************
044400050422     C* Scrittura del file per scarico
044500050422     C************************************************************
044600141222     C     WrtSdgDiw     BEGSR
044700050422     C*
044800050422     C* Record "A" di testa della fornitura
044900050427     C                   EXSR      WrtTpRecA
045000050422     C*
045100141222     C* scrittura dei record B
045200050503     C                   EXSR      GesDati
045300050422     C*
045400050422     C* Record "Z" di coda della fornitura
045500050427     C                   EXSR      WrtTpRecZ
045600050422     C*
045700050422     C                   ENDSR
045800050422      /EJECT
045900141222     C******************************************************
046000141222     C* Riempimento dati per t.r. A
046100141222     C******************************************************
046200141222     C     WrtTpRecA     BegSr
046300141222     C*
046400141222     C                   Clear                   SDGDIW000
046500141222     C                   Clear                   SDGSJ61DSA
046600141222     C*
046700141222     C*-----------------------------------
046800141222     C* DATI IDENTIFICATIVI DELLA FORNITURA
046900141222     C*
047000141222     C*
047100141222     C* Tipo record
047200141222     C                   MoveL     'A'           J61ATR
047300141222     C*
047400141222     C* Codice identificativo della fornitura
047500141222     C                   MoveL     'IVI15'       J61AFOR
047600141222     C*
047700141222     C* Tipo fornitore
047800141222     C*      "01" Soggetti che inviano la propria
047900141222     C*      "10" Intermediari
048000141222     C                   MoveL     '01'          J61ATFO
048100141222     C                   If        $FisInt       <> *blanks
048200141222     C                   MoveL     '10'          J61ATFO
048300141222     C                   EndIF
048400141222     C*
048500141222     C* Codice fiscale del contribuente
048600141222     C                   MoveL     XScCdF        J61ACFI
048700141222     C                   If        $FisInt       <> *blanks
048800141222     C                   Move      $FisInt       J61ACFI
048900141222     C                   EndIF
049000141222     C*
049100141222     C* Progressivo invio
049200141222     C                   Z-Add     0             J61APRGINV
049300141222     C* Num.Tot.Invii
049400141222     C                   Z-Add     0             J61ATOTINV
049500141222     C*
049600141222     C* Carattere di controllo
049700141222     C                   MoveL     'A'           J61AK1
049800141222     C*
049900141222     C* Elementi di fine riga
050000141222     C* Impostare i valori esadecimali '0D' e '0A'
050100141222     C* (caratteri ASCI 'CR' ed 'LF')
050200141222     C*
050300141222     C                   Eval      J61AK2  = *blanks
050400141222     C                   Eval      J61AK2  = X'0D25'
050500141222     C*
050600141222     C                   MoveL     SDGSJ61DSA    DiwRec
050700141222     C                   write     SDGDIW000
050800141222     C                   FEOD      SDGDIW00F
050900141222     C*
051000141222     C                   EndSR
051100050422     C******************************************************
051200050503     C* Gestione dati da trasmettere
051300050422     C******************************************************
051400141222     C     GesDati       BEGSR
051500050426     C*
051600050503     C                   Z-ADD     0             $NrRecB
051700050428     C*
051800141222     C                   EXSR      CicloSdgDIE
051900050428     C*
052000141222     C                   ENDSR
052100050428      /EJECT
052200050422     C******************************************************
052300050428     C* Ciclo di lettura delle dichiarazioni estratte
052400050422     C******************************************************
052500141222     C     CicloSdgDIE   BEGSR
052600050503     C*
052700141222     C                   Eval      DieSocieta  = SocietaJ61
052800141222     C     K01Die01      setll     SdgDie01L
052900141222     C     K01Die01      reade     SdgDie01L                              21
053000050503     C*
053100050428     C                   Eval      $FineDIE    = *In21
053200050428     C*
053300050428     C                   DOW       $FineDIE    = *Off
053400050428     C*
053500150105     C                   if        DieAnn      = *off
053600160505R217 C                   eval      DifSocieta =  DieSocieta
053700160301 "   C                   eval      DifCf      =  'F'
053800160301 "   C                   move      DieNrInter    DifNrInter
053900160301 "   C                   eval      DifDtInter =  DieDtInter
054000160301 "   C     K04Dif01      chain(n)  SdgDif01l
054100160301 "   C                   if        %found(SdgDif01l) and
054200160505R217 C                             DifInviato  = *off
054300150105     C                   exsr      WrtTpRecB
054400150107     C                   exsr      UpdSdgDif
054500160505R217 C                   endif
054600150105     C                   endif
054700050428     C*
054800141222     C     K01Die01      reade     SdgDie01L                              21
054900050503     C*
055000050428     C                   Eval      $FineDIE    = *In21
055100050428     C                   ENDDO
055200050428     C*
055300050428     C                   ENDSR
055400050428     C/EJECT
055500141222     C******************************************************
055600141222     C* Riempimento dati per t.r. B
055700141222     C******************************************************
055800141222     C     WrtTpRecB     BegSr
055900141222     C*
056000141222     C                   Clear                   SDGDIW000
056100141222     C                   Clear                   SDGSJ61DSB
056200141222     C*
056300141222     C* Tipo record
056400141222     C                   MoveL     'B'           J61BTR
056500141222     C*-
056600141222     C* Codice fiscale del contribuente
056700141222     C                   MoveL     XScCdF        J61BCFI
056800141222     C* Progressivo modulo
056900141222     C                   Z-Add     1             J61BPRGMOD
057000141222     C*-
057100141222     C* Identificativo produttore software
057200141230     C                   MoveL     $IdProd       J61BIPS
057300141222     C*-
057400141222     C* Integrativa
057500141222     C                   if        ComIntJ61 = *on
057600141222     C                   Move      1             J61BCINT
057700141222     C                   endif
057800150113     C                   Z-Add     PROTOCJ61     J61BPROT
057900150113     C                   Z-Add     PROTODJ61     J61BPROTD
058000141222     C*
058100141222     C                   Move      *all'0'       J61BDTNASC
058200141222     C                   Move      *all'0'       J61BDTNAS1
058300141222     C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
058400150119     C*
058500160530R224 X****               if        DsContrib <> *blank
058501160530R224 C                   if        $Dati1    <> *blank
058600150119     C* Cognome
058700150119     C                   MoveL     $CogCon       J61BCOGNO
058800150119     C* Nome
058900150119     C                   MoveL     $NomCon       J61BNOME
059000150119     C* Denominazione
059100150119     C                   MoveL     $DenCon       J61BDENOMI
059200150119     C* Partita IVA del contribuente
059300150119     C                   MoveL     $PIvaCon      J61BPIVA
059400150119     C* Comune o Stato estero di nascita
059500150119     C                   MoveL     $ComCon       J61BLUONAS
059600150119     C* Provincia di nascita
059700150119     C                   MoveL     $PrvCon       J61BPRVNAS
059800150119     C* Data di nascita
059900150119     C                   If        $DtnCon       <> *Loval
060000150119     C                   Move      $DtnCon       DtAMG
060100150119     C                   Move      AMGAnno       GMAAnno
060200150119     C                   Move      AMGMese       GMAMese
060300150119     C                   Move      AMGGiorno     GMAGiorno
060400150119     C                   Move      DtGMA         J61BDTNASC
060500150119     C                   Else
060600150119     C                   Move      *zeros        J61BDTNASC
060700150119     C                   EndIf
060800150119     C* Sesso
060900150119     C                   MoveL     $SexCon       J61BSesso
061000150119     C*
061100150119     C                   else
061200150119     C*
061300141222     C* Partita IVA del contribuente
061400141222     C                   MoveL     XScIVA        J61BPIVA
061500141222     C*
061600141222     C* reperisco la natura giuridica del contribuente
061700141229     C                   MoveL     XScNCR        WrkNatGiu         2
061800141222     C                   Exsr      RepNatGiu
061900141222     C*
062000141222     C                   Select
062100141222     C*-----------------------
062200141222     C* PERSONA FISICA
062300141222     C* da impostare solo nel caso di persona fisica
062400141222     C                   When      TPNG16      = '1'
062500141222     C*
062600141222     C                   Exsr      RepPersFis
062700141222     C*-
062800141222     C* Cognome
062900141222     C                   MoveL     $CogRagSog    J61BCOGNO
063000141222     C*-
063100141222     C* Nome
063200141222     C                   MoveL     $NomeSog      J61BNOME
063300141222     C*-
063400141222     C* Sesso
063500141222     C                   If        $SexSog       ='1'
063600141222     C                   MoveL     'M'           J61BSESSO
063700141222     C                   Else
063800141222     C                   MoveL     'F'           J61BSESSO
063900141222     C                   EndIf
064000141222     C*-
064100141222     C* Data di nascita
064200141222     C                   If        $DtNascSog    <> *Loval
064300141222     C                   Move      $DtNascSog    DtAMG
064400141222     C                   Move      AMGAnno       GMAAnno
064500141222     C                   Move      AMGMese       GMAMese
064600141222     C                   Move      AMGGiorno     GMAGiorno
064700141222     C                   Move      DtGMA         J61BDTNASC
064800141222     C                   Else
064900141222     C                   Move      *zeros        J61BDTNASC
065000141222     C                   EndIf
065100141222     C*-
065200141222     C* Comune o Stato estero di nascita
065300141222     C                   MoveL     $LocNascSog   J61BLUONAS
065400141222     C*-
065500141222     C* Provincia di nascita
065600141222     C                   MoveL     $PrvNascSog   J61BPRVNAS
065700141222     C*-----------------------
065800141222     C* PERSONA GIURIDICA
065900141222     C* da impostare solo nel caso di persona non fisica
066000141222     C                   When      TPNG16      = '2'  or
066100141222     C                             TPNG16      = '3'
066200141222     C*-
066300141222     C* Denominazione
066400141222     C                   MoveL     XSCRGS        J61BDENOMI
066500141222     C*
066600141222     C                   EndSl
066700150119     C                   Endif
066800141222     C*
066900141222     C*-----------------------------------
067000141222     C* DATI RELATIVI AL RAPPRESENTANTE FIRMATARIO DELLA DICHIARAZIONE
067100141222     C*
067200141222     C* Cod.Fiscale
067300141222     C                   Move      *blanks       J61BCDFIS1
067400150105     C                   Move      *zeros        J61BCDFIS2
067500141222     C* Se significativo
067600141222     C                   If        $FisSog       <> *blanks
067700141222     C* Cod.Fiscale
067800141222     C                   Move      $FisSog       J61BCDFIS1
067900141222     C* Cod.Fiscale
068000150128R166 C                   if        $FisDic <> *blank
068100150113     C                   Movel     $FisDic       J61BCDFIS2
068200150128R166 C                   endif
068300141222     C* Cod.Carica
068400141222     C                   Move      $CarSog       J61BCARR1
068500141222     C*-
068600141222     C* Persona Fisica
068700141222     c                   If        $COGSOG       <> *blanks
068800141222     C*-
068900141222     C                   Eval      J61BCOGNO1   = $CogSOG
069000141222     C                   Eval      J61BNOME1    = $NomSOG
069100150109     C                   Eval      J61BSESSO1   = $SEXRSOG
069200141222     C                   If        $DtnSog       <> *blanks
069300141222     C                   Move      $DtnSog       DtAMG
069400141222     C                   Exsr      ValorizDTA
069500141222     C                   Eval      J61BDTNAS1   = WrkDt
069600141222     C                   EndIf
069700141222     C                   Eval      J61BLUONA1   = $COMSOG
069800141222     C                   Eval      J61BPRVNA1   = $PRVSOG
069900141222     C                   EndIf
070000141222     C*-
070100141222     C                   EndIf
070200141222     C*
070300141222     C* Telefono
070400141229     C                   if        $Telefon <> *blank
070500141229     C                   move      $Telefon      J61Btel
070600141222     C                   endif
070700141222     C* e-mail
070800141222     C                   MoveL     $Email        J61BEMAIL
070900141222     C*
071000141222     C*-----------------------------------
071100141222     C* DICHIARAZIONE
071200141222     C*
071300141229     C                   if        DieDogana   = *off
071400141229     C* Casella acquisti
071500141229     C                   eval      J61BAcq     = 1
071600141229     C                   else
071700141229     C* Casella importazioni
071800141229     C                   eval      J61BImp     = 1
071900141229     C                   endif
072000141229     C*
072100141229     C* Anno
072200141229     C                   eval      J61BAnno    = DieAnno
072300141229     C*
072301170825R246 C                   If        UDateIso < DataNewMod
072400150105     C                   Eval      J61BDTINP    = *zeros
072500150105     C                   Eval      J61BDTFIP    = *zeros
072501170825R246 C                   Endif
072600150212R168 C                   eval      J61BImpOpe   = *zeros
072700150212R168 C                   eval      J61BImpCor   = *zeros
072800141229     C                   select
072900141229     C* Importo operazione
073000141229     C                   when      DieTpOpe    = '1'
073100150212R168 X****               eval      J61BImpOpe  = DieImporto
073200150212  "  C                   eval      WImporto    = DieImporto
073300150212  "  C                   if        WImporto      <> 0
073400150212  "  C                   exsr      RieImporto
073500150212  "  C                   move      Importo16A    J61BImpOpe
073600150212R168 C                   endif
073700141229     C* Importo fino a concorrenza
073800141229     C                   when      DieTpOpe    = '2'
073900150212R168 X****               eval      J61BImpCor  = DieImporto
074000150212  "  C                   eval      WImporto    = DieImporto
074100150212  "  C                   if        WImporto      <> 0
074200150212  "  C                   exsr      RieImporto
074300150212  "  C                   move      Importo16A    J61BImpCor
074400150212R168 C                   endif
074500141229     C* Periodo
074600141229     C                   when      DieTpOpe    = '3'
074700141229     C                   If        DieDtInDic    <> *loval
074800141229     C                   Move      DieDtInDic    DtAMG
074900141222     C                   Exsr      ValorizDTA
075000141222     C                   Eval      J61BDTINP    = WrkDt
075100141222     C                   EndIf
075200141229     C                   If        DieDtFiDic    <> *loval
075300141229     C                   Move      DieDtFiDic    DtAMG
075400141222     C                   Exsr      ValorizDTA
075500150105     C                   Eval      J61BDTFIP    = WrkDt
075600141222     C                   EndIf
075700141229     C*
075800141229     C                   endsl
075900141229     C* Descrizione merce
076000141229     C                   eval      J61BDMerce  = DieDescr
076100141229     C*
076200141229     C* Numero attribuito dal dichiarante
076300150120     C                   movel     DieNrInter    J61BNumDic
076400150120     C                   CALLB     'XALLINEA'
076500150120     C                   PARM                    J61BNumDic
076600150120     C                   PARM      16            LEN2              5 0
076700150120     C                   PARM      '0'           Input             1
076800141229     C*
076900141229     C* Anno della dichiarazione
076901170825R246 X****               eval      J61BAnnDic  = J61BAnno
076902170825R246 C                   eval      J61BAnnDic  = DIEAnno
077100141229     C*
077200141229     C*-----------------------------------
077300141229     C* DESTINATARIO DELLA DICHIARAZIONE
077400141229     C*
077500150105     C                   eval      J61BPIvaD   = *zeros
077600141229     C* Dogana
077700141229     C                   if        DieDogana   = *on
077800141229     C                   eval      J61BDogana  = 1
077900141229     C                   else
078000141229     C                   eval      J61BDogana  = 0
078100141229     C                   endif
078200141229     C*
078300141230     C                   if        DieDogana   = *off
078400141229     C* Codice fiscale
078500141229     C                   eval      J61BCdFisD  = DieCdFisc
078600141229     C*
078700141229     C* Partita Iva
078800141229     C                   eval      J61BPIvaD   = DiePartIva
078900141222     C*
079000141229     C* Cognome
079100141229     C                   if        DieTpn      = '1'
079200141229     C                   Eval      J61BCognoD  = DieCogRag
079300141229     C                   else
079400141229     C* Denominazione
079500141229     C                   Eval      J61BDenomD  = DieCogRag
079600141229     C                   endif
079700141229     C*
079800141229     C* Nome
079900141229     C                   Eval      J61BNomeD   = DieNome
080000141229     C* Sesso
080100150121     C                   If        DieSex     <> *blank
080200150113     C                   If        DieSex      = '1'
080300150113     C                   Eval      J61BSessoD  = 'M'
080400150113     C                   Else
080500150113     C                   Eval      J61BSessoD  = 'F'
080600150113     C                   EndIf
080700150121     C                   EndIf
080800141230     C                   endif
080900141229     C*
081000141229     C*-----------------------------------
081100141229     C* FIRMA
081200141229     C*
081300141229     C* Firma del dichiarante
081400141229     C                   if        $FirDic    <> *blank
081500141229     C                   Eval      J61BFirma   = 1
081600141229     C                   endif
081700141229     C*
081800141229     C*-----------------------------------
081900141229     C* QUADRO A
082000141229     C*
082100141229     C* Tipo Plafond
082200141229     C                   Eval      J61BTpPlf   = %dec(TipoPlfJ61:1:0)
082300141229     C*
082400141229     C* Casella Dichiarazione IVA annuale presentata
082500141229     C                   Eval      J61BIvaPre  = %dec(DIvaPreJ61:1:0)
082600141229     C*
082700141229     C* Esportazioni
082800141229     C                   Eval      J61BEsport  = %dec(EsportJ61:1:0)
082900141229     C*
083000141229     C* Cessioni intracomunitarie
083100141229     C                   Eval      J61BCeIntr  = %dec(CeIntraJ61:1:0)
083200141229     C*
083300141229     C* Cessioni verso San Marino
083400141229     C                   Eval      J61BCeRsm   = %dec(CeRsmJ61:1:0)
083500141229     C*
083600141229     C* Operazioni assimilate
083700141229     C                   Eval      J61BOpeAss  = %dec(OpeAssJ61:1:0)
083800141229     C*
083900141229     C* Operazioni straordinarie
084000141229     C                   Eval      J61BOpeStr  = %dec(OpeStraJ61:1:0)
084100141229     C*
084200141229     C* Filler
084300141229     C                   Eval      J61BFi2     = *blank
084400141229     C                   Eval      J61BFi3     = *blank
084500141229     C*
084600141222     C*-----------------------------------
084700141229     C* SEZIONE INTERMEDIARIO
084800141222     C*
084900141222     C* Cod.Fisc.Intermediario
085000141222     C                   Move      *blanks       J61BCFINT
085100141222     C* Impegno alla presentazione
085200150107     C                   Z-Add     0             J61BFIRINT
085300141222     C* Data dell'Impegno alla presentazione
085400141222     C                   Move      *all'0'       J61BDTIMPE
085500141222     C*
085600141222     C* Se significativo
085700141222     C                   If        $FisInt       <> *blanks
085800141222     C* Cod.Fisc.Intermediario
085900141222     C                   Move      $FisInt       J61BCFINT
086000141222     C* Data dell'Impegno alla presentazione
086100141222     C                   Move      $ImpDta       DtAMG
086200141222     C                   Exsr      ValorizDTA
086300141222     C                   Eval      J61BDTIMPE   = WrkDt
086400150107     C* Firma dell'intemediario
086500150107     C                   if        $FirInt    <> *blank
086600150107     C                   Eval      J61BFirInt  = 1
086700150107     C                   endif
086800141222     C                   EndIf
086900141229     C*
087000141229     C* Filler
087100141229     C                   Eval      J61BFi4     = *blank
087200141229     C                   Eval      J61BFi5     = *blank
087300141229     C*
087400141229     C*-----------------------------------
087500141229     C* SPAZIO RISERVATO AL SERVIZIO TELEMATICO
087600141229     C*
087700141229     C                   Eval      J61BSRST    = *blank
087800141229     C*
087900141229     C* Filler
088000141229     C                   Eval      J61BFi6     = *blank
088100141229     C                   Eval      J61BFi7     = *blank
088200141229     C                   Eval      J61BFi8     = *blank
088300141229     C                   Eval      J61BFi9     = *blank
088400141229     C                   Eval      J61BFi10    = *blank
088500141229     C                   Eval      J61BFi11    = *blank
088600141229     C                   Eval      J61BFi12    = *blank
088700141229     C                   Eval      J61BFi13    = *blank
088800141229     C                   Eval      J61BFi14    = *blank
088900141229     C                   Eval      J61BFi15    = *blank
089000141222     C*-----------------------------------
089100141229     C* CARATTERI DI CONTROLLO
089200141229     C*
089300141222     C                   MoveL     'A'           J61BK1
089400141222     C*-
089500141222     C* Elementi di fine riga
089600141222     C* Impostare i valori esadecimali '0D' e '0A'
089700141222     C* (caratteri ASCI 'CR' ed 'LF')
089800141222     C*
089900141222     C                   Eval      J61BK2 =  *blanks
090000141222     C                   Eval      J61BK2 =  X'0D25'
090100141222     C*
090200141222     C                   MoveL     SDGSJ61DSB    DiwREC
090300141222     C                   write     SdgDiw000
090400141222     C                   FEOD      SdgDiw00F
090500141229     C*
090600141229     C                   ADD       1             $NrRecB
090700150212     C*
090800150212     C                   EndSR
090900150212R168 C******************************************************
091000150212 "   C* Sposta l'importo in un campo alfanumerico
091100150212R168 C******************************************************
091200150212     C     RieImporto    BEGSR
091300150212     C*
091400150212     C                   Clear                   Importo16A
091500150212     C                   Z-Add     WImporto      Importo16_2
091600150212     C*
091700150212     C                   Move      Importo16_2   CampoWrk
091800150212     C                   Move      CampoInt      Int
091900150212     C                   Move      ','           Virgola
092000150212     C                   Move      CampoDec      Dec
092100150212     C                   Move      CampoIntero   Importo16A
092200150212     C*
092300150212     C****               exsr      Ablank
092400150212R168 C*
092500150212 "   C                   ENDSR
092600150212R168 C/EJECT
092700150212R168 C******************************************************
092800150212 "   C* Toglie gli zeri non significativi
092900150212R168 C******************************************************
093000150212     C     ABlank        BEGSR
093100150212     C*
093200150212     C                   MoveA     Importo16A    SK_Importo
093300150212     C*
093400150212     C                   do        Max$Z         $Z
093500150212     C*
093600150212     C                   if        Sk_Importo($Z) = ' '   or
093700150212     C                             Sk_Importo($Z) = '0'
093800150212     C                   Move      ' '           Sk_Importo($Z)
093900150212     C                   else
094000150212     C                   leave
094100150212     C                   endif
094200150212     C*
094300150212     C                   enddo
094400150212     C*
094500150212     C                   MoveA     Sk_Importo    Importo16A
094600150212R168 C*
094700150212 "   C                   ENDSR
094800150212R168 C/EJECT
094900141222     C******************************************************
095000141222     C* Riempimento dati per t.r. Z
095100141222     C******************************************************
095200141222     C     WrtTpRecZ     BegSr
095300141222     C*
095400141222     C                   Clear                   SDGSJ61DSZ
095500141222     C* Tipo Record
095600141222     C                   Eval      J61ZTR       = 'Z'
095700141229     C*
095800141229     C* Filler
095900141229     C                   Eval      J61ZFi1     = *blank
096000141229     C*
096100141229     C* Numero record di tipo 'B'
096200141229     C                   Z-ADD     $NrRecB       J61ZNB
096300141229     C*
096400141229     C* Filler
096500141229     C                   Eval      J61ZFi2     = *blank
096600141229     C                   Eval      J61ZFi3     = *blank
096700141222     C*-
096800141222     C* Carattere di controllo
096900141222     C                   MoveL     'A'           J61ZK1
097000141222     C*-
097100141222     C* Elementi di fine riga
097200141222     C* Impostare i valori esadecimali '0D' e '0A'
097300141222     C* (caratteri ASCI 'CR' ed 'LF')
097400141222     C*
097500141222     C                   Eval      J61ZK2  = *blanks
097600141222     C                   Eval      J61ZK2  = X'0D25'
097700141222     C*
097800141222     C                   MoveL     SDGSJ61DSZ    DiwREC
097900141222     C                   write     SDGDIW000
098000141222     C                   feod      SDGDIW00F
098100141222     C*
098200141222     C                   EndSR
098300150107     C******************************************************
098400150107     C* Aggiorna estensione
098500150107     C******************************************************
098600150107     C     UpdSdgDif     BegSr
098700150107     C*
098800150115     C                   dou       *in23 = *off  or
098900150115     C                             MsgRtn <> 'R'
099000150107     C                   eval      DifSocieta =  DieSocieta
099100150107     C                   eval      DifCf      =  'F'
099200150120     C                   move      DieNrInter    DifNrInter
099300150107     C                   eval      DifDtInter =  DieDtInter
099400150115     C     K04Dif01      chain     SdgDif01l                          2223
099500150115     C*  Gestione RCD allocato
099600150115     C                   if        *in23 = *on
099700150115     C                   Movel     'SDGDIF00F'   MsgFil
099800150115     C                   exsr      MsgPgm
099900150115     C                   endif
100000150115     C                   enddo
100100150115     C*
100200150115     C                   if        *in23     = *Off   and
100300150115     C                             %found(sdgdif01l)
100400150107     C                   eval      DifDtInvio = UDateIso
100500150112     C                   eval      DifInviato = *on
100600150112     C                   eval      DifFileTel = $File
100700150107     C                   update    SdgDif000
100800150115     C                   else
100900150115     C                   if        *in23     = *On
101000150115     C*****              exsr      MsgErr
101100150115     C                   exsr      EndPgm
101200150115     C                   endif
101300150107     C                   endif
101400150107     C*
101500150107     C                   EndSR
101600141222     C******************************************************
101700141222     C* Valorizzazione data per telematico
101800141222     C******************************************************
101900141222     C     ValorizDTA    BegSr                                                  *
102000141222     C*
102100141222     C                   Clear                   WrkDt
102200141222     C*
102300141222     C                   Move      AMGAnno       GMAAnno
102400141222     C                   Move      AMGMese       GMAMese
102500141222     C                   Move      AMGGiorno     GMAGiorno
102600141222     C*
102700141222     C                   Eval      WrkDt        = DtGMA
102800141222     C*
102900141222     C                   EndSR
103000141222     C******************************************************
103100141222     C* Reperimento della natura giuridica
103200141222     C******************************************************
103300141222     C     RepNatGiu     BEGSR
103400050622     C*
103500050622     C                   Reset                   XATBDS
103600050622     C*
103700050622     C                   MOVE      '1'           XTBRIC
103800141222     C                   MOVE      SocietaJ61    XTBAZI
103900050622     C                   MOVE      *ZERO         XTBKEY
104000050622     C                   MOVE      'G16'         XTBCOD
104100050622     C                   MOVE      XScNCR        XTBKEY
104200050622     C                   CALLB     'XATB'
104300050622     C                   PARM                    XATBDS
104400050622     C                   if        XTBERR      = *Off
104500050622     C                   MoveL     XTBUNI        ANGG16DS
104600050622     C                   endif
104700050622     C*
104800141222     C                   ENDSR
104900141222      /EJECT
105000050503     C************************************************************
105100050503     C* Iscrizione a catalogo del file scarico
105200050503     C************************************************************
105300050503     C     IscrCatF      BEGSR
105400050503     C*
105401170825R246 X****               Move      DieAnno       AnnoAlfa
105402170825R246 C                   Move      *Year         AnnoAlfa
105700050422     C*
105800141229     C                   Eval      $Numero6    = AnnoAlfa    +
105900141229     C                                           $Numero4
106000050422     C*
106100050422     C* valorizzo a destra
106200050427     C                   Move(P)   $Numero6      $Numero
106300050422     C*
106400050427     C                   Move      UDateIso      $DtAlfa
106500050422     C*
106600050422     C                   Call      'X36CATFR'
106700141222     C                   PARM                    SocietaJ61
106800050427     C                   PARM      *Blank        $Unita
106900050422     C                   PARM                    $File
107000050422     C                   PARM                    $Libreria
107100050422     C                   PARM                    $DesFileO
107200141229     C                   PARM      'DF'          $CdScarico        2
107300050428     C                   PARM      'P'           $TpSupp           1
107400050428     C                   PARM      *Blank        $NmSupp          10
107500050422     C                   PARM      '4'           $TpDist           1
107600050422     C                   PARM                    $DtAlfa          10
107700050422     C                   PARM                    $Numero           9
107800050427     C                   PARM      *Blank        $DesBan          44
107900050422     C                   PARM                    KNmus
108000050422     C                   PARM      '0'           Sospeso           1
108100050422     C                   PARM      '1'           Video             1
108200050422     C*
108300050422     C                   ENDSR
108400050422      /EJECT
108500141229     C************************************************************
108600141229     C* Reperisco il numero di elaborazione
108700141229     C************************************************************
108800141229     C     TakeNrElab    BEGSR
108900141229     C*
109000141229     C                   Move      UDateIso      XNmDat
109100141229     C                   Move      UDateIso      XNmDag
109200141229     C*
109300141229     C                   CallB     'XNMR'
109400141229     C                   Parm      '2'           XNmRic            1
109500141229     C                   Parm      XScSoc        XNmSoc            3
109600141229     C                   Parm      *Blank        XNmUni            8
109700141229     C                   Parm      *Blank        XNmCtb            2
109800141229     C                   Parm      *Blank        XNmKey            8
109900141229     C                   Parm      'DICF'        XNmSer            4
110000150116     C                   Parm      *On           XNmCmt            1
110100141229     C                   Parm                    XNmDat           10
110200141229     C                   Parm      *Off          XNmIVA            1
110300141229     C                   Parm      *Off          XNmReg            1
110400141229     C                   Parm                    XNmErr            1
110500141229     C                   Parm                    XNmNum            9 0
110600141229     C                   Parm                    XNmAut            1
110700141229     C                   Parm                    XNmDag           10
110800141229     C*
110900141229     C* se viene rilevato un errore
111000141229     C                   if        XNmErr      = '1'      or
111100141229     C                             XNmErr      = '2'      or
111200141229     C                             XNmErr      = '7'
111300141229     C                   SetOn                                        99
111400141229     C                   else
111500141229     C                   move      xnmnum        $numero4
111600141229     C                   endif
111700141229     C*
111800141229     C                   ENDSR
111900141229     C/EJECT
112000050422     C************************************************************
112100050422     C* OPERAZIONI INIZIALI
112200050422     C************************************************************
112300050422     C     *INZSR        BEGSR
112400050422     C*
112500050422     C* Reperimento parametri
112600050422     C*
112700050422     C     *ENTRY        PLIST                                                  *
112800050422     C                   PARM                    KPJBA                          *
112900150115     C*
113000150115     C* eseguo lo strcmtctl
113100150115     C                   callb     'XSTRCMT'
113200150115     C*
113300150115     C* apro i file
113400150115     C                   open      SDGDIF01L
113500050422     C*
113600050427     C     *DMY          Move      Udate         UDateISO
113700050422     C*
113800050422     C                   ENDSR
113900050422      /EJECT
114000141222     C************************************************************
114100141222     C* Reperimento dati società
114200141222     C************************************************************
114300141222     C     RepSoc        BEGSR
114400141222     C*
114500141222     C                   CALLB     'XSOC'
114600141222     C                   PARM                    TIPXSC            6
114700141222     C                   PARM                    SOCXSC            3
114800141222     C                   PARM                    CDSXSC            9 0
114900141222     C                   PARM                    MODXSC            3
115000141222     C                   PARM      *Blanks       RTNXSC            1
115100141222     C                   PARM                    XSOCDS
115200141222     C                   PARM                    KPJBA
115300141222     C*
115400141222     C                   ENDSR
115500141222      /EJECT
115600050422     C************************************************************
115700050422     C* DEFINIZIONE KLIST
115800050422     C************************************************************
115900050422     C     DefKlist      BEGSR
116000050422     C*
116100050422     C* KLIST
116200141222     C*
116300141222     C* SDGDIE01L
116400141222     C     K01Die01      KLIST
116500141222     C                   KFLD                    DieSocieta
116600050422     C*
116700141222     C* SDGDIF01L
116800141222     C     K04Dif01      KLIST
116900141222     C                   KFLD                    DifSocieta
117000141222     C                   KFLD                    DifCf
117100141222     C                   KFLD                    DifDtInter
117200141222     C                   KFLD                    DifNrInter
117300141222     C*
117400141222     C* SDGSo301L
117401160530R224 X**** K04SO301      KLIST
117402160530R224 C     K05SO301      KLIST
117600141222     C                   KFLD                    SO3Sogg
117700141222     C                   KFLD                    SO3DtFiVL
117800141222     C                   KFLD                    SO3Sys
117900141222     C                   KFLD                    SO3NrAsint
117901160530R224 C                   KFLD                    SO3Dest
118000050422     C*
118100050422     C                   ENDSR
118200050422     C/EJECT
118300050422     C************************************************************
118400050422     C* FINE PROGRAMMA
118500050422     C************************************************************
118600050422     C     ENDPGM        BEGSR
118700150116     C*
118800150116     C*   Faccio sempre ROLBK
118900150116     C* (se la creazione del telematico è andata a buon fine, ho già fatto COMMIT)
119000150116     C                   ROLBK
119100141229     C*
119200141229     C                   if        $Errori     = *On
119300141229     C                   Eval      ERRJ61      = '1'
119400141229     C                   endif
119500141229     C*
119600150107     C                   eval      FileTelJ61  = $File
119700150107     C*
119800141222    >C                   MoveL     SdgSJ61DS     KPJBU
119900050422     C                   SETON                                        LR
120000050422     C*
120100050422     C                   RETURN
120200050422     C*
120300050422     C                   ENDSR
120400050422     C/EJECT
120500150115     C******************************************************
120600150115     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
120700150115     C******************************************************
120800150115     C     MSGPGM        BEGSR
120900150115     C*
121000150115     C                   Z-ADD     2             MSGMSG
121100150115     C                   MoveL     'S'           MSGRSP
121200150115     C                   MoveL     'CR'          MSGTPR
121300150115     C                   MoveL     'S'           MSGEMV
121400150115     C                   Z-ADD     10            MSGRGP
121500150115     C                   MoveL     'S'           MSGLCK
121600150115     C                   MoveA     STATUS        STS
121700150115     C                   MoveL     'N'           MSGCNL
121800150115     C                   MoveL     'N'           MSGVID
121900150115     C                   MoveL     'N'           MSGOPE
122000150115     C                   MoveL     'N'           MSGSTP
122100150115     C                   CALL      'SDGMSGR'
122200150115     C                   PARM                    SDGMSGDS
122300150115     C*
122400150115     C                   ENDSR
122500150115     C************************************************************
122600150115     C* Messaggio errore
122700150115     C************************************************************
122800150115     C     MsgErr        BegSr
122900150115     C*
123000150115     C                   Reset                   SDGMSGDS
123100150115     C*
123200150115     C* Risposte possibili
123300150115     C* "Annulla
123400150115     C                   Eval      IDMSG       =
123500150115     C                             'PROMSG    *LIBL     COS1213'
123600150115     C                   CALLB     'XRTVM'
123700150115     C                   PARM                    IDMSG            27
123800150115     C                   PARM      *Blank        TXTMSG          100
123900150115     C                   PARM                    ERRMSG            1
124000150115     C     ERRMSG        IFNE      *ON
124100150115     C                   MoveL     TXTMSG        XMSGTXT
124200150115     C                   ELSE
124300150115     C                   MoveL     *ALL'?'       XMSGTXT
124400150115     C                   ENDIf
124500150115     C*
124600150115     C                   MoveL     'S'           MSGEMV
124700150115     C                   Z-Add     10            MSGRGP
124800150115     C                   MoveL     'S'           MSGRSP
124900150115     C                   MoveL     '1'           MSGTPR
125000150115      *
125100150115     C                   call      'SDGMSGR'
125200150115     C                   parm                    SDGMSGDS
125300150115     C                   parm                    XMsgTxt         135
125400150115     C*
125500150115     C* Modo di inizializzazione della window:
125600150115     C* ('1'->Close + Open  -->Iø volta dopo l'emissione del video princip.)
125700150115     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
125800150115     C*                       per mantenere il sottofondo del video princip.)
125900150115     C                   parm      '1'           XMsgInz           1
126000150115     C*
126100150115      * SDC0083 - Si è deciso di interrompere l'elaborazione. E' necessario
126200150115      *           rilanciare la creazione del telematico.
126300150115     C                   parm      'SDC0083'     XMsgCd1           7
126400150115     C                   parm                    XMsgCd2           7
126500150115     C                   parm                    XMsgCd3           7
126600150115     C                   parm                    XMsgSn1           7
126700150115     C                   parm                    XMsgCdTxT        63
126800150115     C                   parm                    MsgCdTit         14
126900150115     C                   parm                    MsgClPrtf         1
127000150115     C                   parm                    VarMsg1         100
127100150115     C                   parm                    VarMsg2         100
127200150115     C                   parm                    VarMsg3         100
127300150115     C*
127400150115     C                   ENDSR
