000100110721     H*BIND
000200110721     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300110721     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400110721      *
000500110721     F*----------------------------------------------------*
000600110721      *
000601131014      *  Verifica raggiungimento soglia per Margine
000602131014      *
000700131014      * Annullo i movimenti del Regime del Margine se
000701131014      * per gli anni 2012 e 2013 il documento non è almeno
000702131014      * la soglia di 3600
000800110722      *
002100110826      *
002200110721     F*----------------------------------------------------*
002300131014     F* Movimenti estratti
002700131014     F* Key: TpCom/Anno/Mese/Sys/NrAsReg/NrRigaM/NrRig   UNIQUE
002800131014     FSDGSCF11L UF A E           K DISK    UsrOpn
002900110721     F                                     COMMIT
004700110721     F*------------
004800131014     FSDGSG52P1 O    E             PRINTER OFLIND(*In41)
004900110721     F                                     USROPN
005000110721     D*----------------------------------------------------*
005100110721     D* Parametri per apertura file di stampa
005200131014     D SDG00500DS    E DS
005300110721     D*-------------
005400110721     D* Passaggio Parametri
005500110721     D KPJBA         E DS
005600110721     D*-------------
005700131014     D SDG00507DS    E DS
006600110721     D*---------
006700110721     D* Richiamo a SDGTABDR
006800110721     D SDGTABDS      E DS
006900110721     D*---------
007000110721     D* Contabilità
007100110721     D ANGG84DS      E DS                  INZ
007101151123R195 D*-------------
007102151123 "   D* Tabella degli Stati
007103151123 "   D ANGG12DS      E DS
007200160307R195 D*------------------
007300110721     D* Controlli validità legende
007400110721     D X07ValDS      E DS
007500110721     D*-------------
007600110721     D* Dati di societa
007700110721     D SOC001        E DS                  EXTNAME(SDGSOCDS)
007800110721     D XSOCDS          DS          1000
008300110721     D*-------------
008400110721     D* Reperimento nome PGM
008500110721     D STATUS         SDS           333
008600110721     D  DSPGM            *PROC
008700110721     D  PARMS            *PARMS
008800110721     D*-------------
008900110721     D* DS Esterna gestione messaggi
009000110721     D XMSGDS        E DS                  Inz
009100110721     D  STS          E                     EXTFLD(MSGSTS)
009200110721     D                                     DIM(333)
009300110721     D  JBA          E                     EXTFLD(MSGJBA)
009400110721     D                                     DIM(502)
009500110721     D*---------
009600110721     D* Definizioni parametri standard chiamate moduli
009700110721     D $Esito          S              1
009800110721     D $GesMsg         S              1
009900110721     D $LenOut         S              9B 0
010000110721     D $Struttura      S             10
010100110721     D $CodOper        S              1
010200110721     D $Output         S           4000
010300110721     D $Refresh        S              1
010400110721     D $Uso            S              1
010500110721     D*-------------
010600110721     D* Salvataggio Kpjbu per richiamo altri pgm
010700110721     D SavKPJBU        S            256
010800110721     D*-------------
010900110721     D* Campi di lavoro
010901131014     D $Totale         S                   Like(SCFImpOpe)
010902131014     D $FoundSCF       S              1A
010903131014     D SavSys          S                   Like(SCFSys   )
010904131014     D SavNrAsReg      S                   Like(SCFNrAsReg)
010905131014     D Sav2Sys         S                   Like(SCFSys   )
010906131014     D Sav2NrAsReg     S                   Like(SCFNrAsReg)
011000110721     D UDateISO        S               D
011100131014     D $FineSCF        S              1A
011101131014     D $OkSCF          S              1A
011500110721     D $PrimaStp1      S              1    INZ(*On)
011600131014     D $TpAnn          S                   Like(SCFTpAnn) INZ(*Blank)
011700110721     D $Data           S               D
011800110721     D WSogg           S                   Like(DVASogg)
011900110721     D WKsc            S                   Like(DVACodice)
012000131014     D WKcc            S                   Like(SCFKcc)
012100110721     D WTpInd          S                   Like(DVATpInd)
012200110721     D WCdInd          S                   Like(DVACdInd)
012300110721     D WSNatura        S                   Like(DVASNatura)
012800110721     D*---------
012900110721     D* dati conto
013000110721     D ND002DS       E DS                  INZ
013100111020     D*---------
013200111020     D DataDs          DS
013300111020     D  AnnoDs                        4A
013400111020     D   Tratt1                       1A   Inz('-')
013500111020     D  MeseDs                        2A
013600111020     D   Tratt2                       1A   Inz('-')
013700111020     D  GiornoDs                      2A
014400110721     D*---------
014500110721     D* Dati soggetti
014600110721     D DVASog        E DS
014700110721     D*---------
014800110721     D* driver soggetti
014900110721     D DVARic          S              1
015000110721     D DVASocieta      S              3
015100110721     D DVAUnita        S              8
015200110721     D DVADtRif        S               D
015300110721     D DVAStrutt       S             10
015400110721     D DVALenOut       S              9B 0
015500110721     D DVASnatura      S              1
015600110721     D DVACodice       S              8
015700110721     D DVALineav       S              3
015800110721     D DVAFiliale      S              3  0
015900110721     D DVASogg         S              8
016000110721     D DVATpInd        S              2
016100110721     D DVACdInd        S              3
016200110721     D DVAOutput       S           4000
016300110721     D DVAErrore       S              1
016400110721     D*---------
016500131014     D* per prtf SDGSG52P1 - errori
016600110721     D $TpErr1         S              2A   Inz
016700110721     D*-------------
017200110831     D $PrimaVolta     S              1A
017201151202R195 D*-------------
017202151202  "  D* "Black List"
017203151202  "  D SOTTITTE        C                   CONST('SDGMSG    *LIBL-
017204151202R195 D                                          SDC0087')
018300110721     C*************************************************************
018400110721     C*
018500110721     C                   EXSR      INZVAR
018600110721     C*
018700110721     C                   CALLB     'XSTRCMT'
018800131014     C                   if        NOT %open (SDGSCF11L)
018900131014     C                   OPEN      SDGSCF11L
019000110826     C                   endif
019100110721     C*
019200131014     C* Leggo i soli movimenti Attivi (non annullati),
019300131014     C* Automatici e con Regime del Margine.
019700131014     C* Per ogni registrazione di Regime del Margine
019800131014     C* verifico se superata la soglia di 3600 euro
020100131014     C                   EXSR      RepReg
020400110721     C*
020500110721     C                   EXSR      ENDPGM
020600110721     C/EJECT
020700110722     C************************************************************
020800131014     C* Reperimento registrazioni
020900110722     C************************************************************
021000131014     C     RepReg        BEGSR
021004131014     C*
021005131014     C                   Clear                   SAVSys
021006131014     C                   Clear                   SAVNrAsReg
021007131014     C*
021008131014     C* azzero il totale di registrazione
021009131014     C                   Z-Add     0             $Totale
021010131014     C*
021011131014     C                   Eval      $FoundSCF   = *Off
021100110722     C*
021200110722     C                   EXSR      SETANA
021300110722     C                   EXSR      REDANA
021400110722     C*
021500131014     C                   DOW       $FineSCF    = *Off
021600110722     C*
021900110818     C                   EXSR      GesRighe
022000110722     C*
022100110722     C                   EXSR      REDANA
022200110722     C                   ENDDO
022201131014     C*
022202131014     C                   if        $FoundSCF   = *On
022203131014     C                   EXSR      CtrSoglia
022204131014     C                   endif
022300110722     C*
022400110722     C                   ENDSR
022500110722     C/EJECT
022600110722     C************************************************************
022700110722     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
022800110722     C************************************************************
022900110722     C     SETANA        BEGSR
023000110722     C*
023002131014     C                   Eval      SCFTpCom    = TpCom0507
023003131014     C                   Eval      SCFAnno     = Anno0507
023004131014     C                   Eval      SCFMese     = Mese0507
023006131014     C     K03SCF11      SETLL     SDGSCF11L                              21
030700110722     C*
030800110722     C                   ENDSR
030900110722     C/EJECT
031000110722     C************************************************************
031100110722     C* LETTURA RCD ARCHIVIO PILOTA
031200110722     C************************************************************
031300110722     C     REDANA        BEGSR
031400110722     C*
031401131014     C     K03SCF11      READE     SDGSCF11L                              21
031404131014     C                   Eval      $FineSCF    = *In21
033600110722     C*
033700110722     C                   ENDSR
033800110722     C/EJECT
033801131014     C************************************************************
033802131014     C* LETTURA RCD ARCHIVIO PILOTA
033803131014     C************************************************************
033804131014     C     GesRighe      BEGSR
033828131014     C*
033829131014     C                   if        $FoundSCF   = *On
033830131014     C                   EXSR      CtrSoglia
033831131014     C                   endif
033832131014     C*
033835131014     C                   if        (SCFSys     <> SAVSys     or
033836131014     C                              SCFNrAsReg <> SAVNrAsReg   )
033837131014     C                   Eval      $FoundSCF   = *Off
033838131014     C                   MoveL     SCFSys        SAVSys
033839131014     C                   MoveL     SCFNrAsReg    SAVNrAsReg
033840131014     C                   endif
033842131014     C*
033843131014     C                   EXSR      SELANA
033844131014     C                   if        $OkSCF      = *On
033845131014     C*
033846131014     C                   Eval      $FoundSCF   = *On
033847131014     C* sommo gli importi relativi ad una stessa registrazione
033850131014     C                   Add       SCFImpOpe     $Totale
033851131014     C                   Add       SCFIVAOpe     $Totale
033852131014     C*
033853131014     C                   EXSR      UpdSCF
033861131014     C*
033862131014     C                   endif
033863131014     C*
033864131014     C                   ENDSR
033865131014     C/EJECT
035500110721     C************************************************************
035600131014     C* Controllo soglia
035700110721     C************************************************************
035800131014     C     CtrSoglia     BEGSR
035801131014     C*
035802131014     C* controllo soglia a cambio di registrazione confermo o meno
035803131014     C* l'annullamento per mancato raggiungimento soglia
035804131014     C                   if        (SavSys     <> 0          or
035805131014     C                              SavNrAsReg <> 0            ) and
035806131014     C                             (SCFSys     <> SAVSys     or
035807131014     C                              SCFNrAsReg <> SAVNrAsReg   )
035808131014     C*
035809131014     C* salvo i riferimenti della nuova registrazione, serviranno
035810131014     C* per riposizionarmi dopo la conferma o meno dell'annullamento
035811131014     C                   MoveL     SCFSys        SAV2Sys
035812131014     C                   MoveL     SCFNrAsReg    SAV2NrAsReg
036800110817     C*
036801131014     C                   if        $Totale    >= 3600
036802131014     C* non confermo gli annullamenti perchè la soglia è stata raggiunta
036803131014     C                   ROLBK
036804131014     C                   else
036816131014     C*
036817131014     C* confermo gli annullamenti per mancato raggiungimento soglia
036818131014     C                   COMMIT
036819131014     C                   FEOD      SDGSCF11L
036820131014     C*
036822131014     C* mi riposiziono sull'ultima riga della vecchia registrazione
036823131014     C* mi servono gli estremi per stampare l'avvertimento
036824131014     C                   Eval      SCFSys      = SavSys
036825131014     C                   Eval      SCFNrAsReg  = SavNrAsReg
036826131014     C     K05SCF11      SETGT     SDGSCF11L
036827131014     C     K05SCF11      READPE    SDGSCF11L                              21
036828131014     C* stampo avvertimento
036829131014     C                   Eval      $TpAnn      = '52'
036830131014     C                   EXSR      StpErr1
036831131014     C                   endif
036832131014     C*
036833131014     C* mi riposiziono sulla prima riga della nuova registrazione
036834131014     C* Serve anche per attivare il commit già dalla prima riga
036835131014     C                   Eval      SCFSys      = Sav2Sys
036836131014     C                   Eval      SCFNrAsReg  = Sav2NrAsReg
036837131014     C     K05SCF11      SETLL     SDGSCF11L                              21
036838131014     C     K05SCF11      READE     SDGSCF11L                              21
036839131014     C*
036840131014     C* azzero il totale per la registrazione successiva
036841131014     C                   Z-Add     0             $Totale
036845131014     C                   endif
036846131014     C*
036900110817     C                   ENDSR
037000110817     C/EJECT
037001131014     C************************************************************
037002131014     C* SELEZIONI SULLA RIGA LETTA
037003131014     C************************************************************
037004131014     C     SELANA        BEGSR
037005131014     C*
037006131014     C                   MOVEL     *ON           $OkSCF
037007131014     C*
037008131014     C                   SELECT
037009131014     C*
037010131014     C                   When      SCFAnn      = *On
037011131014     C                   Eval      $OkSCF      = *Off
037012131014     C*
037013131014     C                   When      SCFOrigine <> 'A'
037014131014     C                   Eval      $OkSCF      = *Off
037015131014     C*
037017131014     C                   When      SCFMargine <> '1'
037018131014     C                   Eval      $OkSCF      = *Off
037019151123R195 C*
037020151123 "   C                   When      SCFStato   <> Stato0507    and
037021151123 "   C                             Stato0507  <> *Blank
037022151123R195 C                   Eval      $OkSCF      = *Off
037023131014     C*
037024131014     C                   When      SCFCF      <> CF0507       and
037025131014     C                             CF0507     <> *Blank
037026131014     C                   Eval      $OkSCF      = *Off
037027131014     C*
037028131014     C                   When      SCFPartIVA <> PIVA0507     and
037029131014     C                             PIVA0507   <> *Blank
037030131014     C                   Eval      $OkSCF      = *Off
037031131014     C*
037032131014     C                   When      SCFCdFisc  <> CdFisc0507   and
037033131014     C                             CdFisc0507 <> *Blank
037034131014     C                   Eval      $OkSCF      = *Off
037035131014     C*
037036131014     C                   When      SCFSogg    <> Sogg0507     and
037037131014     C                             Sogg0507   <> *Blank
037038131014     C                   Eval      $OkSCF      = *Off
037039131014     C*
037040131014     C                   When      SCFKcc     <> Kcc10507     and
037041131014     C                             Kcc10507   <> *Blank       and
037042131014     C                             SCFKcc     <> Kcc20507     and
037043131014     C                             Kcc20507   <> *Blank       and
037044131014     C                             SCFKcc     <> Kcc30507     and
037045131014     C                             Kcc30507   <> *Blank       and
037046131014     C                             SCFKcc     <> Kcc40507     and
037047131014     C                             Kcc40507   <> *Blank       and
037048131014     C                             SCFKcc     <> Kcc50507     and
037049131014     C                             Kcc50507   <> *Blank       and
037050131014     C                             SCFKcc     <> Kcc60507     and
037051131014     C                             Kcc60507   <> *Blank
037052131014     C                   Eval      $OkSCF      = *Off
037053131014     C*
037054131014     C                   When      SCFKcc     <> Kcc0507      and
037055131014     C                             Kcc0507    <> *Blank       and
037056131014     C                             SCFKsc     <> Ksc0507      and
037057131014     C                             Ksc0507    <> *Blank
037058131014     C                   Eval      $OkSCF      = *Off
037059131014     C*
037060131014     C                   ENDSL
037061131014     C*
037062131014     C                   ENDSR
037063131014     C/EJECT
037064131014     C************************************************************
037065131014     C* Aggiornamento RIGA LETTA
037066131014     C************************************************************
037067131014     C     UpdSCF        BEGSR
037068131014     C*
037069131014     C* imposto preventivamente che il movimento è annullato,
037070131014     C* se supererà la soglia non confermerò l'annullamento
037071131014     C                   Eval      SCFAnn      = '1'
037072131014     C* 'Margine 2012/2013: totale documento < 3600 euro
037073131014     C                   Eval      SCFTpAnn    = '52'
037074131014     C*
037075131014     C                   UPDATE    SDGSCF000
037076131014     C*
037077131014     C                   ENDSR
037078131014     C/EJECT
115400110721     C************************************************************
115500110721     C* INIZIALIZZAZIONE VARIABILI
115600110721     C************************************************************
115700110721     C     INZVAR        BEGSR
115800110721     C*
115900131014     C                   MoveL     KPJBU         SDG00507DS
116000131014     C                   Eval      Err0507     = *Off
116100110721     C*
116200110721     C* Reperimento dati società
116300131014     C                   MoveL     Soc0507       SOCXSC
116400110721     C                   EXSR      REPSOC
116500110721     C*
116600131014     C                   if        Anno0507    = 0
116700131014     C                   Eval      Err0507     = '2'
116800110721     C                   EXSR      ENDPGM
116900110721     C                   endif
117000110721     C*
117100110721     C* Se non è già aperto, apro il printer file e faccio OVR
117200110721     C                   EXSR      OpnPrtf
117300110721     C*
117400110721     C                   ENDSR
117500110721     C/EJECT
117600110721     C************************************************************
117700110721     C* REPERIMENTO DATI SOCIETA'
117800110721     C************************************************************
117900110721     C     REPSOC        BEGSR
118000110721     C*
118100110721     C                   MoveL     'SOC001'      TIPXSC
118200110721     C                   CALL      'SDGSOCDR'
118300110721     C                   PARM                    TIPXSC            6
118400110721     C                   PARM                    SOCXSC            3
118500110721     C                   PARM                    CDSXSC            9 0
118600110721     C                   PARM                    MODXSC            3
118700110721     C                   PARM      *BLANKS       RTNXSC            1
118800110721     C                   PARM                    XSOCDS
118900110721     C                   PARM                    KPJBA
119000110721     C*
119100110721     C     RTNXSC        IFNE      '1'
119200110721     C                   MoveL     XSOCDS        SOC001
119300110721     C                   else
119400131014     C                   Eval      Err0507     = '1'
119500110721     C                   EXSR      ENDPGM
119600110721     C                   endif
119700110721     C*
119800110721     C                   ENDSR
119900110721     C/EJECT
120000110721     C************************************************************
120100110721     C* Apertura file si stampa
120200110721     C************************************************************
120300110721     C     OpnPrtf       BEGSR
120400110721     C*
120500110721     C* imposto il nome del pgm da mettere in stampa
120600131014     C                   MOVEL     'SDG00500R'   NOMPGM
120700110721     C*-------------------
120800110721     C* valorizzo i dati di testata comuni a tutte le stampe
120900110721     C                   MoveL     KNSIF         NOMSIF
121000110721     C                   MoveL     XSCDSI        NOMDIT
121100110721     C**************     MoveL     DSPGM         NOMPGM
121200110721     C                   CALL      'XNETA'                              29
121300110721     C                   PARM                    NOMSYS
121301151202R195 C* Recupero il titolo con il tipo di comunicazione
121302151202 "   C                   MOVEL     *BLANK        SOTTITT
121303151202 "   C*
121304151202 "   C                   MOVEL     SOTTITTE      IDMSG
121305151202 "   C*
121306151202 "   C                   CALLB     'XRTVM'
121307151202 "   C                   PARM                    IDMSG            27
121308151202 "   C                   PARM                    TXTMSG          100
121309151202 "   C                   PARM                    ERRMSG            1
121310151202 "   C                   PARM      *ON           CTRMSG            1            centratura
121311151202 "   C                   PARM      30            LENMSG            3 0          lun output.
121312151202 "   C     ERRMSG        IFNE      *ON
121313151202 "   C                   MOVEL     TXTMSG        SOTTITT
121314151202 "   C                   ELSE
121315151202 "   C                   MOVEL     *ALL'?'       SOTTITT
121316151202 "   C                   ENDIF
121317151202R195 C*
121400110721     C*
121500110721     C                   EXSR      RieParam
121600110721     C*
121700131014     C                   if        NOT %open(SDGSG52P1)
121800110721     C*
121900110721     C* salvo la kpjbu per ripristinarla dopo il richiamo
122000110721     C                   MoveL     KPJBU         SavKPJBU
122100110721     C*
122200110721     C* eseguo ovrprtf in *Share
122300131014     C                   Reset                   SDG00500DS
122400131014     C                   Eval      KPJBU       = SDG00500DS
122500110721     C*
122600131014     C                   CallB     'SDG00500R'
122700110721     C                   PARM                    KPJBA
122800110721     C*
122900131014     C                   MoveL     KPJBU         SDG00500DS
123000110721     C*
123100131014     C                   if        Esito0500   = *On
123200110721     C*
123300110721     C* Fallito l'ovrprtf del file di spool
123400110721     C* apro il prtf per fare la stampa dell'errore
123500131014     C                   OPEN      SDGSG52P1
123600110721     C*
123700110721     C                   else
123800131014     C                   OPEN      SDGSG52P1
123900110721     C                   endif
124000110721     C*
124100110721     C* ripristino la kpjbu per non "turbare" i richiami successivi
124200110721     C* ad eventuali altri pgm che ricevono la kpjba
124300110721     C                   MoveL     SavKPJBU      KPJBU
124400110721     C*
124500110721     C                   endif
124600110721     C*
124700110721     C                   EndSr
124800110721     C/EJECT
124900110721     C************************************************************
125000110721     C* FINE PROGRAMMA
125100110721     C************************************************************
125200110721     C     ENDPGM        BEGSR
125300110721     C*
125400131014     C                   if        ERR0507     =  *On
125500110721     C                   ROLBK
125600110721     C                   else
125700110721     C                   COMMIT
125800110721     C                   endif
125900110721     C*
126000131014     C                   if        %open (SDGSCF11L)
126100131014     C                   FEOD      SDGSCF11L
126200110721     C                   endif
126300110721     C*
126400131014     C                   EVAL      KPJBU       =  SDG00507DS
126500110721     C*
126600131014     C                   if        Lr0507 = *on
126700131014     C                   if        %open (SDGSG52P1)
126800131014     C                   Close     SDGSG52P1
126900110721     C                   endif
127000110721     C                   SetOn                                        LR
127100110721     C                   else
127200110721     C                   SetOn                                        RT
127300110721     C                   endif
127400110721     C                   Return
127500110721     C*
127600110721     C                   ENDSR
127700110721     C/EJECT
127800110721     C************************************************************
127900110721     C* Imposto i parametri di elaborazione
128000110721     C************************************************************
128100110721     C     RieParam      BEGSR
128101160201R195 C*
128102160201 "   C                   Select
128103160201 "   C* Acquisti da San Marino
128104160201 "   C                   When      TpCom0507   = 'S'
128105160201 "   C                   Eval      *In11       = *On
128106160201 "   C                   Other
128107160201 "   C* Spesometro  o
128108160201 "   C* Acquisti da San Marino
128109160201 "   C                   Eval      *In11       = *Off
128110160201 "   C                   endSl
128111160201R195 C*
128200110721     C*
128300131014     C                   Eval      PACtb       = Ctb0507
128400110721     C                   EXSR      RepCtbD
128500110721     C*
128600131014     C                   Eval      PATpRegZ    = TpRegz0507
128700110721     C                   EXSR      RepTpRegzD
128800110721     C*
128900131014     C                   Eval      PAAnno      = Anno0507
128901160201R195 C                   Eval      PAMese      = Mese0507
128902160201 "   C*
128903151123 "   C                   Eval      PAStato     = Stato0507
128904151123R195 C                   EXSR      RepStatoD
129600110721     C*
129700131014     C                   Eval      PACF        = CF0507
129800110721     C                   EXSR      RepCFD
129900110721     C*
130000131014     C                   Eval      PAPartIVA   = PIVA0507
130100131014     C                   Eval      PACdFisc    = CdFisc0507
130200110721     C*
130300131014     C                   Eval      PASogg      = Sogg0507
130400110721     C                   EXSR      RepSoggD
130500110721     C*
130600131014     C                   Eval      PAKcc1      = Kcc10507
130700131014     C                   Eval      PAKcc2      = Kcc20507
130800131014     C                   Eval      PAKcc3      = Kcc30507
130900131014     C                   Eval      PAKcc4      = Kcc40507
131000131014     C                   Eval      PAKcc5      = Kcc50507
131100131014     C                   Eval      PAKcc6      = Kcc60507
131200110721     C*
131300131014     C                   Eval      PAKsc       = Ksc0507
131400110721     C                   EXSR      RepKscD
131900110721     C*
132000110721     C                   Eval      PAOn1       = XScOn
132100110721     C                   Eval      PAOff1      = XScOff
132200131014     C                   if        DelMan0507  = *On
132300110721     C                   Eval      PADelManu   = XScOn
132400110721     C                   else
132500110721     C                   Eval      PADelManu   = XScOff
132600110721     C                   endif
132700110721     C*
133100110721     C                   Eval      PAOn2       = XScOn
133200110721     C                   Eval      PAOff2      = XScOff
133300131014     C                   if        StpAnn0507  = *On
133400110721     C                   Eval      PAStpAnn    = XScOn
133500110721     C                   else
133600110721     C                   Eval      PAStpAnn    = XScOff
133700110721     C                   endif
133800110721     C*
133900110721     C                   ENDSR
134000110721     C/EJECT
134100110721     C************************************************************
134200110721     C* Reperisco la descrizione della contabilità
134300110721     C************************************************************
134400110721     C     RepCtbD       BEGSR
134500110721     C*
134600110721     C                   Clear                   SDGTABDS
134700110721     C*
134800110721     C                   Eval      XTbRic     =  '1'
134900110721     C                   Eval      XTbAzi     =  XScSoc
135000110721     C                   Eval      XTbKey     =  *Zero
135100110721     C                   Eval      XTbCod     =  'G84'
135200131014     C                   Move      Ctb0507       XTbKey
135300110721     C*
135400110721     C                   CALL      'SDGTABDR'
135500110721     C                   PARM                    SDGTABDS
135600110721     C*
135700110721     C                   if        XTbErr      = '0'
135800110721     C                   MoveL     XTbUni        ANGG84DS
135900110721     C                   MoveL     DsBG84        PACtbD
136000110721     C                   else
136100110721     C                   MoveL     *All'?'       PACtbD
136200110721     C                   endif
136300110721     C*
136400110721     C                   ENDSR
136500110721     C/EJECT
136600110721     C************************************************************
136700110721     C* Reperisco la descrizione del tipo registrazione
136800110721     C************************************************************
136900110721     C     RepTpRegzD    BEGSR
137000110721     C*
137100110721     C                   Clear                   X07VALDS
137200110721     C*
137300110721     C                   Move      '1'           X07Ric
137400110721     C                   MoveL     'ANDIZ000'    X07Trc
137500110721     C                   MoveL     'DSPTPREGZ '  X07Cam
137600131014     C                   MoveL     TpRegz0507    X07Val
137700110721     C*
137800110721     C                   CALLB     'X07VALR'
137900110721     C                   PARM                    X07VALDS
138000110721     C*
138100110721     C                   if        X07ERR      = '0'
138200110721     C                   MoveL     X07Des        PATpRegzD
138300110721     C                   else
138400110721     C                   MoveL     *All'?'       PATpRegzD
138500110721     C                   endif
138600110721     C*
138700110721     C                   ENDSR
138800110721     C/EJECT
138801151123R195 C************************************************************
138802151123 "   C* Reperisco la descrizione dello Stato
138803151123 "   C************************************************************
138804151123R195 C     RepStatoD     BEGSR
138805151123     C*
138806151123     C                   Clear                   SDGTABDS
138807151123     C                   Clear                   AngG12DS
138809151123     C*
138810151123     C                   Eval      XTbRic     =  '1'
138811151123     C                   Eval      XTbAzi     =  XScSoc
138812151123     C                   Eval      XTbKey     =  *Zero
138820151123     C                   Eval      XTbCod      = 'G12'
138822151123     C*
138823151123     C                   Move      Stato0507     XTbKey
138824151123     C*
138825151123     C                   CALL      'SDGTABDR'
138826151123     C                   PARM                    SDGTABDS
138827151123     C*
138838151123     C                   MoveL     XTbUni        ANGG12DS
138839151123     C                   MoveL     DesG12        PAStatoD
138841151123     C*
138842151123R195 C                   ENDSR
138843151123R195 C/EJECT
138900110721     C************************************************************
139000110721     C* Reperisco la descrizione della natura del soggetto
139100110721     C************************************************************
139200110721     C     RepCFD        BEGSR
139300110721     C*
139400110721     C                   Clear                   X07VALDS
139500110721     C*
139600131014     C                   if        CF0507     <> *Blank
139700110721     C*
139800110721     C                   Move      '1'           X07Ric
139900110721     C                   MoveL     'ANDIZ000'    X07Trc
140000110721     C                   MoveL     'DSPCLIFOR '  X07Cam
140100131014     C                   MoveL     CF0507        X07Val
140200110721     C                   Move      *On           X07All
140300110721     C                   CALLB     'X07VALR'
140400110721     C                   PARM                    X07VALDS
140500110721     C*
140600110721     C                   if        X07Err      = '0'
140700110721     C                   MoveL     X07Des        PACFD
140800110721     C                   else
140900110721     C                   MoveL     *All'?'       PACFD
141000110721     C                   endif
141100110721     C*
141200110721     C                   endif
141300110721     C*
141400110721     C                   ENDSR
141500110721     C/EJECT
141600110721     C************************************************************
141700110721     C* Reperisco la descrizione del soggetto
141800110721     C************************************************************
141900110721     C     RepSoggD      BEGSR
142000110721     C*
142100110721     C                   Clear                   PASoggD
142200110721     C*
142300131014     C                   if        Sogg0507   <> *Blank
142400110721     C*
142500131014     C                   Eval      WSogg       = Sogg0507
142600110721     C                   Eval      WKsc        = *Blank
142700110721     C                   Eval      WKcc        = *Blank
142800110721     C                   Eval      WSNatura    = *Blank
142900110721     C                   Eval      WTpInd      = *Blank
143000110721     C                   Eval      WCdInd      = *Blank
143100110721     C                   MoveL     'DVASOG    '  DVAStrutt
143200110721     C                   Eval      DVALENOUT   = %size(DvaSog)
143300110721     C*
143400110721     C                   EXSR      CallDvaSog
143500110721     C*
143600110721     C                   if        DVAErrore  <> '1'
143700110721     C                   Eval      %subst(DVASOG:1:DVALENOUT)
143800110721     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
143900110721     C*
144000110721     C                   MoveL     DVSDes        PASoggD
144100110721     C                   else
144200110721     C                   MoveL     *All'?'       PASoggD
144300110721     C                   endif
144400110721     C*
144500110721     C                   endif
144600110721     C*
144700110721     C                   ENDSR
144800110721     C/EJECT
144900110721     C************************************************************
145000110721     C* Chiamata al driver dei soggetti
145100110721     C************************************************************
145200110721     C     CallDvaSog    BEGSR
145300110721     C*
145400110721     C                   CALLB     'NDDVASOG'
145500110721     C                   PARM      '1'           DVARic
145600110721     C                   PARM      XScSoc        DVASocieta
145700110721     C                   PARM      *Blank        DVAUnita
145800110721     C                   PARM                    DVAStrutt
145900110721     C                   PARM                    DVADtRif
146000110721     C                   PARM                    DVALenOut
146100110721     C                   PARM      WSNatura      DVASNatura
146200110721     C                   PARM      WKsc          DVACodice
146300110721     C                   PARM      *Blank        DVALineaV
146400110721     C                   PARM      *Zero         DVAFiliale
146500110721     C                   PARM      WSogg         DVASogg
146600110721     C                   PARM      WTpInd        DVATpInd
146700110721     C                   PARM      WCdInd        DVACdInd
146800110721     C                   PARM      *Off          DVAErrore
146900110721     C                   PARM                    DVAOutPut
147000110721     C*
147100110721     C                   ENDSR
147200110721     C/EJECT
147300110721     C************************************************************
147400110721     C* Reperisco la descrizione del conto
147500110721     C************************************************************
147600110721     C     RepKscD       BEGSR
147700110721     C*
147800110721     C                   Clear                   PAKscD
147900110721     C*
148000131014     C                   if        Kcc0507    <> *Blank   or
148100131014     C                             Ksc0507    <> *Blank
148200110721     C*
148300131014     C                   Eval      WKcc        = Kcc0507
148400131014     C                   Eval      WKsc        = Ksc0507
148500110721     C                   Eval      $Data       = UDateIso
148600110721     C*
148700110721     C                   EXSR      CallMVC002
148800110721     C*
148900110721     C                   if        $Esito     <> *On
149000110721     C                   MoveL     DesBrev002    PAKscD
149100110721     C                   else
149200110721     C                   MoveL     *All'?'       PAKscD
149300110721     C                   endif
149400110721     C*
149500110721     C                   endif
149600110721     C*
149700110721     C                   ENDSR
149800110721     C/EJECT
149900110721     C***********************************************************
150000110721     C* Driver per controllo conto
150100110721     C***********************************************************
150200110721     C     CallMvc002    BEGSR
150300110721     C*
150400110721     C                   Clear                   ND002DS
150500110721     C*
150600110721     C                   Eval      $LenOut    =  %Size(ND002DS)
150700110721     C*
150800110721     C                   CallB     'NDMVC002'
150900110721     C                   PARM                    XScSoc
151000110721     C                   PARM                    WKcc
151100110721     C                   PARM                    WKsc
151200110721     C                   PARM                    $Data
151300110721     C                   PARM      *Off          $GesMsg
151400110721     C                   PARM      *Off          $Esito
151500110721     C                   PARM      'ND002DS   '  $Struttura
151600110721     C                   PARM                    ND002DS
151700110721     C                   PARM                    $LenOut
151800110721     C*
151900110721     C                   ENDSR
152000110721     C/EJECT
154600110721     C******************************************************
154700131014     C* Preparo dati 'fissi' (SDGSG52P1)     (solo I° volta)
154800110721     C******************************************************
154900110721     C     StpErr1_Inz   BEGSR
155000110721     C*
155100110721     C* Overflow
155200110721     C                   if        *In41       = *On        or
155300110721     C                             $PrimaStp1  = *On
155400110721     C* Testata
155500131014     C                   WRITE     SG52T11
155600110721     C*
155700110721     C                   if        $PrimaStp1  = *On
155800110721     C* Parametri di elaborazione
155900131014     C                   WRITE     SG52PA1
156000110721     C                   endif
156100110721     C*
156200110721     C* Intestazione colonne
156300110721     C                   if        $TpErr1     = *Blank
156400131014     C                   WRITE     SG52T21
156500110721     C                   endif
156600110721     C*
156700110721     C                   Eval      *In41       = *Off
156800110721     C                   Eval      $PrimaStp1  = *Off
156900110721     C                   endif
157000110721     C*
157100110721     C                   ENDSR
157200110721     C/EJECT
157300110721     C************************************************************
157400131014     C* Stampa errori e avvertimenti (SDGSG52P1)
157500110721     C************************************************************
157600110721     C     StpErr1       BEGSR
157700110721     C*
157800110721     C                   EXSR      StpErr1_Inz
157900110721     C*
158000110721     C                   Select
158100110721     C*
158200110721     C* Stampa riga di avvertimento per movimento annullato.
158300110721     C                   When      $TpAnn     <> *Blank
158400110721     C*
158500131014     C                   if        StpAnn0507  = *On
158600110721     C                   Eval      AVVTpAnn    = $TpAnn
158700110721     C                   EXSR      RepTpAnnD
158800131014     C                   WRITE     SG52PAnn
158900110721     C                   EXSR      StpErr1_Dati
159000110721     C                   endif
159100110721     C*
159200110721     C*  ERRORE: elaborazione interrotta. Codice società di collegamento
159300110721     C*   non valido. Codice società =
159400110721     C                   When      $TpErr1     = '02'
159500131014     C                   Eval      P102Soc     = Soc0507
159600131014     C                   WRITE     SG52P102
159700110721     C*
159800110721     C                   OTHER
159900110721     C*  ERR . Avvertimento/errore non codificato.
160000131014     C                   WRITE     SG52P1XX
160100110721     C*
160200110721     C                   endSl
160300110721     C*
160400110721     C                   ENDSR
160500110721     C/EJECT
160600110721     C******************************************************
160700110721     C* Stampo i riferimenti del movimento
160800110721     C******************************************************
160900110721     C     StpErr1_Dati  BEGSR
161000110721     C*
161100131014     C                   Clear                   SG52DA1
161200110721     C*
161300131014     C                   MoveL     SCFKcc        DA1Kcc
161400131014     C                   MoveL     SCFKsc        DA1Ksc
161500110721     C*
161600131014     C                   Eval      WKcc        = SCFKcc
161700131014     C                   Eval      WKsc        = SCFKsc
161800131014     C                   Eval      $Data       = SCFDtDoc
161900110721     C                   EXSR      CallMVC002
162000110721     C                   MoveL     Sogg002       DA1Sogg
162100110721     C*
162200131014     C                   if        SCFDtDoc   <> *Loval
162300131014     C     *JobRun       Move      SCFDtDoc      DA1DtDoc
162400110721     C                   endif
162500131014     C                   Z-Add     SCFNrDoc      DA1NrDoc
162600131014     C                   MoveL     SCFSerDoc     DA1SerDoc
162700110721     C*
162800131014     C**************     MoveL     SCFCausale    DA1Causale
162900110721     C*
163000131014     C**************     if        SCFAliq     > 0
163100131014     C**************     SetOn                                        13
163200131014     C**************     Z-Add     SCFAliq       DA1Aliq
163300131014     C**************     else
163400131014     C**************     SetOff                                       13
163500131014     C**************     Move      SCFRifIva     DA1RifIVA
163600131014     C**************     endif
163700110721     C*
163800110721?????C**************     Z-Add     MOVImporto    DA1ImpDoc
163900110721     C*
164000131014     C                   Z-Add     SCFSys        DA1Sys
164100131014     C                   Z-Add     SCFNrAsReg    DA1NrAsReg
164200110721     C*
164300131014     C                   WRITE     SG52DA1
164400110721     C*
164500110721     C                   ENDSR
164600110721     C/EJECT
164700110721     C************************************************************
164800110721     C* Reperisco la descrizione del tipo annullamento
164900110721     C************************************************************
165000110721     C     RepTpAnnD     BEGSR
165100110721     C*
165200110721     C                   Clear                   X07VALDS
165300110721     C*
165400110721     C                   Move      '1'           X07Ric
165500131014     C                   MoveL     'SDGSCF000 '  X07Trc
165600131014     C                   MoveL     'SCFTPANN  '  X07Cam
165700110721     C                   MoveL     $TpAnn        X07Val
165800110721     C*
165900110721     C                   CALLB     'X07VALR'
166000110721     C                   PARM                    X07VALDS
166100110721     C*
166200110721     C                   if        X07ERR      = '0'
166300110721     C                   MoveL     X07Des        AVVTpAnnD
166400110721     C                   else
166500110721     C                   MoveL     *All'?'       AVVTpAnnD
166600110721     C                   endif
166700110721     C*
166800110721     C                   ENDSR
166900110721     C/EJECT
167000110721     C************************************************************
167100110721     C* DEFINIZIONE CAMPI :
167200110721     C************************************************************
167300110721     C     *INZSR        BEGSR
167400110721     C*
167500110721     C     *ENTRY        PLIST
167600110721     C                   PARM                    KPJBA
167700110721     C*
167800110721     C     *DMY          Move      UDate         UDateISO
167900110721     C*
168000110721     C                   ENDSR
168100110721     C/EJECT
168200110721     C************************************************************
168300110721     C* DEFINIZIONE KLIST
168400110721     C************************************************************
168500110721     C     DefKlist      BEGSR
168600110721     C*
168700110721     C* klist
168800110721     C*
168900131014     C* SDGSCF11L
169100131014     C     K03SCF11      KLIST
169200131014     C                   KFLD                    SCFTpCom
169300131014     C                   KFLD                    SCFAnno
169400131014     C                   KFLD                    SCFMese
177100110721     C*
177101131014     C     K05SCF11      KLIST
177102131014     C                   KFLD                    SCFTpCom
177103131014     C                   KFLD                    SCFAnno
177104131014     C                   KFLD                    SCFMese
177105131014     C                   KFLD                    SCFSys
177106131014     C                   KFLD                    SCFNrAsReg
177107131014     C*
177200110721     C                   ENDSR
177300110721     C/EJECT
