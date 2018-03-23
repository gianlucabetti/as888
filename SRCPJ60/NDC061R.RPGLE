000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940117      *
000500950705    > *            GESTIONE PARTICOLARITA' partite clienti/fornitori
000600071108      *           ---------------------------------------------------
000700941215     F*------------
000800951121    >FNDEFA07L  UF   E           K DISK
000900951122     F*------------
001000951122    >FNDEFP07L  UF   E           K DISK
001100951121     F*------------
001200951121    >FNDPPA01L  UF A E           K DISK    USROPN
001300950706     F                                     COMMIT(FLGCOM)
001400950705    >FNDPPS01L  UF A E           K DISK    USROPN
001500950705     F                                     COMMIT(FLGCOM)
001600000411D0525X***NDCPA01L  IF   E           K DISK
001700000411  "  FNdCpa01l  uf   e           k disk
001800000411D0525FNdMix01l  uf   e           k disk
001801170223R027 FSDGSO301L UF   E           K DISK
001900990218    >FANCDP01L  IF   E           K DISK
002000990218    >FNDPAS03L  IF   E           K DISK
002100960129     F*
002200960129    >FANUNI01L  IF   E           K DISK
002300960129    >F                                     RENAME(ANUNI000:ANUNI01)
002400941215     F*------------
002500950705    >FNDC061D   CF   E             WORKSTN
002600950705     F                                     RENAME(C061D1:FMTD1)
002700950705     F                                     RENAME(C061T1:FMTT1)
002800950705     F                                     RENAME(C061Z1:FMTZ1)
002900940510    >F* eventuali schermi supplementari
003000950705     F                                     RENAME(C061D2:FMTD2)
003100940201     F                                     INFDS(DSFMT)
003200941215     F*------------
003300940128     D*----------------------------------------------------*
003400950404     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003500950404     D $ID             S             27    DIM(5) ALT($OZ)
003600940506     D* Schiere per la gestione dei tasti di funzione
003700940506     D $FM             S             27    DIM(24)
003800940506     D $FX             S             50    DIM(24)
003900940506     D $FC             S              1    DIM(24)
004000940506     D $FR             S              1    DIM(24)
004100940506     D* Video D1
004200940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004300940506     D $FX1            S             50    DIM(24) ALT($FM1)
004400940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
004500940506     D $FR1            S              1    DIM(24) ALT($FC1)
004600940128     D*----------------------------------------------------*
004700940127     D*-------------
004800940127     D* Passaggio Parametri
004900940127     D KPJBA         E DS
005000960129     D*-------------
005100960129     D* Autorizzazioni alla unità
005200960129     D XAutUniDS     E DS
005300940318     D*-------------
005400960129     D* DS Esterna gestione messaggi
005500940318     D XMSGDS        E DS
005600940318     D  STS          E                     EXTFLD(MSGSTS)
005700940721     D                                     DIM(333)
005800940318     D  JBA          E                     EXTFLD(MSGJBA)
005900940721     D                                     DIM(502)
006000950301     D*-------------
006100950301     D* Reperimento Autorizzazioni
006200950301     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006300981109     D**Parametri gestione bonifici italia/estero
006400981109     D*NDCHH1        E DS
006500981109     d**salvataggio parametri
006600981109     D*NDCHH1ITA       S                   LIKE(NDCHH1)
006700981109   0 D*NDCHH1EST       S                   LIKE(NDCHH1)
006800981109      *-------------
006900981109      * Reperimento parametri BONIFICI
007000981109     D ANP039DS      E DS                  INZ
007100990810C1220 *-------------
007200990810  "   * Reperimento parametri NDPPS00F
007300990810C1220D ANP055DS      E DS                  INZ
007400970218     D*-------------
007500970218     D XPAOUT          DS          2000
007600951121     D*
007700981109     D*savitaest****** S              1
007800951121     D* Driver anbra
007900951121     D ND016DS       E DS
008000951121     D Esito           S              1
008100951121     D LenOut          S              9B 0
008200951121     D Struttura       S             10
008300951121     D Operazion       S              2
008400950301     D*-------------
008500950410     D* Passaggio Autorizzazioni
008600950410     D DSAUT           DS           512
008700950410     D*-------------
008800950301     D* Maschera dati autorizzazioni
008900950705    >D CONPPADS      E DS
009000040402     D*-------------
009100950126     D* Reperimento nome PGM
009200950126     D STATUS         SDS           333
009300950126     D  DSPGM            *PROC
009400950411     D  PARMS            *PARMS
009500941209     D*-------------
009600941209     D* Dati di ambiente ottenuti da XSOC
009700941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
009800960129     D    FgO1                        1    overlay(XscFgO)
009900941209     D*-------------
010000941209     D* DS Interna per dati di output di XSOC
010100941209     D XSOCDS          DS          1000
010200940203     D*-------------
010300940201     D* Passaggio Parametri al PGM di manutenzione
010400950705    >D NDC061DS      E DS
010401170223R224 X*ex R027 -------------
010402170223 "   X*ex R027 Parametri del PGM di manutenzione  dati aggiuntivi
010403170223 "   X*ex R027 SDGSD16DS     E DS                  inz
010404170223 "   X*ex R027  RETD16       E                     INZ('0')
010405170223 "   X*ex R027  OPRD16       E                     INZ('0')
010406170223 "   X*ex R027  ERRD16       E                     INZ('0')
010407170223R224 X*ex R027  CMTD16       E                     INZ('0')
010500950705     D*-------------
010600981221     D* Parametri del PGM di manutenzione NDC066R1 est. nominativi
010700981221    >D NDC066DS1     E DS                  inz
010800981221    >D  RET0661      E                     INZ('0')
010900981221    >D  OPR0661      E                     INZ('0')
011000981221    >D  ERR0661      E                     INZ('0')
011100981221     D*-------------
011200950705     D* Ricerca condizioni di pagamento
011300950705    >D A024DS        E DS                  EXTNAME(ANA024DS)
011400951121     D* Ricerca tipo appoggio
011500951121    >D ANA026DS      E DS
011600960129     D*-------------
011700960129     D* Ricerca unità
011800960129    >D A009DS        E DS                  EXTNAME(ANA009DS) INZ
011900950705     D*-------------
012000950705     D* Ricerca capoconti
012100950705     D A200DS        E DS                  EXTNAME(ANA200DS) INZ
012200950705     D*-------------
012300950705     D* Ricerca sottoconti
012400950705     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
012500950705     D*-------------
012600950705     D* Ricerca fornitori e agenti
012700950705     D A400DS        E DS                  EXTNAME(ANA400DS)
012800950706     D*-------------
012900950706     D*  Ds per output DVAGE
013000950706     D DVAGE         E DS                  EXTNAME(DVAGEDS) INZ
013100950706     D*-------------
013200950706     D*  Ds per output per dvriver agenti
013300950706     D DVOAGE        E DS                  EXTNAME(DVOAGEDS) INZ
013400950705     D*-------------
013500950705     D* Richiamo a XATB
013600950705     D XATBDS        E DS                  INZ
013700950705     D*-------------
013800950705     D* Parametri per ricerca codice ANATB00F
013900950705     D XTABDS        E DS                  INZ
014000950706     D*-------------
014100950706     D* Tabella blocco amministrativo
014200950706     D TAB0AB        E DS                  EXTNAME(ANG0ABDS)
014300950706     D*-------------
014400950706     D* Tabella tipo appoggio
014500950706     D TABG25        E DS                  EXTNAME(ANGG25DS)
014600950906     D*-------------
014700950906     D* Ricerca legende
014800950906     D A964DS        E DS                  EXTNAME(ANA964DS) INZ
014900040402     D*-------------
015000040402D1573D* Dati da mod. pnota
015100040402D1573D NDC011DS      E DS                  INZ
015200950906     D*-------------
015300950906     D* Controlli validità legende
015400950906     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
015500940201     D*-------------
015600950706     D* Controllo conti
015700950706     D ND002DS       E DS
015800950706     D*-------------
015900950706     D* Definizioni parametri standard chiamate moduli
016000950706     D$Esito           S              1
016100950706     D$gesmsg          S              1
016200950706     D$lenout          S              9B 0
016300950706     D$struttura       S             10
016400950706     D*-------------
016500940201     D DSFMT           DS
016600940506     D  $TASTO               369    369
016700940201     D  NRG                  370    370
016800940201     D  NCL                  371    371
016900950314     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
017000940201     D*-------------
017100940201     D* posizione cursore
017200940201     D CURSOR          DS
017300940223     D  RIRI                   1      2B 0 INZ
017400940201     D  R$$                    2      2
017500940223     D  COCO                   3      4B 0 INZ
017600940201     D  C$$                    4      4
017700940225     D*-------------
017800940616     D* Costanti
017900951121     D*-------------
018000951121     D DVCOBADS      E DS
018100940225     D*-------------
018200940506     D* Tasti di funzione
018300940506     D F01             C                   CONST(X'31')
018400940506     D F02             C                   CONST(X'32')
018500940506     D F03             C                   CONST(X'33')
018600940506     D F04             C                   CONST(X'34')
018700940506     D F05             C                   CONST(X'35')
018800940506     D F06             C                   CONST(X'36')
018900940506     D F07             C                   CONST(X'37')
019000940506     D F08             C                   CONST(X'38')
019100940506     D F09             C                   CONST(X'39')
019200940506     D F10             C                   CONST(X'3A')
019300940506     D F11             C                   CONST(X'3B')
019400940506     D F12             C                   CONST(X'3C')
019500940506     D F13             C                   CONST(X'B1')
019600940506     D F14             C                   CONST(X'B2')
019700940506     D F15             C                   CONST(X'B3')
019800940506     D F16             C                   CONST(X'B4')
019900940506     D F17             C                   CONST(X'B5')
020000940506     D F18             C                   CONST(X'B6')
020100940506     D F19             C                   CONST(X'B7')
020200940506     D F20             C                   CONST(X'B8')
020300940506     D F21             C                   CONST(X'B9')
020400940506     D F22             C                   CONST(X'BA')
020500940506     D F23             C                   CONST(X'BB')
020600940506     D F24             C                   CONST(X'BC')
020700940506     D ENTER           C                   CONST(X'F1')
020800940506     D ROLDWN          C                   CONST(X'F4')
020900940506     D ROLLUP          C                   CONST(X'F5')
021000950606     D*-------------
021100950606     D* Sottotitolo di testata
021200950705    >D* se non si hanno sottotitoli, mettere tutta la costante = ' '
021300950705     D SOTTIT          C                   CONST('              ')
021400950606     D*-------------
021500950606     D* Variabili per gestione videate
021600950606     D $Gest           S              2A
021700950606     D $Fine           S              1A
021800950928     D $open           S              1A
021900950606     D $InzD1          S              1A
022000950606     D $LastG          S              2A
022100950606     D $LastV          S              2A
022200950606     D*-------------
022300950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
022400950606     D CurR            S              2S 0
022500950606     D CurC            S              2S 0
022600950606     D Win             S             99A
022700950606     D*-------------
022800950606     D* Variabili appoggio specifiche della singola anagrafica
022900950606     D*  Indici
023000950606     D X               S              3S 0
023100950606     D Y               S              3S 0
023200950705     D*------------
023300950705     D* Campi di lavoro
023400950705     D Wdate           S               D
023500950705     D WWBKAMM         S                   like(D1BKAMM)
023600950906     D WWMODRIC        S                   like(D1MODRIC)
023700950705     D WWTPAPPo        S                   like(D1TPAPPo)
023800951121     D TpAppoOld       S                   like(D1TPAPPo)
023900040402D1573D Ctgana1         S                   like(Ppsctgana1)
024000040402  "  D Ctgana2         S                   like(Ppsctgana2)
024100040402D1573D Ctgana3         S                   like(Ppsctgana3)
024200951121     D Testi           C                   'Continua       Annulla'
024300951121     D MSGT            S             70    DIM(2)
024400951121     D TXT             S             27    DIM(2) ctdata perrcd(1)
024500990810C1220 *------------------
024600990810  "  D $D1TitCat       S                   Like(D1TitCat1)
024700990810C1220 *------------------
024800000118C1309D ZDateISO        S               D
024900010105D0807D*------------
025000010105D0807D in05            S              1A
025100101123c2174 * Parametri del PGM di gestione traduzioni
025200101123  "  D XtraDs        E DS                  Inz
025300101123C2174D*-------------
025400940127     C*----------------------------------------------------*
025500940127     C*                MAIN LINE PROGRAM
025600940127     C*----------------------------------------------------*
025700940127     C*
025800940223     C* inizializzazione variabili
025900940223     C                   EXSR      INZVAR
026000940223     C*
026100940223     C     $FINE         DOWEQ     *OFF
026200940202     C     $GEST         CASEQ     'D1'          GESD1
026300950705    >C     $GEST         CASEQ     'D2'          GESD2
026400950907     C                   ENDCS
026500950907     C* Solo aggiornamento
026600950907    >C     $GEST         IFEQ      'AG'
026700950907    >C                   EXSR      AGGANA
026800950907    >C                   EXSR      GESAGG
026900950907     C                   ENDIF
027000950907     C*
027100941027     C                   ENDDO
027200940201     C*
027300940325     C* fine programma
027400940325     C                   EXSR      ENDPGM
027500940325     C/EJECT
027600940325     C************************************************************
027700940325     C* RICERCHE
027800940325     C************************************************************
027900940325     C     ENDPGM        BEGSR
028000941214     C*
028100941214     C* se il COMMIT non è demandato al pgm chiamante
028200950705    >C     CMT061        IFEQ      '0'
028300941214     C*
028400941214     C* test se esistono transazioni attive ed eventualmente cosa fare
028500941214     C                   Z-ADD     0             EXECOD
028600950522     C                   CALL      'XRCMTI'
028700941214     C                   PARM                    EXECOD            1 0
028800941214     C*
028900941214     C                   SELECT
029000941214     C* Annulla richiesta
029100941214     C     EXECOD        WHENEQ    1
029200941214     C                   MOVEL     *OFF          $FINE
029300941214     C                   MOVEL     *HIVAL        $LASTV
029400941214     C* Conferma uscita
029500941214     C     EXECOD        WHENEQ    2
029600941214     C                   ROLBK
029700941214     C*
029800941214     C                   ENDSL
029900941214     C*
030000941214     C                   ENDIF
030100950706     C*
030200941214     C*
030300941214     C* se ho confermato l'uscita proseguo
030400941214     C     $FINE         IFEQ      *ON
030500950706     C*
030600950706     C* disallocazione rcd nel caso di modifica/annullamento
030700950706     C     OPZ061        IFEQ      '02'
030800950706    >C     OPZ061        OREQ      '04'
030900950911    >C     OPZ061        OREQ      '60'
031000950911    >C     OPZ061        OREQ      '61'
031100950706    >C                   UNLOCK    NDPPA01L                             22
031200950706    >C                   UNLOCK    NDPPS01L                             22
031300950706     C                   ENDIF
031400940325     C*
031500940325     C* passaggio dati al pgm chiamante
031600950705    >C                   MOVEL     NDC061DS      KPJBU
031700940325     C*
031800940325     C                   SETON                                            RT
031900940616     C                   RETURN
032000941214     C*
032100941214     C                   ENDIF
032200940325     C*
032300940325     C                   ENDSR
032400940325     C/EJECT
032500940201     C************************************************************
032600940201     C* RICERCHE
032700940201     C************************************************************
032800940127     C     SEARCH        BEGSR
032900940127     C*
033000940127     C* determino Riga/Colonna del cursore
033100940127     C                   MOVE      NRG           R$$
033200940127     C                   MOVE      NCL           C$$
033300940127     C                   Z-ADD     RIRI          CURR
033400940127     C                   Z-ADD     COCO          CURC
033500950705     C*
033600950705     C     D1KCcfact     IFNE      *BLANKS
033700950705     C     D1KCcfact     ANDNE     'C     '
033800950705     C     D1KCcfact     ANDNE     'F     '
033900950706     C                   CALLb     'XALLINEA'
034000950705     C                   PARM                    D1KCcfact
034100950705     C                   PARM      6             LEN               5 0
034200950705     C                   PARM                    XSCINP
034300950705     C                   ENDIF
034400950705     C*
034500950705     C     D1KScfact     IFNE      *BLANKS
034600950706     C                   CALLb     'XALLINEA'
034700950705     C                   PARM                    D1KScfact
034800950705     C                   PARM      8             LEN               5 0
034900950705     C                   PARM                    XSCINP
035000950705     C                   ENDIF
035100940127     C*
035200940224     C* I campi che al momento dell'F4 risultano protetti non devono
035300940224     C* essere abilitati alla funzione di ricerca
035400941027     C*
035500941027     C                   SELECT
035600960129    >C* FMTD1 - UNITA
035700960129    >C     H1NMFL        WHENEQ    'D1UNITA'
035800960129    >C                   RESET                   A009DS
035900960129    >C                   MOVEL     '01'          OPZ009
036000960129    >C                   MOVEL     XSCSOC        SOC009
036100960129    >C                   MOVEL     A009DS        KPJBU
036200960129    >C                   CALL      'ANA009R'
036300960129     C                   PARM                    KPJBA
036400960129    >C                   MOVEL     KPJBU         A009DS
036500960129     C*
0366009601292   >C     UNI009        IFNE      *BLANK
036700960129    >C                   MOVE      UNI009        D1UNITA
0368009601292-  >C                   ENDIF
036900950906     C*
037000950906     C* FMTD1  - modificata/riconcialiata
037100950906     C     H1NMFL        WHENEQ    'D1MODRIC'
037200950906     C                   RESET                   A964DS
037300950906     C                   MOVEL     '01'          OPZ964
037400950906     C                   MOVEL     'NDPPA000'    TRC964
037500950906     C                   MOVEL     'PPAMODRIC'   CAM964
037600950906     C                   MOVEL     A964DS        KPJBU
037700950906     C                   CALL      'ANA964R'
037800950906     C                   PARM                    KPJBA
037900950906     C                   MOVEL     KPJBU         A964DS
038000950906     C                   MOVEL     VAL964        WWmodric
038100950906     C*
038200950906     C     WWmodric      IFNE      *BLANK
038300950906     C                   MOVEL     WWmodric      D1modric
038400950906     C                   ENDIF
038500950906     C* imposto pos. del cursore
038600950906     C                   Z-ADD     CURR          H1RIGA
038700950906     C                   Z-ADD     CURC          H1COLO
038800950906     C*
038900950705     C*
039000950705     C* FMTD1  - BLOCCO AMMINISTRATIVO
039100010105D0807C* L'F4 è consentito solo se il campo non è protetto
039200950705     C     H1NMFL        WHENEQ    'D1BKAMM'
039300010105D0807C     in05          andeq     *Off
039400950705     C                   RESET                   XTABDS
039500950705     C                   MOVE      *BLANK        WWBKAMM
039600950705     C                   MOVE      '01'          XTAOPZ
039700950705     C                   MOVE      XSCSOC        XTAAZI
039800950705     C                   MOVE      XSCLIN        XTALIN
039900950705     C                   Z-ADD     CURR          XTARIG
040000950705     C                   Z-ADD     CURC          XTACOL
040100950705     C                   CLEAR                   KPJBU
040200950705     C                   MOVEL     XTABDS        KPJBU
040300950705     C                   CALL      'ANG0ABR'
040400950705     C                   PARM                    KPJBA
040500950705     C                   MOVEL     KPJBU         XTABDS
040600950705     C     XTAERR        IFNE      '1'
040700950705     C                   MOVE      XTAKEY        WWBKAMM
040800950705     C                   ELSE
040900950705     C                   MOVE      *BLANK        WWBKAMM
041000950705     C                   ENDIF
041100950705     C*
041200950705     C     WWBKAMM       IFNE      *BLANK
041300950705     C                   MOVEL     WWBKAMM       D1BKAMM
041400950705     C                   ENDIF
041500950705     C* imposto pos. del cursore
041600950705     C                   Z-ADD     CURR          H1RIGA
041700950705     C                   Z-ADD     CURC          H1COLO
041800950705     C*
041900950705     C* FMTD1  - tipo aggoggio bancario
042000950705     C     H1NMFL        WHENEQ    'D1TPAPPO'
042100071108D2301c* se non presente il conto factor, si reperiscono  le
042200071108  "  c* coordinate del conto
042300071108  "  C                   if        d1Kccfact = *blanks and
042400071108D2301c                             d1Kscfact = *blanks
042500951121     C                   RESET                   ANA026DS
042600951121     C                   MOVEL     '01'          OPZ026
042700951122     C                   if        Snatura61 = 'C' or Snatura61 = 'F'
042800951122     C                   MOVEL     Snatura61     COF026
042900951122     C                   MOVE      D1Ksc         KSC026
043000951122     C                   MOVE      *blank        FSC026
043100951122     C                   MOVE      *blank        KCC026
043200951122     C                   Else
043300951121     C                   MOVE      D1KCc         KCC026
043400951121     C                   MOVE      D1Ksc         KSC026
043500951122     C                   Endif
043600951121     C                   MOVEL     ANA026DS      KPJBU
043700951121     C                   CALL      'ANA026R'
043800951121     C                   PARM                    KPJBA
043900951121     C                   MOVEL     KPJBU         ANA026DS
044000951121     C*
044100951121     C     TAP026        IFNE      *BLANK
044200951121     C                   MOVEL     TAP026        D1TPAPPO
044300951121     C                   ENDIF
044400071108D2301C                   else
044500071108  "  c                   exsr      RepTpAFact
044600071108D2301c                   endif
044700950705     C* imposto pos. del cursore
044800950705     C                   Z-ADD     CURR          H1RIGA
044900950705     C                   Z-ADD     CURC          H1COLO
045000950705     C*
045100950705     C* FMTD1  - CAPOCONTO FACTOR
045200950705     C     H1NMFL        WHENEQ    'D1KCCFACT'
045300950705     C                   RESET                   A200DS
045400950705     C                   MOVEL     '01'          OPZ200
045500950705     C                   MOVE      D1KCcfact     KCC200
045600950705     C                   MOVEL     A200DS        KPJBU
045700950705     C                   CALL      'ANA200R'
045800950705     C                   PARM                    KPJBA
045900950705     C                   MOVEL     KPJBU         A200DS
046000950705     C*
046100950705     C     KCC200        IFNE      *BLANK
046200950705     C                   MOVEL     KCC200        D1KCCFACT
046300950705     C                   ENDIF
046400950705     C* imposto pos. del cursore
046500950705     C                   Z-ADD     CURR          H1RIGA
046600950705     C                   Z-ADD     CURC          H1COLO
046700950705     C*
046800950705     C* FMTD1  - SOTTOCONTO FACTOR
046900950705     C     H1NMFL        WHENEQ    'D1KSCFACT'
047000950705     C                   RESET                   A201DS
047100950705     C                   MOVE      XSCsoc        SOC201
047200950705     C                   MOVE      D1KCCFACT     KCC201
047300950705     C                   MOVE      D1KsCFACT     KSC201
047400950705     C                   MOVEL     A201DS        KPJBU
047500950705     C                   CALL      'ANA201R'
047600950705     C                   PARM                    KPJBA
047700950705     C                   MOVEL     KPJBU         A201DS
047800950705     C*
047900950705     C     KSC201        IFNE      *BLANK
048000950705     C                   MOVEL     KCC201        D1KCCFACT
048100950705     C                   MOVEL     KSC201        D1KSCFACT
048200950705     C                   ENDIF
048300950705     C* imposto pos. del cursore
048400950705     C                   Z-ADD     CURR          H1RIGA
048500950705     C                   Z-ADD     CURC          H1COLO
048600950705     C*
048700950705     C* FMTD1  - CONDIZIONI DI PAGAMENTO
048800950705     C     H1NMFL        WHENEQ    'D1CONDPAG'
048900950705     C                   RESET                   A024DS
049000950705     C                   MOVEL     '01'          OPZ024
049100950705     C                   MOVEL     A024DS        KPJBU
049200950705     C                   CALL      'ANA024R'
049300950705     C                   PARM                    KPJBA
049400950705     C                   MOVEL     KPJBU         A024DS
049500950705     C*
049600950705     C     COD024        IFNE      *BLANK
049700950705     C                   MOVEL     COD024        D1CONDPAG
049800950705     C                   ENDIF
049900950705     C* imposto pos. del cursore
050000950705     C                   Z-ADD     CURR          H1RIGA
050100950705     C                   Z-ADD     CURC          H1COLO
050200950705     C* RICERCA AGENTE
050300950705     C     H1NMFL        WHENEQ    'D1KSCAGE'
050400950705     C                   RESET                   A400DS
050500950705     C                   MOVEL     '01'          OPZ400
050600950705     C                   MOVE      '3'           AGE400
050700950705     C                   MOVEL     A400DS        KPJBU
050800950705     C                   CALL      'ANA400R'
050900950705     C                   PARM                    KPJBA
051000950705     C                   MOVEL     KPJBU         A400DS
051100950705     C*
051200961119     C     ksc400        IFNE      *BLANK
051300961119     C                   MOVEL     ksc400        D1KSCAGE
051400950705     C                   ENDIF
051500950705     C* imposto pos. del cursore
051600950705     C                   Z-ADD     CURR          H1RIGA
051700950705     C                   Z-ADD     CURC          H1COLO
051800980302     C*
051900980303     C* FMTD1  - Fattura finanziata
052000980302     C     H1NMFL        WHENEQ    'D1FLGFIN'
052100980302     C                   RESET                   A964DS
052200980302     C                   MOVEL     '01'          OPZ964
052300980302     C                   MOVEL     'NDPPS000'    TRC964
052400980302     C                   MOVEL     'PPSFLGFIN'   CAM964
052500980303     C                   MOVEL     *ON           ALL964
052600980302     C                   MOVEL     A964DS        KPJBU
052700980302     C                   CALL      'ANA964R'
052800980302     C                   PARM                    KPJBA
052900980302     C                   MOVEL     KPJBU         A964DS
053000980303      * ritorno dal programma normale
053100980303     C     RET964        IFEQ      *BLANK
053200980303     C                   MOVEL     VAL964        D1FlgFin
053300980303     C                   MOVEL     DES964        D1FlgFinD
053400980303     C                   ENDIF
053500980302     C* imposto pos. del cursore
053600980302     C                   Z-ADD     CURR          H1RIGA
053700980302     C                   Z-ADD     CURC          H1COLO
053800980302     C*
053900990810C1220 * FMTD1  - Categorie di partita
054000990810  "  C     H1NMFL        WHENEQ    'D1CTGANA1'
054100990810  "  C     H1NMFL        OrEq      'D1CTGANA2'
054200000410  "  C     H1NMFL        OrEq      'D1CTGANA3'
054300990810  "   *
054400990810  "  C                   RESET                   XTABDS
054500990810  "  C                   MOVE      '01'          XTAOPZ
054600990810  "  C                   MOVE      XScSoc        XTAAZI
054700990810  "  C                   Z-ADD     CURR          XTARIG
054800990810  "  C                   Z-ADD     CURC          XTACOL
054900990810  "  C                   CLEAR                   KPJBU
055000990810  "  C                   MOVEL     XTABDS        KPJBU
055100990810  "   *
055200990810  "  C                   Select
055300990810  "  C                   When      H1NmFl      = 'D1CTGANA1'
055400990810  "  C                   CALL      'ANG0P1R'
055500990810  "  C                   PARM                    KPJBA
055600990810  "  C                   When      H1NmFl      = 'D1CTGANA2'
055700990810  "  C                   CALL      'ANG0P2R'
055800990810  "  C                   PARM                    KPJBA
055900990810  "  C                   When      H1NmFl      = 'D1CTGANA3'
056000990810  "  C                   CALL      'ANG0P3R'
056100990810  "  C                   PARM                    KPJBA
056200990810  "  C                   EndSl
056300990810  "   *
056400990810  "  C                   MOVEL     KPJBU         XTABDS
056500990810  "  C     XTAERR        IFNE      '1'
056600990810  "  C     XTaKey        AndNe     *Blank
056700990810  "   *
056800990810  "  C                   Select
056900990810  "  C                   When      H1NmFl      = 'D1CTGANA1'
057000990810  "  C                   MOVE      XtaKey        D1CtgAna1
057100990810  "  C                   When      H1NmFl      = 'D1CTGANA2'
057200990810  "  C                   MOVE      XtaKey        D1CtgAna2
057300990810  "  C                   When      H1NmFl      = 'D1CTGANA3'
057400990810  "  C                   MOVE      XtaKey        D1CtgAna3
057500990810  "  C                   EndSl
057600990810  "  C                   ENDIF
057700990810  "  C* imposto pos. del cursore
057800990810  "  C                   Z-ADD     CURR          H1RIGA
057900990810  "  C                   Z-ADD     CURC          H1COLO
058000990810C1220 *
058100940223     C*
058200940223     C* ricerca non abilitata in questa posizione
058300941027     C                   OTHER
058400940926     C                   SETON                                        9899
058500940127     C*
058600941027     C                   ENDSL
058700950612     C*
058800950612     C* imposto pos. del cursore
058900950612     C                   Z-ADD     CURR          H1RIGA
059000950612     C                   Z-ADD     CURC          H1COLO
059100940127     C*
059200940506     C                   MOVE      *HIVAL        $LASTV
059300950612     C*
059400940127     C                   ENDSR
059500071108     C/EJECT
059600071108D2301C************************************************************
059700071108  "  C* Ricerca coordinate factor
059800071108D2301C************************************************************
059900071108     C     RepTpAFact    BEGSR
060000071108     c*
060100071108     C*  reperisco sottonatura factor
060200071108     C                   Clear                   ND002DS
060300071108     C                   EVAL      $LenOut = %Size(ND002DS)
060400071108     C                   CallB     'NDMVC002'
060500071108     C                   PARM                    XSCsoc
060600071108     C                   PARM                    d1kccfact
060700071108     C                   PARM                    d1kscfact
060800071108     C                   PARM                    Wdate
060900071108     C                   PARM      *OFF          $GesMsg
061000071108     C                   PARM      *OFF          $Esito
061100071108     C                   PARM      'ND002DS'     $Struttura
061200071108     C                   PARM                    ND002DS
061300071108     C                   PARM                    $LenOut
061400071108     C* Errore
061500071108     C     $Esito        IFEQ      *Off
061600071108     c*
061700071108     C                   RESET                   ANA026DS
061800071108     C                   MOVEL     '01'          OPZ026
061900071108     C                   if        Snatura002= 'C' or Snatura002= 'F'
062000071108     C                   MOVEL     Snatura002    COF026
062100071108     C                   MOVE      D1Kscfact     KSC026
062200071108     C                   MOVE      *blank        FSC026
062300071108     C                   MOVE      *blank        KCC026
062400071108     C                   Else
062500071108     C                   MOVE      D1KCcfact     KCC026
062600071108     C                   MOVE      D1Kscfact     KSC026
062700071108     C                   Endif
062800071108     C                   MOVEL     ANA026DS      KPJBU
062900071108     C                   CALL      'ANA026R'
063000071108     C                   PARM                    KPJBA
063100071108     C                   MOVEL     KPJBU         ANA026DS
063200071108     C*
063300071108     C     TAP026        IFNE      *BLANK
063400071108     C                   MOVEL     TAP026        D1TPAPPO
063500071108     C                   ENDIF
063600071108     C*
063700071108     c                   endif
063800071108     C*
063900071108D2301C                   ENDSR
064000940225     C/EJECT
064100940225     C************************************************************
064200940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
064300940225     C************************************************************
064400940225     C     GESPIE        BEGSR
064500940225     C*
064600940225     C                   SELECT
064700940225     C*
064800940506     C     $GEST         WHENEQ    'D1'
064900940506     C     $ULKD1        IFEQ      0
065000940506     C* riempimento opzioni
065100940506     C                   EXSR      F24D1
065200940506     C                   ELSE
065300940506     C                   WRITE     FMTZ1
065400940506     C                   ENDIF
065500940225     C*
065600940926     C     $GEST         WHENEQ    'D2'
065700940926     C     $ULKD1        IFEQ      0
065800940926     C* riempimento opzioni
065900940926     C                   EXSR      F24D2
066000940926     C                   ELSE
066100940926     C                   WRITE     FMTZ1
066200940926     C                   ENDIF
066300940225     C                   ENDSL
066400940225     C*
066500940225     C                   ENDSR
066600940131     C/EJECT
066700940131     C************************************************************
066800940510     C* GESTIONE VIDEO RECORD D1
066900940131     C************************************************************
067000940127     C     GESD1         BEGSR
067100940225     C* inizializzazione videata
067200941117   B1C     $INZD1        IFEQ      *ON
067300940127     C                   EXSR      INZD1
067400940223     C                   MOVE      *OFF          $INZD1
067500941117   E1C                   ENDIF
067600940225     C* emissione piede videata se proveniente da altra
067700940225     C* salvataggio valore formato di provenienza
067800941117   B1C     $LASTV        IFNE      'D1'
067900940207     C                   WRITE     FMTT1
068000940225     C                   EXSR      GESPIE
068100940202     C                   MOVE      $LASTV        $LASTG
068200940202     C                   MOVE      'D1'          $LASTV
068300941117   E1C                   ENDIF
068400940707     C*
068500940707     C* se esistono errori sulla videata
068600940707     C* emetto la write del formato a indicatori spenti per vedere
068700940707     C* le eventuali decodifiche
068800941117   B1C     *IN99         IFEQ      *ON
068900940707     C     $GEST         ANDEQ     'D1'
069000940707     C                   MOVEA     *IN           WIN
069100940707     C                   MOVE      *ALL'0'       IN5098           49
069200940707     C                   MOVEA     IN5098        *IN(50)
069300940707     C                   WRITE     FMTD1
069400940707     C                   MOVEA     WIN           *IN
069500941117   E1C                   ENDIF
069600940131     C*
069700941117   B1C                   SELECT
069800940203     C* Annullamento
069900950705    >C     OPZ061        WHENEQ    '04'
070000940224     C* Visualizzazione
070100950705    >C     OPZ061        OREQ      '05'
070200940203     C                   WRITE     FMTD1
070300940203     C                   EXFMT     PROTECT
070400940224     C* Immissione/Modifica/Copia
070500941117   X1C                   OTHER
070600940203     C                   EXFMT     FMTD1
070700941117   E1C                   ENDSL
070800940506     C* controllo abilitazione tasti funzione
070900940506     C*
071000940506     C                   EXSR      CTRKD1
071100940506     C*
071200941117   B1C     *IN99         IFEQ      *OFF
071300940131     C*
071400941117   B2C                   SELECT
071500940609     C* ROLLUP
071600950705    >C     $TASTO        WHENEQ    ROLLUP
071700950705    >C                   MOVE      'D2'          $GEST
071800940916     C* F2=Decodifica
071900940916     C     $TASTO        WHENEQ    F02
072000940916     C                   EXSR      F02D1
072100940131     C* F3=Fine
072200940506     C     $TASTO        WHENEQ    F03
072300940309     C                   EXSR      F03D1
072400940131     C* F4=Ricerca
072500940506     C     $TASTO        WHENEQ    F04
072600940127     C                   EXSR      SEARCH
072700940224     C* F5=Refresh
072800940506     C     $TASTO        WHENEQ    F05
072900940309     C                   EXSR      F05D1
073000940223     C* F8=Successivo
073100940506     C     $TASTO        WHENEQ    F08
073200940309     C                   EXSR      F08D1
073300940131     C* F12=Ritorno
073400940506     C     $TASTO        WHENEQ    F12
073500940309     C                   EXSR      F12D1
073600990317     C* F20=Riapertura partita
073700990317     C     $TASTO        WHENEQ    F20
073800990317     C                   EXSR      F20D1
073900940315     C* F24=Altri tasti
074000940506     C     $TASTO        WHENEQ    F24
074100940315     C                   EXSR      F24D1
074200940131     C*
074300941117   X2C                   OTHER
074400940127     C                   EXSR      CTRD1
074500940510     C* aggiungere le chiamate ai controlli delle videate supplementari
074600950705    >C     *IN99         IFEQ      *OFF
074700950705    >C                   EXSR      CTRD2
074800950705    >C     *IN99         IFEQ      *ON
074900950705    >C                   SETON                                            96
075000950705    >C                   ENDIF
075100950705    >C                   ENDIF
075200940203     C*
075300941117   B3C     $TASTO        IFEQ      F06
075400940207     C     *IN99         ANDEQ     *OFF
075500940224     C* eseguo aggiornamento
075600951120     C* richiedo l' aggiornamento degli effetti se il campo coord.ban
075700951120     C* è cambiato
075800951121     C                   If        TpAppoOld<>D1TpAppo
075900951121     C                   EXSR      CONFEFF
076000951121     C                   Endif
076100951120     C                   EXSR      AGGANA
076200940224     C* eseguo operazioni del dopo-aggiornamento
076300941117   B4C     *IN99         IFEQ      *OFF
076400940318     C                   EXSR      GESAGG
076500941117   E4C                   ENDIF
076600941117   E3C                   ENDIF
076700940203     C*
076800941117   E2C                   ENDSL
076900940224     C*
077000941117   E1C                   ENDIF
077100941027     C*
077200940117     C                   ENDSR
077300940506     C/EJECT
077400940506     C************************************************************
077500940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
077600940506     C************************************************************
077700940506     C     CTRKD1        BEGSR
077800941205     C*
077900940506     C                   SETOFF                                       99
078000940506     C                   MOVEA     $FC1          $FC
078100940506     C                   EXSR      CTRKEY
078200940506     C     *IN99         IFEQ      *ON
078300940506     C                   SETON                                        97
078400940506     C                   ENDIF
078500941205     C*
078600940506     C                   ENDSR
078700941209     C/EJECT
078800941209     C************************************************************
078900941209     C* INIZIALIZZAZIONE VIDEATA DATI
079000941209     C************************************************************
079100940127     C     INZD1         BEGSR
079200940131     C*
079300940131     C                   CLEAR                   FMTD1
079400950705    >C                   CLEAR                   FMTD2
079500950705     C                   EVAL      D1ON1  = xscon
079600950705     C                   EVAL      D1ON2  = xscon
079700950705     C                   EVAL      D1ON3  = xscon
079800950705     C                   EVAL      D1ON4  = xscon
079900950705     C                   EVAL      D1OF1  = xscoFF
080000950705     C                   EVAL      D1OF2  = xscoFF
080100950705     C                   EVAL      D1OF3  = xscoFF
080200950705     C                   EVAL      D1OF4  = xscoFF
080300950705     C                   MOVE      XSCSOC        PPASOCIETA
080400960129      * Impostazione indicatore 11 per gestione campo unità
080500960129     C                   if        fgo1=*on
080600960129     C                   eval      *in11=*on
080700960129     C                   else
080800960129     C                   eval      *in11=*off
080900960129     C                   endif
081000950102     C*
081100990810C1220 *
081200990810  "   * Riempio  titoli categorie di partita
081300990810  "  C                   EXSR      RieTitCat
081400990810C1220 *
081500990810      *
081600990810     C     OPZ061        IFNE      '01'
081700950906    >C                   MOVEL     CTB061        PPACTB
081800950705    >C                   MOVEL     KCC061        PPAKCC
081900950705    >C                   MOVEL     KSC061        PPAKSC
082000950705    >C                   MOVEL     DTPAR061      PPADTPAR
082100950705    >C                   MOVEL     NRPAR061      PPANRPAR
082200950705    >C                   MOVEL     SERPAR061     PPASERPAR
082300950706     C*
082400950706     C* Esegue chain e controlli allocazioni/esistenze archivi
082500950706     C* se vi sono essori, la routine richiama ENDPGM e va a fine
082600950706     C                   EXSR      CHAINANA
082700950706     C*
082800021031D1227x* In questo pgm viene effettuata la
082900021031  "  x* disallocazione rcd nel caso di modifica/annullamento
083000021031  "  x*    OPZ061        IFEQ      '02'
083100021031  " >x*    OPZ061        OREQ      '04'
083200021031  " >x*    OPZ061        OREQ      '60'
083300021031  " >x*    OPZ061        OREQ      '61'
083400021031  " >x*                  UNLOCK    NDPPA01L                             22
083500021031  " >x*                  UNLOCK    NDPPS01L                             22
083600021031D1227x*                  ENDIF
083700950706     C*
083800940224     C* riempio la videata
083900940224     C                   EXSR      RIED1
084000950705    >C* Aggiungere le routine di riempimento dei record succcessivi
084100950705    >C                   EXSR      RIED2
084200040401D1573C* Se passato codice modello e NON sto gestendo ctb 'MD'significa
084300040401  "  c* che devo caricare alcuni dati dal modello
084400040401  "  c                   if        TabPnot061 <> *blanks and
084500040406  "  c                             Ctb061 <>'MD' and DocIva061='1'
084600040401  "  C                   EXSR      RieDaMod
084700040401D1573C                   endif
084800940224     C* richiamo routine dei ctrl per decodificarla
084900940614     C                   EXSR      CTRD1
085000950705    >C* Aggiungere le routine di controllo dei record succcessivi
085100950705    >C                   EXSR      CTRD2
085200940224     C                   MOVE      *ALL'0'       IN5098
085300940224     C                   MOVEA     IN5098        *IN(50)
085400940224     C* definizione indicatori su video
085500940224     C                   SETOFF                                       02
085600950706     C                   SELECT
085700950706     C     OPZ061        WHENEQ    '02'
085800940224     C                   SETON                                        02
085900950706     C     OPZ061        WHENEQ    '05'
086000950706     C                   ENDSL
086100940223     C*
086200950102   X1C                   ELSE
086300950102     C*
086400950102     C* riempio con i default di immissione
086500950403     C                   EXSR      RIEDEF
086600950404     C*
086700950404     C* imposto i campi chiave ricevuti anche se si è in immissione
086800950404     C* perchè potrebbero essere stati passati dei default di chiave
086900950705    >C                   MOVEL     KCC061        D1KCC
087000950705    >C                   MOVEL     KSC061        D1KSC
087100951122     C     DtPar061      ifeq      *loval
087200951122    >C                   Z-add     0             D1DTPAR
087300951122     C                   Else
087400000118C1309C******dmy          MOVEL     DTPAR061      D1DTPAR
087500000118C1309C     *JobRun       MOVEL     DTPAR061      D1DTPAR
087600951122     C                   endif
087700950705    >C                   MOVEL     NRPAR061      D1NRPAR
087800950705    >C                   MOVEL     SERPAR061     D1SERPAR
087900950102   E1C                   ENDIF
088000960312     C*
088100960312     C* Decodifica  e imposto campo snatura61 che se non passato
088200960312     C* deve essere impostato
088300960312     c                   clear                   d1kscd
088400960312     C                   Clear                   ND002DS
088500960312     C                   EVAL      $LenOut = %Size(ND002DS)
088600960312     C                   CallB     'NDMVC002'
088700960312     C                   PARM                    XSCsoc
088800960312     C                   PARM                    d1kcc
088900960312     C                   PARM                    d1ksc
089000960312     C                   PARM                    Wdate
089100960312     C                   PARM      *OFF          $GesMsg
089200960312     C                   PARM      *OFF          $Esito
089300960312     C                   PARM      'ND002DS'     $Struttura
089400960312     C                   PARM                    ND002DS
089500960312     C                   PARM                    $LenOut
089600960312     C* Errore
089700960312   B2C     $Esito        IFEQ      *Off
089800960312     C                   MOVEL     DESBREV002    D1Kscd
089900960312     C                   MOVEL     snatura002    snatura61
090000970218     c* salvo se italia/estero
090100981109     c*********          eval      savitaest = itaest002
090200960312   E2C                   ENDIF
090300960312     C*
090400960312     C*  Campi solo per clienti
090500960312     C*
090600960312     C                   IF        SNATURA61 = 'C'
090700960312     C                   EVAL      *in04=*on
090800960312     C                   else
090900960312     C                   EVAL      *in04=*off
091000960312     C                   endif
091100990317     C*
091200990317     C* Definizione dei tasti funzione utilizzabili a pgm
091300990317     C* Da fare in questo punto perchè devo sapere se la partita è
091400990317     c* chiusa per far vedere un tasto
091500990317     C                   EXSR      DEFOPZ
091600990317     C*
091700990317     C* Reperimento tasti di funzione
091800990317     C*
091900990317     C                   EXSR      REPKEY
092000010105D0807 *
092100010105  "   * Imposto quegli attributi della videata che sono modificabili
092200010105  "   * al richiamo del determinato pgm
092300010105  "  C                   exsr      AtrD1
092400010105D0807 *
092500940224     C*
092600940117     C                   ENDSR
092700950706     C/EJECT
092800990810C1220 ************************************************************
092900990810  "   * Compongo titoli categorie di partita
093000990810  "   ************************************************************
093100990810  "  C     RieTitCat     BEGSR
093200990810  "   *
093300101123c1220 * I° categoria di partita
093400101123c2174** Reperisco traduzioni in lingua se ve ne sono
093500101123  "  c                   Reset                   XtraDs
093600101123  "  C                   Eval      XtrFld     = 'TITCAT1055'
093700101123  "  c                   Exsr      RepTrad
093800101123  "  c                   If        XtrTxt <> *blanks
093900101123  "  c                   Eval      TitCat1055=XtrTxT
094000101123c2174c                   Endif
094100101123c1220C                   Clear                   $D1TitCat
094200990810  "  C                   Move      *All'.'       D1TitCat1
094300990810  "   *           tolgo i blank sia a dx che a sx del titolo
094400990810  "  C                   Eval      $D1TitCat = %Trim(TitCat1055)
094500990810  "   *           unisco i '...' a dx del titolo (uno spazio in mezzo)
094600990810  "  C                   Cat       D1TitCat1:1   $D1TitCat
094700990810  "  C                   MoveL     $D1TitCat     D1TitCat1
094800990810  "  C                   Move      ':'           D1TitCat1
094900990810  "   *
095000101123c1220 * II° categoria di partita
095100101123c2174** Reperisco traduzioni in lingua se ve ne sono
095200101123  "  c                   Reset                   XtraDs
095300101123  "  C                   Eval      XtrFld     = 'TITCAT2055'
095400101123  "  c                   Exsr      RepTrad
095500101123  "  c                   If        XtrTxt <> *blanks
095600101123  "  c                   Eval      TitCat2055=XtrTxT
095700101123c2174c                   Endif
095800101123c1220C                   Clear                   $D1TitCat
095900990810  "  C                   Move      *All'.'       D1TitCat2
096000990810  "   *           tolgo i blank sia a dx che a sx del titolo
096100990810  "  C                   Eval      $D1TitCat = %Trim(TitCat2055)
096200990810  "   *           unisco i '...' a dx del titolo (uno spazio in mezzo)
096300990810  "  C                   Cat       D1TitCat2:1   $D1TitCat
096400990810  "  C                   MoveL     $D1TitCat     D1TitCat2
096500990810  "  C                   Move      ':'           D1TitCat2
096600990810  "   *
096700101123c1220 * III° categoria di partita
096800101123c2174** Reperisco traduzioni in lingua se ve ne sono
096900101123  "  c                   Reset                   XtraDs
097000101123  "  C                   Eval      XtrFld     = 'TITCAT3055'
097100101123  "  c                   Exsr      RepTrad
097200101123  "  c                   If        XtrTxt <> *blanks
097300101123  "  c                   Eval      TitCat3055=XtrTxT
097400101123c2174c                   Endif
097500101123c1220C                   Clear                   $D1TitCat
097600990810  "  C                   Move      *All'.'       D1TitCat3
097700990810  "   *           tolgo i blank sia a dx che a sx del titolo
097800990810  "  C                   Eval      $D1TitCat = %Trim(TitCat3055)
097900990810  "   *           unisco i '...' a dx del titolo (uno spazio in mezzo)
098000990810  "  C                   Cat       D1TitCat3:1   $D1TitCat
098100990810  "  C                   MoveL     $D1TitCat     D1TitCat3
098200990810  "  C                   Move      ':'           D1TitCat3
098300990810  "   *
098400990810C1220C     XRieTitCat    ENDSR
098500101123C2174C************************************************************
098600101123 "   C* Reperimento traduzioni titoli categorie di partita
098700101123C2174C************************************************************
098800101123     C     RepTrad       BEGSR
098900101123     C*
099000101123     c* imposto la lingua della società che è sempre, se presente,
099100101123     c* la lingua dell'utente
099200101123     c                   eval      XtrLin = xsclin
099300101123     c*
099400101123     c                   eval      XtrRic = '1'
099500101123     c                   eval      XtrSoc = xscsoc
099600101123     c                   eval      XtrFil = 'ANPRM'
099700101123     c                   eval      XtrAlc = *off
099800101123     c                   eval      XtrErr = *off
099900101123     C                   Eval      XtrFSC     = *blanks
100000101123     C                   Eval      XtrPar     = *blanks
100100101123     C                   Eval      XtrUtiLin  = 'X'
100200101123     ** Chiamata al Pgm
100300101123     C                   Call      'XTRA'
100400101123     c                   Parm                    XtraDs
100500101123     c* Se vi sono errori pulisco traduzione
100600101123     C                   If        XtrErr <> *off
100700101123     C                   Eval      XtrTxt = *blanks
100800101123     C                   Endif
100900101123     c*
101000101123     c                   endsr
101100101123C2174C/EJECT
101200950706     C************************************************************
101300950706     C* ESEGUE CHAIN ARCHIVI E CONTROLLI ALLOCAZIONI
101400950706     C************************************************************
101500950706     C     CHAINANA      BEGSR
101600950706     C*
101700950706   B1C     *IN22         DOUEQ     *OFF
101800950706     C     MSGRTN        ORNE      'R'
101900950706     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
102000950706   B2C     OPZ061        IFEQ      '02'
102100950706     C     OPZ061        OREQ      '04'
102200950906     C     K07PPA01      CHAIN     NDPPA000                           2122
102300950706     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
102400950706     C                   ELSE
102500950906    >C     K07PPA01      CHAIN(N)  NDPPA000                           2122
102600950706   E2C                   ENDIF
102700950706     C*
102800950706     C* Gestione RCD allocato
102900950706   B2C     *IN22         IFEQ      *ON
103000950706    >C                   MOVEL     'NDPPA00F'    MSGFIL
103100950706     C                   EXSR      MSGPGM
103200950706   E2C                   ENDIF
103300950706   E1C                   ENDDO
103400950706     C* se il rcd da gestire non viene trovato
103500950706     C* oppure è allocato da altro pgm
103600950706     C* ritorno al pgm guida avvertendolo dell'errore
103700950706   B1C     *IN21         IFEQ      *ON
103800950706     C     *IN22         OREQ      *ON
103900950706     C                   MOVE      *ON           $FINE
104000950706     C                   MOVE      '1'           ERR061
104100950706     C                   EXSR      ENDPGM
104200950706     C*
104300950706     C                   ELSE
104400950706     C*
104500950706     C                   MOVE      PPASYS        PPSSYS
104600950706     C                   MOVE      PPANRASINT    PPSNRASINT
104700950706   B2C     *IN22         DOUEQ     *OFF
104800950706     C     MSGRTN        ORNE      'R'
104900950706   B3C     OPZ061        IFEQ      '02'
105000950706    >C     OPZ061        OREQ      '04'
105100950706    >C     K02PPS01      CHAIN     NDPPS000                           2122
105200950706     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
105300950706   X3C                   ELSE
105400950706    >C     K02PPS01      CHAIN(N)  NDPPS000                           2122
105500950706   E3C                   ENDIF
105600950706     C*
105700950706     C* Gestione RCD allocato
105800950706   B3C     *IN22         IFEQ      *ON
105900950706    >C                   MOVEL     'NDPPS00F'    MSGFIL
106000950706     C                   EXSR      MSGPGM
106100950706   E3C                   ENDIF
106200950706   E2C                   ENDDO
106300950706     C*
106400950706     C* se il rcd da gestire non viene trovato
106500950706     C* oppure è allocato da altro pgm
106600950706     C* ritorno al pgm guida avvertendolo dell'errore
106700950706   B2C     *IN21         IFEQ      *ON
106800950706     C     *IN22         OREQ      *ON
106900950706     C                   MOVE      *ON           $FINE
107000950706    >C                   MOVE      '1'           ERR061
107100950706     C                   EXSR      ENDPGM
107200950706   E2C                   ENDIF
107300950706   E1C                   ENDIF
107400950706     C*
107500950706     C                   ENDSR
107600940207     C/EJECT
107700940207     C************************************************************
107800940510     C* RIEMPIMENTO VIDEATA  D1
107900940207     C************************************************************
108000940207     C     RIED1         BEGSR
108100950705     C*
108200950705    >C                   MOVEL     KCC061        D1KCC
108300950705    >C                   MOVEL     KSC061        D1KSC
108400951122     C     DtPar061      ifeq      *loval
108500951122    >C                   Z-add     0             D1DTPAR
108600951122     c                   Else
108700000118C1309C******dmy          MOVEL     DTPAR061      D1DTPAR
108800000118C1309C     *JobRun       MOVEL     DTPAR061      D1DTPAR
108900951122     C                   endif
109000950705    >C                   MOVEL     NRPAR061      D1NRPAR
109100950705    >C                   MOVEL     SERPAR061     D1SERPAR
109200940207     C*
109300950301     C* impostare i campi della videata dai campi del file
109400950301     C*
109500960129     c                   eval      d1unita = ppaunita
109600950705     C                   eval      d1divisa  = ppadivisa
109700950705     C     PPAPARCHIU    IFEQ      *on
109800950705     C                   eval      d1parchiu = xscon
109900950705     C                   eval      *in03 = *on
110000950705     C     PPADTchiu     IFNE      *loval
110100000118C1309C******dmy          move      ppadtchiu     d1dtchiu
110200000118C1309C     *JobRun       move      ppadtchiu     d1dtchiu
110300950705     C                   ELSE
110400950705     C                   MOVE      *zeros        d1dtchiu
110500950705     c                   endif
110600950705     C                   ELSE
110700950705     C                   eval      d1parchiu = xscoff
110800950705     C                   eval      *in03 = *off
110900950705     C                   ENDIF
111000950705     C     PPADTulmo     IFNE      *loval
111100000118C1309C******dmy          move      ppadtulmo     d1dtulmo
111200000118C1309C     *JobRun       move      ppadtulmo     d1dtulmo
111300950705     C                   ELSE
111400950705     C                   MOVE      *zeros        d1dtulmo
111500950705     c                   endif
111600950705     C     PPADTPARP     IFNE      *loval
111700000118C1309C******dmy          move      ppadtparp     d1dtparp
111800000118C1309C     *JobRun       move      ppadtparp     d1dtparp
111900950705     C                   ELSE
112000950705     C                   MOVE      *zeros        d1dtparp
112100950705     c                   endif
112200950705    >C                   EVAL      D1NRPArp = ppanrparp
112300950705    >C                   EVAL      D1serpArp = ppaserparp
112400950705     C     PPsDTDOC      IFNE      *loval
112500000118C1309C******dmy          move      ppsdtDOC      d1dtDOC
112600000118C1309C     *JobRun       move      ppsdtDOC      d1dtDOC
112700950705     C                   ELSE
112800950705     C                   MOVE      *zeros        d1dtdoc
112900950705     c                   endif
113000950705    >C                   EVAL      D1NRdoc  = ppsnrdoc
113100950705    >C                   EVAL      D1serdoc = ppsserdoc
113200950705    >C                   EVAL      D1bkamm  = ppsbkamm
113300950906    >C                   EVAL      D1modric = ppamodric
113400950705    >C                   EVAL      D1ctgana1 = ppsctgana1
113500950705    >C                   EVAL      D1ctgana2 = ppsctgana2
113600950705    >C                   EVAL      D1ctgana3 = ppsctgana3
113700950705    >C                   EVAL      D1tpappo  = ppstpappo
113800951121    >C                   EVAL      tpappoOld = ppstpappo
113900950705    >C                   EVAL      D1kccfact = ppskccfact
114000950705    >C                   EVAL      D1kscfact = ppskscfact
114100950705    >C                   EVAL      D1condpag = ppscondpag
114200950705     C*
114300950705    >C                   IF        PPSpagman  = *on
114400950705    >C                   EVAL      D1pagman  = XSCON
114500950705     C                   ELSE
114600950705    >C                   EVAL      D1pagman  = XSCOff
114700950705     C                   endif
114800950707     c*
114900950705    >C                   EVAL      D1KSCAGE  = ppsKSCAGE
115000950705     C*
115100950705    >C                   IF        PPSCLISOLL = *on
115200950705    >C                   EVAL      D1clisoll = XSCON
115300950705     C                   ELSE
115400950705    >C                   EVAL      D1clisoll = XSCOff
115500950705     C                   endif
115600950705     C*
115700950705    >C                   IF        PPSaddSOLL = *on
115800950705    >C                   EVAL      D1addsoll = XSCON
115900950705     C                   ELSE
116000950705    >C                   EVAL      D1addsoll = XSCOff
116100950705     C                   endif
116200950705     C*
116300950705    >C                   IF        PPSstcred  = *on
116400950705    >C                   EVAL      D1stcred  = XSCON
116500950705     C                   ELSE
116600950705    >C                   EVAL      D1stcred  = XSCOff
116700950705     C                   endif
116800950705     C*
116900950705    >C                   IF        PPSlegale  = *on
117000950705    >C                   EVAL      D1legale  = XSCON
117100950705     C                   ELSE
117200950705    >C                   EVAL      D1legale  = XSCOff
117300950705     C                   endif
117400950705     C*
117500980303    >C****               IF        PPSflgfin  = *on
117600980303    >C****               EVAL      D1flgfin  = XSCON
117700980303     C****               ELSE
117800980303    >C****               EVAL      D1flgfin  = XSCOff
117900980303     C****               endif
118000980303     C                   Eval      D1FlgFin = PpsFlgFin
118100950705     C*
118200971028     C                   MoveL     PPAFlgReg     D1FlgReg
118300080929     c*
118400080929A1270c* se il campo ppsboninc è impostato ad *on, emetto avvertimento
118500080929  "  c* e accendo indicatore 12 per proteggere il campo blocco partita
118600080929  "  c                   if        PpsBoninc = *on
118700080929  "  c                   seton                                        12
118800080929  "  c                   eval      IDMSG='PROMSG    *LIBL     COS2717'
118900080929  "  C                   CALLB     'XRTVM'
119000080929  "  C                   PARM                    IDMSG            27
119100080929  "  C                   PARM                    TXTMSG          100
119200080929  "  C                   PARM                    ERRMSG            1
119300080929  "  C*
119400080929  "  C     ERRMSG        IFNE      *ON
119500080929  "  c                   eval      D1BlkBonif=txtmsg
119600080929  "  c                   endif
119700080929A1270c                   endif
119800971028     C*
119900940207     C                   ENDSR
120000950403     C/EJECT
120100010105D0807C************************************************************
120200010105  "  C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
120300010105  "  C************************************************************
120400010105D0807C     ATRD1         BEGSR
120500010105     C*
120600010105     C* Questa subroutine non serve per proteggere i campi chiave
120700010105     C* in manutenzione (questa operazione avviene direttamente
120800010105     C* all'interno della subroutine INZD1, diversamente dalla
120900010105     C* classica struttura della template).
121000010105     C* E' stata successivamente inserita per proteggere il campo
121100010105     C* "Codice blocco" a seconda della presenza o meno delle
121200010105     C* autorizzazioni sul blocco all'interno della funzione CONPPA.
121300010105     C*
121400010105     C                   SETOFF                                       05
121500010105     C                   eval      in05 = *Off
121600010105      *
121700010105      * Se l'utente non è autorizzato al blocco ed il codice
121800010105      * blocco è vuoto, si protegge il campo, in modo che non
121900010105      * si possano inserire blocchi
122000010105      *
122100010105      * Per i clienti:
122200010105     C                   if        SNatura61 = 'C' and
122300010105     C                             BlcPrCl = *Off and
122400010105     C                             D1BkAmm = *blank
122500010105     C                   seton                                        05
122600010105     C                   eval      in05 = *On
122700010105     C                   endif
122800010105      *
122900010105      * Per i fornitori
123000010105     C                   if        SNatura61 = 'F' and
123100010105     C                             BlcPrFr = *Off and
123200010105     C                             D1BkAmm = *blank
123300010105     C                   seton                                        05
123400010105     C                   eval      in05 = *On
123500010105     C                   endif
123600010105      *
123700010105D0807C                   ENDSR
123800950403     C************************************************************
123900040401     C* RIEMPIMENTO CAMPI DAI DEFAULT
124000950403     C************************************************************
124100950403     C     RIEDEF        BEGSR
124200040401     c*
124300040401     c*
124400950403     C                   ENDSR
124500940916     C/EJECT
124600040401D1573C************************************************************
124700040401     C* RIEMPIMENTO da modello di primanota
124800040401D1573C************************************************************
124900040401     C     RIEDaMod      BEGSR
125000040401     c*
125100040402     C                   RESET                   NDC011DS
125200040402     C                   EVAL      Societa011 = XscSoc
125300040402     C                   EVAL      TabPNot011 = TabPNot061
125400040402     C                   EVAL      Utente011 = XscCUt
125500040402     C                   EVAL      DocIVA011 = DocIva061
125600040402     c                   move      'PP'          Opz011
125700040402     C                   MOVEL     NDC011DS      KPJBU
125800040402     C*
125900040402     C                   CALL      'NDC011R'
126000040402     C                   PARM                    KPJBA
126100040402     C*
126200040402     C                   MOVEL     KPJBU         NDC011DS
126300040402     C*
126400040402     C                   IF        Err011 <> *on
126500040402     C                   If        D1CtgAna1  =  *blanks
126600040402     c                   eval      d1Ctgana1  =  Ctgana1011
126700040402     c                   endif
126800040402     C                   If        D1CtgAna2  =  *blanks
126900040402     c                   eval      d1Ctgana2  =  ctgana2011
127000040402     c                   endif
127100040402     C                   If        D1CtgAna3  =  *blanks
127200040402     c                   eval      d1Ctgana3  =  ctgana3011
127300040402     c                   endif
127400040402     c                   endif
127500040401     c*
127600040401D1573C                   ENDSR
127700940916     C************************************************************
127800940916     C* GESTIONE F02 VIDEO D1
127900940916     C************************************************************
128000940916     C     F02D1         BEGSR
128100940916     C*
128200940916     C* per decodificare la videata richiamo la routine dei ctrl
128300940916     C* e poi disattivo gli eventuali messaggi di errore
128400940916     C                   EXSR      CTRD1
128500940916     C*
128600940916     C                   MOVE      *ALL'0'       IN5098
128700940916     C                   MOVEA     IN5098        *IN(50)
128800940916     C*
128900940916     C                   ENDSR
129000940309     C/EJECT
129100940309     C************************************************************
129200940309     C* GESTIONE F03 VIDEO D1
129300940309     C************************************************************
129400940309     C     F03D1         BEGSR
129500940309     C*
129600940309     C                   MOVE      *ON           $FINE
129700950705    >C                   MOVE      '1'           RET061
129800940325     C* fine programma
129900940325     C                   EXSR      ENDPGM
130000940325     C*
130100940325     C                   ENDSR
130200940309     C/EJECT
130300940309     C************************************************************
130400940309     C* GESTIONE F05 VIDEO D1
130500940309     C************************************************************
130600940309     C     F05D1         BEGSR
130700940309     C*
130800940309     C                   MOVE      *ON           $INZD1
130900940309     C*
131000940309     C                   ENDSR
131100940309     C/EJECT
131200940309     C************************************************************
131300940309     C* GESTIONE F08 VIDEO D1
131400940309     C************************************************************
131500940309     C     F08D1         BEGSR
131600940309     C*
131700940309     C                   MOVE      *ON           $FINE
131800950705    >C                   MOVE      '0'           RET061
131900940309     C*
132000940309     C                   ENDSR
132100940309     C/EJECT
132200940309     C************************************************************
132300940309     C* GESTIONE F12 VIDEO D1
132400940309     C************************************************************
132500940309     C     F12D1         BEGSR
132600940309     C*
132700940309     C                   MOVE      *ON           $FINE
132800950705    >C                   MOVE      '2'           RET061
132900940309     C*
133000940309     C                   ENDSR
133100990317     C/EJECT
133200990317     C************************************************************
133300990317     C* GESTIONE F20 VIDEO D1/D2 Riapertura partita
133400990317     C************************************************************
133500990317     C     F20D1         BEGSR
133600990317     C*
133700990317   B1C     *IN22         DOUEQ     *OFF
133800990317     C     MSGRTN        ORNE      'R'
133900990317     C* blocco il record
134000990317     C     K07PPA01      CHAIN     NDPPA000                           2122
134100990317     C*
134200990317     C* Gestione RCD allocato
134300990317   B2C     *IN22         IFEQ      *ON
134400990317    >C                   MOVEL     'NDPPA00F'    MSGFIL
134500990317     C                   EXSR      MSGPGM
134600990317   E2C                   ENDIF
134700990317   E1C                   ENDDO
134800990317     C* se il rcd da gestire non viene trovato
134900990317     C* oppure è allocato da altro pgm
135000990317     C* ritorno al pgm guida avvertendolo dell'errore
135100990317   B1C     *IN21         IFEQ      *ON
135200990317     C     *IN22         OREQ      *ON
135300990317     C                   MOVE      *ON           $FINE
135400990317     C                   MOVE      '1'           ERR061
135500990317     C                   EXSR      ENDPGM
135600990317     c                   else
135700990317     c                   eval      chiusa061  = *off
135800990317     c                   eval      ppaparchiu = *off
135900990317     c                   move      '0001-01-01'  ppadtchiu
136000990317     c                   except    aggppa
136100990317     c                   endif
136200990317     c*
136300990317     C                   EXSR      GesAgg
136400990317     C*
136500990317     C                   ENDSR
136600940309     C/EJECT
136700940309     C************************************************************
136800940309     C* GESTIONE F24 VIDEO D1
136900940309     C************************************************************
137000940309     C     F24D1         BEGSR
137100940506     C*
137200940506     C                   MOVEA     $FX1          $FX
137300940506     C                   MOVEA     $FC1          $FC
137400940506     C                   MOVEA     $FR1          $FR
137500940506     C                   Z-ADD     $ULKD1        $ULK
137600940506     C                   EXSR      RIEKEY
137700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
137800940720     C                   MOVEA     $FC           $FC1
137900940506     C                   MOVEL     $KEY1         Z1KE1
138000060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
138100940506     C                   MOVEL     $KEY2         Z1KE2
138200940506     C                   WRITE     FMTZ1
138300940506     C*
138400940309     C                   ENDSR
138500940207     C/EJECT
138600940131     C************************************************************
138700940207     C* CONTROLLO VIDEATA
138800950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
138900950613     C* 95 96 97 98
139000940131     C************************************************************
139100940127     C     CTRD1         BEGSR
139200940131     C*
139300940127     C                   SETOFF                                       99
139400940127     C* non imposto pos. del cursore
139500940127     C                   Z-ADD     0             H1RIGA
139600940127     C                   Z-ADD     0             H1COLO
139700940223     C*
139800940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
139900940223     C*
140000950705    >C     OPZ061        IFNE      '04'
140100960129     c*Unita
140200960129     C     FGO1          ifeq      *on
140300960129     C                   MOVE      *BLANKS       D1Unitad
140400960129     C     D1Unita       IFNE      *BLANKS
140500960129     C                   MOVE      XSCsoc        unisocieta
140600960129     C                   MOVE      D1unita       uniunita
140700960129     C     K02UNI01      chain     anuni01l                           21
140800960129     C     *in21         IFEQ      *off
140900960129     C                   MOVEL     unidesbrev    D1unitad
141000960129     C                   ELSE
141100960129     C                   SETON                                        7699
141200960129     C                   END
141300960129     C                   endif
141400960129     C*
141500960129     C* controllo autorizzazione
141600960129     C*
141700960129     C                   RESET                   XAutUniDS
141800960129     C                   EVAL      XAUPrf = KNmUs
141900960129     C                   EVAL      XAUSoc = XscSoc
142000960129     C                   EVAL      XAUUni = D1Unita
142100960129     C                   CALL      'XAUTUNI'
142200960129     C                   PARM                    XAutUniDS
142300960129     C                   IF        XAUAbl = *off
142400960129     C                   SETON                                        7799
142500960129     C                   ENDIF
142600960129     C                   END
142700950706     C*
142800950706    >C* Data partita precedente
142900950706     C*
143000950706     C     D1DtparP      IFNE      *ZERO
143100000118C1309C******dmy          test(D)                 D1DtparP               21
143200000118C1309C     *JobRun       test(D)                 D1DtparP               21
143300950706     C     *IN21         IFeq      *ON
143400950706     C                   SETON                                        5099
143500950706     C                   ENDIF
143600950706     C                   endif
143700950706     C*
143800950706    >C* Data e numero entrami o nessuno
143900950706     C*
144000950706     C                   IF        D1DTPARP <> 0 AND
144100950706     C                             D1NRPARP =  0  or
144200950706     C                             D1DTPARP =  0 AND
144300950706     C                             D1NRPARP <> 0
144400950706     C
144500950706     C                   SETON                                        5199
144600950706     C                   ENDIF
144700950706     C*
144800950706    >C* Serie valida solo se immessi gli altri
144900950706     C*
145000950706     C                   IF        D1serpARP <> *blanks and
145100950706     C                             D1dtPARP =  0
145200950706     C
145300950706     C                   SETON                                        5299
145400950706     C                   ENDIF
145500950906     C*
145600950906     C* flag modificata/riconciliate
145700950906     C                   MOVEL     *BLANK        D1MODRICD
145800950906     C     D1MODRIC      IFNE      *blanks
145900950906     C                   RESET                   X07DS
146000950906     C                   MOVE      '1'           X07RIC
146100950906     C                   MOVEL     'NDPPA000'    X07TRC
146200950906     C                   MOVEL     'PPAMODRIC'   X07CAM
146300950906     C                   MOVEL     D1modric      X07VAL
146400961011     C                   CALLb     'X07VALR'
146500950906     C                   PARM                    X07DS
146600950906   B3C     X07ERR        IFEQ      '0'
146700950906     C                   MOVEL     X07DES        D1modricd
146800950906   X3C                   ELSE
146900950906     C                   SETON                                          6799
147000950906   E3C                   ENDIF
147100950906   E3C                   ENDIF
147200990218     C*
147300990218    >C* Controlli congruenza flag riconciliata e dati partita
147400990218     C* se riconciliata
147500990218     c                   if        d1modric = '2'
147600990218    >C                   MOVE      xscsoc        PasSocieta
147700990218    >C                   MOVE      CTB061        PasCTB
147800990218    >C                   MOVE      KCC061        PasKCC
147900990218    >C                   MOVE      KSC061        PasKSC
148000990218    >C                   MOVE      DTPAR061      PasDTPAR
148100990218    >C                   MOVE      NRPAR061      PasNRPAR
148200990218    >C                   MOVE      SERPAR061     PasSERPAR
148300990218     C* non devono esistere movimenti su ndpas
148400990218     c     K07PAS03      setll     ndpas03l                               21
148500990218     c                   if        *in21 = *on
148600990218     c                   seton                                        6999
148700990218     c                   endif
148800990218     C* devono essere inseriti i dati della partita di riconciliazione
148900990218     C                   IF        D1DtParP =  0 AND
149000990218     C                             D1NrParP =  0
149100990218     c                   seton                                        6899
149200990218     c                   endif
149300990218     c                   else
149400990218     c* se non riconciliata
149500990218     c* non ci devono essere i dati della nuova partita
149600990218     C                   IF        D1DtParP <> 0 or
149700990219     C                             D1NrParP <> 0 or
149800990219     C                             D1SerParP <> *blanks
149900990218     c                   seton                                        6899
150000990218     c                   endif
150100990218     c                   endif
150200950706     C*
150300950706    >C* Data documento
150400950706     C*
150500950706     C     D1DtDOC       IFNE      *ZERO
150600000118C1309C******dmy          test(D)                 D1DtDOC                21
150700000118C1309C     *JobRun       test(D)                 D1DtDOC                21
150800950706     C     *IN21         IFeq      *ON
150900950706     C                   SETON                                        5399
151000950706     C                   ENDIF
151100950706     C                   endif
151200950706     C*
151300950706    >C* Data e numero entrami o nessuno
151400950706     C*
151500950706     C                   IF        D1DTDOC  <> 0 AND
151600950706     C                             D1NRDOC  =  0  or
151700950706     C                             D1DTDOC  =  0 AND
151800950706     C                             D1NRDOC  <> 0
151900950706     C
152000950706     C                   SETON                                        5499
152100950706     C                   ENDIF
152200950706     C*
152300950706    >C* Serie valida solo se immessi gli altri
152400950706     C*
152500950706     C                   IF        D1serDOC  <> *blanks and
152600950706     C                             D1dtDOC  =  0
152700950706     C
152800950706     C                   SETON                                        5599
152900950706     C                   ENDIF
153000950706     C*
153100950706     C* Blocco amministrativo
153200950706     C*
153300010105D0807 *
153400010105  "   * Se l'utente non è autorizzato allo sblocco ed il codice
153500010105  "   * blocco è valorizzato, si controlla che il campo non venga
153600010105  "   * pulito (cioè che la partita non venga sbloccata)
153700010105  "   *
153800010105  "   * Per i clienti:
153900010105  "  C                   if        SNatura61 = 'C' and
154000010105  "  C                             SblPrCl = *Off and
154100010105  "  C                             D1BkAmm = *blank and
154200010105  "  C                             PPSBkAmm <> *blank
154300010105  "  C                   seton                                        7099
154400010105  "  C                   endif
154500010105  "   *
154600010105  "   * Per i fornitori
154700010105  "  C                   if        SNatura61 = 'F' and
154800010105  "  C                             SblPrFr = *Off and
154900010105  "  C                             D1BkAmm = *blank and
155000010105  "  C                             PPSBkAmm <> *blank
155100010105  "  C                   seton                                        7099
155200010105  "  C                   endif
155300010105D0807 *
155400950706     C                   CLEAR                   D1BKAmmd
155500950706     C     D1BKAmm       IFNE      *BLANKS
155600950706     C                   MOVE      XSCCUT        XTBUTE
155700950706     C                   MOVE      '1'           XTBRIC
155800950706     C                   MOVE      XSCSOC        XTBAZI
155900950706     C                   MOVE      *ZERO         XTBKEY
156000950706     C                   MOVE      '0AB'         XTBCOD
156100950706     C                   MOVE      D1BKAmm       XTBKEY
156200950711     C                   CALLB     'XATB'
156300950706     C                   PARM                    XATBDS
156400950706     C     XTBERR        IFEQ      '1'
156500950706     C                   SETON                                          5699
156600950706     C                   ELSE
156700950706     C                   MOVEL     XTBUNI        TAB0AB
156800950706     C                   MOVEL     DES0AB        D1BKAmmd
156900950706     C                   ENDIF
157000950706     C                   ENDIF
157100950706     C*
157200950706     C* Tipo appoggio
157300071108D2301c* del conto
157400950706     C*
157500950706     C                   CLEAR                   D1tpappod
157600071108D2301c     D1tpappo      IFNE      *BLANKS
157700071108D2301c     d1kccfact     andeq     *blanks
157800071108D2301c     d1kscfact     andeq     *blanks
157900951121     C                   Movel     Xscsoc        COBSOC
158000951121     C                   Movel     Snatura61     COBCOF
158100951121     C                   Movel     D1Kcc         COBKCC
158200951121     C                   Movel     D1Ksc         COBKSC
158300951121     C                   Movel     D1TpAppo      COBTAP
158400951122     C     D1Dtpar       ifeq      *zero
158500951122     C                   Movel     *loval        COBDAL
158600951122     C                   Else
158700000118C1309C******dmy          Movel     D1DtPar       COBDAL
158800000118C1309C     *JobRun       Movel     D1DtPar       COBDAL
158900951122     C                   endif
159000951121     C                   Call      'DVCOBA'
159100951121     C                   Parm                    DVCOBADS
159200951121     C*
159300951121     C                   if        COBERR = *off
159400950706     C                   MOVE      XSCCUT        XTBUTE
159500950706     C                   MOVE      '1'           XTBRIC
159600950706     C                   MOVE      XSCSOC        XTBAZI
159700950706     C                   MOVE      *ZERO         XTBKEY
159800950706     C                   MOVE      'G25'         XTBCOD
159900950706     C                   MOVE      D1tpappo      XTBKEY
160000950711     C                   CALLB     'XATB'
160100950706     C                   PARM                    XATBDS
160200951121     C     XTBERR        IFNE      '1'
160300950706     C                   MOVEL     XTBUNI        TABG25
160400950706     C                   MOVEL     DESG25        D1TPAPPOD
160500950706     C                   ENDIF
160600951121     C                   Else
160700951121     C                   SETON                                          5799
160800951121     C                   ENDIF
160900951121     C                   ENDIF
161000950706     C* CONTO FACTOR
161100950706     C                   CLEAR                   D1KScfactd
161200950706     C*
161300950706     C* Controllo del conto
161400950706     C*
161500990303     C                   IF        d1kccfact <> *blanks or
161600990303     C                             d1kscfact <> *blanks
161700990301c1020C*Capoconto e sottoconto devono esistere entrambi o nessuno
161800990301c1020C                   IF        d1kccfact <> *blanks and
161900990301c1020C                             d1kscfact =  *blanks
162000990301c1020C                   SETON                                        5899
162100990303c1020c                   EndIf
162200950706     C*
162300950706     C* xallinea
162400961011     C                   IF        d1kccfact <> 'C    ' and
162500961011     C                             d1kccfact <> 'F    ' and
162600961011     C                             d1kccfact <> *blanks
162700950706     C                   CALLb     'XALLINEA'
162800950706     C                   PARM                    D1KCcfact
162900950706     C                   PARM      6             LEN               5 0
163000950706     C                   PARM                    XSCINP
163100950706   E1C                   endif
163200961011     C                   IF        d1kscfact <> *blanks
163300950706     C                   CALLb     'XALLINEA'
163400950706     C                   PARM                    D1KScfact
163500950706     C                   PARM      8             LEN               5 0
163600950706     C                   PARM                    XSCINP
163700950706     C                   endif
163800950706     C* xcapclifor
163900950706     C                   IF        d1kccfact =  'C    ' or
164000950706     C                             d1kccfact =  'F    '
164100950706     C                   CALLB     'XCAPCLIFOR'
164200950706     C                   PARM                    Xscsoc
164300950706     C                   PARM                    D1KCCFACT
164400950706     C                   PARM                    D1KSCFACT
164500950706   E2C                   endif
164600950706     C*
164700950706     C* Richiamo modulo controllo conto
164800950706     C                   Clear                   ND002DS
164900950706     C                   EVAL      $LenOut = %Size(ND002DS)
165000950706     C                   CallB     'NDMVC002'
165100950706     C                   PARM                    XSCsoc
165200950706     C                   PARM                    d1kccfact
165300950706     C                   PARM                    d1kscfact
165400950706     C                   PARM                    Wdate
165500950706     C                   PARM      *OFF          $GesMsg
165600950706     C                   PARM      *OFF          $Esito
165700950706     C                   PARM      'ND002DS'     $Struttura
165800950706     C                   PARM                    ND002DS
165900950706     C                   PARM                    $LenOut
166000950706     C* Errore
166100950706   B2C     $Esito        IFEQ      *ON
166200950706     C                   SETON                                        5899
166300950706     C                   ELSE
166400950706     C                   MOVEL     DESBREV002    D1Kscfactd
166500950706   E2C                   ENDIF
166600950706     c*
166700071108D2301C* Tipo appoggio del factor
166800071108  "  C*
166900071108  "  C                   CLEAR                   D1tpappod
167000071108  "  c     D1tpappo      IFNE      *BLANKS
167100071108  "  C                   Movel     Xscsoc        COBSOC
167200071108  "  C                   Movel     Snatura002    COBCOF
167300071108  "  C                   Movel     D1Kccfact     COBKCC
167400071108  "  C                   Movel     D1Kscfact     COBKSC
167500071108  "  C                   Movel     D1TpAppo      COBTAP
167600071108  "  C     D1Dtpar       ifeq      *zero
167700071108  "  C                   Movel     *loval        COBDAL
167800071108  "  C                   Else
167900071108  "  C     *JobRun       Movel     D1DtPar       COBDAL
168000071108  "  C                   endif
168100071108  "  C                   Call      'DVCOBA'
168200071108  "  C                   Parm                    DVCOBADS
168300071108  "  C*
168400071108  "  C                   if        COBERR = *off
168500071108  "  C                   MOVE      XSCCUT        XTBUTE
168600071108  "  C                   MOVE      '1'           XTBRIC
168700071108  "  C                   MOVE      XSCSOC        XTBAZI
168800071108  "  C                   MOVE      *ZERO         XTBKEY
168900071108  "  C                   MOVE      'G25'         XTBCOD
169000071108  "  C                   MOVE      D1tpappo      XTBKEY
169100071108  "  C                   CALLB     'XATB'
169200071108  "  C                   PARM                    XATBDS
169300071108  "  C     XTBERR        IFNE      '1'
169400071108  "  C                   MOVEL     XTBUNI        TABG25
169500071108  "  C                   MOVEL     DESG25        D1TPAPPOD
169600071108  "  C                   ENDIF
169700071108  "  C                   Else
169800071108  "  C                   SETON                                          5799
169900071108  "  C                   ENDIF
170000071108D2301C                   ENDIF
170100071108     C*
170200071108     C                   ENDIF
170300110224C2169c* se bonifico in corso per la partita. NON si può modificare
170400110224  "  c* il tipo appoggio se siamo in modalità estrazione per tipo fluss
170500110224  "  c                   if        Modestr039='1'
170600110224  "  c                             and ppsboninc=*on
170700110224  "  c                             and ppstpappo <> d1tpappo
170800110224  "  c                   seton                                        7299
170900110224c2169c                   endif
171000950706     C*
171100950706     C* Controllo codice pagamento
171200950706     C*
171300950706     C                   CLEAR                   D1CONDPAGD
171400950706     C                   IF        D1CONDPAG <> *blanks
171500950706     C                   EVAL      CDPSOCIETA = xscsoc
171600950706     C                   EVAL      CDPcondpag = d1condpag
171700950706     C     K02CDP01      CHAIN     ANCDP01L                           21
171800950706     C     *in21         IFEQ      *off
171900950706     C                   movel     cdpdesbrev    d1condpagd
172000050408D1765c* controllo uso cond. pagam.
172100050408  "  c                   Select
172200050408  "  C                   When      Snatura61='C' and
172300050408  "  c                             CdpUso1='2'
172400050408  "  C                   SETON                                        7199
172500050408  "  C                   When      Snatura61='F' and
172600050408  "  c                             CdpUso1='1'
172700050408  "  C                   SETON                                        7199
172800050408D1765C                   endsl
172900950706     C                   ELSE
173000950706     C                   SETON                                        5999
173100950706     C                   ENDIF
173200950706     C                   ENDIF
173300950706     C*
173400950706     C* Controllo pagamento manuale
173500950706     C*
173600961011     C                   if        d1pagman <> XSCON AND
173700961011     C                             D1PAGMAN <> XSCOFF
173800950706     C                   SETON                                        6699
173900950706     C                   ENDIF
174000990810C1220 *
174100990810  "   * Controllo categorie di partita
174200990810  "  C                   EXSR      CtrlCtgAna
174300990810C1220 *
174400950706     C*
174500950301     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
174600950705    >C*    OPZ061        IFEQ      '01'
174700950705    >C*    OPZ061        OREQ      '03'
174800950301     C*
174900950301     C* imposto i campi chiave e verifico l'esistenza del record
175000950301     C*
175100950705    >C*                  MOVEL     D1OBJ         ANAOBJ
175200950705    >C*                  MOVEL     D1TYP         ANATYP
175300950705    >C*                  MOVEL     D1PRG         ANAPRG
175400950705    >C*    K3ANA1        SETLL     AN___000                               21
175500950705     C*    *IN21         IFEQ      '1'
175600950705     C*                  SETON                                        5999
175700950705     C*                  ENDIF
175800950705     C*                  ENDIF
175900940224     C*
176000950706     C                   ENDIF
176100990909C1221C*
176200990909  "  C* Reperimento autorizzazioni
176300990909  "  C*
176400990909  "  C* l'autorizzazione alla particolarità partita la reperisco qui
176500990909  "  C* in quanto esse variano al variare dell'unità; per cui eseguo
176600990909  "  C* il reperimento qualora:
176700990909  "  C* . non siano già state reperite né passate dal pgm chiamante
176800990909  "  C* . siano state reperite o passate ma l'unità dell'autorizzazi=
176900990909  "  C*   one sia diversa da quella della partita (anche a rischio
177000990909  "  C*   di eseguire sempre il reperimento se l'unità dell'autoriz=
177100990909  "  C*   zazione per il profilo è ' ' cioè tutte)
177200990909  "  C                   IF        *in99 = *off
177300990909  "  C                   IF        AUTDS = *BLANK  or
177400990909  "  C                             XCAUNI <> D1Unita
177500990909  "  C                   CLEAR                   AUTDS
177600990909  "  C                   EVAL      XCAUNI = D1Unita
177700990909  "  C                   EXSR      RepAut
177800990909  "  C                   ENDIF
177900990909  "  C*
178000990909  "  C* controllo l'autorizzazione
178100990909  "  C                   EXSR      CtrlAut
178200990909C1221C                   ENDIF
178300940127     C*
178400940117     C                   ENDSR
178500940207     C/EJECT
178600990810C1220 ************************************************************
178700990810  "   * Controllo categorie di partita
178800990810  "   ************************************************************
178900990810  "  C     CtrlCtgAna    BEGSR
179000990810  "   *
179100990810  "   * I° categoria di partita
179200990810  "  C                   Eval      D1CtgAna1D  = *Blank
179300990810  "  C                   If        D1CtgAna1  <> *Blank
179400990810  "  C                   Eval      XTbRic = '1'
179500990810  "  C                   Eval      XTbAzi = XScSoc
179600990810  "  C                   Eval      XTbKey = *Zero
179700990810  "  C                   Eval      XTbCod = '0P1'
179800990810  "  C                   MOVE      D1CtgAna1     XTbKey
179900990810  "  C                   CALLB     'XATB'
180000990810  "  C                   PARM                    XAtbDS
180100990810  "  C                   If        XtbErr <> '0'
180200990810  "  C                   SetOn                                        6199
180300990810  "  C                   Else
180400990810  "  C                   Eval      D1CtgAna1D  = XTbUni
180500990810  "  C                   EndIf
180600990810  "  C                   EndIf
180700990810  "   * II° categoria di partita
180800990810  "  C                   Eval      D1CtgAna2D  = *Blank
180900990810  "  C                   If        D1CtgAna2  <> *Blank
181000990810  "  C                   Eval      XTbRic = '1'
181100990810  "  C                   Eval      XTbAzi = XScSoc
181200990810  "  C                   Eval      XTbKey = *Zero
181300990810  "  C                   Eval      XTbCod = '0P2'
181400990810  "  C                   MOVE      D1CtgAna2     XTbKey
181500990810  "  C                   CALLB     'XATB'
181600990810  "  C                   PARM                    XAtbDS
181700990810  "  C                   If        XtbErr <> '0'
181800990810  "  C                   SetOn                                        6299
181900990810  "  C                   Else
182000990810  "  C                   Eval      D1CtgAna2D  = XTbUni
182100990810  "  C                   EndIf
182200990810  "  C                   EndIf
182300990810  "   * III° categoria di partita
182400990810  "  C                   Eval      D1CtgAna3D  = *Blank
182500990810  "  C                   If        D1CtgAna3  <> *Blank
182600990810  "  C                   Eval      XTbRic = '1'
182700990810  "  C                   Eval      XTbAzi = XScSoc
182800990810  "  C                   Eval      XTbKey = *Zero
182900990810  "  C                   Eval      XTbCod = '0P3'
183000990810  "  C                   MOVE      D1CtgAna3     XTbKey
183100990810  "  C                   CALLB     'XATB'
183200990810  "  C                   PARM                    XAtbDS
183300990810  "  C                   If        XtbErr <> '0'
183400990810  "  C                   SetOn                                        6399
183500990810  "  C                   Else
183600990810  "  C                   Eval      D1CtgAna3D  = XTbUni
183700990810  "  C                   EndIf
183800990810  "  C                   EndIf
183900990810  "   *
184000990810C1220C     XCtrlCtgAn    ENDSR
184100990909C1221C/EJECT
184200990909  "  C************************************************************
184300990909  "  C* Controllo autorizzazioni
184400990909  "  C* è differente dalla TstAut perchè invece di chiudere il pgm
184500990909  "  C* emette un msg di errore
184600990909  "  C************************************************************
184700990909  "  C     CtrlAut       BEGSR
184800990909  "  C*
184900990909  "  C                   SELECT
185000990909  "  C*
185100990909  "  C* se richiamato in immissione
185200990909  "  C     OPZ061        WHENEQ    '01'
185300990909  "  C*           l'immissione non è al momento prevista
185400990909  "  C*
185500990909  "  C* se richiamato in modifica
185600990909  "  C     OPZ061        WHENEQ    '02'
185700990909  "  C*
185800990909  "  C                   SELECT
185900990909  "  C* clienti
186000990909  "  C                   WHEN      SNATURA61 = 'C'
186100990909  "  C                   IF        ManPrCl = *OFF
186200990909  "  C                   SETON                                        9499
186300990909  "  C                   ENDIF
186400990909  "  C* fornitori
186500990909  "  C                   WHEN      SNATURA61 = 'F'
186600990909  "  C                   IF        ManPrFr = *OFF
186700990909  "  C                   SETON                                        9499
186800990909  "  C                   ENDIF
186900990909  "  C                   ENDSL
187000990909  "  C*
187100990909  "  C* se richiamato in copia
187200990909  "  C     OPZ061        WHENEQ    '03'
187300990909  "  C*           l'immissione non è al momento prevista
187400990909  "  C*
187500990909  "  C* se richiamato in annullamento
187600990909  "  C     OPZ061        WHENEQ    '04'
187700990909  "  C*
187800990909  "  C                   SELECT
187900990909  "  C* clienti
188000990909  "  C                   WHEN      SNATURA61 = 'C'
188100990909  "  C                   IF        AnnPrCl = *OFF
188200990909  "  C                   SETON                                        9499
188300990909  "  C                   ENDIF
188400990909  "  C* fornitori
188500990909  "  C                   WHEN      SNATURA61 = 'F'
188600990909  "  C                   IF        AnnPrFr = *OFF
188700990909  "  C                   SETON                                        9499
188800990909  "  C                   ENDIF
188900990909  "  C                   ENDSL
189000990909  "  C*
189100990909  "  C* se richiamato in visualizzazione
189200990909  "  C     OPZ061        WHENEQ    '05'
189300990909  "  C*
189400990909  "  C                   SELECT
189500990909  "  C* clienti
189600990909  "  C                   WHEN      SNATURA61 = 'C'
189700990909  "  C                   IF        VisPrCl = *OFF
189800990909  "  C                   SETON                                        9499
189900990909  "  C                   ENDIF
190000990909  "  C* fornitori
190100990909  "  C                   WHEN      SNATURA61 = 'F'
190200990909  "  C                   IF        VisPrFr = *OFF
190300990909  "  C                   SETON                                        9499
190400990909  "  C                   ENDIF
190500990909  "  C                   ENDSL
190600990909  "  C*
190700990909  "  C* se richiamato in blocco
190800990909  "  C     OPZ061        WHENEQ    '60'
190900990909  "  C*
191000990909  "  C                   SELECT
191100990909  "  C* clienti
191200990909  "  C                   WHEN      SNATURA61 = 'C'
191300990909  "  C                   IF        BlcPrCl <> *on
191400990909  "  C                   SETON                                        9499
191500990909  "  C                   ENDIF
191600990909  "  C* fornitori
191700990909  "  C                   WHEN      SNATURA61 = 'F'
191800990909  "  C                   IF        BlcPrFr <> *on
191900990909  "  C                   SETON                                        9499
192000990909  "  C                   ENDIF
192100990909  "  C                   ENDSL
192200990909  "  C*
192300990909  "  C* se richiamato in sblocco
192400990909  "  C     OPZ061        WHENEQ    '61'
192500990909  "  C*
192600990909  "  C                   SELECT
192700990909  "  C* clienti
192800990909  "  C                   WHEN      SNATURA61 = 'C'
192900990909  "  C                   IF        SblPrCl <> *on
193000990909  "  C                   SETON                                        9499
193100990909  "  C                   ENDIF
193200990909  "  C* fornitori
193300990909  "  C                   WHEN      SNATURA61 = 'F'
193400990909  "  C                   IF        SblPrFr <> *on
193500990909  "  C                   SETON                                        9499
193600990909  "  C                   ENDIF
193700990909  "  C                   ENDSL
193800990909  "  C*
193900990909  "  C                   ENDSL
194000990909  "  C*
194100990909  "  C* controllo livello di riservatezza
194200990909  "  C                   SELECT
194300990909  "  C* clienti
194400990909  "  C     SNATURA61     WHENEQ    'C'
194500990909  "  C     LivRis061     IFGT      LvrPrCl
194600990909  "  C                   SETON                                        9499
194700990909  "  C                   ENDIF
194800990909  "  C* fornitori
194900990909  "  C     SNATURA61     WHENEQ    'F'
195000990909  "  C     LivRis061     IFGT      LvrPrFr
195100990909  "  C                   SETON                                        9499
195200990909  "  C                   ENDIF
195300990909  "  C*
195400990909  "  C                   ENDSL
195500990909  "  C*
195600990909C1221C                   ENDSR
195700940510     C************************************************************
195800940510     C* GESTIONE VIDEO RECORD  D2
195900940510     C************************************************************
196000950705    >C     GESD2         BEGSR
196100940510     C* emissione piede videata se proveniente da altra
196200940510     C* salvataggio valore formato di provenienza
196300941117   B1C     $LASTV        IFNE      'D2'
196400940510     C                   WRITE     FMTT1
196500940510     C                   EXSR      GESPIE
196600940510     C                   MOVE      $LASTV        $LASTG
196700940510     C                   MOVE      'D2'          $LASTV
196800941117   E1C                   ENDIF
196900940510     C*
197000940707     C* se esistono errori sulla videata
197100940707     C* emetto la write del formato a indicatori spenti per vedere
197200940707     C* le eventuali decodifiche
197300941117   B1C     *IN99         IFEQ      *ON
197400940707     C     $GEST         ANDEQ     'D2'
197500940707     C                   MOVEA     *IN           WIN
197600940707     C                   MOVE      *ALL'0'       IN5098           49
197700940707     C                   MOVEA     IN5098        *IN(50)
197800940707     C                   WRITE     FMTD2
197900940707     C                   MOVEA     WIN           *IN
198000941117   E1C                   ENDIF
198100940707     C*
198200941117   B1C                   SELECT
198300940510     C* Annullamento
198400950705     C     OPZ061        WHENEQ    '04'
198500940510     C* Visualizzazione
198600950705     C     OPZ061        OREQ      '05'
198700940510     C                   WRITE     FMTD2
198800940510     C                   EXFMT     PROTECT
198900940510     C* Immissione/Modifica/Copia
199000941117   X1C                   OTHER
199100940510     C                   EXFMT     FMTD2
199200941117   E1C                   ENDSL
199300941205     C*
199400940510     C* controllo abilitazione tasti funzione
199500941205     C                   EXSR      CTRKD1
199600940510     C*
199700941117   B1C     *IN99         IFEQ      *OFF
199800940510     C*
199900941117   B2C                   SELECT
200000940609     C* ROLDOWN
200100940510     C     $TASTO        WHENEQ    ROLDWN
200200940510     C                   MOVE      'D1'          $GEST
200300940510     C* F3=Fine
200400940929     C     $TASTO        WHENEQ    F03
200500940510     C                   EXSR      F03D1
200600940510     C* F4=Ricerca
200700940929     C     $TASTO        WHENEQ    F04
200800940510     C                   EXSR      SEARCH
200900940510     C* F5=Refresh
201000940929     C     $TASTO        WHENEQ    F05
201100940510     C                   EXSR      F05D2
201200940510     C* F8=Successivo
201300940929     C     $TASTO        WHENEQ    F08
201400940510     C                   EXSR      F08D1
201500940510     C* F12=Ritorno
201600940929     C     $TASTO        WHENEQ    F12
201700940510     C                   EXSR      F12D1
201800990317     C* F20=Riapertura partita
201900990317     C     $TASTO        WHENEQ    F20
202000990317     C                   EXSR      F20D1
202100940510     C* F24=Altri tasti
202200940929     C     $TASTO        WHENEQ    F24
202300940510     C                   EXSR      F24D2
202400940510     C*
202500941117   X2C                   OTHER
202600940510     C                   EXSR      CTRD2
202700941117   B3C     *IN99         IFEQ      *OFF
202800940510     C                   EXSR      CTRD1
202900941117   B4C     *IN99         IFEQ      *ON
203000940510     C                   SETON                                            96
203100941117   E4C                   ENDIF
203200941117   E3C                   ENDIF
203300940510     C*
203400940510     C*
203500941117   B3C     $TASTO        IFEQ      F06
203600940510     C     *IN99         ANDEQ     *OFF
203700940510     C* eseguo aggiornamento
203800951121     C                   If        TpappoOld<>D1Tpappo
203900951121     C                   EXSR      CONFEFF
204000951121     C                   endif
204100951120     C                   EXSR      AGGANA
204200941117     C* eseguo operazioni del dopo-aggiornamento
204300941117   B4C     *IN99         IFEQ      *OFF
204400941117     C                   EXSR      GESAGG
204500941117   E4C                   ENDIF
204600941117   E3C                   ENDIF
204700940510     C*
204800941117   E2C                   ENDSL
204900940510     C*
205000941117   E1C                   ENDIF
205100941027     C*
205200950705    >C                   ENDSR
205300940510     C/EJECT
205400940510     C************************************************************
205500940510     C* RIEMPIMENTO VIDEATA D2
205600940510     C************************************************************
205700950705    >C     RIED2         BEGSR
205800940510     C*
205900940510     C*
206000950705    >C                   ENDSR
206100940510     C/EJECT
206200940510     C************************************************************
206300940510     C* GESTIONE F05 VIDEO D2
206400940609     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
206500940609     C* e il riposizionamento alla prima
206600940510     C************************************************************
206700950705    >C     F05D2         BEGSR
206800940510     C*
206900940510     C                   MOVE      *ON           $INZD1
207000940510     C                   MOVE      'D1'          $GEST
207100940510     C*
207200950705    >C                   ENDSR
207300940510     C************************************************************
207400940510     C* GESTIONE F24 VIDEO D2
207500940510     C************************************************************
207600950705    >C     F24D2         BEGSR
207700940510     C*
207800940722     C                   MOVEA     $FX1          $FX
207900940722     C                   MOVEA     $FC1          $FC
208000940722     C                   MOVEA     $FR1          $FR
208100940722     C                   Z-ADD     $ULKD1        $ULK
208200940510     C                   EXSR      RIEKEY
208300940722     C                   Z-ADD     $ULK          $ULKD1            3 0
208400940722     C                   MOVEA     $FC           $FC1
208500940722     C                   MOVEL     $KEY1         Z1KE1
208600060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
208700940722     C                   MOVEL     $KEY2         Z1KE2
208800940722     C                   WRITE     FMTZ1
208900941117     C*
209000950705    >C                   ENDSR
209100940510     C************************************************************
209200940510     C* CONTROLLO VIDEATA  D2
209300950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
209400950613     C* 95 96 97 98
209500940510     C************************************************************
209600950705    >C     CTRD2         BEGSR
209700940510     C*
209800940510     C                   SETOFF                                       99
209900940510     C* non imposto pos. del cursore
210000940510     C                   Z-ADD     0             H1RIGA
210100940510     C                   Z-ADD     0             H1COLO
210200940510     C*
210300940510     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
210400940510     C*
210500950705     C     OPZ061        IFNE      '04'
210600960129     C*
210700960129     C* agente
210800960129     C*
210900960129     C                   CLEAR                   D1KSCAGED
211000960129     C     D1KSCAGE      ifne      *BLANKS
211100960129     C                   CALLb     'XALLINEA'
211200960129     C                   PARM                    D1KScage
211300960129     C                   PARM      8             LEN               5 0
211400960129     C                   PARM                    XSCINP
211500960129     C*
211600960129     C                   MOVE      '1'           DAGRIC
211700960129     C                   MOVE      XSCSOC        DAGSOC
211800960129     C                   MOVE      XSCDUN        DAGUNI
211900960129     C                   MOVE      '1'           DAGVIS
212000960129     C                   MOVE      *OFF          DAGALC
212100960129     C                   MOVE      1             DAGNRK
212200960129     C                   MOVE      'DVOAGEDS'    DAGSET
212300960129     C                   CLEAR                   DAGDTA
212400960129     C                   MOVEL     D1KSCAGE      DAGKSC
212500960129     C                   MOVE      *OFF          DAGERR
212600960129     C                   CALL      'DVAGE'
212700960129     C                   PARM                    DVAGE
212800960129     C                   PARM                    DVOAGE
212900960129     C*
213000960129     C     DAGERR        IFNE      *OFF
213100960129     C                   SETON                                        6099
213200960129     C                   ELSE
213300960129     C                   MOVEL     ageragsoc     D1KSCAGED
213400960129     C                   ENDIF
213500960129     C                   ENDIF
213600960129     C*
213700960129     C* Cliente da sollecitare
213800960129     C*
213900961011     C                   if        d1CLISOLL <> XSCON AND
214000961011     C                             D1CLISOLL <> XSCOFF
214100960129     C                   SETON                                        6199
214200960129     C                   ENDIF
214300960129     C*
214400960129     C* Addebito interessi
214500960129     C*
214600961011     C                   if        d1addSOLL <> XSCON AND
214700961011     C                             D1addSOLL <> XSCOFF
214800960129     C                   SETON                                        6299
214900960129     C                   ENDIF
215000960129     C*
215100960129     C* star del credere
215200960129     C*
215300961011     C                   if        d1stcred  <> XSCON AND
215400961011     C                             D1stcred  <> XSCOFF
215500960129     C                   SETON                                        6399
215600960129     C                   ENDIF
215700960129     C*
215800960129     C* contenzioso/legale
215900960129     C*
216000961011     C                   if        d1legale  <> XSCON AND
216100961011     C                             D1legale  <> XSCOFF
216200960129     C                   SETON                                        6499
216300960129     C                   ENDIF
216400960129     C*
216500960129     C* fattura finanziaria
216600960129     C*
216700980303     C                   Clear                   D1FlgFinD
216800980303     C                   RESET                   X07DS
216900980303     C                   MOVE      '1'           X07RIC
217000980303     C                   MOVEL     'NDPPS000'    X07TRC
217100980303     C                   MOVEL     'PPSFLGFIN'   X07CAM
217200980303     C                   MOVEL     D1FlgFin      X07VAL
217300980303     C                   MOVE      *ON           X07ALL
217400980303     C                   CALLb     'X07VALR'
217500980303     C                   PARM                    X07DS
217600980303   B3C     X07ERR        IFEQ      '0'
217700980303     C                   MOVEL     X07DES        D1FlgFinD
217800980303     C                   ELSE
217900980303     C                   SetOn                                        6599
218000980303   E3C                   ENDIF
218100980303     C*****              if        d1flgfin  <> XSCON AND
218200980303     C*****                        D1flgfin  <> XSCOFF
218300980303     C*****              SETON                                        6599
218400980303     C*****              ENDIF
218500940510     C*
218600941027     C                   ENDIF
218700940510     C*
218800940510     C                   ENDSR
218900951120     C/EJECT
219000951120     C************************************************************
219100951120     C* AGGIORNAMENTO EFFETTI
219200951120     C************************************************************
219300951121     C     CONFEFF       BEGSR
219400070306D2129C                   move      *on           $Aggio            1
219500070306     c*
219600070306D2129c* mando messaggio solo se esistono effetti
219700070306  "  C                   if        Snatura61 = 'C'
219800070306  "  C                   Movel     D1Kcc         EFAKCC
219900070306  "  C                   Movel     D1Ksc         EFAKSC
220000070306  "  C                   Movel     Xscsoc        EFASocieta
220100070306  "  C                   Movel     Ctb061        EFACTB
220200070306  "  C     D1Dtpar       ifeq      *zero
220300070306  "  C                   Movel     *loval        EfaDtPAr
220400070306  "  C                   Else
220500070306  "  C     *JobRun       Movel     D1DtPar       EfaDtPar
220600070306  "  C                   endif
220700070306  "  C                   Movel     D1SeRPar      EFASerPAr
220800070306  "  C                   Z-add     D1NrPar       EFANrPAr
220900070306  "  C     K07EFA07      Setll     NDEFA07L
221000070306  "  c                   if        not %equal(ndefa07l)
221100070306  "  c                   eval      $Aggio=*off
221200070306  "  c                   endif
221300070306  "  c                   else
221400070306  "  C                   Movel     D1Ksc         EFPKSC
221500070306  "  C                   Movel     Xscsoc        EFPSocieta
221600070306  "  C                   Movel     Ctb061        EFPCTB
221700070306  "  C     D1Dtpar       ifeq      *zero
221800070306  "  C                   Movel     *loval        EFPDTPAR
221900070306  "  C                   Else
222000070306  "  C     *JobRun       Movel     D1DtPar       EFPDTPAR
222100070306  "  C                   endif
222200070306  "  C                   Movel     D1SeRPar      EFPSerPAr
222300070306  "  C                   Z-add     D1NrPar       EFPNrPAr
222400070306  "  C     K06EFP07      Setll     NDEFP07L
222500070306  "  c                   if        not %equal(ndefp07l)
222600070306  "  c                   eval      $Aggio=*off
222700070306  "  c                   endif
222800070306D2129c                   endif
222900951120     C*
223000070306D2129c                   If        $Aggio=*on
223100951121     C                   CLEAR                   XMSGDS
223200951121     C     1             DO        2             X
223300951121     C     TXT(X)        IFNE      *BLANK
223400951121     C                   MOVEL     TXT(X)        IDMSG
223500951121     C                   CALLB     'XRTVM'
223600951121     C                   PARM                    IDMSG            27
223700951121     C                   PARM                    TXTMSG          100
223800951121     C                   PARM                    ERRMSG            1
223900951121     C     ERRMSG        IFNE      *ON
224000951121     C                   MOVEL     TXTMSG        MSGT(X)
224100951121     C                   ELSE
224200951121     C                   MOVEL     *ALL'?'       MSGT(X)
224300951121     C                   ENDIF
224400951121     C                   ENDIF
224500951121     C                   ENDDO
224600951121
224700951121     C                   MOVEL     MSGT(1)       MSGMS1
224800951121     C                   MOVEL     MSGT(2)       MSGMS2
224900951121     C                   eval      XMSGTXT  = Testi
225000951121     C                   Z-ADD     0             MSGMSG
225100951121     C                   MOVEL     'S'           MSGRSP
225200951121     C                   MOVEL     '12'          MSGTPR
225300951121     C                   MOVEL     'S'           MSGEMV
225400951121     C                   Z-ADD     10            MSGRGP
225500951121     C                   MOVEL     'N'           MSGLCK
225600951121     C                   MOVEA     STATUS        STS
225700951121     C                   MOVEL     'N'           MSGCNL
225800951121     C                   MOVEL     'N'           MSGVID
225900951121     C                   MOVEL     'N'           MSGOPE
226000951121     C                   MOVEL     'N'           MSGSTP
226100951121     C                   CALL      'XMSGR'
226200951121     C                   PARM                    XMSGDS
226300951121     C                   PARM                    XMSGTXT         135
226400951121     C                   If        MsgRtn = '1'
226500951121     C* Aggiornamento effetti
226600951121     C                   Exsr      AGGEFF
226700951121     C                   Endif
226800070306D2129c                   endif
226900951121      *
227000951121     C                   ENDSR
227100951121     C/EJECT
227200951121     C************************************************************
227300951121     C* AGGIORNAMENTO EFFETTI  ATTIVI
227400951121     C************************************************************
227500951121     C     AGGEFA        BEGSR
227600951121     C*
227700951121     C                   setoff                                       2122
227800951122     C     K07EFA07      Setll     NDEFA07L
227900951121     C     *in21         Doweq     *off
228000951121     C*
228100951121   B1C     *IN22         DOUEQ     *OFF
228200951121     C     MSGRTN        ORNE      'R'
228300951122     C     K07EFA07      READE     NDEFA07L                             2221
228400951121     C*
228500951121     C* Gestione RCD allocato
228600951121   B2C     *IN22         IFEQ      *ON
228700951121    >C                   MOVEL     'NDEFA00F'    MSGFIL
228800951121     C                   EXSR      MSGPGM
228900951121   E2C                   ENDIF
229000951121   E1C                   ENDDO
229100951121     C* oppure è allocato da altro pgm
229200951121     C* ritorno al pgm guida avvertendolo dell'errore
229300951122   B1C     *IN22         IFEQ      *ON
229400951121     C                   MOVE      *ON           $FINE
229500951121     C                   MOVE      '1'           ERR061
229600951121     C                   EXSR      ENDPGM
229700951122     C                   Endif
229800990317     c*
229900990317     C* Aggiorno solo se non presentato
230000990317     C                   if        *in21 = *off and Efastatus <> '4'
230100951121     C                   Movel     D1TpAppo      EFATpAppo
230200990317     C                   Movel     CobAbi        EFAAbi
230300990317     C                   Movel     CobCab        EFACab
230400951121     C                   Update    NDEFA000
230500951121     C                   endif
230600951121
230700951121     C                   EndDO
230800951121     C*
230900951121     C                   Endsr
231000951122     C/EJECT
231100951122     C************************************************************
231200951122     C* AGGIORNAMENTO EFFETTI  PASSIVI
231300951122     C************************************************************
231400951122     C     AGGEFP        BEGSR
231500951122     C*
231600951122     C                   setoff                                       2122
231700951122     C     K06EFP07      Setll     NDEFP07L
231800951122     C     *in21         Doweq     *off
231900951122     C*
232000951122   B1C     *IN22         DOUEQ     *OFF
232100951122     C     MSGRTN        ORNE      'R'
232200951122     C     K06EFP07      Reade     NDEFP07L                             2221
232300951122     C*
232400951122     C* Gestione RCD allocato
232500951122   B2C     *IN22         IFEQ      *ON
232600951122    >C                   MOVEL     'NDEFA00F'    MSGFIL
232700951122     C                   EXSR      MSGPGM
232800951122   E2C                   ENDIF
232900951122   E1C                   ENDDO
233000951122     C* se il rcd da gestire non viene trovato
233100951122     C* oppure è allocato da altro pgm
233200951122     C* ritorno al pgm guida avvertendolo dell'errore
233300951122     C     *IN22         IfEQ      *ON
233400951122     C                   MOVE      *ON           $FINE
233500951122     C                   MOVE      '1'           ERR061
233600951122     C                   EXSR      ENDPGM
233700951122     C                   endif
233800951122     C*
233900951122     C                   if        *in21 = *off and EFPSTATUS='1'
234000951122     C                   Movel     Banca016      EFPBanca
234100951122     C                   Movel     Linea016      EFPLinea
234200951122     C                   Movel     Kcc016        EFPKccPres
234300951122     C                   Movel     Ksc016        EFPKscPres
234400951122     C                   Update    NDEFP000
234500951122     C                   endif
234600951122
234700951122     C                   EndDO
234800951122     C*
234900951122     C                   Endsr
235000951121     C/EJECT
235100951121     C************************************************************
235200951121     C* AGGIORNAMENTO EFFETTI
235300951121     C************************************************************
235400951121     C     AGGEFF        BEGSR
235500951121     C*
235600951121     C                   Movel     XscSoc        COBSOC
235700951121     C                   Movel     Snatura61     COBCOF
235800951121     C                   Movel     D1Kcc         COBKCC
235900951121     C                   Movel     D1Ksc         COBKSC
236000951121     C                   Movel     D1TpAppo      COBTAP
236100951122     C     D1Dtpar       ifeq      *zero
236200951122     C                   Movel     *loval        COBDAL
236300951122     C                   Else
236400000118C1309C******dmy          Movel     D1DtPar       COBDAL
236500000118C1309C     *JobRun       Movel     D1DtPar       COBDAL
236600951122     C                   endif
236700951121     C                   Call      'DVCOBA'
236800951121     C                   Parm                    DVCOBADS
236900951121     C
237000951121     C                   if        Snatura61 = 'C'
237100951121     C                   Movel     D1Kcc         EFAKCC
237200951121     C                   Movel     D1Ksc         EFAKSC
237300951121     C                   Movel     Xscsoc        EFASocieta
237400951121     C                   Movel     Ctb061        EFACTB
237500951122     C     D1Dtpar       ifeq      *zero
237600951122     C                   Movel     *loval        EfaDtPAr
237700951122     C                   Else
237800000118C1309C******dmy          Movel     D1DtPar       EfaDtPar
237900000118C1309C     *JobRun       Movel     D1DtPar       EfaDtPar
238000951122     C                   endif
238100951121     C                   Movel     D1SeRPar      EFASerPAr
238200951121     C                   Z-add     D1NrPar       EFANrPAr
238300951121     C                   Exsr      AGGEFA
238400951121     C*
238500951121     C                   Else
238600951121     C* reperisco banca e linea
238700951121     C*
238800951121     C                   Eval      Operazion = '01'
238900951121     C                   Eval      Struttura  = 'ND016DS'
239000951121     C                   Eval      LenOut    = %Size(ND016DS)
239100951121     C                   Callb     'NDDVABRA'
239200951121     C                   PARM                    Operazion
239300951121     C                   PARM                    XscSoc
239400951121     C                   PARM                    *Omit
239500951121     C                   PARM                    *omit
239600951121     C                   PARM                    CobKcb
239700951121     C                   PARM                    CobKsb
239800951121     C                   PARM                    Struttura
239900951121     C                   PARM                    LenOut
240000951121     C                   PARM                    Esito
240100951121     C                   PARM                    ND016DS
240200951122     C*
240300951122     C                   Movel     D1Ksc         EFPKSC
240400951122     C                   Movel     Xscsoc        EFPSocieta
240500951122     C                   Movel     Ctb061        EFPCTB
240600951122     C     D1Dtpar       ifeq      *zero
240700951122     C                   Movel     *loval        EFPDTPAR
240800951122     C                   Else
240900000118C1309C******dmy          Movel     D1DtPar       EFPDTPAR
241000000118C1309C     *JobRun       Movel     D1DtPar       EFPDTPAR
241100951122     C                   endif
241200951122     C                   Movel     D1SeRPar      EFPSerPAr
241300951122     C                   Z-add     D1NrPar       EFPNrPAr
241400951122     C                   Exsr      AGGEFP
241500951121     C*
241600951121     C                   endif
241700951121     C*
241800951121     C                   ENDSR
241900940224     C/EJECT
242000940224     C************************************************************
242100950301     C* AGGIORNAMENTO ANAGRAFICA
242200940224     C************************************************************
242300941214     C     AGGANA        BEGSR
242400940207     C*
242500950907     C                   SELECT
242600950907     C     OPZ061        WHENEQ    '60'
242700950907     C                   eval      D1BKAMM = BKAMM061
242800950907     C                   eval      OPZ061 = '02'
242900950907     C     OPZ061        WHENEQ    '61'
243000950907     C                   eval      D1BKAMM = *blanks
243100950907     C                   eval      OPZ061 = '02'
243200950907     C                   ENDSL
243300950907     C*
243400940207     C                   SELECT
243500950705    >C     OPZ061        WHENEQ    '01'
243600950705    >C     OPZ061        OREQ      '03'
243700940318     C* se rcd non scrivibile attivo errore
243800940318     C     *IN22         IFEQ      *ON
243900941201     C                   SETON                                        9599
244000941027     C                   ENDIF
244100950706     C*
244200950706     C* Blocco il record di nuovo, perchè lo avevo rilasciato
244300950705    >C     OPZ061        WHENEQ    '02'
244400950706    >C     OPZ061        OREQ      '04'
244500950706     C*
244600950706     c* aggancio archivi
244700950706     C                   EXSR      CHAINANA
244800950706     c*
244900950706     C     OPZ061        IFEQ      '02'
245000950706     C                   EXSR      RIEANA
245100950706    >C                   UPDATE    NDPPA000
245200950706    >C                   UPDATE    NDPPS000
245300950706     C                   endif
245400950706    >C     OPZ061        IFEQ      '04'
245500950706    >C                   DELETE    NDPPA000
245600950706    >C                   DELETE    NDPPS000
245700950706     C                   endif
245800940207     C                   ENDSL
245900940203     C*
246000950310     C* richiama pgm User Exit
246100950310     c*
246200950511     C     USRERR        IFEQ      *BLANK
246300950310     C* imposta chiave univoca  per user exit
246400950314     C                   MOVE      XSCSOC        XUKSOC
246500950512     C                   MOVE      XSCDUN        XUKDUN
246600950328     C                   MOVEL     DSPGM         XUKPGM
246700950705    >C                   MOVEL     PPSSYS        XUKFL1
246800950705    >C                   MOVEL     PPSNRASINT    XUKFL2
246900950404     C                   MOVEL     XUKDS         KPJBU
247000950328     C     DSPGM         CAT       'U':0         USREXT           10
247100950310     C                   CALL      USREXT                               21
247200950310     C                   PARM                    KPJBA
247300950310     C     *IN21         IFEQ      *ON
247400950310     C                   MOVE      *ON           USRERR            1
247500950310     C                   ENDIF
247600950310     C                   ENDIF
247700040401     C*
247800940203     C                   ENDSR
247900940224     C/EJECT
248000940224     C************************************************************
248100940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
248200940224     C************************************************************
248300941214     C     RIEANA        BEGSR
248400950705     C*
248500950705     C                   MOVE      XSCSOC        PPASOCIETA
248600950906    >C                   MOVEL     CTB061        PPACTB
248700950705    >C                   MOVEL     KCC061        PPAKCC
248800950705    >C                   MOVEL     KSC061        PPAKSC
248900950705    >C                   MOVEL     DTPAR061      PPADTPAR
249000950705    >C                   MOVEL     NRPAR061      PPANRPAR
249100950705    >C                   MOVEL     SERPAR061     PPASERPAR
249200940224     C*
249300950301     C* imposto i campi del file da quelli video
249400960129     c                   eval      ppaunita = d1unita
249500950906     C                   eval      ppamodric = d1modric
249600950705     C                   eval      ppadivisa = d1divisa
249700950705     C     d1parchiu     IFEQ      xscon
249800950705     C                   eval      ppaparchiu = *on
249900950705     C                   ELSE
250000950705     C                   eval      ppaparchiu = *off
250100950705     C                   ENDIF
250200950705     C     D1DTchiu      IFNE      0
250300000118C1309C******dmy          move      d1dtchiu      ppadtchiu
250400000118C1309C     *JobRun       move      d1dtchiu      ppadtchiu
250500950705     C                   ELSE
250600950705     C                   MOVE      *loval        ppadtchiu
250700950705     c                   endif
250800950705     C     d1DTulmo      IFNE      0
250900000118C1309C******dmy          move      d1dtulmo      ppadtulmo
251000000118C1309C     *JobRun       move      d1dtulmo      ppadtulmo
251100950705     C                   ELSE
251200950705     C                   MOVE      *loval        ppadtulmo
251300950705     c                   endif
251400950707     C     D1DTPARP      IFNE      0
251500000118C1309C******dmy          move      D1dtparp      PPAdtparp
251600000118C1309C     *JobRun       move      D1dtparp      PPAdtparp
251700950705     C                   ELSE
251800950705     C                   MOVE      *loval        ppadtparp
251900950705     c                   endif
252000950705    >C                   EVAL      ppaNRPArp = d1nrparp
252100950705    >C                   EVAL      ppaserpArp = d1serparp
252200950705     C     d1DTDOC       IFNE      0
252300000118C1309C******dmy          move      d1dtDOC       ppsdtDOC
252400000118C1309C     *JobRun       move      d1dtDOC       ppsdtDOC
252500950705     C                   ELSE
252600950705     C                   MOVE      *loval        ppsdtdoc
252700950705     c                   endif
252800950705    >C                   EVAL      ppSNRdoc  = D1nrdoc
252900950705    >C                   EVAL      ppSserdoc = D1serdoc
253000950705    >C                   EVAL      ppSbkamm  = D1bkamm
253100080929A1270c* flag bonifici in corso NONN SI DEVE TOCCARE, perchè GESTITO
253200080929  "  C* dalla procedura BONIFICI, deve rimanere impostato come è
253300080929  "  x***                move      *off          ppsboninc
253400080929  "  x* ex C1973
253500080929  "  x* si distinguono i seguenti 3 casi:
253600080929  "  x* 1) se partita non bloccata, allora non può essere in corso di
253700080929  "  x*    bonifico
253800080929  "  x* 2) se partita bloccata con blocco diverso da quello per i
253900080929  "  x*    bonifici allora non può essere estratta per i bonifici
254000080929  "  x*    (e quindi non può essere in corso di bonifico....)
254100080929  "  x* 3) se partita bloccata con blocco uguale a quello per i
254200080929  "  x*    bonifici allora la partita è in corso di bonifico
254300080929  "  x*
254400080929  "  x*                  if        ppsbkamm <> *blanks
254500080929  "  x*
254600080929  "  x*                  If        BkAmm039   <> *blanks    And
254700080929  "  x*                            BkAmm039    = PPSBkAmm
254800080929  "  x*                  Eval      PpsBonInc   = *On
254900080929A1270x*                  EndIf
255000080929     x*
255100080929     x**se italia
255200080929   2 x***                if        savitaest = '0'
255300080929     x***                eval      ndchh1 = ndchh1ita
255400080929   3 x***                if        h1blc <> *blanks and h1blc = ppsbkamm
255500080929     x***                eval      ppsboninc = *on
255600080929   3ex***                endif
255700080929     x***
255800080929   2xx***                else
255900080929     x**se estero
256000080929     x***                eval      ndchh1 = ndchh1est
256100080929   3 x***                if        h1blc <> *blanks and h1blc = ppsbkamm
256200080929     x***                eval      ppsboninc = *on
256300080929   3ex***                endif
256400080929   2ex***                endif
256500080929A1270x***                endif
256600981109      *
256700950705    >C                   EVAL      ppSctgana1 = D1ctgana1
256800950705    >C                   EVAL      ppSctgana2 = D1ctgana2
256900950705    >C                   EVAL      ppSctgana3 = D1ctgana3
257000950705    >C                   EVAL      ppStpappo  = D1tpappo
257100950705    >C                   EVAL      ppSkccfact = D1kccfact
257200950705    >C                   EVAL      ppSkscfact = D1kscfact
257300950705    >C                   EVAL      ppScondpag = D1condpag
257400950705    >C                   IF        d1PAGMAN  = xscon
257500950705    >C                   EVAL      ppsPAGMAN  = *on
257600950705     C                   ELSE
257700950705    >C                   EVAL      ppsPAGMAN  = *off
257800950705     C                   endif
257900950705     C*
258000950705     C*  Campi solo per clienti
258100950705     C*
258200950705    >C                   EVAL      ppsKSCAGE  = d1KSCAGE
258300950705     C*
258400950705    >C                   IF        d1CLISOLL = xscon
258500950705    >C                   EVAL      ppsclisoll = *on
258600950705     C                   ELSE
258700950705    >C                   EVAL      ppsclisoll = *off
258800950705     C                   endif
258900950705     C*
259000950705    >C                   IF        d1addSOLL = xscon
259100950705    >C                   EVAL      ppsaddsoll = *on
259200950705     C                   ELSE
259300950705    >C                   EVAL      ppsaddsoll = *off
259400950705     C                   endif
259500950705     C*
259600950705    >C                   IF        d1stcred  = xscon
259700950705    >C                   EVAL      ppsstcred  = *on
259800950705     C                   ELSE
259900950705    >C                   EVAL      ppsstcred  = *off
260000950705     C                   endif
260100950705     C*
260200950705    >C                   IF        d1legale  = xscon
260300950705    >C                   EVAL      ppslegale  = *on
260400950705     C                   ELSE
260500950705    >C                   EVAL      ppslegale  = *off
260600950705     C                   endif
260700950705     C*
260800980303    >C****               IF        d1flgfin  = XSCON
260900980303    >C****               EVAL      ppsflgfin  = *on
261000980303     C****               ELSE
261100980303    >C****               EVAL      ppsflgfin  = *off
261200980303     C****               endif
261300980303     C                   Eval      PpsFlgFin = D1FlgFin
261400950301     C*
261500971028     C                   MoveL     D1FlgReg      PPAFlgReg
261600971028     C*
261700940207     C                   ENDSR
261800940224     C/EJECT
261900940224     C************************************************************
262000940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
262100940224     C************************************************************
262200940224     C     GESAGG        BEGSR
262300941214     C*
262400941214     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
262500950705    >C     CMT061        IFEQ      '0'
262600941214     C                   COMMIT
262700941214     C                   ENDIF
262800940224     C*
262900940224     C* segnalo al pgm chiamante l'avvenuta conferma
263000950705    >C                   MOVE      *ON           OPR061
263100940224     C* segnalo al pgm chiamante l'aver premuto l'F6
263200950705    >C                   MOVE      '0'           RET061
263300940224     C*
263400941027     C                   SELECT
263500950301     C*
263600950301     C* nel caso di immissione
263700950705    >C     OPZ061        WHENEQ    '01'
263800950301     C* ripristino la videata iniziale
263900940224     C                   MOVE      *ON           $INZD1
264000941228     C                   MOVE      'D1'          $GEST
264100950301     C*
264200950301     C* nel caso di modifica
264300950705    >C     OPZ061        WHENEQ    '02'
264400950301     C* ripasso i campi che devono essere reimpostati sul sfl
264500950705     C                   MOVE      PPSDTDOC      dtdoc061
264600950705    >C                   MOVE      PPSNRDOC      NRDOC061
264700950705    >C                   MOVE      PPSSERDOC     SERDOC061
264800950906    >C                   MOVE      PPSbkamm      bkamm061
264900960313    >C                   MOVE      PPScondpag    condpag061
265000960313    >C                   MOVE      PPaunita      unita061
265100960313    >C                   MOVE      PPSkscage     kscage061
265200950301     C* e ritorno al pgm chiamante
265300950301     C                   MOVE      *ON           $FINE
265400950301     C*
265500950301     C* per tutte le altre opzioni
265600940224     C                   OTHER
265700950301     C* ritorno al pgm chiamante
265800940224     C                   MOVE      *ON           $FINE
265900941027     C                   ENDSL
266000940224     C*
266100981221     C* Se avvenuto aggiornamento e il conto è gestito a "estensione
266200981221     C*  nominativi partite" (RCOTecnico='2') allora chiamo pgm apposito
266300000411D0525 *  a meno che non si stia eseguendo una cancellazione
266400981221     C                   If        Opr061 = *On and
266500000911     C                             Tecnico002 = '2'
266600000911D0525C                             and Opz061 <> '04'
266700000911     C* verifico esistenza ...
266800981221     C     K02CPA01      SETLL     NDCPA01L                               22
266900981221     C                   If        *in22 = *On
267000981221     C                   MOVE      '02'          OPZ0661
267100981221     C                   Else
267200981221     C                   MOVE      '01'          OPZ0661
267300981221     C                   EndIf
267400981221     C                   MOVE      D1KCC         KCC0661
267500981221     C                   MOVE      D1KSC         KSC0661
267600981221     C                   MOVEL     D1KSCD        DESBRE0661
267700981221     C     D1DTPAR       IFGT      0
267800000118C1309C******DMY          MOVE      D1DTPAR       DTPAR0661
267900000118C1309C     *JobRun       MOVE      D1DTPAR       DTPAR0661
268000981221     C                   ELSE
268100981221     C                   MOVE      *loval        DTPAR0661
268200981221     C                   ENDIF
268300981221     C                   MOVE      D1NRPAR       NRPAR0661
268400981221     C                   MOVE      D1SERPAR      SERPAR0661
268500981221     C* imposto chiave unique
268600981221     C                   MOVE      PPASYS        SYS0661
268700981221     C                   MOVE      PPANRASINT    NRASIN0661
268800981221     C*
268900981221     C                   MOVE      *BLANK        KPJBU
269000981221     C                   MOVEL     NDC066DS1     KPJBU
269100981221     C                   CALL      'NDC066R1'
269200981221     C                   PARM                    KPJBA
269300981221     C                   MOVEL     KPJBU         NDC066DS1
269301170223R224 X*ex R027
269302170223  "  X*ex R027           if        OPZ0661     = '02'       or
269303170223  "  X*ex R027                     OPR0661     = '1'
269304170223  "  X*ex R027           EXSR      CallDatiAgg
269305170223  "  X*ex R027           endif
269306170223R224 X*ex R027
269400000411     C                   EndIf
269500000411D0525 *
269600000911  "   * Se avvenuto aggiornamento e sto cancellando NdPpa, controllo
269700000911  "   *  l'eventuale esistenza di record di NdCpA e di NdMix collegati.o
269800000911  "   *  Se esistono li cancello.
269900000411  "  C                   if        Opr061 = *On
270000000411  "  C                             and Opz061 = '04'
270100000911  "   *  NDCPA
270200000911  "  C     K02Cpa01      setll     NdCpa01l
270300000911  "  C     K02Cpa01      reade     NdCpa01l                               21
270400000911  "  C                   if        *in21 = *Off
270500000911  "  C                   delete    NdCpa000
270600000911  "  C                   endif
270601170223D0525 *
270602170223R027 C                   Eval      SO3Sogg    = *Blank
270603170223  "  C                   Eval      SO3DtFiVl  = *Loval
270604170223  "  C                   Eval      SO3Sys     = PPASys
270605170223  "  C                   Eval      SO3NrAsInt = PPANrAsInt
270606170223  "  C     K04SO301      SETLL     SDGSO301L
270607170223  "  C     K04SO301      READE     SDGSO301L                              21
270608170223R224 X*ex R027           if        *in21 = *Off
270609170223R224 C                   dow       *in21 = *Off
270610170223R027 C                   delete    SDGSO3000
270611170223R224 X*ex R027           endif
270612170223 "   C     K04SO301      READE     SDGSO301L                              21
270613170223R224 C                   enddo
270614170223R027  *
270615170223D0525 *  NDMIX
270900000411  "  C     K02Mix01      setll     NdMix01l
271000000411  "  C     K02Mix01      reade     NdMix01l                               21
271100000411  "  C                   dow       *in21 = *Off
271200000411  "  C                   delete    NdMix000
271300000411  "  C     K02Mix01      reade     NdMix01l                               21
271400000411  "  C                   enddo
271500000411D0525C                   endif
271600981221     C*
271700940224     C                   ENDSR
271800941205     C/EJECT
271801170223R224 X*ex R027 ***************************************************
271802170223 "   X*ex R027 Richiamo pgm per dati anagrafici aggiuntivi
271803170223 "   X*ex R027 ***************************************************
271804170223 "   X*ex R027 CallDatiAgg   BEGSR
271805170223 "   X*ex R027
271806170223 "   X*ex R027           RESET                   SDGSD16DS
271807170223 "   X*ex R027
271808170223 "   X*ex R027           MOVEL     '02'          OPZD16
271809170223 "   X*ex R027 CMT061        IFEQ      '1'
271810170223 "   X*ex R027           MOVEL     '1'           CMTD16
271811170223 "   X*ex R027           ELSE
271812170223 "   X*ex R027           MOVEL     '0'           CMTD16
271813170223 "   X*ex R027           ENDIF
271814170223 "   X*ex R027
271815170223 "   X*ex R027           Eval      SoggD16     = *Blank
271816170223 "   X*ex R027           Eval      DtFiVlD16   = *Loval
271817170223 "   X*ex R027           Eval      SysD16      = PPASys
271818170223 "   X*ex R027           Eval      NrAsIntD16  = PPANrAsInt
271819170223 "   X*ex R027
271820170223 "   X*ex R027           MOVEL     SDGSD16DS     KPJBU
271821170223 "   X*ex R027
271822170223 "   X*ex R027           CALL      'SDGSD16R'
271823170223 "   X*ex R027           PARM                    KPJBA
271824170223 "   X*ex R027
271825170223 "   X*ex R027           MOVEL     KPJBU         SDGSD16DS
271826170223 "   X*ex R027
271827170223 "   X*ex R027 ritorno da PGM gestione
271828170223 "   X*ex R027           EXSR      RTND16
271829170223 "   X*ex R027
271830170223 "   X*ex R027 se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
271831170223 "   X*ex R027 *IN99         IFEQ      *ON
271832170223 "   X*ex R027           SETON                                        40
271833170223 "   X*ex R027           ENDIF
271834170223 "   X*ex R027
271835170223 "   X*ex R027           MOVE      *HIVAL        $LASTV
271836170223 "   X*ex R027
271837170223 "   X*ex R027           ENDSR
271838170223 "   X*ex R027 /EJECT
271839170223 "   X*ex R027 ***************************************************
271840170223 "   X*ex R027 GESTIONE RITORNO DA PGM DI GESTIONE
271841170223 "   X*ex R027 ***************************************************
271842170223 "   X*ex R027 RTND16        BEGSR
271843170223 "   X*ex R027
271844170223 "   X*ex R027 modo di ritorno
271845170223 "   X*ex R027
271846170223 "   X*ex R027           SELECT
271847170223 "   X*ex R027 questi modi di utilizzo dei valori di ritorno dal
271848170223 "   X*ex R027 pgm di manutenzione rcd di anagrafica sono delle
271849170223 "   X*ex R027 proposte, normalmente sempre valide, ma modificabili
271850170223 "   X*ex R027 per situazioni particolari
271851170223 "   X*ex R027 1 = F3
271852170223 "   X*ex R027 RETD16        WHENEQ    '1'
271853170223 "   X*ex R027           MOVE      *ON           $FINE                          fine pgm
271854170223 "   X*ex R027           MOVE      '1'           RET061
271855170223 "   X*ex R027 2 = F12
271856170223 "   X*ex R027 RETD16        WHENEQ    '2'
271857170223 "   X*ex R027 se il pgm di manutenzione ha prima una guida, è necessario anche
271858170223 "   X*ex R027 questo test, altrimenti prevale il retD04 e il sfl non viene
271859170223 "   X*ex R027 ricaricato
271860170223 "   X*ex R027 OPRD16        ANDNE     '1'
271861170223 "   X*ex R027
271862170223 "   X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
271863170223 "   X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
271864170223 "   X*ex R027 OPZD16        IFNE      '02'
271865170223 "   X*ex R027           else
271866170223 "   X*ex R027           endif
271867170223 "   X*ex R027
271868170223 "   X*ex R027           ENDSL
271869170223 "   X*ex R027
271870170223 "   X*ex R027 operazione eseguite dal pgm chiamato
271871170223 "   X*ex R027
271872170223 "   X*ex R027           SELECT
271873170223 "   X*ex R027 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
271874170223 "   X*ex R027 OPRD16        WHENEQ    '1'
271875170223 "   X*ex R027
271876170223 "   X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
271877170223 "   X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
271878170223 "   X*ex R027 OPZD16        IFNE      '02'
271879170223 "   X*ex R027 se l'opzione con cui è stato chiamato è la modifica
271880170223 "   X*ex R027 aggiorno subito la riga di sfl e non richiedo il ripristino sfl
271881170223 "   X*ex R027           ELSE
271882170223 "   X*ex R027 ********  EXSR      AGGS1
271883170223 "   X*ex R027           ENDIF
271884170223 "   X*ex R027
271885170223 "   X*ex R027           ENDSL
271886170223 "   X*ex R027
271887170223 "   X*ex R027 funzione non eseguibile per errore :
271888170223 "   X*ex R027
271889170223 "   X*ex R027           SELECT
271890170223 "   X*ex R027 1 = funzione richiamata chiusa in errore
271891170223 "   X*ex R027 ERRD16        WHENEQ    '1'
271892170223 "   X*ex R027           SETON                                        9399      emiss.SFLMSGID
271893170223 "   X*ex R027 2 = funzione richiamata senza la dovuta autorizzazione
271894170223 "   X*ex R027 ERRD16        WHENEQ    '2'
271895170223 "   X*ex R027           SETON                                        9499      emiss.SFLMSGID
271896170223 "   X*ex R027
271897170223 "   X*ex R027           ENDSL
271898170223 "   X*ex R027
271899170223 "   X*ex R027           ENDSR
271900170223R224 X*ex R027 EJECT
271901941205     C************************************************************
272000941205     C* CONTROLLO ABILITAZIONE TASTI
272100941205     C************************************************************
272200941205     C     CTRKEY        BEGSR
272300941205     C*
272400941205     C                   SELECT
272500941205     C     $TASTO        WHENEQ    F01
272600941205     C     $FC(1)        IFEQ      '0'
272700941205     C                   SETON                                        99
272800941205     C                   ENDIF
272900941205     C     $TASTO        WHENEQ    F02
273000941205     C     $FC(2)        IFEQ      '0'
273100941205     C                   SETON                                        99
273200941205     C                   ENDIF
273300941205     C     $TASTO        WHENEQ    F03
273400941205     C     $FC(3)        IFEQ      '0'
273500941205     C                   SETON                                        99
273600941205     C                   ENDIF
273700941205     C     $TASTO        WHENEQ    F04
273800941205     C     $FC(4)        IFEQ      '0'
273900941205     C                   SETON                                        99
274000941205     C                   ENDIF
274100941205     C     $TASTO        WHENEQ    F05
274200941205     C     $FC(5)        IFEQ      '0'
274300941205     C                   SETON                                        99
274400941205     C                   ENDIF
274500941205     C     $TASTO        WHENEQ    F06
274600941205     C     $FC(6)        IFEQ      '0'
274700941205     C                   SETON                                        99
274800941205     C                   ENDIF
274900941205     C     $TASTO        WHENEQ    F07
275000941205     C     $FC(7)        IFEQ      '0'
275100941205     C                   SETON                                        99
275200941205     C                   ENDIF
275300941205     C     $TASTO        WHENEQ    F08
275400941205     C     $FC(8)        IFEQ      '0'
275500941205     C                   SETON                                        99
275600941205     C                   ENDIF
275700941205     C     $TASTO        WHENEQ    F09
275800941205     C     $FC(09)       IFEQ      '0'
275900941205     C                   SETON                                        99
276000941205     C                   ENDIF
276100941205     C     $TASTO        WHENEQ    F10
276200941205     C     $FC(10)       IFEQ      '0'
276300941205     C                   SETON                                        99
276400941205     C                   ENDIF
276500941205     C     $TASTO        WHENEQ    F11
276600941205     C     $FC(11)       IFEQ      '0'
276700941205     C                   SETON                                        99
276800941205     C                   ENDIF
276900941205     C     $TASTO        WHENEQ    F12
277000941205     C     $FC(12)       IFEQ      '0'
277100941205     C                   SETON                                        99
277200941205     C                   ENDIF
277300941205     C     $TASTO        WHENEQ    F13
277400941205     C     $FC(13)       IFEQ      '0'
277500941205     C                   SETON                                        99
277600941205     C                   ENDIF
277700941205     C     $TASTO        WHENEQ    F14
277800941205     C     $FC(14)       IFEQ      '0'
277900941205     C                   SETON                                        99
278000941205     C                   ENDIF
278100941205     C     $TASTO        WHENEQ    F15
278200941205     C     $FC(15)       IFEQ      '0'
278300941205     C                   SETON                                        99
278400941205     C                   ENDIF
278500941205     C     $TASTO        WHENEQ    F16
278600941205     C     $FC(16)       IFEQ      '0'
278700941205     C                   SETON                                        99
278800941205     C                   ENDIF
278900941205     C     $TASTO        WHENEQ    F17
279000941205     C     $FC(17)       IFEQ      '0'
279100941205     C                   SETON                                        99
279200941205     C                   ENDIF
279300941205     C     $TASTO        WHENEQ    F18
279400941205     C     $FC(18)       IFEQ      '0'
279500941205     C                   SETON                                        99
279600941205     C                   ENDIF
279700941205     C     $TASTO        WHENEQ    F19
279800941205     C     $FC(19)       IFEQ      '0'
279900941205     C                   SETON                                        99
280000941205     C                   ENDIF
280100941205     C     $TASTO        WHENEQ    F20
280200941205     C     $FC(20)       IFEQ      '0'
280300941205     C                   SETON                                        99
280400941205     C                   ENDIF
280500941205     C     $TASTO        WHENEQ    F21
280600941205     C     $FC(21)       IFEQ      '0'
280700941205     C                   SETON                                        99
280800941205     C                   ENDIF
280900941205     C     $TASTO        WHENEQ    F22
281000941205     C     $FC(22)       IFEQ      '0'
281100941205     C                   SETON                                        99
281200941205     C                   ENDIF
281300941205     C     $TASTO        WHENEQ    F23
281400941205     C     $FC(23)       IFEQ      '0'
281500941205     C                   SETON                                        99
281600941205     C                   ENDIF
281700941205     C     $TASTO        WHENEQ    F24
281800941205     C     $FC(24)       IFEQ      '0'
281900941205     C                   SETON                                        99
282000941205     C                   ENDIF
282100941205     C                   ENDSL
282200941205     C*
282300941205     C                   ENDSR
282400940318     C/EJECT
282500940318     C******************************************************
282600940322     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
282700940318     C******************************************************
282800940318     C     MSGPGM        BEGSR
282900940318     C*
283000940318     C                   Z-ADD     2             MSGMSG
283100940318     C                   MOVEL     'S'           MSGRSP
283200940318     C                   MOVEL     'CR'          MSGTPR
283300940318     C                   MOVEL     'S'           MSGEMV
283400940318     C                   Z-ADD     10            MSGRGP
283500940318     C                   MOVEL     'S'           MSGLCK
283600940318     C                   MOVEA     STATUS        STS
283700940318     C                   MOVEL     'N'           MSGCNL
283800940318     C                   MOVEL     'N'           MSGVID
283900940318     C                   MOVEL     'N'           MSGOPE
284000940318     C                   MOVEL     'N'           MSGSTP
284100950522     C                   CALL      'XMSGR'
284200940318     C                   PARM                    XMSGDS
284300940318     C*
284400940318     C                   ENDSR
284500940207     C/EJECT
284600940131     C************************************************************
284700940131     C* OPERAZIONI INIZIALI
284800940131     C************************************************************
284900940131     C     *INZSR        BEGSR
285000000118C1309C     *DMY          Move      Udate         ZDateISO
285100000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
285200940131     C*
285300940127     C* Reperimento parametri
285400940127     C*
285500940117     C     *ENTRY        PLIST
285600940117     C                   PARM                    KPJBA
285700950410     C                   PARM                    DSAUT
285800950612     C*
285900990909C1221C* se queste specifiche rimangono qui allora solo la prima volta
286000990909  "  C* avrò un passaggio di dati mentre nelle altre rimarranno i dati
286100990909  "  C* precedenti
286200990909  "  C***  PARMS         IFLT      2
286300990909  "  C***                MOVE      *BLANK        AUTDS
286400990909  "  C***                ELSE
286500990909  "  C***                MOVEL     DSAUT         AUTDS
286600990909C1221C***                ENDIF
286700950928     C*
286800950928     C                   eval      $open = *off
286900950928     C*
287000940223     C                   ENDSR
287100970218     C/EJECT
287200970218     C************************************************************
287300970218     C* REPERIMENTO PARAMETRI GESTIONE
287400970218     C************************************************************
287500970218     C     REPPAR        BEGSR
287600970218     C*
287700970218     C                   CALLb     'XPAR'
287800970218     C                   PARM                    XPASOC            3
287900970218     C                   PARM                    XPAPAR            8
288000970218     C                   PARM                    XPAOUT
288100970218     C                   PARM                    XPAERR            1
288200970218     C                   PARM                    XPARIC            1
288300970218     C*
288400970218     C                   ENDSR
288500940223     C/EJECT
288600940506     C************************************************************
288700940506     C* REPERIMENTO TASTI DI FUNZIONE
288800940506     C************************************************************
288900940506     C     REPKEY        BEGSR
289000940506     C                   MOVEA     $FM1          $FM
289100950906     C                   MOVEA     $Fc1          $Fc
289200940506     C                   EXSR      REPKKK
289300940506     C                   MOVEA     $FX           $FX1
289400940614     C*
289500940614     C                   ENDSR
289600940506     C/EJECT
289700940506     C************************************************************
289800940506     C* REPERIMENTO TASTI DI FUNZIONE
289900940506     C************************************************************
290000940506     C     REPKKK        BEGSR
290100940506     C*
290200940506     C                   CLEAR                   $FX
290300940506     C     1             DO        24            X
290400940506     C*
290500940506     C     $FM(X)        IFNE      *BLANK
290600950906     C     $Fc(X)        ANDNE     '0'
290700940506     C*
290800940506     C                   MOVEL     $FM(X)        IDMSG
290900950522     C                   CALLB     'XRTVM'
291000940506     C                   PARM                    IDMSG            27
291100940506     C                   PARM                    TXTMSG          100
291200940506     C                   PARM                    ERRMSG            1
291300940506     C*
291400940506     C     ERRMSG        IFNE      *ON
291500940506     C                   MOVEL     TXTMSG        $FX(X)
291600940506     C                   ELSE
291700940506     C                   MOVEL     *ALL' '       $FX(X)
291800941027     C                   ENDIF
291900940506     C*
292000941027     C                   ENDIF
292100940506     C*
292200941027     C                   ENDDO
292300940506     C*
292400940506     C                   ENDSR
292500940509     C/EJECT
292600940223     C************************************************************
292700940223     C* INIZIALIZZAZIONE VARIABILI
292800940223     C************************************************************
292900940223     C     INZVAR        BEGSR
293000941215     C*
293100941215     C* Dati da PGM guida
293200941215     C*
293300950705    >C                   MOVEL     KPJBU         NDC061DS
293400941201     C*
293500941201     C* Reperimento dati società
293600941201     C*
293700941115    >C                   MOVEL     'SOC001'      TIPXSC
293800941115     C                   MOVEL     *BLANK        SOCXSC
293900941115     C                   EXSR      REPSOC
294000941115     C     RTNXSC        IFNE      '1'
294100941115     C                   MOVEL     XSOCDS        SOC001
294200941115     C                   ELSE
294300941115     C                   RETURN
294400941115     C                   ENDIF
294500970218     C*
294600970218     C* Reperimento parametri di gestione bonifici italia
294700970218     C*
294800981109     C*****              MOVEL     *BLANK        XPARIC
294900981109     C*****              MOVEL     XSCSOC        XPASOC
295000981109     C*****              MOVEL     'CHH1ITA'     XPAPAR
295100981109     C*****              MOVEL     *BLANK        XPAOUT
295200981109     C*****              EXSR      REPPAR
295300981109     C*****XPAERR        IFEQ      '0'
295400981109     C*****              MOVEL     XPAOUT        ndchh1
295500981109     C*****              MOVEL     ndchh1        ndchh1ita
295600981109     c*****              else
295700981109     c*****              clear                   ndchh1ita
295800981109     C*****              ENDIF
295900981109     C*****
296000981109     C**Reperimento parametri di gestione bonifici estero
296100981109     C*****
296200981109     C*****              MOVEL     *BLANK        XPARIC
296300981109     C*****              MOVEL     XSCSOC        XPASOC
296400981109     C*****              MOVEL     'CHH1EST'     XPAPAR
296500981109     C*****              MOVEL     *BLANK        XPAOUT
296600981109     C*****              EXSR      REPPAR
296700981109     C*****XPAERR        IFEQ      '0'
296800981109     C*****              MOVEL     XPAOUT        ndchh1
296900981109     C*****              MOVEL     ndchh1        ndchh1est
297000981109     C*****              ELSE
297100981109     c*****              clear                   ndchh1est
297200981109     C*****              ENDIF
297300981109     C*****
297400981109     C                   MOVEL     *BLANK        XPARIC
297500981109     C                   MOVEL     XSCSOC        XPASOC
297600981109     C                   MOVEL     'BONIFICI'    XPAPAR
297700981109     C                   MOVEL     *BLANK        XPAOUT
297800981109     C                   MOVEL     *Off          XPAERR
297900981109     C                   EXSR      REPPAR
298000981109     C     XPAERR        IFEQ      '0'
298100981109     C                   Movel     XPaOut        Anp039DS
298200981109     C                   ELSE
298300981109     C                   Clear                   Anp039DS
298400981109     C                   ENDIF
298500990810C1220 *recupero il paramentro modulo NDPPS00F
298600990810  "   *per avere le intestazioni delle categorie di partita
298700990810  "  C                   MOVEL     *BLANK        XPARIC
298800990810  "  C                   MOVEL     XSCSOC        XPASOC
298900990810  "  C                   MOVEL     'NDPPS00F'    XPAPAR
299000990810  "  C                   MOVEL     *BLANK        XPAOUT
299100990810  "  C                   MOVEL     *Off          XPAERR
299200990810  "  C                   EXSR      REPPAR
299300990810  "  C     XPAERR        IFEQ      '0'
299400990810  "  C                   Movel     XPaOut        Anp055DS
299500990810  "  C                   ELSE
299600990810  "  C                   Clear                   Anp055DS
299700990810  "  C                   ENDIF
299800990810C1220 *
299900950928     C* Attiva Commitment control se necessario
300000950928     C*
300100950928    >C     CMT061        IFEQ      '0'
300200950928     C                   eval      flgcom = *on
300300950928    >C                   CALLB     'XSTRCMT'
300400950928     C                   ENDIF
300500950928     C     $open         ifeq      *off
300600950928    >C                   OPEN      NDPPA01L
300700950928    >C                   OPEN      NDPPS01L
300800950928     C                   eval      $open = *on
300900950928     C                   ENDIF
301000990909C1221C*
301100990909  "  C* testo se sono stati passati i dati di autorizzazione
301200990909  "  C*
301300990909  "  C     PARMS         IFLT      2
301400990909  "  C                   MOVE      *BLANK        AUTDS
301500990909  "  C                   ELSE
301600990909  "  C                   MOVEL     DSAUT         AUTDS
301700990909  "  C                   MOVEL     XCADDS        CONPPADS
301800990909  "  C* Ctrl se l'opzione con cui è stato richiamato è congruente con
301900990909  "  C* le autorizzazioni
302000990909  "  C                   EXSR      TSTAUT
302100990909  "  C                   ENDIF
302200990909  "  C*****************************************************************
302300990909  "  C* questa parte di reperimento non deve essere qui in quanto è
302400990909  "  C* necessario avere l'unità della particolarità partita per
302500990909  "  C* chiamare XCHKAUT correttamente. Spostato il tutto nella CTRD1.
302600990909  "  C* Nel caso non venisse passato alcuna DS delle autorizzazioni
302700990909  "  C* e quindi il controllo fosse demandato al momento della CTRD1
302800990909  "  C* non sarà possibile cambiare da opzione=modifica in visualizza
302900990909  "  C* anche se l'utente fosse autorizzato.
303000990909  "  C*****************************************************************
303100990909  "  C*****
303200990909  "  C**Reperimento autorizzazioni
303300990909  "  C*****
303400990909  "  C**le*autorizzazioni potrebbero essere già state passate,
303500990909  "  C**nel qual caso non vanno reperite
303600990909  " >C*****AUTDS         IFEQ      *BLANK
303700990909  " >C*****              EXSR      REPAUT
303800990909  " >C*****              ELSE
303900990909  " >C*****              MOVEL     XCADDS        CONPPADS
304000990909  " >C*****              ENDIF
304100990909  "  C*****
304200990909  "  C**Ctrl se l'opzione con cui è stato richiamato è congruente con
304300990909  "  C**le*autorizzazione
304400990909  "  C*****
304500990909  " >C*****              EXSR      TSTAUT
304600990909C1221C*****************************************************************
304700940128     C*
304800940223     C* Variabili per gestione videate
304900940128     C*
305000950907     C     OPZ061        IFNE      '60'
305100950907     C     OPZ061        ANDNE     '61'
305200940223     C                   MOVE      'D1'          $GEST
305300950907     C                   ELSE
305400950907     C                   MOVE      'AG'          $GEST
305500950907     C                   endif
305600940223     C                   MOVE      *OFF          $FINE
305700940223     C                   MOVE      *ON           $INZD1
305800940224     C                   MOVE      *BLANK        $LASTG
305900940224     C                   MOVE      *BLANK        $LASTV
306000940506     C                   Z-ADD     0             $ULKD1            3 0
306100940127     C*
306200940223     C* Variabili appoggio
306300940127     C*
306400940224     C                   Z-ADD     0             CURR
306500940224     C                   Z-ADD     0             CURC
306600940207     C*
306700940207     C* Valorizzazione campi univoci testate
306800940207     C*
306900940207     C                   CLEAR                   FMTT1
307000940207     C                   MOVEL     KNSIF         NOMSIF
307100941110     C                   MOVEL     XSCDSI        NOMDIT
307200941027     C                   MOVEL     DSPGM         NOMPGM
307300950331     C* recupero il sottotitolo (se c'è)
307400950331     C                   MOVEL     *BLANK        T1TIT
307500950331     C     SOTTIT        IFNE      *BLANK
307600950331     C                   MOVEL     SOTTIT        IDMSG
307700950522     C                   CALLB     'XRTVM'
307800950331     C                   PARM                    IDMSG            27
307900950331     C                   PARM                    TXTMSG          100
308000950331     C                   PARM                    ERRMSG            1
308100950331     C                   PARM      *ON           CTRMSG            1
308200950331     C                   PARM      30            LENMSG            3 0
308300950331     C     ERRMSG        IFNE      *ON
308400950331     C                   MOVEL     TXTMSG        T1TIT
308500950331     C                   ELSE
308600950331     C                   MOVEL     *ALL'?'       T1TIT
308700950331     C                   ENDIF
308800950331     C                   ENDIF
308900950331     C* recupero il tipo operazione
309000950331     C                   Z-ADD     1             X
309100950705    >C     OPZ061        LOOKUP    $OZ(X)                                 21
309200950331     C     *IN21         IFEQ      *ON
309300950331     C                   MOVEL     $ID(X)        IDMSG
309400950522     C                   CALLB     'XRTVM'
309500950331     C                   PARM                    IDMSG            27
309600950331     C                   PARM                    TXTMSG          100
309700950331     C                   PARM                    ERRMSG            1
309800950331     C                   PARM      *ON           CTRMSG            1
309900950331     C                   PARM      30            LENMSG            3 0
310000950331     C     ERRMSG        IFNE      *ON
310100950331     C                   MOVEL     TXTMSG        T1OPE
310200950331     C                   ELSE
310300950331     C                   MOVEL     *ALL'?'       T1OPE
310400950331     C                   ENDIF
310500950331     C                   ENDIF
310600950427     C*
310700950427     C* Reperisco e memorizzo default in caso di immissione
310800940224     C*
310900940224     C* Inizializzazione/Decodifica prima videata
311000940224     C*
311100940224     C                   EXSR      INZD1
311200940224     C                   MOVE      *OFF          $INZD1
311300940127     C*
311400940117     C                   ENDSR
311500941209     C/EJECT
311600941209     C************************************************************
311700941209     C* REPERIMENTO DATI SOCIETA'
311800941209     C************************************************************
311900941209     C     REPSOC        BEGSR
312000941209     C*
312100950522     C                   CALLB     'XSOC'
312200941209     C                   PARM                    TIPXSC            6
312300941209     C                   PARM                    SOCXSC            3
312400941209     C                   PARM                    CDSXSC            9 0
312500941209     C                   PARM                    MODXSC            3
312600941209     C                   PARM      *BLANKS       RTNXSC            1
312700941209     C                   PARM                    XSOCDS
312800941209     C                   PARM                    KPJBA
312900941209     C*
313000941209     C                   ENDSR
313100950301     C/EJECT
313200950301     C************************************************************
313300950301     C* REPERIMENTO AUTORIZZAZIONI UTENTE
313400950301     C************************************************************
313500950301     C     REPAUT        BEGSR
313600950301     C*
313700990909C1221C* asteriscate la clear e la valorizzazione di xcauni perchè
313800990909  "  C* spostate alla chiamata
313900990909C1221C****************** CLEAR                   AUTDS
314000990909     C*
314100990909     C                   MOVEL     XSCSOC        XCASOC
314200990909C1221C****************** MOVEL     XSCDUN        XCAUNI
314300950301     C                   Z-ADD     0             XCAGRP
314400950301     C                   MOVEL     KNMUS         XCAPRF
314500950705    >C                   MOVEL     'CONPPA'      XCAFNC
314600950301    >C                   MOVEL     *ZERO         XCAVFU
314700950301    >C                   MOVE      'CK'          XCATCK
314800950301    >C                   MOVE      '1'           XCANAU
314900950301     C*
315000950522     C                   CALLB     'XCHKAUT'
315100950301     C                   PARM                    AUTDS
315200950301     C*
315300950301     C* errori/non abilitazione
315400950301     C     XCARTN        IFNE      0
315500980819     C                   MOVE      '2'           Err061
315600980819     c                   MOVE      *ON           $FINE
315700950301     C                   EXSR      ENDPGM
315800950301     C                   ELSE
315900950705    >C                   MOVEL     XCADDS        CONPPADS
316000950301     C                   ENDIF
316100950301     C*
316200950301     C                   ENDSR
316300950301     C/EJECT
316400950301     C************************************************************
316500950301     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
316600950705    >C* in questo esempio, l'anagrafica possiede un flag di
316700950705    >C* autorizzazione per ognuna delle opzioni std
316800950301     C************************************************************
316900950301     C     TSTAUT        BEGSR
317000950301     C*
317100950301     C                   SELECT
317200950301     C*
317300950301     C* se richiamato in immissione
317400950705    >C     OPZ061        WHENEQ    '01'
317500950301     C*
317600950301     C* se richiamato in modifica
317700950705    >C     OPZ061        WHENEQ    '02'
317800990909C1221C**** OPZ061        oreq      '60'
317900990909C1221C**** OPZ061        OREQ      '61'
318000950301     C* ma l'utente non è abilitato
318100950706     C* clienti
318200950706     C     SNATURA61     ifeq      'C'
318300950706    >C     MANprcl       ANDEQ     *OFF
318400950301     C* testo se l'utente è abilitato almeno alla visualizzazione
318500950706    >C     VISprcl       IFEQ      *ON
318600950706    >C                   MOVE      '05'          OPZ061
318700950706     C                   ELSE
318800950301     C* altrimenti esco da pgm avvertendolo
318900950706    >C                   MOVE      '2'           ERR061
319000950706     C                   MOVE      *ON           $FINE
319100950706     C                   EXSR      ENDPGM
319200950706     C                   ENDIF
319300950706     C                   ENDIF
319400950706     C* fornitori
319500950706     C     SNATURA61     ifeq      'F'
319600950706    >C     MANprFR       ANDEQ     *OFF
319700950706     C* testo se l'utente è abilitato almeno alla visualizzazione
319800950706    >C     VISprFR       IFEQ      *ON
319900950706    >C                   MOVE      '05'          OPZ061
320000950706     C                   ELSE
320100950706     C* altrimenti esco da pgm avvertendolo
320200950706    >C                   MOVE      '2'           ERR061
320300950706     C                   MOVE      *ON           $FINE
320400950706     C                   EXSR      ENDPGM
320500950706     C                   ENDIF
320600950706     C                   ENDIF
320700950301     C*
320800950301     C* se richiamato in copia
320900950705    >C     OPZ061        WHENEQ    '03'
321000950301     C*
321100950301     C* se richiamato in annullamento
321200950705    >C     OPZ061        WHENEQ    '04'
321300950706     C* ma l'utente non è abilitato
321400950706     C* clienti
321500950706     C     SNATURA61     ifeq      'C'
321600950706    >C     ANNPRCL       ANDEQ     *OFF
321700950301     C* esco da pgm avvertendolo
321800950706    >C                   MOVE      '2'           ERR061
321900950706     C                   MOVE      *ON           $FINE
322000950706     C                   EXSR      ENDPGM
322100950706     C                   ENDIF
322200950706     C* fornitori
322300950706     C     SNATURA61     ifeq      'F'
322400950706    >C     ANNPRFR       ANDEQ     *OFF
322500950706     C* esco da pgm avvertendolo
322600950706    >C                   MOVE      '2'           ERR061
322700950706     C                   MOVE      *ON           $FINE
322800950706     C                   EXSR      ENDPGM
322900950706     C                   ENDIF
323000950301     C*
323100950301     C* se richiamato in visualizzazione
323200950705    >C     OPZ061        WHENEQ    '05'
323300950301     C* ma l'utente non è abilitato
323400950706     C* clienti
323500950706     C     SNATURA61     IFEQ      'C'
323600950706    >C     VISprcl       ANDEQ     *OFF
323700950301     C* esco da pgm avvertendolo
323800950706    >C                   MOVE      '2'           ERR061
323900950706     C                   MOVE      *ON           $FINE
324000950706     C                   EXSR      ENDPGM
324100950706     C                   endif
324200950706     C* fornitori
324300950706     C     SNATURA61     IFEQ      'F'
324400950706    >C     VISprFR       ANDEQ     *OFF
324500950706     C* esco da pgm avvertendolo
324600950706    >C                   MOVE      '2'           ERR061
324700950706     C                   MOVE      *ON           $FINE
324800950706     C                   EXSR      ENDPGM
324900950706     C                   endif
325000950301     C*
325100950301     C                   ENDSL
325200950301     C*
325300950706     C* controllo livello di riservatezza
325400950706     C                   SELECT
325500950706     C* clienti
325600950706     C     SNATURA61     WHENEQ    'C'
325700950706     C     LIVRIS061     IFGT      LvrPRCL
325800950706     C* esco da pgm avvertendolo
325900950706    >C                   MOVE      '2'           ERR061
326000950706     C                   MOVE      *ON           $FINE
326100950706     C                   EXSR      ENDPGM
326200950706     C                   ENDIF
326300950706     C* fornitori
326400950706     C     SNATURA61     WHENEQ    'F'
326500950706     C     LIVRIS061     IFGT      LvrPRFR
326600950706     C* esco da pgm avvertendolo
326700950706    >C                   MOVE      '2'           ERR061
326800950706     C                   MOVE      *ON           $FINE
326900950706     C                   EXSR      ENDPGM
327000950706     C                   ENDIF
327100950706     C
327200950706     C                   ENDSL
327300950706     C*
327400950301     C                   ENDSR
327500940715     C/EJECT
327600940715     C************************************************************
327700940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
327800940715     C************************************************************
327900940715     C     DEFOPZ        BEGSR
328000940715     C*
328100940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
328200940715     C* con S o N a seconda della utilizzabilità o meno
328300940715     C* del tasto funzione correlato
328400940715     C*
328500940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
328600940715     C* di compilazione
328700940715     C                   RESET                   $FC1
328800940715     C                   RESET                   $FR1
328900940715     C*
329000940715     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
329100940715     C* invalido alcune opzioni
329200940715     C                   SELECT
329300940715     C* Se pgm richiamato in interrogazione disattivo :
329400950705    >C     OPZ061        WHENEQ    '05'
329500940715     C* F4=Ricerca
329600940715     C                   Z-ADD     4             X
329700940720     C                   MOVE      '0'           $FC1(X)
329800940715     C* F5=Ripristino
329900940715     C                   Z-ADD     5             X
330000940720     C                   MOVE      '0'           $FC1(X)
330100940715     C* F6=Conferma
330200940715     C                   Z-ADD     06            X
330300940720     C                   MOVE      '0'           $FC1(X)
330400990317     C* F20=Riapertura partita
330500990317     C                   Z-ADD     20            X
330600990317     C                   MOVE      '0'           $FC1(X)
330700940715     C* Se pgm richiamato in annullamento disattivo :
330800950705    >C     OPZ061        WHENEQ    '04'
330900940715     C* F4=Ricerca
331000940715     C                   Z-ADD     4             X
331100940720     C                   MOVE      '0'           $FC1(X)
331200940715     C* F5=Ripristino
331300940715     C                   Z-ADD     5             X
331400940720     C                   MOVE      '0'           $FC1(X)
331500990317     C* F20=Riapertura partita
331600990317     C                   Z-ADD     20            X
331700990317     C                   MOVE      '0'           $FC1(X)
331800940715     C*
331900940715     C                   OTHER
332000940715     C*
332100940715     C                   ENDSL
332200990317     C*
332300990317     c* se la partita NON è chiusa, disattivo F20
332400990317     C                   IF        PpaParChiu = *off
332500990317     C                   Z-ADD     20            X
332600990317     C                   MOVE      '0'           $FC1(X)
332700990317     C                   ENDIF
332800940715     C*
332900940715     C                   ENDSR
333000950403     C/EJECT
333100950403     C************************************************************
333200950403     C* REPERIMENTO CAMPI DI DEFAULT
333300950403     C************************************************************
333400950403     C     REPDEF        BEGSR
333500950403     C*
333600950403     C*
333700950403     C                   ENDSR
333800940128     C************************************************************
333900950606     C* DEFINIZIONE KLIST
334000940223     C************************************************************
334100950606     C     DefKlist      BEGSR
334200940223     C*
334300940223     C* klist
334400940207     C*
334500950705    >C* NDPPA01L
334600950906    >C     K07PPA01      KLIST
334700950705    >C                   KFLD                    PPAsocieta
334800950906    >C                   KFLD                    PPACTB
334900950705    >C                   KFLD                    PPAKCC
335000950705    >C                   KFLD                    PPAKSC
335100950705    >C                   KFLD                    PPADTPAR
335200950705    >C                   KFLD                    PPANRPAR
335300950705    >C                   KFLD                    PPASERPAR
335400951121    >C* NDEFA07L
335500951122    >C     K06EFP07      KLIST
335600951201    >C                   KFLD                    EFPsocieta
335700951201    >C                   KFLD                    EFPCtb
335800951201    >C                   KFLD                    EFPKSC
335900951201    >C                   KFLD                    EFPDTPAR
336000951201    >C                   KFLD                    EFPNRPAR
336100951201    >C                   KFLD                    EFPSERPAR
336200951122    >C* NDEFA07L
336300951122    >C     K07EFA07      KLIST
336400951201    >C                   KFLD                    EFAsocieta
336500951201    >C                   KFLD                    EFACTB
336600951201    >C                   KFLD                    EFAKCC
336700951201    >C                   KFLD                    EFAKSC
336800951201    >C                   KFLD                    EFADTPAR
336900951201    >C                   KFLD                    EFANRPAR
337000951201    >C                   KFLD                    EFASERPAR
337100950705    >C* NDPPS01L
337200950705    >C     K02PPS01      KLIST
337300950705    >C                   KFLD                    PPSSYS
337400950705    >C                   KFLD                    PPSNRASINT
337500950706    >C* ANCDP01L
337600950706    >C     K02CDP01      KLIST
337700950706    >C                   KFLD                    CDPSOCIETA
337800950706    >C                   KFLD                    CDPCONDPAG
337900960129    >C* anuni01L
338000960129    >C     K02uni01      KLIST
338100960129    >C                   KFLD                    uniSOCIETA
338200960129    >C                   KFLD                    uniunita
338300960129     C*
338400981221    >C* NDCPA01L
338500981221    >C     k02cpa01      KLIST
338600981221    >C                   KFLD                    PPASYS
338700981221    >C                   KFLD                    PPANRASINT
338701170223R027 C*
338702170223  "  C* SDGSO301L
338703170223  "  C     K04SO301      KLIST
338704170223  "  C                   KFLD                    SO3Sogg
338705170223  "  C                   KFLD                    SO3DtFiVL
338706170223  "  C                   KFLD                    SO3Sys
338707170223R027 C                   KFLD                    SO3NrAsint
338800990218     C*
338900990218    >C* NDPAS03L
339000990218    >C     K07PAS03      KLIST
339100990218    >C                   KFLD                    PasSocieta
339200990218    >C                   KFLD                    PasCTB
339300990218    >C                   KFLD                    PasKCC
339400990218    >C                   KFLD                    PasKSC
339500990218    >C                   KFLD                    PasDTPAR
339600990218    >C                   KFLD                    PasNRPAR
339700990218    >C                   KFLD                    PasSERPAR
339800000411D0525 *
339900000411  "  C* NdMix01l
340000000411  "  C     K02Mix01      klist
340100000411  "  C                   kfld                    PpaSys
340200000411D0525C                   kfld                    PpaNrAsInt
340300940127     C*
340400940117     C                   ENDSR
340500940509     C/EJECT
340600940509     C************************************************************
340700940509     C* RIEMPIMENTO TASTI FUNZIONE
340800940509     C************************************************************
340900940509     C     RIEKEY        BEGSR
341000940509     C*
341100940509     C* Input :
341200940509     C* - $FX = schiera con tutti i testi disponibili
341300940509     C* - $FC = schiere con i flag di validità associati ai testi
341400940509     C* - $FR = schiere con i flag di REVERSE IMAGE
341500940509     C* - $ULK = ultimo testo caricato
341600940722     C* - $L01 = Lunghezza prima riga
341700940722     C* - $L02 = Lunghezza seconda riga
341800940509     C* Output :
341900940509     C* - $ULK
342000940609     C* - $KEY1= 1° riga di tasti funzione
342100940609     C* - $KEY2= 2° riga di tasti funzione
342200940509     C*
342300950522     C                   CALLB     'XKEYMSG'
342400940509     C* input
342500940509     C                   PARM                    $FX
342600940509     C                   PARM                    $FC
342700940509     C                   PARM                    $FR
342800940509     C                   PARM                    $ULK              3 0
342900940509     C* output
343000940509     C                   PARM                    $KEY1            79
343100940509     C                   PARM                    $KEY2            79
343200940722    >C* input
343300941116    >C                   PARM      79            $L01              3 0
343400941116    >C                   PARM      79            $L02              3 0
343500940509     C*
343600060224C1913C                   PARM                    $ALLFUNCT       480
343700060224C1913C*
343800940509     C                   ENDSR
343900990317     C*
344000990317     ONDPPA000  E            aggppa
344100990317     O                       ppaparchiu
344200990317     O                       ppadtchiu
344300941214**
34440095040401PROMSG    *LIBL     COS0001
34450095040402PROMSG    *LIBL     COS0002
34460095040403PROMSG    *LIBL     COS0003
34470095040404PROMSG    *LIBL     COS0004
34480095040405PROMSG    *LIBL     COS0005
344900940509** TASTI DI FUNZIONE  D1
345000940509PROMSG    *LIBL     KEY0001  01
345100940509PROMSG    *LIBL     KEY0002  02
345200940509PROMSG    *LIBL     KEY0003  03
345300940509PROMSG    *LIBL     KEY0004  04
345400940509PROMSG    *LIBL     KEY0005  05
345500940509PROMSG    *LIBL     KEY0006  06
345600940509PROMSG    *LIBL     KEY0007  07
345700940509PROMSG    *LIBL     KEY0008  08
345800940509PROMSG    *LIBL     KEY0009  09
345900940509PROMSG    *LIBL     KEY0010  10
346000940509PROMSG    *LIBL     KEY0011  11
346100940509PROMSG    *LIBL     KEY0012  12
346200940509PROMSG    *LIBL     KEY0013  13
346300940509PROMSG    *LIBL     KEY0014  14
346400940509PROMSG    *LIBL     KEY0015  15
346500940509PROMSG    *LIBL     KEY0016  16
346600940509PROMSG    *LIBL     KEY0017  17
346700940509PROMSG    *LIBL     KEY0018  18
346800940509PROMSG    *LIBL     KEY0019  19
346900990317PROMSG    *LIBL     KEY0186  20
347000940509PROMSG    *LIBL     KEY0021  21
347100940509PROMSG    *LIBL     KEY0022  22
347200940509PROMSG    *LIBL     KEY0023  23
347300940509PROMSG    *LIBL     KEY0024  24
347400940509**  ABILITAZIONE D1
3475009407200N  01
3476009407200N  02
3477009407201N  03
3478009407201N  04
3479009407201N  05
3480009407201N  06
3481009407200N  07
3482009407201N  08
3483009407200N  09
3484009411160N  10
3485009407200N  11
3486009407201N  12
3487009407200N  13
3488009407200N  14
3489009407200N  15
3490009407200N  16
3491009407200N  17
3492009407200N  18
3493009407200N  19
3494009903171S  20
3495009407200N  21
3496009407200N  22
3497009509130N  23   Impostare a '1' il valore della riga 23 se si usano le opzioni
3498009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
349900951121**
350000951121PROMSG    *LIBL     COS1019
350100951121PROMSG    *LIBL     COS1020
