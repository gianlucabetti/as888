000100980721     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970520     H*PARMS CVTOPT(*NONE)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940117      *
000500070611    >>*               Gestione impegni
000600941201      *           -----------------------------
000700941215     F*------------
000800070611    >FNDIMP01L  UF A E           K DISK
000900070611     F
001000140313     FNdPsc01L  IF   E           K DISK
001100140313     F***NdPsc01L  UF   E           K DISK
001200070611     FNdPpa01L  IF   E           K DISK
001300941215     F*------------
001400070611    >FNDC987D   CF   E             WORKSTN
001500070611    >F                                     RENAME(C987D1:FMTD1)
001600070611    >F                                     RENAME(C987T1:FMTT1)
001700070611    >F                                     RENAME(C987Z1:FMTZ1)
001800940510    >F* eventuali schermi supplementari
001900070611    >F***                                  RENAME(C987D2:FMTD2)
002000940201     F                                     INFDS(DSFMT)
002100941215     F*------------
002200940128     D*----------------------------------------------------*
002300950404     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002400950404     D $ID             S             27    DIM(5) ALT($OZ)
002500940506     D* Schiere per la gestione dei tasti di funzione
002600940506     D $FM             S             27    DIM(24)
002700940506     D $FX             S             50    DIM(24)
002800940506     D $FC             S              1    DIM(24)
002900940506     D $FR             S              1    DIM(24)
003000940506     D* Video D1
003100940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003200940506     D $FX1            S             50    DIM(24) ALT($FM1)
003300940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003400940506     D $FR1            S              1    DIM(24) ALT($FC1)
003500960117     D* Schiera contente i nomi dei programmi da richiamare con i tasti
003600960117     D* funzione personalizzati
003700960122     D $PGF1           S             10    DIM(25)
003800940128     D*----------------------------------------------------*
003900950726     D $D1L01          S              3  0
004000950726     D $D1L02          S              3  0
004100940127     D*-------------
004200940127     D* Passaggio Parametri
004300940127     D KPJBA         E DS
004400940318     D*-------------
004500960125     D* DS Esterna gestione messaggi
004600940318     D XMSGDS        E DS
004700960130     D  STS          E                     EXTFLD(MSGSTS)
004800960130     D                                     DIM(333)
004900960130     D  JBA          E                     EXTFLD(MSGJBA)
005000960130     D                                     DIM(502)
005100950301     D*-------------
005200950301     D* Reperimento Autorizzazioni
005300950301     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005400950301     D*-------------
005500950410     D* Passaggio Autorizzazioni
005600950410     D DSAUT           DS           512
005700950410     D*-------------
005800950301     D* Maschera dati autorizzazioni
005900070611    >D CONPPADS      E DS
006000950126     D*-------------
006100950126     D* Reperimento nome PGM
006200950126     D STATUS         SDS           333
006300950126     D  DSPGM            *PROC
006400950411     D  PARMS            *PARMS
006500941209     D*-------------
006600941209     D* Dati di ambiente ottenuti da XSOC
006700970828     D SOC001        E DS                  EXTNAME(XSOC001DS)
006800941209     D*-------------
006900941209     D* DS Interna per dati di output di XSOC
007000941209     D XSOCDS          DS          1000
007100940203     D*-------------
007200940201     D* Passaggio Parametri al PGM di manutenzione
007300070611    >D NDC987DS      E DS
007400070611     D*-------------
007500070611     D* Richiamo a XATB
007600070611     D XATBDS        E DS
007700070611     D*-------------
007800070611     D* Tabella esito impegno
007900070611     D TAB042        E DS                  EXTNAME(ANG042DS)
008000950606     D*-------------
008100960202     D* campo di salvataggio per la DS d'ingresso durante la chiamata
008200960202     D* alla funzione personalizzata
008300070611    >D $SaveDS         S                   like(NDC987DS)
008400940201     D*-------------
008500940201     D DSFMT           DS
008600940506     D  $TASTO               369    369
008700940201     D  NRG                  370    370
008800940201     D  NCL                  371    371
008900970912     D*-------------
009000970912     D* DS per UserExit
009100970912     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
009200940201     D*-------------
009300940201     D* posizione cursore
009400940201     D CURSOR          DS
009500940223     D  RIRI                   1      2B 0 INZ
009600940201     D  R$$                    2      2
009700940223     D  COCO                   3      4B 0 INZ
009800940201     D  C$$                    4      4
009900940225     D*-------------
010000940616     D* Costanti
010100940225     D*-------------
010200940506     D* Tasti di funzione
010300940506     D F01             C                   CONST(X'31')
010400940506     D F02             C                   CONST(X'32')
010500940506     D F03             C                   CONST(X'33')
010600940506     D F04             C                   CONST(X'34')
010700940506     D F05             C                   CONST(X'35')
010800940506     D F06             C                   CONST(X'36')
010900940506     D F07             C                   CONST(X'37')
011000940506     D F08             C                   CONST(X'38')
011100940506     D F09             C                   CONST(X'39')
011200940506     D F10             C                   CONST(X'3A')
011300940506     D F11             C                   CONST(X'3B')
011400940506     D F12             C                   CONST(X'3C')
011500940506     D F13             C                   CONST(X'B1')
011600940506     D F14             C                   CONST(X'B2')
011700940506     D F15             C                   CONST(X'B3')
011800940506     D F16             C                   CONST(X'B4')
011900940506     D F17             C                   CONST(X'B5')
012000940506     D F18             C                   CONST(X'B6')
012100940506     D F19             C                   CONST(X'B7')
012200940506     D F20             C                   CONST(X'B8')
012300940506     D F21             C                   CONST(X'B9')
012400940506     D F22             C                   CONST(X'BA')
012500940506     D F23             C                   CONST(X'BB')
012600940506     D F24             C                   CONST(X'BC')
012700940506     D ENTER           C                   CONST(X'F1')
012800940506     D ROLDWN          C                   CONST(X'F4')
012900940506     D ROLLUP          C                   CONST(X'F5')
013000950606     D*-------------
013100950606     D* Sottotitolo di testata
013200070611     D*SOTTIT          C                   CONST('PROMSG    *LIBL-
013300070611     D*                                         COSxxxx')
013400070611     D SOTTIT          C                   CONST(' ')
013500950606     D*-------------
013600950606     D* Variabili per gestione videate
013700950606     D $Gest           S              2A
013800950606     D $Fine           S              1A
013900950606     D $InzD1          S              1A
014000950606     D $LastG          S              2A
014100950606     D $LastV          S              2A
014200970623     D $FlgDef         S              1A
014300950606     D*-------------
014400950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
014500950606     D CurR            S              2S 0
014600950606     D CurC            S              2S 0
014700950606     D Win             S             99A
014800950606     D*-------------
014900950606     D* Variabili appoggio specifiche della singola anagrafica
015000950606     D*  Indici
015100950606     D X               S              3S 0
015200950606     D Y               S              3S 0
015300960117     D* Indice della schiera tasti funzione
015400970912     D PosFun          S              2  0
015500070611     D*-------------
015600070611     D* Controllo conti
015700070611     D ND002DS       E DS                  inz
015800950606     D*-------------
015900070611     D* Definizioni parametri standard chiamate moduli
016000070611     D Esito           S              1
016100070611     D gesmsg          S              1
016200070611     D lenout          S              9B 0
016300070611     D Data            S               D
016400070611     D strutturaO      S             10
016500070611     D Operazione      S              2
016600070611     D LenIn           S              9B 0
016700070611     D refresh         S              1
016800070611     D uso             S              1
016900070611     DOutput           S           4000
017000940127     C*----------------------------------------------------*
017100940127     C*                MAIN LINE PROGRAM
017200940127     C*----------------------------------------------------*
017300940127     C*
017400940223     C* inizializzazione variabili
017500940223     C                   EXSR      INZVAR
017600940223     C*
017700940223     C     $FINE         DOWEQ     *OFF
017800940202     C     $GEST         CASEQ     'D1'          GESD1
017900070611    >C***  $GEST         CASEQ     'D2'          GESD2
018000941027     C                   ENDCS
018100941027     C                   ENDDO
018200940201     C*
018300940325     C* fine programma
018400940325     C                   EXSR      ENDPGM
018500940325     C/EJECT
018600940325     C************************************************************
018700960209     C* Fine Programma
018800940325     C************************************************************
018900940325     C     ENDPGM        BEGSR
019000941214     C*
019100941214     C* se ho confermato l'uscita proseguo
019200941214     C     $FINE         IFEQ      *ON
019300940325     C*
019400940325     C* disallocazione rcd nel caso di modifica/annullamento
019500070611    >C     OPZ987        IFEQ      '02'
019600070611    >C     OPZ987        OREQ      '04'
019700070611    >C                   UNLOCK    NDIMP01L                             22
019800941027     C                   ENDIF
019900940325     C*
020000940325     C* passaggio dati al pgm chiamante
020100070611    >C                   MOVEL     NDC987DS      KPJBU
020200940325     C*
020300940325     C                   SETON                                            RT
020400940616     C                   RETURN
020500941214     C*
020600941214     C                   ENDIF
020700940325     C*
020800940325     C                   ENDSR
020900940325     C/EJECT
021000940201     C************************************************************
021100940201     C* RICERCHE
021200940201     C************************************************************
021300940127     C     SEARCH        BEGSR
021400940127     C*
021500940127     C* determino Riga/Colonna del cursore
021600940127     C                   MOVE      NRG           R$$
021700940127     C                   MOVE      NCL           C$$
021800970912     C                   Z-ADD     RIRI          CURR
021900970912     C                   Z-ADD     COCO          CURC
022000941027     C*
022100941027     C                   SELECT
022200940223     C*
022300940223     C* ricerca non abilitata in questa posizione
022400941027     C                   OTHER
022500940926     C                   SETON                                        9899
022600940127     C*
022700941027     C                   ENDSL
022800950612     C*
022900950612     C* imposto pos. del cursore
023000970912     C                   Z-ADD     CURR          H1RIGA
023100970912     C                   Z-ADD     CURC          H1COLO
023200940127     C*
023300940506     C                   MOVE      *HIVAL        $LASTV
023400950612     C*
023500940127     C                   ENDSR
023600940225     C/EJECT
023700940225     C************************************************************
023800940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
023900940225     C************************************************************
024000940225     C     GESPIE        BEGSR
024100940225     C*
024200940225     C                   SELECT
024300940225     C*
024400940506     C     $GEST         WHENEQ    'D1'
024500940506     C     $ULKD1        IFEQ      0
024600940506     C* riempimento opzioni
024700940506     C                   EXSR      F24D1
024800940506     C                   ELSE
024900940506     C                   WRITE     FMTZ1
025000940506     C                   ENDIF
025100940225     C*
025200070611     C*    $GEST         WHENEQ    'D2'
025300070611     C*    $ULKD1        IFEQ      0
025400940926     C* riempimento opzioni
025500070611     C*                  EXSR      F24D2
025600070611     C*                  ELSE
025700070611     C*                  WRITE     FMTZ1
025800070611     C*                  ENDIF
025900940225     C                   ENDSL
026000940225     C*
026100940225     C                   ENDSR
026200960117     C/EJECT
026300940131     C************************************************************
026400940510     C* GESTIONE VIDEO RECORD D1
026500940131     C************************************************************
026600940127     C     GESD1         BEGSR
026700940225     C* inizializzazione videata
026800941117   B1C     $INZD1        IFEQ      *ON
026900940127     C                   EXSR      INZD1
027000940223     C                   MOVE      *OFF          $INZD1
027100941117   E1C                   ENDIF
027200940225     C* emissione piede videata se proveniente da altra
027300940225     C* salvataggio valore formato di provenienza
027400941117   B1C     $LASTV        IFNE      'D1'
027500940207     C                   WRITE     FMTT1
027600940225     C                   EXSR      GESPIE
027700940202     C                   MOVE      $LASTV        $LASTG
027800940202     C                   MOVE      'D1'          $LASTV
027900941117   E1C                   ENDIF
028000940707     C*
028100940707     C* se esistono errori sulla videata
028200940707     C* emetto la write del formato a indicatori spenti per vedere
028300940707     C* le eventuali decodifiche
028400941117   B1C     *IN99         IFEQ      *ON
028500940707     C     $GEST         ANDEQ     'D1'
028600940707     C                   MOVEA     *IN           WIN
028700940707     C                   MOVE      *ALL'0'       IN5098           49
028800940707     C                   MOVEA     IN5098        *IN(50)
028900940707     C                   WRITE     FMTD1
029000940707     C                   MOVEA     WIN           *IN
029100941117   E1C                   ENDIF
029200960122     C* Inizializzazione indice tasti funzione personalizzati
029300970912     C                   Eval      PosFun = 25
029400940131     C*
029500941117   B1C                   SELECT
029600940203     C* Annullamento
029700070611    >C     OPZ987        WHENEQ    '04'
029800940224     C* Visualizzazione
029900070611    >C     OPZ987        OREQ      '05'
030000940203     C                   WRITE     FMTD1
030100940203     C                   EXFMT     PROTECT
030200940224     C* Immissione/Modifica/Copia
030300941117   X1C                   OTHER
030400940203     C                   EXFMT     FMTD1
030500941117   E1C                   ENDSL
030600960605     C*
030700960605     C* azzero la posizione del cursore
030800970912     C                   Z-ADD     0             H1RIGA
030900970912     C                   Z-ADD     0             H1COLO
031000940506     C*
031100960605     C* controllo abilitazione tasti funzione
031200940506     C                   EXSR      CTRKD1
031300940506     C*
031400941117   B1C     *IN99         IFEQ      *OFF
031500940131     C*
031600941117   B2C                   SELECT
031700940609     C* ROLLUP
031800070611    >C**   $TASTO        WHENEQ    ROLLUP
031900070611    >C**                 MOVE      'D2'          $GEST
032000940916     C* F2=Decodifica
032100940916     C     $TASTO        WHENEQ    F02
032200940916     C                   EXSR      F02D1
032300940131     C* F3=Fine
032400940506     C     $TASTO        WHENEQ    F03
032500940309     C                   EXSR      F03D1
032600940131     C* F4=Ricerca
032700940506     C     $TASTO        WHENEQ    F04
032800940127     C                   EXSR      SEARCH
032900940224     C* F5=Refresh
033000940506     C     $TASTO        WHENEQ    F05
033100940309     C                   EXSR      F05D1
033200940223     C* F8=Successivo
033300940506     C     $TASTO        WHENEQ    F08
033400940309     C                   EXSR      F08D1
033500940131     C* F12=Ritorno
033600940506     C     $TASTO        WHENEQ    F12
033700940309     C                   EXSR      F12D1
033800940315     C* F24=Altri tasti
033900940506     C     $TASTO        WHENEQ    F24
034000940315     C                   EXSR      F24D1
034100940131     C*
034200941117   X2C                   OTHER
034300940127     C                   EXSR      CTRD1
034400940510     C* aggiungere le chiamate ai controlli delle videate supplementari
034500070611    >C*    *IN99         IFEQ      *OFF
034600070611    >C*                  EXSR      CTRD2
034700070611    >C*    *IN99         IFEQ      *ON
034800970912    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
034900970912    >C* indicatori uguali in videate differenti, genererei conflitti
035000070611    >C*                  MOVE      *ALL'0'       IN5098           49
035100070611    >C*                  MOVEA     IN5098        *IN(50)
035200070611    >C*                  SETON                                        9996                       96
035300070611    >C*                  ENDIF
035400070611    >C*                  ENDIF
035500940203     C*
035600941117   B3C     $TASTO        IFEQ      F06
035700940207     C     *IN99         ANDEQ     *OFF
035800940224     C* eseguo aggiornamento
035900941214     C                   EXSR      AGGANA
036000940224     C* eseguo operazioni del dopo-aggiornamento
036100941117   B4C     *IN99         IFEQ      *OFF
036200940318     C                   EXSR      GESAGG
036300941117   E4C                   ENDIF
036400941117   E3C                   ENDIF
036500940203     C*
036600941117   E2C                   ENDSL
036700940224     C*
036800941117   E1C                   ENDIF
036900941027     C*
037000940117     C                   ENDSR
037100940506     C/EJECT
037200940506     C************************************************************
037300940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
037400940506     C************************************************************
037500940506     C     CTRKD1        BEGSR
037600941205     C*
037700940506     C                   SETOFF                                       99
037800940506     C                   MOVEA     $FC1          $FC
037900940506     C                   EXSR      CTRKEY
038000940506     C     *IN99         IFEQ      *ON
038100940506     C                   SETON                                        97
038200940506     C                   ENDIF
038300941205     C*
038400940506     C                   ENDSR
038500941209     C/EJECT
038600941209     C************************************************************
038700941209     C* INIZIALIZZAZIONE VIDEATA DATI
038800941209     C************************************************************
038900940127     C     INZD1         BEGSR
039000940131     C*
039100940131     C                   CLEAR                   FMTD1
039200070611    >C**                 CLEAR                   FMTD2
039300950102     C*
039400980604     C* per tutte le opzioni NON immissione
039500070611   B>C     OPZ987        IFNE      '01'
039600070611    >C                   MOVEL     XscSoc        IMPSocieta
039700070611    >C                   MOVEL     Kcc987        IMPkcc
039800070611    >C                   MOVEL     Ksc987        IMPksc
039900070611    >C                   MOVE      dtpar987      IMPdtpar
040000070611     c                   z-add     nrpar987      ImpNrPar
040100070611     c                   move      serpar987     ImpSerPar
040200070611    >C                   MOVE      dtsca987      IMPdtsca
040300100426c2156c                   z-add     progr987      ImpProgr
040400940318     C*
040500941201   B2C     *IN22         DOUEQ     *OFF
040600940318     C     MSGRTN        ORNE      'R'
040700940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
040800070611   B>C     OPZ987        IFEQ      '02'
040900070611    >C     OPZ987        OREQ      '04'
041000100426c2156x***  K07Imp01      CHAIN     NdImp000                           2122
041100100426c2156C     K08Imp01      CHAIN     NdImp000                           2122
041200940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
041300941201   X3C                   ELSE
041400100426c2156x***  K07Imp01      CHAIN(N)  NdImp000                           2122
041500100426c2156C     K08Imp01      CHAIN(N)  NdImp000                           2122
041600941201   E3C                   ENDIF
041700940318     C*
041800940318     C* Gestione RCD allocato
041900941201   B3C     *IN22         IFEQ      *ON
042000070611    >C                   MOVEL     'NDIMP00F'    MSGFIL
042100940318     C                   EXSR      MSGPGM
042200941201   E3C                   ENDIF
042300941201   E2C                   ENDDO
042400940224     C*
042500940224     C* se il rcd da gestire non viene trovato
042600940318     C* oppure è allocato da altro pgm
042700940224     C* ritorno al pgm guida avvertendolo dell'errore
042800941201   B2C     *IN21         IFEQ      *ON
042900940318     C     *IN22         OREQ      *ON
043000940224     C                   MOVE      *ON           $FINE
043100070611    >C                   MOVE      '1'           ERR987
043200940224     C*
043300940224     C* altrimenti
043400941201   X2C                   ELSE
043500940224     C* riempio la videata
043600940224     C                   EXSR      RIED1
043700070611    >C* Aggiungere le routine di riempimento dei record succcessivi
043800070611    >C**                 EXSR      RIED2
043900940224     C* richiamo routine dei ctrl per decodificarla
044000940614     C                   EXSR      CTRD1
044100070611    >C* Aggiungere le routine di controllo dei record succcessivi
044200070611    >C***                EXSR      CTRD2
044300940224     C                   MOVE      *ALL'0'       IN5098
044400940224     C                   MOVEA     IN5098        *IN(50)
044500971021   E2C                   ENDIF
044600940223     C*
044700980604     C* per immissione
044800950102   X1C                   ELSE
044900950102     C*
045000960412     C* riempio con i default di immissione  (se reperiti)
045100970623     C                   IF        $FlgDef = *on
045200950403     C                   EXSR      RIEDEF
045300970623     C                   ENDIF
045400950404     C*
045500950404     C* imposto i campi chiave ricevuti anche se si è in immissione
045600950404     C* perchè potrebbero essere stati passati dei default di chiave
045700950102   E1C                   ENDIF
045800980604     C*
045900980604     C* imposto quegli attributi della videata che sono modificabili
046000980604     C* al richiamo del determinato pgm
046100980604     C                   EXSR      ATRD1
046200940224     C*
046300940117     C                   ENDSR
046400940207     C/EJECT
046500940207     C************************************************************
046600940510     C* RIEMPIMENTO VIDEATA  D1
046700940207     C************************************************************
046800940207     C     RIED1         BEGSR
046900070611     C*
047000070611     C                   If        Impfitt=*on
047100070611     c                   seton                                        17
047200070611     c                   else
047300070611     c                   setoff                                       17
047400070611     c                   endif
047500940207     C*
047600950301     C* impostare i campi della videata dai campi del file
047700950301     C*
047800070611    >C                   MOVEL     ImpKcc        D1kcc
047900070611    >C                   MOVEL     ImpKsc        D1ksc
048000070611     c* Reperisco tutti i dati del conto
048100070611     c                   exsr      callmvc002
048200070611     c                   eval      d1desbrev=desbrev002
048300070611     c                   if        ImpDtPar <> *loval
048400070611    >C     *jobrun       MOVE      Impdtpar      D1Dtpar
048500070611     c                   endif
048600070611    >C                   MOVEL     Impnrpar      D1nrpar
048700070611    >C                   MOVEL     Impserpar     D1serpar
048800070611     c                   if        ImpDtSca <> *loval
048900070611    >C     *jobrun       MOVE      Impdtsca      D1Dtsca
049000070611     c                   endif
049100070611     c                   z-add     Impimporto    D1Importo
049200070611     c                   if        ImpDtImp <> *loval
049300070611    >C     *jobrun       MOVE      ImpdtImp      D1DtImp
049400070611     c                   endif
049500070611     c                   z-add     ImpImpImp     D1ImpImp
049600070611    >C                   MOVEL     ImpEsito      D1Esito
049700070611    >C                   MOVEL     ImpNota       D1Nota
049800070611     c* decodifica esito
049900070611     C                   MOVE      '1'           XTBRIC
050000070611     C                   MOVE      XSCSOC        XTBAZI
050100070611     C                   MOVE      *ZERO         XTBKEY
050200070611     C                   MOVE      '042'         XTBCOD
050300070611     C                   MOVE      impesito      XTBKEY
050400070611     C                   CALLb     'XATB'
050500070611     C                   PARM                    XATBDS
050600070611     C     XTBERR        IFEQ      *Off
050700070611     C                   MOVEL     XTBUNI        TAB042
050800070611     C                   MOVEL     DEs042        D1esitoD
050900070611     c                   endif
051000940207     C*
051100940207     C                   ENDSR
051200070611     C*----------------------------------------------------------
051300070611     C* Chiamata NDMVC002
051400070611     C*----------------------------------------------------------
051500070611     C     CallMvc002    BEGSR
051600070611     C*
051700070611     C* Controllo conto
051800070611     C                   Clear                   ND002DS
051900070611     C                   EVAL      LenOut = %Size(ND002DS)
052000070611     C                   eval      Data = *loval
052100070611     C                   CallB     'NDMVC002'
052200070611     C                   PARM                    xscsoc
052300070611     C                   PARM                    d1kcc
052400070611     C                   PARM                    d1ksc
052500070611     C                   PARM                    Data
052600070611     C                   PARM      *OFF          GesMsg
052700070611     C                   PARM      *OFF          Esito
052800070611     C                   PARM      'ND002DS'     StrutturaO
052900070611     C                   PARM                    ND002DS
053000070611     C                   PARM                    LenOut
053100070611     C*
053200070611     C                   ENDSR
053300970912     C************************************************************
053400980604     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
053500970912     C************************************************************
053600970925     C     ATRD1         BEGSR
053700970912     C*
053800970912     C*
053900970912     C                   SETOFF                                       02
054000980604     C*
054100970912     C* in caso di manutenzione
054200070611    >C     OPZ987        IFEQ      '02'
054300970912     C* proteggo i campi chiave
054400970912     C                   SETON                                        02        PR campi key
054500970912   E3C                   ENDIF
054600970912     C*
054700970912     C                   ENDSR
054800950403     C/EJECT
054900950403     C************************************************************
055000950403     C* RIEMPIMENTO CAMPI DAI DEFAULT
055100950403     C************************************************************
055200950403     C     RIEDEF        BEGSR
055300950403     C*
055400950403     C*
055500950403     C                   ENDSR
055600940916     C/EJECT
055700940916     C************************************************************
055800940916     C* GESTIONE F02 VIDEO D1
055900940916     C************************************************************
056000940916     C     F02D1         BEGSR
056100940916     C*
056200940916     C* per decodificare la videata richiamo la routine dei ctrl
056300940916     C* e poi disattivo gli eventuali messaggi di errore
056400940916     C                   EXSR      CTRD1
056500940916     C*
056600940916     C                   MOVE      *ALL'0'       IN5098
056700940916     C                   MOVEA     IN5098        *IN(50)
056800940916     C*
056900940916     C                   ENDSR
057000940309     C/EJECT
057100940309     C************************************************************
057200940309     C* GESTIONE F03 VIDEO D1
057300940309     C************************************************************
057400940309     C     F03D1         BEGSR
057500940309     C*
057600940309     C                   MOVE      *ON           $FINE
057700070611    >C                   MOVE      '1'           RET987
057800940325     C* fine programma
057900940325     C                   EXSR      ENDPGM
058000940325     C*
058100940325     C                   ENDSR
058200940309     C/EJECT
058300940309     C************************************************************
058400940309     C* GESTIONE F05 VIDEO D1
058500940309     C************************************************************
058600940309     C     F05D1         BEGSR
058700940309     C*
058800940309     C                   MOVE      *ON           $INZD1
058900940309     C*
059000940309     C                   ENDSR
059100940309     C/EJECT
059200940309     C************************************************************
059300940309     C* GESTIONE F08 VIDEO D1
059400940309     C************************************************************
059500940309     C     F08D1         BEGSR
059600940309     C*
059700940309     C                   MOVE      *ON           $FINE
059800070611    >C                   MOVE      '0'           RET987
059900940309     C*
060000940309     C                   ENDSR
060100940309     C/EJECT
060200940309     C************************************************************
060300940309     C* GESTIONE F12 VIDEO D1
060400940309     C************************************************************
060500940309     C     F12D1         BEGSR
060600940309     C*
060700940309     C                   MOVE      *ON           $FINE
060800070611    >C                   MOVE      '2'           RET987
060900940309     C*
061000940309     C                   ENDSR
061100940309     C/EJECT
061200940309     C************************************************************
061300940309     C* GESTIONE F24 VIDEO D1
061400940309     C************************************************************
061500940309     C     F24D1         BEGSR
061600940506     C*
061700940506     C                   MOVEA     $FX1          $FX
061800940506     C                   MOVEA     $FC1          $FC
061900940506     C                   MOVEA     $FR1          $FR
062000970912     C                   Z-ADD     $ULKD1        $ULK
062100970912     C                   Z-ADD     $D1L01        $L01
062200970912     C                   Z-ADD     $D1L02        $L02
062300940506     C                   EXSR      RIEKEY
062400970912     C                   Z-ADD     $ULK          $ULKD1            3 0
062500940720     C                   MOVEA     $FC           $FC1
062600940506     C                   MOVEL     $KEY1         Z1KE1
062700060224     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
062800940506     C                   MOVEL     $KEY2         Z1KE2
062900940506     C                   WRITE     FMTZ1
063000940506     C*
063100940309     C                   ENDSR
063200940207     C/EJECT
063300940131     C************************************************************
063400940207     C* CONTROLLO VIDEATA
063500950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
063600950613     C* 95 96 97 98
063700940131     C************************************************************
063800940127     C     CTRD1         BEGSR
063900940131     C*
064000940127     C                   SETOFF                                       99
064100940223     C*
064200940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
064300940223     C*
064400070611    >C     OPZ987        IFNE      '04'
064500940203     C*
064600070611     c                   ENDIF
064700940127     C*
064800940117     C                   ENDSR
064900940224     C/EJECT
065000940224     C************************************************************
065100950301     C* AGGIORNAMENTO ANAGRAFICA
065200940224     C************************************************************
065300941214     C     AGGANA        BEGSR
065400940207     C*
065500940207     C                   SELECT
065600070611    >C     OPZ987        WHENEQ    '01'
065700070611    >C     OPZ987        OREQ      '03'
065800941214     C                   EXSR      RIEANA
065900070611    >C                   WRITE     NDIMP000                             22
066000940318     C* se rcd non scrivibile attivo errore
066100940318     C     *IN22         IFEQ      *ON
066200941201     C                   SETON                                        9599
066300941027     C                   ENDIF
066400070611    >C     OPZ987        WHENEQ    '02'
066500941214     C                   EXSR      RIEANA
066600070611    >C                   UPDATE    NDIMP000
066700070611    >C     OPZ987        WHENEQ    '04'
066800070611    >C***                DELETE    NDIMP000
066900070611     c                   eval      IMPANN='1'
067000070611    >C                   UPDATE    NDIMP000
067100070611     C* e pulisco blocco rata se presente
067200070611     c                   eval      PpaSocieta=Societa987
067300070611     c                   eval      PpaCtb    ='CG'
067400070612     c                   eval      Ppakcc    =kcc987
067500070611     c                   eval      Ppaksc    =ksc987
067600070611     c                   eval      PpaDtPar=Dtpar987
067700070611     c                   eval      PpaNrPar=NrPar987
067800070611     c                   eval      PpaSerPar=Serpar987
067900070611     c     K07Ppa01      chain     ndppa01l
068000070611     c                   if        %found(NdPpa01l)
068100070611     c                   eval      PscSys = Ppasys
068200070611     c                   eval      PscNrasInt=PpaNrasint
068300070611     c                   eval      PscDtSca  =ImpDtSca
068400070612     c     K03Psc01      chain     NdPsc01l                             22
068500070611     c                   if        %found(NdPsc01l)
068600140313     c***                clear                   PscBKAmm
068700140313     c***                eval      PscCliSoll=*off
068800140313     C***                update    NdPsc000
068900070611     c                   endif
069000070611     c                   endif
069100940207     C                   ENDSL
069200940203     C*
069300950310     C* richiama pgm User Exit
069400950310     c*
069500950511     C     USRERR        IFEQ      *BLANK
069600981228     C* passa opzione ricevuta a user exit
069700070611    >C                   MOVEL     OPZ987        XUKOPZ
069800950310     C* imposta chiave univoca  per user exit
069900950314     C                   MOVE      XSCSOC        XUKSOC
070000950512     C                   MOVE      XSCDUN        XUKDUN
070100950328     C                   MOVEL     DSPGM         XUKPGM
070200070611    >C*                  MOVEL     ___Codice     XUKFL1
070300950404     C                   MOVEL     XUKDS         KPJBU
070400950328     C     DSPGM         CAT       'U':0         USREXT           10
070500950310     C                   CALL      USREXT                               21
070600950310     C                   PARM                    KPJBA
070700950310     C     *IN21         IFEQ      *ON
070800950310     C                   MOVE      *ON           USRERR            1
070900950310     C                   ENDIF
071000950310     C                   ENDIF
071100980319     C*
071200940203     C                   ENDSR
071300940224     C/EJECT
071400940224     C************************************************************
071500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
071600940224     C************************************************************
071700941214     C     RIEANA        BEGSR
071800940224     C*
071900070611    >C                   CLEAR                   NDIMP000
072000950301     C*
072100950301     C* imposto i campi del file da quelli video
072200940207     C*
072300940207     C                   ENDSR
072400940224     C/EJECT
072500940224     C************************************************************
072600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
072700940224     C************************************************************
072800940224     C     GESAGG        BEGSR
072900941214     C*
073000940224     C* segnalo al pgm chiamante l'avvenuta conferma
073100070611    >C                   MOVE      *ON           OPR987
073200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
073300070611    >C                   MOVE      '0'           RET987
073400940224     C*
073500941027     C                   SELECT
073600950301     C*
073700950301     C* nel caso di immissione
073800070611    >C     OPZ987        WHENEQ    '01'
073900950301     C* ripristino la videata iniziale
074000940224     C                   MOVE      *ON           $INZD1
074100941228     C                   MOVE      'D1'          $GEST
074200950301     C*
074300950301     C* nel caso di modifica
074400070611    >C     OPZ987        WHENEQ    '02'
074500950301     C* e ritorno al pgm chiamante
074600950301     C                   MOVE      *ON           $FINE
074700950301     C*
074800950301     C* per tutte le altre opzioni
074900940224     C                   OTHER
075000950301     C* ritorno al pgm chiamante
075100940224     C                   MOVE      *ON           $FINE
075200941027     C                   ENDSL
075300940224     C*
075400940224     C                   ENDSR
075500941205     C/EJECT
075600941205     C************************************************************
075700941205     C* CONTROLLO ABILITAZIONE TASTI
075800941205     C************************************************************
075900941205     C     CTRKEY        BEGSR
076000941205     C*
076100941205     C                   SELECT
076200941205     C     $TASTO        WHENEQ    F01
076300970912     C                   Z-ADD     01            PosFun
076400941205     C     $FC(1)        IFEQ      '0'
076500941205     C                   SETON                                        99
076600941205     C                   ENDIF
076700941205     C     $TASTO        WHENEQ    F02
076800970912     C                   Z-ADD     02            PosFun
076900941205     C     $FC(2)        IFEQ      '0'
077000941205     C                   SETON                                        99
077100941205     C                   ENDIF
077200941205     C     $TASTO        WHENEQ    F03
077300970912     C                   Z-ADD     03            PosFun
077400941205     C     $FC(3)        IFEQ      '0'
077500941205     C                   SETON                                        99
077600941205     C                   ENDIF
077700941205     C     $TASTO        WHENEQ    F04
077800970912     C                   Z-ADD     04            PosFun
077900941205     C     $FC(4)        IFEQ      '0'
078000941205     C                   SETON                                        99
078100941205     C                   ENDIF
078200941205     C     $TASTO        WHENEQ    F05
078300970912     C                   Z-ADD     05            PosFun
078400941205     C     $FC(5)        IFEQ      '0'
078500941205     C                   SETON                                        99
078600941205     C                   ENDIF
078700941205     C     $TASTO        WHENEQ    F06
078800970912     C                   Z-ADD     06            PosFun
078900941205     C     $FC(6)        IFEQ      '0'
079000941205     C                   SETON                                        99
079100941205     C                   ENDIF
079200941205     C     $TASTO        WHENEQ    F07
079300970912     C                   Z-ADD     07            PosFun
079400941205     C     $FC(7)        IFEQ      '0'
079500941205     C                   SETON                                        99
079600941205     C                   ENDIF
079700941205     C     $TASTO        WHENEQ    F08
079800970912     C                   Z-ADD     08            PosFun
079900941205     C     $FC(8)        IFEQ      '0'
080000941205     C                   SETON                                        99
080100941205     C                   ENDIF
080200941205     C     $TASTO        WHENEQ    F09
080300970912     C                   Z-ADD     09            PosFun
080400941205     C     $FC(09)       IFEQ      '0'
080500941205     C                   SETON                                        99
080600941205     C                   ENDIF
080700941205     C     $TASTO        WHENEQ    F10
080800970912     C                   Z-ADD     10            PosFun
080900941205     C     $FC(10)       IFEQ      '0'
081000941205     C                   SETON                                        99
081100941205     C                   ENDIF
081200941205     C     $TASTO        WHENEQ    F11
081300970912     C                   Z-ADD     11            PosFun
081400941205     C     $FC(11)       IFEQ      '0'
081500941205     C                   SETON                                        99
081600941205     C                   ENDIF
081700941205     C     $TASTO        WHENEQ    F12
081800970912     C                   Z-ADD     12            PosFun
081900941205     C     $FC(12)       IFEQ      '0'
082000941205     C                   SETON                                        99
082100941205     C                   ENDIF
082200941205     C     $TASTO        WHENEQ    F13
082300970912     C                   Z-ADD     13            PosFun
082400941205     C     $FC(13)       IFEQ      '0'
082500941205     C                   SETON                                        99
082600941205     C                   ENDIF
082700941205     C     $TASTO        WHENEQ    F14
082800970912     C                   Z-ADD     14            PosFun
082900941205     C     $FC(14)       IFEQ      '0'
083000941205     C                   SETON                                        99
083100941205     C                   ENDIF
083200941205     C     $TASTO        WHENEQ    F15
083300970912     C                   Z-ADD     15            PosFun
083400941205     C     $FC(15)       IFEQ      '0'
083500941205     C                   SETON                                        99
083600941205     C                   ENDIF
083700941205     C     $TASTO        WHENEQ    F16
083800970912     C                   Z-ADD     16            PosFun
083900941205     C     $FC(16)       IFEQ      '0'
084000941205     C                   SETON                                        99
084100941205     C                   ENDIF
084200941205     C     $TASTO        WHENEQ    F17
084300970912     C                   Z-ADD     17            PosFun
084400941205     C     $FC(17)       IFEQ      '0'
084500941205     C                   SETON                                        99
084600941205     C                   ENDIF
084700941205     C     $TASTO        WHENEQ    F18
084800970912     C                   Z-ADD     18            PosFun
084900941205     C     $FC(18)       IFEQ      '0'
085000941205     C                   SETON                                        99
085100941205     C                   ENDIF
085200941205     C     $TASTO        WHENEQ    F19
085300970912     C                   Z-ADD     19            PosFun
085400941205     C     $FC(19)       IFEQ      '0'
085500941205     C                   SETON                                        99
085600941205     C                   ENDIF
085700941205     C     $TASTO        WHENEQ    F20
085800970912     C                   Z-ADD     20            PosFun
085900941205     C     $FC(20)       IFEQ      '0'
086000941205     C                   SETON                                        99
086100941205     C                   ENDIF
086200941205     C     $TASTO        WHENEQ    F21
086300970912     C                   Z-ADD     21            PosFun
086400941205     C     $FC(21)       IFEQ      '0'
086500941205     C                   SETON                                        99
086600941205     C                   ENDIF
086700941205     C     $TASTO        WHENEQ    F22
086800970912     C                   Z-ADD     22            PosFun
086900941205     C     $FC(22)       IFEQ      '0'
087000941205     C                   SETON                                        99
087100941205     C                   ENDIF
087200941205     C     $TASTO        WHENEQ    F23
087300970912     C                   Z-ADD     23            PosFun
087400941205     C     $FC(23)       IFEQ      '0'
087500941205     C                   SETON                                        99
087600941205     C                   ENDIF
087700941205     C     $TASTO        WHENEQ    F24
087800970912     C                   Z-ADD     24            PosFun
087900941205     C     $FC(24)       IFEQ      '0'
088000941205     C                   SETON                                        99
088100941205     C                   ENDIF
088200941205     C                   ENDSL
088300941205     C*
088400941205     C                   ENDSR
088500940318     C/EJECT
088600940318     C******************************************************
088700940322     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
088800940318     C******************************************************
088900940318     C     MSGPGM        BEGSR
089000940318     C*
089100970912     C                   Z-ADD     2             MSGMSG
089200940318     C                   MOVEL     'S'           MSGRSP
089300940318     C                   MOVEL     'CR'          MSGTPR
089400940318     C                   MOVEL     'S'           MSGEMV
089500970912     C                   Z-ADD     10            MSGRGP
089600940318     C                   MOVEL     'S'           MSGLCK
089700960130     C                   MOVEA     STATUS        STS
089800940318     C                   MOVEL     'N'           MSGCNL
089900940318     C                   MOVEL     'N'           MSGVID
090000940318     C                   MOVEL     'N'           MSGOPE
090100940318     C                   MOVEL     'N'           MSGSTP
090200950522     C                   CALL      'XMSGR'
090300940318     C                   PARM                    XMSGDS
090400940318     C*
090500940318     C                   ENDSR
090600940207     C/EJECT
090700940131     C************************************************************
090800940131     C* OPERAZIONI INIZIALI
090900940131     C************************************************************
091000940131     C     *INZSR        BEGSR
091100940131     C*
091200940127     C* Reperimento parametri
091300940127     C*
091400940117     C     *ENTRY        PLIST
091500940117     C                   PARM                    KPJBA
091600950410     C                   PARM                    DSAUT
091700951010     C*
091800951010     C* Reperimento dati società
091900951010     C*
092000951010    >C                   MOVEL     'SOC001'      TIPXSC
092100951010     C                   MOVEL     *BLANK        SOCXSC
092200951010     C                   EXSR      REPSOC
092300960410     C*
092400960410     C* Controlli dati società
092500960410     C*
092600960410     C     RTNXSC        IFNE      '1'
092700960410     C                   MOVEL     XSOCDS        SOC001
092800960410     C                   ELSE
092900070611    >C                   MOVE      '1'           Err987
093000960410     C                   MOVE      *on           $Fine
093100960410     C                   EXSR      EndPgm
093200960410     C                   ENDIF
093300950612     C*
093400951010     C* testo se sono stati passati i dati di autorizzazione
093500951010     C*
093600950410     C     PARMS         IFLT      2
093700950410     C                   MOVE      *BLANK        AUTDS
093800950410     C                   ELSE
093900950410     C                   MOVEL     DSAUT         AUTDS
094000950410     C                   ENDIF
094100950726     C*
094200950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
094300950726     C                   EVAL      $D1L01 = %Size(Z1Ke1)
094400950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
094500940506     C*
094600940506     C* Reperimento tasti di funzione
094700940506     C*
094800940506     C                   EXSR      REPKEY
094900980319     C*
095000960117     C* Richiamo routine per aggiunta tasti funzione personalizzati
095100980319     C*
095200960117     C                   CAllb     'X46USREXT'
095300960117     C                   PARM                    *omit
095400960117     C                   PARM                    *omit
095500960117     C                   PARM                    *omit
095600960117     C                   PARM                    *omit
095700960117     C                   PARM                    *omit
095800960117     C                   PARM                    $FX1
095900960117     C                   PARM                    $FC1
096000960117     C                   PARM                    $PGF1
096100960117     C                   PARM                    DsPgm
096200960117     C                   PARM                    xsccut
096300960117     C                   PARM                    xscgrp
096400980319     C*
096500940223     C                   ENDSR
096600940223     C/EJECT
096700940506     C************************************************************
096800940506     C* REPERIMENTO TASTI DI FUNZIONE
096900940506     C************************************************************
097000940506     C     REPKEY        BEGSR
097100980319     C*
097200940506     C                   MOVEA     $FM1          $FM
097300980319     C                   MOVEA     $FC1          $FC
097400940506     C                   EXSR      REPKKK
097500940506     C                   MOVEA     $FX           $FX1
097600940614     C*
097700940614     C                   ENDSR
097800940506     C/EJECT
097900940506     C************************************************************
098000940506     C* REPERIMENTO TASTI DI FUNZIONE
098100940506     C************************************************************
098200940506     C     REPKKK        BEGSR
098300940506     C*
098400940506     C                   CLEAR                   $FX
098500940506     C     1             DO        24            X
098600940506     C*
098700940506     C     $FM(X)        IFNE      *BLANK
098800980319     C     $FC(X)        ANDNE     '0'
098900940506     C*
099000940506     C                   MOVEL     $FM(X)        IDMSG
099100950522     C                   CALLB     'XRTVM'
099200940506     C                   PARM                    IDMSG            27
099300940506     C                   PARM                    TXTMSG          100
099400940506     C                   PARM                    ERRMSG            1
099500940506     C*
099600940506     C     ERRMSG        IFNE      *ON
099700940506     C                   MOVEL     TXTMSG        $FX(X)
099800940506     C                   ELSE
099900940506     C                   MOVEL     *ALL' '       $FX(X)
100000941027     C                   ENDIF
100100940506     C*
100200941027     C                   ENDIF
100300940506     C*
100400941027     C                   ENDDO
100500940506     C*
100600940506     C                   ENDSR
100700940509     C/EJECT
100800940223     C************************************************************
100900940223     C* INIZIALIZZAZIONE VARIABILI
101000940223     C************************************************************
101100940223     C     INZVAR        BEGSR
101200941215     C*
101300941215     C* Dati da PGM guida
101400941215     C*
101500070611    >C                   MOVEL     KPJBU         NDC987DS
101600951206     C*
101700951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
101800951206     C* reperimento dati società
101900070611    >C                   If        Societa987 <> xscsoc    and
102000070611    >C                             Opz987 <> *blank
102100960119     C                   MOVEL     'SOC001'      TIPXSC
102200070611    >C                   MOVEL     Societa987    SOCXSC
102300951206     C                   EXSR      REPSOC
102400951010     C     RTNXSC        IFNE      '1'
102500951010     C                   MOVEL     XSOCDS        SOC001
102600951010     C                   ELSE
102700070611    >C                   MOVE      '1'           Err987
102800960131     C                   MOVE      *on           $Fine
102900960131     C                   EXSR      EndPgm
103000951010     C                   ENDIF
103100960410     C                   endif
103200950301     C*
103300950301     C* Reperimento autorizzazioni
103400950301     C*
103500950301     C* le autorizzazioni potrebbero essere già state passate,
103600950301     C* nel qual caso non vanno reperite
103700950301    >C     AUTDS         IFEQ      *BLANK
103800950301    >C                   EXSR      REPAUT
103900950301    >C                   ELSE
104000070611    >C                   MOVEL     XCADDS        conppads
104100950301    >C                   ENDIF
104200950301     C*
104300950301     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
104400950301     C* le autorizzazione
104500950301     C*
104600950301    >C                   EXSR      TSTAUT
104700940715     C*
104800940715     C* Definizione dei tasti funzione utilizzabili a pgm
104900940715     C*
105000940715     C                   EXSR      DEFOPZ
105100940128     C*
105200940223     C* Variabili per gestione videate
105300940128     C*
105400940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
105500940223     C                   MOVE      *OFF          $FINE                          fine pgm
105600970623     C                   MOVE      *OFF          $FlgDef                        fine pgm
105700940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
105800940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
105900940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
106000970912     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
106100960605     C* azzero la posizione del cursore
106200970912     C                   Z-ADD     0             H1RIGA
106300970912     C                   Z-ADD     0             H1COLO
106400940127     C*
106500940223     C* Variabili appoggio
106600940127     C*
106700970912     C                   Z-ADD     0             CURR
106800970912     C                   Z-ADD     0             CURC
106900940207     C*
107000940207     C* Valorizzazione campi univoci testate
107100940207     C*
107200940207     C                   CLEAR                   FMTT1
107300940207     C                   MOVEL     KNSIF         NOMSIF
107400941110     C                   MOVEL     XSCDSI        NOMDIT
107500941027     C                   MOVEL     DSPGM         NOMPGM
107600950331     C* recupero il sottotitolo (se c'è)
107700950331     C                   MOVEL     *BLANK        T1TIT
107800950331     C     SOTTIT        IFNE      *BLANK
107900950331     C                   MOVEL     SOTTIT        IDMSG
108000950522     C                   CALLB     'XRTVM'
108100950331     C                   PARM                    IDMSG            27
108200950331     C                   PARM                    TXTMSG          100
108300950331     C                   PARM                    ERRMSG            1
108400950331     C                   PARM      *ON           CTRMSG            1            centratura
108500950331     C                   PARM      30            LENMSG            3 0          lun output.
108600950331     C     ERRMSG        IFNE      *ON
108700950331     C                   MOVEL     TXTMSG        T1TIT
108800950331     C                   ELSE
108900950331     C                   MOVEL     *ALL'?'       T1TIT
109000950331     C                   ENDIF
109100950331     C                   ENDIF
109200950331     C* recupero il tipo operazione
109300970912     C                   Z-ADD     1             X
109400070611    >C     OPZ987        LOOKUP    $OZ(X)                                 21
109500950331     C     *IN21         IFEQ      *ON
109600950331     C                   MOVEL     $ID(X)        IDMSG
109700950522     C                   CALLB     'XRTVM'
109800950331     C                   PARM                    IDMSG            27
109900950331     C                   PARM                    TXTMSG          100
110000950331     C                   PARM                    ERRMSG            1
110100950331     C                   PARM      *ON           CTRMSG            1            centratura
110200950331     C                   PARM      30            LENMSG            3 0          lun output.
110300950331     C     ERRMSG        IFNE      *ON
110400950331     C                   MOVEL     TXTMSG        T1OPE
110500950331     C                   ELSE
110600950331     C                   MOVEL     *ALL'?'       T1OPE
110700950331     C                   ENDIF
110800950331     C                   ENDIF
110900940224     C*
111000940224     C* Inizializzazione/Decodifica prima videata
111100940224     C*
111200940224     C                   EXSR      INZD1
111300940224     C                   MOVE      *OFF          $INZD1
111400940127     C*
111500940117     C                   ENDSR
111600941209     C/EJECT
111700941209     C************************************************************
111800941209     C* REPERIMENTO DATI SOCIETA'
111900941209     C************************************************************
112000941209     C     REPSOC        BEGSR
112100941209     C*
112200950522     C                   CALLB     'XSOC'
112300941209     C                   PARM                    TIPXSC            6
112400941209     C                   PARM                    SOCXSC            3
112500941209     C                   PARM                    CDSXSC            9 0
112600941209     C                   PARM                    MODXSC            3
112700941209     C                   PARM      *BLANKS       RTNXSC            1
112800941209     C                   PARM                    XSOCDS
112900941209     C                   PARM                    KPJBA
113000941209     C*
113100941209     C                   ENDSR
113200950301     C/EJECT
113300950301     C************************************************************
113400950301     C* REPERIMENTO AUTORIZZAZIONI UTENTE
113500950301     C************************************************************
113600950301     C     REPAUT        BEGSR
113700950301     C*
113800950301     C                   CLEAR                   AUTDS
113900950301     C*
114000950301     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
114100950301     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
114200970912     C                   Z-ADD     0             XCAGRP                         GRUPPO
114300950301     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
114400070611    >C                   MOVEL     'CONPPA'      XCAFNC                         FUNZIONE
114500950301    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
114600950301    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
114700950301    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
114800950301     C*
114900950522     C                   CALLB     'XCHKAUT'
115000950301     C                   PARM                    AUTDS
115100950301     C*
115200950301     C* errori/non abilitazione
115300950301     C     XCARTN        IFNE      0
115400070611    >C                   MOVE      '2'           Err987
115500990604     C                   MOVE      *ON           $FINE
115600950301     C                   EXSR      ENDPGM
115700950301     C                   ELSE
115800070611    >C                   MOVEL     XCADDS        CONPPADS
115900950301     C                   ENDIF
116000950301     C*
116100950301     C                   ENDSR
116200950301     C/EJECT
116300950301     C************************************************************
116400950301     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
116500950301     C************************************************************
116600950301     C     TSTAUT        BEGSR
116700950301     C*
116800950301     C                   SELECT
116900950301     C*
117000950301     C* se richiamato in immissione
117100070611    >C     OPZ987        WHENEQ    '01'
117200950301     C* ma l'utente non è abilitato
117300070611    >C*    IMM___        ANDEQ     *OFF
117400950301     C* esco da pgm avvertendolo
117500070611    >C*                  MOVE      '2'           ERR987
117600070611     C*                  MOVE      *ON           $FINE
117700070611     C*                  EXSR      ENDPGM
117800950301     C*
117900950301     C* se richiamato in modifica
118000070611    >C     OPZ987        WHENEQ    '02'
118100950301     C* ma l'utente non è abilitato
118200070611    >C*    MAN___        ANDEQ     *OFF
118300070611     C* testo se l'utente è abilitato almeno alla visualizzazione
118400070611    >C*    VIS___        IFEQ      *ON
118500070611    >C*                  MOVE      '05'          OPZ987
118600070611     C*                  ELSE
118700950301     C* altrimenti esco da pgm avvertendolo
118800070611    >C*                  MOVE      '2'           ERR987
118900070611     C*                  MOVE      *ON           $FINE
119000070611     C*                  EXSR      ENDPGM
119100070611     C*                  ENDIF
119200950301     C*
119300950301     C* se richiamato in copia
119400070611    >C     OPZ987        WHENEQ    '03'
119500950301     C* ma l'utente non è abilitato
119600070611    >C*    IMM___        ANDEQ     *OFF
119700950301     C* esco da pgm avvertendolo
119800070611    >C*                  MOVE      '2'           ERR987
119900070611     C*                  MOVE      *ON           $FINE
120000070611     C*                  EXSR      ENDPGM
120100070611     C*
120200950301     C* se richiamato in annullamento
120300070611    >C     OPZ987        WHENEQ    '04'
120400950301     C* ma l'utente non è abilitato
120500070611    >C     ANNIMP        ANDEQ     *OFF
120600950301     C* esco da pgm avvertendolo
120700070611    >C                   MOVE      '2'           ERR987
120800950301     C                   MOVE      *ON           $FINE
120900950301     C                   EXSR      ENDPGM
121000950301     C*
121100950301     C* se richiamato in visualizzazione
121200070611    >C     OPZ987        WHENEQ    '05'
121300950301     C* ma l'utente non è abilitato
121400070611    >C     VISIMP        ANDEQ     *OFF
121500950301     C* esco da pgm avvertendolo
121600070611    >C                   MOVE      '2'           ERR987
121700950301     C                   MOVE      *ON           $FINE
121800950301     C                   EXSR      ENDPGM
121900950301     C*
122000950301     C                   ENDSL
122100950301     C*
122200950301     C                   ENDSR
122300940715     C/EJECT
122400940715     C************************************************************
122500940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
122600940715     C************************************************************
122700940715     C     DEFOPZ        BEGSR
122800940715     C*
122900940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
123000940715     C* con S o N a seconda della utilizzabilità o meno
123100940715     C* del tasto funzione correlato
123200940715     C*
123300940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
123400940715     C* di compilazione
123500940715     C                   RESET                   $FC1
123600940715     C                   RESET                   $FR1
123700940715     C*
123800940715     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
123900940715     C* invalido alcune opzioni
124000940715     C                   SELECT
124100940715     C* Se pgm richiamato in interrogazione disattivo :
124200070611    >C     OPZ987        WHENEQ    '05'
124300980319     C* F2=Decodifica
124400980319     C                   Z-ADD     2             X
124500980319     C                   MOVE      '0'           $FC1(X)
124600940715     C* F4=Ricerca
124700970912     C                   Z-ADD     4             X
124800940720     C                   MOVE      '0'           $FC1(X)
124900940715     C* F5=Ripristino
125000970912     C                   Z-ADD     5             X
125100940720     C                   MOVE      '0'           $FC1(X)
125200940715     C* F6=Conferma
125300970912     C                   Z-ADD     06            X
125400940720     C                   MOVE      '0'           $FC1(X)
125500940715     C* Se pgm richiamato in annullamento disattivo :
125600070611    >C     OPZ987        WHENEQ    '04'
125700980319     C* F2=Decodifica
125800980319     C                   Z-ADD     2             X
125900980319     C                   MOVE      '0'           $FC1(X)
126000940715     C* F4=Ricerca
126100970912     C                   Z-ADD     4             X
126200940720     C                   MOVE      '0'           $FC1(X)
126300940715     C* F5=Ripristino
126400970912     C                   Z-ADD     5             X
126500940720     C                   MOVE      '0'           $FC1(X)
126600940715     C*
126700940715     C                   OTHER
126800940715     C*
126900940715     C                   ENDSL
127000940715     C*
127100940715     C                   ENDSR
127200950403     C/EJECT
127300950403     C************************************************************
127400950403     C* REPERIMENTO CAMPI DI DEFAULT
127500950403     C************************************************************
127600950403     C     REPDEF        BEGSR
127700950403     C*
127800950403     C*
127900950403     C                   ENDSR
128000940128     C************************************************************
128100950606     C* DEFINIZIONE KLIST
128200940223     C************************************************************
128300950606     C     DefKlist      BEGSR
128400940223     C*
128500940223     C* klist
128600940207     C*
128700070611    >C* NDIMP01L
128800100426c2156x***  K07IMP01      KLIST
128900100426c2156C     K08IMP01      KLIST
129000070611    >C                   KFLD                    IMPSocieta
129100070611    >C                   KFLD                    IMPkcc
129200070611    >C                   KFLD                    IMPksc
129300070611    >C                   KFLD                    IMPDtpar
129400070611    >C                   KFLD                    IMPNrpar
129500070611    >C                   KFLD                    IMPserpar
129600070611    >C                   KFLD                    IMPdtSca
129700100426c2156C                   KFLD                    IMPprogr
129800070611     C* NDPPA01L
129900070611     C     K07PPA01      KLIST
130000070611     C                   KFLD                    PPASocieta
130100070611     C                   KFLD                    PPACtb
130200070611     C                   KFLD                    PPAKcc
130300070611     C                   KFLD                    PPAKsc
130400070611     C                   KFLD                    PPADtPar
130500070611     C                   KFLD                    PPANrPar
130600070611     C                   KFLD                    PPASerPar
130700070611     C*NDpsc01l
130800070612     C     K03PSc01      KLIST
130900070611     C                   KFLD                    PSCSys
131000070611     C                   KFLD                    PSCNrasInt
131100070611     C                   KFLD                    PSCDtSca
131200940127     C*
131300940117     C                   ENDSR
131400940509     C/EJECT
131500940509     C************************************************************
131600940509     C* RIEMPIMENTO TASTI FUNZIONE
131700940509     C************************************************************
131800940509     C     RIEKEY        BEGSR
131900940509     C*
132000940509     C* Input :
132100940509     C* - $FX = schiera con tutti i testi disponibili
132200940509     C* - $FC = schiere con i flag di validità associati ai testi
132300940509     C* - $FR = schiere con i flag di REVERSE IMAGE
132400940509     C* - $ULK = ultimo testo caricato
132500940722     C* - $L01 = Lunghezza prima riga
132600940722     C* - $L02 = Lunghezza seconda riga
132700940509     C* Output :
132800940509     C* - $ULK
132900940609     C* - $KEY1= 1° riga di tasti funzione
133000940609     C* - $KEY2= 2° riga di tasti funzione
133100940509     C*
133200950522     C                   CALLB     'XKEYMSG'
133300940509     C* input
133400940509     C                   PARM                    $FX
133500940509     C                   PARM                    $FC
133600940509     C                   PARM                    $FR
133700940509     C                   PARM                    $ULK              3 0
133800940509     C* output
133900940509     C                   PARM                    $KEY1            79
134000940509     C                   PARM                    $KEY2            79
134100950726     C* input
134200950726     C                   PARM                    $L01              3 0
134300950726     C                   PARM                    $L02              3 0
134400940509     C*
134500060224     C                   PARM                    $ALLFUNCT       480
134600060224     C*
134700940509     C                   ENDSR
134800980319** $OZ / $ID
13490095040401PROMSG    *LIBL     COS0001
13500007061102PROMSG    *LIBL     COS0002
13510095040403PROMSG    *LIBL     COS0003
13520095040404PROMSG    *LIBL     COS0004
13530095040405PROMSG    *LIBL     COS0005
135400980319** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
135500940509PROMSG    *LIBL     KEY0001  01
135600940509PROMSG    *LIBL     KEY0002  02
135700940509PROMSG    *LIBL     KEY0003  03
135800940509PROMSG    *LIBL     KEY0004  04
135900940509PROMSG    *LIBL     KEY0005  05
136000940509PROMSG    *LIBL     KEY0006  06
136100940509PROMSG    *LIBL     KEY0007  07
136200940509PROMSG    *LIBL     KEY0008  08
136300940509PROMSG    *LIBL     KEY0009  09
136400940509PROMSG    *LIBL     KEY0010  10
136500940509PROMSG    *LIBL     KEY0011  11
136600940509PROMSG    *LIBL     KEY0012  12
136700940509PROMSG    *LIBL     KEY0013  13
136800940509PROMSG    *LIBL     KEY0014  14
136900940509PROMSG    *LIBL     KEY0015  15
137000940509PROMSG    *LIBL     KEY0016  16
137100940509PROMSG    *LIBL     KEY0017  17
137200940509PROMSG    *LIBL     KEY0018  18
137300940509PROMSG    *LIBL     KEY0019  19
137400940509PROMSG    *LIBL     KEY0020  20
137500940509PROMSG    *LIBL     KEY0021  21
137600940509PROMSG    *LIBL     KEY0022  22
137700940509PROMSG    *LIBL     KEY0023  23
137800940509PROMSG    *LIBL     KEY0024  24
137900980319**  ABILITAZIONE D1 : $FC1 / $FR1
1380009407200N  01
1381009407200N  02
1382009407201N  03
1383009407201N  04
1384009407201N  05
1385009407201N  06
1386009407200N  07
1387009407201N  08
1388009407200N  09
1389009411160N  10
1390009407200N  11
1391009407201N  12
1392009407200N  13
1393009407200N  14
1394009407200N  15
1395009407200N  16
1396009407200N  17
1397009407200N  18
1398009407200N  19
1399009407200N  20
1400009407200N  21
1401009407200N  22
1402009507260N  23   Impostare a '1' se si utilizzano opzioni
1403009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
