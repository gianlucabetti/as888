000100981228     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(QILE)
000200981216     H*PARMS OPTION(*NOXREF) CVTOPT(*NONE)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400981216     H*----------------------------------------------------*
000500981216     H*
000600981216    >H*      Gestione estensione nominativi partite
000700981216     H*     ----------------------------------------
000800981216     H*
000900981216     H*----------------------------------------------------*
001000981216    >FNDCPA01L  UF A E           K DISK    USROPN COMMIT(FLGCOM)
001001170223R027 F*------------
001002170223R027 FSDGSO301L UF   E           K DISK
001100970120     F*------------
001200981216     FNDPAS01L  IF   E           K DISK
001300970120     F*------------
001400970120     FNDPPA01L  IF   E           K DISK
001500940211     F*------------
001600070206A1210X**ANCAP01L  IF   E           K DISK
001700100824C2163x**ANCP101L  IF   E  ex A1210 K DISK
001800981229     F*------------
001900961204    >FNDC066D1  CF   E             WORKSTN
002000961204     F                                     RENAME(C066D1:FMTD1)
002100100723C2163F                                     RENAME(C066D2:FMTD2)
002200950712     F                                     RENAME(C066Z1:FMTZ1)
002300950712     F                                     RENAME(C066T1:FMTT1)
002400961206     F                                     INFDS(DSFMT)
002500950704     D*----------------------------------------------------*
002600981216     D*-------------
002700950704     D $TX             S             50    DIM(50)
002800950704     D $FL             S              1    DIM(50)
002900950712     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003000950712     D $ID             S             27    DIM(5) ALT($OZ)
003100940909     D* Schiere generali per la gestione dei tasti di funzione
003200940506     D $FM             S             27    DIM(24)
003300940506     D $FX             S             50    DIM(24)
003400940506     D $FC             S              1    DIM(24)
003500940506     D $FR             S              1    DIM(24)
003600940506     D* Schiere per la gestione dei tasti di funzione
003700981216     D* Video D1
003800950630     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003900950630     D $FX1            S             50    DIM(24)
004000950630     D $FC1            S              1    DIM(24)
004100950630     D $FR1            S              1    DIM(24)
004200940128     D*----------------------------------------------------*
004300940211     D* Passaggio Parametri
004400940211     D KPJBA         E DS
004500950712     D*-------------
004600950712     D* Reperimento Autorizzazioni
004700950712     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
004800950712     D*-------------
004900950712     D* Passaggio Autorizzazioni
005000950712     D DSAUT           DS           512
005100950712     D*-------------
005200950712     D* Maschera dati autorizzazioni
005300950712    >D CONPPADS      E DS
005400940211     D*-------------
005500981231     D* Controllo ABI/CAB
005600981231     D X59ABIDS      E DS                  INZ
005700981231     D*-------------
005800941209     D* Dati di ambiente ottenuti da XSOC
005900941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
006000941209     D*-------------
006100941209     D* DS Interna per dati di output di XSOC
006200941209     D XSOCDS          DS          1000
006300950712     D*-------------
006400950712     D* DS gestione parametri modulo
006500950712     D XPAOUT          DS          2000
006600940325     D*-------------
006700940325     D* Parametri in ricezione
006800961204    >D NDC066DS1     E DS
006801170223R224 X*ex R027 -----
006802170223 "   X*ex R027 Parametri del PGM di manutenzione  dati aggiuntivi
006803170223 "   X*ex R027 SDGSD16DS     E DS                  inz
006804170223 "   X*ex R027 RETD16       E                     INZ('0')
006805170223 "   X*ex R027 OPRD16       E                     INZ('0')
006806170223 "   X*ex R027 ERRD16       E                     INZ('0')
006807170223R224 X*ex R027 CMTD16       E                     INZ('0')
006900950713     D*-------------
007000981229     D* Richiamo a XATB
007100981229     D XATBDS        E DS                  INZ
007200981229     D*-------------
007300981229     D* Parametri per ricerca codice ANATB00F
007400981229     D XTABDS        E DS                  INZ
007500981229     D*-------------
007600990107     D* Passaggio Parametri al PGM ANA194R
007700990107     D ANA194DS      E DS                  Inz
007800990107     D  Err194       E                     Inz('0')
007900990107     D*-------------
008000990107     D* Passaggio Parametri al PGM ANA196R
008100990107     D ANA196DS      E DS                  Inz
008200990107     D  Err196       E                     Inz('0')
008300070206A1210X* Passaggio Parametri al PGM ANA801R
008400070206  "  X*A801DS        E DS                  EXTNAME(ANA801DS)
008500070206A1210X* ERR801       E                     INZ('0')
008600070206A1210D* Passaggio Parametri al PGM ANA801R
008700070206  "  D A803DS        E DS                  EXTNAME(ANA803DS)
008800070206A1210D  ERR803       E                     INZ('0')
008900100726C2163D*-------------
009000100726  "  D* Ricerca legende
009100100726  "  D Ana964DS      E DS                  INZ
009200100726  "  D*-------------
009300100726  "  D* Controlli validità legende
009400100726C2163D X07ValDS      E DS                  INZ
009500990107     D*-------------
009600981229     D* DS esterna provincia
009700981229     D TABC17        E DS                  EXTNAME(ANGC17DS) INZ
009800981229     D*-------------
009900981229     D* DS esterna stato
010000981229     D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
010100981229     D*-------------
010200981216     D DSFMT           DS           512
010300981216     D  $TASTO               369    369
010400981216     D  NRG                  370    370
010500981216     D  NCL                  371    371
010600961210     D*-------------
010700940127     D* Reperimento nome PGM
010800940127     D STATUS         SDS           333
010900940127     D  DSPGM            *PROC
011000950411     D  PARMS            *PARMS
011100940207     D*-------------
011200940616     D* Costanti
011300950712     D*-------------
011400981216     D SOTTIT          C                   CONST('PROMSG    *LIBL-
011500981216     D                                          COS0440')
011600940616     D*-------------
011700940506     D* Tasti di funzione
011800940506     D F01             C                   CONST(X'31')
011900940506     D F02             C                   CONST(X'32')
012000940506     D F03             C                   CONST(X'33')
012100940506     D F04             C                   CONST(X'34')
012200940506     D F05             C                   CONST(X'35')
012300940506     D F06             C                   CONST(X'36')
012400940506     D F07             C                   CONST(X'37')
012500940506     D F08             C                   CONST(X'38')
012600940506     D F09             C                   CONST(X'39')
012700940506     D F10             C                   CONST(X'3A')
012800940506     D F11             C                   CONST(X'3B')
012900940506     D F12             C                   CONST(X'3C')
013000940506     D F13             C                   CONST(X'B1')
013100940506     D F14             C                   CONST(X'B2')
013200940506     D F15             C                   CONST(X'B3')
013300940506     D F16             C                   CONST(X'B4')
013400940506     D F17             C                   CONST(X'B5')
013500940506     D F18             C                   CONST(X'B6')
013600940506     D F19             C                   CONST(X'B7')
013700940506     D F20             C                   CONST(X'B8')
013800940506     D F21             C                   CONST(X'B9')
013900940506     D F22             C                   CONST(X'BA')
014000940506     D F23             C                   CONST(X'BB')
014100940506     D F24             C                   CONST(X'BC')
014200940506     D ENTER           C                   CONST(X'F1')
014300100726C2163D ROLDWN          C                   CONST(X'F4')
014400100805C2163D ROLLUP          C                   CONST(X'F5')
014500961206     D*-------------
014600961206     D*  Indici
014700961206     D X               S              3S 0
014800950524     D*-------------
014900950606     D* Variabili per gestione videate
015000981216     D $Open           S              1A
015100950606     D $Gest           S              2A
015200950606     D $Fine           S              1A
015300981216     D $InzD1          S              1A
015400950606     D $LastG          S              2A
015500950606     D $LastV          S              2A
015600950606     D $EndFile        S              1A
015700950606     D $Esci           S              1A
015800990107     D Win             S             99A
015900950606     D*-------------
016000990107     D* variabili d'appoggio
016100990107     D WWPrv           S                   Like(CPAProv)
016200990107     D WWSta           S                   Like(CPAStato)
016300990107     D WWCap           S                   Like(CPACap)
016400990107     D*-------------
016500990104     D* Attributi video
016600990104     D* Normale
016700990104     D Normale         C                   CONST(X'20')
016800990104     D* Alta intensità/Reverse image
016900990104     D HiRi            C                   CONST(X'23')
017000990104     D* Non protetto/Alta intensità/Separatore colonne
017100990104     D HiCs            C                   CONST(X'32')
017200990104     D* Non protetto/Alta intensità/Separatore colonne/Reverse image
017300990104     D HiCsRi          C                   CONST(X'33')
017400990104     D* Protetto/Alta intensità/Separatore colonne
017500990104     D PrHiCs          C                   CONST(X'B2')
017600990104     D* Protetto/Alta intensità/Separatore colonne/Reverse image
017700990104     D PrHiCsRi        C                   CONST(X'B3')
017800100824     D*--------------
017900100825C2163D* Passaggio dati a pgm controllo CAP
018000100825  "  D WProvincia      S              2A
018100100825  "  D WStato          S              4A
018200100825  "  D WCAP            S              9A
018300100825  "  D WLocalita       S             34A
018400100825C2163D WErrore         S              1A
018500990104     D*-------------
018600000118C1309D ZDateISO        S               D
018700100726C2163D DataISO         S               D
018800940127     C*----------------------------------------------------*
018900940127     C*                MAIN LINE PROGRAM
019000940127     C*----------------------------------------------------*
019100940127     C*
019200940223     C* inizializzazione variabili
019300940223     C                   EXSR      INZVAR
019400981216     C*
019500981216     C* controlli iniziali
019600981216     C                   EXSR      CtrInizio
019700981216     C*
019800981216     C* riempimento video D1
019900981216     C                   EXSR      RieD1
020000961206     C*
020100940223     C     $FINE         DOWEQ     *OFF
020200981216     C     $GEST         CASEQ     'D1'          GESD1
020300100723C2163C     $GEST         CASEQ     'D2'          GESD2
020400941027     C                   ENDCS
020500941027     C                   ENDDO
020600940325     C* fine programma
020700940325     C                   EXSR      ENDPGM
020800940225     C*
020900940325     C/EJECT
021000940325     C************************************************************
021100940325     C* FINE PROGRAMMA
021200940325     C************************************************************
021300940325     C     ENDPGM        BEGSR
021400950712     C*
021500950712     C* se il COMMIT non è demandato al pgm chiamante
021600981216    >C     CMT0661       IFEQ      '0'
021700950712     C*
021800950712     C* test se esistono transazioni attive ed eventualmente cosa fare
021900950712     C                   Z-ADD     0             EXECOD
022000950712     C                   CALL      'XRCMTI'
022100950712     C                   PARM                    EXECOD            1 0
022200950712     C*
022300950712     C                   SELECT
022400950712     C* Annulla richiesta
022500950712     C     EXECOD        WHENEQ    1
022600950712     C                   MOVEL     *OFF          $FINE
022700950712     C                   MOVEL     *HIVAL        $LASTV
022800950712     C* Conferma uscita
022900950712     C     EXECOD        WHENEQ    2
023000950712     C                   ROLBK
023100950712     C*
023200950712     C                   ENDSL
023300950712     C*
023400950712     C                   ENDIF
023500950712     C*
023600950712     C* se ho confermato l'uscita proseguo
023700950712     C     $FINE         IFEQ      *ON
023800950712     C*
023900950712     C* passaggio dati al pgm chiamante
024000961204    >C                   MOVEL     NDC066DS1     KPJBU
024100961206     C*
024200981231     C                   SETON                                            LR
024300950712     C                   RETURN
024400950712     C*
024500950712     C                   ENDIF
024600950712     C*
024700950712     C                   ENDSR
024800940224     C/EJECT
024900940224     C************************************************************
025000940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
025100940224     C************************************************************
025200940224     C     GESPIE        BEGSR
025300940224     C*
025400940224     C                   SELECT
025500981216     C*
025600981216     C     $GEST         WHENEQ    'D1'
025700981216     C                   EXSR      F24D1
025800950704     C                   WRITE     FMTZ1
025900100726     C*
026000100726C2163C     $GEST         WHENEQ    'D2'
026100100726  "  C     $ULKD1        IFEQ      0
026200100726  "  C* riempimento opzioni
026300100726  "  C                   EXSR      F24D2
026400100726  "  C                   ELSE
026500100726  "  C                   WRITE     FMTZ1
026600100825C2163C                   ENDIF
026700940506     C                   ENDSL
026800100723     C*
026900940506     C*
027000940224     C                   ENDSR
027100940307     C/EJECT
027200961206     C************************************************************
027300981216     C* GESTIONE F24 VIDEO D1
027400961206     C************************************************************
027500981216     C     F24D1         BEGSR
027600961206     C*
027700961206     C                   MOVEA     $FX1          $FX
027800961206     C                   MOVEA     $FC1          $FC
027900961206     C                   MOVEA     $FR1          $FR
028000981216     C                   Z-ADD     $ULKD1        $ULK
028100961206    >C                   Z-ADD     79            $L01
028200961206    >C                   Z-ADD     79            $L02
028300961206     C                   EXSR      RIEKEY
028400981216     C                   Z-ADD     $ULK          $ULKD1            3 0
028500961206     C                   MOVEA     $FC           $FC1
028600961206     C                   MOVEL     $KEY1         Z1KE1
028700060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
028800961206     C                   MOVEL     $KEY2         Z1KE2
028900961206     C                   WRITE     FMTZ1
029000961206     C*
029100961206     C                   ENDSR
029200961206     C/EJECT
029300961206     C************************************************************
029400961206     C     RIEKEY        BEGSR
029500961206     C************************************************************
029600961206     C*
029700961206     C* Input :
029800961206     C* - $FX = schiera con tutti i testi disponibili
029900961206     C* - $FC = schiere con i flag di validità associati ai testi
030000961206     C* - $FR = schiere con i flag di REVERSE IMAGE
030100961206     C* - $ULK = ultimo testo caricato
030200961206     C* - $L01 = Lunghezza prima riga
030300961206     C* - $L02 = Lunghezza seconda riga
030400961206     C* Output :
030500961206     C* - $ULK
030600961206     C* - $KEY1= 1° riga di tasti funzione
030700961206     C* - $KEY2= 2° riga di tasti funzione
030800961206     C*
030900961206     C                   CALLB     'XKEYMSG'
031000961206     C* input
031100961206     C                   PARM                    $FX
031200961206     C                   PARM                    $FC
031300961206     C                   PARM                    $FR
031400961206     C                   PARM                    $ULK              3 0
031500961206     C* output
031600961206     C                   PARM                    $KEY1            79
031700961206     C                   PARM                    $KEY2            79
031800961206     C* input
031900961206    >C                   PARM                    $L01              3 0
032000961206    >C                   PARM                    $L02              3 0
032100961206     C*
032200060224C1913C                   PARM                    $ALLFUNCT       480
032300060224C1913C*
032400961206     C                   ENDSR
032500961206     C/EJECT
032600940127     C************************************************************
032700940131     C* GESTIONE LISTA
032800940127     C************************************************************
032900981216     C     GESD1         BEGSR
033000941205     C*
033100961210     c     su            tag
033200961210     C*
033300990107     C* se esistono errori sulla videata
033400990107     C* emetto la write del formato a indicatori spenti per vedere
033500990107     C* le eventuali decodifiche
033600990107     C     *IN99         IFEQ      *ON
033700990107     C                   MOVEA     *IN           WIN
033800990107     C                   MOVE      *ALL'0'       IN5098           49
033900990107     C                   MOVEA     IN5098        *IN(50)
034000990107     C                   WRITE     FMTD1
034100990107     C                   MOVEA     WIN           *IN
034200990107     C                   ENDIF
034300990107     C*
034400961204     C                   EXFMT     FMTD1
034500981221     C*
034600981221     C* controllo abilitazione tasti funzione
034700981221     C                   EXSR      CTRKD1
034800961206     C*
034900961210     C* F12=Ritorno
035000961210     C     $TASTO        ifeq      F12
035100981216     C                   EXSR      F12D1
035200961211     c     $fine         ifeq      *on
035300961211     c                   exsr      endpgm
035400961211     c                   end
035500961210     c                   end
035600961210     C*
035700961210     C     *IN99         IFEQ      *on
035800961210     C                   goto      su
035900961210     C                   end
036000961210     C*
036100961210     C     *IN99         IFEQ      *OFF
036200940127     C*
036300941027     C                   SELECT
036400100723C2163C* ROLLUP
036500100723  "  C     $TASTO        WHENEQ    ROLLUP
036600100723C2163C                   MOVE      'D2'          $GEST
036700961211     C*
036800990107     C* F4=Ricerca
036900990107     C     $TASTO        WHENEQ    F04
037000990107     C                   EXSR      SEARCH
037100990107     C*
037200941027     C                   OTHER
037300981229     C                   EXSR      CTRD1
037400100723     C*
037500100723C2163C     *IN99         IFEQ      *OFF
037600100723  "  C                   EXSR      CTRD2
037700100723  "  C     *IN99         IFEQ      *ON
037800100723  "  C                   SETON                                        96
037900100723  "  C                   ENDIF
038000100723C2163C                   ENDIF
038100981221     C*
038200981221   B3C     $TASTO        IFEQ      F16
038300981221     C     *IN99         ANDEQ     *OFF
038400981221     C* eseguo annullamento
038500981221     C                   Move      '04'          OPZ0661
038600981221     C                   EXSR      AGGANA
038700981221   E3C                   ENDIF
038800950712     C*
038900950712   B3C     $TASTO        IFEQ      F06
039000950712     C     *IN99         ANDEQ     *OFF
039100950712     C* eseguo aggiornamento
039200950712     C                   EXSR      AGGANA
039300950712   E3C                   ENDIF
039400941205     C*
039500941027     C                   ENDSL
039600940127     C*
039700940506     C                   ENDIF
039800941205     C*
039900940127     C                   ENDSR
040000950714     C/EJECT
040100990107     C************************************************************
040200990107     C* RICERCHE
040300990107     C************************************************************
040400990107     C     SEARCH        BEGSR
040500990107     C*
040600990107     C* determino Riga/Colonna del cursore
040700990107     C*********          MOVE      NRG           R$$
040800990107     C*********          MOVE      NCL           C$$
040900990107     C*********          Z-ADD     RIRI          CURR
041000990107     C*********          Z-ADD     COCO          CURC
041100990107     C*
041200990107   B1C                   SELECT
041300990107     C*
041400990107     C* FMTD1  - PROVINCIA
041500990107     C     H1NMFL        WHENEQ    'D1PROV'
041600990107     C                   RESET                   XTABDS
041700990107     C                   MOVE      '01'          XTAOPZ
041800990107     C                   MOVE      XSCSOC        XTAAZI
041900990107     C                   MOVE      XSCLIN        XTALIN
042000990107     C*******            Z-ADD     CURR          XTARIG
042100990107     C*******            Z-ADD     CURC          XTACOL
042200990107     C                   CLEAR                   KPJBU
042300990107     C                   MOVEL     XTABDS        KPJBU
042400990107     C                   CALL      'ANGC17R'
042500990107     C                   PARM                    KPJBA
042600990107     C                   MOVEL     KPJBU         XTABDS
042700990107     C     XTAERR        IFNE      '1'
042800990107     C                   MOVE      XTAKEY        WWPRV
042900990107     C                   ELSE
043000990107     C                   MOVE      *BLANK        WWPRV
043100990107     C                   ENDIF
043200990107     C*
043300990107     C     WWPRV         IFNE      *BLANK
043400990107     C                   MOVEL     WWPRV         D1PROV
043500990107     C                   ENDIF
043600990107     C*
043700070206a1210x* FMTD1  - C.A.P.
043800070206  "  x*    H1NMFL        WhenEq    'D1CAP'
043900070206  "  x*                  Reset                   A801DS
044000070206  "  x*                  Move      '01'          OPZ801
044100070206  "  x*                  Move      XscSoc        Societa801
044200070206  "  x*                  Move      D1Stato       Stato801
044300070206  "  x*                  Clear                   KPJBU
044400070206  "  x*                  Movel     A801DS        KPJBU
044500070206  "  x*                  Call      'ANA801R'
044600070206  "  x*                  Parm                    KPJBA
044700070206  "  x*                  Movel     KPJBU         A801DS
044800070206  "  x*    ERR801        IfNe      '1'
044900070206  "  x*                  Move      CAP801        WWCAP
045000070206  "  x*                  Else
045100070206  "  x*                  Move      *BLANK        WWCAP
045200070220  "  x*                  EndIf
045300070220  "  x**   WWCAP         IfNe      *BLANK
045400070220  "  x**                 Eval      D1Cap  =  WWCap
045500070220  "  x**                 If        D1Localit = *Blank
045600070220  "  X**                 Eval      D1Localit =  Localit801
045700070220  "  x**                 EndIf
045800070220  "  x**                 If        D1Stato  =  *blank
045900070220  "  x**                 Eval      D1Stato  =  Stato801
046000070220  "  x**                 Endif
046100070220  "  x**                 If        D1Prov =  *blank
046200070220  "  x**                 Eval      D1Prov =  Prov801
046300070220  "  x**                 Endif
046400070220A1210X**                 ENDIF
046500070220A1210C* FMTD1  - C.A.P.
046600070206  "  C     H1NMFL        WhenEq    'D1CAP'
046700070220  "  C     H1NMFL        oreq      'D1LOCALIT'
046800070206  "  C                   Reset                   A803DS
046900070206  "  C                   Move      '01'          OPZ803
047000070220  "  C                   move      xscsoc        Societa803
047100070206  "  C                   Move      D1Stato       Stato803
047200070220  "  C                   Move      D1Cap         Cap803
047300070220  "  C                   Move      D1Prov        Prov803
047400070220  "  C                   Move      D1Localit     Localit803
047500070206  "  C                   Clear                   KPJBU
047600070206  "  C                   Movel     A803DS        KPJBU
047700070206  "  C                   Call      'ANA803R'
047800070206  "  C                   Parm                    KPJBA
047900070206  "  C                   Movel     KPJBU         A803DS
048000070220  "  C     REt803        IfNe      '2'
048100070220  "  C                   Move      CAP803        D1CAP
048200070220  "  C                   Eval      D1Localit =  Localit803
048300070220  "  c                   Eval      D1Prov =  Prov803
048400070220  "  C                   Eval      D1Stato  =  Stato803
048500070206a1210C                   EndIf
048600990107     C*
048700990107     C*
048800990107     C* FMTD1  - STATO
048900990107     C     H1NMFL        WHENEQ    'D1STATO'
049000990107     C                   RESET                   XTABDS
049100990107     C                   MOVE      '01'          XTAOPZ
049200990107     C                   MOVE      XSCSOC        XTAAZI
049300990107     C                   MOVE      XSCLIN        XTALIN
049400990107     C********           Z-ADD     CURR          XTARIG
049500990107     C********           Z-ADD     CURC          XTACOL
049600990107     C                   CLEAR                   KPJBU
049700990107     C                   MOVEL     XTABDS        KPJBU
049800990107     C                   CALL      'ANGG12R'
049900990107     C                   PARM                    KPJBA
050000990107     C                   MOVEL     KPJBU         XTABDS
050100990107     C     XTAERR        IFNE      '1'
050200990107     C                   MOVE      XTAKEY        WWSTA
050300990107     C                   ELSE
050400990107     C                   MOVE      *BLANK        WWSTA
050500990107     C                   ENDIF
050600990107     C*
050700990107     C     WWSTA         IFNE      *BLANK
050800990107     C                   MOVEL     WWSTA         D1STATO
050900990107     C                   ENDIF
051000990107     C*
051100990107     C* FMTD1  - ABI
051200990107     C     H1NMFL        WHENEQ    'D1ABI'
051300990107     C                   RESET                   Ana194Ds
051400990107     C                   MOVEL     D1ABI         ABI194
051500990107     C                   MOVEL     XSCSOC        Societa194
051600990107     C                   Movel     '01'          Opz194
051700990107     C                   MOVEL     Ana194Ds      KPJBU
051800990107     C                   CALL      'ANA194R'
051900990107     C                   PARM                    KPJBA
052000990107     C                   MOVEL     KPJBU         Ana194Ds
052100990107     C*
052200990107 b1  C     ABI194        IFNE      *BLANK
052300990107     C     ERR194        ANDEQ     *OFF
052400990107     C                   MOVE      ABI194        D1ABI
052500990107 e1  C                   ENDIF
052600990107     C*
052700990107     C     H1NMFL        WHENEQ    'D1CAB'
052800990107     C                   RESET                   Ana196Ds
052900990107     C                   MOVEL     D1ABI         ABI196
053000990107     C                   MOVEL     D1CAB         CAB196
053100990107     C                   MOVEL     XSCSOC        Societa196
053200990107     C                   Movel     '01'          Opz196
053300990107     C                   MOVEL     Ana196Ds      KPJBU
053400990107     C                   CALL      'ANA196R'
053500990107     C                   PARM                    KPJBA
053600990107     C                   MOVEL     KPJBU         Ana196Ds
053700990107     C*
053800990107 b1  C     CAB196        IfNe      *Blank
053900990107     C     ERR196        AndEq     *Off
054000990107     C                   Move      Cab196        D1Cab
054100990107     C                   If        D1Abi = *blank
054200990107     C                   Move      Abi196        D1Abi
054300990107     C                   EndIf
054400990107 e1  C                   EndIf
054500100723     C*
054600100723C2163C* FMTD2  - LEGENDA SESSO
054700100723  "  C     H1NMFL        WHENEQ    'D2SESSO'
054800100726  "  C                   RESET                   Ana964Ds
054900100723  "  C                   MOVEL     '01'          OPZ964
055000100723  "  C                   MOVEL     'ANSOG000'    TRC964
055100100723  "  C                   MOVEL     'SOGSEX'      CAM964
055200100726  "  C                   MOVEL     Ana964Ds      KPJBU
055300100723  "  C                   CALL      'ANA964R'
055400100723  "  C                   PARM                    KPJBA
055500100726  "  C                   MOVEL     KPJBU         Ana964Ds
055600100723  "  C*
055700100723  "  C     ret964        IFNE      '2'
055800100723  "  C                   MOVEL     val964        D2SESSO
055900100723  "  C                   MOVEL     des964        D2SESSOD
056000100723  "  C                   ENDIF
056100100723  "  C*
056200100723  "  C* FMTD1  - PROVINCIA
056300100726  "  C     H1NMFL        WHENEQ    'D2PROVNA'
056400100723  "  C                   RESET                   XTABDS
056500100723  "  C                   MOVE      '01'          XTAOPZ
056600100723  "  C                   MOVE      XSCSOC        XTAAZI
056700100723  "  C                   MOVE      XSCLIN        XTALIN
056800100723  "  C                   CLEAR                   KPJBU
056900100723  "  C                   MOVEL     XTABDS        KPJBU
057000100723  "  C                   CALL      'ANGC17R'
057100100723  "  C                   PARM                    KPJBA
057200100723  "  C                   MOVEL     KPJBU         XTABDS
057300100723  "  C     XTAERR        IFNE      '1'
057400100723  "  C                   MOVE      XTAKEY        WWPRV
057500100723  "  C                   else
057600100723  "  C                   MOVE      *BLANK        WWPRV
057700100723  "  C                   endif
057800100726  "  C                   If        WWPRV <> *blanks
057900100726  "  C                   MOVE      WWPRV         d2provna
058000100723  "  C                   ENDIF
058100100723  "  C* FMTD1  - STATO
058200100726  "  C     H1NMFL        WHENEQ    'D2STATNA'
058300100723  "  C                   RESET                   XTABDS
058400100723  "  C                   MOVE      '01'          XTAOPZ
058500100723  "  C                   MOVE      XSCSOC        XTAAZI
058600100723  "  C                   MOVE      XSCLIN        XTALIN
058700100723  "  C                   CLEAR                   KPJBU
058800100723  "  C                   MOVEL     XTABDS        KPJBU
058900100723  "  C                   CALL      'ANGG12R'
059000100723  "  C                   PARM                    KPJBA
059100100723  "  C                   MOVEL     KPJBU         XTABDS
059200100723  "  C     XTAERR        IFNE      '1'
059300100723  "  C                   MOVE      XTAKEY        WWSTA
059400100723  "  C                   ELSE
059500100723  "  C                   MOVE      *BLANK        WWSTA
059600100723  "  C                   ENDIF
059700100726  "  C                   If        WWSTA <> *blanks
059800100726  "  C                   MOVE      WWSTA         D2Statna
059900100723  "  C                   ENDIF
060000100723  "  C* FMTD1  - C.A.P.
060100100726  "  C     H1NMFL        WhenEq    'D2CAPNA'
060200100726  "  C     H1NMFL        oreq      'D2COMUNA'
060300100723  "  C*
060400100723  "  C                   Reset                   A803DS
060500100723  "  C                   Move      XscSoc        Societa803
060600100723  "  C                   Move      '01'          OPZ803
060700100726  "  C                   Move      D2Statna      Stato803
060800100726  "  C                   Move      D2Capna       Cap803
060900100726  "  C                   Move      D2Provna      Prov803
061000100726  "  C                   Move      D2Comuna      Localit803
061100100723  "  C                   Clear                   KPJBU
061200100723  "  C                   Movel     A803DS        KPJBU
061300100723  "  C                   Call      'ANA803R'
061400100723  "  C                   Parm                    KPJBA
061500100723  "  C                   Movel     KPJBU         A803DS
061600100723  "  C     Ret803        IfNe      '2'
061700100726  "  C                   eval      D2CapNA=  Cap803
061800100726  "  C                   Eval      D2ComuNa=  Localit803
061900100726  "  C                   Eval      D2ProvNA=  Prov803
062000100726  "  C                   Eval      D2StatNA=  Stato803
062100100723C2163C                   ENDIF
062200100723     C*
062300990107     C* ricerca non abilitata in questa posizione
062400990107   X1C                   OTHER
062500990107     C                   SETON                                        9899
062600990107     C*
062700990107   E1C                   ENDSL
062800990107     C*
062900990107     C* imposto pos. del cursore
063000990107     C********           Z-ADD     CURR          H1RIGA
063100990107     C********           Z-ADD     CURC          H1COLO
063200990107     C*
063300990107     C                   MOVE      *HIVAL        $LASTV
063400990107     C*
063500990107     C                   ENDSR
063600990107     C/EJECT
063700981229     C************************************************************
063800981229     C* CONTROLLO VIDEATA
063900981229     C************************************************************
064000981229     C     CTRD1         BEGSR
064100981229     C*
064200981229     C                   SETOFF                                       99
064300981229     C* non imposto pos. del cursore
064400981229     C                   Z-ADD     0             H1RIGA
064500981229     C                   Z-ADD     0             H1COLO
064600981229     C*
064700981229     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
064800981229     C*
064900981229     C     OPZ0661       IFNE      '04'
065000981229     C*
065100981229     C* Ctrl obbligatorietà ragione sociale
065200981229     C     D1DESCR       IFEQ      *BLANK
065300981229     C                   SETON                                        5299
065400981229     C                   ENDIF
065500981229     C*
065600981229     C* Ctrl obbligatorietà indirizzo
065700981229     C     D1INDIRIZ     IFEQ      *BLANK
065800981229     C                   SETON                                        5399
065900981229     C                   ENDIF
066000981229     C*
066100981229     C* CTRL OBBLIGATORIETA' LOCALITA'
066200981229     C     D1LOCALIT     IFEQ      *BLANK
066300981229     C                   SETON                                        5499
066400981229     C                   ENDIF
066500990107     C*
066600990107     C* CTRL VALIDITA' PROVINCIA
066700990107     C                   MoveL     *Blank        D1ProvD
066800990107     C     D1PROV        IFNE      *BLANK
066900990107     C                   Reset                   XAtbDs
067000990107     C                   MOVE      '1'           XTBRIC
067100990107     C                   MOVE      XSCSOC        XTBAZI
067200990107     C                   MOVE      *ZERO         XTBKEY
067300990107     C                   MOVE      'C17'         XTBCOD
067400990107     C                   MOVE      D1PROV        WRK06A            6
067500990107     C                   MOVEL     D1STATO       WRK06A
067600990107     C                   MOVE      WRK06A        XTBKEY
067700990107     C                   CALLb     'XATB'
067800990107     C                   PARM                    XATBDS
067900990107     C     XTBERR        IFEQ      '0'
068000990107     C                   MOVEL     XTBUNI        TABC17
068100990107     C                   MOVEL     DESC17        D1ProvD
068200990107     C                   ELSE
068300990107     C                   SETON                                        5699
068400990107     C                   ENDIF
068500990107     C                   ENDIF
068600981229     C*
068700981229     C* Ctrl obbligatorietà stato
068800981229     C                   Clear                   TabG12
068900990107     C                   Clear                   D1StatoD
069000990107     C                   Clear                   D1CapD
069100981229   1bC                   if        D1Stato  =    *Blank
069200981229     C                   SETON                                        5599
069300981229     C*
069400981229     C* Ctrl validità stato
069500981229   1xC                   else
069600981229     C                   Reset                   XAtbDs
069700981229     C                   MOVE      '1'           XTBRIC
069800981229     C                   MOVE      XSCSOC        XTBAZI
069900981229     C                   MOVE      *Zero         XTBKEY
070000981229     C                   MOVE      'G12'         XTBCOD
070100981229     C                   MOVE      D1Stato       XTBKEY
070200981229     C                   CALLb     'XATB'
070300981229     C                   PARM                    XATBDS
070400981229   2bC     XTBErr        IFEQ      '0'
070500981229     C                   MOVEL     XTBUNI        TABG12
070600990107     C                   MOVEL     DESG12        D1StatoD
070700100824C2163C*
070800100824  "  C* Controllo coerenza C.A.P.
070900100824  "  C                   movel     D1Localit     WLocalita
071000100824  "  C                   movel     D1Cap         WCAP
071100100824  "  C                   movel     D1Prov        WProvincia
071200100824  "  C                   movel     D1Stato       WStato
071300100824  "  C*
071400100824  "  C                   exsr      CtrCap
071500100824  "  C*
071600100824  "  C* Se reperita provincia la propone a video
071700100824  "  C                   If        D1Prov = *blanks
071800100824  "  C                             and WProvincia <> *blanks
071900100824  "  C                   eval      D1Prov = WProvincia
072000100824  "  C                   endif
072100100824  "  C* Se reperita località la propone a video
072200100824  "  C                   If        D1Localit = *blanks
072300100824  "  C                             and WLocalita <> *blanks
072400100824  "  C                   eval      D1Localit = WLocalita
072500100824  "  C                   endif
072600100824  "  C*-
072700100824  "  x***Ctrl flag esistenza CAP
072800100824  "  x***                If        D1Cap <> *Blank
072900100825C2163x***                          and CECG12 = *On
073000070206A1210X**                 Move      D1Stato       CapStato
073100070206  "  X**                 Move      D1Cap         CapCap
073200070206A1210x***  K02CAP01      Chain     Ancap01l                           21
073300100824C2163x*** ex A1210       Move      D1Stato       CP1Stato
073400100824  "  x*** ex A1210       Move      D1Cap         CP1Cap
073500100824  "  x***  K02Cp101      Chain     Ancp101l                           21
073600100824  "  x***                If        *in21  =  *on
073700100824  "  x***                Seton                                        6099
073800101011C2163x***                Else
073900070206A1210X***                Movel     CapLocalit    D1CapD
074000100824C2163x*** ex A1210       Movel     CP1Localit    D1CapD
074100100824C2163x***                If        D1Prov =  *blank
074200070206A1210X**                           and  CapProv  <>  *blank
074300070206A1210X**                 Move      CapProv       D1Prov
074400100824C2163x*** ex A1210                 and  CP1Prov  <>  *blank
074500100824  "  x*** ex A1210       Move      Cp1Prov       D1Prov
074600100824  "  x***                Endif
074700100824C2163x***                If        D1Localit =  *blank
074800070206A1210x**                           and  CapLocalit  <>  *blank
074900070206A1210x**                 Move      CapLocalit    D1Localit
075000100824C2163x*** ex A1210                 and  Cp1Localit  <>  *blank
075100100824  "  x*** ex A1210       Move      Cp1Localit    D1Localit
075200100824  "  x***                Setoff                                       54
075300100824  "  x***                Endif
075400100824  "  x***                Endif
075500100824  "  x***                Endif
075600100824  "  x***
075700100824  "  x*** rl nr. caratteri del CAP
075800100824  "  x***                if        D1CAP     <>  *Blank    and
075900100824  "  x***                          CAPG12    <>  0
076000100824  "  x***  ' '           CHECKR    D1CAP         X
076100100824  "  x***                if        X         <>  CAPG12
076200100824  "  x***                SetOn                                        5999
076300100824  "  x***                EndIf
076400100824  "  x***                EndIf
076500100824C2163x***
076600981229   2xC                   Else
076700981229     C* si è verificato un errore
076800981229     C                   SetOn                                        5799
076900981229   2eC                   ENDIF
077000981229     C*
077100981229   1eC                   endif
077200981229     C*
077300981229     C* CTRL VALIDITA' CODICE FISCALE
077400981229   1bC                   if        D1CDFisc <>    *Blank
077500981229     C* eseguo i controlli formali
077600010215B0334X***                Call      'XCDFIS'
077700010215B0334C                   Callb     'XCDFIS'
077800981229     C                   Parm                    D1CDFisc
077900981229     C                   Parm                    XStato            1 0
078000981229     C*
078100981229     C* se rilevato un errore
078200981229   3bC                   if        XStato    <     0
078300010215B0334X***                Call      'XPARIVA'
078400010215B0334C                   Callb     'XPARIVA'
078500981229     C                   Parm                    D1CDFisc
078600981229     C                   Parm      *Zero         XStato            1 0
078700981229     C                   Parm                    XScSoc
078800981229   4bC                   if        XStato    <     0
078900981229     C* se trovato errore
079000981229     C                   SetOn                                        5899
079100981229   4eC                   endif
079200981229   4eC                   endif
079300981229     C*
079400981229   3eC                   Endif
079500981229     C*
079600981229     C* controllo validità PARTITA IVA
079700981229   1bC                   if        D1PartIVA <> *Blank
079800981229     C* se non richiesta forzatura
079900981229     C                   if        D1Stato    =  XScSta           and
080000981229     C                             ( %SubSt(D1PartIVA:1:1) < '0'  or
080100981229     C                               %SubSt(D1PartIVA:1:1) > '9'     )
080200981229     C* se il soggetto è italiano
080300981229     C* e il primo carattere del codice part. IVA non è un numero
080400981229     C* NON faccio il controllo con XPARIVA perchè trovando nel 1° carattere
080500981229     C* una lettera lo considerrebbe di un soggetto estero e quindi non
080600981229     C* farebbe alcun controllo formale
080700981229     C*
080800981229     C* segnalo l'errore formale sulla partita IVA
080900981229     C                   SetOn                                        6199
081000981229     C*
081100981229     C                   else
081200010215B0334X***                Call      'XPARIVA'
081300010215B0334C                   Callb     'XPARIVA'
081400981229     C                   Parm                    D1PartIVA
081500981229     C                   Parm                    XStato            1 0
081600981229     C                   PARM                    XScsoc
081700981229     C* se rilevato un errore
081800981229   4bC                   if        XStato    <     0
081900981229     C                   SetOn                                        6199
082000981229   4eC                   endif
082100981229     C*
082200981229     C                   endif
082300981229     C*
082400981229   2eC                   Endif
082500981229     C*
082600981231     C* controllo ABI/CAB
082700990107     C                   Clear                   D1AbiCabD
082800981231     C                   If        D1Abi <> *Blank and D1Cab <> *Blank
082900981231     C                   Reset                   X59AbiDs
083000981231     C                   eval      X59abi = D1Abi
083100981231     C                   eval      X59cab = D1Cab
083200981231     C                   Movel     XscSoc        X59Soc
083300981231     C                   Call      'X59ABI'
083400981231     C                   Parm                    X59AbiDs
083500981231     C                   If        X59Err = '1'
083600981231     C                   SetOn                                        6299
083700990107     C                   Else
083800990107     C                   MoveL     X59DesIst     D1AbiCabD
083900981231     C                   Endif
084000981231     C                   Endif
084100981231     C*
084200981231     C
084300981231     C*
084400981229     C                   ENDIF
084500981229     C*
084600981229     C                   ENDSR
084700981229     C/EJECT
084800950714     C************************************************************
084900981216     C* RIEMPIMENTO VIDEO D1
085000950714     C************************************************************
085100981216     C     RIED1         BEGSR
085200981216     C*
085300981216     C                   EVAL      d1kcc     = kcc0661
085400981216     C                   EVAL      d1ksc     = ksc0661
085500981216     C                   EVAL      d1desbrev = desbre0661
085600000118C1309C******DMY          MOVE      dtpar0661     d1dtpar
085700000118C1309C     *JobRun       MOVE      dtpar0661     d1dtpar
085800981216     C                   EVAL      d1nrpar   = nrpar0661
085900981216     C                   EVAL      d1serpar  = serpar0661
086000100723C2163C                   eval      D2On = XscOn
086100100723C2163C                   eval      D2Off = XscOff
086200981216     C*
086300961206     C                   if        *in21     = *off
086400981216     C                   EVAL      d1descr   = cpadescr
086500981216     C                   EVAL      d1indiriz = cpaindiriz
086600981216     C                   EVAL      d1cap     = cpacap
086700981216     C                   EVAL      d1localit = cpalocalit
086800981216     C                   EVAL      d1prov    = cpaprov
086900981216     C                   EVAL      d1stato   = cpastato
087000981216     C                   EVAL      d1cdfisc  = cpacdfisc
087100981216     C                   EVAL      d1partiva = cpapartiva
087200981216     C                   EVAL      d1tredich = cpatredich
087300981216     C                   EVAL      d1infsup  = cpainfsup
087400981216     C                   EVAL      d1bolpor  = cpabolpor
087500981216     C                   EVAL      d1keygest = cpakeygest
087600981216     C                   EVAL      d1corfil  = cpacorfil
087700981216     C                   EVAL      d1abi     = cpaabi
087800981216     C                   EVAL      d1cab     = cpacab
087900981216     C                   EVAL      d1ccb     = cpaccb
088000981216     C                   EVAL      d1cin     = cpacin
088100100723C2163C                   EVAL      D2COGN    = CPACOGNOME
088200100723  "  C                   EVAL      D2NOME    = CPANOME
088300100726  "  C*
088400100726  "  C                   if        CPADtNa    <> *loval
088500100726  "  C                   movel     CPADtNa       DataISO
088600100726  "  C                   IF        DataISO = *LOVAL
088700100726  "  C                   EVAL      D2DtNas = *ZEROS
088800100726  "  C                   ELSE
088900100726  "  C                   Select
089000100726  "  C* formato mm/gg/aaaa
089100100726  "  C                   When      $DatFmt = '*MDY'
089200100726  "  C     *USA          Move      DataISO       D2DtNas
089300100726  "  C* formato aa/mm/gg
089400100726  "  C                   When      $DatFmt = '*YMD'
089500100726  "  C     *ISO          Move      DataISO       D2DtNas
089600100726  "  C* formato gg/mm/aaaa
089700100726  "  C                   When      $DatFmt = '*DMY'
089800100726  "  C     *eur          MOVEL     DataISO       D2DtNas
089900100726  "  C                   EndSl
090000100726  "  C                   endif
090100100726  "  C                   endif
090200100726  "  C*
090300100723  "  C                   EVAL      D2SESSO   = CPASESSO
090400100723  "  C                   EVAL      D2CAPNA   = CPACAPNA
090500100723  "  C                   EVAL      D2COMUNA  = CPACOMNA
090600100723  "  C                   EVAL      D2PROVNA  = CPAPRVNA
090700100723  "  C                   EVAL      D2STATNA  = CPASTANA
090800100723  "  C                   If        CPABlackl = *on
090900100723  "  C                   EVAL      D2BLKLIST = XscOn
091000100723  "  C                   else
091100100723  "  C                   EVAL      D2BLKLIST = XscOff
091200100723C2163C                   endif
091300961206     C                   else
091400981216     C                   EVAL      d1descr   = *blank
091500981216     C                   EVAL      d1indiriz = *blank
091600981216     C                   EVAL      d1cap     = *blank
091700981216     C                   EVAL      d1localit = *blank
091800981216     C                   EVAL      d1prov    = *blank
091900981216     C                   EVAL      d1stato   = *blank
092000981216     C                   EVAL      d1cdfisc  = *blank
092100981216     C                   EVAL      d1partiva = *blank
092200981216     C                   EVAL      d1tredich = *blank
092300981216     C                   EVAL      d1infsup  = *blank
092400981216     C                   EVAL      d1bolpor  = *blank
092500981216     C                   EVAL      d1keygest = *blank
092600981216     C                   EVAL      d1corfil  = *blank
092700981216     C                   EVAL      d1abi     = *blank
092800981216     C                   EVAL      d1cab     = *blank
092900981216     C                   EVAL      d1ccb     = *blank
093000981216     C                   EVAL      d1cin     = *blank
093100100723C2163C                   EVAL      D2COGN    = *blank
093200100723  "  C                   EVAL      D2NOME    = *blank
093300100723  "  C                   EVAL      D2DTNAS   = *zeros
093400100723  "  C                   EVAL      D2SESSO   = *blank
093500100723  "  C                   EVAL      D2CAPNA   = *blank
093600100723  "  C                   EVAL      D2COMUNA  = *blank
093700100723  "  C                   EVAL      D2PROVNA  = *blank
093800100723  "  C                   EVAL      D2STATNA  = *blank
093900100723C2163C                   EVAL      D2BLKLIST = xscoff
094000961206     C                   endif
094100981216     C*
094200950714     C                   ENDSR
094300950714     C/EJECT
094400100723C2163C************************************************************
094500100723 "   C* GESTIONE F24 VIDEO D1
094600100723 "   C************************************************************
094700100723C2163C     F24D2         BEGSR
094800100723     C*
094900100723     C                   MOVEA     $FX1          $FX
095000100723     C                   MOVEA     $FC1          $FC
095100100723     C                   MOVEA     $FR1          $FR
095200100723     C                   Z-ADD     $ULKD1        $ULK
095300100723     C                   EXSR      RIEKEY
095400100723     C                   Z-ADD     $ULK          $ULKD1            3 0
095500100723     C                   MOVEA     $FC           $FC1
095600100723     C                   MOVEL     $KEY1         Z1KE1
095700100723     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
095800100723     C                   MOVEL     $KEY2         Z1KE2
095900100723     C                   WRITE     FMTZ1
096000100723     C*
096100100723C2163C                   ENDSR
096200100723C2163C************************************************************
096300100723 "   C* CONTROLLO VIDEATA
096400100723 "   C************************************************************
096500100723C2163C     CTRD2         BEGSR
096600100723     C*
096700100723     C                   SETOFF                                       99
096800100723     C* non imposto pos. del cursore
096900100723     C                   Z-ADD     0             H1RIGA
097000100723     C                   Z-ADD     0             H1COLO
097100100723     C*
097200100723     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
097300100723     C*
097400100726     C     OPZ0661       IFNE      '04'
097500100723     C*
097600100723     C                   If        (D2COGN=*Blanks or D2NOME=*Blanks)
097700100723     C                             and (D2COMUNA<>*Blanks or D2PROVNA<>*Blanks
097800100723     C                             or D2CAPNA<>*Blanks or D2STATNA<>*Blanks
097900100723     C                             or D2SESSO<>*Blanks or D2DTNAS<>*Zero)
098000100723     C                   seton                                        6599
098100100723     C                   endif
098200100723     C*
098300100723     C                   If        (D2COGN<>*Blanks or D2NOME<>*Blanks)
098400100723     C                             and (D2COMUNA=*Blanks or D2PROVNA=*Blanks
098500100726     C                             or D2STATNA=*Blanks
098600100723     C                             or D2SESSO=*Blanks or D2DTNAS=*Zero)
098700100723     C                   seton                                        6599
098800100723     C                   endif
098900100723     C*
099000100723     C* CTRL formale data nascita
099100100723     C     D2DTNAS       IFNE      0
099200100723     C                   Select
099300100723     C* formato mm/gg/aaaa
099400100723     C                   When      $DatFmt = '*MDY'
099500100723     C     *USA          TEST(D)                 D2DTNAS                66
099600100723     C* formato aa/mm/gg
099700100723     C                   When      $DatFmt = '*YMD'
099800100723     C     *ISO          TEST(D)                 D2DTNAS                66
099900100723     C* formato gg/mm/aaaa
100000100723     C                   When      $DatFmt = '*DMY'
100100100723     C     *eur          TEST(D)                 D2DTNAS                66
100200100723     C                   EndSl
100300100723     C* data ok
100400100726     C                   If        *in66 = *on
100500100723     C                   SETON                                        99
100600100726     C                   ENDIF
100700100726     C                   endif
100800100723     C*
100900100723     C* CTRL VALIDITA' FLAG SESSO
101000100723     C                   MOVEL     *BLANK        D2SESSOD
101100100726     C                   If        D2Sesso <> *blanks
101200100726     C                   RESET                   x07valds
101300100723     C                   MOVE      '1'           X07RIC
101400100723     C                   MOVEL     'ANSOG000'    X07TRC
101500100723     C                   MOVEL     'SOGSEX'      X07CAM
101600100723     C                   MOVEL     D2SESSO       X07VAL
101700100723     c                   move      *off          x07all
101800100723     C                   CALLb     'X07VALR'
101900100726     C                   PARM                    x07valds
102000100723     C     X07ERR        IFEQ      '0'
102100100723     C                   MOVEL     X07DES        D2SESSOD
102200100723     C                   ELSE
102300100723     C                   SETON                                        6799
102400100723     C                   ENDIF
102500100726     C                   endif
102600100723     C*
102700100723     C* decodifica Provincia di nascita
102800100726     C                   clear                   D2ProvNaD
102900100723     C                   If        D2PROVNA <> *Blank
103000100723     C                   MOVE      '1'           XTBRIC
103100100723     C                   MOVE      XSCSOC        XTBAZI
103200100723     C                   MOVE      *ZERO         XTBKEY
103300100723     C                   MOVE      'C17'         XTBCOD
103400100723     C                   MOVE      D2PROVNA      WRK06A            6
103500100723     C                   MOVEL     D2STATNA      WRK06A
103600100723     C                   MOVE      WRK06A        XTBKEY
103700100723     C                   CALLb     'XATB'
103800100723     C                   PARM                    XATBDS
103900100723     C     XTBERR        IFEQ      '0'
104000100723     C                   MOVEL     XTBUNI        TABC17
104100100723     C                   MOVEL     DESC17        D2PROVNAD
104200100723     C                   else
104300100723     C                   seton                                        6899
104400100723     C                   EndIf
104500100723     C                   EndIf
104600100723     C*
104700100723     C* decodifica  STATO di nascita
104800100726     C                   clear                   D2CapNaD
104900100726     C                   clear                   D2StatNaD
105000100726  1b C                   If        D2STATNA <> *Blank
105100100723     C                   Reset                   XAtbDs
105200100723     C                   MOVE      '1'           XTBRIC
105300100723     C                   MOVE      XSCSOC        XTBAZI
105400100723     C                   MOVE      *ZERO         XTBKEY
105500100723     C                   MOVE      'G12'         XTBCOD
105600100723     C                   MOVE      D2STATNA      XTBKEY
105700100723     C                   CALLb     'XATB'
105800100723     C                   PARM                    XATBDS
105900100726  2b C     XTBERR        IFEQ      '0'
106000100723     C                   MOVEL     XTBUNI        TABG12
106100100723     C                   MOVEL     DESG12        D2STATNAD
106200100726     C*
106300100824     C* Controllo coerenza C.A.P.
106400100824     C                   movel     D2ComuNa      WLocalita
106500100824     C                   movel     D2CapNa       WCAP
106600100824     C                   movel     D2ProvNa      WProvincia
106700100824     C                   movel     D2StatNa      WStato
106800100824     C*
106900100824     C                   exsr      CtrCap
107000100824     C*
107100100824     C* Se reperita provincia la propone a video
107200100824     C                   If        D2ProvNa = *blanks
107300100824     C                             and WProvincia <> *blanks
107400100824     C                   eval      D2ProvNa = WProvincia
107500100824     C                   endif
107600100824     C* Se reperita località la propone a video
107700100824     C                   If        D2ComuNa = *blanks
107800100824     C                             and WLocalita <> *blanks
107900100824     C                   eval      D2ComuNa = WLocalita
108000100824     C                   endif
108100100824     C*
108200100726   2xC                   else
108300100723     C                   seton                                        6999
108400100726   2eC                   EndIf
108500100726   1eC                   EndIf
108600100723     C*
108700100723     C                   If        D2BLKLIST  <> xscon
108800100723     C                             and D2BLKLIST <> xscoff
108900100723     C                   seton                                        6499
109000100723     C                   endif
109100100723     C*
109200100723     C                   ENDIF
109300100723     C*
109400100723C2163C                   ENDSR
109500100824C2163C************************************************************
109600100824  "  C* Controll CAP
109700100824  "  C************************************************************
109800100824C2163C     CtrCAP        BegSR
109900100824     C*
110000100824     C                   eval      WErrore = *off
110100100824     C*
110200100824     C                   CALL      'PJXX051R'
110300100824     C                   PARM                    WProvincia
110400100824     C                   PARM                    WStato
110500100824     C                   PARM                    WCAP
110600100824     C                   PARM                    WLocalita
110700100824     C                   PARM                    WErrore
110800100824     C                   PARM                    TABG12
110900100824     C*
111000100824     C                   select
111100100824     C* 1 = Località errata o C.A.P. non congruente con la località
111200100824     C                   when      WErrore = '1'
111300100824     C                   seton                                        7499
111400100824     C* 2 = Campo  obbligatorio
111500100824     C                   when      WErrore = '2'
111600100824     C                   seton                                        7599
111700100824     C* 3 = C.A.P. inesistente
111800100824     C                   when      WErrore = '3'
111900100824     C                   seton                                        7699
112000100824     C* 4 = C.A.P. ha un numero di caratteri diversi
112100100824     C                   when      WErrore = '4'
112200100824     C                   seton                                        7799
112300100824     C                   endsl
112400100824     C*
112500100824C2163C                   EndSR
112600100723C2163C************************************************************
112700100723 "   C* GESTIONE VIDEO RECORD D2
112800100723 "   C************************************************************
112900100723C2163C     GESD2         BEGSR
113000100723     C*
113100100723     C* se esistono errori sulla videata
113200100723     C* emetto la write del formato a indicatori spenti per vedere
113300100723     C* le eventuali decodifiche
113400100723     C     *IN99         IFEQ      *ON
113500100723     C     $GEST         ANDEQ     'D2'
113600100723     C                   MOVEA     *IN           WIN
113700100723     C                   MOVE      *ALL'0'       IN5098           49
113800100723     C                   MOVEA     IN5098        *IN(50)
113900100723     C                   WRITE     FMTD2
114000100723     C                   MOVEA     WIN           *IN
114100100723   1EC                   ENDIF
114200100723     C*
114300100723     C                   EXFMT     FMTD2
114400100723     C* controllo abilitazione tasti funzione
114500100723     C*
114600100723     C                   EXSR      CTRKD1
114700100723     C*
114800100723     C     *IN99         IFEQ      *OFF
114900100723     C*
115000100723     C                   SELECT
115100100723     C* ROLLDWN
115200100723     C     $TASTO        WHEnEQ    ROLDWN
115300100723     C                   MOVE      'D1'          $GEST
115400100723     C* F4=Ricerca
115500100723     C     $TASTO        WHENEQ    F04
115600100723     C                   EXSR      SEARCH
115700100723     C* F12=Ritorno
115800100723     C     $TASTO        WHENEQ    F12
115900100723     C                   EXSR      F12D1
116000100723     C*
116100100723   2XC                   OTHER
116200100723     C                   EXSR      CTRD2
116300100723     C* aggiungere le chiamate ai controlli delle videate supplementari
116400100723     C     *IN99         IFEQ      *OFF
116500100723     C                   EXSR      CTRD1
116600100723     C     *IN99         IFEQ      *ON
116700100723     C                   SETON                                        96
116800100723     C                   ENDIF
116900100723     C                   ENDIF
117000100723     C*
117100100723     C     $TASTO        IFEQ      F16
117200100723     C     *IN99         ANDEQ     *OFF
117300100723     C* eseguo annullamento
117400100723     C                   Move      '04'          OPZ0661
117500100723     C                   EXSR      AGGANA
117600100723     C                   ENDIF
117700100723     C*
117800100723   B3C     $TASTO        IFEQ      F06
117900100723     C     *IN99         ANDEQ     *OFF
118000100723     C* eseguo aggiornamento
118100100723     C                   EXSR      AGGANA
118200100723   E3C                   ENDIF
118300100723     C*
118400100723     C                   ENDSL
118500100723     C*
118600100723     C                   ENDIF
118700100723     C*
118800100723C2163C                   ENDSR
118900940128     C************************************************************
119000981216     C* CONTROLLI INIZIALI
119100940128     C************************************************************
119200981216     C     CtrInizio     BEGSR
119300940128     C*
119400940201     C                   MOVE      *OFF          *IN99
119500961210     C                   MOVE      *OFF          *IN50
119600950713     C*
119700950713     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
119800961210     C     *in99         ifeq      *off                                          1a
119900950713     C*
120000981216    >C     OPZ0661       IFEQ      '01'                                          2a
120100981216    >C     OPZ0661       orEQ      '02'                                          2a
120200950713     C*
120300950713     C* imposto i campi chiave e verifico l'esistenza del record
120400950713     C*
120500981216    >C                   MOVEL     SYS0661       cpaSYS
120600981216    >C                   MOVEL     NRASIN0661    cpaNRASINT
120700981216    >C     k02cpa01      chain     NDCPA01L                           21
120800961210     C     *IN21         IFEQ      '0'                                           3a
120900950713     C                   SETON                                        8099
121000970414     C                   else                                                    3b
121100981216     C                   eval      opz0661='01'                                  3b
121200970414     C                   ENDIF                                                   3b
121300950713     C*
121400961210     C                   ENDIF                                                   2b
121500961206     C*
121600981216    >C     OPZ0661       IFEQ      '02'                                          3a
121700981216    >C     OPZ0661       oreq      '05'
121800981216    >C     OPZ0661       oreq      '04'
121900961206     C*
122000961206     C* imposto i campi chiave e verifico l'esistenza del record
122100961206     C*
122200981216    >C                   MOVEL     SYS0661       cpaSYS
122300981216    >C                   MOVEL     NRASIN0661    cpaNRASINT
122400981216    >C     k02cpa01      chain     NDCPA01L                           21
122500961210     C     *IN21         IFEQ      '1'                                           2a
122600961206     C                   SETON                                        8099
122700961210     C                   ENDIF                                                   2b
122800961206     C*
122900981216    >C     OPZ0661       IFEQ      '05'                                          2a
123000981216    >C     OPZ0661       oreq      '04'
123100961206     C                   eval      ragsoc1a = prHics
123200961206     C                   eval      indiri1a = prHics
123300961206     C                   eval      cap1a    = prHics
123400961206     C                   eval      locali1a = prHics
123500961206     C                   eval      prov1a   = prHics
123600990104     C                   eval      stato1a  = prHics
123700961206     C                   eval      cdfisc1a = prHics
123800961206     C                   eval      partiva1a= prHics
123900961206     C                   eval      tred1a   = prHics
124000961206     C                   eval      infsup1a = prHics
124100961206     C                   eval      bolpor1a = prHics
124200961206     C                   eval      keygest1a= prHics
124300961206     C                   eval      corfil1a = prHics
124400981216     C                   eval      abi1a    = prHics
124500981216     C                   eval      cab1a    = prHics
124600981216     C                   eval      ccb1a    = prHics
124700981216     C                   eval      cin1a    = prHics
124800100723C2163C*
124900100723  "  C                   eval      D2Cogn1A    = prHics
125000100723  "  C                   eval      D2Nome1A    = prHics
125100100723  "  C                   eval      D2Sesso1A   = prHics
125200100723  "  C                   eval      D2DtNas1A   = prHics
125300100723  "  C                   eval      D2CapNa1A   = prHics
125400100723  "  C                   eval      D2ComuNa1A  = prHics
125500100723  "  C                   eval      D2ProvNa1A  = prHics
125600100723  "  C                   eval      D2StatNa1A  = prHics
125700100723C2163C                   eval      D2BlkLis1A  = prHics
125800100723     C                   ENDIF                                                   2b
125900100723     C                   ENDIF                                                   3b
126000100723     C*
126100961210     C                   ENDIF                                                   1b
126200940202     C*
126300940202     C                   ENDSR
126400950713     C/EJECT
126500950713     C************************************************************
126600950713     C* AGGIORNAMENTO ANAGRAFICA
126700950713     C************************************************************
126800950713     C     AGGANA        BEGSR
126900950713     C*
127000981216     C                   EXSR      RIEANA
127100981216     C     OPZ0661       ifeq      '02'
127200981216     C                   update    NDCPA000
127201170223R224 X*ex R027           EXSR      CallDatiAgg
127300961206     C                   ENDIF
127400981216     C     OPZ0661       ifeq      '01'
127500981216     C                   write     NDCPA000
127501170223R224 X*ex R027           EXSR      CallDatiAgg
127600961206     C                   ENDIF
127700981216     C     OPZ0661       ifeq      '04'
127800981216     C                   delete    NDCPA000
127801170223R027 C*
127802170223  "  C                   Eval      SO3Sogg    = *Blank
127803170223  "  C                   Eval      SO3DtFiVl  = *Loval
127804170223  "  C                   Eval      SO3Sys     = CPASys
127805170223  "  C                   Eval      SO3NrAsInt = CPANrAsInt
127806170223  "  C     K04SO301      SETLL     SDGSO301L
127807170223  "  C     K04SO301      READE     SDGSO301L                              21
127808170223R224 X*ex R027           if        *in21 = *Off
127809170223R224 C                   dow       *in21 = *Off
127810170223R027 C                   delete    SDGSO3000
127811170223R224 X*ex R027           endif
127812170223 "   C     K04SO301      READE     SDGSO301L                              21
127813170223R224 C                   enddo
127814170223R027 C*
127815170223R027 C*************      EXSR      CallDatiAgg
127900961206     C                   ENDIF
128000950928     C*
128100950928     C* eseguo operazioni del dopo-aggiornamento
128200950928     C                   EXSR      GESAGG
128300950713     C*
128400950713     C                   ENDSR
128500950713     C/EJECT
128501170223R224 X*ex R027 ***************************************************
128502170223 "   X*ex R027 Richiamo pgm per dati anagrafici aggiuntivi
128503170223 "   X*ex R027 ***************************************************
128504170223 "   X*ex R027 CallDatiAgg   BEGSR
128505170223 "   X*ex R027
128506170223 "   X*ex R027           RESET                   SDGSD16DS
128507170223 "   X*ex R027
128508170223 "   X*ex R027           MOVEL     OPZ0661       OPZD16
128509170223 "   X*ex R027 CMT061        IFEQ      '1'
128510170223 "   X*ex R027           MOVEL     '1'           CMTD16
128511170223 "   X*ex R027           ELSE
128512170223 "   X*ex R027           MOVEL     '0'           CMTD16
128513170223 "   X*ex R027           ENDIF
128514170223 "   X*ex R027
128515170223 "   X*ex R027           Eval      SoggD16     = *Blank
128516170223 "   X*ex R027           Eval      DtFiVlD16   = *Loval
128517170223 "   X*ex R027           Eval      SysD16      = Sys0661
128518170223 "   X*ex R027           Eval      NrAsIntD16  = NrAsIn0661
128519170223 "   X*ex R027
128520170223 "   X*ex R027           MOVEL     SDGSD16DS     KPJBU
128521170223 "   X*ex R027
128522170223 "   X*ex R027           CALL      'SDGSD16R'
128523170223 "   X*ex R027           PARM                    KPJBA
128524170223 "   X*ex R027
128525170223 "   X*ex R027           MOVEL     KPJBU         SDGSD16DS
128526170223 "   X*ex R027
128527170223 "   X*ex R027           CALL      'SDGSD16R'
128528170223 "   X*ex R027           PARM                    KPJBA
128529170223 "   X*ex R027
128530170223 "   X*ex R027           MOVEL     KPJBU         SDGSD16DS
128531170223 "   X*ex R027
128532170223 "   X*ex R027 ritorno da PGM gestione
128533170223 "   X*ex R027           EXSR      RTND16
128534170223 "   X*ex R027
128535170223 "   X*ex R027           ENDSR
128536170223 "   X*ex R027 /EJECT
128537170223 "   X*ex R027 ***************************************************
128538170223 "   X*ex R027 GESTIONE RITORNO DA PGM DI GESTIONE
128539170223 "   X*ex R027 ***************************************************
128540170223 "   X*ex R027 RTND16        BEGSR
128541170223 "   X*ex R027
128542170223 "   X*ex R027 modo di ritorno
128543170223 "   X*ex R027
128544170223 "   X*ex R027           SELECT
128545170223 "   X*ex R027 questi modi di utilizzo dei valori di ritorno dal
128546170223 "   X*ex R027 pgm di manutenzione rcd di anagrafica sono delle
128547170223 "   X*ex R027 proposte, normalmente sempre valide, ma modificabili
128548170223 "   X*ex R027 per situazioni particolari
128549170223 "   X*ex R027 1 = F3
128550170223 "   X*ex R027 RETD16        WHENEQ    '1'
128551170223 "   X*ex R027           MOVE      *ON           $FINE                          fine pgm
128552170223 "   X*ex R027           MOVE      '1'           RET0661
128553170223 "   X*ex R027 2 = F12
128554170223 "   X*ex R027 RETD16        WHENEQ    '2'
128555170223 "   X*ex R027 se il pgm di manutenzione ha prima una guida, è necessario anche
128556170223 "   X*ex R027 questo test, altrimenti prevale il retD04 e il sfl non viene
128557170223 "   X*ex R027 ricaricato
128558170223 "   X*ex R027 OPRD16        ANDNE     '1'
128559170223 "   X*ex R027
128560170223 "   X*ex R027           ENDSL
128561170223 "   X*ex R027
128562170223 "   X*ex R027 funzione non eseguibile per errore :
128563170223 "   X*ex R027
128564170223 "   X*ex R027           SELECT
128565170223 "   X*ex R027 1 = funzione richiamata chiusa in errore
128566170223 "   X*ex R027 ERRD16        WHENEQ    '1'
128567170223 "   X*ex R027           SETON                                        9399      emiss.SFLMSGID
128568170223 "   X*ex R027 2 = funzione richiamata senza la dovuta autorizzazione
128569170223 "   X*ex R027 ERRD16        WHENEQ    '2'
128570170223 "   X*ex R027           SETON                                        9499      emiss.SFLMSGID
128571170223 "   X*ex R027
128572170223 "   X*ex R027           ENDSL
128573170223 "   X*ex R027
128574170223 "   X*ex R027           ENDSR
128575170223R224 X*ex R027 EJECT
128600950713     C************************************************************
128700981216     C* Riempimento campi file
128800950713     C************************************************************
128900981216     C     RIEANA        BEGSR
129000950713     C*
129100981216     C                   EVAL      cpadescr  = d1descr
129200981216     C                   EVAL      cpaindiriz= d1indiriz
129300981216     C                   EVAL      cpacap    = d1cap
129400981216     C                   EVAL      cpalocalit= d1localit
129500981216     C                   EVAL      cpaprov   = d1prov
129600981216     C                   EVAL      cpastato  = d1stato
129700981216     C                   EVAL      cpacdfisc = d1cdfisc
129800981216     C                   EVAL      cpapartiva= d1partiva
129900981216     C                   EVAL      cpatredich= d1tredich
130000981216     C                   EVAL      cpainfsup = d1infsup
130100981216     C                   EVAL      cpabolpor = d1bolpor
130200981216     C                   EVAL      cpakeygest= d1keygest
130300981216     C                   EVAL      cpacorfil = d1corfil
130400981216     C                   EVAL      cpaabi    = d1abi
130500981216     C                   EVAL      cpacab    = d1cab
130600981216     C                   EVAL      cpaccb    = d1ccb
130700981216     C                   EVAL      cpacin    = d1cin
130800100723C2163C*
130900100723  "  C                   eval      CPACOGNOME = D2COGN
131000100723  "  C                   eval      CPANOME    = D2NOME
131100100726  "  C*
131200100723  "  C                   If        D2DTNAS <> *zeros
131300100723  "  C                   Select
131400100723  "  C* formato mm/gg/aaaa
131500100723  "  C                   When      $DatFmt = '*MDY'
131600100723  "  C     *USA          Move      D2DTNAS       DataISO
131700100723  "  C* formato aa/mm/gg
131800100723  "  C                   When      $DatFmt = '*YMD'
131900100723  "  C     *ISO          Move      D2DTNAS       DataISO
132000100723  "  C* formato gg/mm/aaaa
132100100723  "  C                   When      $DatFmt = '*DMY'
132200100723  "  C     *eur          MOVEL     D2DTNAS       DataISO
132300100723  "  C                   EndSl
132400100723  "  C                   MOVEL     DataISO       CPADTNA
132500100726  "  C*
132600100723  "  C                   ELSE
132700100726  "  C*
132800100726  "  C                   eval      CPADTNA = *loval
132900100726  "  C*
133000100723  "  C                   ENDIF
133100100723  "  C*
133200100723  "  C                   eval      CPASESSO   = D2SESSO
133300100723  "  C                   eval      CPACAPNA   = D2CAPNA
133400100723  "  C                   eval      CPACOMNA   = D2COMUNA
133500100723  "  C                   eval      CPAPRVNA   = D2PROVNA
133600100723  "  C                   eval      CPASTANA   = D2STATNA
133700100723  "  C                   if        D2Blklist = xscon
133800100723  "  C                   MOVEL     *on           CPABLACKL
133900100723  "  C                   else
134000100723  "  C                   MOVEL     *off          CPABLACKL
134100100723  "  C                   endif
134200100723C2163C*
134300100723     C*
134400950713     C                   ENDSR
134500950713     C/EJECT
134600950713     C************************************************************
134700950713     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
134800950713     C************************************************************
134900950713     C     GESAGG        BEGSR
135000950713     C*
135100950713     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
135200981216    >C     CMT0661       IFEQ      '0'
135300950713     C                   COMMIT
135400950713     C                   ENDIF
135500950713     C*
135600981216     C                   UNLOCK    NDCPA01L
135700950713     C*
135800950713     C* segnalo al pgm chiamante l'avvenuta conferma
135900981216    >C                   MOVE      *ON           OPR0661
136000950713     C* segnalo al pgm chiamante l'aver premuto l'F6
136100981216    >C                   MOVE      '0'           RET0661
136200950713     C*
136300950713     C                   MOVE      *ON           $FINE
136400950713     C*
136500950713     C                   ENDSR
136600950713     C/EJECT
136700950713     C************************************************************
136800981216     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
136900950713     C* INPUT
137000981216     C* $FC1 = Abilitazioni videata D1
137100950713     C* OUTPUT
137200950713     C* *IN97 *IN99 Errori
137300950713     C************************************************************
137400981216     C     CTRKD1        BEGSR
137500950713     C*
137600950713     C                   SETOFF                                       99
137700950713     C                   MOVEA     $FC1          $FC
137800950713     C                   EXSR      CTRKEY
137900950713     C     *IN99         IFEQ      *ON
138000950713     C                   SETON                                        97
138100950713     C                   ENDIF
138200950713     C*
138300950713     C                   ENDSR
138400950713     C/EJECT
138500950713     C************************************************************
138600981216     C* GESTIONE F12 VIDEO D1
138700950713     C************************************************************
138800981216     C     F12D1         BEGSR
138900950713     C*
139000950713     C                   MOVE      *ON           $FINE
139100981216    >C                   MOVE      '2'           RET0661
139200981216    >C                   MOVE      *OFF          OPR0661
139300950713     C*
139400950713     C                   ENDSR
139500950713     C/EJECT
139600950706     C************************************************************
139700950706     C* CONTROLLO ABILITAZIONE TASTI
139800950706     C* INPUT
139900950706     C* $TASTO =Tasto funzione premuto
140000950706     C* $FC = Tasti abilitati
140100950706     C* OUTPUT
140200950706     C* IN99 = ERrore
140300950706     C************************************************************
140400950706     C     CTRKEY        BEGSR
140500950706     C*
140600941205     C                   SELECT
140700941205     C     $TASTO        WHENEQ    F01
140800941205     C     $FC(1)        IFEQ      '0'
140900941205     C                   SETON                                        99
141000941205     C                   ENDIF
141100941205     C     $TASTO        WHENEQ    F02
141200941205     C     $FC(2)        IFEQ      '0'
141300941205     C                   SETON                                        99
141400941205     C                   ENDIF
141500941205     C     $TASTO        WHENEQ    F03
141600941205     C     $FC(3)        IFEQ      '0'
141700941205     C                   SETON                                        99
141800941205     C                   ENDIF
141900941205     C     $TASTO        WHENEQ    F04
142000941205     C     $FC(4)        IFEQ      '0'
142100941205     C                   SETON                                        99
142200941205     C                   ENDIF
142300941205     C     $TASTO        WHENEQ    F05
142400941205     C     $FC(5)        IFEQ      '0'
142500941205     C                   SETON                                        99
142600941205     C                   ENDIF
142700941205     C     $TASTO        WHENEQ    F06
142800941205     C     $FC(6)        IFEQ      '0'
142900941205     C                   SETON                                        99
143000941205     C                   ENDIF
143100941205     C     $TASTO        WHENEQ    F07
143200941205     C     $FC(7)        IFEQ      '0'
143300941205     C                   SETON                                        99
143400941205     C                   ENDIF
143500941205     C     $TASTO        WHENEQ    F08
143600941205     C     $FC(8)        IFEQ      '0'
143700941205     C                   SETON                                        99
143800941205     C                   ENDIF
143900941205     C     $TASTO        WHENEQ    F09
144000941205     C     $FC(09)       IFEQ      '0'
144100941205     C                   SETON                                        99
144200941205     C                   ENDIF
144300941205     C     $TASTO        WHENEQ    F10
144400941205     C     $FC(10)       IFEQ      '0'
144500941205     C                   SETON                                        99
144600941205     C                   ENDIF
144700941205     C     $TASTO        WHENEQ    F11
144800941205     C     $FC(11)       IFEQ      '0'
144900941205     C                   SETON                                        99
145000941205     C                   ENDIF
145100941205     C     $TASTO        WHENEQ    F12
145200941205     C     $FC(12)       IFEQ      '0'
145300941205     C                   SETON                                        99
145400941205     C                   ENDIF
145500941205     C     $TASTO        WHENEQ    F13
145600941205     C     $FC(13)       IFEQ      '0'
145700941205     C                   SETON                                        99
145800941205     C                   ENDIF
145900941205     C     $TASTO        WHENEQ    F14
146000941205     C     $FC(14)       IFEQ      '0'
146100941205     C                   SETON                                        99
146200941205     C                   ENDIF
146300941205     C     $TASTO        WHENEQ    F15
146400941205     C     $FC(15)       IFEQ      '0'
146500941205     C                   SETON                                        99
146600941205     C                   ENDIF
146700941205     C     $TASTO        WHENEQ    F16
146800941205     C     $FC(16)       IFEQ      '0'
146900941205     C                   SETON                                        99
147000941205     C                   ENDIF
147100941205     C     $TASTO        WHENEQ    F17
147200941205     C     $FC(17)       IFEQ      '0'
147300941205     C                   SETON                                        99
147400941205     C                   ENDIF
147500941205     C     $TASTO        WHENEQ    F18
147600941205     C     $FC(18)       IFEQ      '0'
147700941205     C                   SETON                                        99
147800941205     C                   ENDIF
147900941205     C     $TASTO        WHENEQ    F19
148000941205     C     $FC(19)       IFEQ      '0'
148100941205     C                   SETON                                        99
148200941205     C                   ENDIF
148300941205     C     $TASTO        WHENEQ    F20
148400941205     C     $FC(20)       IFEQ      '0'
148500941205     C                   SETON                                        99
148600941205     C                   ENDIF
148700941205     C     $TASTO        WHENEQ    F21
148800941205     C     $FC(21)       IFEQ      '0'
148900941205     C                   SETON                                        99
149000941205     C                   ENDIF
149100941205     C     $TASTO        WHENEQ    F22
149200941205     C     $FC(22)       IFEQ      '0'
149300941205     C                   SETON                                        99
149400941205     C                   ENDIF
149500941205     C     $TASTO        WHENEQ    F23
149600941205     C     $FC(23)       IFEQ      '0'
149700941205     C                   SETON                                        99
149800941205     C                   ENDIF
149900941205     C     $TASTO        WHENEQ    F24
150000941205     C     $FC(24)       IFEQ      '0'
150100941205     C                   SETON                                        99
150200941205     C                   ENDIF
150300941205     C                   ENDSL
150400941205     C*
150500941205     C                   ENDSR
150600940223     C/EJECT
150700940131     C************************************************************
150800940131     C* OPERAZIONI INIZIALI
150900940131     C************************************************************
151000940131     C     *INZSR        BEGSR
151100000118C1309C     *DMY          Move      Udate         ZDateISO
151200000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
151300100723C2163C*
151400100723  "  C* reperisco DATFMT del job
151500100723  "  C                   CallB     'XDATFMT'
151600100723C2163C                   Parm                    $DatFmt           4
151700940131     C*
151800940127     C* Reperimento parametri
151900940127     C*
152000940117     C     *ENTRY        PLIST
152100940117     C                   PARM                    KPJBA
152200950712     C                   PARM                    DSAUT
152300950712     C*
152400950712     C     PARMS         IFLT      2
152500950712     C                   MOVE      *BLANK        AUTDS
152600950712     C                   ELSE
152700950712     C                   MOVEL     DSAUT         AUTDS
152800950712     C                   ENDIF
152900950406     C*
153000940506     C* Reperimento tasti di funzione
153100940506     C*
153200940506     C                   EXSR      REPKEY
153300950928     C*
153400950928     C                   EVAL      $open = *off
153500940127     C*
153600940117     C                   ENDSR
153700940223     C/EJECT
153800940223     C************************************************************
153900940223     C* INIZIALIZZAZIONE VARIABILI
154000940223     C************************************************************
154100981216     C     INZVAR        BEGSR
154200951124     C*
154300951124     C* Reperimento dati società
154400951124     C*
154500951124    >C                   MOVEL     'SOC001'      TIPXSC
154600951124     C                   MOVEL     *BLANK        SOCXSC
154700951124     C                   EXSR      REPSOC
154800951124     C     RTNXSC        IFNE      '1'
154900951124    >C                   MOVEL     XSOCDS        SOC001
155000951124     C                   ELSE
155100951124     C                   MOVE      *ON           $FINE
155200951124     C                   EXSR      ENDPGM
155300951124     C                   ENDIF
155400950711     C*
155500941215     C* Dati da PGM chiamante
155600941215     C*
155700981216    >C                   MOVEL     KPJBU         NDC066DS1
155800981221     C*
155900981221     C* Se non ricevuto il nr. assoluto lo reperisco
156000981221     C                   If        NrAsIn0661 = 0
156100981221     C                   ExSr      FindNrAsI
156200981221     C                   Endif
156300950928     C*
156400950928     C* Attiva Commitment control se necessario
156500950928     C*
156600981216    >C     CMT0661       IFEQ      '0'
156700950928     C                   eval      flgcom = *on
156800950928    >C                   CALLB     'XSTRCMT'
156900950928     C                   ENDIF
157000950928     C     $open         ifeq      *off
157100981216    >C                   OPEN      NDCPA01L
157200950928     C                   eval      $open  = *on
157300950928     C                   ENDIF
157400950712     C*
157500950712     C* Reperimento autorizzazioni
157600950712     C*
157700950712    >C     AUTDS         IFEQ      *BLANK
157800950712    >C                   EXSR      REPAUT
157900950712    >C                   ELSE
158000950712    >C                   MOVEL     XCADDS        CONPPADS
158100950712    >C                   ENDIF
158200950712     C*
158300950712     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
158400950712     C* le autorizzazione
158500950712     C*
158600950712    >C                   EXSR      TSTAUT
158700950711     C*
158800961206     C* Variabili per gestione videate
158900961206     C*
159000961206     C                   MOVE      *OFF          $FINE
159100981216     C                   MOVE      *ON           $INZD1
159200961206     C                   MOVE      *OFF          $ESCI
159300961206     C                   MOVE      *HIVAL        $LASTV
159400981216     C                   MOVE      'D1'          $GEST
159500961206     C                   MOVE      *OFF          $Endfile
159600981216     C*
159700950711     C* Definizione delle opzioni utilizzabili a pgm
159800950711     C*
159900950711     C                   EXSR      DEFOPZ
160000950712     C*
160100950712     C* Valorizzazione campi univoci testate
160200950712     C*
160300950712     C                   CLEAR                   FMTT1
160400950712     C                   MOVEL     KNSIF         NOMSIF
160500950712     C                   MOVEL     XSCDSI        NOMDIT
160600950712     C                   MOVEL     DSPGM         NOMPGM
160700950712     C* recupero il sottotitolo (se c'è)
160800950712     C                   MOVEL     *BLANK        T1TIT
160900950712     C     SOTTIT        IFNE      *BLANK
161000950712     C                   MOVEL     SOTTIT        IDMSG
161100950712     C                   CALLB     'XRTVM'
161200950712     C                   PARM                    IDMSG            27
161300950712     C                   PARM                    TXTMSG          100
161400950712     C                   PARM                    ERRMSG            1
161500950712     C                   PARM      *ON           CTRMSG            1
161600950712     C                   PARM      30            LENMSG            3 0
161700950712     C     ERRMSG        IFNE      *ON
161800950712     C                   MOVEL     TXTMSG        T1TIT
161900950712     C                   ELSE
162000950712     C                   MOVEL     *ALL'?'       T1TIT
162100950712     C                   ENDIF
162200950712     C                   ENDIF
162300950712     C* recupero il tipo operazione
162400950712     C                   Z-ADD     1             X
162500981216    >C     OPZ0661       LOOKUP    $OZ(X)                                 21
162600950712     C     *IN21         IFEQ      *ON
162700950712     C                   MOVEL     $ID(X)        IDMSG
162800950712     C                   CALLB     'XRTVM'
162900950712     C                   PARM                    IDMSG            27
163000950712     C                   PARM                    TXTMSG          100
163100950712     C                   PARM                    ERRMSG            1
163200950712     C                   PARM      *ON           CTRMSG            1
163300950712     C                   PARM      30            LENMSG            3 0
163400950712     C     ERRMSG        IFNE      *ON
163500950712     C                   MOVEL     TXTMSG        T1OPE
163600950712     C                   ELSE
163700950712     C                   MOVEL     *ALL'?'       T1OPE
163800950712     C                   ENDIF
163900950712     C                   ENDIF
164000950712     C*
164100950712     C                   WRITE     FMTT1
164200950712     C*
164300940223     C                   ENDSR
164400981221     C/EJECT
164500981216     C*----------------------------------------------------------
164600981216     C* Reperimento nr. assoluto interno
164700981216     C*----------------------------------------------------------
164800981221     C     FindNrAsI     BEGSR
164900981216     C*
165000981216     C                   Eval      PASSys = Sys0661
165100981216     C                   Eval      PASNrAsReg = NrAsGe0661
165200981216     C     k02pas01      Chain     NDPAS01L                           21
165300981216     C                   If        *In21 = *Off
165400981216     C* OK reperimento partita
165500981216     C     k07ppa01      Chain     NDPPA01L                           21
165600981216     C                   If        *In21 = *Off
165700981216     C* OK reperimento anagrafico partita
165800981216     C                   Eval      NrAsIn0661 = PPANrAsInt
165900981216     C                   Else
166000981216     C* Errore reperimento anagrafico partita
166100981216     C                   Move      '1'           Err0661
166200981216     C                   Move      *On           $Fine
166300981216     C                   ExSr      EndPgm
166400981216     C                   EndIf
166500981216     C                   Else
166600981216     C* Errore reperimento partita
166700981216     C                   Move      '1'           Err0661
166800981216     C                   Move      *On           $Fine
166900981216     C                   ExSr      EndPgm
167000981216     C                   EndIf
167100981216     C*
167200981221     C                   ENDSR
167300950712     C/EJECT
167400950706     C************************************************************
167500950706     C* REPERIMENTO DATI SOCIETA'
167600950706     C************************************************************
167700950706     C     REPSOC        BEGSR
167800950706     C*
167900950522     C                   CALLB     'XSOC'
168000941209     C                   PARM                    TIPXSC            6
168100941209     C                   PARM                    SOCXSC            3
168200941209     C                   PARM                    CDSXSC            9 0
168300941209     C                   PARM                    MODXSC            3
168400941209     C                   PARM      *BLANKS       RTNXSC            1
168500941209     C                   PARM                    XSOCDS
168600941209     C                   PARM                    KPJBA
168700941209     C*
168800941209     C                   ENDSR
168900950712     C/EJECT
169000950712     C************************************************************
169100950712     C* REPERIMENTO AUTORIZZAZIONI UTENTE
169200950712     C************************************************************
169300950712     C     REPAUT        BEGSR
169400950712     C*
169500950712     C                   CLEAR                   AUTDS
169600950712     C*
169700950712     C                   MOVEL     XSCSOC        XCASOC
169800950712     C                   MOVEL     XSCDUN        XCAUNI
169900950712     C                   Z-ADD     0             XCAGRP
170000950712     C                   MOVEL     KNMUS         XCAPRF
170100950712    >C                   MOVEL     'CONPPA'      XCAFNC
170200950712    >C                   MOVEL     *ZERO         XCAVFU
170300950712    >C                   MOVE      'CK'          XCATCK
170400950712    >C                   MOVE      '1'           XCANAU
170500950712     C*
170600950712     C                   CALLB     'XCHKAUT'
170700950712     C                   PARM                    AUTDS
170800950712     C*
170900950712     C* errori/non abilitazione
171000950712     C     XCARTN        IFNE      0
171100950712     C                   EXSR      ENDPGM
171200950712     C                   ELSE
171300950712    >C                   MOVEL     XCADDS        CONPPADS
171400950712     C                   ENDIF
171500950712     C*
171600950712     C                   ENDSR
171700950712     C/EJECT
171800950712     C************************************************************
171900950712     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
172000950712     C************************************************************
172100950712     C     TSTAUT        BEGSR
172200950712     C*
172300950712     C                   SELECT
172400950712     C*
172500950712     C* se richiamato in immissione
172600981216    >C     OPZ0661       WHENEQ    '01'
172700950712     C* ma l'utente non è abilitato
172800950712    >C     IMMPRmx       ANDEQ     *OFF
172900950712     C* esco da pgm avvertendolo
173000981216    >C                   MOVE      '2'           ERR0661
173100950712     C                   MOVE      *ON           $FINE
173200950712     C                   EXSR      ENDPGM
173300950712     C*
173400950712     C* se richiamato in modifica
173500981216    >C     OPZ0661       WHENEQ    '02'
173600950712     C* ma l'utente non è abilitato
173700950712    >C     MANprmx       ANDEQ     *OFF
173800950712     C* testo se l'utente è abilitato almeno alla visualizzazione
173900950712    >C     VISprmx       IFEQ      *ON
174000981216    >C                   MOVE      '05'          OPZ0661
174100950712     C                   ELSE
174200950712     C* altrimenti esco da pgm avvertendolo
174300981216    >C                   MOVE      '2'           ERR0661
174400950712     C                   MOVE      *ON           $FINE
174500950712     C                   EXSR      ENDPGM
174600950712     C                   ENDIF
174700950712     C*
174800950712     C* se richiamato in annullamento
174900981216    >C     OPZ0661       WHENEQ    '04'
175000950712     C* ma l'utente non è abilitato
175100961206    >C     ANNPRmx       ANDEQ     *OFF
175200950712     C* esco da pgm avvertendolo
175300981216    >C                   MOVE      '2'           ERR0661
175400961206     C                   MOVE      *ON           $FINE
175500961206     C                   EXSR      ENDPGM
175600950712     C*
175700950712     C* se richiamato in visualizzazione
175800981216    >C     OPZ0661       WHENEQ    '05'
175900950712     C* ma l'utente non è abilitato
176000950712    >C     VISprmx       ANDEQ     *OFF
176100950712     C* esco da pgm avvertendolo
176200981216    >C                   MOVE      '2'           ERR0661
176300950712     C                   MOVE      *ON           $FINE
176400950712     C                   EXSR      ENDPGM
176500950712     C*
176600950712     C                   ENDSL
176700950712     C*
176800950712     C                   ENDSR
176900941124     C/EJECT
177000940131     C************************************************************
177100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
177200940131     C************************************************************
177300940225     C     DEFOPZ        BEGSR
177400981216     C*
177500981216     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
177600981216     C* con 1 o 0 a seconda della utilizzabilità o meno
177700981216     C* del tasto funzione correlato
177800981216     C*
177900981216     C* Inizializzo a zero tutta la schiera
178000981216     C                   move      '0'           $FC1
178100981216     C                   move      'N'           $FR1
178200981216     C*
178300981216     C* Abilito i tasti video 'D1'
178400990107     C*
178500990107     C* F2=Decodifica
178600990107     C                   eval      $fc1(2)=*on
178700990107     C* F4=Ricerca
178800990107     C                   eval      $fc1(4)=*on
178900990107     C* F6=Conferma
179000981216     C                   eval      $fc1(6)=*on
179100990107     C* F12=Ritorno
179200981216     C                   eval      $fc1(12)=*on
179300990107     C*
179400990107     C* Disabilito i tasti video 'D1' in base all'opzione
179500990107     C*
179600981216     C                   if        opz0661 = '05'
179700990107     C                   eval      $fc1(2)=*off
179800990107     C                   eval      $fc1(4)=*off
179900981216     C                   eval      $fc1(6)=*off
180000981216     C                   endif
180100990107     C                   if        opz0661 = '04'
180200990107     C                   eval      $fc1(2)=*off
180300990107     C                   eval      $fc1(4)=*off
180400990107     C                   endif
180500981221     C                   if        opz0661 = '02'
180600981221     C                   eval      $fc1(16)=*on
180700981221     C                   endif
180800981216     C*
180900981216     C                   exsr      GESPIE
181000981216     C*
181100940225     C                   ENDSR
181200940225     C/EJECT
181300940506     C************************************************************
181400940506     C* REPERIMENTO TASTI DI FUNZIONE
181500940506     C************************************************************
181600940506     C     REPKEY        BEGSR
181700981216     C*
181800940506     C                   MOVEA     $FM1          $FM
181900981216     C                   MOVEA     $FC1          $FC
182000940506     C                   EXSR      REPKKK
182100940506     C                   MOVEA     $FX           $FX1
182200981216     C*
182300950630     C                   ENDSR
182400950712     C/EJECT
182500940506     C************************************************************
182600940506     C* REPERIMENTO TASTI DI FUNZIONE
182700940506     C************************************************************
182800940506     C     REPKKK        BEGSR
182900940506     C*
183000940506     C                   CLEAR                   $FX
183100940506     C     1             DO        24            X
183200940506     C*
183300940506     C     $FM(X)        IFNE      *BLANK
183400950906     C     $Fc(X)        andne     '0'
183500940506     C*
183600940506     C                   MOVEL     $FM(X)        IDMSG
183700950522     C                   CALLB     'XRTVM'
183800940506     C                   PARM                    IDMSG            27
183900940506     C                   PARM                    TXTMSG          100
184000940506     C                   PARM                    ERRMSG            1
184100940506     C*
184200940506     C     ERRMSG        IFNE      *ON
184300940506     C                   MOVEL     TXTMSG        $FX(X)
184400940506     C                   ELSE
184500940506     C                   MOVEL     *ALL' '       $FX(X)
184600941027     C                   ENDIF
184700940506     C*
184800941027     C                   ENDIF
184900940506     C*
185000941027     C                   ENDDO
185100940506     C*
185200940506     C                   ENDSR
185300950713     C/EJECT
185400940128     C************************************************************
185500950606     C* DEFINIZIONE KLIST
185600940128     C************************************************************
185700950606     C     DefKlist      BEGSR
185800940128     C*
185900981216    >C* NDCPA01L
186000981216    >C     k02cpa01      KLIST
186100981216    >C                   KFLD                    CPASys
186200981216    >C                   KFLD                    CPANrAsInt
186201170223R027 C*
186202170223  "  C* SDGSO301L
186203170223  "  C     K04SO301      KLIST
186204170223  "  C                   KFLD                    SO3Sogg
186205170223  "  C                   KFLD                    SO3DtFiVL
186206170223  "  C                   KFLD                    SO3Sys
186207170223R027 C                   KFLD                    SO3NrAsint
186300981221     C*
186400981221     C* NDPAS01L
186500981221     C     k02pas01      KLIST
186600981221     C                   KFLD                    PASSys
186700981221     C                   KFLD                    PASNrAsReg
186800981221     C*
186900981221     C* NDPPA01L
187000981221     C     k07ppa01      KLIST
187100981221     C                   KFLD                    PASSocieta
187200981221     C                   KFLD                    PASCtb
187300981221     C                   KFLD                    PASKcc
187400981221     C                   KFLD                    PASKsc
187500981221     C                   KFLD                    PASDtPar
187600981221     C                   KFLD                    PASNrPar
187700981221     C                   KFLD                    PASSerPar
187800100824     C*
187900070206A1210X* ANCAP01L
188000070206  "  X*    K02CAP01      KLIST
188100070206  "  X*                  KFLD                    CAPSTATO
188200070206A1210X*                  KFLD                    CAPCAP
188300100824C2163x**ANCP101L  ex A1210
188400100824  "  x***  K02CP101      KLIST
188500100824  "  x***                KFLD                    Cp1STATO
188600100824C2163C*** ex A1210       KFLD                    Cp1CAP
188700981229     C*
188800940117     C                   ENDSR
188900940223     C/EJECT
189000981216** $OZ / $ID
18910095071201PROMSG    *LIBL     COS0001
18920095071202PROMSG    *LIBL     COS0002
18930095071203PROMSG    *LIBL     COS0003
18940095071204PROMSG    *LIBL     COS0004
18950095071205PROMSG    *LIBL     COS0005
189600981216** TESTI TASTI DI FUNZIONE - $FM1 / $FX1
189700940506PROMSG    *LIBL     KEY0001  01
189800940506PROMSG    *LIBL     KEY0002  02
189900940506PROMSG    *LIBL     KEY0003  03
190000940506PROMSG    *LIBL     KEY0004  04
190100940506PROMSG    *LIBL     KEY0005  05
190200940506PROMSG    *LIBL     KEY0006  06
190300940506PROMSG    *LIBL     KEY0007  07
190400940506PROMSG    *LIBL     KEY0008  08
190500961204PROMSG    *LIBL     KEY0009  09
190600940506PROMSG    *LIBL     KEY0010  10
190700940506PROMSG    *LIBL     KEY0011  11
190800940506PROMSG    *LIBL     KEY0012  12
190900940506PROMSG    *LIBL     KEY0013  13
191000940506PROMSG    *LIBL     KEY0014  14
191100940506PROMSG    *LIBL     KEY0015  15
191200940506PROMSG    *LIBL     KEY0016  16
191300940506PROMSG    *LIBL     KEY0017  17
191400940506PROMSG    *LIBL     KEY0018  18
191500940506PROMSG    *LIBL     KEY0019  19
191600940506PROMSG    *LIBL     KEY0020  20
191700940506PROMSG    *LIBL     KEY0021  21
191800940506PROMSG    *LIBL     KEY0022  22
191900950712PROMSG    *LIBL     KEY0066  23
192000940506PROMSG    *LIBL     KEY0024  24
