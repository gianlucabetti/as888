000100150306     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300951128     H*PARMS CVTOPT(*DATETIME)
000400940223     H DECEDIT('0,') DATEDIT(*DMY.)
000500940117      *
000600150306      *       Ricezione attestazione certificazione
000700940223      *
000800940223      *
000900940128     F*----------------------------------------------------*
001000150306     FSDGSK65D  CF   E             WORKSTN
001100150306     F                                     RENAME(SK65D1:FMTD1)
001200150306     F                                     RENAME(SK65T1:FMTT1)
001300150306     F                                     RENAME(SK65Z1:FMTZ1)
001400940201     F                                     INFDS(DSFMT)
001500940128     D*----------------------------------------------------*
001600940506     D* Schiere per la gestione dei tasti di funzione
001700940506     D $FM             S             27    DIM(24)
001800940506     D $FX             S             50    DIM(24)
001900940506     D $FC             S              1    DIM(24)
002000940506     D $FR             S              1    DIM(24)
002100951128     D* Video D1
002200940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002300940506     D $FX1            S             50    DIM(24) ALT($FM1)
002400940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
002500940506     D $FR1            S              1    DIM(24) ALT($FC1)
002600940128     D*----------------------------------------------------*
002700940127     D* Passaggio Parametri
002800940127     D KPJBA         E DS
002900940318     D*-------------
003000970207     D* Passaggio parametri ai pgm
003100150306     D SK65DS        E DS                  EXTNAME(SDGSK65DS) INZ
003200150306     D SK66DS        E DS                  EXTNAME(SDGSK66DS) INZ
003300970207     D*-------------
003400940318     D* DS Esterna gestione messaggi per COMIT
003500940318     D XMSGDS        E DS
003600940318     D  STS          E                     EXTFLD(MSGSTS)
003700940721     D                                     DIM(333)
003800940318     D  JBA          E                     EXTFLD(MSGJBA)
003900940721     D                                     DIM(502)
004000950301     D*-------------
004100950616    >D* Reperimento Autorizzazioni
004200950616    >D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
004300950607     D*-------------
004400950607     D* Dati per pgm XNOAUT
004500950607     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
004600950301     D*-------------
004700950410     D* Passaggio Autorizzazioni
004800950410     D DSAUT           DS           512
004900950530     D*-------------
005000950126     D* Reperimento nome PGM
005100950126     D STATUS         SDS           333
005200950126     D  DSPGM            *PROC
005300950411     D  PARMS            *PARMS
005400960515     D*-------------
005500150306     D* Dati di ambiente ottenuti da SDGSOCDR
005600150306     D SOC001        E DS                  EXTNAME(SDGSOCDS )
005700941209     D*-------------
005800150306     D* DS Interna per dati di output di SDGSOCDR
005900941209     D XSOCDS          DS          1000
006000940203     D*-------------
006100960823     D* Parametri di base
006200960820     D XPAOUT          DS          2000
006300960820     D*-------------
006400150306     D* Parametri modulo SCARSUMA
006500150306     D ANP022DS      E DS
006600150306     D*-------------
006700960823     D* Controlli validità legende
006800960823     D X07DS         E DS                  EXTNAME(X07VALDS)
006900960823     D*-------------
007000940201     D DSFMT           DS
007100940506     D  $TASTO               369    369
007200940201     D  NRG                  370    370
007300940201     D  NCL                  371    371
007400950314     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
007500940201     D*-------------
007600940201     D* posizione cursore
007700940201     D CURSOR          DS
007800950530     D  RIRI                   1      2B 0 INZ
007900950530     D  R$$                    2      2
008000940223     D  COCO                   3      4B 0 INZ
008100940201     D  C$$                    4      4
008200940225     D*-------------
008300940506     D* Tasti di funzione
008400940506     D F01             C                   CONST(X'31')
008500940506     D F02             C                   CONST(X'32')
008600940506     D F03             C                   CONST(X'33')
008700940506     D F04             C                   CONST(X'34')
008800940506     D F05             C                   CONST(X'35')
008900940506     D F06             C                   CONST(X'36')
009000940506     D F07             C                   CONST(X'37')
009100940506     D F08             C                   CONST(X'38')
009200940506     D F09             C                   CONST(X'39')
009300940506     D F10             C                   CONST(X'3A')
009400940506     D F11             C                   CONST(X'3B')
009500940506     D F12             C                   CONST(X'3C')
009600940506     D F13             C                   CONST(X'B1')
009700940506     D F14             C                   CONST(X'B2')
009800940506     D F15             C                   CONST(X'B3')
009900940506     D F16             C                   CONST(X'B4')
010000940506     D F17             C                   CONST(X'B5')
010100940506     D F18             C                   CONST(X'B6')
010200940506     D F19             C                   CONST(X'B7')
010300940506     D F20             C                   CONST(X'B8')
010400940506     D F21             C                   CONST(X'B9')
010500940506     D F22             C                   CONST(X'BA')
010600940506     D F23             C                   CONST(X'BB')
010700940506     D F24             C                   CONST(X'BC')
010800940506     D ENTER           C                   CONST(X'F1')
010900940506     D ROLDWN          C                   CONST(X'F4')
011000940506     D ROLLUP          C                   CONST(X'F5')
011100950626     D*-------------
011200950626     D* Sottotitolo di testata
011300150306     D SOTTIT          C                   CONST('                           ')
011400150306     D*-------------
011500150306     D CallPgm         S              1
011600940127     C*----------------------------------------------------*
011700940127     C*                MAIN LINE PROGRAM
011800940127     C*----------------------------------------------------*
011900940127     C*
012000940223     C* inizializzazione variabili
012100940223     C                   EXSR      INZVAR
012200940223     C*
012300940223     C     $FINE         DOWEQ     *OFF
012400940202     C     $GEST         CASEQ     'D1'          GESD1
012500941027     C                   ENDCS
012600941027     C                   ENDDO
012700940201     C*
012800940325     C* fine programma
012900940325     C                   EXSR      ENDPGM
013000940325     C/EJECT
013100940325     C************************************************************
013200951129     C* FINE PROGRAMMA
013300940325     C************************************************************
013400940325     C     ENDPGM        BEGSR
013500950530     C*
013600941214     C*
013700941214     C* se ho confermato l'uscita proseguo
013800941214     C     $FINE         IFEQ      *ON
013900940325     C*
014000950606     C                   SETON                                            LR
014100940616     C                   RETURN
014200941214     C*
014300941214     C                   ENDIF
014400940325     C*
014500940325     C                   ENDSR
014600940325     C/EJECT
014700940201     C************************************************************
014800940201     C* RICERCHE
014900940201     C************************************************************
015000940127     C     SEARCH        BEGSR
015100940127     C*
015200940127     C* determino Riga/Colonna del cursore
015300940127     C                   MOVE      NRG           R$$
015400940127     C                   MOVE      NCL           C$$
015500940127     C                   Z-ADD     RIRI          CURR
015600940127     C                   Z-ADD     COCO          CURC
015700940127     C*
015800940224     C* I campi che al momento dell'F4 risultano protetti non devono
015900940224     C* essere abilitati alla funzione di ricerca
016000941027     C*
016100941027     C                   SELECT
016200960515     C*
016300951128     C*
016400940223     C* ricerca non abilitata in questa posizione
016500941027     C                   OTHER
016600940926     C                   SETON                                        9899
016700940127     C*
016800941027     C                   ENDSL
016900951128     C*
017000951128     C* imposto pos. del cursore
017100951128     C                   Z-ADD     CURR          H1RIGA
017200951128     C                   Z-ADD     CURC          H1COLO
017300940127     C*
017400940506     C                   MOVE      *HIVAL        $LASTV
017500951128     C*
017600940127     C                   ENDSR
017700940225     C/EJECT
017800940225     C************************************************************
017900940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
018000940225     C************************************************************
018100940225     C     GESPIE        BEGSR
018200940225     C*
018300940225     C                   SELECT
018400940506     C     $GEST         WHENEQ    'D1'
018500940506     C     $ULKD1        IFEQ      0
018600940506     C* riempimento opzioni
018700940506     C                   EXSR      F24D1
018800940506     C                   ELSE
018900940506     C                   WRITE     FMTZ1
019000940506     C                   ENDIF
019100951129     C                   ENDSL
019200940225     C*
019300940225     C                   ENDSR
019400940131     C/EJECT
019500940131     C************************************************************
019600940510     C* GESTIONE VIDEO RECORD D1
019700940131     C************************************************************
019800940127     C     GESD1         BEGSR
019900940225     C* inizializzazione videata
020000941117   B1C     $INZD1        IFEQ      *ON
020100940127     C                   EXSR      INZD1
020200940223     C                   MOVE      *OFF          $INZD1
020300941117   E1C                   ENDIF
020400940225     C* emissione piede videata se proveniente da altra
020500940225     C* salvataggio valore formato di provenienza
020600941117   B1C     $LASTV        IFNE      'D1'
020700940207     C                   WRITE     FMTT1
020800940225     C                   EXSR      GESPIE
020900940202     C                   MOVE      $LASTV        $LASTG
021000940202     C                   MOVE      'D1'          $LASTV
021100941117   E1C                   ENDIF
021200940707     C*
021300940707     C* se esistono errori sulla videata
021400940707     C* emetto la write del formato a indicatori spenti per vedere
021500940707     C* le eventuali decodifiche
021600941117   B1C     *IN99         IFEQ      *ON
021700940707     C     $GEST         ANDEQ     'D1'
021800940707     C                   MOVEA     *IN           WIN
021900940707     C                   MOVE      *ALL'0'       IN5098           49
022000940707     C                   MOVEA     IN5098        *IN(50)
022100940707     C                   WRITE     FMTD1
022200940707     C                   MOVEA     WIN           *IN
022300941117   E1C                   ENDIF
022400940131     C*
022500940203     C                   EXFMT     FMTD1
022600940506     C* controllo abilitazione tasti funzione
022700940506     C*
022800940506     C                   EXSR      CTRKD1
022900940506     C*
023000941117   B1C     *IN99         IFEQ      *OFF
023100940131     C*
023200941117   B2C                   SELECT
023300940609     C* ROLLUP
023400950626    >C*    $TASTO        WHENEQ    ROLLUP
023500950626    >C*                  MOVE      'D2'          $GEST
023600940916     C* F2=Decodifica
023700940916     C     $TASTO        WHENEQ    F02
023800940916     C                   EXSR      F02D1
023900940131     C* F3=Fine
024000940506     C     $TASTO        WHENEQ    F03
024100940309     C                   EXSR      F03D1
024200940131     C* F4=Ricerca
024300940506     C     $TASTO        WHENEQ    F04
024400940127     C                   EXSR      SEARCH
024500940224     C* F5=Refresh
024600940506     C     $TASTO        WHENEQ    F05
024700940309     C                   EXSR      F05D1
024800940223     C* F8=Successivo
024900940506     C     $TASTO        WHENEQ    F08
025000940309     C                   EXSR      F08D1
025100940131     C* F12=Ritorno
025200940506     C     $TASTO        WHENEQ    F12
025300940309     C                   EXSR      F12D1
025400940315     C* F24=Altri tasti
025500940506     C     $TASTO        WHENEQ    F24
025600940315     C                   EXSR      F24D1
025700940131     C*
025800941117   X2C                   OTHER
025900940127     C                   EXSR      CTRD1
026000940203     C*
026100941117   B3C     $TASTO        IFEQ      F06
026200940207     C     *IN99         ANDEQ     *OFF
026300950530     C* eseguo lancio
026400950530     C                   EXSR      EXELAN
026500950629     C                   move      *on           $fine
026600951129     C                   EXSR      ENDPGM
026700941117   E3C                   ENDIF
026800940203     C*
026900941117   E2C                   ENDSL
027000940224     C*
027100941117   E1C                   ENDIF
027200941027     C*
027300940117     C                   ENDSR
027400941209     C/EJECT
027500941209     C************************************************************
027600941209     C* INIZIALIZZAZIONE VIDEATA DATI
027700941209     C************************************************************
027800940127     C     INZD1         BEGSR
027900940131     C*
028000940131     C                   CLEAR                   FMTD1
028100951128    >C*
028200951128     C                   Z-ADD     1             $PIED1
028300150306     C*
028400150306     C* se il pgm e'stato richiamato imposto i valori ricevuti dalla DS
028500150306     C                   If        CallPgm     = '1'
028600150306     C*
028700150306     C                   Z-Add     AnnoK65       D1Anno
028800150306     C*
028900150306     C* se il pgm NON e stato richiamato
029000150306     C                   ElSe
029100150306     C*
029200150306     C* Immettere i valodi di default a video
029300150306     C                   Eval      D1Anno      = *Year
029400150306     C                   Eval      D1Anno      = D1Anno - 1
029500150306     C*
029600150306     C                   EndIf
029700940224     C*
029800940117     C                   ENDSR
029900940207     C/EJECT
030000940916     C************************************************************
030100940916     C* GESTIONE F02 VIDEO D1
030200940916     C************************************************************
030300940916     C     F02D1         BEGSR
030400940916     C*
030500940916     C* per decodificare la videata richiamo la routine dei ctrl
030600940916     C* e poi disattivo gli eventuali messaggi di errore
030700940916     C                   EXSR      CTRD1
030800940916     C*
030900940916     C                   MOVE      *ALL'0'       IN5098
031000940916     C                   MOVEA     IN5098        *IN(50)
031100940916     C*
031200940916     C                   ENDSR
031300940309     C/EJECT
031400940309     C************************************************************
031500940309     C* GESTIONE F03 VIDEO D1
031600940309     C************************************************************
031700940309     C     F03D1         BEGSR
031800940309     C*
031900940309     C                   MOVE      *ON           $FINE
032000940325     C* fine programma
032100940325     C                   EXSR      ENDPGM
032200940325     C*
032300940325     C                   ENDSR
032400940309     C/EJECT
032500940309     C************************************************************
032600940309     C* GESTIONE F05 VIDEO D1
032700940309     C************************************************************
032800940309     C     F05D1         BEGSR
032900940309     C*
033000940309     C                   MOVE      *ON           $INZD1
033100940309     C*
033200940309     C                   ENDSR
033300940309     C/EJECT
033400940309     C************************************************************
033500940309     C* GESTIONE F08 VIDEO D1
033600940309     C************************************************************
033700940309     C     F08D1         BEGSR
033800940309     C*
033900940309     C                   MOVE      *ON           $FINE
034000940309     C*
034100940309     C                   ENDSR
034200940309     C/EJECT
034300940309     C************************************************************
034400940309     C* GESTIONE F12 VIDEO D1
034500940309     C************************************************************
034600940309     C     F12D1         BEGSR
034700940309     C*
034800940309     C                   MOVE      *ON           $FINE
034900940309     C*
035000940309     C                   ENDSR
035100950530     C/EJECT
035200940309     C************************************************************
035300940309     C* GESTIONE F24 VIDEO D1
035400940309     C************************************************************
035500940309     C     F24D1         BEGSR
035600940506     C*
035700940506     C                   MOVEA     $FX1          $FX
035800940506     C                   MOVEA     $FC1          $FC
035900940506     C                   MOVEA     $FR1          $FR
036000940506     C                   Z-ADD     $ULKD1        $ULK
036100950530     C                   Z-ADD     79            $L01
036200950530     C                   Z-ADD     79            $L02
036300940506     C                   EXSR      RIEKEY
036400940506     C                   Z-ADD     $ULK          $ULKD1            3 0
036500940720     C                   MOVEA     $FC           $FC1
036600940506     C                   MOVEL     $KEY1         Z1KE1
036700940506     C                   MOVEL     $KEY2         Z1KE2
036800940506     C                   WRITE     FMTZ1
036900940506     C*
037000940309     C                   ENDSR
037100950530     C/EJECT
037200940131     C************************************************************
037300940207     C* CONTROLLO VIDEATA
037400940131     C************************************************************
037500940127     C     CTRD1         BEGSR
037600940131     C*
037700940127     C                   SETOFF                                       99
037800940127     C* non imposto pos. del cursore
037900940127     C                   Z-ADD     0             H1RIGA
038000940127     C                   Z-ADD     0             H1COLO
038100940224     C*
038200150306     C                   If        D1Anno      = 0
038300150306     C                   Seton                                        5099
038400150306     C                   EndIf
038500960820     C*
038600940117     C                   ENDSR
038700940207     C/EJECT
038800940224     C************************************************************
038900950530     C* ESECUZIONE LANCIO
039000940224     C************************************************************
039100950530     C     EXELAN        BEGSR
039200960823     C*
039300150306     C                   reset                   SK66DS
039400150306     C                   move      XscSoc        SocK66
039500150306     C                   Eval      AnnoK66     = D1Anno
039600970207     C*
039700150306     C                   movel     SK66DS        KPJBU
039800150306     C*
039900150306     C                   call      'SDGSK66R'
040000150306     C                   parm                    KPJBA
040100150306     C*
040200150306     C                   movel     KPJBU         SK66DS
040300150306     C*
040400150306     C* ritorno da PGM gestione
040500150306     C                   Exsr      RTNK66
040600951128     C*
040700940203     C                   ENDSR
040800941205     C/EJECT
040900150306     C************************************************************
041000150306     C* GESTIONE RITORNO DA PGM DI GESTIONE
041100150306     C************************************************************
041200150306     C     RTNK66        BegSr
041300150306     C*
041400150306     C* funzione non eseguibile per errore :
041500150306     C*
041600150306     C                   Select
041700150306     C* 0 = funzione eseguita correttamente
041800150306     C     ERRK66        WhenEq    '0'
041900150306     C                   Eval      $Fine       = *On
042000150306     C*
042100150306     C*
042200150306     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
042300150306     C     ERRK66        WhenEq    '1'
042400150306     C     ERRK66        OREQ      '2'
042500150306     C                   Seton                                        9399
042600150306     C*
042700150306     C                   EndSl
042800150306     C*
042900150306     C                   EndSr
043000950530     C************************************************************
043100950530     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
043200950530     C************************************************************
043300950530     C     CTRKD1        BEGSR
043400950530     C*
043500950530     C                   SETOFF                                       99
043600950530     C                   MOVEA     $FC1          $FC
043700950530     C                   EXSR      CTRKEY
043800950530     C     *IN99         IFEQ      *ON
043900950530     C                   SETON                                        97
044000950530     C                   ENDIF
044100950530     C*
044200950530     C                   ENDSR
044300950530     C/EJECT
044400941205     C************************************************************
044500941205     C* CONTROLLO ABILITAZIONE TASTI
044600941205     C************************************************************
044700941205     C     CTRKEY        BEGSR
044800941205     C*
044900941205     C                   SELECT
045000941205     C     $TASTO        WHENEQ    F01
045100941205     C     $FC(1)        IFEQ      '0'
045200941205     C                   SETON                                        99
045300941205     C                   ENDIF
045400941205     C     $TASTO        WHENEQ    F02
045500941205     C     $FC(2)        IFEQ      '0'
045600941205     C                   SETON                                        99
045700941205     C                   ENDIF
045800941205     C     $TASTO        WHENEQ    F03
045900941205     C     $FC(3)        IFEQ      '0'
046000941205     C                   SETON                                        99
046100941205     C                   ENDIF
046200941205     C     $TASTO        WHENEQ    F04
046300941205     C     $FC(4)        IFEQ      '0'
046400941205     C                   SETON                                        99
046500941205     C                   ENDIF
046600941205     C     $TASTO        WHENEQ    F05
046700941205     C     $FC(5)        IFEQ      '0'
046800941205     C                   SETON                                        99
046900941205     C                   ENDIF
047000941205     C     $TASTO        WHENEQ    F06
047100941205     C     $FC(6)        IFEQ      '0'
047200941205     C                   SETON                                        99
047300941205     C                   ENDIF
047400941205     C     $TASTO        WHENEQ    F07
047500941205     C     $FC(7)        IFEQ      '0'
047600941205     C                   SETON                                        99
047700941205     C                   ENDIF
047800941205     C     $TASTO        WHENEQ    F08
047900941205     C     $FC(8)        IFEQ      '0'
048000941205     C                   SETON                                        99
048100941205     C                   ENDIF
048200941205     C     $TASTO        WHENEQ    F09
048300941205     C     $FC(09)       IFEQ      '0'
048400941205     C                   SETON                                        99
048500941205     C                   ENDIF
048600941205     C     $TASTO        WHENEQ    F10
048700941205     C     $FC(10)       IFEQ      '0'
048800941205     C                   SETON                                        99
048900941205     C                   ENDIF
049000941205     C     $TASTO        WHENEQ    F11
049100941205     C     $FC(11)       IFEQ      '0'
049200941205     C                   SETON                                        99
049300941205     C                   ENDIF
049400941205     C     $TASTO        WHENEQ    F12
049500941205     C     $FC(12)       IFEQ      '0'
049600941205     C                   SETON                                        99
049700941205     C                   ENDIF
049800941205     C     $TASTO        WHENEQ    F13
049900941205     C     $FC(13)       IFEQ      '0'
050000941205     C                   SETON                                        99
050100941205     C                   ENDIF
050200941205     C     $TASTO        WHENEQ    F14
050300941205     C     $FC(14)       IFEQ      '0'
050400941205     C                   SETON                                        99
050500941205     C                   ENDIF
050600941205     C     $TASTO        WHENEQ    F15
050700941205     C     $FC(15)       IFEQ      '0'
050800941205     C                   SETON                                        99
050900941205     C                   ENDIF
051000941205     C     $TASTO        WHENEQ    F16
051100941205     C     $FC(16)       IFEQ      '0'
051200941205     C                   SETON                                        99
051300941205     C                   ENDIF
051400941205     C     $TASTO        WHENEQ    F17
051500941205     C     $FC(17)       IFEQ      '0'
051600941205     C                   SETON                                        99
051700941205     C                   ENDIF
051800941205     C     $TASTO        WHENEQ    F18
051900941205     C     $FC(18)       IFEQ      '0'
052000941205     C                   SETON                                        99
052100941205     C                   ENDIF
052200941205     C     $TASTO        WHENEQ    F19
052300941205     C     $FC(19)       IFEQ      '0'
052400941205     C                   SETON                                        99
052500941205     C                   ENDIF
052600941205     C     $TASTO        WHENEQ    F20
052700941205     C     $FC(20)       IFEQ      '0'
052800941205     C                   SETON                                        99
052900941205     C                   ENDIF
053000941205     C     $TASTO        WHENEQ    F21
053100941205     C     $FC(21)       IFEQ      '0'
053200941205     C                   SETON                                        99
053300941205     C                   ENDIF
053400941205     C     $TASTO        WHENEQ    F22
053500941205     C     $FC(22)       IFEQ      '0'
053600941205     C                   SETON                                        99
053700941205     C                   ENDIF
053800941205     C     $TASTO        WHENEQ    F23
053900941205     C     $FC(23)       IFEQ      '0'
054000941205     C                   SETON                                        99
054100941205     C                   ENDIF
054200941205     C     $TASTO        WHENEQ    F24
054300941205     C     $FC(24)       IFEQ      '0'
054400941205     C                   SETON                                        99
054500941205     C                   ENDIF
054600941205     C                   ENDSL
054700941205     C*
054800941205     C                   ENDSR
054900940207     C/EJECT
055000940131     C************************************************************
055100940131     C* OPERAZIONI INIZIALI
055200940131     C************************************************************
055300940131     C     *INZSR        BEGSR
055400940131     C*
055500940127     C* Reperimento parametri
055600940127     C*
055700940117     C     *ENTRY        PLIST
055800940117     C                   PARM                    KPJBA
055900960919     C                   PARM                    STATO             1
056000940506     C*
056100940506     C* Reperimento tasti di funzione
056200940506     C*
056300940506     C                   EXSR      REPKEY
056400940506     C*
056500940223     C                   ENDSR
056600940223     C/EJECT
056700940506     C************************************************************
056800940506     C* REPERIMENTO TASTI DI FUNZIONE
056900940506     C************************************************************
057000940506     C     REPKEY        BEGSR
057100951002     C*
057200940506     C                   MOVEA     $FM1          $FM
057300951002     C                   MOVEA     $FC1          $FC
057400940506     C                   EXSR      REPKKK
057500940506     C                   MOVEA     $FX           $FX1
057600940614     C*
057700940614     C                   ENDSR
057800940506     C/EJECT
057900940506     C************************************************************
058000940506     C* REPERIMENTO TASTI DI FUNZIONE
058100940506     C************************************************************
058200940506     C     REPKKK        BEGSR
058300940506     C*
058400940506     C                   CLEAR                   $FX
058500940506     C     1             DO        24            X
058600940506     C*
058700940506     C     $FM(X)        IFNE      *BLANK
058800951002     C     $FC(X)        ANDNE     '0'
058900940506     C*
059000940506     C                   MOVEL     $FM(X)        IDMSG
059100950522     C                   CALLB     'XRTVM'
059200940506     C                   PARM                    IDMSG            27
059300940506     C                   PARM                    TXTMSG          100
059400940506     C                   PARM                    ERRMSG            1
059500940506     C*
059600940506     C     ERRMSG        IFNE      *ON
059700940506     C                   MOVEL     TXTMSG        $FX(X)
059800940506     C                   ELSE
059900940506     C                   MOVEL     *ALL' '       $FX(X)
060000941027     C                   ENDIF
060100940506     C*
060200941027     C                   ENDIF
060300940506     C*
060400941027     C                   ENDDO
060500940506     C*
060600940506     C                   ENDSR
060700940509     C/EJECT
060800940223     C************************************************************
060900940223     C* INIZIALIZZAZIONE VARIABILI
061000940223     C************************************************************
061100940223     C     INZVAR        BEGSR
061200150306     C*
061300150306     C                   MoveL     KPJBU         SK65DS
061400150306     C*
061500150306     C* se il pgm e stato richiamato imposto i valori ricevuti dalla DS
061600150306     C                   If        SocK65 <> *Blanks
061700150306     C                   Eval      CallPgm     = '1'
061800150306     C                   ElSe
061900150306     C                   Eval      CallPgm     = '0'
062000150306     C                   EndIf
062100960919     C*
062200150306     C                   MOVEL     'SOC001'      TIPXSC
062300150306     C                   MoveL     SocK65        SOCXSC
062400941115     C                   EXSR      REPSOC
062500941115     C     RTNXSC        IFNE      '1'
062600941115     C                   MOVEL     XSOCDS        SOC001
062700941115     C                   ELSE
062800941115     C                   RETURN
062900941115     C                   ENDIF
063000951128     C*
063100950301     C* Reperimento autorizzazioni
063200150306     C                   EXSR      REPAUT
063300150306     C*
063400150306     C* Reperimento libreria file
063500150306     C*****              EXSR      RepLibFile
063600940715     C*
063700940715     C* Definizione dei tasti funzione utilizzabili a pgm
063800940715     C                   EXSR      DEFOPZ
063900940128     C*
064000940223     C* Variabili per gestione videate
064100940128     C*
064200940223     C                   MOVE      'D1'          $GEST
064300940223     C                   MOVE      *OFF          $FINE
064400940223     C                   MOVE      *ON           $INZD1
064500940224     C                   MOVE      *BLANK        $LASTG
064600940224     C                   MOVE      *BLANK        $LASTV
064700940506     C                   Z-ADD     0             $ULKD1            3 0
064800950530     C                   Z-ADD     0             $ULKD3            3 0
064900940127     C*
065000940223     C* Variabili appoggio
065100940127     C*
065200940224     C                   Z-ADD     0             CURR
065300940224     C                   Z-ADD     0             CURC
065400940207     C*
065500940207     C* Valorizzazione campi univoci testate
065600940207     C*
065700940207     C                   CLEAR                   FMTT1
065800940207     C                   MOVEL     KNSIF         NOMSIF
065900941110     C                   MOVEL     XSCDSI        NOMDIT
066000150306     C                   MOVEL     KCDAZ         NOMAZN
066100950331     C* recupero il sottotitolo (se c'è)
066200150306     C                   MOVEL     *BLANK        D1TIT
066300950331     C     SOTTIT        IFNE      *BLANK
066400960820     C                   MOVEL     SOTTIT        WWstit           27
066500960820     C                   Movel     WWstit        IDMSG
066600950522     C                   CALLB     'XRTVM'
066700950331     C                   PARM                    IDMSG            27
066800950331     C                   PARM                    TXTMSG          100
066900950331     C                   PARM                    ERRMSG            1
067000950331     C                   PARM      *ON           CTRMSG            1
067100950331     C                   PARM      30            LENMSG            3 0
067200950331     C     ERRMSG        IFNE      *ON
067300150306     C                   MOVEL     TXTMSG        D1TIT
067400950331     C                   ELSE
067500150306     C                   MOVEL     *ALL'?'       D1TIT
067600950331     C                   ENDIF
067700950331     C                   ENDIF
067800960820     C*
067900940224     C* Inizializzazione/Decodifica prima videata
068000940224     C*
068100940224     C                   EXSR      INZD1
068200940224     C                   MOVE      *OFF          $INZD1
068300960823     C*
068400940117     C                   ENDSR
068500941209     C/EJECT
068600941209     C************************************************************
068700941209     C* REPERIMENTO DATI SOCIETA'
068800941209     C************************************************************
068900941209     C     REPSOC        BEGSR
069000941209     C*
069100150306     C                   CALL      'SDGSOCDR'
069200941209     C                   PARM                    TIPXSC            6
069300941209     C                   PARM                    SOCXSC            3
069400941209     C                   PARM                    CDSXSC            9 0
069500941209     C                   PARM                    MODXSC            3
069600941209     C                   PARM      *BLANKS       RTNXSC            1
069700941209     C                   PARM                    XSOCDS
069800941209     C                   PARM                    KPJBA
069900941209     C*
070000941209     C                   ENDSR
070100950301     C/EJECT
070200150306     C************************************************************
070300150306     C* Reperimento libreria per scarico su supporto magnetico
070400150306     C************************************************************
070500150306     C     RepLibFile    BegSr
070600150306     C*
070700150306     C                   Clear                   ANP022DS
070800150306     C*
070900150306     C                   CALLB     'XPAR'
071000150306     C                   Parm                    XScSoc
071100150306     C                   Parm      'SCARSUMA'    XPaPar            8
071200150306     C                   Parm                    XPaOut         2000
071300150306     C                   Parm      *OFF          XPaErr            1
071400150306     C                   Parm      ' '           XPaRic            1
071500150306     C*
071600150306     C                   If        XPaErr      = *On
071700150306     C*
071800150306     C* Errore: fallito reperimento recupero parametro modulo &1
071900150306     C* Valorizzo le variabili del msg PRO2184
072000150306     C                   MoveL     XPaPar        P1Param
072100150306     C                   Seton                                        6399
072200150306     C*
072300150306     C                   ElSe
072400150306     C                   MoveL     XPaOut        ANP022DS
072500150306     C                   EndIf
072600150306     C*
072700150306     C                   EndSr
072800950301     C************************************************************
072900950301     C* REPERIMENTO AUTORIZZAZIONI UTENTE
073000950301     C************************************************************
073100950301     C     REPAUT        BEGSR
073200950301     C*
073300950301     C*
073400950301     C                   ENDSR
073500950530     C/EJECT
073600940715     C************************************************************
073700940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
073800940715     C************************************************************
073900940715     C     DEFOPZ        BEGSR
074000940715     C*
074100940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
074200940715     C* con S o N a seconda della utilizzabilità o meno
074300940715     C* del tasto funzione correlato
074400940715     C*
074500940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
074600940715     C* di compilazione
074700940715     C                   RESET                   $FC1
074800940715     C                   RESET                   $FR1
074900940715     C*
075000940715     C                   ENDSR
075100950530     C/EJECT
075200940128     C************************************************************
075300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
075400940223     C************************************************************
075500940223     C     DEFCAM        BEGSR
075600940223     C*
075700940223     C* klist
075800940207     C*
075900940223     C*
076000940223     C* Variabili per gestione videate
076100940223     C*
076200940223     C                   MOVE      *BLANK        $GEST             2
076300940223     C                   MOVE      *BLANK        $FINE             1
076400940223     C                   MOVE      *BLANK        $INZD1            1
076500940223     C                   MOVE      *BLANK        $LASTG            2
076600940223     C                   MOVE      *BLANK        $LASTV            2
076700940225     C                   Z-ADD     0             $PIED1            3 0
076800940223     C*
076900940223     C* Variabili appoggio
077000940223     C*
077100940223     C                   Z-ADD     0             CURR              2 0
077200940223     C                   Z-ADD     0             CURC              2 0
077300940223     C                   MOVE      *ZEROS        WIN              99
077400940223     C*
077500940223     C* Indici
077600940223     C*
077700940223     C                   Z-ADD     0             X                 3 0
077800940223     C                   Z-ADD     0             Y                 3 0
077900940127     C*
078000940117     C                   ENDSR
078100940509     C/EJECT
078200940509     C************************************************************
078300940509     C* RIEMPIMENTO TASTI FUNZIONE
078400940509     C************************************************************
078500940509     C     RIEKEY        BEGSR
078600940509     C*
078700940509     C* Input :
078800940509     C* - $FX = schiera con tutti i testi disponibili
078900940509     C* - $FC = schiere con i flag di validità associati ai testi
079000940509     C* - $FR = schiere con i flag di REVERSE IMAGE
079100940509     C* - $ULK = ultimo testo caricato
079200940722     C* - $L01 = Lunghezza prima riga
079300940722     C* - $L02 = Lunghezza seconda riga
079400940509     C* Output :
079500940509     C* - $ULK
079600940609     C* - $KEY1= 1° riga di tasti funzione
079700940609     C* - $KEY2= 2° riga di tasti funzione
079800940509     C*
079900950522     C                   CALLB     'XKEYMSG'
080000940509     C* input
080100940509     C                   PARM                    $FX
080200940509     C                   PARM                    $FC
080300940509     C                   PARM                    $FR
080400940509     C                   PARM                    $ULK              3 0
080500940509     C* output
080600940509     C                   PARM                    $KEY1            79
080700940509     C                   PARM                    $KEY2            79
080800940722    >C* input
080900950530    >C                   PARM                    $L01              3 0
081000950530    >C                   PARM                    $L02              3 0
081100940509     C*
081200940509     C                   ENDSR
081300940509** TASTI DI FUNZIONE  D1
081400940509PROMSG    *LIBL     KEY0001  01
081500940509PROMSG    *LIBL     KEY0002  02
081600940509PROMSG    *LIBL     KEY0003  03
081700940509PROMSG    *LIBL     KEY0004  04
081800940509PROMSG    *LIBL     KEY0005  05
081900940509PROMSG    *LIBL     KEY0006  06
082000940509PROMSG    *LIBL     KEY0007  07
082100940509PROMSG    *LIBL     KEY0008  08
082200940509PROMSG    *LIBL     KEY0009  09
082300940509PROMSG    *LIBL     KEY0010  10
082400940509PROMSG    *LIBL     KEY0011  11
082500940509PROMSG    *LIBL     KEY0012  12
082600940509PROMSG    *LIBL     KEY0013  13
082700940509PROMSG    *LIBL     KEY0014  14
082800940509PROMSG    *LIBL     KEY0015  15
082900940509PROMSG    *LIBL     KEY0016  16
083000940509PROMSG    *LIBL     KEY0017  17
083100940509PROMSG    *LIBL     KEY0018  18
083200940509PROMSG    *LIBL     KEY0019  19
083300940509PROMSG    *LIBL     KEY0020  20
083400940509PROMSG    *LIBL     KEY0021  21
083500940509PROMSG    *LIBL     KEY0022  22
083600940509PROMSG    *LIBL     KEY0023  23
083700940509PROMSG    *LIBL     KEY0024  24
083800951128**  ABILITAZIONE D1
0839009407200N  01
0840009608200N  02
0841009407201N  03
0842009608200N  04
0843009511290N  05
0844009407201N  06
0845009407200N  07
0846009505300N  08
0847009407200N  09
0848009411160N  10
0849009407200N  11
0850009511291N  12
0851009407200N  13
0852009407200N  14
0853009407200N  15
0854009407200N  16
0855009407200N  17
0856009511290N  18
0857009407200N  19
0858009407200N  20
0859009608230N  21
0860009407200N  22
0861009412140N  23   Lasciare sempre impostato a '0' il valore della riga 23
0862009412140N  24   Lasciare sempre impostata a '0' il valore della riga 24
