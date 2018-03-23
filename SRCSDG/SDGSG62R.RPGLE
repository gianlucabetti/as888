000100070907     H*PARMS OPTION(*NOXREF *NODEBUGIO)
000200070907     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000300070907     H*PARMS CVTOPT(*NONE)
000400070907     H**********************************************************************
000500070907     H DECEDIT('0,') DATEDIT(*DMY.)
000600070907     H**********************************************************************
000700070907      *
000800100730      *         ------------------------------------------------
000900110811      *                   Comunicazione Operazioni Iva
001000100730      *                   Manutenzione file telematico
001100130909      *                        Tipi Recod A B E Z
001200100730      *         ------------------------------------------------
001300100730      *
001400070907     F**********************************************************************
001500130909     FSDGETE00F UF   E             DISK    USROPN
001600070907     F                                     INFDS(RRNPFDS)                       *
001700070907     F*------------
001800130909     FSDGSG62D  CF   E             WORKSTN
001900130909     F                                     RENAME(SG62D1:FMTD1)
002000130912     F                                     RENAME(SG62DA1:FMTDA1)
002100130912     F                                     RENAME(SG62DA2:FMTDA2)
002200130912     F                                     RENAME(SG62DA3:FMTDA3)
002300130912     F                                     RENAME(SG62DB1:FMTDB1)
002400130912     F                                     RENAME(SG62DB2:FMTDB2)
002500130912     F                                     RENAME(SG62DB3:FMTDB3)
002600130912     F                                     RENAME(SG62DZ1:FMTDZ1)
002700130912     F                                     RENAME(SG62DE1:FMTDE1)
002800130909     F                                     RENAME(SG62T1:FMTT1)
002900130909     F                                     RENAME(SG62Z1:FMTZ1)
003000130909     F                                     RENAME(SG62Z2:FMTZ2)
003100070907     F                                     INFDS(DSFMT)
003200070907     D**********************************************************************
003300070907     D* numero tipo operaz
003400070907     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003500070907     D $ID             S             27    DIM(5) ALT($OZ)
003600070907     D*----------------------------------------------------*
003700070907     D*Schiere tasti funzione generiche
003800070907     D* percorso IdMsg
003900070907     D $FM             S             27    DIM(24)
004000070907     D*  testo msg
004100070907     D $FX             S             50    DIM(24)
004200070907     D* abilitazione ('0'/'1')
004300070907     D $FC             S              1    DIM(24)
004400070907     D*  tipo abilitazione ('S'/'N')
004500070907     D $FR             S              1    DIM(24)
004600070907     D*----------------------------------------------------*
004700070907     D* Tasti funzione D1
004800070907     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004900070907     D $FX1            S             50    DIM(24) ALT($FM1)
005000070907     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005100070907     D $FR1            S              1    DIM(24) ALT($FC1)
005200070907     D*----------------------------------------------------*
005300070907     D* Tasti funzione A1-2-3/B1-2-3-4/Z1-2-3
005400070907     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005500070907     D $FX2            S             50    DIM(24) ALT($FM2)
005600070907     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
005700070907     D $FR2            S              1    DIM(24) ALT($FC2)
005800070907     D*----------------------------------------------------*
005900070907     D* Schiera contente i nomi dei programmi da richiamare con i tasti
006000070907     D* funzione personalizzati
006100070907     D $PGF1           S             10    DIM(25)
006200070907     D*----------------------------------------------------*
006300070907     D* dimensioni variabili che conterranno i tasti D1
006400070907     D $D1L01          S              3  0
006500070907     D $D1L02          S              3  0
006600070907     D* dimensioni variabili che conterranno i tasti An/Bn/Zn
006700070907     D $DxL01          S              3  0
006800070907     D $DxL02          S              3  0
006900070907     D*----------------------------------------------------*
007000070907    >D* Inserire qui la definizione di altre variabili per reperire
007100070907    >D* la lunghezza di altre videate che usano un piede diverso da
007200070907    >D* STD
007300070907     D*----------------------------------------------------*
007400070907     D* Passaggio Parametri
007500070907     D KPJBA         E DS
007600070907     D*----------------------------------------------------*
007700070907     D* DS Esterna gestione messaggi
007800100820     D SDGMSGDS      E DS                  INZ
007900070907     D  STS          E                     EXTFLD(MSGSTS)
008000070907     D                                     DIM(333)
008100070907     D  JBA          E                     EXTFLD(MSGJBA)
008200070907     D                                     DIM(502)
008300070907     D*----------------------------------------------------*
008400070907     D* Reperimento Autorizzazioni
008500070907     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
008600070907     D*-------------
008700070907     D* Dati per pgm XNOAUT
008800070907     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
008900070907     D*------
009000070907     D* Passaggio Autorizzazioni
009100070907     D DSAUT           DS           512
009200100730     D*----------------------------------------------------*
009300070907     D* Maschera dati autorizzazioni
009400070907    >D CONCIVDS      E DS
009500070907     D*----------------------------------------------------*
009600070907     D* Reperimento nome PGM
009700070907     D STATUS         SDS           333
009800070907     D  DSPGM            *PROC
009900070907     D  PARMS            *PARMS
010000070907     D*----------------------------------------------------*
010100070907     D* Dati di ambiente ottenuti da XSOC
010200070907     D SOC001        E DS                  EXTNAME(XSOC001DS)
010300070907     D*-------------
010400070907     D* DS Interna per dati di output di XSOC
010500070907     D XSOCDS          DS          1000
010600070907     D*----------------------------------------------------*
010700070907     D* Passaggio Parametri
010800130909     D SDGSG62DS     E DS
010900070907     D*-------------
011000070907     D* Catalogo file per scarico su supporto magnetico
011100070907     D ANA122DS      E DS
011200070907     D*-------------
011300070907     D* Parametri modulo SCARSUMA
011400070907     D ANP022DS      E DS
011500070907     D*----------------------------------------------------*
011600070907     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011700070907     D* alla funzione personalizzata
011800130909     D $SaveDS         S                   like(SDGSG62DS)
011900070907     D*----------------------------------------------------*
012000070907     D DSFMT           DS
012100070907     D  $TASTO               369    369
012200070907     D  NRG                  370    370
012300070907     D  NCL                  371    371
012400070907     D*----------------------------------------------------*
012500070907     D* DS per UserExit
012600070907     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
012700070907     D*----------------------------------------------------*
012800070907     D* posizione cursore
012900070907     D CURSOR          DS
013000070907     D  RIRI                   1      2B 0 INZ
013100070907     D  R$$                    2      2
013200070907     D  COCO                   3      4B 0 INZ
013300070907     D  C$$                    4      4
013400070907     D*----------------------------------------------------*
013500070907     D* Costanti
013600070907     D*----------------------------------------------------*
013700070907     D* Tasti di funzione
013800070907     D F01             C                   CONST(X'31')
013900070907     D F02             C                   CONST(X'32')
014000070907     D F03             C                   CONST(X'33')
014100070907     D F04             C                   CONST(X'34')
014200070907     D F05             C                   CONST(X'35')
014300070907     D F06             C                   CONST(X'36')
014400070907     D F07             C                   CONST(X'37')
014500070907     D F08             C                   CONST(X'38')
014600070907     D F09             C                   CONST(X'39')
014700070907     D F10             C                   CONST(X'3A')
014800070907     D F11             C                   CONST(X'3B')
014900070907     D F12             C                   CONST(X'3C')
015000070907     D F13             C                   CONST(X'B1')
015100070907     D F14             C                   CONST(X'B2')
015200070907     D F15             C                   CONST(X'B3')
015300070907     D F16             C                   CONST(X'B4')
015400070907     D F17             C                   CONST(X'B5')
015500070907     D F18             C                   CONST(X'B6')
015600070907     D F19             C                   CONST(X'B7')
015700070907     D F20             C                   CONST(X'B8')
015800070907     D F21             C                   CONST(X'B9')
015900070907     D F22             C                   CONST(X'BA')
016000070907     D F23             C                   CONST(X'BB')
016100070907     D F24             C                   CONST(X'BC')
016200070907     D ENTER           C                   CONST(X'F1')
016300070907     D ROLDWN          C                   CONST(X'F4')
016400070907     D ROLLUP          C                   CONST(X'F5')
016500070907     D*----------------------------------------------------*
016600070907     D* Sottotitolo di testata
016700160208R195 X*SOTTITD1        C                   CONST('SDGMSG    *LIBL-
016800160208 "   X****                                      SDC0058')
016900160208 "   X*SOTTITD2        C                   CONST('SDGMSG    *LIBL-
017000160208R195 X****                                      SDC0073')
017001160208R195 D* "Spesometro"
017002160208 "   D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
017003160208 "   D                                          SDC0087')
017004160208 "   D* "Black List"
017005160208 "   D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
017006160208 "   D                                          SDC0076')
017007160208 "   D* "Acquisti da San Marino"
017008160208 "   D SOTTITT4        C                   CONST('SDGMSG    *LIBL-
017009160208R195 D                                          SDC0075')
017100070907     D*----------------------------------------------------*
017200070907     D* Variabili per gestione videate
017300070907     D $Gest           S              2A
017400070907     D $Fine           S              1A
017500070907     D $InzDX          S              1A
017600070907     D $InzD1          S              1A
017700070907     D $LastG          S              2A
017800070907     D $LastV          S              2A
017900070907     D*----------------------------------------------------*
018000070907     D* Variabili appoggio sempre presenti in tutte le anagrafiche
018100070907     D CurR            S              2S 0
018200070907     D CurC            S              2S 0
018300070907     D Win             S             99A
018400070907     D*----------------------------------------------------*
018500070907     D* Variabili appoggio specifiche della singola anagrafica
018600070907     D*----------------------------------------------------*
018700070907     D*  Indici
018800070907     D X               S              3S 0
018900070907     D Y               S              3S 0
019000070907     D*----------------------------------------------------*
019100070907     D* Indice della schiera tasti funzione
019200070907     D PosFun          S              2  0
019300070907     D*----------------------------------------------------*
019400130909     D* OpzG62 usato in parzializzazione
019500130909     D* e WOpzG62 valorizzato dalla parzializzazione per manutenzione
019600130909     D WOpzG62         S                   like(OpzG62)
019700070907     D*----------------------------------------------------*
019800070907     D $RecOk          S              1a
019900070907     D $RecFound       S              1a
020000070907     D $RecAlloc       S              1a
020100070907     D*----------------------------------------------------*
020200130909     D* Nr rec SDGETE00F
020300070907     D  RrnPfDs        DS           512                                         *
020400070907     D   RrnPf               397    400B 0                                      *
020500100730     D*------------------
020600130909     D* Tipo record "A"
020700100730     D*------------------
020800130909     D TpRecADS        DS
020900111013     D* prima mappa
021000130911     D  GA001010
021100130911     D  GA00101
021200130911     D  GA00201
021300130911     D  GA00301
021400130911     D  GA00401
021500130911     D  GA00701
021600130911     D  GA00702
021700130911     D  GA00703
021800130911     D  GA00704
021900130911     D  GA00705
022000130911     D  GA00706
022100130911     D  GA00707
022200130911     D  GA00801
022300130911     D  GA00901
022400130911     D  GA01001
022500130911     D  GA01002
022600110811     D*----------
022700111013     D* seconda mappa
022800130911     D  GA01101
022900130911     D  GA01102
023000130911     D  GA01103
023100130911     D  GA01104
023200130911     D  GA01105
023300130911     D  GA01106
023400130911     D  GA01107
023500130911     D  GA01108
023600130911     D  GA01119
023700130911     D  GA01110
023800130911R078 D  GA01111
023900130911R078 D  GA01112
024000130911R078 D  GA01113
024100130911R078 D  GA01114
024200130911R078 D  GA01115
024300130912     D*----------
024400130912     D* terza mappa
024500130912R078 D  GA01116
024600130912R078 D  GA01201
024700130912R078 D  GA01202
024800130912R078 D  GA01203
024900130912R078 D  GA01301
025000130912R078 D  GA01401
025100130911     D*------------------
025200130911     D* Tipo record "B"
025300130911     D*------------------
025400130911     D TpRecBDS        DS
025500130911     D* prima mappa
025600130911     D  GB001010
025700130911     D  GB00101
025800130911     D  GB00102
025900130911     D  GB00201
026000130911     D  GB00301
026100130911     D  GB00401
026200130911     D  GB00402
026300130911     D  GB00403
026400130911     D  GB00501
026500130911     D  GB00502
026600130911     D  GB00601
026700130911     D  GB00602
026800130911     D  GB00701
026900130911     D  GB00702
027000130911     D  GB00703
027100130911     D  GB00704
027200130911     D  GB00705
027300130911     D  GB00706
027400130911     D  GB00707
027500130911     D  GB00708
027600130911     D  GB00709
027700130911     D  GB00710
027800130911     D  GB00711
027900130911     D  GB00801
028000130911     D  GB00802
028100130911     D  GB00803
028200130911     D  GB00804
028300130911     D  GB00805
028400130911     D  GB00806
028500130911     D* seconda mappa
028600130911     D  GB00901
028700130911     D  GB00902
028800130911     D  GB00903
028900130911     D  GB00904
029000130911     D  GB01001
029100130911     D  GB01101
029200130911     D  GB01201
029300130911     D  GB01301
029400130911     D  GB01302
029500130911R078 D  GB01401
029600130911R078 D  GB01402
029700130911R078 D  GB01403
029800130911R078 D  GB01404
029900130911R078 D  GB01501
030000130911R078 D  GB01502
030100130911     D  GB01503
030200130911     D  GB01601
030300130911     D  GB01602
030400130911     D  GB01603
030500130911     D  GB01701
030600130911     D* terza mappa
030700130911     D  GB01802
030800130911     D  GB01803
030900130911     D  GB01804
031000130911     D  GB01805
031100130911     D  GB01806
031200130911     D  GB01807
031300130911     D  GB01808
031400130911     D  GB01809
031500100730     D*-----------------
031600130909     D* Tipo record "Z"
031700100730     D*-----------------
031800130909     D TpRecZDS        DS
031900111013     D* prima mappa
032000130911     D  GZ00101
032100130911     D  GZ00102
032200130911     D  GZ00201
032300130911     D  GZ00301
032400130911     D  GZ00401
032500130911     D  GZ00501
032600130911     D  GZ00601
032700130911     D  GZ00701
032800130911     D  GZ00801
032900130911     D*-----------------
033000130911     D* Tipo record "E"
033100130911     D*-----------------
033200130911     D TpRecEDS        DS
033300130911     D* prima mappa
033400130911     D  GE001010
033500130911     D  GE00101
033600130911     D  GE00201
033700130911     D  GE00301
033800130911     D  GE00401
033900130911     D  GE00501
034000130911     D  GE00601
034100130911     D  GE00701
034200130911     D  GE00801
034300130911     D  GE00901
034400130911     D  GE01001
034500130911     D  GE01101
034600130911     D  GE01201
034700130911     D  GE01301
034800100730     D*------------------
034900070907     D*----------------------------------------------------------------
035000070907     D* Ds contenente i valori del msg PRO1797
035100070907     D §Fil_Lib        DS
035200070907     D  §File                        10A
035300070907     D  §Libreria                    10A
035400070907     C******************************************************
035500070907     C*                MAIN LINE PROGRAM                   *
035600070907     C******************************************************
035700070907     C*
035800070907     C* inizializzazione variabili
035900070907     C                   EXSR      INZVAR
036000070907     C*
036100070907     C     $FINE         DOWEQ     *OFF
036200070907     C     $GEST         CASEQ     'D1'          GESD1
036300070907     C     $GEST         CASEQ     'DX'          GESDX
036400100730     C     $GEST         CASEQ     'A1'          GESDA1
036500100730     C     $GEST         CASEQ     'A2'          GESDA2
036600130912     C     $GEST         CASEQ     'A3'          GESDA3
036700100730     C     $GEST         CASEQ     'B1'          GESDB1
036800100730     C     $GEST         CASEQ     'B2'          GESDB2
036900130912     C     $GEST         CASEQ     'B3'          GESDB3
037000130912     C     $GEST         CASEQ     'E1'          GESDE1
037100130912     C     $GEST         CASEQ     'Z1'          GESDZ1
037200070907     C                   ENDCS
037300070907     C                   ENDDO
037400070907     C*
037500070907     C* fine programma
037600070907     C                   EXSR      ENDPGM
037700070907     C************************************************************
037800070907     C* Fine Programma
037900070907     C************************************************************
038000070907     C     ENDPGM        BEGSR
038100070907     C*
038200070907     C* disallocazione rcd
038300130909     C                   UNLOCK    SDGETE00F                            22
038400070907     C*
038500070907     C* passaggio dati al pgm chiamante
038600130909     C                   MoveL     SDGSG62DS     KPJBU
038700070907     C*
038800070907     C* Se pgm Guida non è stato richiamato posso chiudere in LR
038900130909     C                   if        OpzG62 = *blank
039000070907     C                   eval      *inLR = *on
039100070907     C* altrimenti chiudo in RT
039200070907     C                   else
039300070907     C                   eval      *inRT = *on
039400070907     C                   endif
039500070907     C*
039600070907     C                   RETURN
039700070907     C*
039800070907     C*
039900070907     C     XEndPgm       ENDSR
040000070907     C************************************************************
040100070907     C* GESTIONE EMISSIONE TASTI FUNZIONE
040200070907     C************************************************************
040300070907     C     GESPIE        BEGSR
040400070907     C*
040500070907     C                   SELECT
040600070907     C*
040700070907     C     $GEST         WHENEQ    'D1'
040800070907     C     $ULKD1        IFEQ      0
040900070907     C                   EXSR      F24D1
041000070907     C                   ELSE
041100070907     C                   WRITE     FMTZ1
041200070907     C                   ENDIF
041300070907     C*
041400070907     C*
041500100803     C     $GEST         WHENEQ    'A1'
041600100803     C     $GEST         oreq      'A2'
041700100803     C     $GEST         oreq      'A3'
041800100803     C     $GEST         oreq      'B1'
041900100803     C     $GEST         oreq      'B2'
042000100803     C     $GEST         oreq      'B3'
042100130912     C     $GEST         oreq      'E1'
042200100803     C     $GEST         oreq      'Z1'
042300100803     C     $GEST         oreq      'Z2'
042400100803     C     $GEST         oreq      'Z3'
042500070907     C     $ULKDx        IFEQ      0
042600070907     C                   EXSR      F24Dx
042700070907     C                   ELSE
042800070907     C                   WRITE     FMTZ2
042900070907     C                   ENDIF
043000070907     C*
043100070907     C                   ENDSL
043200070907     C*
043300070907     C     XGesPie       ENDSR
043400070907     C************************************************************
043500070907     C* GESTIONE PARZIALIZZAZIONI
043600070907     C************************************************************
043700070907     C     GESD1         BEGSR
043800070907     C* inizializzazione videata
043900070907     C     $INZD1        IFEQ      *ON
044000070907     C                   EXSR      INZD1
044100070907     C                   Move      *OFF          $INZD1
044200070907     C                   ENDIF
044300070907     C* emissione piede videata se proveniente da altra
044400070907     C* salvataggio valore formato di provenienza
044500070907     C     $LASTV        IFNE      'D1'
044600070907     C                   WRITE     FMTT1
044700070907     C                   EXSR      GESPIE
044800070907     C                   Move      $LASTV        $LASTG
044900070907     C                   Move      'D1'          $LASTV
045000070907     C                   ENDIF
045100070907     C*
045200070907     C* se esistono errori sulla videata
045300070907     C* emetto la write del formato a indicatori spenti per vedere
045400070907     C* le eventuali decodifiche
045500070907     C     *IN99         IFEQ      *ON
045600070907     C                   MoveA     *IN           WIN
045700070907     C                   Move      *ALL'0'       IN5098           49
045800070907     C                   MoveA     IN5098        *IN(50)
045900070907     C                   WRITE     FMTD1
046000070907     C                   MoveA     WIN           *IN
046100070907     C                   ENDIF
046200070907     C*
046300070907     C                   EXFMT     FMTD1
046400070907     C*
046500070907     C* azzero la posizione del cursore
046600070907     C                   Z-ADD     0             H1RIGA
046700070907     C                   Z-ADD     0             H1COLO
046800070907     C*
046900070907     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
047000070907     C* INPUT
047100070907     C* $FC1 = Abilitazioni videata D1
047200070907     C* OUTPUT
047300070907     C* *IN97 + *IN99 Errori
047400070907     C                   MoveA     $FC1          $FC
047500070907     C                   EXSR      CTRKD1
047600070907     C*
047700070907     C     *IN99         IFEQ      *OFF
047800070907     C*
047900070907     C                   SELECT
048000070907     C* F2=Decodifica
048100070907     C     $TASTO        WHENEQ    F02
048200070907     C                   EXSR      F02D1
048300070907     C* F3=Fine
048400070907     C     $TASTO        WHENEQ    F03
048500070907     C                   EXSR      F03D1
048600070907     C* F4=Ricerca
048700070907     C     $TASTO        WHENEQ    F04
048800070907     C                   EXSR      SEARCH
048900070907     C* F5=Refresh
049000070907     C     $TASTO        WHENEQ    F05
049100070907     C                   EXSR      F05D1
049200070907     C* F12=Ritorno
049300070907     C     $TASTO        WHENEQ    F12
049400070907     C                   EXSR      F12D1
049500070907     C* F24=Altri tasti
049600070907     C     $TASTO        WHENEQ    F24
049700070907     C                   EXSR      F24D1
049800070907     C*
049900070907     C                   OTHER
050000070907     C                   EXSR      CTRD1
050100070907     C*
050200070907     C* richiamo dettaglio se non ci sono errori in parzializzazione
050300070907     C                   if        *in99 = *off
050400070907     C                   exsr      SetAna
050500070907     C                   exsr      GesDx
050600070907     C                   endif
050700070907     C*
050800070907     C                   ENDSL
050900070907     C*
051000070907     C                   ENDIF
051100070907     C*
051200070907     C     XGesD1        ENDSR
051300070907     C************************************************************
051400070907     C* RICERCHE
051500070907     C************************************************************
051600070907     C     SEARCH        BEGSR
051700070907     C*
051800070907     C* determino Riga/Colonna del cursore
051900070907     C                   Move      NRG           R$$
052000070907     C                   Move      NCL           C$$
052100070907     C                   Z-ADD     RIRI          CURR
052200070907     C                   Z-ADD     COCO          CURC
052300070907     C*
052400070907     C*
052500070907     C                   SELECT
052600070907     C*
052700070907    >C*--------------------------------------------------------------
052800070907    >C* inserire le routine di ricerca
052900070907    >C* RICORDATE: per tutti i campi che in certe condizioni risultano PR,
053000070907    >C* NON deve essere possibile richiamare i rispettivi pgm in ricerca,
053100070907    >C* SEMMAI richiamarli in visualizzazione (opzione=05)
053200070907    >C*--------------------------------------------------------------
053300070907     C*
053400070907     C* FMTD1  - file su supporto magnetico
053500070907     C     H1NMFL        WHENEQ    'D1FILE'
053600070907     C                   Reset                   ANA122DS
053700070907     C                   Move      '01'          Opz122
053800070907     C                   Move      XScSoc        Societa122
053900070907     C                   MoveL     XScSoc        Soc122
054000131119R132 C*
054100160208R132 C                   Select
054101160208R195 C*
054102160208 "   C* se Black List Mensile/trimestrale
054200160208R195 C                   When      TpComG62    = 'B'
054300140401R132 C                   Move      'BP'          TpScar122
054301160208R195 C*
054302160208 "   C* se Black List annuale
054303160208R195 C                   When      TpComG62    = 'L'
054304160208R132 C                   Move      'BA'          TpScar122
054305160208R195 C*
054306160208R195 C* se Acquisti da San Marino
054400140401R133 C                   When      TpComG62    = 'S'
054500140401R133 C                   Move      'SM'          TpScar122
054501160208R195 C*
054600131119R132 C                   Other
054601160208R195 C* se Spesometro
054700110824     C                   Move      'OP'          TpScar122
054800131119R132 C                   endSl
054900131119R132 C*
055000070907     C                   Clear                   KPJBU
055100070907     C                   Movel     ANA122DS      KPJBU
055200070907     C                   Call      'ANA122R'
055300070907     C                   Parm                    KPJBA
055400070907     C                   Movel     KPJBU         ANA122DS
055500070907     C*
055600070907     C                   if        File122  <>   *Blanks
055700070907     C                   MoveL     File122       D1File
055800070907     C                   endif
055900070907     C*
056000070907     C* ricerca non abilitata in questa posizione
056100070907   X1C                   OTHER
056200070907     C                   SETON                                        9899
056300070907     C*
056400070907   E1C                   ENDSL
056500070907     C*
056600070907     C* imposto pos. del cursore
056700070907     C                   Z-ADD     CURR          H1RIGA
056800070907     C                   Z-ADD     CURC          H1COLO
056900070907     C*
057000070907     C                   Move      *HIVAL        $LASTV
057100070907     C*
057200070907     C                   ENDSR
057300070907     C/EJECT
057400070907     C************************************************************
057500070907     C* Carico e passo alle videate da manutenere
057600070907     C************************************************************
057700111220     C     GesDx         BEGSR
057800070907     C*
057900130909     C                   Move      '02'          WOpzG62
058000070907     C*
058100070907     C                   Move      *on           $InzDx
058200070907     C                   exsr      InzDx
058300070907     C                   Move      *off          $InzDx
058400070907     C*
058500070907     C*  se reperito rec richiesto
058600070907     C                   if        $RecOk      = *On
058700070907     C*
058800070907     C*  passo a dettaglio richiesto
058900070907     C                   select
059000130912     C                   when      D1TpRec='A'
059100100730     C                   Move      'A1'          $Gest
059200130912     C                   when      D1TpRec='B'
059300100730     C                   Move      'B1'          $Gest
059400130912     C                   when      D1TpRec='E'
059500130912     C                   Move      'E1'          $Gest
059600130912     C                   when      D1TpRec='Z'
059700130912     C                   Move      'Z1'          $Gest
059800070907     C                   endsl
059900070907     C*
060000070907     C                   else
060100070907     C                   Move      'D1'          $Gest
060200070907     C*
060300070907     C                   endif
060400070907     C*
060500070907     C     XGesDx        ENDSR
060600070907     C************************************************************
060700070907     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
060800070907     C************************************************************
060900070907     C     INZD1         BEGSR
061000070907     C*
061100070907     C                   CLEAR                   FMTD1
061200160208R195 X*ex R149           Select
061300160208 "   X*ex R149           When      TPCOMG62    = 'S'
061400160208 "   X*ex R149           Eval      *In11       = *On
061500160208 "   X*ex R149           Other
061600160208 "   X*ex R149           Eval      *In11       = *Off
061700160208 "   X*ex R149           endSl
061800160208R195 X*ex R149
061801160208R195 C*
061802160208 "   C                   Select
061803160208 "   C*
061804160208 "   C* Black list Polivalente ANTE 2016
061805160208 "   C                   When      TpComG62    = 'B'
061806160208 "   C                   Eval      *In11       = *On
061811160208 "   C*
061812160208 "   C                   Other
061813160208 "   C* Spesometro
061814160208 "   C* o Black list annuale da 2016 o Acq.da San Marino
061815160208 "   C                   Eval      *In11       = *Off
061817160208 "   C*
061818160208R195 C                   endSl
061900070907     C*
062000070907     C* Se pgm  richiamato con parametri forniti , li imposto nelle
062100070907     C* parzializzazioni
062200070907     C                   select
062300070907     C* il pgm è richiamato e
062400070907     C* sono stati passati dei valori di parzializzazione ma non
062500070907     C* la chiave univoca
062600130909     C                   when      OpzG62 <> *blank
062700070907     C* imposto i valori passati sui campi a video
062800130909     C                   Move      TpReCG62      D1TpRec
062900070907     C* il pgm non è richiamato
063000130909     C                   when      OpzG62 =  *blank
063100070907     C* imposto dei valori di default
063200070907     C                   Move      *blank        D1TpRec
063300070907     C                   endsl
063400070907     C*
063500070907     C     XInzD1        ENDSR
063600070907     C************************************************************
063700070907     C* GESTIONE F02 VIDEO D1
063800070907     C************************************************************
063900070907     C     F02D1         BEGSR
064000070907     C*
064100070907     C* per decodificare la videata richiamo la routine dei ctrl
064200070907     C* e poi disattivo gli eventuali messaggi di errore
064300070907     C                   EXSR      CTRD1
064400070907     C*
064500070907     C                   Move      *ALL'0'       IN5098
064600070907     C                   MoveA     IN5098        *IN(50)
064700070907     C*
064800070907     C     XF02D1        ENDSR
064900070907     C************************************************************
065000070907     C* GESTIONE F03 VIDEO D1
065100070907     C************************************************************
065200070907     C     F03D1         BEGSR
065300070907     C*
065400070907     C                   Move      *ON           $FINE
065500130909     C                   Move      '1'           RETG62
065600070907     C* fine programma
065700070907     C                   EXSR      ENDPGM
065800070907     C*
065900070907     C     XF03D1        ENDSR
066000070907     C************************************************************
066100070907     C* GESTIONE F05 VIDEO D1
066200070907     C************************************************************
066300070907     C     F05D1         BEGSR
066400070907     C*
066500070907     C                   Move      *ON           $INZD1
066600070907     C*
066700070907     C     XF05D1        ENDSR
066800070907     C************************************************************
066900070907     C* GESTIONE F12 VIDEO D1
067000070907     C************************************************************
067100070907     C     F12D1         BEGSR
067200070907     C*
067300070907     C*  torno a menu
067400070907     C                   Move      *ON           $FINE
067500130909     C                   Move      '2'           RETG62
067600070907     C*
067700070907     C     XF12D1        ENDSR
067800070907     C************************************************************
067900070907     C* GESTIONE F24 VIDEO D1
068000070907     C* input
068100070907     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
068200070907     C* $FX1 =Testi tasti funzione
068300070907     C* $FC1 = Abilitazioni
068400070907     C* $FR1 = Inversioni di fondo
068500070907     C************************************************************
068600070907     C     F24D1         BEGSR
068700070907     C*
068800070907     C                   MoveA     $FX1          $FX
068900070907     C                   MoveA     $FC1          $FC
069000070907     C                   MoveA     $FR1          $FR
069100070907     C                   Z-ADD     $ULKD1        $ULK
069200070907     C                   Z-ADD     $D1L01        $L01
069300070907     C                   Z-ADD     $D1L02        $L02
069400070907     C                   EXSR      RIEKEY
069500070907     C                   Z-ADD     $ULK          $ULKD1            3 0
069600070907     C                   MoveA     $FC           $FC1
069700070907     C                   MoveL     $KEY1         Z1KE1
069800070907     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
069900070907     C                   MoveL     $KEY2         Z1KE2
070000070907     C                   WRITE     FMTZ1
070100070907     C*
070200070907     C     XF24D1        ENDSR
070300070907     C************************************************************
070400070907     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
070500070907     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
070600070907     C* 93 94 96 97 98
070700070907     C************************************************************
070800070907     C     CTRD1         BEGSR
070900070907     C*
071000070907     C                   SETOFF                                       99
071100070907     C                   if        D1Tprec = *blank
071200070907     C                   eval      *in99 = *on
071300070907     C                   endif
071400070907     C*
071500070907     C* file da trasmettere
071600070907     C                   EXSR      CtrFile
071700070907     C*
071800070907     C     XCtrD1        ENDSR
071900070907     C***********************************************************
072000070907     C* Controllo file da trasmettere
072100070907     C***********************************************************
072200070907     C     CtrFile       BEGSR
072300070907     C*
072400070907     C                   if        D1File     = *Blank
072500070907     C                   SetOn                                        5199
072600070907     C                   else
072700070907     C* verifico se esiste, nella libreria
072800070907     C* dei file per lo scarico su supporto magnetico,
072900070907     C* il file indicato a video
073000100730     C                   EXSR      ChkFile
073100070907     C*
073200070907     C                   endif
073300070907     C*
073400070907     C                   ENDSR
073500070907     C/EJECT
073600070907     C************************************************************
073700100730     C* Verifico l'esistenza del file
073800070907     C************************************************************
073900100730     C     ChkFile       BEGSR
074000070907     C*
074100130909     C                   CLOSE     SDGETE00F
074200070907     C*
074300070907     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
074400070907     C                   Eval      Cmd         =
074500070907     C                             'CHKOBJ     OBJ('        +
074600070907     C                             %Trim(Lib022) + '/' +
074700070907     C                             %Trim(D1File) +
074800070907     C                             ') OBJTYPE(*FILE)'
074900070907     C*
075000070907     C                   Call      'QCMDEXC'                            21
075100070907     C                   Parm                    Cmd             120
075200070907     C                   Parm      120           Len              15 5
075300070907     C*
075400070907     C                   if        *In21       = *On
075500070907     C*
075600070907     C* Errore: dico che non il file non è stato trovato nella libreria
075700070907     C* Valorizzo le variabili del msg PRO1797
075800070907     C                   Move      D1File        §File
075900070907     C                   Move      Lib022        §Libreria
076000070907     C                   Move      §Fil_Lib      P1Fil_Lib
076100070907     C                   SetOn                                        5099
076200070907     C*
076300070907     C                   else
076400070907     C*
076500070907     C* sovrascrivo il membro con il nome del file
076600100730     C                   EXSR      OvrFile
076700130909     C                   OPEN      SDGETE00F
076800070907     C                   endif
076900070907     C*
077000070907     C*
077100070907     C                   ENDSR
077200070907     C/Eject
077300070907     C************************************************************
077400070907     C* Sovrascrivo il membro con il nome del file
077500070907     C************************************************************
077600100730     C     OvrFile       BEGSR
077700070907     C*
077800070907     C                   Eval      Cmd         =
077900130909     C                             'OVRDBF     FILE(SDGETE00F) '+
078000070907     C                             'TOFILE('                    +
078100070907     C                             %Trim(Lib022) + '/' +
078200070907     C                             %Trim(D1File) +
078300070907     C                             ')'
078400070907     C*
078500070907     C                   Call      'QCMDEXC'                            21
078600070907     C                   Parm                    Cmd             120
078700070907     C                   Parm      120           Len              15 5
078800070907     C*
078900070907     C                   if        *In21       = *On
079000130909     C                   Move      '1'           ERRG62
079100070907     C                   Move      *On           $Fine
079200070907     C                   EXSR      ENDPGM
079300070907     C                   endif
079400070907     C*
079500070907     C                   ENDSR
079600070907     C/Eject
079700070907     C************************************************************
079800070907     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
079900070907     C************************************************************
080000070907     C     CTRKD1        BEGSR
080100070907     C*
080200070907     C                   SETOFF                                       99
080300070907     C                   MoveA     $FC1          $FC
080400070907     C                   EXSR      CTRKEY
080500070907     C     *IN99         IFEQ      *ON
080600070907     C                   SETON                                        97
080700070907     C                   ENDIF
080800070907     C*
080900070907     C     XCtrKD1       ENDSR
081000070907     C************************************************************
081100070907     C* Chiamata programma con tasto personalizzato
081200070907     C************************************************************
081300070907     C     FPerDx        BEGSR
081400070907     C*
081500070907     C* salvo la DS di ingresso
081600130909     C                   MoveL     SDGSG62DS     $SaveDS
081700070907     C*
081800130909     C                   Move      xscsoc        SocietaG62
081900130909     C* passare nei campi appositi della DS STDG62DS il valore
082000070907     C* della chiave della VL univoca
082100130909     C                   MoveL     D1TpRec       TpReCG62
082200070907     C*
082300130909     C                   MoveL     SDGSG62DS     KPJBU
082400070907     C                   CALL      $PGF1(PosFun)                        22
082500070907     C                   PARM                    KPJBA
082600100730     C***                PARM                    NOMPGM
082700070907     C*
082800070907     C* ripristino la DS di ingresso
082900130909     C                   MoveL     $SaveDS       SDGSG62DS
083000070907     C*
083100070907     C     XFPerDx       ENDSR
083200070907     C************************************************************
083300070907     C* CONTROLLO ABILITAZIONE TASTI VIDEATE
083400070907     C************************************************************
083500070907     C     CTRKDx        BEGSR
083600070907     C*
083700070907     C                   SETOFF                                       99
083800070907     C                   MoveA     $FC2          $FC
083900070907     C                   EXSR      CTRKEY
084000070907     C     *IN99         IFEQ      *ON
084100070907     C                   SETON                                        97
084200070907     C                   ENDIF
084300070907     C*
084400070907     C     XCtrKDx       ENDSR
084500070907     C************************************************************
084600070907     C* INIZIALIZZAZIONE VIDEATA DATI
084700070907     C************************************************************
084800070907     C     INZDx         BEGSR
084900070907     C*
085000070907     C* In base al tipo record scelto, utilizzo diverse videate:
085100070907     C                   select
085200100730     C                   when      D1TpRec='A'
085300130912     C                   clear                   FmtDA1
085400130912     C                   clear                   FmtDA2
085500130912     C                   clear                   FmtDA3
085600130912     C                   clear                   FmtDB1
085700130912     C                   clear                   FmtDB2
085800130912     C                   clear                   FmtDB3
085900130912     C                   clear                   FmtDZ1
086000130912     C                   clear                   FmtDZ1
086100070907     C                   endsl
086200070907     C*
086300070907     C* NON IMMISSIONE
086400130909     C                   if        WOpzG62 <> '01'
086500070907     C*
086600070907     C*  LETTURA
086700070907     C                   exsr      RedAna
086800070907     C*
086900070907     C*  DOPO LETTURA
087000070907     C                   select
087100070907     C*   NON TROVATO tipo rec richiesto oppure allocato
087200070907     C                   when      $RecOk = *off
087300070907     C                   if        $RecFound   = *Off    or
087400070907     C                             $RecAlloc   = *On
087500070907     C                   Move      'D1'          $GEST
087600130909     C                   Move      '1'           ERRG62
087700070907     C*    errore da emettere in parzializzaione
087800070907     C*    PRO0007-(F1) La funzione richiamata si è chiusa in errore
087900070907     C                   eval      *in93 = *on
088000070907     C                   eval      *in99 = *on
088100070907     C                   endif
088200070907     C*
088300070907     C*   TROVATO tp rec da gestire
088400070907     C                   when      $RecOk = *on
088500070907     C*
088600070907     C*    riempio videata tp rec richiesto
088700070907     C*    e richiamo ctrl per decodifica
088800070907     C                   select
088900130912     C                   when      D1TpRec='A'
089000130912     C                   eval      TPRECADS = ETEREC
089100100730     C                   exsr      CtrDA1
089200100730     C                   exsr      CtrDA2
089300130912     C                   exsr      CtrDA3
089400130912     C                   when      D1TpRec='B'
089500130912     C                   eval      TPRECBDS = ETEREC
089600100730     C                   exsr      CtrDB1
089700100730     C                   exsr      CtrDB2
089800130912     C                   exsr      CtrDB3
089900130912     C                   when      D1TpRec='E'
090000130912     C                   eval      TPRECEDS = ETEREC
090100130912     C                   exsr      CtrDE1
090200130912     C                   when      D1TpRec='Z'
090300130912     C                   eval      TPRECZDS = ETEREC
090400130912     C                   exsr      CtrDZ1
090500130912     C                   endsl
090600070907     C                   Move      *all'0'       In5098
090700070907     C                   Movea     In5098        *in(50)
090800070907     C*
090900070907     C                   endsl
091000070907     C*
091100070907     C                   endif
091200070907     C*
091300070907     C     XInzDX        ENDSR
091400070907     C************************************************************
091500070907     C* posizionamento sul file
091600070907     C************************************************************
091700070907     C     SetAna        BEGSR
091800070907     C*
091900070907     C                   eval      $RecFound   = *Off
092000070907     C*
092100130909     C     1             setll     SDGETE00F
092200070907     C*
092300070907     C                   ENDSR
092400070907     C************************************************************
092500070907     C* Gegstione elttura rec da caricare per modifica
092600070907     C************************************************************
092700070907     C     RedAna        BEGSR
092800070907     C*
092900070907     C* REPERISCO NRR DA LEGGERE
093000070907     C*  cerco il n°relativo rec contenente il tipo rec richiesto
093100070907     C                   eval      $RecOk = *off
093200070907     C                   eval      $RecAlloc   = *Off
093300070907     C*
093400130909     C                   read(n)   SDGETE00F                              21
093500070907     C                   dow       *in21 = *off
093600070907     C*
093700130909     C                   if        %subst(ETEREC: 1 : 1)=D1TpRec
093800070907     C                   eval      $RecOk = *on
093900070907     C                   eval      $RecFound   = *On
094000070907     C                   leave
094100070907     C                   endif
094200070907     C*
094300130909     C                   read(n)   SDGETE00F                              21
094400070907     C                   enddo
094500070907     C*
094600070907     C*
094700070907     C* LETTURA
094800070907     C*  trovato tipo rec richiesto
094900070907     C                   if        $RecOk = *on
095000070907     C*
095100070907     C*  provo a leggere il rec fino a quando il rec non è allocato
095200070907     C*  oppure l'utente vuole Ritentare a leggere il rec allocato
095300070907     C                   dou       *in22 = *off
095400070907     C                             or MsgRtn <> 'R'
095500070907     C*  alloco se Modifica/Annullamento
095600130909    >C                   if        WOpzG62='02' or WOpzG62='04'
095700130909     C     RrnPf         chain     SDGETE00F                          2122
095800070907     C*  NON alloco se Copia/Visualizzazione
095900070907     C                   else
096000130909     C     RrnPf         chain(n)  SDGETE00F                          2122
096100070907     C                   endif
096200070907     C*
096300070907     C*  Gestione RCD allocato
096400070907     C                   if        *in22 = *on
096500070907     C                   eval      $RecAlloc   = *On
096600130909    >C                   Movel     'SDGETE00F'   MsgFil
096700070907     C                   exsr      MsgPgm
096800070907     C                   endif
096900070907     C*
097000070907     C                   enddo
097100070907     C*
097200070907     C* rcd allocato da altro pgm: memorizzo errore
097300070907     C                   if        *in22 = *on
097400070907     C                   eval      $RecOk = *off
097500070907     C                   endif
097600070907     C*
097700070907     C                   endif
097800070907     C*
097900070907     C     XRedAna       ENDSR
098000070907     C************************************************************
098100070907     C* GESTIONE F24 VIDEI
098200070907     C************************************************************
098300070907     C     F24Dx         BEGSR
098400070907     C*
098500070907     C                   MoveA     $FX2          $FX
098600070907     C                   MoveA     $FC2          $FC
098700070907     C                   MoveA     $FR2          $FR
098800070907     C                   Z-ADD     $ULKDx        $ULK
098900070907     C                   Z-ADD     $DxL01        $L01
099000070907     C                   Z-ADD     $DxL02        $L02
099100070907     C                   EXSR      RIEKEY
099200070907     C                   Z-ADD     $ULK          $ULKDx            3 0
099300070907     C                   MoveA     $FC           $FC2
099400070907     C                   MoveL     $KEY1         Z2KE1
099500070907     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
099600070907     C                   MoveL     $KEY2         Z2KE2
099700070907     C                   WRITE     FMTZ2
099800070907     C*
099900070907     C     XF24Dx        ENDSR
100000070907     C************************************************************
100100070907     C* GESTIONE VIDEO RECORD D01
100200070907     C************************************************************
100300100730     C     GESDA1        BEGSR
100400070907     C* inizializzazione videata
100500070907     C     $INZDX        IFEQ      *ON
100600070907     C                   EXSR      INZDX
100700070907     C                   Move      *OFF          $INZDX
100800070907     C                   ENDIF
100900070907     C* emissione piede videata se proveniente da altra
101000070907     C* salvataggio valore formato di provenienza
101100070907     C     $LASTV        IFNE      '01'
101200070907     C                   WRITE     FMTT1
101300070907     C                   EXSR      GESPIE
101400070907     C                   Move      $LASTV        $LASTG
101500070907     C                   Move      '01'          $LASTV
101600070907     C                   ENDIF
101700070907     C*
101800070907     C* se esistono errori sulla videata
101900070907     C* emetto la write del formato a indicatori spenti per vedere
102000070907     C* le eventuali decodifiche
102100070907     C     *IN99         IFEQ      *ON
102200100803     C     $GEST         ANDEQ     'A1'
102300070907     C                   MoveA     *IN           WIN
102400070907     C                   Move      *ALL'0'       IN5098           49
102500070907     C                   MoveA     IN5098        *IN(50)
102600130912     C                   WRITE     FMTDA1
102700070907     C                   MoveA     WIN           *IN
102800070907     C                   ENDIF
102900070907     C* Inizializzazione indice tasti funzione personalizzati
103000070907     C                   Eval      PosFun = 25
103100070907     C*
103200070907     C                   SELECT
103300070907     C* Annullamento
103400130909     C     WOpzG62       WHENEQ    '04'
103500070907     C* Visualizzazione
103600130909     C     WOpzG62       OREQ      '05'
103700130912     C                   WRITE     FMTDA1
103800070907     C                   EXFMT     PROTECT
103900070907     C* Immissione/Modifica/Copia
104000070907     C                   OTHER
104100130912     C                   EXFMT     FMTDA1
104200070907     C                   ENDSL
104300070907     C*
104400070907     C* azzero la posizione del cursore
104500070907     C                   Z-ADD     0             H1RIGA
104600070907     C                   Z-ADD     0             H1COLO
104700070907     C*
104800070907     C* controllo abilitazione tasti funzione
104900070907     C                   EXSR      CTRKDx
105000070907     C*
105100070907     C     *IN99         IFEQ      *OFF
105200070907     C*
105300070907     C                   SELECT
105400070907     C* ROLLUP
105500070907     C     $TASTO        WHENEQ    ROLLUP
105600100803     C                   Move      'A2'          $GEST
105700070907     C* F3=Fine
105800070907     C     $TASTO        WHENEQ    F03
105900070907     C                   EXSR      F03D01
106000070907     C* F5=Refresh
106100070907     C     $TASTO        WHENEQ    F05
106200070907     C                   EXSR      F05D01
106300070907     C* F8=Successivo
106400070907     C     $TASTO        WHENEQ    F08
106500070907     C                   EXSR      F08D01
106600070907     C* F12=Ritorno
106700070907     C     $TASTO        WHENEQ    F12
106800070907     C                   EXSR      F12D01
106900070907     C* F24=Altri tasti
107000070907     C     $TASTO        WHENEQ    F24
107100070907     C                   EXSR      F24Dx
107200070907     C* Personalizzata
107300070907     C     $PGF1(PosFun) WHENNE    *BLANK
107400070907     C                   EXSR      FPerDx
107500070907     C*
107600070907     C                   OTHER
107700100730     C                   EXSR      CtrDA1
107800070907     C     *IN99         IFEQ      *OFF
107900100730     C                   EXSR      CtrDA2
108000130912     C                   EXSR      CtrDA3
108100070907     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
108200070907     C* indicatori uguali in videate differenti, genererei conflitti
108300070907     C     *IN99         IFEQ      *ON
108400070907     C                   Move      *ALL'0'       IN5098           49
108500070907     C                   MoveA     IN5098        *IN(50)
108600070907     C                   SETON                                        9996
108700070907     C                   ENDIF
108800070907     C                   ENDIF
108900070907     C*
109000070907     C* eseguo aggiornamento
109100070907     C     $TASTO        IFEQ      F06
109200070907     C     *IN99         ANDEQ     *OFF
109300070907     C                   EXSR      AGGANA
109400070907     C                   ENDIF
109500070907     C*
109600070907     C                   ENDSL
109700070907     C*
109800070907     C                   ENDIF
109900070907     C*
110000100730     C     XGESDA1       ENDSR
110100070907     C************************************************************
110200070907     C* GESTIONE F03 VIDEO D01
110300070907     C************************************************************
110400070907     C     F03D01        BEGSR
110500070907     C*
110600070907     C                   Move      *ON           $FINE
110700130909     C                   Move      '1'           RETG62
110800070907     C* fine programma
110900070907     C                   EXSR      ENDPGM
111000070907     C*
111100070907     C     XF03D01       ENDSR
111200070907     C************************************************************
111300070907     C* GESTIONE F05 VIDEO D01
111400070907     C************************************************************
111500070907     C     F05D01        BEGSR
111600070907     C*
111700070907     C                   Move      *ON           $INZDX
111800070907     C*
111900070907     C     XF05D01       ENDSR
112000070907     C************************************************************
112100070907     C* GESTIONE F08 VIDEO D01
112200070907     C************************************************************
112300070907     C     F08D01        BEGSR
112400070907     C*
112500070907     C                   Move      'DX'          $GEST
112600130909    >C                   MOVE      '0'           RETG62
112700070907     C*
112800070907     C                   ENDSR
112900070907     C/EJECT
113000070907     C************************************************************
113100070907     C* GESTIONE F12 VIDEO D01
113200070907     C************************************************************
113300070907     C     F12D01        BEGSR
113400070907     C*
113500070907     C                   Move      'D1'          $GEST
113600070907     C*
113700070907     C     XF12D01       ENDSR
113800070907     C************************************************************
113900130912     C* CONTROLLO VIDEATA  DA1
114000070907     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
114100070907     C* 95 96 97 98
114200070907     C************************************************************
114300100730     C     CtrDA1        BEGSR
114400070907     C*
114500070907     C                   SETOFF                                       99
114600070907     C*
114700100730     C     XCtrDA1       ENDSR
114800070907     C************************************************************
114900130912     C* GESTIONE VIDEO RECORD  DA2
115000070907     C************************************************************
115100100730     C     GESDA2        BEGSR
115200070907     C* emissione piede videata se proveniente da altra
115300070907     C* salvataggio valore formato di provenienza
115400070907     C     $LASTV        IFNE      '02'
115500070907     C                   WRITE     FMTT1
115600070907     C                   EXSR      GESPIE
115700070907     C                   Move      $LASTV        $LASTG
115800070907     C                   Move      '02'          $LASTV
115900070907     C                   ENDIF
116000070907     C*
116100070907     C* se esistono errori sulla videata
116200070907     C* emetto la write del formato a indicatori spenti per vedere
116300070907     C* le eventuali decodifiche
116400070907     C     *IN99         IFEQ      *ON
116500100803     C     $GEST         ANDEQ     'A2'
116600070907     C                   MoveA     *IN           WIN
116700070907     C                   Move      *ALL'0'       IN5098           49
116800070907     C                   MoveA     IN5098        *IN(50)
116900130912     C                   WRITE     FMTDA2
117000070907     C                   MoveA     WIN           *IN
117100070907     C                   ENDIF
117200070907     C* Inizializzazione indice tasti funzione personalizzati
117300070907     C                   Eval      PosFun = 25
117400070907     C*
117500070907     C                   SELECT
117600070907     C* Annullamento
117700130909     C     WOpzG62       WHENEQ    '04'
117800070907     C* Visualizzazione
117900130909     C     WOpzG62       OREQ      '05'
118000130912     C                   WRITE     FMTDA2
118100070907     C                   EXFMT     PROTECT
118200070907     C* Immissione/Modifica/Copia
118300070907     C                   OTHER
118400130912     C                   EXFMT     FMTDA2
118500070907     C                   ENDSL
118600070907     C*
118700070907     C* azzero la posizione del cursore
118800070907     C                   Z-ADD     0             H1RIGA
118900070907     C                   Z-ADD     0             H1COLO
119000070907     C*
119100070907     C* controllo abilitazione tasti funzione
119200070907     C                   EXSR      CTRKDx
119300070907     C*
119400070907     C     *IN99         IFEQ      *OFF
119500070907     C                   SELECT
119600130912     C* ROLLUP
119700130912     C     $TASTO        WHENEQ    ROLLUP
119800130912     C                   Move      'A3'          $GEST
119900070907     C* ROLDOWN
120000070907     C     $TASTO        WHENEQ    ROLDWN
120100100803     C                   Move      'A1'          $GEST
120200070907     C* F3=Fine
120300070907     C     $TASTO        WHENEQ    F03
120400070907     C                   EXSR      F03D01
120500070907     C* F5=Refresh
120600070907     C     $TASTO        WHENEQ    F05
120700070907     C                   EXSR      F05D01
120800070907     C* F8=Successivo
120900070907     C     $TASTO        WHENEQ    F08
121000070907     C                   EXSR      F08D01
121100070907     C* F12=Ritorno
121200070907     C     $TASTO        WHENEQ    F12
121300070907     C                   EXSR      F12D01
121400070907     C* F24=Altri tasti
121500070907     C     $TASTO        WHENEQ    F24
121600070907     C                   EXSR      F24Dx
121700070907     C* Personalizzata
121800070907     C     $PGF1(PosFun) Whenne    *BLANK
121900070907     C                   EXSR      FperDx
122000070907     C*
122100070907     C                   OTHER
122200100730     C                   EXSR      CtrDA2
122300070907     C     *IN99         IFEQ      *OFF
122400100730     C                   EXSR      CtrDA1
122500130912     C                   EXSR      CtrDA3
122600070907     C* Spengo gli indicatori di errore per il controllo su altri schermi
122700070907     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
122800070907     C* videate
122900070907     C     *IN99         IFEQ      *ON
123000070907     C                   Move      *ALL'0'       IN5098           49
123100070907     C                   MoveA     IN5098        *IN(50)
123200070907     C                   SETON                                        9996
123300070907     C                   ENDIF
123400070907     C                   ENDIF
123500070907     C*
123600070907     C* eseguo aggiornamento
123700070907     C     $TASTO        IFEQ      F06
123800070907     C     *IN99         ANDEQ     *OFF
123900070907     C                   EXSR      AGGANA
124000070907     C                   ENDIF
124100070907     C*
124200070907     C                   ENDSL
124300070907     C*
124400070907     C                   ENDIF
124500070907     C*
124600100730     C     XGESDA2       ENDSR
124700130912     C************************************************************
124800130912     C* CONTROLLO VIDEATA  DA2
124900130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
125000130912     C* 95 96 97 98
125100130912     C************************************************************
125200130912     C     CtrDA2        BEGSR
125300130912     C*
125400130912     C                   SETOFF                                       99
125500130912     C*
125600130912     C     XCtrDA2       ENDSR
125700130912     C************************************************************
125800130912     C* GESTIONE VIDEO RECORD  DA3
125900130912     C************************************************************
126000130912     C     GESDA3        BEGSR
126100130912     C* emissione piede videata se proveniente da altra
126200130912     C* salvataggio valore formato di provenienza
126300130912     C     $LASTV        IFNE      '03'
126400130912     C                   WRITE     FMTT1
126500130912     C                   EXSR      GESPIE
126600130912     C                   Move      $LASTV        $LASTG
126700130912     C                   Move      '03'          $LASTV
126800130912     C                   ENDIF
126900130912     C*
127000130912     C* se esistono errori sulla videata
127100130912     C* emetto la write del formato a indicatori spenti per vedere
127200130912     C* le eventuali decodifiche
127300130912     C     *IN99         IFEQ      *ON
127400130912     C     $GEST         ANDEQ     'A3'
127500130912     C                   MoveA     *IN           WIN
127600130912     C                   Move      *ALL'0'       IN5098           49
127700130912     C                   MoveA     IN5098        *IN(50)
127800130912     C                   WRITE     FMTDA3
127900130912     C                   MoveA     WIN           *IN
128000130912     C                   ENDIF
128100130912     C* Inizializzazione indice tasti funzione personalizzati
128200130912     C                   Eval      PosFun = 25
128300130912     C*
128400130912     C                   SELECT
128500130912     C* Annullamento
128600130912     C     WOpzG62       WHENEQ    '04'
128700130912     C* Visualizzazione
128800130912     C     WOpzG62       OREQ      '05'
128900130912     C                   WRITE     FMTDA3
129000130912     C                   EXFMT     PROTECT
129100130912     C* Immissione/Modifica/Copia
129200130912     C                   OTHER
129300130912     C                   EXFMT     FMTDA3
129400130912     C                   ENDSL
129500130912     C*
129600130912     C* azzero la posizione del cursore
129700130912     C                   Z-ADD     0             H1RIGA
129800130912     C                   Z-ADD     0             H1COLO
129900130912     C*
130000130912     C* controllo abilitazione tasti funzione
130100130912     C                   EXSR      CTRKDx
130200130912     C*
130300130912     C     *IN99         IFEQ      *OFF
130400130912     C                   SELECT
130500130912     C* ROLDOWN
130600130912     C     $TASTO        WHENEQ    ROLDWN
130700130912     C                   Move      'A2'          $GEST
130800130912     C* F3=Fine
130900130912     C     $TASTO        WHENEQ    F03
131000130912     C                   EXSR      F03D01
131100130912     C* F5=Refresh
131200130912     C     $TASTO        WHENEQ    F05
131300130912     C                   EXSR      F05D01
131400130912     C* F8=Successivo
131500130912     C     $TASTO        WHENEQ    F08
131600130912     C                   EXSR      F08D01
131700130912     C* F12=Ritorno
131800130912     C     $TASTO        WHENEQ    F12
131900130912     C                   EXSR      F12D01
132000130912     C* F24=Altri tasti
132100130912     C     $TASTO        WHENEQ    F24
132200130912     C                   EXSR      F24Dx
132300130912     C* Personalizzata
132400130912     C     $PGF1(PosFun) Whenne    *BLANK
132500130912     C                   EXSR      FperDx
132600130912     C*
132700130912     C                   OTHER
132800130912     C                   EXSR      CtrDA3
132900130912     C     *IN99         IFEQ      *OFF
133000130912     C                   EXSR      CtrDA1
133100130912     C                   EXSR      CtrDA2
133200130912     C* Spengo gli indicatori di errore per il controllo su altri schermi
133300130912     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
133400130912     C* videate
133500130912     C     *IN99         IFEQ      *ON
133600130912     C                   Move      *ALL'0'       IN5098           49
133700130912     C                   MoveA     IN5098        *IN(50)
133800130912     C                   SETON                                        9996
133900130912     C                   ENDIF
134000130912     C                   ENDIF
134100130912     C*
134200130912     C* eseguo aggiornamento
134300130912     C     $TASTO        IFEQ      F06
134400130912     C     *IN99         ANDEQ     *OFF
134500130912     C                   EXSR      AGGANA
134600130912     C                   ENDIF
134700130912     C*
134800130912     C                   ENDSL
134900130912     C*
135000130912     C                   ENDIF
135100130912     C*
135200130912     C     XGESDA3       ENDSR
135300130912     C************************************************************
135400130912     C* CONTROLLO VIDEATA  DA3
135500130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
135600130912     C* 95 96 97 98
135700130912     C************************************************************
135800130912     C     CtrDA3        BEGSR
135900130912     C*
136000130912     C                   SETOFF                                       99
136100130912     C*
136200130912     C     XCtrDA3       ENDSR
136300070907     C************************************************************
136400070907     C* GESTIONE VIDEO RECORD D31
136500070907     C************************************************************
136600100730     C     GESDB1        BEGSR
136700070907     C* inizializzazione videata
136800070907     C     $INZDX        IFEQ      *ON
136900070907     C                   EXSR      INZDX
137000070907     C                   Move      *OFF          $INZDX
137100070907     C                   ENDIF
137200070907     C* emissione piede videata se proveniente da altra
137300070907     C* salvataggio valore formato di provenienza
137400070907     C     $LASTV        IFNE      '31'
137500070907     C                   WRITE     FMTT1
137600070907     C                   EXSR      GESPIE
137700070907     C                   Move      $LASTV        $LASTG
137800070907     C                   Move      '31'          $LASTV
137900070907     C                   ENDIF
138000070907     C*
138100070907     C* se esistono errori sulla videata
138200070907     C* emetto la write del formato a indicatori spenti per vedere
138300070907     C* le eventuali decodifiche
138400070907     C     *IN99         IFEQ      *ON
138500100803     C     $GEST         ANDEQ     'B1'
138600070907     C                   MoveA     *IN           WIN
138700070907     C                   Move      *ALL'0'       IN5098           49
138800070907     C                   MoveA     IN5098        *IN(50)
138900130912     C                   WRITE     FMTDB1
139000070907     C                   MoveA     WIN           *IN
139100070907     C                   ENDIF
139200070907     C* Inizializzazione indice tasti funzione personalizzati
139300070907     C                   Eval      PosFun = 25
139400070907     C*
139500070907     C                   SELECT
139600070907     C* Annullamento
139700130909     C     WOpzG62       WHENEQ    '04'
139800070907     C* Visualizzazione
139900130909     C     WOpzG62       OREQ      '05'
140000130912     C                   WRITE     FMTDB1
140100070907     C                   EXFMT     PROTECT
140200070907     C* Immissione/Modifica/Copia
140300070907     C                   OTHER
140400130912     C                   EXFMT     FMTDB1
140500070907     C                   ENDSL
140600070907     C*
140700070907     C* azzero la posizione del cursore
140800070907     C                   Z-ADD     0             H1RIGA
140900070907     C                   Z-ADD     0             H1COLO
141000070907     C*
141100070907     C* controllo abilitazione tasti funzione
141200070907     C                   EXSR      CTRKDx
141300070907     C*
141400070907     C     *IN99         IFEQ      *OFF
141500070907     C*
141600070907     C                   SELECT
141700070907     C* ROLLUP
141800070907     C     $TASTO        WHENEQ    ROLLUP
141900100803     C                   Move      'B2'          $GEST
142000070907     C* F3=Fine
142100070907     C     $TASTO        WHENEQ    F03
142200070907     C                   EXSR      F03D01
142300070907     C* F5=Refresh
142400070907     C     $TASTO        WHENEQ    F05
142500070907     C                   EXSR      F05D01
142600070907     C* F8=Successivo
142700070907     C     $TASTO        WHENEQ    F08
142800070907     C                   EXSR      F08D01
142900070907     C* F12=Ritorno
143000070907     C     $TASTO        WHENEQ    F12
143100070907     C                   EXSR      F12D01
143200070907     C* F24=Altri tasti
143300070907     C     $TASTO        WHENEQ    F24
143400070907     C                   EXSR      F24Dx
143500070907     C* Personalizzata
143600070907     C     $PGF1(PosFun) WHENNE    *BLANK
143700070907     C                   EXSR      FPerDx
143800070907     C*
143900070907     C                   OTHER
144000100730     C                   EXSR      CtrDB1
144100070907     C     *IN99         IFEQ      *OFF
144200100730     C                   EXSR      CtrDB2
144300130912     C                   EXSR      CtrDB3
144400070907     C* Spengo gli indicatori di errore per il controllo su altri schermi
144500070907     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
144600070907     C* videate
144700070907     C     *IN99         IFEQ      *ON
144800070907     C                   Move      *ALL'0'       IN5098           49
144900070907     C                   MoveA     IN5098        *IN(50)
145000070907     C                   SETON                                        9996
145100070907     C                   ENDIF
145200070907     C                   ENDIF
145300070907     C*
145400070907     C* eseguo aggiornamento
145500070907     C     $TASTO        IFEQ      F06
145600070907     C     *IN99         ANDEQ     *OFF
145700070907     C                   EXSR      AGGANA
145800070907     C                   ENDIF
145900070907     C*
146000070907     C                   ENDSL
146100070907     C*
146200070907     C                   ENDIF
146300070907     C*
146400070907     C     XGesDB1       ENDSR
146500070907     C************************************************************
146600070907     C* CONTROLLO VIDEATA
146700070907     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
146800070907     C* 95 96 97 98
146900070907     C************************************************************
147000100730     C     CtrDB1        BEGSR
147100070907     C*
147200070907     C                   SETOFF                                       99
147300070907     C*
147400100730     C     XCtrDB1       ENDSR
147500070907     C************************************************************
147600070907     C* GESTIONE VIDEO RECORD  D32
147700070907     C************************************************************
147800100730     C     GESDB2        BEGSR
147900070907     C* emissione piede videata se proveniente da altra
148000070907     C* salvataggio valore formato di provenienza
148100070907     C     $LASTV        IFNE      '32'
148200070907     C                   WRITE     FMTT1
148300070907     C                   EXSR      GESPIE
148400070907     C                   Move      $LASTV        $LASTG
148500070907     C                   Move      '32'          $LASTV
148600070907     C                   ENDIF
148700070907     C*
148800070907     C* se esistono errori sulla videata
148900070907     C* emetto la write del formato a indicatori spenti per vedere
149000070907     C* le eventuali decodifiche
149100070907     C     *IN99         IFEQ      *ON
149200100803     C     $GEST         ANDEQ     'B2'
149300070907     C                   MoveA     *IN           WIN
149400070907     C                   Move      *ALL'0'       IN5098           49
149500070907     C                   MoveA     IN5098        *IN(50)
149600130912     C                   WRITE     FMTDB2
149700070907     C                   MoveA     WIN           *IN
149800070907     C                   ENDIF
149900070907     C* Inizializzazione indice tasti funzione personalizzati
150000070907     C                   Eval      PosFun = 25
150100070907     C*
150200070907     C                   SELECT
150300070907     C* Annullamento
150400130909     C     WOpzG62       WHENEQ    '04'
150500070907     C* Visualizzazione
150600130909     C     WOpzG62       OREQ      '05'
150700130912     C                   WRITE     FMTDB2
150800070907     C                   EXFMT     PROTECT
150900070907     C* Immissione/Modifica/Copia
151000070907     C                   OTHER
151100130912     C                   EXFMT     FMTDB2
151200070907     C                   ENDSL
151300070907     C*
151400070907     C* azzero la posizione del cursore
151500070907     C                   Z-ADD     0             H1RIGA
151600070907     C                   Z-ADD     0             H1COLO
151700070907     C*
151800070907     C* controllo abilitazione tasti funzione
151900070907     C                   EXSR      CTRKDx
152000070907     C*
152100070907     C     *IN99         IFEQ      *OFF
152200070907     C*
152300070907     C                   SELECT
152400130912R078 C*    ROLLUP
152500130912 "   C     $TASTO        WHENEQ    ROLLUP
152600130912R078 C                   Move      'B3'          $GEST
152700070907     C* ROLDOWN
152800070907     C     $TASTO        WHENEQ    ROLDWN
152900100803     C                   Move      'B1'          $GEST
153000070907     C* F3=Fine
153100070907     C     $TASTO        WHENEQ    F03
153200070907     C                   EXSR      F03D01
153300070907     C* F5=Refresh
153400070907     C     $TASTO        WHENEQ    F05
153500070907     C                   EXSR      F05D01
153600070907     C* F8=Successivo
153700070907     C     $TASTO        WHENEQ    F08
153800070907     C                   EXSR      F08D01
153900070907     C* F12=Ritorno
154000070907     C     $TASTO        WHENEQ    F12
154100070907     C                   EXSR      F12D01
154200070907     C* F24=Altri tasti
154300070907     C     $TASTO        WHENEQ    F24
154400070907     C                   EXSR      F24Dx
154500070907     C* Personalizzata
154600070907     C     $PGF1(PosFun) Whenne    *BLANK
154700070907     C                   EXSR      FperDx
154800070907     C*
154900070907     C                   OTHER
155000100730     C                   EXSR      CtrDB2
155100070907     C     *IN99         IFEQ      *OFF
155200100730     C                   EXSR      CtrDB1
155300130912     C                   EXSR      CtrDB3
155400070907     C* Spengo gli indicatori di errore per il controllo su altri schermi
155500070907     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
155600070907     C* videate
155700070907     C     *IN99         IFEQ      *ON
155800070907     C                   Move      *ALL'0'       IN5098           49
155900070907     C                   MoveA     IN5098        *IN(50)
156000070907     C                   SETON                                        9996
156100070907     C                   ENDIF
156200070907     C                   ENDIF
156300070907     C*
156400070907     C* eseguo aggiornamento
156500070907     C     $TASTO        IFEQ      F06
156600070907     C     *IN99         ANDEQ     *OFF
156700070907     C                   EXSR      AGGANA
156800070907     C                   ENDIF
156900070907     C*
157000070907     C                   ENDSL
157100070907     C*
157200070907     C                   ENDIF
157300070907     C*
157400070907     C     XGesDB2       ENDSR
157500070907     C************************************************************
157600070907     C* CONTROLLO VIDEATA  32
157700070907     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
157800070907     C* 95 96 97 98
157900070907     C************************************************************
158000100730     C     CtrDB2        BEGSR
158100070907     C*
158200070907     C                   SETOFF                                       99
158300070907     C*
158400100730     C     XCtrDB2       ENDSR
158500130912     C************************************************************
158600130912     C* GESTIONE VIDEO RECORD  DB3
158700130912     C************************************************************
158800130912     C     GESDB3        BEGSR
158900130912     C* emissione piede videata se proveniente da altra
159000130912     C* salvataggio valore formato di provenienza
159100130912     C     $LASTV        IFNE      '33'
159200130912     C                   WRITE     FMTT1
159300130912     C                   EXSR      GESPIE
159400130912     C                   Move      $LASTV        $LASTG
159500130912     C                   Move      '33'          $LASTV
159600130912     C                   ENDIF
159700130912     C*
159800130912     C* se esistono errori sulla videata
159900130912     C* emetto la write del formato a indicatori spenti per vedere
160000130912     C* le eventuali decodifiche
160100130912     C     *IN99         IFEQ      *ON
160200130912     C     $GEST         ANDEQ     'B3'
160300130912     C                   MoveA     *IN           WIN
160400130912     C                   Move      *ALL'0'       IN5098           49
160500130912     C                   MoveA     IN5098        *IN(50)
160600130912     C                   WRITE     FMTDB3
160700130912     C                   MoveA     WIN           *IN
160800130912     C                   ENDIF
160900130912     C* Inizializzazione indice tasti funzione personalizzati
161000130912     C                   Eval      PosFun = 25
161100130912     C*
161200130912     C                   SELECT
161300130912     C* Annullamento
161400130912     C     WOpzG62       WHENEQ    '04'
161500130912     C* Visualizzazione
161600130912     C     WOpzG62       OREQ      '05'
161700130912     C                   WRITE     FMTDB3
161800130912     C                   EXFMT     PROTECT
161900130912     C* Immissione/Modifica/Copia
162000130912     C                   OTHER
162100130912     C                   EXFMT     FMTDB3
162200130912     C                   ENDSL
162300130912     C*
162400130912     C* azzero la posizione del cursore
162500130912     C                   Z-ADD     0             H1RIGA
162600130912     C                   Z-ADD     0             H1COLO
162700130912     C*
162800130912     C* controllo abilitazione tasti funzione
162900130912     C                   EXSR      CTRKDx
163000130912     C*
163100130912     C     *IN99         IFEQ      *OFF
163200130912     C*
163300130912     C                   SELECT
163400130912     C* ROLDOWN
163500130912     C     $TASTO        WHENEQ    ROLDWN
163600130912     C                   Move      'B2'          $GEST
163700130912     C* F3=Fine
163800130912     C     $TASTO        WHENEQ    F03
163900130912     C                   EXSR      F03D01
164000130912     C* F5=Refresh
164100130912     C     $TASTO        WHENEQ    F05
164200130912     C                   EXSR      F05D01
164300130912     C* F8=Successivo
164400130912     C     $TASTO        WHENEQ    F08
164500130912     C                   EXSR      F08D01
164600130912     C* F12=Ritorno
164700130912     C     $TASTO        WHENEQ    F12
164800130912     C                   EXSR      F12D01
164900130912     C* F24=Altri tasti
165000130912     C     $TASTO        WHENEQ    F24
165100130912     C                   EXSR      F24Dx
165200130912     C* Personalizzata
165300130912     C     $PGF1(PosFun) Whenne    *BLANK
165400130912     C                   EXSR      FperDx
165500130912     C*
165600130912     C                   OTHER
165700130912     C                   EXSR      CtrDB3
165800130912     C     *IN99         IFEQ      *OFF
165900130912     C                   EXSR      CtrDB1
166000130912     C                   EXSR      CtrDB2
166100130912     C* Spengo gli indicatori di errore per il controllo su altri schermi
166200130912     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
166300130912     C* videate
166400130912     C     *IN99         IFEQ      *ON
166500130912     C                   Move      *ALL'0'       IN5098           49
166600130912     C                   MoveA     IN5098        *IN(50)
166700130912     C                   SETON                                        9996
166800130912     C                   ENDIF
166900130912     C                   ENDIF
167000130912     C*
167100130912     C* eseguo aggiornamento
167200130912     C     $TASTO        IFEQ      F06
167300130912     C     *IN99         ANDEQ     *OFF
167400130912     C                   EXSR      AGGANA
167500130912     C                   ENDIF
167600130912     C*
167700130912     C                   ENDSL
167800130912     C*
167900130912     C                   ENDIF
168000130912     C*
168100130912     C     XGesDB3       ENDSR
168200130912     C************************************************************
168300130912     C* CONTROLLO VIDEATA  DB3
168400130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
168500130912     C* 95 96 97 98
168600130912     C************************************************************
168700130912     C     CtrDB3        BEGSR
168800130912     C*
168900130912     C                   SETOFF                                       99
169000130912     C*
169100130912     C     XCtrDB3       ENDSR
169200130912     C************************************************************
169300130912     C* GESTIONE VIDEO RECORD DE1
169400130912     C************************************************************
169500130912     C     GESDE1        BEGSR
169600130912     C* inizializzazione videata
169700130912     C     $INZDX        IFEQ      *ON
169800130912     C                   EXSR      INZDX
169900130912     C                   Move      *OFF          $INZDX
170000130912     C                   ENDIF
170100130912     C* emissione piede videata se proveniente da altra
170200130912     C* salvataggio valore formato di provenienza
170300130912     C     $LASTV        IFNE      '41'
170400130912     C                   WRITE     FMTT1
170500130912     C                   EXSR      GESPIE
170600130912     C                   Move      $LASTV        $LASTG
170700130912     C                   Move      '41'          $LASTV
170800130912     C                   ENDIF
170900130912     C*
171000130912     C* se esistono errori sulla videata
171100130912     C* emetto la write del formato a indicatori spenti per vedere
171200130912     C* le eventuali decodifiche
171300130912     C     *IN99         IFEQ      *ON
171400130912     C     $GEST         ANDEQ     'E1'
171500130912     C                   MoveA     *IN           WIN
171600130912     C                   Move      *ALL'0'       IN5098           49
171700130912     C                   MoveA     IN5098        *IN(50)
171800130912     C                   WRITE     FMTDE1
171900130912     C                   MoveA     WIN           *IN
172000130912     C                   ENDIF
172100130912     C* Inizializzazione indice tasti funzione personalizzati
172200130912     C                   Eval      PosFun = 25
172300130912     C*
172400130912     C                   SELECT
172500130912     C* Annullamento
172600130912     C     WOpzG62       WHENEQ    '04'
172700130912     C* Visualizzazione
172800130912     C     WOpzG62       OREQ      '05'
172900130912     C                   WRITE     FMTDE1
173000130912     C                   EXFMT     PROTECT
173100130912     C* Immissione/Modifica/Copia
173200130912     C                   OTHER
173300130912     C                   EXFMT     FMTDE1
173400130912     C                   ENDSL
173500130912     C*
173600130912     C* azzero la posizione del cursore
173700130912     C                   Z-ADD     0             H1RIGA
173800130912     C                   Z-ADD     0             H1COLO
173900130912     C*
174000130912     C* controllo abilitazione tasti funzione
174100130912     C                   EXSR      CTRKDx
174200130912     C*
174300130912     C     *IN99         IFEQ      *OFF
174400130912     C*
174500130912     C                   SELECT
174600130912     C* ROLLUP
174700130912     C**** $TASTO        WHENEQ    ROLLUP
174800130912     C****               Move      'A2'          $GEST
174900130912     C* F3=Fine
175000130912     C     $TASTO        WHENEQ    F03
175100130912     C                   EXSR      F03D01
175200130912     C* F5=Refresh
175300130912     C     $TASTO        WHENEQ    F05
175400130912     C                   EXSR      F05D01
175500130912     C* F8=Successivo
175600130912     C     $TASTO        WHENEQ    F08
175700130912     C                   EXSR      F08D01
175800130912     C* F12=Ritorno
175900130912     C     $TASTO        WHENEQ    F12
176000130912     C                   EXSR      F12D01
176100130912     C* F24=Altri tasti
176200130912     C     $TASTO        WHENEQ    F24
176300130912     C                   EXSR      F24Dx
176400130912     C* Personalizzata
176500130912     C     $PGF1(PosFun) WHENNE    *BLANK
176600130912     C                   EXSR      FPerDx
176700130912     C*
176800130912     C                   OTHER
176900130912     C                   EXSR      CtrDE1
177000130912     C     *IN99         IFEQ      *OFF
177100130912     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
177200130912     C* indicatori uguali in videate differenti, genererei conflitti
177300130912     C     *IN99         IFEQ      *ON
177400130912     C                   Move      *ALL'0'       IN5098           49
177500130912     C                   MoveA     IN5098        *IN(50)
177600130912     C                   SETON                                        9996
177700130912     C                   ENDIF
177800130912     C                   ENDIF
177900130912     C*
178000130912     C* eseguo aggiornamento
178100130912     C     $TASTO        IFEQ      F06
178200130912     C     *IN99         ANDEQ     *OFF
178300130912     C                   EXSR      AGGANA
178400130912     C                   ENDIF
178500130912     C*
178600130912     C                   ENDSL
178700130912     C*
178800130912     C                   ENDIF
178900130912     C*
179000130912     C     XGESDE1       ENDSR
179100130912     C************************************************************
179200130912     C* CONTROLLO VIDEATA  DE1
179300130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
179400130912     C* 95 96 97 98
179500130912     C************************************************************
179600130912     C     CtrDE1        BEGSR
179700130912     C*
179800130912     C                   SETOFF                                       99
179900130912     C*
180000130912     C     XCtrDE1       ENDSR
180100130912     C************************************************************
180200130912     C* GESTIONE VIDEO RECORD DZ1
180300130912     C************************************************************
180400130912     C     GESDZ1        BEGSR
180500130912     C* inizializzazione videata
180600130912     C     $INZDX        IFEQ      *ON
180700130912     C                   EXSR      INZDX
180800130912     C                   Move      *OFF          $INZDX
180900130912     C                   ENDIF
181000130912     C* emissione piede videata se proveniente da altra
181100130912     C* salvataggio valore formato di provenienza
181200130912     C     $LASTV        IFNE      '42'
181300130912     C                   WRITE     FMTT1
181400130912     C                   EXSR      GESPIE
181500130912     C                   Move      $LASTV        $LASTG
181600130912     C                   Move      '42'          $LASTV
181700130912     C                   ENDIF
181800130912     C*
181900130912     C* se esistono errori sulla videata
182000130912     C* emetto la write del formato a indicatori spenti per vedere
182100130912     C* le eventuali decodifiche
182200130912     C     *IN99         IFEQ      *ON
182300130912     C     $GEST         ANDEQ     'Z1'
182400130912     C                   MoveA     *IN           WIN
182500130912     C                   Move      *ALL'0'       IN5098           49
182600130912     C                   MoveA     IN5098        *IN(50)
182700130912     C                   WRITE     FMTDZ1
182800130912     C                   MoveA     WIN           *IN
182900130912     C                   ENDIF
183000130912     C* Inizializzazione indice tasti funzione personalizzati
183100130912     C                   Eval      PosFun = 25
183200130912     C*
183300130912     C                   SELECT
183400130912     C* Annullamento
183500130912     C     WOpzG62       WHENEQ    '04'
183600130912     C* Visualizzazione
183700130912     C     WOpzG62       OREQ      '05'
183800130912     C                   WRITE     FMTDZ1
183900130912     C                   EXFMT     PROTECT
184000130912     C* Immissione/Modifica/Copia
184100130912     C                   OTHER
184200130912     C                   EXFMT     FMTDZ1
184300130912     C                   ENDSL
184400130912     C*
184500130912     C* azzero la posizione del cursore
184600130912     C                   Z-ADD     0             H1RIGA
184700130912     C                   Z-ADD     0             H1COLO
184800130912     C*
184900130912     C* controllo abilitazione tasti funzione
185000130912     C                   EXSR      CTRKDx
185100130912     C*
185200130912     C     *IN99         IFEQ      *OFF
185300130912     C*
185400130912     C                   SELECT
185500130912     C* ROLLUP
185600130912     C     $TASTO        WHENEQ    ROLLUP
185700130912     C                   Move      'Z2'          $GEST
185800130912     C* F3=Fine
185900130912     C     $TASTO        WHENEQ    F03
186000130912     C                   EXSR      F03D01
186100130912     C* F5=Refresh
186200130912     C     $TASTO        WHENEQ    F05
186300130912     C                   EXSR      F05D01
186400130912     C* F8=Successivo
186500130912     C     $TASTO        WHENEQ    F08
186600130912     C                   EXSR      F08D01
186700130912     C* F12=Ritorno
186800130912     C     $TASTO        WHENEQ    F12
186900130912     C                   EXSR      F12D01
187000130912     C* F24=Altri tasti
187100130912     C     $TASTO        WHENEQ    F24
187200130912     C                   EXSR      F24Dx
187300130912     C* Personalizzata
187400130912     C     $PGF1(PosFun) WHENNE    *BLANK
187500130912     C                   EXSR      FPerDx
187600130912     C*
187700130912     C                   OTHER
187800130912     C                   EXSR      CtrDZ1
187900130912     C     *IN99         IFEQ      *OFF
188000130912     C* Spengo gli indicatori di errore per il controllo su altri schermi
188100130912     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
188200130912     C* videate
188300130912     C     *IN99         IFEQ      *ON
188400130912     C                   Move      *ALL'0'       IN5098           49
188500130912     C                   MoveA     IN5098        *IN(50)
188600130912     C                   SETON                                        9996
188700130912     C                   ENDIF
188800130912     C                   ENDIF
188900130912     C*
189000130912     C* eseguo aggiornamento
189100130912     C     $TASTO        IFEQ      F06
189200130912     C     *IN99         ANDEQ     *OFF
189300130912     C                   EXSR      AGGANA
189400130912     C                   ENDIF
189500130912     C*
189600130912     C                   ENDSL
189700130912     C*
189800130912     C                   ENDIF
189900130912     C*
190000130912     C     XGesDZ1       ENDSR
190100130912     C************************************************************
190200130912     C* CONTROLLO VIDEATA
190300130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
190400130912     C* 95 96 97 98
190500130912     C************************************************************
190600130912     C     CtrDZ1        BEGSR
190700130912     C*
190800130912     C                   SETOFF                                       99
190900130912     C*
191000130912     C     XCtrDZ1       ENDSR
191100070907     C************************************************************
191200070907     C* AGGIORNAMENTO ANAGRAFICA
191300070907     C************************************************************
191400070907     C     AGGANA        BEGSR
191500070907     C*
191600070907     C*-------------------------------------------
191700070907     C                   select
191800130909    >C                   when      WOpzG62 ='02'
191900070907     C                   exsr      RieAna
192000130909    >C                   update    SDGETE000
192100070907     C                   endsl
192200070907     C*
192300070907     C*-------------------------------------------
192400070907     C* richiama pgm User Exit
192500070907     C     USRERR        IFEQ      *BLANK
192600070907     C* passa opzione ricevuta a user exit
192700130909    >C                   MoveL     WOpzG62       XUKOPZ
192800070907     C* imposta chiave univoca  per user exit
192900070907     C                   Move      XSCSOC        XUKSOC
193000070907     C                   Move      XSCDUN        XUKDUN
193100070907     C                   MoveL     DSPGM         XUKPGM
193200070907    >C                   MoveL     D1TpRec       XUKFL1
193300070907    >C* ho fino a 6 campi chiave passabili
193400070907     C                   MoveL     XUKDS         KPJBU
193500070907     C     DSPGM         CAT       'U':0         USREXT           10
193600070907     C                   CALL      USREXT                               21
193700070907     C                   PARM                    KPJBA
193800070907     C     *IN21         IFEQ      *ON
193900070907     C                   Move      *ON           USRERR            1
194000070907     C                   ENDIF
194100070907     C                   ENDIF
194200070907     C*
194300070907     C*-------------------------------------------
194400070907     C* eseguo operazioni del dopo-aggiornamento
194500070907     C                   if        *in99 = *off
194600070907     C* segnalo al pgm chiamante l'avvenuta conferma
194700130909     C                   Move      *ON           OPRG62
194800070907     C* segnalo al pgm chiamante l'aver premuto l'F6
194900130909     C                   Move      '0'           RETG62
195000070907     C* verifico se ci sono altri record dello stesso tipo scelto
195100070907     C                   Move      'DX'          $GEST
195200070907     C                   endif
195300070907     C*
195400070907     C     XAggAna       ENDSR
195500070907     C************************************************************
195600070907     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
195700070907     C************************************************************
195800070907     C     RIEANA        BEGSR
195900070907     C*
196000070907     C                   select
196100130912     C                   when      D1TpRec='A'
196200130912     C                   eval      ETEREC= TPRECADS
196300130912     C                   when      D1TpRec='B'
196400130912     C                   eval      ETEREC= TPRECBDS
196500130912     C                   when      D1TpRec='E'
196600130912     C                   eval      ETEREC= TPRECEDS
196700130912     C                   when      D1TpRec='Z'
196800130912     C                   eval      ETEREC= TPRECZDS
196900070907     C                   endsl
197000070907     C*
197100070907     C     XRieAna       ENDSR
197200070907     C************************************************************
197300070907     C* CONTROLLO ABILITAZIONE TASTI
197400070907     C************************************************************
197500070907     C     CTRKEY        BEGSR
197600070907     C*
197700070907     C                   SELECT
197800070907     C     $TASTO        WHENEQ    F01
197900070907     C                   Z-ADD     01            PosFun
198000070907     C     $FC(1)        IFEQ      '0'
198100070907     C                   SETON                                        99
198200070907     C                   ENDIF
198300070907     C     $TASTO        WHENEQ    F02
198400070907     C                   Z-ADD     02            PosFun
198500070907     C     $FC(2)        IFEQ      '0'
198600070907     C                   SETON                                        99
198700070907     C                   ENDIF
198800070907     C     $TASTO        WHENEQ    F03
198900070907     C                   Z-ADD     03            PosFun
199000070907     C     $FC(3)        IFEQ      '0'
199100070907     C                   SETON                                        99
199200070907     C                   ENDIF
199300070907     C     $TASTO        WHENEQ    F04
199400070907     C                   Z-ADD     04            PosFun
199500070907     C     $FC(4)        IFEQ      '0'
199600070907     C                   SETON                                        99
199700070907     C                   ENDIF
199800070907     C     $TASTO        WHENEQ    F05
199900070907     C                   Z-ADD     05            PosFun
200000070907     C     $FC(5)        IFEQ      '0'
200100070907     C                   SETON                                        99
200200070907     C                   ENDIF
200300070907     C     $TASTO        WHENEQ    F06
200400070907     C                   Z-ADD     06            PosFun
200500070907     C     $FC(6)        IFEQ      '0'
200600070907     C                   SETON                                        99
200700070907     C                   ENDIF
200800070907     C     $TASTO        WHENEQ    F07
200900070907     C                   Z-ADD     07            PosFun
201000070907     C     $FC(7)        IFEQ      '0'
201100070907     C                   SETON                                        99
201200070907     C                   ENDIF
201300070907     C     $TASTO        WHENEQ    F08
201400070907     C                   Z-ADD     08            PosFun
201500070907     C     $FC(8)        IFEQ      '0'
201600070907     C                   SETON                                        99
201700070907     C                   ENDIF
201800070907     C     $TASTO        WHENEQ    F09
201900070907     C                   Z-ADD     09            PosFun
202000070907     C     $FC(09)       IFEQ      '0'
202100070907     C                   SETON                                        99
202200070907     C                   ENDIF
202300070907     C     $TASTO        WHENEQ    F10
202400070907     C                   Z-ADD     10            PosFun
202500070907     C     $FC(10)       IFEQ      '0'
202600070907     C                   SETON                                        99
202700070907     C                   ENDIF
202800070907     C     $TASTO        WHENEQ    F11
202900070907     C                   Z-ADD     11            PosFun
203000070907     C     $FC(11)       IFEQ      '0'
203100070907     C                   SETON                                        99
203200070907     C                   ENDIF
203300070907     C     $TASTO        WHENEQ    F12
203400070907     C                   Z-ADD     12            PosFun
203500070907     C     $FC(12)       IFEQ      '0'
203600070907     C                   SETON                                        99
203700070907     C                   ENDIF
203800070907     C     $TASTO        WHENEQ    F13
203900070907     C                   Z-ADD     13            PosFun
204000070907     C     $FC(13)       IFEQ      '0'
204100070907     C                   SETON                                        99
204200070907     C                   ENDIF
204300070907     C     $TASTO        WHENEQ    F14
204400070907     C                   Z-ADD     14            PosFun
204500070907     C     $FC(14)       IFEQ      '0'
204600070907     C                   SETON                                        99
204700070907     C                   ENDIF
204800070907     C     $TASTO        WHENEQ    F15
204900070907     C                   Z-ADD     15            PosFun
205000070907     C     $FC(15)       IFEQ      '0'
205100070907     C                   SETON                                        99
205200070907     C                   ENDIF
205300070907     C     $TASTO        WHENEQ    F16
205400070907     C                   Z-ADD     16            PosFun
205500070907     C     $FC(16)       IFEQ      '0'
205600070907     C                   SETON                                        99
205700070907     C                   ENDIF
205800070907     C     $TASTO        WHENEQ    F17
205900070907     C                   Z-ADD     17            PosFun
206000070907     C     $FC(17)       IFEQ      '0'
206100070907     C                   SETON                                        99
206200070907     C                   ENDIF
206300070907     C     $TASTO        WHENEQ    F18
206400070907     C                   Z-ADD     18            PosFun
206500070907     C     $FC(18)       IFEQ      '0'
206600070907     C                   SETON                                        99
206700070907     C                   ENDIF
206800070907     C     $TASTO        WHENEQ    F19
206900070907     C                   Z-ADD     19            PosFun
207000070907     C     $FC(19)       IFEQ      '0'
207100070907     C                   SETON                                        99
207200070907     C                   ENDIF
207300070907     C     $TASTO        WHENEQ    F20
207400070907     C                   Z-ADD     20            PosFun
207500070907     C     $FC(20)       IFEQ      '0'
207600070907     C                   SETON                                        99
207700070907     C                   ENDIF
207800070907     C     $TASTO        WHENEQ    F21
207900070907     C                   Z-ADD     21            PosFun
208000070907     C     $FC(21)       IFEQ      '0'
208100070907     C                   SETON                                        99
208200070907     C                   ENDIF
208300070907     C     $TASTO        WHENEQ    F22
208400070907     C                   Z-ADD     22            PosFun
208500070907     C     $FC(22)       IFEQ      '0'
208600070907     C                   SETON                                        99
208700070907     C                   ENDIF
208800070907     C     $TASTO        WHENEQ    F23
208900070907     C                   Z-ADD     23            PosFun
209000070907     C     $FC(23)       IFEQ      '0'
209100070907     C                   SETON                                        99
209200070907     C                   ENDIF
209300070907     C     $TASTO        WHENEQ    F24
209400070907     C                   Z-ADD     24            PosFun
209500070907     C     $FC(24)       IFEQ      '0'
209600070907     C                   SETON                                        99
209700070907     C                   ENDIF
209800070907     C                   ENDSL
209900070907     C*
210000070907     C     XCtrKey       ENDSR
210100070907     C******************************************************
210200070907     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
210300070907     C******************************************************
210400070907     C     MSGPGM        BEGSR
210500070907     C*
210600070907     C                   Z-ADD     2             MSGMSG
210700070907     C                   MoveL     'S'           MSGRSP
210800070907     C                   MoveL     'CR'          MSGTPR
210900070907     C                   MoveL     'S'           MSGEMV
211000070907     C                   Z-ADD     10            MSGRGP
211100070907     C                   MoveL     'S'           MSGLCK
211200070907     C                   MoveA     STATUS        STS
211300070907     C                   MoveL     'N'           MSGCNL
211400070907     C                   MoveL     'N'           MSGVID
211500070907     C                   MoveL     'N'           MSGOPE
211600070907     C                   MoveL     'N'           MSGSTP
211700100820     C                   CALL      'SDGMSGR'
211800100820     C                   PARM                    SDGMSGDS
211900070907     C*
212000100820     C                   ENDSR
212100070907     C************************************************************
212200070907     C* OPERAZIONI INIZIALI
212300070907     C************************************************************
212400070907     C     *INZSR        BEGSR
212500070907     C*
212600070907     C* Reperimento parametri
212700070907     C     *ENTRY        PLIST
212800070907     C                   PARM                    KPJBA
212900070907     C                   PARM                    DSAUT
213000070907     C*
213100070907     C*
213200070907     C* testo se sono stati passati i dati di autorizzazione
213300070907     C     PARMS         IFLT      2
213400070907     C                   Move      *BLANK        AUTDS
213500070907     C                   ELSE
213600070907     C                   MoveL     DSAUT         AUTDS
213700070907     C                   ENDIF
213800070907     C*
213900070907     C* reperisco le dimensioni delle variabili che conterranno i tasti
214000070907     C                   EVAL      $D1L01 = %Size(Z1Ke1)
214100070907     C                   EVAL      $D1L02 = %Size(Z1Ke2)
214200070907     C                   EVAL      $DxL01 = %Size(Z2Ke1)
214300070907     C                   EVAL      $DxL02 = %Size(Z2Ke2)
214400070907     C*
214500070907     C* Reperimento tasti di funzione
214600070907     C                   EXSR      REPKEY
214700070907     C*
214800070907     C* Richiamo routine per aggiunta tasti funzione personalizzati
214900070907     C                   CAllb     'X46USREXT'
215000070907     C                   PARM                    *omit
215100070907     C                   PARM                    *omit
215200070907     C                   PARM                    *omit
215300070907     C                   PARM                    *omit
215400070907     C                   PARM                    *omit
215500070907     C                   PARM                    $FX1
215600070907     C                   PARM                    $FC1
215700070907     C                   PARM                    $PGF1
215800070907     C                   PARM                    DsPgm
215900070907     C                   PARM                    xsccut
216000070907     C                   PARM                    xscgrp
216100070907     C*
216200070907     C     XInzSr        ENDSR
216300070907     C************************************************************
216400070907     C* REPERIMENTO TASTI DI FUNZIONE
216500070907     C************************************************************
216600070907     C     REPKEY        BEGSR
216700070907     C*
216800070907     C                   MoveA     $FM1          $FM
216900070907     C                   MoveA     $FC1          $FC
217000070907     C                   EXSR      REPKKK
217100070907     C                   MoveA     $FX           $FX1
217200070907     C*
217300070907     C                   MoveA     $FM2          $FM
217400070907     C                   MoveA     $FC2          $FC
217500070907     C                   EXSR      REPKKK
217600070907     C                   MoveA     $FX           $FX2
217700070907     C*
217800070907     C     XRepKey       ENDSR
217900070907     C************************************************************
218000070907     C* REPERIMENTO TASTI DI FUNZIONE
218100070907     C************************************************************
218200070907     C     REPKKK        BEGSR
218300070907     C*
218400070907     C                   CLEAR                   $FX
218500070907     C     1             DO        24            X
218600070907     C*
218700070907     C     $FM(X)        IFNE      *BLANK
218800070907     C     $FC(X)        ANDNE     '0'
218900070907     C*
219000070907     C                   MoveL     $FM(X)        IDMSG
219100070907     C                   CALLB     'XRTVM'
219200070907     C                   PARM                    IDMSG            27
219300070907     C                   PARM                    TXTMSG          100
219400070907     C                   PARM                    ERRMSG            1
219500070907     C*
219600070907     C     ERRMSG        IFNE      *ON
219700070907     C                   MoveL     TXTMSG        $FX(X)
219800070907     C                   ELSE
219900070907     C                   MoveL     *ALL' '       $FX(X)
220000070907     C                   ENDIF
220100070907     C*
220200070907     C                   ENDIF
220300070907     C*
220400070907     C                   ENDDO
220500070907     C*
220600070907     C     XRepKKK       ENDSR
220700070907     C************************************************************
220800070907     C* INIZIALIZZAZIONE VARIABILI
220900070907     C************************************************************
221000070907     C     INZVAR        BEGSR
221100070907     C*
221200070907     C* Dati da PGM guida
221300130909     C                   MoveL     KPJBU         SDGSG62DS
221400070907     C*
221500070907     C                   MoveL     'SOC001'      TIPXSC
221600130909     C                   MoveL     SocietaG62    SOCXSC
221700070907     C                   EXSR      REPSOC
221800070907     C     RTNXSC        IFNE      '1'
221900070907     C                   MoveL     XSOCDS        SOC001
222000070907     C                   ELSE
222100130909     C                   Move      '1'           ErrG62
222200070907     C                   Move      *on           $Fine
222300070907     C                   EXSR      EndPgm
222400070907     C                   ENDIF
222500070907     C*
222600070907     C*
222700070907     C* Reperisco la libreria in cui sarà memorizzato il file
222800070907     C* che verrà creato
222900070907     C                   EXSR      RepLib
223000070907     C*
223100070907     C* Reperimento autorizzazioni
223200070907     C*
223300070907    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
223400070907    >C*   asteriscare la chiamata alla subroutine REPAUT
223500070907     C* le autorizzazioni potrebbero essere già state passate,
223600070907     C* nel qual caso non vanno reperite
223700070907    >C     AUTDS         IFEQ      *BLANK
223800070907    >C                   EXSR      REPAUT
223900070907    >C                   ELSE
224000070907    >C                   MoveL     XCADDS        CONCIVDS
224100070907    >C                   ENDIF
224200070907     C*
224300070907     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
224400070907     C* le autorizzazione
224500070907     C*
224600070907    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
224700070907    >C*   asteriscare la chiamata alla subroutine TSTAUT
224800070907    >C*                  EXSR      TSTAUT
224900070907     C*
225000070907     C* Definizione dei tasti funzione utilizzabili a pgm
225100070907     C                   EXSR      DEFOPZ
225200070907     C*
225300070907     C* Variabili per gestione videate
225400070907     C                   Move      'D1'          $GEST
225500070907     C                   Move      *OFF          $FINE
225600070907     C                   Move      *Off          $INZDX
225700070907     C                   Move      *ON           $INZD1
225800070907     C                   Move      *BLANK        $LASTG
225900070907     C                   Move      *BLANK        $LASTV
226000070907     C                   Z-ADD     0             $ULKD1            3 0
226100070907     C                   Z-ADD     0             $ULKDx            3 0
226200070907     C* azzero la posizione del cursore
226300070907     C                   Z-ADD     0             H1RIGA
226400070907     C                   Z-ADD     0             H1COLO
226500070907     C*
226600070907     C* Variabili appoggio
226700070907     C                   Z-ADD     0             CURR
226800070907     C                   Z-ADD     0             CURC
226900070907     C*
227000070907     C* Valorizzazione campi univoci testate
227100070907     C                   CLEAR                   FMTT1
227200070907     C                   MoveL     KNSIF         NOMSIF
227300070907     C                   MoveL     XSCDSI        NOMDIT
227400100803     C                   MOVEL     KCDAZ         NOMAZN
227500100803     C                   MOVEL     DSPGM         NOMPGM           10
227600070907     C* recupero il sottotitolo (se c'è)
227700160208R195 X****               MoveL     *BLANK        D1TIT
227800160208 "   X**** SOTTITD1      IFNE      *BLANK
227900160208 "   X****               MoveL     SOTTITD1      IDMSG
228000160208 "   X****               CALLB     'XRTVM'
228100160208 "   X****               PARM                    IDMSG            27
228200160208 "   X****               PARM                    TXTMSG          100
228300160208 "   X****               PARM                    ERRMSG            1
228400160208 "   X**** ERRMSG        IFNE      *ON
228500160208 "   X****               MoveL     TXTMSG        D1TIT
228600160208 "   X****               ELSE
228700160208 "   X****               MoveL     *ALL'?'       D1TIT
228800160208 "   X****               ENDIF
228900160208 "   X****               ENDIF
229000160208 "   X**** recupero il sottotitolo (se c'è)
229100160208 "   X****               MoveL     *BLANK        T1TIT
229200160208 "   X**** SOTTITD2      IFNE      *BLANK
229300160208 "   X****               MoveL     SOTTITD2      IDMSG
229400160208 "   X****               CALLB     'XRTVM'
229500160208 "   X****               PARM                    IDMSG            27
229600160208 "   X****               PARM                    TXTMSG          100
229700160208 "   X****               PARM                    ERRMSG            1
229800160208 "   X****               PARM      *On           CTRMSG            1            centratura
229900160208 "   X****               PARM      30            LENMSG            3 0          lun output.
230000160208 "   X**** ERRMSG        IFNE      *ON
230100160208 "   X****               MoveL     TXTMSG        T1TIT
230200160208 "   X****               ElSe
230300160208 "   X****               MoveL     *ALL'?'       T1TIT
230400160208 "   X****               EndIf
230500160208R195 X****               EndIf
230501160208R195 C*
230502160208 "   C*
230503160208 "   C* recupero il titolo con il tipo comunicazione
230504160208 "   C                   MOVEL     *BLANK        T1TIT
230505160208 "   C*
230506160208 "   C                   Select
230507160208 "   C* Comunicazione Black List
230508160208 "   C* Polivalente ANTE 2016
230509160208 "   C                   When      TpComG62    = 'B'
230510160208 "   C* o Black list annuale da 2016
230511160208 "   C                             or
230512160208 "   C                             TpComG62    = 'L'
230513160208 "   C*
230514160208 "   C                   if        SOTTITT3   <> *BLANK
230515160208 "   C                   MOVEL     SOTTITT3      IDMSG
230516160208 "   C                   endif
230517160208 "   C*
230518160208 "   C* Comunicazione Acquisti di San Marino
230519160208 "   C                   When      TpComG62    = 'S'
230520160208 "   C*
230521160208 "   C                   if        SOTTITT4   <> *BLANK
230522160208 "   C                   MOVEL     SOTTITT4      IDMSG
230523160208 "   C                   endif
230524160208 "   C*
230525160208 "   C                   Other
230526160208 "   C*
230527160208 "   C* Comunicazione Spesometro Clienti/Fornitori
230528160208 "   C                   if        SOTTITT1   <> *BLANK
230529160208 "   C                   MOVEL     SOTTITT1      IDMSG
230530160208 "   C                   endif
230531160208 "   C*
230532160208 "   C                   endSl
230533160208 "   C*
230534160208 "   C                   CALLB     'XRTVM'
230535160208 "   C                   PARM                    IDMSG            27
230536160208 "   C                   PARM                    TXTMSG          100
230537160208 "   C                   PARM                    ERRMSG            1
230538160208 "   C                   PARM      *ON           CTRMSG            1            centratura
230539160208 "   C                   PARM      30            LENMSG            3 0          lun output.
230540160208 "   C     ERRMSG        IFNE      *ON
230541160208 "   C                   MOVEL     TXTMSG        T1TIT
230542160208 "   C                   ELSE
230543160208 "   C                   MOVEL     *ALL'?'       T1TIT
230544160208 "   C                   ENDIF
230545160208R195 C*
230600070907     C* recupero il tipo operazione
230700070907     C                   Z-ADD     1             X
230800130909     C     OPZG62        LOOKUP    $OZ(X)                                 21
230900070907     C     *IN21         IFEQ      *ON
231000070907     C                   MoveL     $ID(X)        IDMSG
231100070907     C                   CALLB     'XRTVM'
231200070907     C                   PARM                    IDMSG            27
231300070907     C                   PARM                    TXTMSG          100
231400070907     C                   PARM                    ERRMSG            1
231500070907     C                   PARM      *ON           CTRMSG            1
231600070907     C                   PARM      30            LENMSG            3 0
231700070907     C     ERRMSG        IFNE      *ON
231800100730     C                   MoveL     TXTMSG        D1TIT
231900070907     C                   ELSE
232000100730     C                   MoveL     *ALL'?'       D1TIT
232100070907     C                   ENDIF
232200070907     C                   ENDIF
232300070907     C*
232400070907     C* Inizializzazione/Decodifica prima videata
232500070907     C                   EXSR      INZD1
232600070907     C                   Move      *OFF          $INZD1
232700070907     C*
232800070907     C     XInzVar       ENDSR
232900070907     C************************************************************
233000070907     C* REPERIMENTO AUTORIZZAZIONI UTENTE
233100070907     C************************************************************
233200070907     C     REPAUT        BEGSR
233300070907     C*
233400070907     C                   CLEAR                   AUTDS
233500070907     C*
233600070907     C                   MOVEL     XSCSOC        XCASOC
233700070907     C                   MOVEL     XSCDUN        XCAUNI
233800070907     C                   Z-ADD     0             XCAGRP
233900070907     C                   MOVEL     KNMUS         XCAPRF
234000070907    >C                   MOVEL     'CONCIV'      XCAFNC
234100070907    >C                   MOVEL     *ZERO         XCAVFU
234200070907    >C                   MOVE      'CK'          XCATCK
234300070907    >C                   MOVE      '1'           XCANAU
234400070907     C*
234500070907     C                   CALLB     'XCHKAUT'
234600070907     C                   PARM                    AUTDS
234700070907     C*
234800070907     C* errori/non abilitazione
234900070907     C     XCARTN        IFNE      0
235000070907     C                   MOVE      *ON           $fine
235100070907     C                   EXSR      ENDPGM
235200070907     C                   ELSE
235300070907     C                   MOVEL     XCADDS        CONCIVDS
235400070907     C* Se non autorizzati alla funzione oppure                              ed
235500070907     C     MANALL        IFEQ      *OFF
235600070907     C                   MOVE      XCAFNC        XNOFNC
235700070907     C                   MOVE      XCAPRF        XNOPRF
235800070907     C                   MOVE      XCACUT        XNOCUT
235900070907     C                   MOVE      XCATCK        XNOTCK
236000070907     C                   MOVE      '1'           XNOERR
236100070907     C                   CALL      'XNOAUTR'
236200070907     C                   PARM                    XNOADS
236300070907     C                   MOVE      *ON           $fine
236400070907     C                   EXSR      ENDPGM
236500070907     C                   ENDIF
236600070907     C                   ENDIF
236700070907     C*
236800070907     C                   ENDSR
236900070907     C/EJECT
237000070907     C************************************************************
237100070907     C* REPERIMENTO DATI SOCIETA'
237200070907     C************************************************************
237300070907     C     REPSOC        BEGSR
237400070907     C*
237500070907     C                   CALLB     'XSOC'
237600070907     C                   PARM                    TIPXSC            6
237700070907     C                   PARM                    SOCXSC            3
237800070907     C                   PARM                    CDSXSC            9 0
237900070907     C                   PARM                    MODXSC            3
238000070907     C                   PARM      *BLANKS       RTNXSC            1
238100070907     C                   PARM                    XSOCDS
238200070907     C                   PARM                    KPJBA
238300070907     C*
238400070907     C     XRepSoc       ENDSR
238500070907     C************************************************************
238600070907     C* Rerimento libreria per scarico su supporto magnetico
238700070907     C************************************************************
238800070907     C     REPLib        BEGSR
238900070907     C*
239000070907     C                   Clear                   ANP022DS
239100070907     C*
239200070907     C                   CALLB     'XPAR'
239300070907     C                   Parm                    XScSoc
239400070907     C                   Parm      'SCARSUMA'    XPaPar            8
239500070907     C                   Parm                    XPaOut         2000
239600070907     C                   Parm      *OFF          XPaErr            1
239700070907     C                   Parm      ' '           XPaRic            1
239800070907     C*
239900070907     C                   if        XPaErr      = *On
240000130909     C                   Move      '1'           ErrG62
240100070907     C                   Move      *on           $Fine
240200070907     C                   EXSR      EndPgm
240300070907     C*
240400070907     C                   else
240500070907     C                   MoveL     XPaOut        ANP022DS
240600070907     C                   endif
240700070907     C*
240800070907     C                   ENDSR
240900070907     C/EJECT
241000070907     C************************************************************
241100070907     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
241200070907     C************************************************************
241300070907     C     DEFOPZ        BEGSR
241400070907     C*
241500070907     C* Questa routine deve valorizzare la schiera $FC1/$FR1
241600070907     C* con S o N a seconda della utilizzabilità o meno
241700070907     C* del tasto funzione correlato
241800070907     C*
241900070907     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
242000070907     C* di compilazione
242100070907     C                   RESET                   $FC1
242200070907     C                   RESET                   $FR1
242300070907     C*
242400070907     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
242500070907     C* invalido alcune opzioni
242600070907     C                   SELECT
242700070907     C* Se pgm richiamato in interrogazione disattivo :
242800130909     C     OPZG62        WHENEQ    '05'
242900070907     C* F2=Decodifica
243000070907     C                   Z-ADD     2             X
243100070907     C                   Move      '0'           $FC1(X)
243200070907     C* F4=Ricerca
243300070907     C                   Z-ADD     4             X
243400070907     C                   Move      '0'           $FC1(X)
243500070907     C* F5=Ripristino
243600070907     C                   Z-ADD     5             X
243700070907     C                   Move      '0'           $FC1(X)
243800070907     C* F6=Conferma
243900070907     C                   Z-ADD     06            X
244000070907     C                   Move      '0'           $FC1(X)
244100070907     C* Se pgm richiamato in annullamento disattivo :
244200130909     C     OPZG62        WHENEQ    '04'
244300070907     C* F2=Decodifica
244400070907     C                   Z-ADD     2             X
244500070907     C                   Move      '0'           $FC1(X)
244600070907     C* F4=Ricerca
244700070907     C                   Z-ADD     4             X
244800070907     C                   Move      '0'           $FC1(X)
244900070907     C* F5=Ripristino
245000070907     C                   Z-ADD     5             X
245100070907     C                   Move      '0'           $FC1(X)
245200070907     C*
245300070907     C                   OTHER
245400070907     C*
245500070907     C                   ENDSL
245600070907     C*
245700070907     C     XDefOpz       ENDSR
245800070907     C************************************************************
245900070907     C* DEFINIZIONE KLIST
246000070907     C************************************************************
246100070907     C     DefKlist      BEGSR
246200070907     C*
246300070907     C* klist
246400070907     C*
246500070907     C     XdefKList     ENDSR
246600070907     C************************************************************
246700070907     C* RIEMPIMENTO TASTI FUNZIONE
246800070907     C************************************************************
246900070907     C     RIEKEY        BEGSR
247000070907     C*
247100070907     C* Input :
247200070907     C* - $FX = schiera con tutti i testi disponibili
247300070907     C* - $FC = schiere con i flag di validità associati ai testi
247400070907     C* - $FR = schiere con i flag di REVERSE IMAGE
247500070907     C* - $ULK = ultimo testo caricato
247600070907     C* - $L01 = Lunghezza prima riga
247700070907     C* - $L02 = Lunghezza seconda riga
247800070907     C* Output :
247900070907     C* - $ULK
248000070907     C* - $KEY1= 1° riga di tasti funzione
248100070907     C* - $KEY2= 2° riga di tasti funzione
248200070907     C*
248300070907     C                   CALLB     'XKEYMSG'
248400070907     C* input
248500070907     C                   PARM                    $FX
248600070907     C                   PARM                    $FC
248700070907     C                   PARM                    $FR
248800070907     C                   PARM                    $ULK              3 0
248900070907     C* output
249000070907     C                   PARM                    $KEY1            79
249100070907     C                   PARM                    $KEY2            79
249200070907     C* input
249300070907     C                   PARM                    $L01              3 0
249400070907     C                   PARM                    $L02              3 0
249500070907     C*
249600070907     C                   PARM                    $ALLFUNCT       480
249700070907     C*
249800070907     C     XRieKey       ENDSR
249900070907     C************************************************************
250000070907** $OZ / $ID
25010007090701PROMSG    *LIBL     COS0001
25020007090702PROMSG    *LIBL     COS0002
25030007090703PROMSG    *LIBL     COS0003
25040007090704PROMSG    *LIBL     COS0004
25050007090705PROMSG    *LIBL     COS0005
250600070907** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
250700070907PROMSG    *LIBL     KEY0001  01
250800070907PROMSG    *LIBL     KEY0002  02
250900070907PROMSG    *LIBL     KEY0003  03
251000070907PROMSG    *LIBL     KEY0004  04
251100070907PROMSG    *LIBL     KEY0005  05
251200070907PROMSG    *LIBL     KEY0006  06
251300070907PROMSG    *LIBL     KEY0007  07
251400070907PROMSG    *LIBL     KEY0008  08
251500070907PROMSG    *LIBL     KEY0009  09
251600070907PROMSG    *LIBL     KEY0010  10
251700070907PROMSG    *LIBL     KEY0011  11
251800070907PROMSG    *LIBL     KEY0012  12
251900070907PROMSG    *LIBL     KEY0013  13
252000070907PROMSG    *LIBL     KEY0014  14
252100070907PROMSG    *LIBL     KEY0015  15
252200070907PROMSG    *LIBL     KEY0016  16
252300070907PROMSG    *LIBL     KEY0017  17
252400070907PROMSG    *LIBL     KEY0018  18
252500070907PROMSG    *LIBL     KEY0019  19
252600070907PROMSG    *LIBL     KEY0020  20
252700070907PROMSG    *LIBL     KEY0021  21
252800070907PROMSG    *LIBL     KEY0022  22
252900070907PROMSG    *LIBL     KEY0023  23
253000070907PROMSG    *LIBL     KEY0024  24
253100070907**  ABILITAZIONE D1 : $FC1 / $FR1
2532000709070N  01
2533000709070N  02
2534000709071N  03
2535000709071N  04
2536000709071N  05
2537000709070N  06
2538000709070N  07
2539000709070N  08
2540000709070N  09
2541000709070N  10
2542000709070N  11
2543000709071N  12
2544000709070N  13
2545000709070N  14
2546000709070N  15
2547000709070N  16
2548000709070N  17
2549000709070N  18
2550000709070N  19
2551000709070N  20
2552000709070N  21
2553000709070N  22
2554000709070N  23   Impostare a '1' se si utilizzano opzioni
2555000709071N  24   Lasciare sempre impostata a '1' il valore della riga 24
255600070907** TASTI DI FUNZIONE  0/3/9 : $FM2 / $FX2
255700070907PROMSG    *LIBL     KEY0001  01
255800070907PROMSG    *LIBL     KEY0002  02
255900070907PROMSG    *LIBL     KEY0003  03
256000070907PROMSG    *LIBL     KEY0004  04
256100070907PROMSG    *LIBL     KEY0005  05
256200070907PROMSG    *LIBL     KEY0006  06
256300070907PROMSG    *LIBL     KEY0007  07
256400070907PROMSG    *LIBL     KEY0008  08
256500070907PROMSG    *LIBL     KEY0009  09
256600070907PROMSG    *LIBL     KEY0010  10
256700070907PROMSG    *LIBL     KEY0011  11
256800070907PROMSG    *LIBL     KEY0012  12
256900070907PROMSG    *LIBL     KEY0013  13
257000070907PROMSG    *LIBL     KEY0014  14
257100070907PROMSG    *LIBL     KEY0015  15
257200070907PROMSG    *LIBL     KEY0016  16
257300070907PROMSG    *LIBL     KEY0017  17
257400070907PROMSG    *LIBL     KEY0018  18
257500070907PROMSG    *LIBL     KEY0019  19
257600070907PROMSG    *LIBL     KEY0020  20
257700070907PROMSG    *LIBL     KEY0021  21
257800070907PROMSG    *LIBL     KEY0022  22
257900070907PROMSG    *LIBL     KEY0023  23
258000070907PROMSG    *LIBL     KEY0024  24
258100070907**  ABILITAZIONE 0/3/9 : $FC2 / $FR2
2582000709070N  01
2583000709070N  02
2584000709071N  03
2585000709070N  04
2586000709071N  05
2587000709071N  06
2588000709070N  07
2589000709070N  08
2590000709070N  09
2591000709070N  10
2592000709070N  11
2593000709071N  12
2594000709070N  13
2595000709070N  14
2596000709070N  15
2597000709070N  16
2598000709070N  17
2599000709070N  18
2600000709070N  19
2601000709070N  20
2602000709070N  21
2603000709070N  22
2604000709070N  23   Impostare a '1' se si utilizzano opzioni
2605000709071N  24   Lasciare sempre impostata a '1' il valore della riga 24
