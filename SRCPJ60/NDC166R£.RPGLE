000100000706     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND NDC166BND) ACTGRP(QILE)
000200981126     H*PARMS OPTION(*NOXREF) CVTOPT(*NONE)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400001228      *
000500001228      *
000600001228C1396 * ATTENZIONE: il video NDC166D è in "share" con il modulo NDC166R1
000700001228  "   *             (che gestisce il subfile prospetto liquidazione), per
000800001229  "   *             cui nel caso di modifica del video è necessario almeno
000900001228C1396 *             ricompilare anche il modulo NDC166R1
001000001228      *
001100940117      *
001200940128     F*----------------------------------------------------*
001300000720C1396X*****NDIVT01L  UF A E           K DISK    RENAME(NDIVT000:NDIVT001)
001400020429C1550 * Soc/TpLiq/Anno/Mese/tipTot
001500000720C1396FNDIVT01L  IF   E           K DISK
001600980720     F*------------
001700020423C1550 * Soc/TpLiq/Anno/PerLiq/TpReg/Libro/Aliq/RifIva/CausTes/CausRig
001800980721    >FNDRIV03L  IF   E           K DISK    RENAME(NDRIV000:NDRIV003)
001900000720C1396X*------------
002000000720C1396X*****ANOPE01L  IF   E           K DISK
002100000720     F*------------
002200951221    >FNDPLF01L  IF   E           K DISK
002300000717C1396F*------------
002400000717C1396FANALR01L  IF   E           K DISK
002500001004C1425FANDLI01L  IF   E           K DISK
002600040520C1762 * Iva di Gruppo: Knraz/TipTotG/Societa/TpLiqS/AnnoS/MeseS
002700040520C1762FNDIVG02L  IF   E           K DISK    RENAME(NDIVG000:NDIVG002)
002800090120C2106FANAIV01L  IF   E           K DISK
002900090120C2106FANAIL02L  IF   E           K DISK
003000950717     F*----------------------------------------------------*
003100950717    >FNDC166D   CF   E             WORKSTN
003200951222     F                                     RENAME(C166D0:FMTD0)
003300020423C1550F                                     RENAME(C166D9:FMTD9)
003400020429C1550F                                     RENAME(C166Z9:FMTZ9)
003500020423     F                                     RENAME(C166D1:FMTD1)
003600950717     F                                     RENAME(C166T1:FMTT1)
003700950717     F                                     RENAME(C166Z1:FMTZ1)
003800000717C1396F                                     RENAME(C166D2_0:FMTD2_0)
003900950717     F                                     RENAME(C166D2:FMTD2)
004000950717     F                                     RENAME(C166Z2:FMTZ2)
004100951113     F                                     RENAME(C166D3:FMTD3)
004200000706     F                                     RENAME(C166D4:FMTD4)
004300000706     F                                     RENAME(C166Z4:FMTZ4)
004400030623C1668F                                     RENAME(C166D7:FMTD7)
004500040726C1781F                                     RENAME(C166D8:FMTD8)
004600940201     F                                     INFDS(DSFMT)
004700940128     D*----------------------------------------------------*
004800951113     D* Schiera per XMSGR
004900981211     D $COS            S             27    DIM(4) CTDATA PERRCD(1)
005000951113     D*-------------
005100940506     D* Schiere per la gestione dei tasti di funzione
005200940506     D $FM             S             27    DIM(24)
005300940506     D $FX             S             50    DIM(24)
005400940506     D $FC             S              1    DIM(24)
005500940506     D $FR             S              1    DIM(24)
005600950717     D* Video D1
005700940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005800940506     D $FX1            S             50    DIM(24) ALT($FM1)
005900940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006000940506     D $FR1            S              1    DIM(24) ALT($FC1)
006100950717     D* Video D2
006200950717     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006300950717     D $FX2            S             50    DIM(24) ALT($FM2)
006400950717     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006500950717     D $FR2            S              1    DIM(24) ALT($FC2)
006600991230     D* Video D4 Valori assunti
006700991230     D $FM4            S             27    DIM(24) CTDATA PERRCD(1)
006800991230     D $FX4            S             50    DIM(24) ALT($FM4)
006900991230     D $FC4            S              1    DIM(24) CTDATA PERRCD(1)
007000991230     D $FR4            S              1    DIM(24) ALT($FC4)
007100020423C1550 * Video D9 Tipo liquidazione
007200020423  "  D $FM9            S             27    DIM(24) CTDATA PERRCD(1)
007300020423  "  D $FX9            S             50    DIM(24) ALT($FM9)
007400020423  "  D $FC9            S              1    DIM(24) CTDATA PERRCD(1)
007500020423C1550D $FR9            S              1    DIM(24) ALT($FC9)
007600940128     D*----------------------------------------------------*
007700940127     D* Passaggio Parametri
007800940127     D KPJBA         E DS
007900940318     D*-------------
008000940318     D* DS Esterna gestione messaggi per COMIT
008100940318     D XMSGDS        E DS
008200940318     D  STS          E                     EXTFLD(MSGSTS)
008300940721     D                                     DIM(333)
008400940318     D  JBA          E                     EXTFLD(MSGJBA)
008500940721     D                                     DIM(502)
008600950301     D*-------------
008700991230     D* Valori assunti
008800991230     D XMPEDS        E DS                  INZ
008900991230     D XMPEDS1       E DS                  INZ
009000991230     D DSMPE           DS                  INZ
009100991230     D  $Val                          1
009200991230    >D  $Bol                          1
009300991230    >D  $StpCau                       1
009400001229C1457D  $ColPrLq                      1
009500020222D1095D  $ProsLiq                      1
009600021010D1095D  $StpTotR                      1
009700030623c1668D  $intesta                      1
009800031128C1703D  $StpLiqG                      1
009900090626C2131D WAnno           S                   Like(D1Anno)
010000090626C2131D WImpInp         S                   Like(ImporD0202)
010100090626C2131D WImpOut         S                   Like(ImporD0202)
010200991230     D*-------------
010300950616    >D* Reperimento Autorizzazioni
010400950616    >D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010500950607     D*-------------
010600950607     D* Dati per pgm XNOAUT
010700950607     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
010800950301     D*-------------
010900950410     D* Passaggio Autorizzazioni
011000950410     D DSAUT           DS           512
011100950410     D*-------------
011200950301     D* Maschera dati autorizzazioni
011300950717    >D CCIVDS        E DS                  EXTNAME(CONCIVDS)
011400950126     D*-------------
011500950126     D* Reperimento nome PGM
011600950126     D STATUS         SDS           333
011700950126     D  DSPGM            *PROC
011800950411     D  PARMS            *PARMS
011900950530     D*-------------
012000941209     D* Dati di ambiente ottenuti da XSOC
012100941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
012200970319     D    XscPlafond                  1    overlay(XscFgo:3)
012300970319     D    XscDare                     1    overlay(XscFgo:7)
012400970319     D    XscAvere                    1    overlay(XscFgo:8)
012500980720     D    XscIvaRgm                   1    overlay(XscFgo:9)
012600981118     D    XscIvaLiq                   1    overlay(XscFgo:10)
012700090626C2131D*----------------
012800090626  "  D* numeri decimali della m.c.
012900090626  "  D NrDecMC         S              1  0
013000090626C2131D WNrDecMC        S              1
013100941209     D*-------------
013200941209     D* DS Interna per dati di output di XSOC
013300941209     D XSOCDS          DS          1000
013400940203     D*-------------
013500950530     D* Passaggio Parametri al PGM di stampa
013600950717    >D C167DS        E DS                  EXTNAME(NDC167DS)
013700950717    >D  ERR167       E                     INZ('0')
013800940201     D*-------------
013900970319     D* Parametri pgm NDC423R
014000970319     D ndc423ds      E DS
014100970319     D*-------------
014200001229C1457D* Ricerca legende
014300001229  "  D A964DS        E DS                  EXTNAME(ANA964DS)
014400001229  "  D*-------------
014500001229  "  D* Controlli validità legende
014600001229C1457X* Decodifica mese
014700951113     D X07DS         E DS                  EXTNAME(X07VALDS)
014800950726     D*-------------
014900030623c1668D* Tabella numeri pagina
015000030623  "  D Ang008Ds      E DS
015100030623  "  D*-------------
015200030623  "  D* Richiamo a XATB
015300030623  "  D XATBDS        E DS                  INZ
015400030623  "  D*-------------
015500030623  "  D* Parametri per ricerca codice ANATB00F
015600030623  "  D XTABDS        E DS                  inz
015700030623  "  D*-------------
015800030623  "  D* Passaggio Parametri al PGM di manutenzione intestazione
015900030623C1668D NDC098DS      E DS                  INZ
016000090626C2131D*-------------
016100090626  "  D* PRORATA: percetuali di detraibilità IVA
016200090626C2131D ANG046DS      E DS
016300030623     D*-------------
016400980720     D* Decodifica periodo
016500980720     D X62DS         E DS                  EXTNAME(X62IVALQDS)
016600980720     D*-------------
016700940201     D DSFMT           DS
016800940506     D  $TASTO               369    369
016900940201     D  NRG                  370    370
017000940201     D  NCL                  371    371
017100950314     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
017200940201     D*-------------
017300940201     D* posizione cursore
017400940201     D CURSOR          DS
017500950530     D  RIRI                   1      2B 0 INZ
017600950530     D  R$$                    2      2
017700940223     D  COCO                   3      4B 0 INZ
017800940201     D  C$$                    4      4
017900940225     D*-------------
018000940506     D* Tasti di funzione
018100940506     D F01             C                   CONST(X'31')
018200940506     D F02             C                   CONST(X'32')
018300940506     D F03             C                   CONST(X'33')
018400940506     D F04             C                   CONST(X'34')
018500940506     D F05             C                   CONST(X'35')
018600940506     D F06             C                   CONST(X'36')
018700940506     D F07             C                   CONST(X'37')
018800940506     D F08             C                   CONST(X'38')
018900940506     D F09             C                   CONST(X'39')
019000940506     D F10             C                   CONST(X'3A')
019100940506     D F11             C                   CONST(X'3B')
019200940506     D F12             C                   CONST(X'3C')
019300940506     D F13             C                   CONST(X'B1')
019400940506     D F14             C                   CONST(X'B2')
019500940506     D F15             C                   CONST(X'B3')
019600940506     D F16             C                   CONST(X'B4')
019700940506     D F17             C                   CONST(X'B5')
019800940506     D F18             C                   CONST(X'B6')
019900940506     D F19             C                   CONST(X'B7')
020000940506     D F20             C                   CONST(X'B8')
020100940506     D F21             C                   CONST(X'B9')
020200940506     D F22             C                   CONST(X'BA')
020300940506     D F23             C                   CONST(X'BB')
020400940506     D F24             C                   CONST(X'BC')
020500940506     D ENTER           C                   CONST(X'F1')
020600950717     D*-------------
020700950717     D* Sottotitolo di testata
020800950530     D SOTTIT          C                   CONST('                            ')
020900950717     D*-------------
021000950717     D* Campi di lavoro
021100981125     D mese            S              2  0
021200981125     D mesepr          S              2  0
021300980720     D meseIn          S              2  0
021400980720     D meseFin         S              2  0
021500980720     D WmeseIn         S              2  0
021600980720     D WmeseFin        S              2  0
021700950717     D anno            S              4  0
021800981125     D annopr          S              4  0
021900980720     D periodo         S              2A
022000951221     D stpplaf         S              1A
022100951222     D stpconf         S              1A
022200951221     D*-------------
022300971008     D* Testi bottoni risposta di XMSGR
022400971008     D TxtBott         C                   CONST('PROMSG    *LIBL-
022500971010     D                                          COS1203')
022600980402     D*----------
022700000802C1396D* Parametro modulo IVA
022800000802  "  D ANP045DS      E DS
022900000802C1396D*-------------
023000980402     D* Parametro modulo liquidazione IVA
023100980402    >D NDC149DS      E DS                  INZ
023200970319     D*-------------
023300980401     D* Parametri di output XPAR
023400980401     D XpaOut          DS          2000
023500980401     D*-------------
023600000518D0576D $ImpNegat       S              1A
023700000802C1396D WSocCap         S                   Like(Cap045)
023800000202C1329D Oggi            S               D
023900000202  "  D CdInter         S                   Like(DVSCdInter)
024000000202  "  D NrDec           S              1  0
024100000202  "  D WImpArr         S                   Like(IVTTotale)
024200000202  "  D WImpArr0D       S             16S 0
024300000202  "  D W3015           S             30S15
024400000202  "  D*-------------
024500000202  "  D* Prototipazione procedure EURO
024600000202  "  D/COPY *LIBL/SRCCPY,PJX002PR
024700000202C1329D*-------------
024800000821C1396D* DS campi ANOPE00F
024900000821  "  D ANOPEDS       E DS                  Inz ExtName(ANOPE00F)
025000000821  "  D*-------------
025100000821  "  D* Definizioni controllo causale
025200000821  "  D Esito           S              1
025300000821  "  D GesMsg          S              1
025400000821  "  D LenOut          S              9B 0
025500000821  "  D StrutturaO      S             10
025600000821  "  D Refresh         S              1
025700000821  "  D Uso             S              1
025800000821  "  D Output          S           4000
025900000821  "  D Causale         S              4
026000000821  "  D*-------------
026100000821  "  D* Parametri modulo NDC166R1
026200000717  "  D Ret166          S              1A
026300000717  "  D Err166          S              1A
026400000717  "  D*-------------
026500000717  "  D* Campi di lavoro per calcolo dati liquidazione
026600000717  "  D TotAcq          S                   Like(RIVImporto)
026700000717  "  D TotVendite      S                   Like(RIVImporto)
026800000717  "  D SalPerPr        S                   Like(RIVImporto)
026900000721  "  D SliPerPr        S                   Like(RIVImporto)
027000000721  "  D SliPerSu        S                   Like(RIVImporto)
027100000717  "  D OpAttive        S                   Like(RIVImporto)
027200000717  "  D OpPassive       S                   Like(RIVImporto)
027300000717  "  D OpAttCesIn      S                   Like(RIVImporto)
027400020524  "  D OpPasAcqIn      S                   Like(RIVImporto)
027500000717C1396D*-------------
027600020927C1583D SalNuovo        S                   Like(RIVImporto)
027700020927C1583 *-------------
027800001004C1425D SAVLibro        S                   Like(DLILibro)
027900001004C1425D*-------------
028000031128C1703 * Iva di gruppo
028100031128  "  D NDC426DS      E DS                  Inz
028200031201  "  D  RET426       E                     INZ('0')
028300031201  "  D  OPR426       E                     INZ('0')
028400031201  "  D  ERR426       E                     INZ('0')
028500031128C1703D $Mese           S              2s 0 inz
028600040520C1762 * memorizza se la liq. è stata trasferita in Iva di Gruppo
028700040520C1762D $LiqIvaG        S              1a   inz('0')
028800040726C1781 *-------------
028900091218C1781D $TpTesto167     S                   like(TpTesto167) inz
029000091218C2149D $TpTestG167     S                   like(TpTesto167) inz
029100091218C1781 *-------------
029200040726  "   * DS esterna x ricerca testo lettera
029300040726  "  D Ana720DS      E DS                  INZ
029400040726  "   *-------------
029500040726  "   * Variabili per chiamata a X72Txt
029600040726  "  D XEsito          S              1A
029700040726  "  D XOpe            S              1A
029800040726  "  D XSocieta        S              3A
029900040726  "  D XTpTesto        S              2A
030000040726  "  D XUnita          S              8A
030100040726  "  D XLingua         S              1A
030200040726  "  D XClaUte         S              2A
030300040726  "  D XPosRig         S              1A
030400040726  "  D XNrRiga         S              9P 0
030500040726  "  D XRibat          S              1A
030600040726  "  D XTesRig         S             70A
030700040726  "  D XNrChiavi       S              1  0
030800040726C1781D XUsoUnita       S              1A
030900090120C2106 *-------------
031000090120C2106D* Ricerca codici attività IVA
031100090120C2106D ANA220DS      E DS
031200940127     C*----------------------------------------------------*
031300940127     C*                MAIN LINE PROGRAM
031400940127     C*----------------------------------------------------*
031500940127     C*
031600940223     C                   EXSR      INZVAR
031700940223     C*
031800940223     C     $FINE         DOWEQ     *OFF
031900020423C1550C     $GEST         CASEQ     'D9'          GESD9
032000020423     C     $GEST         CASEQ     'D1'          GESD1
032100950616    >C     $GEST         CASEQ     'D2'          GESD2
032200991230     C     $GEST         CASEQ     'D4'          GESD4
032300941027     C                   ENDCS
032400941027     C                   ENDDO
032500940201     C*
032600940325     C* fine programma
032700940325     C                   EXSR      ENDPGM
032800940325     C/EJECT
032900940325     C************************************************************
033000950717     C* FINE PROGRAMMA
033100940325     C************************************************************
033200940325     C     ENDPGM        BEGSR
033300941214     C*
033400941214     C* se ho confermato l'uscita proseguo
033500941214     C     $FINE         IFEQ      *ON
033600940325     C*
033700950606     C                   SETON                                            LR
033800940616     C                   RETURN
033900941214     C*
034000941214     C                   ENDIF
034100940325     C*
034200940325     C                   ENDSR
034300940325     C/EJECT
034400940201     C************************************************************
034500940201     C* RICERCHE
034600940201     C************************************************************
034700940127     C     SEARCH        BEGSR
034800940127     C*
034900940127     C* determino Riga/Colonna del cursore
035000940127     C                   MOVE      NRG           R$$
035100940127     C                   MOVE      NCL           C$$
035200940127     C                   Z-ADD     RIRI          CURR
035300940127     C                   Z-ADD     COCO          CURC
035400940127     C*
035500940224     C* I campi che al momento dell'F4 risultano protetti non devono
035600940224     C* essere abilitati alla funzione di ricerca
035700941027     C*
035800941027     C                   SELECT
035900020429C1550 *
036000020430  "   * ricerca tipo liquidazione
036100020429  "  C                   When      (H1NmFl = 'D9TPLIQ'
036200020429  "  C                              or H1NmFl = 'D1TPLIQ'
036300020429  "  C                              or H1NmFl = 'D2TPLIQ')
036400020429  "  C                             and
036500020429  "  C                             XScIvaRgm = '2'
036600020429  "  C                   Reset                   A964DS
036700020430  "   *  posso scegliere con l'F4 il tipo liq solo dal D9
036800020429  "  C                   movel     '01'          Opz964
036900020429  "  C                   if         H1NmFl = 'D1TPLIQ'
037000020429  "  C                              or H1NmFl = 'D2TPLIQ'
037100020429  "  C                   movel     '05'          Opz964
037200020429  "  C                   endif
037300020429  "  C                   movel     'ANDIZ000'    Trc964
037400020429  "  C                   movel     'DSPIVALIQ'   Cam964
037500020429  "  C                   movel     A964DS        Kpjbu
037600020429  "  C                   call      'ANA964R'
037700020429  "  C                   parm                    Kpjba
037800020429  "  C                   movel     Kpjbu         A964DS
037900020429  "  C                   if        Val964 <> *blank
038000020429  "  C                             and H1NmFl = 'D9TPLIQ'
038100020429  "  C                   movel     Val964        D9TpLiq
038200020429  "  C                   clear                   D9TpLiqD
038300020429  "  C                   endif
038400020429C1550 *
038500001229C1457C*
038600001229  "  C* Stampa colonne D/A prospetto liquidazione
038700001229  "  C     H1NMFL        WHENEQ    'D2COLPRLQ'
038800001229  "  C                   Clear                   D2ColPrLqD
038900001229  "  C                   RESET                   A964DS
039000001229  "  C                   MOVEL     '01'          OPZ964
039100001229  "  C                   MOVEL     'ANDIZ000'    TRC964
039200001229  "  C                   MOVEL     'DSPDASXDX'   CAM964
039300001229  "  C                   MOVEL     *OFF          ALL964
039400001229  "  C                   MOVEL     A964DS        KPJBU
039500001229  "  C                   CALL      'ANA964R'
039600001229  "  C                   PARM                    KPJBA
039700001229  "  C                   MOVEL     KPJBU         A964DS
039800001229  "  C     VAL964        IFNE      *BLANK
039900001229  "  C                   MOVEL     VAL964        D2ColPrLq
040000001229  "  C                   MOVEL     DES964        D2ColPrLqD
040100001229C1457C                   ENDIF
040200040726C1781 *
040300040727  "   * Tipo testo
040400091218C1781C     H1NMFL        WHENEQ    'D8TPTESTO'
040500100601C2146x**** H1NMFL        oreq      'D8TPTESTOG'
040600100601C2149C     H1NMFL        oreq      'D8TPTESTOG'
040700040726C1781C                   exsr      Search_TpTesto
040800090120C2106C*
040900090120  "  C* Codice attività IVA
041000090120  "  C     H1NMFL        WHENEQ    'D1CDATT'
041100090120  "  C                   CLEAR                   KPJBU
041200090120  "  C                   RESET                   ANA220DS
041300090120  "  C                   MOVE      '01'          OPZ220
041400090120  "  C                   MOVEL     ANA220DS      KPJBU
041500090120  "  C                   CALL      'ANA220R'
041600090120  "  C                   PARM                    KPJBA
041700090120  "  C                   MOVEL     KPJBU         ANA220DS
041800090120  "  C     CDA220        IFNE      *BLANK
041900090120  "  C                   move      CDA220        D1CDATT
042000090120C2106C                   ENDIF
042100940223     C*
042200940223     C* ricerca non abilitata in questa posizione
042300941027     C                   OTHER
042400001229C1457X* imposto pos. del cursore
042500001229  "  X*****              Z-ADD     CURR          H1RIGA
042600001229C1457X*****              Z-ADD     CURC          H1COLO
042700940926     C                   SETON                                        9899
042800940127     C*
042900941027     C                   ENDSL
043000001229C1457C*
043100001229  "  C* imposto pos. del cursore
043200001229  "  C                   Z-ADD     CURR          H1RIGA
043300001229C1457C                   Z-ADD     CURC          H1COLO
043400940127     C*
043500940506     C                   MOVE      *HIVAL        $LASTV
043600000201     C*
043700940127     C                   ENDSR
043800940225     C/EJECT
043900040726C1781 ************************************************************
044000040726  "   * Ricerca tipo testo
044100040726  "   ************************************************************
044200040726  "  C     Search_TpTestoBegsr
044300040726  "   *
044400040726  "  C                   reset                   Ana720Ds
044500040726  "  C                   movel     '01'          Opz720
044600040726  "  C                   movel     XScLin        Lingua720
044700040726  "  C                   movel(p)  Ana720Ds      Kpjbu
044800040726  "  C                   call      'ANA720R'
044900040726  "  C                   parm                    Kpjba
045000040726  "  C                   movel     Kpjbu         Ana720Ds
045100091218C1781C                   if        TpTxt720 <> *blanks
045200091218C2149c                   if        h1NmFl='D8TPTESTO'
045300091218C1781C                   movel     TpTxt720      D8TpTesto
045400091218C2149C                   else
045500091218  "  C                   movel     TpTxt720      D8TpTestoG
045600091218C2149C                   endif
045700091218C1781C                   endif
045800040726  "   *
045900040726C1781C     XSearch_TpTestEndsr
046000940225     C************************************************************
046100940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
046200940225     C************************************************************
046300940225     C     GESPIE        BEGSR
046400940225     C*
046500940225     C                   SELECT
046600940225     C*
046700940506     C     $GEST         WHENEQ    'D1'
046800940506     C     $ULKD1        IFEQ      0
046900940506     C* riempimento opzioni
047000940506     C                   EXSR      F24D1
047100940506     C                   ELSE
047200940506     C                   WRITE     FMTZ1
047300940506     C                   ENDIF
047400940225     C*
047500940926     C     $GEST         WHENEQ    'D2'
047600950717     C     $ULKD2        IFEQ      0
047700940926     C* riempimento opzioni
047800950717     C                   EXSR      F24D2
047900940926     C                   ELSE
048000950717     C                   WRITE     FMTZ2
048100940926     C                   ENDIF
048200991230     C*
048300991230     C* Modifica valori assunti
048400991230     C     $GEST         WHENEQ    'D4'
048500991230     C     $ULKD4        IFEQ      0
048600991230     C                   EXSR      F24D4
048700991230     C                   ELSE
048800991230     C                   WRITE     FMTZ4
048900991230     C                   ENDIF
049000950717     C*
049100020423C1550 * Gestione tipo liquidazione
049200020423  "  C     $GEST         WHENEQ    'D9'
049300020423  "  C     $ULKD9        IFEQ      0
049400020423  "  C                   EXSR      F24D9
049500020423  "  C                   ELSE
049600020423  "  C                   WRITE     FMTZ9
049700020423  "  C                   ENDIF
049800020423C1550 *
049900940225     C                   ENDSL
050000940225     C*
050100940225     C                   ENDSR
050200940131     C/EJECT
050300951222     C************************************************************
050400951222     C* GESTIONE FINESTRA D0
050500951222     C************************************************************
050600951222     C     GESD0         BEGSR
050700951222     C*
050800951222     C                   CLEAR                   FMTD0
050900000201     C                   z-add     1             d0tpela
051000951222     C     *in99         doueq     *off
051100951222     C                   EXFMT     FMTD0
051200000718C1396C                   Eval      XMSGINZ = *On
051300951222     C                   setoff                                       99
051400951222     C                   if        *inkc = *on
051500951222     C                   move      *on           $fine
051600951222     C                   EXSR      ENDPGM
051700951222     C                   endif
051800000201     C                   if        d0tpela = 0
051900951222     C                   seton                                        5099
052000951222     C                   endif
052100951222     C                   enddo
052200951222     C*
052300951222     C                   ENDSR
052400951222     C/EJECT
052500951222     C************************************************************
052600951222     C* GESTIONE FINESTRA D3
052700951222     C************************************************************
052800951222     C     GESD3         BEGSR
052900951222     C*
053000951222     C                   CLEAR                   FMTD3
053100970319     C*
053200970319     C     XScPlafond    ifeq      '1'
053300970319     C* fisso
053400970319     C                   seton                                        11
053500970319     C                   else
053600970319     C* mobile
053700970319     C                   setoff                                       11
053800970319     C                   endif
053900970319     C*
054000951222     C                   move      xscoff        d3stpdef
054100951222     C                   move      xscon         d3on1
054200951222     C                   move      xscoff        d3of1
054300970319     C*
054400951222     C                   RESET                   X07DS
054500951222     C                   MOVE      '1'           X07RIC
054600951222     C                   MOVEL     'ANDIZ000'    X07TRC
054700951222     C                   MOVEL     'DSPMESE'     X07CAM
054800951222     C                   MOVEL     d1mese        X07VAL
054900951222     C                   MOVE      *OFF          X07ALL
055000961008     C                   CALLb     'X07VALR'
055100951222     C                   PARM                    X07DS
055200951222   B3C     X07ERR        IFEQ      '0'
055300951222     C                   MOVEL     X07DES        d3mese1
055400951222     C                   MOVEL     X07DES        d3mese2
055500951222   X3C                   ELSE
055600951222     C                   MOVE      d1mese        d3mese1
055700951222     C                   MOVE      d1mese        d3mese2
055800951222   E3C                   ENDIF
055900951222     C                   move      d1anno        d3anno1
056000951222     C                   move      d1anno        d3anno2
056100970319     C*
056200951222     C                   z-add     plfimpeff     d3acqeff
056300970320     C                   z-add     plfplafond    d3plafond
056400970319     C*
056500970319     C     XScPlafond    ifeq      '1'
056600970319     C                   EXSR      RepFisso
056700970319     C                   else
056800970319     C                   EXSR      RepMobile
056900970319     C                   endif
057000970319     C*
057100951222     C     *in99         doueq     *off
057200000718C1396C     *inkf         andeq     *on
057300951222     C     *inkl         oreq      *on
057400951222     C                   EXFMT     FMTD3
057500000718C1396C                   Eval      XMSGINZ = *On
057600951222     C                   setoff                                       99
057700951222     C                   if        *inkl = *on
057800951222     C                   move      *off          stpconf
057900951222     C                   else
058000951222     C                   move      *on           stpconf
058100951222     C                   if        d3stpdef <> xscon and d3stpdef <> xscoff
058200951222     C                   seton                                        6099
058300951222     C                   endif
058400951222     C                   endif
058500951222     C                   enddo
058600951222     C*
058700951222     C                   ENDSR
058800951222     C/EJECT
058900940131     C************************************************************
059000940510     C* GESTIONE VIDEO RECORD D1
059100940131     C************************************************************
059200940127     C     GESD1         BEGSR
059300940225     C* inizializzazione videata
059400941117   B1C     $INZD1        IFEQ      *ON
059500940127     C                   EXSR      INZD1
059600940223     C                   MOVE      *OFF          $INZD1
059700941117   E1C                   ENDIF
059800940225     C* emissione piede videata se proveniente da altra
059900940225     C* salvataggio valore formato di provenienza
060000941117   B1C     $LASTV        IFNE      'D1'
060100940207     C                   WRITE     FMTT1
060200940225     C                   EXSR      GESPIE
060300940202     C                   MOVE      $LASTV        $LASTG
060400940202     C                   MOVE      'D1'          $LASTV
060500941117   E1C                   ENDIF
060600940707     C*
060700940707     C* se esistono errori sulla videata
060800940707     C* emetto la write del formato a indicatori spenti per vedere
060900940707     C* le eventuali decodifiche
061000941117   B1C     *IN99         IFEQ      *ON
061100940707     C     $GEST         ANDEQ     'D1'
061200940707     C                   MOVEA     *IN           WIN
061300940707     C                   MOVE      *ALL'0'       IN5098           49
061400940707     C                   MOVEA     IN5098        *IN(50)
061500940707     C                   WRITE     FMTD1
061600940707     C                   MOVEA     WIN           *IN
061700941117   E1C                   ENDIF
061800940131     C*
061900940203     C                   EXFMT     FMTD1
062000000718C1396C                   Eval      XMSGINZ = *On
062100950717     C*
062200940506     C* controllo abilitazione tasti funzione
062300940506     C                   EXSR      CTRKD1
062400940506     C*
062500941117   B1C     *IN99         IFEQ      *OFF
062600940131     C*
062700941117   B2C                   SELECT
062800950717     C* F2=Decodifica
062900950717     C     $TASTO        WHENEQ    F02
063000950717     C                   EXSR      F02D1
063100940131     C* F3=Fine
063200940506     C     $TASTO        WHENEQ    F03
063300940309     C                   EXSR      F03D1
063400940131     C* F4=Ricerca
063500940506     C     $TASTO        WHENEQ    F04
063600940127     C                   EXSR      SEARCH
063700940224     C* F5=Refresh
063800940506     C     $TASTO        WHENEQ    F05
063900940309     C                   EXSR      F05D1
064000951129     C* F12=Ritorno
064100951129     C     $TASTO        WHENEQ    F12
064200951129     C                   EXSR      F12D1
064300030414D1320C* F21=Pers. batch
064400030414  "  C     $TASTO        WHENEQ    F21
064500030414D1320C                   EXSR      F21D1
064600940315     C* F24=Altri tasti
064700940506     C     $TASTO        WHENEQ    F24
064800940315     C                   EXSR      F24D1
064900940131     C*
065000941117   X2C                   OTHER
065100950717     C*
065200940127     C                   EXSR      CTRD1
065300950717     C*
065400950530    >C     *IN99         IFEQ      *OFF
065500951222     C                   move      *on           stpconf
065600000802C1396C                   move      *off          stpplaf
065700951222     C*
065800000719C1396X* Richiesta stampa solo plafond
065900080423c1396X*****d0tpela       ifeq      2
066000080423C2089c* se non chiesta stampa senza plafond
066100080423C2089c                   if        D0tpEla <> 3
066200080423C1396C* Se gestito plafond ...
066300000718  "  C                   if        XScPlafond = '1' or XScPlafond = '2'
066400000718  "  C* Verifica esistenza plafond per il periodo:
066500000718C1396C*  se esiste emissione finestra plafond, se non esiste avvertimento
066600951221     C                   EXSR      WINPLF
066700000718C1396C* Se richiesta stampa solo plafond ...
066800951222     C                   if        stpconf = *on
066900000718C1396C                             and d0tpela = 2
067000951221     C* Eseguo lancio stampa riepiloghi solo plafond
067100951221     C                   move      '2'           stpplaf
067200000720C1396C                   Move      XScSoc        Soc167
067300000720  "  C                   Move      D1Anno        Ann167
067400000720C1396C                   Move      D1Mese        Per167
067500951221     C                   EXSR      EXELAN
067600951221     C* Fine programma
067700951221     C                   move      *on           $fine
067800951221     C                   EXSR      ENDPGM
067900951222     C                   endif
068000951221     C*
068100000718C1396X*****              else
068200000718C1396C                   endif
068300080423c2089C                   endif
068400951221     C*
068500951221     C* Gestione video D2
068600000718C1396C                   if        stpconf = *on
068700951221     C                   MOVE      *ON           $INZD2
068800951221    >C                   MOVE      'D2'          $GEST
068900000718     C                   endif
069000951221     C*
069100950530    >C                   ENDIF
069200940203     C*
069300941117   E2C                   ENDSL
069400940224     C*
069500941117   E1C                   ENDIF
069600941027     C*
069700940117     C                   ENDSR
069800941209     C/EJECT
069900941209     C************************************************************
070000950717     C* INIZIALIZZAZIONE VIDEATA D1
070100941209     C************************************************************
070200940127     C     INZD1         BEGSR
070300940131     C*
070400940131     C                   CLEAR                   FMTD1
070500940225     C*
070600020423C1550 * Se regime iva trasportatori mista
070700020423  "   * visualizzo 'tipo liquidazione'
070800020423  "   * (già impostato in D9)
070900020423  "  C                   if        XScIvaRgm = '2'
071000020423  "  C                   eval      *in15 = '1'
071100020423  "  C                   move      D9TpLiq       D1TpLiq
071200020429  "  C                   move      D9TpLiqD      D1TpLiqD
071300020430  "  C                   else
071400020430  "  C                   eval      *in15 = '0'
071500020430  "  C                   move      *blank        D1TpLiq
071600020430  "  C                   move      *blank        D1TpLiqD
071700020430  "  C                   endif
071800020423C1550 *
071900000202     C* Liquidazione:
072000000202     C                   If        XscIvaLiq = '1'
072100000202     C* - "mese"
072200000202     C                   SetOff                                       13
072300000202     C                   Else
072400000202     C* - "periodo"
072500000202     C                   SetOn                                        13
072600000202     C                   Endif
072700090120C2106C*
072800090120  "  C* Attiva indicatore 45 se gestite più attività IVA
072900090120  "  C                   If         GesPlur045 = *on
073000090120  "  C                   SetOn                                        45
073100090120C2106C                   EndIf
073200000202     C*
073300950717     C* Presento a video l'ultimo periodo memorizzato
073400020423C1550X*****xscsoc        setgt     ndriv03l
073500020423C1550X*****xscsoc        readpe    ndriv03l                               21
073600020423C1550 * con il tipo liquidazione richiesto (se regime iva mista)
073700020423  "   * (se regime iva non è traportatori mista, il tipo liq è vuoto)
073800020423  "  C                   move      XScSoc        RIVSoc                   21
073900020423  "  C                   move      D1TpLiq       RIVTpLiq
074000020423  "  C     K2riv3        setgt     ndriv03l
074100020423C1550C     K2riv3        readpe    ndriv03l                               21
074200980720   B1C     *IN21         IFEQ      *OFF
074300950717     C                   move      rivanno       d1anno
074400000202   B2C                   If        XscIvaLiq = '1'
074500980721     C                   move      rivmese       d1mese
074600000202   X2C                   Else
074700980720     C                   move      rivperliq     d1perliq
074800000202   E2C                   EndIf
074900980720   X1C                   ELSE
075000950717     C                   z-add     0             d1anno
075100000202   B2C                   If        XscIvaLiq='1'
075200950717     C                   z-add     0             d1mese
075300000202   X2C                   Else
075400990802     C                   z-add     0             d1perliq
075500000202   E2C                   EndIf
075600980720   E1C                   ENDIF
075700950726     C*
075800000202     C* Decodifica mese (liq. mensile)
075900000202     C                   If        XscIvaLiq = '1' and
076000000202     C                             *in21 = *off
076100950726     C                   RESET                   X07DS
076200950726     C                   MOVE      '1'           X07RIC
076300950726     C                   MOVEL     'ANDIZ000'    X07TRC
076400950726     C                   MOVEL     'DSPMESE'     X07CAM
076500950726     C                   MOVEL     d1mese        X07VAL
076600950726     C                   MOVE      *OFF          X07ALL
076700961008     C                   CALLb     'X07VALR'
076800950726     C                   PARM                    X07DS
076900950726   B3C     X07ERR        IFEQ      '0'
077000950726     C                   MOVEL     X07DES        d1mesed
077100950726   X3C                   ELSE
077200950726     C                   MOVE      *blank        d1mesed
077300950726   E3C                   ENDIF
077400980720     C                   EndIf
077500950530     C*
077600000202     C* Decodifica periodo (liq. non mensile)
077700000202     C                   If        XscIvaLiq <> '1' and
077800000202     C                             *in21 = *off
077900980720     C                   RESET                   X62DS
078000980720     C                   MOVEL     XscSoc        X62SOCIETA
078100980720     C                   MOVEL     d1anno        X62ANNO
078200980720     C                   MOVEL     d1perliq      X62PERLIQ
078300000202     C                   z-add     0             x62mese
078400000202     C                   z-add     0             x62giorno
078500980720     C                   CALL      'X62IVALQ'
078600980720     C                   PARM                    X62DS
078700980720   B3C     X62ESITO      IFEQ      '0'
078800980720     C                   MOVEL     X62PERLIQD    d1PERLIQD
078900980720   X3C                   ELSE
079000980720     C                   MOVE      *blank        d1perliqd
079100980720   E3C                   ENDIF
079200980720     C                   EndIf
079300000202     C*
079400940117     C                   ENDSR
079500940207     C/EJECT
079600940916     C************************************************************
079700940916     C* GESTIONE F02 VIDEO D1
079800940916     C************************************************************
079900940916     C     F02D1         BEGSR
080000940916     C*
080100940916     C* per decodificare la videata richiamo la routine dei ctrl
080200940916     C* e poi disattivo gli eventuali messaggi di errore
080300940916     C                   EXSR      CTRD1
080400940916     C*
080500940916     C                   MOVE      *ALL'0'       IN5098
080600940916     C                   MOVEA     IN5098        *IN(50)
080700940916     C*
080800940916     C                   ENDSR
080900940309     C/EJECT
081000940309     C************************************************************
081100940309     C* GESTIONE F03 VIDEO D1
081200940309     C************************************************************
081300940309     C     F03D1         BEGSR
081400940309     C*
081500940309     C                   MOVE      *ON           $FINE
081600940325     C* fine programma
081700940325     C                   EXSR      ENDPGM
081800940325     C*
081900940325     C                   ENDSR
082000940309     C/EJECT
082100940309     C************************************************************
082200940309     C* GESTIONE F05 VIDEO D1
082300940309     C************************************************************
082400940309     C     F05D1         BEGSR
082500940309     C*
082600940309     C                   MOVE      *ON           $INZD1
082700940309     C*
082800940309     C                   ENDSR
082900940309     C/EJECT
083000951129     C************************************************************
083100951129     C* GESTIONE F12 VIDEO D1
083200951129     C************************************************************
083300951129     C     F12D1         BEGSR
083400951129     C*
083500020429C1550 * se regime iva trasp. mista, devo ritornare alla videata
083600020429  "   * che gestisce il tipo liquidazione
083700020429C1550C                   if        XScIvaRgm <> '2'
083800020429     C                   MOVE      *ON           $FINE
083900020429C1550C                   else
084000040520  "  C                   move      'D9'          $GEST
084100020429C1550C                   endif
084200951129     C*
084300951129     C                   ENDSR
084400951129     C/EJECT
084500030414D1320C************************************************************
084600030414  "  C* GESTIONE F21 VIDEO D1
084700030414D1320C************************************************************
084800030414     C     F21D1         BEGSR
084900030414     C*
085000030414     C                   MOVE      'C167'        KCOAZ
085100030414     C                   CALL      'BCH09'
085200030414     C                   PARM                    KPJBA
085300030414     C*
085400030414D1320C                   ENDSR
085500940309     C************************************************************
085600940309     C* GESTIONE F24 VIDEO D1
085700940309     C************************************************************
085800940309     C     F24D1         BEGSR
085900940506     C*
086000940506     C                   MOVEA     $FX1          $FX
086100940506     C                   MOVEA     $FC1          $FC
086200940506     C                   MOVEA     $FR1          $FR
086300940506     C                   Z-ADD     $ULKD1        $ULK
086400950530     C                   Z-ADD     79            $L01
086500950530     C                   Z-ADD     79            $L02
086600940506     C                   EXSR      RIEKEY
086700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
086800940720     C                   MOVEA     $FC           $FC1
086900940506     C                   MOVEL     $KEY1         Z1KE1
087000060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
087100940506     C                   MOVEL     $KEY2         Z1KE2
087200940506     C                   WRITE     FMTZ1
087300940506     C*
087400940309     C                   ENDSR
087500950530     C/EJECT
087600940131     C************************************************************
087700950717     C* CONTROLLO VIDEATA D1
087800940131     C************************************************************
087900940127     C     CTRD1         BEGSR
088000940131     C*
088100940127     C                   SETOFF                                       99
088200940127     C* non imposto pos. del cursore
088300940127     C                   Z-ADD     0             H1RIGA
088400940127     C                   Z-ADD     0             H1COLO
088500950717     C*
088600950717     C* Controllo validità anno
088700950717     C     d1anno        ifeq      0
088800950717     C                   seton                                        9950
088900950717     C                   endif
089000950717     C*
089100000202     C* Controllo validità e decodifica mese (liq. mensile)
089200040520C1762C                   exsr      CtrD1Mese
089300040520C1762 *
089400040520C1762X**specifiche qui asteriscate e riunite in CtrD1Mese
089500040513  "  X****               If        XscIvaLiq = '1'
089600040513  "  X****               MOVE      *blank        d1mesed
089700040513  "  X**** d1mese        iflt      1
089800040513  "  X**** d1mese        orgt      12
089900040513  "  X****               seton                                        9951
090000040513  "  X****               else
090100040513  "  X****               RESET                   X07DS
090200040513  "  X****               MOVE      '1'           X07RIC
090300040513  "  X****               MOVEL     'ANDIZ000'    X07TRC
090400040513  "  X****               MOVEL     'DSPMESE'     X07CAM
090500040513  "  X****               MOVEL     d1mese        X07VAL
090600040513  "  X****               MOVE      *OFF          X07ALL
090700040513  "  X****               CALLb     'X07VALR'
090800040513  "  X****               PARM                    X07DS
090900040513  "  X**** X07ERR        IFEQ      '0'
091000040513  "  X****               MOVEL     X07DES        d1mesed
091100040513  "  X****               ENDIF
091200040513  "  X****               endif
091300040520C1762X****               EndIf
091400000202     C*
091500000202     C* Controllo validità e decodifica periodo (liq. non mensile)
091600040520C1762C                   exsr      CtrD1PerLiq
091700040520C1762 *
091800040520C1762X**specifiche qui asteriscate e riunite in CtrD1Mese
091900040513  "  X****               If        XscIvaLiq <> '1'
092000040513  "  X****               MOVE      *blank        d1perliqd
092100040513  "  X**** d1perliq      iflt      0
092200040513  "  X**** d1perliq      orgt      12
092300040513  "  X****               seton                                        9953
092400040513  "  X****               else
092500040513  "  X****               RESET                   X62DS
092600040513  "  X****               MOVEL     XscSoc        X62SOCIETA
092700040513  "  X****               MOVEL     d1anno        X62ANNO
092800040513  "  X****               MOVEL     d1perliq      X62PERLIQ
092900040513  "  X****               z-add     0             x62mese
093000040513  "  X****               z-add     0             x62giorno
093100040513  "  X****               CALL      'X62IVALQ'
093200040513  "  X****               PARM                    X62DS
093300040513  "  X**** X62ESITO      IFEQ      '0'
093400040513  "  X****               MOVEL     X62PERLIQD    d1PERLIQD
093500040513  "  X****               Extrct    x62dtinper:*m mesein
093600040513  "  X****               Extrct    x62dtfiper:*m mesefin
093700040513  "  X****               ELSE
093800040513  "  X****               seton                                        9953
093900040513  "  X****               ENDIF
094000040513  "  X****               endif
094100040520C1762X****               EndIf
094200950530     C*
094300950717     C* Controllo esistenza periodo su NDRIV
094400040520C1762C                   exsr      CtrD1NdrivOk
094500040520C1762 *
094600040520C1762X**specifiche qui asteriscate e riunite in CtrD1Mese
094700040513  "  X**** *in99         ifeq      *off
094800040513  "  X***ex C1936 * d0tpela       andne     2
094900040513  "  X****               move      xscsoc        rivsoc
095000040513  "  X****               move      d1anno        rivanno
095100040513  "  X****               If        XscIvaLiq='1'
095200040513  "  X****               move      d1mese        rivPerliq
095300040513  "  X****               Else
095400040513  "  X****               move      d1perliq      rivPerliq
095500040513  "  X****               Endif
095600040513  "  X****ex C1550 **    move      *blank        RIVTpLiq
095700040513  "  X****ex C1550 **    if        XScIvaRgm = '2'
095800040513  "  X****ex C1550 **    move      D1TpLiq       RIVTpLiq
095900040513  "  X****ex C1550 **    endif
096000040513  "  X****ex C1550X**k3riv3        chain     ndriv03l                           21
096100040513  "  X****ex C1550 **Soc/TpLiq/Anno/PerLiq                             1
096200040513  "  X****ex C1550 **k4riv3        chain     ndriv03l                           21
096300040513  "  X**** *in21         ifeq      *on
096400040513  "  X**L'errore non è più bloccante, ma viene emessa window di avvertimento
096500040513  "  X**(serve per presentare la liquidazione iva del periodo anche se in
096600040513  "  X***questo non ci sono movimenti riportando sul periodo corrente gli
096700040513  "  X***eventuali progressivi del periodo precedente)
096800040513  "  X****               EXSR      EMTWIN3
096900040513  "  X****               if        MSGRTN = 'A'
097000040513  "  X****               seton                                        99
097100040513  "  X****               endif
097200040513  "  X****               endif
097300040520C1762X****               endif
097400940224     C*
097500040520C1762X****************** ENDSR
097600090120C2106C* Codice attività
097700090120  "  C                   If         GesPlur045 = *on
097800090120  "  C     K2AIV1        CHAIN     ANAIV01L                           27
097900090120  "  C                   IF        (*IN27 = *ON)
098000090120  "  C                   SETON                                        9946
098100090120  "  C                   EVAL      AIVDESBREV = *BLANK
098200090120  "  C                   ENDIF
098300090120C2106C                   ENDIF
098400040520C1762C     XCtrD1        ENDSR
098500040513  "   ************************************************************
098600040513  "   * Controllo validità e decodifica mese (liq. mensile)
098700040513  "  X* Specifiche che erano in CtrD1 e qui riunite
098800040513  "   ************************************************************
098900040513  "  C     CtrD1Mese     Begsr
099000040520C1762 *
099100040513     C                   If        XscIvaLiq = '1'
099200040513     C                   MOVE      *blank        d1mesed
099300040513     C     d1mese        iflt      1
099400040513     C     d1mese        orgt      12
099500040513     C                   seton                                        9951
099600040513     C                   else
099700040513     C                   RESET                   X07DS
099800040513     C                   MOVE      '1'           X07RIC
099900040513     C                   MOVEL     'ANDIZ000'    X07TRC
100000040513     C                   MOVEL     'DSPMESE'     X07CAM
100100040513     C                   MOVEL     d1mese        X07VAL
100200040513     C                   MOVE      *OFF          X07ALL
100300040513     C                   CALLb     'X07VALR'
100400040513     C                   PARM                    X07DS
100500040513     C     X07ERR        IFEQ      '0'
100600040513     C                   MOVEL     X07DES        d1mesed
100700040513     C                   ENDIF
100800040513     C                   endif
100900040513     C                   EndIf
101000040513     C*
101100040520C1762C     XCtrD1Mese    Endsr
101200040513  "   ************************************************************
101300040513  "   * Controllo validità e decodifica periodo (liq. non mensile)
101400040513  "  X* Specifiche che erano in CtrD1 e qui riunite
101500040513  "   ************************************************************
101600040513  "  C     CtrD1PerLiq   Begsr
101700040520C1762 *
101800040513     C* Controllo validità e decodifica periodo (liq. non mensile)
101900040513     C                   If        XscIvaLiq <> '1'
102000040513     C                   MOVE      *blank        d1perliqd
102100040513     C     d1perliq      iflt      0
102200040513     C     d1perliq      orgt      12
102300040513     C                   seton                                        9953
102400040513     C                   else
102500040513     C                   RESET                   X62DS
102600040513     C                   MOVEL     XscSoc        X62SOCIETA
102700040513     C                   MOVEL     d1anno        X62ANNO
102800040513     C                   MOVEL     d1perliq      X62PERLIQ
102900040513     C                   z-add     0             x62mese
103000040513     C                   z-add     0             x62giorno
103100040513     C                   CALL      'X62IVALQ'
103200040513     C                   PARM                    X62DS
103300040513     C     X62ESITO      IFEQ      '0'
103400040513     C                   MOVEL     X62PERLIQD    d1PERLIQD
103500040513     C                   Extrct    x62dtinper:*m mesein
103600040513     C                   Extrct    x62dtfiper:*m mesefin
103700040513     C                   ELSE
103800040513     C                   seton                                        9953
103900040513     C                   ENDIF
104000040513     C                   endif
104100040513     C                   EndIf
104200040520C1762 *
104300040513  "  C     XCtrD1PerLiq  Endsr
104400040513  "   ************************************************************
104500040513  "   * Controllo esiste riepilogativo per il periodo chiesto
104600040513  "  X* Specifiche che erano in CtrD1 e qui riunite
104700040513  "   ************************************************************
104800040513  "  C     CtrD1NdrivOk  Begsr
104900040520C1762 *
105000040513     C     *in99         ifeq      *off
105100040513C1936C     d0tpela       andne     2
105200040513     C                   move      xscsoc        rivsoc
105300040513     C                   move      d1anno        rivanno
105400040513     C                   If        XscIvaLiq='1'
105500040513     C                   move      d1mese        rivPerliq
105600040513     C                   Else
105700040513     C                   move      d1perliq      rivPerliq
105800040513     C                   Endif
105900040513C1550C                   move      *blank        RIVTpLiq
106000040513  "  C                   if        XScIvaRgm = '2'
106100040513  "  C                   move      D1TpLiq       RIVTpLiq
106200040513C1550C                   endif
106300040513C1550X*****k3riv3        chain     ndriv03l                           21
106400040520C1550 * Soc/TpLiq/Anno/PerLiq                                          1
106500040520C1550C     k4riv3        chain     ndriv03l                           21
106600040520     C     *in21         ifeq      *on
106700040513     C* L'errore non è più bloccante, ma viene emessa window di avvertimento
106800040513     C* (serve per presentare la liquidazione iva del periodo anche se in
106900040513     C*  questo non ci sono movimenti riportando sul periodo corrente gli
107000040513     C*  eventuali progressivi del periodo precedente)
107100040513     C                   EXSR      EMTWIN3
107200040513     C                   if        MSGRTN = 'A'
107300040513     C                   seton                                        99
107400040513     C                   endif
107500040513     C                   endif
107600040513     C                   endif
107700040520C1762 *
107800040520C1762C     XCtrD1NdrivOk Endsr
107900020423C1550 ************************************************************
108000040505  "   * GESTIONE VIDEO RECORD D9 (impostazione tipo liquidazione)
108100020423  "   ************************************************************
108200020423  "  C     GESD9         BEGSR
108300020423  "   * inizializzazione videata
108400020423  "  C     $INZD9        IFEQ      *ON
108500020423  "  C                   EXSR      INZD9
108600020423  "  C                   MOVE      *OFF          $INZD9
108700020423  "  C                   ENDIF
108800020423  "  C* emissione piede videata se proveniente da altra
108900020423  "  C* salvataggio valore formato di provenienza
109000020423  "  C     $LASTV        IFNE      'D9'
109100020423  "  C                   WRITE     FMTT1
109200020423  "  C                   EXSR      GESPIE
109300020423  "  C                   MOVE      $LASTV        $LASTG
109400020423  "  C                   MOVE      'D9'          $LASTV
109500020423  "  C                   ENDIF
109600020423  "  C*
109700020423  "  C* se esistono errori sulla videata
109800020423  "  C* emetto la write del formato a indicatori spenti per vedere
109900020423  "  C* le eventuali decodifiche
110000020423  "  C     *IN99         IFEQ      *ON
110100020423  "  C     $GEST         ANDEQ     'D9'
110200020423  "  C                   MOVEA     *IN           WIN
110300020423  "  C                   MOVE      *ALL'0'       IN5098           49
110400020423  "  C                   MOVEA     IN5098        *IN(50)
110500020423  "  C                   WRITE     FMTD9
110600020423  "  C                   MOVEA     WIN           *IN
110700020423  "  C                   ENDIF
110800020423  "  C*
110900020423  "  C                   EXFMT     FMTD9
111000020423  "  C                   Eval      XMSGINZ = *On
111100020423  "  C*
111200020423  "  C* controllo abilitazione tasti funzione
111300020423  "  C                   EXSR      CTRKD9
111400020423  "  C*
111500020423  "  C     *IN99         IFEQ      *OFF
111600020423  "  C*
111700020423  "  C                   SELECT
111800020423  "  C* F2=Decodifica
111900020423  "  C     $TASTO        WHENEQ    F02
112000020423  "  C                   EXSR      F02D9
112100020423  "  C* F3=Fine
112200020423  "  C     $TASTO        WHENEQ    F03
112300020423  "  C                   EXSR      F03D9
112400020423  "  C* F4=Ricerca
112500020423  "  C     $TASTO        WHENEQ    F04
112600020423  "  C                   EXSR      SEARCH
112700020423  "  C* F5=Refresh
112800020423  "  C     $TASTO        WHENEQ    F05
112900020423  "  C                   EXSR      F05D9
113000020423  "  C* F12=Ritorno
113100020423  "  C     $TASTO        WHENEQ    F12
113200020423  "  C                   EXSR      F12D9
113300020423  "  C* F24=Altri tasti
113400020423  "  C     $TASTO        WHENEQ    F24
113500020423  "  C                   EXSR      F24D9
113600020423  "  C*
113700020423  "  C                   OTHER
113800020423  "  C*
113900020423  "  C                   EXSR      CTRD9
114000020423  "  C*
114100020423  "  C     *IN99         IFEQ      *OFF
114200020423  "  C* Gestione video D1
114300020423  "  C                   MOVE      *ON           $INZD1
114400020423  "  C                   MOVE      'D1'          $GEST
114500020423  "  C                   ENDIF
114600020423  "   *
114700020423  "  C                   ENDSL
114800020423  "   *
114900020423  "  C                   ENDIF
115000020423  "   *
115100020423C1550C     XGesD9        ENDSR
115200020423C1550 ************************************************************
115300020423  "   * INIZIALIZZAZIONE VIDEATA D9
115400020423  "   ************************************************************
115500020423  "  C     INZD9         BEGSR
115600020423  "   *
115700020423  "  C                   CLEAR                   FMTD9
115800020423  "   *
115900020423C1550C     XInzD9        Endsr
116000020423C1550 ************************************************************
116100020423  "   * CONTROLLO ABILITAZIONE TASTI VIDEATA D9
116200020423  "   ************************************************************
116300020423  "  C     CTRKD9        BEGSR
116400020423  "   *
116500020423  "  C                   SETOFF                                       99
116600020423  "  C                   MOVEA     $FC9          $FC
116700020423  "  C                   EXSR      CTRKEY
116800020423  "  C     *IN99         IFEQ      *ON
116900020423  "  C                   SETON                                        97
117000020423  "  C                   ENDIF
117100020423  "   *
117200020423C1550C     XCtrKD9       ENDSR
117300020423C1550 ************************************************************
117400020423  "   * GESTIONE F02 VIDEO D9
117500020423  "   ************************************************************
117600020423  "  C     F02D9         BEGSR
117700020423  "   *
117800020423  "   * per decodificare la videata richiamo la routine dei ctrl
117900020423  "   * e poi disattivo gli eventuali messaggi di errore
118000020423  "  C                   EXSR      CTRD9
118100020423  "   *
118200020423  "  C                   MOVE      *ALL'0'       IN5098
118300020423  "  C                   MOVEA     IN5098        *IN(50)
118400020423  "   *
118500020423C1550C     XF02D9        ENDSR
118600020423C1550 ************************************************************
118700020423  "   * GESTIONE F03 VIDEO D9
118800020423  "   ************************************************************
118900020423  "  C     F03D9         BEGSR
119000020423  "   *
119100020423  "  C                   MOVE      *ON           $FINE
119200020423  "   * fine programma
119300020423  "  C                   EXSR      ENDPGM
119400020423  "   *
119500020423C1550C     XF03D9        ENDSR
119600020423C1550 ************************************************************
119700020423  "   * GESTIONE F05 VIDEO D9
119800020423  "   ************************************************************
119900020423  "  C     F05D9         Begsr
120000020423  "   *
120100020423  "  C                   MOVE      *ON           $INZD9
120200020423  "   *
120300020423C1550C     XF05D9        Endsr
120400020423C1550 ************************************************************
120500020423  "   * GESTIONE F12 VIDEO D9
120600020423  "   ************************************************************
120700020423  "  C     F12D9         BEGSR
120800020423  "   *
120900020423  "  C                   MOVE      *ON           $FINE
121000020423  "   *
121100020423C1550C     XF12D9        ENDSR
121200020423C1550 ************************************************************
121300020423  "   * GESTIONE F24 VIDEO D9
121400020423  "   ************************************************************
121500020423  "  C     F24D9         BEGSR
121600020423  "   *
121700020423  "  C                   MOVEA     $FX9          $FX
121800020423  "  C                   MOVEA     $FC9          $FC
121900020423  "  C                   MOVEA     $FR9          $FR
122000020423  "  C                   Z-ADD     $ULKD9        $ULK
122100020423  "  C                   Z-ADD     79            $L01
122200020423  "  C                   Z-ADD     79            $L02
122300020423  "  C                   EXSR      RIEKEY
122400020423  "  C                   Z-ADD     $ULK          $ULKD9            3 0
122500020423  "  C                   MOVEA     $FC           $FC9
122600060301C1550C                   MOVEL     $KEY1         Z9KE1
122700060224C1913C                   MOVEL     $ALLFUNCT     H9ALLFUNCT
122800060301C1550C                   MOVEL     $KEY2         Z9KE2
122900020423  "  C                   WRITE     FMTZ9
123000020423  "   *
123100020423C1550C     XF24D9        ENDSR
123200020423C1550 ************************************************************
123300020423  "   * CONTROLLO VIDEATA D9
123400020423  "   ************************************************************
123500020423  "  C     CTRD9         BEGSR
123600020423  "   *
123700020423  "  C                   SETOFF                                       99
123800020423  "   *
123900020423  "   * non imposto pos. del cursore
124000020423  "  C                   Z-ADD     0             H1RIGA
124100020423  "  C                   Z-ADD     0             H1COLO
124200020423  "   *
124300020423  "   * Controllo validità tipo liquidazione
124400020423  "  C                   clear                   D9TpLiqD
124500020423  "  C                   reset                   X07DS
124600020423  "  C                   move      '1'           X07RIC
124700020423  "  C                   movel     'ANDIZ000'    X07TRC
124800020423  "  C                   movel     'DSPIVALIQ'   X07CAM
124900020423  "  C                   movel     D9TpLiq       X07VAL
125000020423  "  C                   callb     'X07VALR'
125100020423  "  C                   parm                    X07DS
125200020423  "  C                   if        X07Err = '0'
125300020423  "  C                   movel     X07DES        D9TpLiqD
125400020423  "  C                   else
125500020423  "   * PRO0048 - Il valore indicato per il campo non è valido
125600020423  "  C                   eval      *in54 = *on
125700020423  "  C                   eval      *in99 = *on
125800020423  "  C                   endif
125900020423  "   *
126000020423C1550C     XCtrD9        ENDSR
126100940510     C************************************************************
126200950717     C* GESTIONE VIDEO RECORD D2
126300940510     C************************************************************
126400950530    >C     GESD2         BEGSR
126500950717     C* inizializzazione videata
126600950717   B1C     $INZD2        IFEQ      *ON
126700950717     C                   EXSR      INZD2
126800950717     C                   MOVE      *OFF          $INZD2
126900950717   E1C                   ENDIF
127000940510     C* emissione piede videata se proveniente da altra
127100940510     C* salvataggio valore formato di provenienza
127200941117   B1C     $LASTV        IFNE      'D2'
127300940510     C                   WRITE     FMTT1
127400940510     C                   EXSR      GESPIE
127500940510     C                   MOVE      $LASTV        $LASTG
127600940510     C                   MOVE      'D2'          $LASTV
127700941117   E1C                   ENDIF
127800940510     C*
127900940707     C* se esistono errori sulla videata
128000940707     C* emetto la write del formato a indicatori spenti per vedere
128100940707     C* le eventuali decodifiche
128200941117   B1C     *IN99         IFEQ      *ON
128300940707     C     $GEST         ANDEQ     'D2'
128400940707     C                   MOVEA     *IN           WIN
128500940707     C                   MOVE      *ALL'0'       IN5098           49
128600940707     C                   MOVEA     IN5098        *IN(50)
128700940707     C                   WRITE     FMTD2
128800940707     C                   MOVEA     WIN           *IN
128900941117   E1C                   ENDIF
129000940707     C*
129100000717C1396C                   WRITE     FMTD2_0
129200940510     C                   EXFMT     FMTD2
129300000718C1396C                   Eval      XMSGINZ = *On
129400941205     C*
129500940510     C* controllo abilitazione tasti funzione
129600950717     C                   EXSR      CTRKD2
129700940510     C*
129800941117   B1C     *IN99         IFEQ      *OFF
129900940510     C*
130000941117   B2C                   SELECT
130100950717     C* F2=Decodifica
130200950717     C     $TASTO        WHENEQ    F02
130300950717     C                   EXSR      F02D2
130400940510     C* F3=Fine
130500940929     C     $TASTO        WHENEQ    F03
130600940510     C                   EXSR      F03D1
130700940510     C* F4=Ricerca
130800940929     C     $TASTO        WHENEQ    F04
130900940510     C                   EXSR      SEARCH
131000950721     C* F5=Refresh
131100950721     C     $TASTO        WHENEQ    F05
131200950721     C                   EXSR      F05D2
131300940510     C* F12=Ritorno
131400940929     C     $TASTO        WHENEQ    F12
131500950530     C                   EXSR      F12D2
131600030623C1668C* F14=Impostazione intestazione
131700030623  "  C     $TASTO        WHENEQ    F14
131800030623C1668C                   EXSR      F14D2
131900991230     C* F18=Valori assunti
132000991230     C     $TASTO        WHENEQ    F18
132100991230     C                   EXSR      F18D2
132200040726C1781C* F19=Testo
132300040726  "  C     $TASTO        WHENEQ    F19
132400040726C1781C                   EXSR      GesD8
132500940510     C* F24=Altri tasti
132600940929     C     $TASTO        WHENEQ    F24
132700950717     C                   EXSR      F24D2
132800940510     C*
132900941117   X2C                   OTHER
133000950717     C*
133100950721     C* Controlli
133200940510     C                   EXSR      CTRD2
133300950721     C*
133400001117D0766X* Rieseguo calcolo nuovo saldo in base alle modifiche fatte a video
133500001117D0766C* Eseguo calcoli x campi modificabili a video
133600950721     C     *in99         ifeq      *off
133700950721     C                   EXSR      CALD2
133800950721     C                   endif
133900940510     C*
134000941117   B3C     $TASTO        IFEQ      F06
134100940510     C     *IN99         ANDEQ     *OFF
134200951222     C                   move      *on           stpconf
134300000719C1396X* Finestra plafond (se gestito)
134400000718  "  X*****              if        XScPlafond = '1' or XScPlafond = '2'
134500000718  "  X*****              EXSR      WINPLF
134600000718  "  X*****              else
134700000718  "  X*****              move      *off          stpplaf
134800000718  "  X*****              endif
134900000718  "  C* Riempimento NDC167DS con dati liquidazione
135000000719  "  C                   ExSr      Rie167DsL
135100020222  "  c*
135200020222C1396C* Prospetto liquidazione
135300020524C1550 * Anche se non richiesto il prospetto di liquidazione
135400020524  "   * chiamo ugualmente il pgm che lo gestisce per
135500020524C1550 * fare solo l'aggiornamento di NDIVT
135600020524C1550X*ex D1095  se richiesto stampa prospetto liquidazione
135700020524C1550X*ex D1095          IF        d2ProsLiq = xscon
135800000718C1396C                   ExSr      ProspLiq
135900020524C1550X*ex D1095          endif
136000020222     c*
136100040204C1703 *
136200040204  "   * Se richiesto Iva di gruppo e rimasto confermata la stampa liq
136300040204  "  C                   if        D2StpLiqG = XScOn and
136400040204  "  C                             StpConf = *on
136500040204  "  C                   exsr      IvaDiGruppo
136600040204  "  C                   endif
136700040204C1703 *
136800000718     C                   if        stpconf = *on
136900000720C1396X* Aggiornamento file NDIVT
137000000720C1396X*****              EXSR      AGGIVT
137100961202     C* Eseguo lancio stampa riepiloghi IVA
137200950530     C                   EXSR      EXELAN
137300951113     C* Fine programma
137400951113     C                   move      *on           $fine
137500950717     C                   EXSR      ENDPGM
137600951222     C                   endif
137700941117   E3C                   ENDIF
137800940510     C*
137900941117   E2C                   ENDSL
138000940510     C*
138100941117   E1C                   ENDIF
138200941027     C*
138300950530    >C                   ENDSR
138400950530     C/EJECT
138500000714C1396C************************************************************
138600000714  "  C* Riempimeto ds NDC167DS con dati liquidazione
138700000714  "  C************************************************************
138800000719C1396C     Rie167DsL     BEGSR
138900000714     C*
139000000714     C                   RESET                   C167DS
139100000714     C*
139200000714     C                   move      xscsoc        soc167
139300000714     C*
139400020429C1550 * se iva trasp.mista tipo liquid. è valorizzata, altrimenti blank
139500020429  "  C                   if        XScIvaRgm ='2'
139600020429  "  C                   move      D1TpLiq       TpLiq167
139700020429  "  C                   else
139800020429  "  C                   move      *blanks       TpLiq167
139900020429  "  C                   endif
140000020429C1550 *
140100000714     C                   move      d1anno        ann167
140200000720     C                   move      anno          annp167
140300000714     C                   If        XscIvaLiq = '1'
140400000714     C                   move      d1mese        per167
140500000720     C                   move      mese          perp167
140600000714     C                   Else
140700000714     C                   move      d1perliq      per167
140800000720     C                   move      periodo       perp167
140900000714     C                   EndIf
141000000714     C*
141100000714     C     d2bol         ifeq      xscon
141200000714     C                   move      *on           bol167
141300000714     C                   else
141400000714     C                   move      *off          bol167
141500000714     C                   endif
141600000714     C*
141700000714     C     d2stpcau      ifeq      xscon
141800000714     C                   move      *on           cau167
141900000714     C                   else
142000000714     C                   move      *off          cau167
142100000714     C                   endif
142200001229C1457C*
142300001229C1457C                   Eval      ColPrLq167 = D2ColPrLq
142400020222     C*
142500020222d1095C     d2prosliq     ifeq      xscon
142600020222  "  C                   move      *on           prosliq167
142700020222  "  C                   else
142800020222  "  C                   move      *off          prosliq167
142900020222d1095C                   endif
143000021010C1590 *
143100021022D1095C                   if        D2StpTotR  = XScOn
143200021010  "  C                   move      *on           StpTotR167
143300021010  "  C                   else
143400021010  "  C                   move      *off          StpTotR167
143500021010d1095C                   endif
143600031201C1703 *
143700031201  "  C                   if        D2StpLiqG  = XScOn
143800031128  "  C                   move      *on           StpLiqG167
143900031128  "  C                   else
144000031128  "  C                   move      *off          StpLiqG167
144100031201  "  C                   move      *blank        MeseG167
144200031128C1703C                   endif
144300000714     C*
144400000714     C                   z-add     salperpr      spr167
144500000721     C                   z-add     sliperpr      slp167
144600000721     C                   z-add     slipersu      sls167
144700000714     C*
144800000717     C                   Eval      IvaDeb167 = TotVendite
144900000719     C                   Eval      IvaCre167 = TotAcq
145000000717     C*
145100000717     C                   Eval      OpAtt167 = OpAttive
145200000717     C                   Eval      OpAttCI167 = OpAttCesIn
145300000719     C                   Eval      OpPas167 = OpPassive
145400000719     C                   Eval      OpPasAI167 = OpPasAcqIn
145500000717     C*
145600000802     C                   Eval      SocCap167 = WSocCap
145700030623     C*
145800030623C1668C     d2Intesta     ifeq      xscon
145900030623  "  C                   move      *on           Intesta167
146000030623  "  C                   else
146100030623  "  C                   move      *off          Intesta167
146200030623C1668C                   endif
146300090120C2106C*
146400090120  "  C                   If         GesPlur045 = *on
146500090120  "  C                   move      d2cdatt       cdatt167
146600090120C2106C                   EndIf
146700000802     C*
146800000714C1396C                   ENDSR
146900000714C1396C************************************************************
147000000714  "  C* Prospetto liquidazione
147100000714  "  C************************************************************
147200000714C1396C     ProspLiq      BEGSR
147300000714     C*
147400000714     C                   MoveL     C167DS        KPJBU
147500000714     C*
147600000714     C                   CallB     'NDC166R1'
147700000714     C                   Parm                    KPJBA
147800000714     C                   Parm                    SOC001
147900000714     C                   Parm                    Ret166
148000000717     C                   Parm                    Err166
148100000714     C*
148200000714     C                   MoveL     KPJBU         C167DS
148300000714     C*
148400000717     C* Se errore o no F6 non proseguo
148500000717     C                   If        Err166 <> '0' or Ret166 <> '0'
148600000717     C                   Eval      StpConf = *Off
148700000717     C                   EndIf
148800000717     C* Se F3 esco dal pgm
148900000717     C                   If        Ret166 = '1'
149000000717     C                   Eval      $Fine = *On
149100000717     C                   EndIf
149200000714     C*
149300000714C1396C                   ENDSR
149400031128C1703 ************************************************************
149500031128  "   * Iva di gruppo
149600031128  "   ************************************************************
149700031128  "  C     IvaDiGruppo   Begsr
149800031128  "   *
149900031128  "  C                   reset                   ndc426ds
150000031128  "   *
150100031128  "  C                   movel     '02'          Opz426
150200031128  "  C                   move      XScSoc        Societa426
150300031128  "  C                   move      D1Anno        AnnoG426
150400031128  "   *  casi in cui è sempre indicato un mese e non un periodo
150500031128  "   *  (tipo liq. della società mensile oppure regime trasp. mista)
150600031128  "  C                   if        XScIvaLiq = '1' or
150700031128  "  C                             XScIvaRgm = '2'
150800031128  "  C                   move      D1Mese        MeseG426
150900031128  "   *  casi in cui è indicato un periodo
151000031128  "   *  (tipo liq. della società non mensile)
151100031128  "  C                   else
151200031128  "  C                   move      XScIvaLiq     $TpLiqNum
151300031128  "  C     D1PerLiq      mult      $TpLiqNum     $Mese
151400031128  "  C                   move      $Mese         MeseG426
151500040119  "  C                   move      *off          Storico426
151600091218  "  C                   endif
151700091218  "   *
151800091218  "  C                   movel     Ndc426ds      Kpjbu
151900031128  "  C                   call      'NDC426R'
152000031128  "  C                   parm                    KPJBA
152100031128  "   *
152200031128  "  C                   movel     Kpjbu         Ndc426ds
152300031128  "   *
152400040204  "   * Se errore oppure F12 non proseguo
152500040204  "  C                   if        Err426 <> '0'  or  Ret426 <> '0'
152600040204  "  C                   move      *off          StpConf
152700031201  "  C                   else
152800031201  "  C                   move      MeseG426      MeseG167
152900031128  "  C                   endif
153000040204  "   *
153100040204  "   * Se F3 esco dal pgm
153200040204  "  C                   if        Ret426 = '1'
153300040204  "  C                   eval      $Fine = *on
153400040204  "  C                   endif
153500031128  "   *
153600031128C1703C     XIvaDiGruppo  Endsr
153700950530     C************************************************************
153800950717     C* INIZIALIZZAZIONE VIDEATA D2
153900950530     C************************************************************
154000950717     C     INZD2         BEGSR
154100950530     C*
154200000717C1396C                   CLEAR                   FMTD2_0
154300950717     C                   CLEAR                   FMTD2
154400090120C2106C*
154500090120  "  C* Attiva indicatore 45 se gestite più attività IVA
154600090120  "  C                   If         GesPlur045 = *on
154700090120  "  C                   SetOn                                        45
154800090120  "  C                   eval      d2cdatt = d1cdatt
154900090626C2106C                   movel     AIVDESBREV    d2dbcda
155000090626C2131C*
155100090626  "  C* se gestito il PRORATA, mostro i dati del prorata
155200090626  "  C                   if        AIVProRata  = *On
155300090626  "  C                   Eval      *In18       = *On
155400090626  "  C                   EXSR      RepProRata
155500090626  "  C                   Eval      D2ProRata   = Perc046
155600090626C2131C                   endif
155700090120C2106C                   EndIf
155800000202     C*
155900020424C1550 * Se regime iva trasportatori mista
156000020424  "   * visualizzo 'tipo liquidazione'
156100020424  "   * (già impostato in D9 e valorizzato in D1)
156200020424  "  C                   if        XScIvaRgm = '2'
156300020424  "  C                   eval      *in15 = '1'
156400020424  "  C                   move      D1TpLiq       D2TpLiq
156500020429  "  C                   movel     D1TpLiqD      D2TpLiqD
156600020429  "  C                   else
156700020430  "  C                   eval      *in15 = '0'
156800020429  "  C                   move      *blank        D2TpLiq
156900020429  "  C                   move      *blank        D2TpLiqD
157000020429  "  C                   endif
157100020424C1550 *
157200000202     C* Regime Iva per costanti iva slittata
157300020430C1550X****************** If        XscIvaRgm = '0'
157400020430C1550 * visualizzo l'iva slittata solo se regime iva trasp. normali
157500020430C1550C                   If        XscIvaRgm <>'1'
157600000202     C* - normale
157700000202     C                   SetOff                                       14
157800000202     C                   Else
157900000202     C* - trasportatori
158000000202     C                   SetOn                                        14
158100000202     C                   Endif
158200000202     C*
158300000202     C* Liquidazione per costanti mese/periodo
158400000202     C                   If        XscIvaLiq <> '1'
158500000202     C* - "periodo"
158600000202     C                   SetOn                                        13
158700000202     C                   Else
158800000202     C* - "mese"
158900000202     C                   SetOff                                       13
159000000202     C                   Endif
159100980722     C*
159200950717     C                   z-add     d1anno        d2anno
159300000202     C                   If        XscIvaLiq='1'
159400950717     C                   z-add     d1mese        d2mese
159500950726     C                   move      d1mesed       d2mesed
159600000202     C                   Else
159700990802     C                   z-add     d1perliq      d2perliq
159800980720     C                   move      d1perliqd     d2perliqd
159900000202     C                   EndIf
160000991230     C*
160100980720     C                   move      xscon         d2on1
160200950721     C                   move      xscoff        d2of1
160300991230     C                   move      xscon         d2on2
160400991230     C                   move      xscoff        d2of2
160500020222d1095C                   move      xscon         d2on3
160600020222d1095C                   move      xscoff        d2of3
160700021010C1590C                   move      XScOn         D2On4
160800021010C1590C                   move      XScOff        D2Of4
160900030623C1668C                   move      XScOn         D2On5
161000030623C1568C                   move      XScOff        D2Of5
161100031128C1703C                   move      XScOn         D2On6
161200031128C1703C                   move      XScOff        D2Of6
161300991230     C*
161400991230     C* Se richiesto impostazione dati da ultima elaborazione ...
161500991230     C     $Val          IFEQ      *ON
161600991230     C                   move      $bol          d2bol
161700030623C1668C                   move      $intesta      d2intesta
161800991230     C                   move      $stpcau       d2stpcau
161900001229C1457C                   Move      $ColPrLq      D2ColPrLq
162000020222D1095C                   Move      $ProsLiq      D2ProsLiq
162100021010C1590C                   Move      $StpTotR      D2StpTotR
162200031128C1703C                   Move      $StpLiqG      D2StpLiqG
162300991230     C                   ELSE
162400991230     C                   move      xscoff        d2bol
162500991230     C                   move      xscoff        d2stpcau
162600001229C1457C                   Move      '1'           D2ColPrLq
162700020222D1095C                   Move      xscoff        D2ProsLiq
162800021010C1590C                   Move      xScOff        D2StpTotR
162900030623c1668C                   Move      xScOff        D2Intesta
163000031128C1703C                   Move      XScOff        D2StpLiqG
163100991230     C                   ENDIF
163200000202     C*
163300031128C1703 * Se società controllante, visualizzo 'Stampa iva di gruppo'
163400031128  "  C                   if        SocCtr149 = *on
163500031128  "  C                   eval      *in16 = *on
163600031128  "  C                   else
163700031128  "  C                   eval      *in16 = *off
163800031128  "  C                   move      XScOff        D2StpLiqG
163900031128  "  C                   endif
164000031128C1703 *
164100000720     C* Periodo precedente
164200020524C1550C                   exsr      RepPerPre
164300000717C1396C*
164400000717  "  C* Inizializzazione campi work
164500000721  "  C                   Clear                   SalPerPr
164600000721  "  C                   Clear                   SliPerPr
164700000721  "  C                   Clear                   SliPerSu
164800000721  "  C                   Clear                   TotAcq
164900000717  "  C                   Clear                   OpAttive
165000000717  "  C                   Clear                   OpAttCesIn
165100000721  "  C                   Clear                   TotVendite
165200000717  "  C                   Clear                   OpPassive
165300000717C1396C                   Clear                   OpPasAcqIn
165400981125     C*
165500000201     C* SALDO PERIODO PRECEDENTE
165600020429C1550C                   exsr      InzSalPerP
165700000201     C*
165800000721C1396C* IVA SLITTATA DAL PERIODO PRECEDENTE (se regime iva trasportatori)
165900000721C1396X* SALDO ACQ. INTRA PERIODO PRECEDENTE
166000020429C1550C                   exsr      InzSliPerP
166100000202     C*
166200990802     C*
166300000719C1396C* IVA A CREDITO E OP. PASSIVE (E DI CUI ACQ. INTRA) PERIODO CORRENTE
166400000720C1396X* SALDO PERIODO CORRENTE
166500020429C1550C                   exsr      InzIvaCre
166600000719C1396C*
166700000719  "  C* IVA A DEBITO E OP. ATTIVE (E DI CUI CESS. INTRA) PERIODO CORRENTE
166800000719C1396C*
166900020429C1550C                   exsr      InzIvaDeb
167000980720     C*
167100000721C1396C* IVA SLITTATA AL PERIODO SUCCESSIVO (se regime iva trasportatori)
167200000721  "  X* SALDO ACQ. INTRA PERIODO CORRENTE
167300000721  "  X*
167400000721C1396X*****              z-add     0             salintra
167500020429C1550C                   exsr      InzSliPerS
167600980720     C*
167700980720     C* Valorizzo i campi a video
167800000202     C*
167900000202     C* - saldo periodo precedente
168000980720     C     salperpr      ifgt      0
168100980720     C                   z-add     salperpr      d2salperpr
168200980720     C                   move      XscAvere      d2daperpr
168300980720     C                   else
168400980720     C                   eval      d2salperpr=salperpr*-1
168500980720     C                   move      XscDare       d2daperpr
168600980720     C                   endif
168700040520C1762 *
168800040520  "   * se liq trasferita in iva di gruppo, evidenzio la stringa
168900040520  "   * 'in Iva di gruppo'
169000040520  "  C                   eval      *in09 = *off
169100040520  "  C                   if        $LiqIvaG = *on
169200040520  "  C                   eval      *in09 = *on
169300040520  "  C                   endif
169400040520C1762 *
169500000721C1396C* - iva slittata dal periodo precedente
169600000721  "  X* - saldo intra periodo precedente
169700000721  "  X*****salintpr      ifgt      0
169800000721  "  C     sliperpr      ifgt      0
169900000721  "  X*****              z-add     salintpr      d2salintpr
170000000721  "  C                   z-add     sliperpr      d2sliperpr
170100000721  "  X*****              move      XscAvere      d2daintpr
170200000721C1396C                   move      XscAvere      d2daslipr
170300980720     C                   else
170400000721C1396X*****              eval      d2salintpr=salintpr*-1
170500000721  "  C                   eval      d2sliperpr = sliperpr * -1
170600000721  "  X*****              move      XscDare       d2daintpr
170700000721C1396C                   move      XscDare       d2daslipr
170800980720     C                   endif
170900000719C1396X* - saldo periodo corrente
171000000719  "  X*****salperio      ifgt      0
171100000719  "  X*****              z-add     salperio      d2salperio
171200000719  "  X*****              move      XscAvere      d2daperio
171300000719  "  X*****              else
171400000719  "  X*****              eval      d2salperio=salperio*-1
171500000719  "  X*****              move      XscDare       d2daperio
171600000721  "  X*****              endif
171700000721  "  C* - iva slittata al periodo successivo
171800000721  "  X* - saldo intra periodo corrente
171900000721  "  X*****salintra      ifgt      0
172000000721  "  C     slipersu      ifgt      0
172100000721  "  X*****              z-add     salintra      d2salintra
172200000721  "  C                   z-add     slipersu      d2slipersu
172300000721  "  X*****              move      XscAvere      d2daintra
172400000721C1396C                   move      XscAvere      d2daslisu
172500980720     C                   else
172600000721C1396X*****              eval      d2salintra=salintra*-1
172700000721  "  C                   eval      d2slipersu = slipersu * -1
172800000721  "  X*****              move      XscDare       d2daintra
172900000721C1396C                   move      XscDare       d2daslisu
173000980720     C                   endif
173100000719C1396X* - nuovo saldo
173200000719  "  X*****salnuovo      ifgt      0
173300000719  "  X*****              z-add     salnuovo      d2salnuovo
173400000719  "  X*****              move      XscAvere      d2danuovo
173500000719  "  X*****              else
173600000719  "  X*****              eval      d2salnuovo=salnuovo*-1
173700000719  "  X*****              move      XscDare       d2danuovo
173800000719  "  X*****              endif
173900000719  "  C* - iva a debito
174000000719  "  C     TotVendite    ifgt      0
174100000719  "  C                   z-add     TotVendite    d2ivadeb
174200000719  "  C                   move      XscAvere      d2daivadeb
174300000719  "  C                   else
174400000719  "  C                   eval      d2ivadeb=TotVendite*-1
174500000719  "  C                   move      XscDare       d2daivadeb
174600000719  "  C                   endif
174700000719  "  C* - iva a credito
174800000719  "  C     TotAcq        ifgt      0
174900000719  "  C                   z-add     TotAcq        d2ivacred
175000000719  "  C                   move      XscAvere      d2daivacre
175100000719  "  C                   else
175200000719  "  C                   eval      d2ivacred=TotAcq*-1
175300000719  "  C                   move      XscDare       d2daivacre
175400000719C1396C                   endif
175500020927C1583 *
175600020927  "   * Nuovo Saldo: saldo periodo prec. +/- saldo periodo attuale
175700020927  "   *                                 (iva a debito - iva a credito);
175800020927  "   *   (Se non richiesto stampa prospetto liquidazione
175900020927  "   *    non viene visualizzato da nessuna parte il nuovo saldo
176000020927  "   *    perciò lo calolo a video)
176100020927  "  C                   eval      SalNuovo = SalPerPr +
176200040520  "  C                                        (TotVendite + TotAcq)             inizializzione S5
176300040520  "  C                   z-add     SalNuovo      D2SalNuovo
176400020927  "  C                   if        SalNuovo > 0
176500020927  "  C                   move      XScAvere      D2DANuovo
176600020927  "  C                   else
176700020927  "  C                   eval      D2SalNuovo = D2SalNuovo * -1
176800020927  "  C                   move      XScDare       D2DANuovo
176900020927  "  C                   endif
177000020927C1583 *
177100090626C2131C*
177200090626  "  C* calcolo IVA detraibile per effetto del prorata
177300090626  "  C                   if        GesPlur045  = *On     and
177400090626  "  C                             AIVProRata  = *On
177500090626  "  C                   EXSR      CalcProRata
177600090626  "  C                   endif
177700090626C2131C*
177800020222D1095c* se richiesto stampa prospetto liquidazione
177900020222D1095C                   IF        d2ProsLiq = xscon
178000001229C1457C*
178100001229  "  C* decodifica la videata
178200001229C1457C                   EXSR      F02D2
178300020222D1095C                   endif
178400950719     C*
178500020524C1550X****************** ENDSR
178600020524C1550C     XInzD2        Endsr
178700950530     C/EJECT
178800090626C2131C************************************************************
178900090626  "  C* Reperisco la percentuale di detraibilità del prorata
179000090626  "  C************************************************************
179100090626C2131C     RepProRata    BEGSR
179200090626     C*
179300090626     C                   Clear                   XAtbDS
179400090626     C                   Clear                   ANG046DS
179500090626     C*
179600090626     C                   Eval      XTbRic      = '1'
179700090626     C                   Eval      XTbAzi      = XScSoc
179800090626     C                   Eval      XTbKey      = *Zero
179900090626     C                   Eval      XTbCod      = '046'
180000090626     C                   if        D1Anno      > 0
180100090629     C                   Eval      WAnno       = D1Anno
180200090626     C                   Move      WAnno         XTbKey
180300090626     C                   endif
180400090626     C*
180500090626     C                   CALLB     'XATB'
180600090626     C                   PARM                    XAtbDS
180700090626     C*
180800090626     C                   if        XTbErr      = '0'
180900090626     C                   MoveL     XtbUni        ANG046DS
181000090626     C                   endif
181100090626     C*
181200090626C2131C                   ENDSR
181300090626C2131C/EJECT
181400090626C2131C************************************************************
181500090626  "  C* Calcolo IVA detraibile per effetto del prorata
181600090626  "  C************************************************************
181700090626C2131C     CalcProRata   BEGSR
181800090626     C*
181900090626     C                   Z-Add     TotAcq        WImpInp
182000090626     C*
182100090626     C                   CallP     X0201MultC(WImpInp: D2ProRata: 100:
182200090626     C                                        'H': '0': NrDecMC: WImpOut)
182300090626     C*
182400090626     C                   Z-Add     WImpOut       D2IVACred2
182500090626     C*
182600090626     C                   if        D2IVACred2  < 0
182700090626     C                   Eval      D2IVACred2  = - D2IVACred2
182800090626     C                   move      XscDare       D2DAIVACr2
182900090626     C                   else
183000090626     C                   move      XscAvere      D2DAIVACr2
183100090626     C                   endif
183200090626     C*
183300090626     C*
183400090626     C                   Eval      D2SalNuov2  = SalNuovo - TotAcq
183500090626     C                                           + WImpOut
183600090626     C*
183700090626     C                   if        D2SalNuov2  < 0
183800090626     C                   Eval      D2SalNuov2  = - D2SalNuov2
183900090626     C                   move      XscDare       D2DANuovo2
184000090626     C                   else
184100090626     C                   move      XscAvere      D2DANuovo2
184200090626     C                   endif
184300090626     C*
184400090626C2131C                   ENDSR
184500090626C2131C/EJECT
184600020524C1550 ************************************************************
184700020524  "   * Reperisco periodo precedente
184800020524  "   ************************************************************
184900020524  "  C     RepPerPre     Begsr
185000020524  "   *
185100020524  "  C                   select
185200020524  "   *
185300020524  "   * --------------------------------------------------------
185400020524  "   * se regime iva mista
185500020524  "  C                   when      XScIvaRgm = '2'
185600020524  "   *
185700020524  "   * in regime iva misto, devo indicare il tipo liquidazione
185800020524  "   * che voglio lanciare e se indico una liquidazione
185900020524  "   * non mensili, devo indicare l'ultimo mese del periodo di liquid.
186000020524  "   * (es: se voglio lanciare la liquidazione trimestrale
186100020524  "   *      del II° trimestre 2002, a video indico anno 2002
186200020524  "   *      mese 6, periodo 6; il periodo precedente corrisponde
186300020524  "   *      all'anno 2002, mese 3, periodo 3.
186400020524  "   *  (la descrizione del periodo viene reperito in base
186500020524  "   *  al tipo liquid. impostata sull'anag. società, che in
186600020524  "   *  regime iva trasp. mista è sempre mensile)
186700020524  "   *
186800020524  "  C                   move      D2TpLiq       $TpLiqNum         1 0
186900020524  "   *
187000020524  "   * se il mese di lancio di liquidazione supera il
187100020524  "   * tipo liquidazione indicato a video
187200020524  "  C                   if        D2Mese > $TpLiqNum
187300020524  "   *
187400020524  "   *   il mese finale del periodo precedente è nello stesso anno
187500020524  "   *   dell'anno di lancio
187600020524  "  C                   eval      Mese = D2Mese - $TpLiqNum
187700020524  "  C                   eval      Anno = D2Anno
187800020524  "   *
187900020524  "   * se il mese di lancio di liquidazione è inferiore al
188000020524  "   * tipo liquidazione indicato a video
188100020524  "  C                   else
188200020524  "   *
188300020524  "   *   il mese finale del periodo precedente è nell'anno precedente
188400020524  "  C                   eval      Mese = 12 - ($TpLiqNum - D2Mese)
188500020524  "  C                   eval      Anno = D2Anno - 1
188600020524  "   *
188700020524  "  C                   endif
188800020524  "   *
188900020524  "   * reperisco la descrizione del periodo precedente
189000020524  "   * (in regime iva trasp. misto corrisponde sempre al mese
189100020524  "   *  in quanto dipende dal tipo liq. impostato in anag. societa)
189200020524  "  C                   reset                   X62DS
189300020524  "  C                   move      XScSoc        X62Societa
189400020524  "  C                   move      Anno          X62Anno
189500020524  "  C                   move      Mese          X62Mese
189600020524  "  C                   move      *Blanks       X62PerLiq
189700020524  "  C                   z-add     0             X62Giorno
189800020524  "  C                   call      'X62IVALQ'
189900020524  "  C                   parm                    X62ds
190000020524  "  C                   if        X62Esito = '0'
190100020524  "  C                   move      X62Perliq     Periodo
190200020524  "  C                   endif
190300020524  "   *
190400020524  "   *
190500020524  "   * --------------------------------------------------------
190600020524  "   * se regime iva non mista
190700020524  "  C                   when      XScIvaRgm <> '2'
190800020524  "   *
190900020524C1550X*specifiche trasferite dalla routine InzD2
191000020524     C                   If        XscIvaLiq = '1'
191100020524     C     d1mese        ifeq      1
191200020524     C                   eval      mese=12
191300020524     C                   eval      anno=d1anno-1
191400020524     C                   else
191500020524     C                   eval      mese=d1mese-1
191600020524     C                   eval      anno=d1anno
191700020524     C                   endif
191800020524     C                   Else
191900020524     C     d1perliq      ifeq      1
192000020524     C                   eval      anno=d1anno-1
192100020524     C                   else
192200020524     C                   eval      anno=d1anno
192300020524     C                   endif
192400020524     C                   exsr      ricperpre
192500020524     C                   EndIf
192600020524C1550 *
192700020524  "  C                   endsl
192800020524  "   *
192900020524C1550C     XRepPerPre    Endsr
193000020429C1550 ************************************************************
193100020429  "   * SALDO PERIODO PRECEDENTE
193200020429  "  X* (specifiche preesistenti in INZD2 e qui solo riunite
193300020429  "  X* in questa routine)
193400020429  "   ************************************************************
193500020429C1550C     InzSalPerP    Begsr
193600020429C1396X*
193700020429C1396X*****              z-add     0             salperpr
193800020429     C*
193900020429     C                   move      xscsoc        ivtsoc
194000020429     C                   move      anno          ivtanno
194100020429     C                   If        XscIvaLiq = '1'
194200020429     C                   move      mese          ivtmese
194300020429     C                   Else
194400020429     C                   move      periodo       ivtmese
194500020429     C                   EndIf
194600020429     C                   move      'N'           ivttiptot
194700090120C2106C*
194800090120  "  C* Se gestite più attività IVA aggancia codice attività
194900090120  "  C                   If         GesPlur045 = *on
195000090120  "  C                   move      d1cdatt       ivtcdatt
195100090120  "  C                   else
195200090120  "  C                   move      *blank        ivtcdatt
195300090120C2106C                   EndIf
195400020429C1396X*****k4ivt1        chain(n)  ndivt01l                           21
195500020429C1550X*ex C1396  k4ivt1        chain     ndivt01l                           21
195600020429C1550C                   move      D2TpLiq       IVTTpLiq
195700090120C2106X*ex C1550  k5ivt1        chain     ndivt01l                           21
195800090120C2106C     k6ivt1        chain     ndivt01l                           21
195900020429     C     *in21         ifeq      *off
196000020429     C* Se il saldo al mese precedente è dare (a credito)
196100020429     C*    lo propongo come saldo precedente
196200020429     C* Se il saldo al mese precedente è avere (a debito)
196300020429     C*    lo propongo se <= a lire 50.000 (dato che non l'ho pagato !!!)
196400020429     C     ivtalmedar    ifeq      1
196500020429     C                   eval      salperpr=ivtalmesal*-1
196600020429     C                   else
196700020429     C     ivtalmesal    ifgt      ImpLimV149
196800020429     C                   z-add     0             salperpr
196900020429     C                   else
197000020429     C                   eval      salperpr=ivtalmesal
197100020429     C                   endif
197200020429     C                   endif
197300020429     C                   endif
197400020429C1550 *
197500040520C1762 * saldo periodo precedente = 0 se trasferito in Iva di Gruppo
197600040520  "  C                   exsr      InzSalPerP_G
197700040520  "  C                   if        $LiqIvaG = *on
197800040520  "  C                   z-add     0             salperpr
197900040520  "  C                   endif
198000040520C1762 *
198100040520C1550C     XInzSalPeP    Endsr
198200040520C1762 ************************************************************
198300040520  "   * Ctr se saldo periodo precedente trasferito in Iva di Gruppo
198400040520  "   ************************************************************
198500040520  "  C     InzSalPerP_G  Begsr
198600040520  "   *
198700040520  "   * memorizza se la liquidazione è stata trasferita in Iva di G
198800040520  "  C                   eval      $LiqIvaG = *off
198900040520  "   *
199000040520  "  C                   z-add     *zero         IVGKnraz
199100040520  "  C                   movel     'N'           IVGTipTotG
199200040520  "  C                   movel     XScSoc        IVGSocieta
199300040520  "  C                   movel     XScIvaRgm     IVGRegimeS
199400040520  "   * tipo liq in IVG sempre valorizzato sui rec con IVGTipTotG='N'
199500040520  "  C                   movel     XScIvaLiq     IVGTpLiqS
199600040520  "   *   se regime iva trasportatori misto il tipo liq
199700040520  "   *   è indicato dal campo a video
199800040520  "  C                   if        XScIvaRgm = '2'
199900040520  "  C                   movel     D2TpLiq       IVGTpLiqS
200000040520  "  C                   endif
200100040520  "   * anno e mese precedente
200200040520  "  C                   move      Anno          IVGAnnoS
200300040520  "  C                   if        XScIvaLiq = '1'
200400040520  "  C                   move      Mese          IVGMeseS
200500040520  "  C                   else
200600040520  "  C                   move      Periodo       IVGMeseS
200700040520  "  C                   endif
200800040520  "  C     K07IVG02      chain     NDIVG02l                           23
200900040520  "  C                   if        *in23 = *off
201000040520  "  C                   eval      $LiqIvaG = *on
201100040520  "  C                   endif
201200040520  "  C
201300040520  "   *
201400040520C1762C     XInzSalPerP_G Endsr
201500040520C1550 ************************************************************
201600020429  "   * IVA SLITTATA DAL PERIODO PRECEDENTE (se iva trasp.normali)
201700020429  "  X* (specifiche preesistenti in INZD2 e qui solo riunite
201800020429  "  X* in questa routine)
201900020429  "   ************************************************************
202000020429  "  C     InzSliPerP    Begsr
202100020429C1550 *
202200020429C1396X*****              z-add     0             salintpr
202300020429  "  X*****              z-add     0             totvendite
202400020429  "  X*
202500020429C1396C                   If        XScIvaRgm = '1'
202600020429     C                   move      xscsoc        ivtsoc
202700020429     C                   move      anno          ivtanno
202800020429     C                   If        XscIvaLiq = '1'
202900020429     C                   move      mese          ivtmese
203000020429     C                   Else
203100020429     C                   move      periodo       ivtmese
203200020429     C                   EndIf
203300020429     C                   move      'I'           ivttiptot
203400090120C2106C*
203500090120  "  C* Se gestite più attività IVA imposta codice attività
203600090120  "  C                   If         GesPlur045 = *on
203700090120  "  C                   move      d1cdatt       ivtcdatt
203800090120  "  C                   else
203900090120  "  C                   move      *blank        ivtcdatt
204000090120C2106C                   EndIf
204100020429C1396X*****k4ivt1        chain(n)  ndivt01l                           21
204200020429C1550X*ex C1396  k4ivt1        chain     ndivt01l                          21
204300020429C1550C                   move      D2TpLiq       IVTTpLiq
204400090120C2106X*ex C1550  k5ivt1        chain     ndivt01l                           21
204500090120C2106C     k6ivt1        chain     ndivt01l                           21
204600020429     C     *in21         ifeq      *off
204700020429C1396X* Se dare=1 (credito) imposto saldo reperito
204800020429C1396X* altrimenti avere=1 (debito) visualizzo 0 (perchè già pagato)
204900020429     C     ivtdare       ifeq      '1'
205000020429C1396X*****              eval      salintpr=ivttotale*-1
205100020429C1396C                   eval      sliperpr=ivttotale*-1
205200020429     C                   else
205300020429C1396X*****              eval      salintpr=ivttotale
205400020429C1396C                   eval      sliperpr=ivttotale
205500020429     C                   endif
205600020429C1396X* Se il regime è trasportatori e il saldo iva slittata dal periodo
205700020429  "  X* precedente è stato modificato a video lo considero nel totale
205800020429  "  X* vendite
205900020429  "  X* PERCHE' ??? (lo asterisco !!!)
206000020429  "  X*****              If        XscIvaRgm = '1' and
206100020429  "  X*****                        ivtflagmod = '1'
206200020429  "  X*****              z-add     ivttotale     totvendite
206300020429C1396X*****              EndIf
206400020429     C                   endif
206500020429C1396C                   EndIf
206600020429C1550 *
206700020429  "  C     XInzSliPeP    Endsr
206800020429  "   ************************************************************
206900020429  "   * IVA A CREDITO E OP. PASSIVE (E DI CUI ACQ. INTRA) PERIODO CORRENTE
207000020429  "  X* (specifiche preesistenti in INZD2 e qui solo riunite
207100020429  "  X* in questa routine)
207200020429  "   ************************************************************
207300020429C1550C     InzIvaCre     Begsr
207400020429     C*
207500020429C1396X*****              z-add     0             totacq
207600020429C1396X*****              z-add     0             totacqint
207700020429     C* Calcolo totale acquisti
207800020429C1425C                   Clear                   SAVLibro
207900020429     C                   move      xscsoc        rivsoc
208000020429     C                   move      d1anno        rivanno
208100020429     C                   If        XscIvaLiq = '1'
208200020429     C                   move      d1mese        rivperliq
208300020429     C                   Else
208400020429     C                   move      d1perliq      rivperliq
208500020429     C                   EndIf
208600020429     C                   move      '1'           rivtpreg
208700020429C1550X**** k4riv3        setll     ndriv03l
208800020429C1550X**** k4riv3        reade     ndriv03l                               21
208900020430C1550C                   move      D2TpLiq       RIVTpLiq
209000020429  "  C     k5riv3        setll     ndriv03l
209100020429C1550C     k5riv3        reade     ndriv03l                               21
209200020429     C     *in21         doweq     *off
209300020429C1396X* Verifica decalage causale
209400020429  "  X*****              move      xscsoc        opesocieta
209500020429  "  X*****              move      rivcaustes    opecausale
209600020429  "  X*****k2ope1        chain     anope01l                           22
209700020429  "  X*****opedecala     ifeq      '1'
209800020429  "  X*****              sub       rivimporto    totacqint
209900020429  "  X*****              else
210000020429  "  X*****              add       rivimporto    totacq
210100020429C1396X*****              endif
210200020429C1425C* Reperisco impostazioni libro iva
210300020429  "  C                   If        SAVLibro <> RIVLibro
210400020429  "  C                   Eval      DLISocieta = RIVSoc
210500020429  "  C                   Eval      DLITpReg = RIVTpReg
210600020429  "  C                   Eval      DLILibro = RIVLibro
210700020429  "  C     k03dli01      Chain     ANDLI01L                           22
210800020429  "  C   22              Clear                   DLISospens
210900020429  "  C                   Eval      SAVLibro = RIVLibro
211000090120C1425C                   EndIf
211100090120C2106C*
211200090120  "  C* Se gestite più attività IVA verifica informazioni aggiuntive
211300090120  "  C* libro/registro IVA
211400090120  "  C                   If         GesPlur045 = *on
211500090120  "  C                   exsr      Repanail
211600090120  "  C                   EndIf
211700090120  "  C                   If        (GesPlur045 = *on and ailcdatt = d1cdatt
211800090120C2106C                              or GesPlur045 = *off)
211900090120C1425C* Se libro sospeso/differito non lo considero nel totale acquisti !!!
212000040520C1425C                   If        DLISospens <> '1' and DLISospens <> '4'
212100020429C1396C                   sub       rivimporto    totacq
212200020429C1425C                   EndIf
212300020429C1396C* Calcolo operazioni passive (al netto dell'iva)
212400020429C1396C*  e il di cui acquisti intracomunitari
212500020429C1425C*  (considero anche libri sospesi/differiti !!!)
212600020429C1396C                   Move      RIVSoc        ALRSocieta
212700020429  "  C                   Move      RIVTpReg      ALRTpReg
212800020429  "  C                   Move      RIVLibro      ALRLibro
212900020429  "  C                   Z-Add     RIVAliq       ALRAliq
213000020429  "  C                   Move      RIVRifIva     ALRRifIva
213100020429  "  C     k05alr01      Chain     ANALR01L                           22
213200020429  "  C                   If        *In22 = *Off
213300020429  "  C                   If        %SubSt(ALRAlleIva:1:1) = *On
213400020429  "  C                   Sub       RIVImponib    OpPassive
213500020429  "  C                   Sub       RIVImpInd     OpPassive
213600020429  "  C                   EndIf
213700020429  "  C                   If        %SubSt(ALRAlleIva:2:1) = *On
213800020429  "  C                   Sub       RIVImponib    OpPasAcqIn
213900020429  "  C                   Sub       RIVImpInd     OpPasAcqIn
214000020429  "  C                   EndIf
214100020429C1396C                   EndIf
214200090120C2106C                   EndIf
214300020429C1550X**** k4riv3        reade     ndriv03l                               21
214400020429C1550C     k5riv3        reade     ndriv03l                               21
214500020429     C                   enddo
214600020429C1550 *
214700040520  "  C     XInzIvaCre    Endsr
214800040520  "   ************************************************************
214900020429  "   * IVA A DEBITO E OP. ATTIVE (E DI CUI CESS. INTRA) PERIODO CORRENTE
215000020429  "  X* (specifiche preesistenti in INZD2 e qui solo riunite
215100020429  "  X* in questa routine)
215200020429  "   ************************************************************
215300020429  "  C     InzIvaDeb     Begsr
215400020429C1550 *
215500020429     C* Calcolo totale vendite
215600020429C1425C                   Clear                   SAVLibro
215700020429     C                   move      xscsoc        rivsoc
215800020429     C                   move      d1anno        rivanno
215900020429     C                   If        XscIvaLiq = '1'
216000020429     C                   move      d1mese        rivperliq
216100020429     C                   Else
216200020429     C                   move      d1perliq      rivperliq
216300020429     C                   EndIf
216400020429     C                   move      '2'           rivtpreg
216500020429C1550X**** k4riv3        setll     ndriv03l
216600020429C1550X**** k4riv3        reade     ndriv03l                               21
216700020430C1550C                   move      D2TpLiq       RIVTpLiq
216800020429  "  C     k5riv3        setll     ndriv03l
216900020429C1550C     k5riv3        reade     ndriv03l                               21
217000020429     C     *in21         doweq     *off
217100020429C1425C* Reperisco impostazioni libro iva
217200020429  "  C                   If        SAVLibro <> RIVLibro
217300020429  "  C                   Eval      DLISocieta = RIVSoc
217400020429  "  C                   Eval      DLITpReg = RIVTpReg
217500020429  "  C                   Eval      DLILibro = RIVLibro
217600020429  "  C     k03dli01      Chain     ANDLI01L                           22
217700020429  "  C   22              Clear                   DLISospens
217800020429  "  C                   Eval      SAVLibro = RIVLibro
217900090120C1425C                   EndIf
218000090120C2106C*
218100090120  "  C* Se gestite più attività IVA verifica informazioni aggiuntive
218200090120  "  C* libro/registro IVA
218300090120  "  C                   If         GesPlur045 = *on
218400090120  "  C                   exsr      Repanail
218500090120  "  C                   EndIf
218600090120  "  C                   If        (GesPlur045 = *on and ailcdatt = d1cdatt
218700090120C2106C                              or GesPlur045 = *off)
218800090120C1425C* Se libro sospeso/differito non lo considero nel totale vendite !!!
218900020429C1425C                   If        DLISospens <> '1' and DLISospens <> '4'
219000020429     C                   add       rivimporto    totvendite
219100020429C1425C                   EndIf
219200020429C1396C* Calcolo operazioni attive (al netto dell'iva)
219300020429C1396C*  e il di cui cessioni intracomunitarie
219400020429C1425C*  (considero anche libri sospesi/differiti !!!)
219500020429C1396C                   Move      RIVSoc        ALRSocieta
219600020429  "  C                   Move      RIVTpReg      ALRTpReg
219700020429  "  C                   Move      RIVLibro      ALRLibro
219800020429  "  C                   Z-Add     RIVAliq       ALRAliq
219900020429  "  C                   Move      RIVRifIva     ALRRifIva
220000020429  "  C     k05alr01      Chain     ANALR01L                           22
220100020429  "  C                   If        *In22 = *Off and
220200020429  "  C                             (%SubSt(ALRAlleIva:1:1) = *On or
220300020429  "  C                              %SubSt(ALRAlleIva:2:1) = *On)
220400020429  "  C* controllo se causale di autofattura e eventualmente escludo ...
220500020429  "  C                   Eval      Causale = RIVCausTes
220600020429  "  C                   ExSr      CallMVC003
220700020429  "  C                   If        OPETpDocI <> '8'
220800020429  "  C                   If        %SubSt(ALRAlleIva:1:1) = *On
220900020429  "  C                   Add       RIVImponib    OpAttive
221000020429  "  C                   Add       RIVImpInd     OpAttive
221100020429  "  C                   EndIf
221200020429  "  C                   If        %SubSt(ALRAlleIva:2:1) = *On
221300020429  "  C                   Add       RIVImponib    OpAttCesIn
221400020429  "  C                   Add       RIVImpInd     OpAttCesIn
221500020429  "  C                   EndIf
221600020429  "  C                   EndIf
221700020429C1396C                   EndIf
221800090120C2106C                   EndIf
221900020429C1550X**** k4riv3        reade     ndriv03l                               21
222000020429C1550C     k5riv3        reade     ndriv03l                               21
222100020429     C                   enddo
222200020429     C* Calcolo totale corrispettivi (aggiungo alle vendite)
222300020429C1425C                   Clear                   SAVLibro
222400020429     C                   move      xscsoc        rivsoc
222500020429     C                   move      d1anno        rivanno
222600020429     C                   If        XscIvaLiq = '1'
222700020429     C                   move      d1mese        rivperliq
222800020429     C                   Else
222900020429     C                   move      d1perliq      rivperliq
223000020429     C                   EndIf
223100020429     C                   move      '3'           rivtpreg
223200020429C1550X**** k4riv3        setll     ndriv03l
223300020429C1550X**** k4riv3        reade     ndriv03l                               21
223400020430C1550C                   move      D2TpLiq       RIVTpLiq
223500020429  "  C     k5riv3        setll     ndriv03l
223600020429C1550C     k5riv3        reade     ndriv03l                               21
223700020429     C     *in21         doweq     *off
223800020429C1425C* Reperisco impostazioni libro iva
223900020429  "  C                   If        SAVLibro <> RIVLibro
224000020429  "  C                   Eval      DLISocieta = RIVSoc
224100020429  "  C                   Eval      DLITpReg = RIVTpReg
224200020429  "  C                   Eval      DLILibro = RIVLibro
224300020429  "  C     k03dli01      Chain     ANDLI01L                           22
224400020429  "  C   22              Clear                   DLISospens
224500020429  "  C                   Eval      SAVLibro = RIVLibro
224600090120C1425C                   EndIf
224700090120C2106C*
224800090120  "  C* Se gestite più attività IVA verifica informazioni aggiuntive
224900090120  "  C* libro/registro IVA
225000090120  "  C                   If         GesPlur045 = *on
225100090120  "  C                   exsr      Repanail
225200090120  "  C                   EndIf
225300090120  "  C                   If        (GesPlur045 = *on and ailcdatt = d1cdatt
225400090120C2106C                              or GesPlur045 = *off)
225500090120C1425C* Se libro sospeso/differito non lo considero nel totale vendite !!!
225600020429C1425C                   If        DLISospens <> '1' and DLISospens <> '4'
225700020429     C                   add       rivimporto    totvendite
225800020429C1425C                   EndIf
225900020429C1396C* Calcolo operazioni attive (al netto dell'iva)
226000020429C1396C*  e il di cui cessioni intracomunitarie
226100020429C1425C*  (considero anche libri sospesi/differiti !!!)
226200020429C1396C                   Move      RIVSoc        ALRSocieta
226300020429  "  C                   Move      RIVTpReg      ALRTpReg
226400020429  "  C                   Move      RIVLibro      ALRLibro
226500020429  "  C                   Z-Add     RIVAliq       ALRAliq
226600020429  "  C                   Move      RIVRifIva     ALRRifIva
226700020429  "  C     k05alr01      Chain     ANALR01L                           22
226800020429  "  C                   If        *In22 = *Off
226900020429  "  C                   If        %SubSt(ALRAlleIva:1:1) = *On
227000020429  "  C                   Add       RIVImponib    OpAttive
227100020429  "  C                   Add       RIVImpInd     OpAttive
227200020429  "  C                   EndIf
227300020429  "  C                   If        %SubSt(ALRAlleIva:2:1) = *On
227400020429  "  C                   Add       RIVImponib    OpAttCesIn
227500020429  "  C                   Add       RIVImpInd     OpAttCesIn
227600020429  "  C                   EndIf
227700020429C1396C                   EndIf
227800090120C2106C                   EndIf
227900020429C1550X**** k4riv3        reade     ndriv03l                               21
228000020429C1550C     k5riv3        reade     ndriv03l                               21
228100020429     C                   enddo
228200020429     C*
228300020429D1043X* Arrotondo totale vendite (NON SERVE + CON EURO)
228400020429  "  X*                  Z-Add     TotVendite    WImpArr
228500020429  "  X*                  ExSr      ArroItlEur
228600020429  "  X*                  Z-Add     WImpArr       TotVendite
228700020429  "  X*
228800020429  "  X* Arrotondo totale acquisti (NON SERVE + CON EURO)
228900020429  "  X*                  Z-Add     TotAcq        WImpArr
229000020429  "  X*                  ExSr      ArroItlEur
229100020429  "  X*                  Z-Add     WImpArr       TotAcq
229200020429  "  X*
229300020429  "  X* Arrotondo operazioni attive
229400020429  "  X*                  Z-Add     OpAttive      WImpArr
229500020429  "  X*                  ExSr      ArroItlEur
229600020429  "  X*                  Z-Add     WImpArr       OpAttive
229700020429  "  X*
229800020429  "  X* Arrotondo o.a. di cui cessioni intracomunitarie
229900020429  "  X*                  Z-Add     OpAttCesIn    WImpArr
230000020429  "  X*                  ExSr      ArroItlEur
230100020429  "  X*                  Z-Add     WImpArr       OpAttCesIn
230200020429  "  X*
230300020429  "  X* Arrotondo operazioni passive
230400020429  "  X*                  Z-Add     OpPassive     WImpArr
230500020429  "  X*                  ExSr      ArroItlEur
230600020429  "  X*                  Z-Add     WImpArr       OpPassive
230700020429  "  X*
230800020429  "  X* Arrotondo o.p. di cui acquisti intracomunitari
230900020429  "  X*                  Z-Add     OpPasAcqIn    WImpArr
231000020429  "  X*                  ExSr      ArroItlEur
231100020429  "  X*                  Z-Add     WImpArr       OpPasAcqIn
231200020429D1043X*
231300020429C1396X* Saldo periodo
231400020429C1396X*****              eval      salperio=totvendite-totacq
231500020429C1550 *
231600020429C1550C     XInzIvaDeb    Endsr
231700020429  "   ************************************************************
231800020429  "   * IVA SLITTATA AL PERIODO SUCCESSIVO (se iva trasp. normali)
231900020429  "  X* (specifiche preesistenti in INZD2 e qui solo riunite
232000020429  "  X* in questa routine)
232100020429  "   ************************************************************
232200020429C1550C     InzSliPerS    Begsr
232300020429     C*
232400020429C1396C                   If        XScIvaRgm = '1'
232500020429     C                   move      xscsoc        ivtsoc
232600020429     C                   move      d1anno        ivtanno
232700020429     C                   If        XscIvaLiq = '1'
232800020429     C                   move      d1mese        ivtmese
232900020429     C                   Else
233000020429     C                   move      d1perliq      ivtmese
233100020429     C                   EndIf
233200020429     C                   move      'I'           ivttiptot
233300090120C2106C*
233400090120  "  C* Se gestite più attività IVA aggancia codice attività
233500090120  "  C                   If         GesPlur045 = *on
233600090120  "  C                   move      d1cdatt       ivtcdatt
233700090120  "  C                   else
233800090120  "  C                   move      *blank        ivtcdatt
233900090120C2106C                   EndIf
234000020429C1396X*****k4ivt1        chain(n)  ndivt01l                           21
234100020429C1550X*ex C1396  k4ivt1        chain     ndivt01l                           21
234200020429C1550C                   move      D2TpLiq       IVTTpLiq
234300090120C2106X*ex C1550  k5ivt1        chain     ndivt01l                           21
234400090120C2106C     k6ivt1        chain     ndivt01l                           21
234500020429     C     *in21         ifeq      *off
234600020429C1396X*****              z-add     ivttotale     salintra
234700020429C1396C                   z-add     ivttotale     slipersu
234800020429     C     ivtdare       ifeq      '1'
234900020429C1396X*****              eval      salintra=salintra*-1
235000020429C1396C                   eval      slipersu = slipersu * -1
235100020429     C                   endif
235200020429     C                   endif
235300020429C1396C                   EndIf
235400020429  "  X*
235500020429  "  X* NUOVO SALDO
235600020429  "  X*
235700020429  "  X* Dato che il decalage intra non esiste più viene usato il campo
235800020429  "  X* intra per evidenziare l'IVA vendite slittata trasp. che, essendo già
235900020429  "  X* stata sottratta dal calcolo del saldo, non deve più essere usata
236000020429  "  X* altrimenti verrebbe sottratta 2 volte.
236100020429  "  X*****XscIvaRgm     ifeq      '1'
236200020429  "  X*****              eval      salnuovo=salperpr+salperio
236300020429  "  X*****              else
236400020429  "  X*****              eval      salnuovo=salperpr+salintpr+salperio
236500020429  "  X*****              endif
236600020429  "  X*
236700020429  "  X* Salvataggio valori modificabili a video
236800020429  "  X*****              z-add     salperpr      savperpr
236900020429C1396X*****              z-add     salintpr      savintpr
237000020429C1550 *
237100020429C1550C     XInzSliPeS    Endsr
237200020103D1043x************************************************************
237300020103  "  x* Arrotondamento importo in ITL o EUR (NON SERVE + CON EURO)
237400020103  "  x************************************************************
237500020103  "  x*    ArroItlEur    BEGSR
237600020103  "  x*
237700020103  "  x* Se l'importo è negativo si rende positivo (per il calcolo) ...
237800020103  "  x*                  If        WImpArr < 0
237900020103  "  x*                  Eval      $ImpNegat = *On
238000020103  "  x*                  Eval      WImpArr = WImpArr * -1
238100020103  "  x*                  Else
238200020103  "  x*                  Eval      $ImpNegat = *Off
238300020103  "  x*                  EndIf
238400020103  "  x*
238500020103  "  x*                  Select
238600020103  "  x*
238700020103  "  x* 1) ITL: alle 1000 lire superiori se ultime tre cifre > 500 lire,
238800020103  "  x*         altrimenti alle 1000 lire inferiori
238900020103  "  x*                  When      CdInter = 'ITL'
239000020103  "  x*                  Add       499           WImpArr
239100020103  "  x*    WImpArr       Div       1000          WImpArr0D
239200020103  "  x*    WImpArr0D     Mult      1000          WImpArr
239300020103  "  x*
239400020103  "  x* 2) EUR: all'unità di euro superiore se i decimali sono >= 50,
239500020103  "  x*         altrimenti all'unità di euro inferiore
239600020103  "  x*                  When      CdInter = 'EUR'
239700020103  "  x*                  Z-Add     WImpArr       W3015
239800020103  "  x*                  Z-Add     0             NrDec
239900020103  "  x*                  CallP     X0201Arrot(W3015: 'H': NrDec)
240000020103  "  x*                  Z-Add     W3015         WImpArr
240100020103  "  x*
240200020103  "  x*                  EndSl
240300020103  "  x*
240400020103  "  x* Se l'importo era negativo ...
240500020103  "  x*                  If        $ImpNegat = *On
240600020103  "  x*                  Eval      WImpArr = WImpArr * -1
240700020103  "  x*                  EndIf
240800020103  "  x*
240900020103D1043x*                  ENDSR
241000000202     C/EJECT
241100981125     C************************************************************
241200981125     C* RIECERCA PERIODO PRECEDENTE
241300981125     C************************************************************
241400981125     C     RICPERPRE     BEGSR
241500981125     C*
241600981125     C* Ricerco la data iniziale di periodo, sottraggo 1 gg. e
241700000202     C* richiamo la routine per ricavarne il periodo.
241800000202     C*
241900981125     C                   move      *blanks       periodo
242000981125     C                   reset                   x62ds
242100981125     C                   move      XscSoc        x62societa
242200981125     C                   move      d1anno        x62anno
242300981125     C                   move      d1perliq      x62perliq
242400981125     C                   z-add     0             x62mese
242500000202     C                   z-add     0             x62giorno
242600000202     C                   call      'X62IVALQ'
242700981125     C                   parm                    x62ds
242800000202     C*
242900981125     C     x62esito      ifeq      '0'
243000000202     C*
243100000202     C     x62dtinper    subdur    1:*d          x62dtinper
243200000202     C                   extrct    x62dtinper:*m mesepr
243300000202     C                   extrct    x62dtinper:*y annopr
243400981125     C                   reset                   x62ds
243500981125     C                   move      XscSoc        x62societa
243600981125     C                   move      annopr        x62anno
243700981125     C                   move      mesepr        x62mese
243800981125     C                   move      *blanks       x62perliq
243900000202     C                   z-add     0             x62giorno
244000000202     C                   call      'X62IVALQ'
244100981125     C                   parm                    x62ds
244200981125     C     x62esito      ifeq      '0'
244300000202     C                   move      x62perliq     periodo
244400981125     C                   endif
244500000202     C*
244600981125     C                   endif
244700000202     C*
244800981125     C                   ENDSR
244900981125     C/EJECT
245000950717     C************************************************************
245100950717     C* GESTIONE F02 VIDEO D2
245200950717     C************************************************************
245300950717     C     F02D2         BEGSR
245400950717     C*
245500950717     C* per decodificare la videata richiamo la routine dei ctrl
245600950717     C* e poi disattivo gli eventuali messaggi di errore
245700950717     C                   EXSR      CTRD2
245800950717     C*
245900950717     C                   MOVE      *ALL'0'       IN5098
246000950717     C                   MOVEA     IN5098        *IN(50)
246100950717     C*
246200950717     C                   ENDSR
246300950717     C/EJECT
246400950721     C************************************************************
246500950721     C* GESTIONE F05 VIDEO D2
246600950721     C************************************************************
246700950721     C     F05D2         BEGSR
246800950721     C*
246900950721     C                   MOVE      *ON           $INZD2
247000950721     C*
247100950721     C                   ENDSR
247200950721     C/EJECT
247300950530     C************************************************************
247400950530     C* GESTIONE F12 VIDEO D2
247500950530     C************************************************************
247600950530     C     F12D2         BEGSR
247700950530     C*
247800950530     C                   MOVE      'D1'          $GEST
247900950530     C*
248000950530     C                   ENDSR
248100950530     C/EJECT
248200030623C1668C************************************************************
248300030623     C* Impostazione intestazione
248400030623C1668C************************************************************
248500030623     C     F14D2         BEGSR
248600030623     c*
248700030623     C                   reset                   NDC098DS
248800030623     C                   eval      tip098='RIEPIVA'
248900030623     C                   eval      opz098='02'
249000030623     c                   eval      kpjbu = ndc098ds
249100030623     C                   CALL      'NDC098R'
249200030623     C                   parm                    kpjba
249300030623     c                   eval      ndc098ds = kpjbu
249400030623     c* F3=Fine
249500030623     c                   if        ret098='1'
249600030623     C                   MOVE      *ON           $FINE
249700030623     C                   EXSR      ENDPGM
249800030623     c                   endif
249900030623     C*
250000030623C1668C                   ENDSR
250100991230     C************************************************************
250200991230     C* GESTIONE F18 VIDEO D2
250300991230     C************************************************************
250400991230     C     F18D2         BEGSR
250500991230     C*
250600991230     C                   MOVE      'D4'          $GEST
250700991230     C                   MOVE      *ON           $INZD4
250800991230     C*
250900991230     C                   ENDSR
251000991230     C/EJECT
251100040726C1781 ************************************************************
251200040726  "   * Impostazione tipo testo
251300040726  "   ************************************************************
251400040726  "  C     GesD8         Begsr
251500040726  "   *
251600091218C1781C                   clear                   FmtD8
251700091218C2149 * Se società controllante, visualizzo 'Testo per iva di gruppo'
251800091218  "  C                   if        SocCtr149 = *on
251900091218  "  C                   eval      *in17 = *on
252000091218  "  C                   else
252100091218  "  C                   eval      *in17 = *off
252200091218C2149C                   endif
252300091218C1781 *
252400040726  "   * se prima di uscire dal pgm, ho già confermato il campo,
252500040726  "   * lo ripropondo
252600091218c1781C                   move      $TpTesto167   D8TpTesto
252700091218c2149C                   move      $TpTestG167   D8TpTestoG
252800091218c1781 *
252900040726  "   * emetto la window fino a quando no errori, no F6, no F12
253000040726  "  C                   dou       *in99 = *off
253100040726  "  C                             and *inkf = *on
253200040726  "  C                             or  *inkl = *on
253300040726  "   *
253400040726  "  C                   exfmt     FmtD8
253500040726  "  C                   eval      XMsgInz = *on
253600040726  "   *
253700040726  "  C                   setoff                                       99
253800040726  "   *
253900040726  "   *
254000040726  "  C                   select
254100040726  "   *
254200040726  "   * F4=Ricerca
254300040726  "  C                   when      *inkd = *on
254400040726  "  C                   exsr      Search
254500040726  "   *
254600040726  "   * Invio/F6
254700040726  "  C                   other
254800040726  "   *
254900040726  "   * Controllo tp testo
255000040726  "  C                   exsr      CtrlTesto
255100040726  "   *
255200040726  "   * F6=Conferma
255300040726  "  C                   if        *inkf = *on and *in99=*off
255400091218C1781C                   movel     D8TpTesto     $TpTesto167
255500091218C2149C                   movel     D8TpTestoG    $TpTestG167
255600091218C1781C                   endif
255700040726  "   *
255800040726  "  C                   endsl
255900040726  "   *
256000040726  "  C                   enddo
256100040726  "   *
256200040726  "   *
256300040726C1781C     XGesD8        Endsr
256400040726C1781 ************************************************************
256500040726  "   * Controllo tipo testo
256600040726  "   ************************************************************
256700040726  "  C     CtrlTesto     Begsr
256800040726  "   *
256900040726  "  C                   if        D8TpTesto<>*blank
257000040726  "   *
257100040726  "  C                   eval      XSocieta = XScSoc
257200040726  "  C                   eval      XTpTesto = D8TpTesto
257300040726  "  C                   eval      XUnita = XScDUn
257400040726  "  C                   eval      XLingua = XScLin
257500040726  "  C                   eval      XClaUte = *blanks
257600040726  "  C                   eval      XPosRig = 'A'
257700040726  "  C                   eval      XNrRiga = *zeros
257800040726  "  C                   eval      XRibat = *blanks
257900040726  "  C                   eval      XTesRig = *blanks
258000040726  "  C                   eval      XNrChiavi= 4
258100040726  "  C                   eval      XUsoUnita = '1'
258200040726  "   *
258300040726  "  C                   call      'X72TXT'
258400040726  "  C                   parm      '0'           XEsito
258500040726  "  C                   parm      '6'           XOpe
258600040726  "  C                   parm                    XSocieta
258700040726  "  C                   parm                    XTpTesto
258800040726  "  C                   parm                    XUnita
258900040726  "  C                   parm                    XLingua
259000040726  "  C                   parm                    XClaUte
259100040726  "  C                   parm                    XPosRig
259200040726  "  C                   parm                    XNrRiga
259300040726  "  C                   parm                    XRibat
259400040726  "  C                   parm                    XTesRig
259500040726  "  C                   parm                    XNrChiavi
259600040726  "  C                   parm                    XUsoUnita
259700040726  "   *
259800040726  "  C                   if        XEsito <> *off
259900040726  "   *    PRO0438-Tipo testo inesistente
260000040726  "  C                   eval      *in63 = *on
260100040726  "  C                   eval      *in99 = *on
260200040727  "  C                   endif
260300040726  "   *
260400040726  "  C                   endif
260500091218C1781 *
260600091218C2149C* TESTO per iva di gruppo
260700091218  "  C                   if        D8TpTestoG<>*blank
260800091218  "   *
260900091218  "  C                   eval      XSocieta = XScSoc
261000091218  "  C                   eval      XTpTesto = D8TpTestoG
261100091218  "  C                   eval      XUnita = XScDUn
261200091218  "  C                   eval      XLingua = XScLin
261300091218  "  C                   eval      XClaUte = *blanks
261400091218  "  C                   eval      XPosRig = 'A'
261500091218  "  C                   eval      XNrRiga = *zeros
261600091218  "  C                   eval      XRibat = *blanks
261700091218  "  C                   eval      XTesRig = *blanks
261800091218  "  C                   eval      XNrChiavi= 4
261900091218  "  C                   eval      XUsoUnita = '1'
262000091218  "   *
262100091218  "  C                   call      'X72TXT'
262200091218  "  C                   parm      '0'           XEsito
262300091218  "  C                   parm      '6'           XOpe
262400091218  "  C                   parm                    XSocieta
262500091218  "  C                   parm                    XTpTesto
262600091218  "  C                   parm                    XUnita
262700091218  "  C                   parm                    XLingua
262800091218  "  C                   parm                    XClaUte
262900091218  "  C                   parm                    XPosRig
263000091218  "  C                   parm                    XNrRiga
263100091218  "  C                   parm                    XRibat
263200091218  "  C                   parm                    XTesRig
263300091218  "  C                   parm                    XNrChiavi
263400091218  "  C                   parm                    XUsoUnita
263500091218  "   *
263600091218  "  C                   if        XEsito <> *off
263700091218  "   *    PRO0438-Tipo testo inesistente
263800091218  "  C                   eval      *in64 = *on
263900091218  "  C                   eval      *in99 = *on
264000091218  "  C                   endif
264100091218  "   *
264200091218C2149C                   endif
264300040726C1781C     XCtrlTesto    Endsr
264400950717     C************************************************************
264500950717     C* GESTIONE F24 VIDEO D2
264600950717     C************************************************************
264700950717     C     F24D2         BEGSR
264800950717     C*
264900950717     C                   MOVEA     $FX2          $FX
265000950717     C                   MOVEA     $FC2          $FC
265100950717     C                   MOVEA     $FR2          $FR
265200950717     C                   Z-ADD     $ULKD2        $ULK
265300950717     C                   Z-ADD     79            $L01
265400950717     C                   Z-ADD     79            $L02
265500950717     C                   EXSR      RIEKEY
265600950717     C                   Z-ADD     $ULK          $ULKD2            3 0
265700950717     C                   MOVEA     $FC           $FC2
265800950717     C                   MOVEL     $KEY1         Z2KE1
265900060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
266000950717     C                   MOVEL     $KEY2         Z2KE2
266100950717     C                   WRITE     FMTZ2
266200950717     C*
266300950717     C                   ENDSR
266400950717     C/EJECT
266500940510     C************************************************************
266600940510     C* CONTROLLO VIDEATA  D2
266700940510     C************************************************************
266800950530    >C     CTRD2         BEGSR
266900940510     C*
267000940510     C                   SETOFF                                       99
267100940510     C* non imposto pos. del cursore
267200940510     C                   Z-ADD     0             H1RIGA
267300940510     C                   Z-ADD     0             H1COLO
267400950727     C*
267500950727     C* Controllo dare/avere
267600961009     C     d2daperpr     ifne      XscDare
267700961009     C     d2daperpr     andne     XscAvere
267800950727     C                   seton                                        5199
267900950727     C                   endif
268000000721C1396X*****d2daintpr     ifne      XscDare
268100000721  "  X*****d2daintpr     andne     XscAvere
268200000721  "  X*****              seton                                        5299
268300000721  "  X*****              endif
268400000721  "  C                   If        XScIvaRgm = '1'
268500000721  "  C     d2daslipr     ifne      XscDare
268600000721  "  C     d2daslipr     andne     XscAvere
268700000721  "  C                   seton                                        5299
268800000721  "  C                   endif
268900000721  "  C                   EndIf
269000000721  "  X*****d2daperio     ifne      XscDare
269100000719  "  X*****d2daperio     andne     XscAvere
269200000719  "  X*****              seton                                        5399
269300000719  "  X*****              endif
269400000719  "  C     d2daivadeb    ifne      XscDare
269500000719  "  C     d2daivadeb    andne     XscAvere
269600000719  "  C                   seton                                        5399
269700000719  "  C                   endif
269800000719  "  X*****d2danuovo     ifne      XscDare
269900000719  "  X*****d2danuovo     andne     XscAvere
270000000719  "  X*****              seton                                        5499
270100000719  "  X*****              endif
270200000719  "  C     d2daivacre    ifne      XscDare
270300000719  "  C     d2daivacre    andne     XscAvere
270400000719  "  C                   seton                                        5499
270500000721  "  C                   endif
270600000721  "  C                   If        XScIvaRgm = '1'
270700000721  "  X*****d2daintra     ifne      XscDare
270800000721  "  X*****d2daintra     andne     XscAvere
270900000721  "  X*****              seton                                        5599
271000000721  "  X*****              endif
271100000721  "  C     d2daslisu     ifne      XscDare
271200000721  "  C     d2daslisu     andne     XscAvere
271300000721  "  C                   seton                                        5599
271400000721  "  C                   endif
271500000721C1396C                   EndIf
271600950721     C*
271700950721     C* Controllo flag stampa causali
271800950721     C     d2stpcau      ifne      xscon
271900950721     C     d2stpcau      andne     xscoff
272000950721     C                   seton                                        5099
272100950721     C                   endif
272200991230     C*
272300991230     C* Controllo flag stampa bollato
272400991230     C     d2bol         ifne      xscon
272500991230     C     d2bol         andne     xscoff
272600991230     C                   seton                                        5699
272700991230     C                   endif
272800030623     c*
272900030623c1668C* Controllo flag stampa intestazione
273000030623  "  C     d2intesta     ifne      xscon
273100030623  "  C     d2intesta     andne     xscoff
273200030623  "  C                   seton                                        6299
273300030623C1668C                   endif
273400030623     c*
273500001229C1457C*
273600001229  "  C* Controllo flag stampa colonne D/A prospetto liquidazione
273700001229  "  C                   Clear                   D2ColPrLqD
273800001229  "  C                   RESET                   X07DS
273900001229  "  C                   MOVE      '1'           X07RIC
274000001229  "  C                   MOVEL     'ANDIZ000'    X07TRC
274100001229  "  C                   MOVEL     'DSPDASXDX'   X07CAM
274200001229  "  C                   MOVEL     D2ColPrLq     X07VAL
274300001229  "  C                   MOVE      *OFF          X07ALL
274400001229  "  C                   CALLb     'X07VALR'
274500001229  "  C                   PARM                    X07DS
274600001229  "  C     X07ERR        IFEQ      '0'
274700001229  "  C                   MOVEL     X07DES        D2ColPrLqD
274800001229  "  C                   ELSE
274900001229  "  C                   SETON                                        9957
275000001229C1457C                   ENDIF
275100020222     c* Controllo flag S/N stampa prospetto di liquidazione
275200020222D1095C                   if        d2ProsLiq <> xscon and d2prosliq
275300020222  "  C                             <> xscoff
275400020222  "  c                   seton                                        5899
275500020222D1095C                   ENDIF
275600021010C1590 *
275700021022  "   * Controllo flag S/N stampa limiti nr protocolli
275800021010  "  C                   if        D2StpTotR <> XScOn   and
275900021010  "  C                             D2StpTotR <> XScOff
276000021010  "   * PRO0048 - Il valore indicato per il campo non è valido
276100021010  "  C                   eval      *in59 = *on
276200021010  "  C                   eval      *in99 = *on
276300021010C1590C                   endif
276400031128C1703 *
276500040520C1762X**ex C1703*********Controllo flag S/N stampa limiti nr protocolli
276600040520C1762 * Controllo flag S/N stampa iva di Gruppo
276700040520C1703C                   if        D2StpLiqG <> XScOn   and
276800031128  "  C                             D2StpLiqG <> XScOff
276900031128  "   * PRO0048 - Il valore indicato per il campo non è valido
277000031128  "  C                   eval      *in61 = *on
277100031128  "  C                   eval      *in99 = *on
277200031128C1703C                   endif
277300950721     C*
277400940510     C                   ENDSR
277500950530     C/EJECT
277600950721     C************************************************************
277700001117D0766X* RICALCOLO NUOVO SALDO
277800001117D0766C* CALCOLI VIDEO D2
277900950721     C************************************************************
278000950721     C     CALD2         BEGSR
278100950721     C*
278200961009     C     d2daperpr     ifeq      XscDare
278300950721     C                   eval      salperpr=d2salperpr*-1
278400950721     C                   else
278500950721     C                   z-add     d2salperpr    salperpr
278600950721     C                   endif
278700001117     C*
278800001117C1396X*****d2daintpr     ifeq      XscDare
278900001117D0766C     d2daslipr     ifeq      XscDare
279000001117C1396X*****              eval      salintpr=d2salintpr*-1
279100001117D0766C                   eval      sliperpr=d2sliperpr*-1
279200001117C1396X*****              else
279300001117D0766C                   else
279400001117C1396X*****              z-add     d2salintpr    salintpr
279500001117D0766C                   z-add     d2sliperpr    sliperpr
279600001117C1396X*****              endif
279700001117D0766C                   endif
279800001117C1396X*
279900000719  "  X* Ricalcolo
280000000719  "  X* Dato che il decalage intra non esiste più viene usato il campo
280100000719  "  X* intra per evidenziare l'IVA vendite slittata trasp. che, essendo già
280200000719  "  X* stata sottratta dal calcolo del saldo, non deve più essere usata
280300000719  "  X* altrimenti verrebbe sottratta 2 volte
280400000719  "  X*****XscIvaRgm     ifeq      '1'
280500000719  "  X*****              eval      salnuovo=salperpr+salperio
280600000719  "  X*****              else
280700000719  "  X*****              eval      salnuovo=salperpr+salintpr+salperio
280800000719  "  X*****              endif
280900000719  "  X*
281000000719  "  X*****salnuovo      ifgt      0
281100000719  "  X*****              z-add     salnuovo      d2salnuovo
281200000719  "  X*****              move      XscAvere      d2danuovo
281300000719  "  X*****              else
281400000719  "  X*****              eval      d2salnuovo=salnuovo*-1
281500000719  "  X*****              move      XscDare       d2danuovo
281600000719C1396X*****              endif
281700990802     C*
281800020927C1583 *
281900020927  "   * Ricalcolo saldo nuovo
282000020927  "   * Nuovo Saldo: saldo periodo prec. +/- saldo periodo attuale
282100020927  "   *                                 (iva a debito - iva a credito);
282200020927  "  C                   eval      SalNuovo = SalPerPr +
282300040520  "  C                                        (TotVendite + TotAcq)             inizializzione S5
282400040520  "  C                   z-add     SalNuovo      D2SalNuovo
282500020927  "  C                   if        SalNuovo > 0
282600020927  "  C                   move      XScAvere      D2DANuovo
282700020927  "  C                   else
282800020927  "  C                   eval      D2SalNuovo = D2SalNuovo * -1
282900020927  "  C                   move      XScDare       D2DANuovo
283000020927  "  C                   endif
283100020927C1583 *
283200950721     C                   ENDSR
283300950721     C/EJECT
283400000720C1396X************************************************************
283500000720  "  X* AGGIORNAMENTO FILE NDIVT
283600000720  "  X************************************************************
283700000720  "  X*****AGGIVT        BEGSR
283800000720  "  X*
283900000720  "  X* Saldo periodo precedente
284000000720  "  X*
284100000720  "  X*****salperpr      ifne      savperpr
284200000720  "  X*****              move      xscsoc        ivtsoc
284300000720  "  X*****              move      anno          ivtanno
284400000720  "  X*****              If        XscIvaLiq = '1'
284500000720  "  X*****              move      mese          ivtmese
284600000720  "  X*****              Else
284700000720  "  X*****              move      periodo       ivtmese
284800000720  "  X*****              EndIf
284900000720  "  X*****              move      'N'           ivttiptot
285000000720  "  X*****k4ivt1        chain     ndivt01l                           21
285100000720  "  X*****              z-add     d2salperpr    ivtalmesal
285200000720  "  X*****              z-add     0             ivttotale
285300000720  "  X*****d2daperpr     ifeq      XscDare
285400000720  "  X*****              eval      ivtalmedar    = 1
285500000720  "  X*****              eval      ivtalmeave    = 0
285600000720  "  X*****              else
285700000720  "  X*****              eval      ivtalmedar    = 0
285800000720  "  X*****              eval      ivtalmeave    = 1
285900000720  "  X*****              endif
286000000720  "  X*****              move      '1'           ivtflagmod
286100000720  "  X******in21         ifeq      *off
286200000720  "  X*****              update    ndivt001
286300000720  "  X*****              else
286400000720  "  X* reperisco periodo precedente
286500000720  "  X*****              exsr      repper
286600000720  "  X*****              move      xscsoc        ivtsoc
286700000720  "  X*****              move      anno          ivtanno
286800000720  "  X*****              If        XscIvaLiq = '1'
286900000720  "  X*****              move      mese          ivtmese
287000000720  "  X*****              Else
287100000720  "  X*****              move      periodo       ivtmese
287200000720  "  X*****              EndIf
287300000720  "  X*****              move      'N'           ivttiptot
287400000720  "  X*****              write     ndivt001
287500000720  "  X*****              endif
287600000720  "  X*****              endif
287700000720  "  X*
287800000720  "  X* Saldo Intra periodo precedente
287900000720  "  X*
288000000720  "  X*****salintpr      ifne      savintpr
288100000720  "  X*****              move      xscsoc        ivtsoc
288200000720  "  X*****              move      anno          ivtanno
288300000720  "  X*****              If        XscIvaLiq = '1'
288400000720  "  X*****              move      mese          ivtmese
288500000720  "  X*****              Else
288600000720  "  X*****              move      periodo       ivtmese
288700000720  "  X*****              EndIf
288800000720  "  X*****              move      'I'           ivttiptot
288900000720  "  X*****k4ivt1        chain     ndivt01l                           21
289000000720  "  X*****              z-add     d2salintpr    ivttotale
289100000720  "  X*****d2daintpr     ifeq      XscDare
289200000720  "  X*****              move      '1'           ivtdare
289300000720  "  X*****              move      '0'           ivtavere
289400000720  "  X*****              else
289500000720  "  X*****              move      '0'           ivtdare
289600000720  "  X*****              move      '1'           ivtavere
289700000720  "  X*****              endif
289800000720  "  X*****              move      '1'           ivtflagmod
289900000720  "  X*****              clear                   ivtalmesal
290000000720  "  X*****              clear                   ivtalmedar
290100000720  "  X*****              clear                   ivtalmeave
290200000720  "  X******in21         ifeq      *off
290300000720  "  X*****              update    ndivt001
290400000720  "  X*****              else
290500000720  "  X* reperisco periodo precedente
290600000720  "  X*****              exsr      repper
290700000720  "  X*****              move      xscsoc        ivtsoc
290800000720  "  X*****              move      anno          ivtanno
290900000720  "  X*****              If        XscIvaLiq = '1'
291000000720  "  X*****              move      mese          ivtmese
291100000720  "  X*****              Else
291200000720  "  X*****              move      periodo       ivtmese
291300000720  "  X*****              EndIf
291400000720  "  X*****              move      'I'           ivttiptot
291500000720  "  X*****              write     ndivt001
291600000720  "  X*****              endif
291700000720  "  X*****              endif
291800000720  "  X*
291900000720  "  X* Saldo periodo
292000000720  "  X*
292100000720  "  X*****              move      xscsoc        ivtsoc
292200000720  "  X*****              move      d2anno        ivtanno
292300000720  "  X*****              If        XscIvaLiq = '1'
292400000720  "  X*****              move      d2mese        ivtmese
292500000720  "  X*****              Else
292600000720  "  X*****              move      d2perliq      ivtmese
292700000720  "  X*****              EndIf
292800000720  "  X*****              move      'N'           ivttiptot
292900000720  "  X*****k4ivt1        chain     ndivt01l                           21
293000000720  "  X*****              z-add     d2salperio    ivttotale
293100000719  "  X*****d2daperio     ifeq      XscDare
293200000719  "  X*****              move      '1'           ivtdare
293300000719  "  X*****              move      '0'           ivtavere
293400000719  "  X*****              else
293500000719  "  X*****              move      '0'           ivtdare
293600000719  "  X*****              move      '1'           ivtavere
293700000719  "  X*****              endif
293800000720  "  X*
293900000720  "  X* Nuovo saldo
294000000720  "  X*
294100000720  "  X*****              eval      ivtalmesal    = d2salnuovo
294200000719  "  X*****d2danuovo     ifeq      XscDare
294300000719  "  X*****              eval      ivtalmedar    = 1
294400000719  "  X*****              eval      ivtalmeave    = 0
294500000719  "  X*****              else
294600000719  "  X*****              eval      ivtalmedar    = 0
294700000719  "  X*****              eval      ivtalmeave    = 1
294800000720  "  X*****              endif
294900000720  "  X*****              move      '0'           ivtflagmod
295000000720  "  X******in21         ifeq      *on
295100000720  "  X*****              move      xscsoc        ivtsoc
295200000720  "  X*****              move      d2anno        ivtanno
295300000720  "  X*****              If        XscIvaLiq = '1'
295400000720  "  X*****              move      d2mese        ivtmese
295500000720  "  X*****              Else
295600000720  "  X*****              move      d2perliq      ivtmese
295700000720  "  X*****              EndIf
295800000720  "  X*****              move      'N'           ivttiptot
295900000720  "  X*****              write     ndivt001
296000000720  "  X*****              else
296100000720  "  X*****              update    ndivt001
296200000720  "  X*****              endif
296300000720  "  X*
296400000720  "  X* Saldo Intra periodo corrente
296500000720  "  X*
296600000720  "  X*****              move      xscsoc        ivtsoc
296700000720  "  X*****              move      d2anno        ivtanno
296800000720  "  X*****              If        XscIvaLiq = '1'
296900000720  "  X*****              move      d2mese        ivtmese
297000000720  "  X*****              Else
297100000720  "  X*****              move      d2perliq      ivtmese
297200000720  "  X*****              EndIf
297300000720  "  X*****              move      'I'           ivttiptot
297400000720  "  X*****k4ivt1        chain     ndivt01l                           21
297500000720  "  X*****              z-add     d2salintra    ivttotale
297600000720  "  X*****d2daintra     ifeq      XscDare
297700000720  "  X*****              move      '1'           ivtdare
297800000720  "  X*****              move      '0'           ivtavere
297900000720  "  X*****              else
298000000720  "  X*****              move      '0'           ivtdare
298100000720  "  X*****              move      '1'           ivtavere
298200000720  "  X*****              endif
298300000720  "  X*****              move      '0'           ivtflagmod
298400000720  "  X*****              clear                   ivtalmesal
298500000720  "  X*****              clear                   ivtalmedar
298600000720  "  X*****              clear                   ivtalmeave
298700000720  "  X******in21         ifeq      *on
298800000720  "  X*****              move      xscsoc        ivtsoc
298900000720  "  X*****              move      d2anno        ivtanno
299000000720  "  X*****              If        XscIvaLiq = '1'
299100000720  "  X*****              move      d2mese        ivtmese
299200000720  "  X*****              Else
299300000720  "  X*****              move      d2perliq      ivtmese
299400000720  "  X*****              EndIf
299500000720  "  X*****              move      'I'           ivttiptot
299600000720  "  X*****              write     ndivt001
299700000720  "  X*****              else
299800000720  "  X*****              update    ndivt001
299900000720  "  X*****              endif
300000000720  "  X*
300100000720C1396X*****              ENDSR
300200980820     C************************************************************
300300980820     C* REPERIMENTO PERIODO PRECEDENTE
300400980820     C************************************************************
300500980820     C     REPPER        BEGSR
300600980820     C*
300700981119     C                   IF        XscIvaLiq='1'
300800000202     C* Ricerca mese precedente
300900980820     C     d1mese        ifeq      1
301000980820     C                   eval      mese=12
301100980820     C                   eval      anno=d1anno-1
301200980820     C                   else
301300980820     C                   eval      mese=d1mese-1
301400980820     C                   eval      anno=d1anno
301500980820     C                   endif
301600000202     C                   ELSE
301700000202   B1C* Ricerca periodo precedente
301800980820     C                   reset                   x62ds
301900000202     C     d1perliq      ifeq      0
302000000202     C     d1perliq      oreq      1
302100980820     C                   eval      anno=d1anno-1
302200980820   X1C                   else
302300980820     C                   eval      anno=d1anno
302400000202   E1C                   endif
302500000202   B1C                   If        mesein=1
302600980820     C                   eval      mese=12
302700000202   X1C                   else
302800980820     C                   eval      mese=mesein -1
302900000202   E1C                   endif
303000980820     C                   move      XscSoc        x62societa
303100980820     C                   move      anno          x62anno
303200980820     C                   move      mese          x62mese
303300000202     C                   z-add     0             x62giorno
303400000202     C                   call      'X62IVALQ'
303500980820     C                   parm                    x62ds
303600980820   B1C     x62esito      ifeq      '0'
303700980820     C                   move      x62perliq     periodo
303800980820   X1C                   else
303900980820     C                   move      *blank        periodo
304000980820   E1C                   endif
304100000202     C                   endif
304200980820     C*
304300980820     C                   ENDSR
304400980820     C/EJECT
304500980807     C************************************************************
304600980807     C* FINESTRA PLAFOND
304700980807     C************************************************************
304800980807     C     WINPLF        BEGSR
304900980807     C*
305000980807     C                   move      xscsoc        plfsocieta
305100980807     C                   move      d1mese        plfamcomp
305200980807     C                   movel     d1anno        plfamcomp
305300980807     C     k2plf1        chain     ndplf01l                           21
305400980807     C*
305500980807     C     *in21         ifeq      *on
305600980807     C*
305700980807     C* Plafond non ancora calcolato per il periodo:
305800000201     C     d0tpela       ifeq      2
305900980807     C* Se richieta stp solo plafond -> finestra avvertimento e poi fine
306000980807     C                   EXSR      EMTWIN2
306100980807     C                   move      *on           $fine
306200980807     C                   EXSR      ENDPGM
306300980807     C                   else
306400980807     C* Se richieta stp di tutto -> finestra conferma stp solo riepiloghi
306500000802C1396X*****              move      *off          stpplaf
306600980807     C                   EXSR      EMTWIN1
306700980807     C                   if        MSGRTN = 'A'
306800980807     C                   move      *on           $fine
306900980807     C                   EXSR      ENDPGM
307000980807     C                   endif
307100980807     C                   endif
307200980807     C*
307300980807     C                   else
307400980807    >C*
307500000718     C* Plafond O.K. per il periodo: emissione finestra riepilogo plafond
307600980807     C                   move      *on           stpplaf
307700000718     C                   EXSR      GESD3
307800980807     C*
307900980807     C                   endif
308000980807     C*
308100980807     C                   ENDSR
308200980807     C/EJECT
308300951113     C************************************************************
308400970319     C* REPERIMENTO ESPORTAZ./IMPORTAZ. EFFETT. (PLAFOND MOBILE)
308500951113     C************************************************************
308600970319     C     RepMobile     BEGSR
308700951113     C*
308800970319     C* Reperimento di esportazioni/importazioni effettuate
308900970319     C* negli ultimi 12 mesi precedenti
309000980721     C                   movel     d1anno        anno              4 0
309100951221     C                   move      d1mese        mese              2 0
309200951113     C                   DO        12
309300951113     C                   if        mese = 1
309400951113     C                   eval      mese = 12
309500951113     C                   eval      anno = anno - 1
309600951113     C                   else
309700951113     C                   eval      mese = mese - 1
309800951113     C                   endif
309900951113     C                   move      xscsoc        plfsocieta
310000951113     C                   move      mese          plfamcomp
310100951113     C                   movel     anno          plfamcomp
310200951113     C     k2plf1        chain     ndplf01l                           21
310300951113     C     *in21         ifeq      *off
310400970320     C                   add       plfespeff     d3espeffp
310500970320     C                   add       plfimpeff     d3acqeffp
310600951113     C                   endif
310700951113     C                   ENDDO
310800951113     C*
310900951113     C                   ENDSR
311000951113     C/EJECT
311100970319     C************************************************************
311200970319     C* REPERIMENTO ESPORTAZ./IMPORTAZ. EFFETT. (PLAFOND FISSO)
311300970319     C************************************************************
311400970319     C     RepFisso      BEGSR
311500970319     C*
311600970319     C* Chiamata a routine esterna NDC423R per reperimento:
311700970319     C* 1) esportazioni effettuate nell'anno solare precedente
311800970319     C* 2) importazioni effettuate nell'anno corrente fino al mese prec.
311900970319     C                   clear                   ndc423ds
312000980807     C                   move      xscsoc        soc423
312100980807     C                   move      '1'           tipplaf423
312200980807     C                   move      d1mese        amcomp423
312300980807     C                   movel     d1anno        amcomp423
312400980807     C                   CALL      'NDC423R'
312500980807     C                   parm                    ndc423ds
312600980807     C                   z-add     espeasp423    d3espeffp
312700980807     C                   z-add     impeac423     d3acqeffp
312800980807     C*
312900980807     C                   ENDSR
313000970319     C/EJECT
313100991230     C************************************************************
313200991230     C* GESTIONE VALORI ASSUNTI
313300991230     C************************************************************
313400991230     C     GESD4         BEGSR
313500991230     C*
313600991230   B1C     $INZD4        IFEQ      *ON
313700991230     C                   EXSR      INZD4
313800991230     C                   MOVE      *OFF          $INZD4
313900991230   E1C                   ENDIF
314000991230     C*
314100991230   B1C     $LASTV        IFNE      'D4'
314200991230     C                   EXSR      GESPIE
314300991230     C                   MOVE      $LASTV        $LASTG
314400991230     C                   MOVE      'D4'          $LASTV
314500991230   E1C                   ENDIF
314600991230     C*
314700991230     C* se esistono errori sulla videata
314800991230     C* emetto la write del formato a indicatori spenti per vedere
314900991230     C* le eventuali decodifiche
315000991230   B1C     *IN99         IFEQ      *ON
315100991230     C                   MOVEA     *IN           WIN
315200991230     C                   MOVE      *ALL'0'       IN5098           49
315300991230     C                   MOVEA     IN5098        *IN(50)
315400991230     C                   WRITE     FMTD4
315500991230     C                   MOVEA     WIN           *IN
315600991230   E1C                   ENDIF
315700991230     C*
315800991230     C                   EXFMT     FMTD4
315900000718C1396C                   Eval      XMSGINZ = *On
316000991230     C*
316100991230     C* controllo abilitazione tasti funzione
316200991230     C                   EXSR      CTRKD4
316300991230     C*
316400991230   B1C     *IN99         IFEQ      *OFF
316500991230     C*
316600991230   B2C                   SELECT
316700991230     C* F3=Fine
316800991230     C     $TASTO        WHENEQ    F03
316900991230     C                   EXSR      F03D1
317000991230     C* F12=Ritorno
317100991230     C     $TASTO        WHENEQ    F12
317200991230     C                   EXSR      F12D4
317300991230     C* F24=Altri tasti
317400991230     C     $TASTO        WHENEQ    F24
317500991230     C                   EXSR      F24D4
317600991230     C*
317700991230     C                   OTHER
317800991230     C                   EXSR      CTRD4
317900991230     C     *IN99         IFEQ      *OFF
318000991230     C                   MOVE      $LASTG        $GEST
318100991230     C                   ENDIF
318200991230     C*
318300991230   E2C                   ENDSL
318400991230     C*
318500991230   E1C                   ENDIF
318600991230     C*
318700991230     C                   ENDSR
318800991230     C/EJECT
318900991230     C************************************************************
319000991230     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
319100991230     C************************************************************
319200991230     C     INZD4         BEGSR
319300991230     C*
319400991230     C                   CLEAR                   FMTD4
319500991230     C     $VAL          IFEQ      *ON
319600991230     C                   MOVE      XSCON         D4VAL
319700991230     C                   ELSE
319800991230     C                   MOVE      XSCOFF        D4VAL
319900991230     C                   ENDIF
320000991230     C                   MOVE      XSCON         D4ON
320100991230     C                   MOVE      XSCOFF        D4OF
320200991230     C*
320300991230     C                   ENDSR
320400991230     C/EJECT
320500991230     C************************************************************
320600991230     C* GESTIONE F12 VIDEO D4
320700991230     C************************************************************
320800991230     C     F12D4         BEGSR
320900991230     C*
321000991230     C                   MOVE      $LASTG        $GEST
321100991230     C*
321200991230     C                   ENDSR
321300991230     C/EJECT
321400991230     C************************************************************
321500991230     C* GESTIONE F24 VIDEO D4
321600991230     C************************************************************
321700991230     C     F24D4         BEGSR
321800991230     C*
321900991230     C                   MOVEA     $FX4          $FX
322000991230     C                   MOVEA     $FC4          $FC
322100991230     C                   MOVEA     $FR4          $FR
322200991230     C                   Z-ADD     $ULKD4        $ULK
322300991230     C                   Z-ADD     44            $L01
322400991230     C                   Z-ADD     0             $L02
322500991230     C                   EXSR      RIEKEY
322600991230     C                   Z-ADD     $ULK          $ULKD4            3 0
322700991230     C                   MOVEA     $FC           $FC4
322800991230     C                   MOVEL     $KEY1         Z4KE1
322900060224C1913C                   MOVEL     $ALLFUNCT     H4ALLFUNCT
323000991230     C                   WRITE     FMTZ4
323100991230     C*
323200991230     C                   ENDSR
323300991230     C/EJECT
323400991230     C************************************************************
323500991230     C* CONTROLLO VIDEATA D4
323600991230     C************************************************************
323700991230     C     CTRD4         BEGSR
323800991230     C*
323900991230     C                   SETOFF                                       99
324000991230     C* non imposto pos. del cursore
324100991230     C                   Z-ADD     0             H1RIGA
324200991230     C                   Z-ADD     0             H1COLO
324300991230     C*
324400991230     C     D4VAL         IFNE      XSCON
324500991230     C     D4VAL         ANDNE     XSCOFF
324600991230     C                   SETON                                        5099
324700991230     C                   ENDIF
324800991230     C*
324900991230     C     D4VAL         IFEQ      XSCON
325000991230     C                   MOVE      *ON           $VAL
325100991230     C                   ELSE
325200991230     C                   MOVE      *OFF          $val
325300991230     C                   ENDIF
325400991230     C*
325500991230     C                   ENDSR
325600991230     C/EJECT
325700940224     C************************************************************
325800950530     C* ESECUZIONE LANCIO
325900940224     C************************************************************
326000950530     C     EXELAN        BEGSR
326100940207     C*
326200000714C1396X*****              RESET                   C167DS
326300000714  "  X*****              move      xscsoc        soc167
326400000714  "  X*****              move      d1anno        ann167
326500000714  "  X*****              If        XscIvaLiq = '1'
326600000714  "  X*****              move      d1mese        mes167
326700000714  "  X*****              move      d1mese        per167
326800000714  "  X*****              Else
326900000714  "  X*****              move      d1perliq      per167
327000000714  "  X*****              move      *blank        mes167
327100000714  "  X*****              EndIf
327200000714  "  X*****d2bol         ifeq      xscon
327300000714  "  X*****              move      *on           bol167
327400000714  "  X*****              else
327500000714  "  X*****              move      *off          bol167
327600000714  "  X*****              endif
327700000719  "  X* Dati x riepiloghi IVA
327800000714  "  X*****d2stpcau      ifeq      xscon
327900000714  "  X*****              move      *on           cau167
328000000714  "  X*****              else
328100000714  "  X*****              move      *off          cau167
328200000714  "  X*****              endif
328300000714  "  X*****              z-add     salperpr      spr167
328400000714  "  X*****              z-add     salintpr      ipr167
328500000714  "  X*****              z-add     salperio      spe167
328600000714  "  X*****              z-add     salintra      ipe167
328700000714C1396X*****              z-add     salnuovo      snu167
328800951221     C* Dati x plafond
328900951221     C                   move      stpplaf       stp167
329000951113     C     d3stpdef      ifeq      xscon
329100951113     C                   move      *on           sdp167
329200951113     C                   else
329300951113     C                   move      *off          sdp167
329400951113     C                   endif
329500970320     C                   z-add     d3espeffp     espeffp167
329600970320     C                   z-add     d3acqeffp     acqeffp167
329700951113     C                   z-add     d3acqeff      acqeff167
329800970320     C                   z-add     d3plafond     plafond167
329900991230     C* se richiesto solo plafond no intestazione di pagina (bollato = si)
330000991230     C                   If        StpPlaf = '2'
330100991230     C                   Eval      Bol167 = *On
330200991230     C                   EndIf
330300030623C1668C* Se richiesta stampa intestazione
330400030623  "  c                   if        Intesta167=*on and
330500030623  "  c* e se numerazione riepilogo iva in coda
330600030623  "  C                             NumRiep045='1' and
330700030623  "  c* e se numerazione registri iva x libro
330800030623  "  C                             Numeraz045=' '
330900030623  "  c* chiedo quale è l'ultimo libro stampato
331000060601C1668C     *in99         doueq     *off
331100060601C1934X*ex C1668  $tasto        andne     F02
331200060601C1934X*ex C1668  $tasto        andne     F04
331300060601C1934c     $tasto        andeq     F06
331400060531C1668c                   exfmt     FMTD7
331500030623  "  c* decodifica
331600030623  "  c                   if        $tasto=F02
331700030623  "  C                   EXSR      CtrLibro
331800030623  "  c                   endif
331900030623  "  c* ricerca
332000030623  "  c                   if        $tasto=F04
332100030623  "  C                   EXSR      CercaLibro
332200030623  "  c                   endif
332300030623  "  c* controllo
332400030623  "  C                   EXSR      CtrLibro
332500030623  "  c                   enddo
332600030623  "  c                   move      D7libro       Libro167
332700030623c1668C                   ENDIF
332800040726C1781c                   move      $TpTesto167   TpTesto167
332900091218C2149c                   move      $TpTestG167   TpTestG167
333000030623     c*
333100950721     C                   movel     C167DS        KPJBU
333200030623     C
333300031201C1703C                   if        knmus = 'DEBUG'
333400031201  "  C                   CALL      'NDC167R'
333500031201  "  C                   PARM                    KPJBA
333600031201C1703C                   else
333700030414D1320C                   MOVE      'C167'        KCOAZ
333800030414  "  x***                CALL      'NDC167R'
333900030414D1320C                   CALL      'BCH10'
334000950721     C                   PARM                    KPJBA
334100031201C1703C                   endif
334200950721     C                   movel     KPJBU         C167DS
334300991230     C*
334400991230     C* Aggiorno valori assunti
334500991230     C                   EXSR      AGGMPE
334600950727     C* Fine
334700950727     C                   move      *on           $fine
334800950717     C*
334900940203     C                   ENDSR
335000941205     C/EJECT
335100030623c1668C************************************************************
335200030623     C* Cerca libro tabella 008
335300030623c1668C************************************************************
335400030623     c     CercaLibro    begsr
335500030623     c*
335600030623     C                   RESET                   XTABDS
335700030623     C                   MOVE      '01'          XTAOPZ
335800030623     C                   MOVE      XSCSOC        XTAAZI
335900030623     C                   MOVE      XSCLIN        XTALIN
336000030623     C                   Z-ADD     CURR          XTARIG
336100030623     C                   Z-ADD     CURC          XTACOL
336200030623     c                   eval      Xtapos =2
336300030623     C                   CLEAR                   KPJBU
336400030623     C                   MOVEL     XTABDS        KPJBU
336500030623     C                   CALL      'ANG008R'
336600030623     C                   PARM                    KPJBA
336700030623     C                   MOVEL     KPJBU         XTABDS
336800030623     C     XTAERR        IFNE      '1'
336900030623     C                   MOVE      XTAKEY        d7libro
337000030623     C                   ENDIF
337100030623     c*
337200030623c1668c                   endsr
337300030623c1668C************************************************************
337400030623     C* CONTROLLO esistenza libro tabella 008
337500030623c1668C************************************************************
337600030623     C     CtrLibro      BEGSR
337700030623     c*
337800030623     c                   setoff                                       99
337900030623     c*
338000030623     C                   IF        D7Libro = *blanks
338100030623     c                   seton                                        5099
338200030623     c                   endif
338300030623     c* aggancio tabella 008 PER LIBRO e il registro è sempre quello
338400030623     c* delle vendite
338500030623     c                   reset                   xatbds
338600030623     C                   MOVE      xscsoc        XTBAZI
338700030623     C                   MOVE      '008'         XTBcod
338800030623     c                   eval      xtbkey = '00000000000' + '2' + d7libro
338900030623     C                   MOVE      '1'           XTBRIC
339000030623     C                   MOVE      '0'           XTBALC
339100030623     C                   CALLB     'XATB'
339200030623     c                   parm                    xatbds
339300030623     c                   if        xtbErr = *off
339400030623     c                   eval      ang008ds = xtbuni
339500030623     c                   if        %subst(ang008ds:27:9) = *blanks
339600030623     c                   eval      %subst(ang008ds:27:9) = *zeros
339700030623     c                   endif
339800030623     c                   if        %subst(ang008ds:21:6) = *blanks
339900030623     c                   eval      %subst(ang008ds:21:6) = *zeros
340000030623     c                   endif
340100030623     c                   z-add     Pagina008     d7NumRiep         6 0
340200030623     c                   eval      d7libroD=desBrv008
340300030623     c                   else
340400030623     c                   seton                                        5099
340500030623     c                   endif
340600030623     C*
340700030623c1668C                   ENDSR
340800950530     C************************************************************
340900950530     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
341000950530     C************************************************************
341100950530     C     CTRKD1        BEGSR
341200950530     C*
341300950530     C                   SETOFF                                       99
341400950530     C                   MOVEA     $FC1          $FC
341500950530     C                   EXSR      CTRKEY
341600950530     C     *IN99         IFEQ      *ON
341700950530     C                   SETON                                        97
341800950530     C                   ENDIF
341900950530     C*
342000950530     C                   ENDSR
342100950530     C/EJECT
342200950530     C************************************************************
342300950717     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
342400950530     C************************************************************
342500950717     C     CTRKD2        BEGSR
342600950530     C*
342700950530     C                   SETOFF                                       99
342800950717     C                   MOVEA     $FC2          $FC
342900950530     C                   EXSR      CTRKEY
343000950530     C     *IN99         IFEQ      *ON
343100950530     C                   SETON                                        97
343200950530     C                   ENDIF
343300950530     C*
343400950530     C                   ENDSR
343500950530     C/EJECT
343600991230     C************************************************************
343700991230     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D4
343800991230     C************************************************************
343900991230     C     CTRKD4        BEGSR
344000991230     C*
344100991230     C                   SETOFF                                       99
344200991230     C                   MOVEA     $FC4          $FC
344300991230     C                   EXSR      CTRKEY
344400991230     C     *IN99         IFEQ      *ON
344500991230     C                   SETON                                        97
344600991230     C                   ENDIF
344700991230     C*
344800991230     C                   ENDSR
344900991230     C/EJECT
345000941205     C************************************************************
345100941205     C* CONTROLLO ABILITAZIONE TASTI
345200941205     C************************************************************
345300941205     C     CTRKEY        BEGSR
345400941205     C*
345500941205     C                   SELECT
345600941205     C     $TASTO        WHENEQ    F01
345700941205     C     $FC(1)        IFEQ      '0'
345800941205     C                   SETON                                        99
345900941205     C                   ENDIF
346000941205     C     $TASTO        WHENEQ    F02
346100941205     C     $FC(2)        IFEQ      '0'
346200941205     C                   SETON                                        99
346300941205     C                   ENDIF
346400941205     C     $TASTO        WHENEQ    F03
346500941205     C     $FC(3)        IFEQ      '0'
346600941205     C                   SETON                                        99
346700941205     C                   ENDIF
346800941205     C     $TASTO        WHENEQ    F04
346900941205     C     $FC(4)        IFEQ      '0'
347000941205     C                   SETON                                        99
347100941205     C                   ENDIF
347200941205     C     $TASTO        WHENEQ    F05
347300941205     C     $FC(5)        IFEQ      '0'
347400941205     C                   SETON                                        99
347500941205     C                   ENDIF
347600941205     C     $TASTO        WHENEQ    F06
347700941205     C     $FC(6)        IFEQ      '0'
347800941205     C                   SETON                                        99
347900941205     C                   ENDIF
348000941205     C     $TASTO        WHENEQ    F07
348100941205     C     $FC(7)        IFEQ      '0'
348200941205     C                   SETON                                        99
348300941205     C                   ENDIF
348400941205     C     $TASTO        WHENEQ    F08
348500941205     C     $FC(8)        IFEQ      '0'
348600941205     C                   SETON                                        99
348700941205     C                   ENDIF
348800941205     C     $TASTO        WHENEQ    F09
348900941205     C     $FC(09)       IFEQ      '0'
349000941205     C                   SETON                                        99
349100941205     C                   ENDIF
349200941205     C     $TASTO        WHENEQ    F10
349300941205     C     $FC(10)       IFEQ      '0'
349400941205     C                   SETON                                        99
349500941205     C                   ENDIF
349600941205     C     $TASTO        WHENEQ    F11
349700941205     C     $FC(11)       IFEQ      '0'
349800941205     C                   SETON                                        99
349900941205     C                   ENDIF
350000941205     C     $TASTO        WHENEQ    F12
350100941205     C     $FC(12)       IFEQ      '0'
350200941205     C                   SETON                                        99
350300941205     C                   ENDIF
350400941205     C     $TASTO        WHENEQ    F13
350500941205     C     $FC(13)       IFEQ      '0'
350600941205     C                   SETON                                        99
350700941205     C                   ENDIF
350800941205     C     $TASTO        WHENEQ    F14
350900941205     C     $FC(14)       IFEQ      '0'
351000941205     C                   SETON                                        99
351100941205     C                   ENDIF
351200941205     C     $TASTO        WHENEQ    F15
351300941205     C     $FC(15)       IFEQ      '0'
351400941205     C                   SETON                                        99
351500941205     C                   ENDIF
351600941205     C     $TASTO        WHENEQ    F16
351700941205     C     $FC(16)       IFEQ      '0'
351800941205     C                   SETON                                        99
351900941205     C                   ENDIF
352000941205     C     $TASTO        WHENEQ    F17
352100941205     C     $FC(17)       IFEQ      '0'
352200941205     C                   SETON                                        99
352300941205     C                   ENDIF
352400941205     C     $TASTO        WHENEQ    F18
352500941205     C     $FC(18)       IFEQ      '0'
352600941205     C                   SETON                                        99
352700941205     C                   ENDIF
352800941205     C     $TASTO        WHENEQ    F19
352900941205     C     $FC(19)       IFEQ      '0'
353000941205     C                   SETON                                        99
353100941205     C                   ENDIF
353200941205     C     $TASTO        WHENEQ    F20
353300941205     C     $FC(20)       IFEQ      '0'
353400941205     C                   SETON                                        99
353500941205     C                   ENDIF
353600941205     C     $TASTO        WHENEQ    F21
353700941205     C     $FC(21)       IFEQ      '0'
353800941205     C                   SETON                                        99
353900941205     C                   ENDIF
354000941205     C     $TASTO        WHENEQ    F22
354100941205     C     $FC(22)       IFEQ      '0'
354200941205     C                   SETON                                        99
354300941205     C                   ENDIF
354400941205     C     $TASTO        WHENEQ    F23
354500941205     C     $FC(23)       IFEQ      '0'
354600941205     C                   SETON                                        99
354700941205     C                   ENDIF
354800941205     C     $TASTO        WHENEQ    F24
354900941205     C     $FC(24)       IFEQ      '0'
355000941205     C                   SETON                                        99
355100941205     C                   ENDIF
355200941205     C                   ENDSL
355300941205     C*
355400941205     C                   ENDSR
355500940207     C/EJECT
355600951113     C******************************************************
355700951221     C* EMISSIONE FINESTRA CONFERMA STP SOLO RIEPILOGHI    *
355800951113     C******************************************************
355900951221     C     EMTWIN1       BEGSR
356000951113     C*
356100951113     C                   CLEAR                   XMSGDS
356200000718C1396C                   CLEAR                   XMSGTXT
356300971008     C* Txt messaggio 1
356400951113     C                   MOVEL     $COS(1)       IDMSG
356500951113     C                   CALLB     'XRTVM'
356600951113     C                   PARM                    IDMSG            27
356700951113     C                   PARM                    TXTMSG          100
356800951113     C                   PARM                    ERRMSG            1
356900951113     C     ERRMSG        IFNE      *ON
357000951113     C                   MOVEL     TXTMSG        MSGMS2
357100951113     C                   ELSE
357200951113     C                   MOVEL     *ALL'?'       MSGMS2
357300951113     C                   ENDIF
357400971008     C* Txt messaggio 2
357500951114     C                   MOVEL     $COS(2)       IDMSG
357600951114     C                   CALLB     'XRTVM'
357700951114     C                   PARM                    IDMSG            27
357800951114     C                   PARM                    TXTMSG          100
357900951114     C                   PARM                    ERRMSG            1
358000951114     C     ERRMSG        IFNE      *ON
358100951114     C                   MOVEL     TXTMSG        MSGMS3
358200951114     C                   ELSE
358300951114     C                   MOVEL     *ALL'?'       MSGMS3
358400951114     C                   ENDIF
358500971008     C* Txt bottoni risposta
358600971008     C                   MOVEL     TxtBott       IDMSG
358700971008     C                   CALLB     'XRTVM'
358800971008     C                   PARM                    IDMSG            27
358900971008     C                   PARM                    TXTMSG          100
359000971008     C                   PARM                    ERRMSG            1
359100971008     C                   PARM      *OFF          CTRMSG            1
359200971008     C                   PARM      135           LENMSG            3 0
359300971008     C     ERRMSG        IFNE      *ON
359400971008     C                   MOVEL     TXTMSG        XMSGTXT
359500971008     C                   ELSE
359600971008     C                   MOVEL     *ALL'?'       XMSGTXT
359700971008     C                   ENDIF
359800971008     C*
359900951113     C                   Z-ADD     0             MSGMSG
360000951221     C                   MOVEL     'S'           MSGRSP
360100951221     C                   MOVEL     'AC'          MSGTPR
360200951113     C                   MOVEL     'S'           MSGEMV
360300951113     C                   Z-ADD     10            MSGRGP
360400951113     C                   MOVEL     'N'           MSGLCK
360500951113     C                   MOVEA     STATUS        STS
360600951113     C                   MOVEL     'N'           MSGCNL
360700951113     C                   MOVEL     'N'           MSGVID
360800951113     C                   MOVEL     'N'           MSGOPE
360900951113     C                   MOVEL     'N'           MSGSTP
361000971008     C*
361100951113     C                   CALL      'XMSGR'
361200951113     C                   PARM                    XMSGDS
361300951221     C                   PARM                    XMSGTXT         135
361400000718C1396C                   PARM                    XMSGINZ           1
361500000718C1396C                   Eval      XMSGINZ = *Off
361600951113     C*
361700951113     C                   ENDSR
361800951113     C/EJECT
361900951221     C******************************************************
362000951221     C* EMISSIONE FINESTRA AVVERTIMENTO CALCOLO PLAFOND    *
362100951221     C******************************************************
362200951222     C     EMTWIN2       BEGSR
362300951221     C*
362400951221     C                   CLEAR                   XMSGDS
362500000718C1396C                   CLEAR                   XMSGTXT
362600951221     C*
362700951221     C                   MOVEL     $COS(1)       IDMSG
362800951221     C                   CALLB     'XRTVM'
362900951221     C                   PARM                    IDMSG            27
363000951221     C                   PARM                    TXTMSG          100
363100951221     C                   PARM                    ERRMSG            1
363200951221     C     ERRMSG        IFNE      *ON
363300951221     C                   MOVEL     TXTMSG        MSGMS2
363400951221     C                   ELSE
363500951221     C                   MOVEL     *ALL'?'       MSGMS2
363600951221     C                   ENDIF
363700951222     C                   MOVEL     $COS(3)       IDMSG
363800951221     C                   CALLB     'XRTVM'
363900951221     C                   PARM                    IDMSG            27
364000951221     C                   PARM                    TXTMSG          100
364100951221     C                   PARM                    ERRMSG            1
364200951221     C     ERRMSG        IFNE      *ON
364300951221     C                   MOVEL     TXTMSG        MSGMS3
364400951221     C                   ELSE
364500951221     C                   MOVEL     *ALL'?'       MSGMS3
364600951221     C                   ENDIF
364700951221     C                   Z-ADD     0             MSGMSG
364800951221     C                   MOVEL     'N'           MSGRSP
364900951221     C                   MOVEL     'S'           MSGEMV
365000951221     C                   Z-ADD     10            MSGRGP
365100951221     C                   MOVEL     'N'           MSGLCK
365200951221     C                   MOVEA     STATUS        STS
365300951221     C                   MOVEL     'N'           MSGCNL
365400951221     C                   MOVEL     'N'           MSGVID
365500951221     C                   MOVEL     'N'           MSGOPE
365600951221     C                   MOVEL     'N'           MSGSTP
365700000718     C*
365800951221     C                   CALL      'XMSGR'
365900951221     C                   PARM                    XMSGDS
366000000718C1396C                   PARM                    XMSGTXT         135
366100000718  "  C                   PARM                    XMSGINZ           1
366200000718C1396C                   Eval      XMSGINZ = *Off
366300951221     C*
366400951221     C                   ENDSR
366500951221     C/EJECT
366600981211     C******************************************************
366700981211     C* Window conferma elaborazione periodo senza riepil. *
366800981211     C******************************************************
366900981211     C     EMTWIN3       BEGSR
367000981211     C*
367100981211     C                   CLEAR                   XMSGDS
367200000718C1396C                   CLEAR                   XMSGTXT
367300981211     C* Txt messaggio 1
367400981211     C                   MOVEL     $COS(4)       IDMSG
367500981211     C                   CALLB     'XRTVM'
367600981211     C                   PARM                    IDMSG            27
367700981211     C                   PARM                    TXTMSG          100
367800981211     C                   PARM                    ERRMSG            1
367900981211     C     ERRMSG        IFNE      *ON
368000981211     C                   MOVEL     TXTMSG        MSGMS2
368100981211     C                   ELSE
368200981211     C                   MOVEL     *ALL'?'       MSGMS2
368300981211     C                   ENDIF
368400981211     C* Txt messaggio 2
368500981211     C                   MOVEL     $COS(2)       IDMSG
368600981211     C                   CALLB     'XRTVM'
368700981211     C                   PARM                    IDMSG            27
368800981211     C                   PARM                    TXTMSG          100
368900981211     C                   PARM                    ERRMSG            1
369000981211     C     ERRMSG        IFNE      *ON
369100981211     C                   MOVEL     TXTMSG        MSGMS3
369200981211     C                   ELSE
369300981211     C                   MOVEL     *ALL'?'       MSGMS3
369400981211     C                   ENDIF
369500981211     C* Txt bottoni risposta
369600981211     C                   MOVEL     TxtBott       IDMSG
369700981211     C                   CALLB     'XRTVM'
369800981211     C                   PARM                    IDMSG            27
369900981211     C                   PARM                    TXTMSG          100
370000981211     C                   PARM                    ERRMSG            1
370100981211     C                   PARM      *OFF          CTRMSG            1
370200981211     C                   PARM      135           LENMSG            3 0
370300981211     C     ERRMSG        IFNE      *ON
370400981211     C                   MOVEL     TXTMSG        XMSGTXT
370500981211     C                   ELSE
370600981211     C                   MOVEL     *ALL'?'       XMSGTXT
370700981211     C                   ENDIF
370800981211     C*
370900981211     C                   Z-ADD     0             MSGMSG
371000981211     C                   MOVEL     'S'           MSGRSP
371100981211     C                   MOVEL     'AC'          MSGTPR
371200981211     C                   MOVEL     'S'           MSGEMV
371300981211     C                   Z-ADD     10            MSGRGP
371400981211     C                   MOVEL     'N'           MSGLCK
371500981211     C                   MOVEA     STATUS        STS
371600981211     C                   MOVEL     'N'           MSGCNL
371700981211     C                   MOVEL     'N'           MSGVID
371800981211     C                   MOVEL     'N'           MSGOPE
371900981211     C                   MOVEL     'N'           MSGSTP
372000981211     C*
372100981211     C                   CALL      'XMSGR'
372200981211     C                   PARM                    XMSGDS
372300981211     C                   PARM                    XMSGTXT         135
372400000718C1396C                   PARM                    XMSGINZ           1
372500000718C1396C                   Eval      XMSGINZ = *Off
372600981211     C*
372700981211     C                   ENDSR
372800981211     C/EJECT
372900940131     C************************************************************
373000940131     C* OPERAZIONI INIZIALI
373100940131     C************************************************************
373200940131     C     *INZSR        BEGSR
373300940131     C*
373400000202C1329C     *DMY          Move      UDate         Oggi
373500000202     C*
373600940127     C* Reperimento parametri
373700940127     C*
373800940117     C     *ENTRY        PLIST
373900940117     C                   PARM                    KPJBA
374000940506     C*
374100940506     C* Reperimento tasti di funzione
374200940506     C*
374300940506     C                   EXSR      REPKEY
374400940506     C*
374500940223     C                   ENDSR
374600940223     C/EJECT
374700940506     C************************************************************
374800940506     C* REPERIMENTO TASTI DI FUNZIONE
374900940506     C************************************************************
375000940506     C     REPKEY        BEGSR
375100951002     C*
375200940506     C                   MOVEA     $FM1          $FM
375300951002     C                   MOVEA     $FC1          $FC
375400940506     C                   EXSR      REPKKK
375500940506     C                   MOVEA     $FX           $FX1
375600950530     C*
375700950717     C                   MOVEA     $FM2          $FM
375800951002     C                   MOVEA     $FC2          $FC
375900950530     C                   EXSR      REPKKK
376000950717     C                   MOVEA     $FX           $FX2
376100991230     C*
376200991230     C                   MOVEA     $FM4          $FM
376300991230     C                   MOVEA     $FC4          $FM
376400991230     C                   EXSR      REPKKK
376500991230     C                   MOVEA     $FX           $FX4
376600940614     C*
376700020429C1550 *
376800020429  "  C                   MOVEA     $FM9          $FM
376900020429  "  C                   MOVEA     $FC9          $FM
377000020429  "  C                   EXSR      REPKKK
377100020429  "  C                   MOVEA     $FX           $FX9
377200020429C1550 *
377300940614     C                   ENDSR
377400940506     C/EJECT
377500940506     C************************************************************
377600940506     C* REPERIMENTO TASTI DI FUNZIONE
377700940506     C************************************************************
377800940506     C     REPKKK        BEGSR
377900940506     C*
378000940506     C                   CLEAR                   $FX
378100940506     C     1             DO        24            X
378200940506     C*
378300940506     C     $FM(X)        IFNE      *BLANK
378400951002     C     $FC(X)        ANDNE     '0'
378500940506     C*
378600940506     C                   MOVEL     $FM(X)        IDMSG
378700950522     C                   CALLB     'XRTVM'
378800940506     C                   PARM                    IDMSG            27
378900940506     C                   PARM                    TXTMSG          100
379000940506     C                   PARM                    ERRMSG            1
379100940506     C*
379200940506     C     ERRMSG        IFNE      *ON
379300940506     C                   MOVEL     TXTMSG        $FX(X)
379400940506     C                   ELSE
379500940506     C                   MOVEL     *ALL' '       $FX(X)
379600941027     C                   ENDIF
379700940506     C*
379800941027     C                   ENDIF
379900940506     C*
380000941027     C                   ENDDO
380100940506     C*
380200940506     C                   ENDSR
380300940509     C/EJECT
380400940223     C************************************************************
380500940223     C* INIZIALIZZAZIONE VARIABILI
380600940223     C************************************************************
380700940223     C     INZVAR        BEGSR
380800951221     C*
380900941201     C* Reperimento dati società
381000941201     C*
381100941115    >C                   MOVEL     'SOC001'      TIPXSC
381200941115     C                   MOVEL     *BLANK        SOCXSC
381300941115     C                   EXSR      REPSOC
381400941115     C     RTNXSC        IFNE      '1'
381500941115     C                   MOVEL     XSOCDS        SOC001
381600941115     C                   ELSE
381700941115     C                   RETURN
381800941115     C                   ENDIF
381900951221     C*
382000980807     C* Forzatura Regime se non presente in anagrafica
382100000202     C                   If        XscIvaRgm = ' '
382200000202     C                   Eval      XscIvaRgm = '0'
382300000202     C                   EndIf
382400980807     C*
382500981118     C* Forzatura liquidazione se non presente in anagrafica
382600000202     C                   If        XscIvaLiq = ' '
382700000202     C                   Eval      XscIvaLiq = '1'
382800000202     C                   EndIf
382900090626C2131C*
383000090626  "  C* reperisco dai dati di società i numeri dei decimali della moneta di
383100090626  "  C* conto
383200090626  "  C                   Eval      WNrDecMC   =  %SubSt(XScSKp:6:1)
383300090626C2131C                   Move      WNrDecMC      NrDecMC
383400000202     C*
383500000202C1329C* Reperisco codice internazionale m.c.
383600000202  "  C                   Clear                   CdInter
383700000202  "  C                   If        X0201RpDVS(XScDiv: Oggi:
383800000202  "  C                                        ANDVS00F: %Size(ANDVS00F):
383900000202  "  C                                        ANDVX00F: %Size(ANDVX00F)) = 0
384000000202  "  C                   Move      DVSCdInter    CdInter
384100000202C1329C                   EndIf
384200991230     C*
384300991230     C* Reperimento valori default utente
384400991230     C*
384500991230     C                   EXSR      REPDFT
384600981118     C*
384700950301     C* Reperimento autorizzazioni
384800950301     C*
384900950301    >C                   EXSR      REPAUT
385000000802C1396C*
385100000802  "  C* Reperimento parametro IVA (anagrafiche iva)
385200000802  "  C*
385300000802  "  C                   CALLB     'XPAR'
385400000802  "  C                   parm                    xscsoc
385500000802  "  C                   parm      'IVA     '    xpapar            8
385600000802  "  C                   parm                    xpaout
385700000802  "  C                   parm      *OFF          xpaerr            1
385800000802  "  C                   parm      ' '           xparic            1
385900000802  "  C     xpaerr        ifeq      *ON
386000000802  "  C                   MOVE      *ON           $FINE
386100000802  "  C                   EXSR      ENDPGM
386200000802  "  C                   else
386300000802  "  C                   movel     xpaout        ANP045DS
386400000802  "  C                   endif
386500000802  "  C* Imposto società capofila per accesso al file NDCEL00F
386600000802  "  C                   If        Cap045 <> *Blank
386700000802  "  C                   Eval      WSocCap = Cap045
386800000802  "  C                   Else
386900000802  "  C                   Eval      WSocCap = XScSoc
387000000802C1396C                   EndIf
387100980401     C*
387200000202     C* Reperimento parametri IVALIQ (importo limite versamento)
387300980401     C*
387400980401     C                   CALLB     'XPAR'
387500980401     C                   parm                    xscsoc
387600980402     C                   parm      'IVALIQ  '    xpapar            8
387700980401     C                   parm                    xpaout
387800980401     C                   parm      *OFF          xpaerr            1
387900980401     C                   parm      ' '           xparic            1
388000980401     C     xpaerr        ifeq      *ON
388100980401     C                   MOVE      *ON           $FINE
388200980401     C                   EXSR      ENDPGM
388300980402     C                   else
388400980402     C                   movel     xpaout        NDC149DS
388500980401     C                   endif
388600980401     C*
388700980401     C                   if        XScPlafond = '1' or XScPlafond = '2'
388800980401     C* Se gestito plafond emetto finestra per chiedere se si vuole
388900980401     C* la stampa dei riepiloghi o del plafond solamente
389000980401     C                   EXSR      GESD0
389100980401     C                   else
389200980401     C* altrimenti considero la stampa di tutto
389300000201     C                   z-add     1             d0tpela
389400980401     C                   endif
389500940715     C*
389600940715     C* Definizione dei tasti funzione utilizzabili a pgm
389700940715     C*
389800940715     C                   EXSR      DEFOPZ
389900940128     C*
390000940223     C* Variabili per gestione videate
390100940128     C*
390200940223     C                   MOVE      'D1'          $GEST
390300940223     C                   MOVE      *OFF          $FINE
390400940223     C                   MOVE      *ON           $INZD1
390500950717     C                   MOVE      *OFF          $INZD2
390600020423     C                   MOVE      *OFF          $INZD9
390700940224     C                   MOVE      *BLANK        $LASTG
390800940224     C                   MOVE      *BLANK        $LASTV
390900940506     C                   Z-ADD     0             $ULKD1            3 0
391000950719     C                   Z-ADD     0             $ULKD2            3 0
391100991230     C                   Z-ADD     0             $ULKD4            3 0
391200020423     C                   Z-ADD     0             $ULKD9            3 0
391300940127     C*
391400940223     C* Variabili appoggio
391500940127     C*
391600940224     C                   Z-ADD     0             CURR
391700940224     C                   Z-ADD     0             CURC
391800940207     C*
391900940207     C* Valorizzazione campi univoci testate
392000940207     C*
392100940207     C                   CLEAR                   FMTT1
392200940207     C                   MOVEL     KNSIF         NOMSIF
392300941110     C                   MOVEL     XSCDSI        NOMDIT
392400941027     C                   MOVEL     DSPGM         NOMPGM
392500950331     C* recupero il sottotitolo (se c'è)
392600950331     C                   MOVEL     *BLANK        T1TIT
392700950331     C     SOTTIT        IFNE      *BLANK
392800950331     C                   MOVEL     SOTTIT        IDMSG
392900950522     C                   CALLB     'XRTVM'
393000950331     C                   PARM                    IDMSG            27
393100950331     C                   PARM                    TXTMSG          100
393200950331     C                   PARM                    ERRMSG            1
393300950331     C                   PARM      *ON           CTRMSG            1
393400950331     C                   PARM      30            LENMSG            3 0
393500950331     C     ERRMSG        IFNE      *ON
393600950331     C                   MOVEL     TXTMSG        T1TIT
393700950331     C                   ELSE
393800950331     C                   MOVEL     *ALL'?'       T1TIT
393900950331     C                   ENDIF
394000950331     C                   ENDIF
394100980720     C*
394200940224     C* Inizializzazione/Decodifica prima videata
394300020423C1550 *
394400020423  "   * se è regime iva trasportatori mista
394500020423  "   * passo prima dalla videata che chiede il tipo liquidazione
394600020423  "  C                   if        XScIvaRgm = '2'
394700020423  "  C                   move      'D9'          $Gest
394800020423  "  C                   move      *on           $InZD9
394900020423  "  C                   exsr      InzD9
395000020423  "  C                   move      *off          $InzD9
395100020423  "   *
395200020423  "   * non sono in regime iva trasportatori mista
395300020423C1550C                   else
395400940224     C*
395500940224     C                   EXSR      INZD1
395600940224     C                   MOVE      *OFF          $INZD1
395700020423C1550 *
395800020423  "  C                   endif
395900020423C1550 *
396000951221     C*
396100940117     C                   ENDSR
396200941209     C/EJECT
396300941209     C************************************************************
396400941209     C* REPERIMENTO DATI SOCIETA'
396500941209     C************************************************************
396600941209     C     REPSOC        BEGSR
396700941209     C*
396800950522     C                   CALLB     'XSOC'
396900941209     C                   PARM                    TIPXSC            6
397000941209     C                   PARM                    SOCXSC            3
397100941209     C                   PARM                    CDSXSC            9 0
397200941209     C                   PARM                    MODXSC            3
397300941209     C                   PARM      *BLANKS       RTNXSC            1
397400941209     C                   PARM                    XSOCDS
397500941209     C                   PARM                    KPJBA
397600941209     C*
397700941209     C                   ENDSR
397800950301     C/EJECT
397900991230     C************************************************************
398000991230     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
398100991230     C************************************************************
398200991230     C     REPDFT        BEGSR
398300991230     C*
398400991230     C* preparo i parametri di chiamata al driver
398500991230     C                   CLEAR                   XMPEDS
398600991230     C                   EVAL      TpChiamata = '01'
398700991230     C                   EVAL      NChiavi = 5
398800991230     C                   EVAL      VLogica = 2
398900991230     C*
399000991230     C* preparo le chiavi per la chiamata al driver
399100991230     C                   CLEAR                   XMPEDS1
399200991230     C                   MOVEL     XSCSOC        DSMPESOC
399300991230     C                   MOVEL     DSPGM         DSMPEPGM
399400991230     C                   MOVEL     KNMUS         DSMPEPRF
399500991230     C* Si reperiscono i dati di default a seconda delle modalità con
399600991230     C* cui il pgm è usato
399700991230     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
399800991230     C* MPETIP = 'PV'  pgm richiamato da menù
399900991230     C* MPETIP = 'PB'  x lancio Batch
400000991230     C                   MOVEL     'PB'          DSMPETIP
400100991230     C*
400200991230     C                   CALLB     'XMPE'
400300991230     C                   PARM                    XMPEDS
400400991230     C                   PARM                    XMPEDS1
400500991230     C*
400600991230     C     Risultato     IFEQ      '0'
400700991230     C                   MOVEL     DSMPEPAR      DSMPE
400800991230     C* nel caso sia la prima volta che l'utente apre questa anagrafica
400900991230     C* i suoi valori di default vengono decisi a pgm
401000991230     C                   ELSE
401100991230     C                   MOVE      *OFF          $VAL
401200991230     C                   ENDIF
401300991230     C*
401400991230     C                   ENDSR
401500991230     C/EJECT
401600991230     C************************************************************
401700991230     C* AGGIORNAMENTO VALORI ASSUNTI
401800991230     C************************************************************
401900991230     C     AGGMPE        BEGSR
402000991230     C*
402100991230     C* preparo i parametri di chiamata al driver
402200991230     C                   CLEAR                   XMPEDS
402300991230     C                   EVAL      TpChiamata = '03'
402400991230     C                   EVAL      NChiavi = 5
402500991230     C                   EVAL      VLogica = 2
402600991230     C*
402700991230     C* preparo le chiavi per la chiamata al driver
402800991230     C                   CLEAR                   XMPEDS1
402900991230     C                   MOVEL     XSCSOC        DSMPESOC
403000991230     C                   MOVEL     DSPGM         DSMPEPGM
403100991230     C                   MOVEL     KNMUS         DSMPEPRF
403200991230     C*
403300991230     C* Si memorizzano i dati di default a seconda delle modalità con
403400991230     C* cui il pgm è usato
403500991230     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
403600991230     C* MPETIP = 'PV'  pgm richiamato da menù
403700991230     C* MPETIP = 'PB'  x lancio Batch
403800991230     C*
403900991230     C                   MOVEL     'PB'          DSMPETIP
404000991230     C* dati
404100991230     C                   MOVE      D2Bol         $Bol
404200030623c1668C                   MOVE      D2intesta     $Intesta
404300991230     C                   MOVE      D2StpCau      $StpCau
404400001229C1457C                   Move      D2ColPrLq     $ColPrLq
404500020222d1095C                   Move      D2ProsLiq     $ProsLiq
404600021010C1590C                   Move      D2StpTotR     $StpTotR
404700031128C1703C                   Move      D2StpLiqG     $StpLiqG
404800991230     C                   MOVEL     DSMPE         DSMPEPAR
404900991230     C*
405000991230     C                   CALLB     'XMPE'
405100991230     C                   PARM                    XMPEDS
405200991230     C                   PARM                    XMPEDS1
405300991230     C*
405400991230     C                   ENDSR
405500991230     C/EJECT
405600950301     C************************************************************
405700950301     C* REPERIMENTO AUTORIZZAZIONI UTENTE
405800950301     C************************************************************
405900950301     C     REPAUT        BEGSR
406000950301     C*
406100950301     C                   CLEAR                   AUTDS
406200950301     C*
406300950301     C                   MOVEL     XSCSOC        XCASOC
406400950301     C                   MOVEL     XSCDUN        XCAUNI
406500950301     C                   Z-ADD     0             XCAGRP
406600950301     C                   MOVEL     KNMUS         XCAPRF
406700950717    >C                   MOVEL     'CONCIV'      XCAFNC
406800950301    >C                   MOVEL     *ZERO         XCAVFU
406900950301    >C                   MOVE      'CK'          XCATCK
407000950301    >C                   MOVE      '1'           XCANAU
407100950301     C*
407200950522     C                   CALLB     'XCHKAUT'
407300950301     C                   PARM                    AUTDS
407400950301     C*
407500950301     C* errori/non abilitazione
407600950301     C     XCARTN        IFNE      0
407700950717     C                   MOVE      *ON           $FINE
407800950301     C                   EXSR      ENDPGM
407900950301     C                   ELSE
408000950717    >C                   MOVEL     XCADDS        CCIVDS
408100950616     C* Se non autorizzati alla funzione oppure                              ed
408200950717    >C     STPRIV        IFEQ      *OFF
408300950607     C                   MOVE      XCAFNC        XNOFNC
408400950607     C                   MOVE      XCAPRF        XNOPRF
408500950607     C                   MOVE      XCACUT        XNOCUT
408600950607     C                   MOVE      XCATCK        XNOTCK
408700950607     C                   MOVE      '1'           XNOERR
408800950717     C                   CALL      'XNOAUTR'
408900950607     C                   PARM                    XNOADS
409000950717     C                   MOVE      *ON           $FINE
409100950607     C                   EXSR      ENDPGM
409200950607     C                   ENDIF
409300950301     C                   ENDIF
409400950301     C*
409500950301     C                   ENDSR
409600950530     C/EJECT
409700940715     C************************************************************
409800940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
409900940715     C************************************************************
410000940715     C     DEFOPZ        BEGSR
410100940715     C*
410200940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
410300940715     C* con S o N a seconda della utilizzabilità o meno
410400940715     C* del tasto funzione correlato
410500940715     C*
410600940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
410700940715     C* di compilazione
410800940715     C                   RESET                   $FC1
410900940715     C                   RESET                   $FR1
411000950717     C                   RESET                   $FC2
411100950717     C                   RESET                   $FR2
411200020423C1550C                   RESET                   $FC9
411300020423C1550C                   RESET                   $FR9
411400940715     C*
411500940715     C                   ENDSR
411600950530     C/EJECT
411700940128     C************************************************************
411800940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
411900940223     C************************************************************
412000940223     C     DEFCAM        BEGSR
412100940223     C*
412200940223     C* klist
412300950717     C*
412400020429C1550X**** k4ivt1        klist
412500040513C1550C     k5ivt1        klist
412600980720     C                   kfld                    ivtsoc
412700020429C1550C                   kfld                    IVTTpLiq
412800980720     C                   kfld                    ivtanno
412900980720     C                   kfld                    ivtmese
413000980720     C                   kfld                    ivttiptot
413100090120C2106C     k6ivt1        klist
413200090120  "  C                   kfld                    ivtsoc
413300090120  "  C                   kfld                    IVTTpLiq
413400090120  "  C                   kfld                    ivtanno
413500090120  "  C                   kfld                    ivtmese
413600090120  "  C                   kfld                    ivttiptot
413700090120C2106C                   kfld                    ivtcdatt
413800950717     C*
413900020430C1550X***  k3ivt1        klist
414000020430  "  X***                kfld                    ivtsoc
414100020430  "  X***                kfld                    ivtanno
414200020430C1550X***                kfld                    ivtmese
414300940207     C*
414400020423C1550 * NDRIV03L
414500020423  "  C     k5riv3        klist
414600020423  "  C                   kfld                    rivsoc
414700020423  "  C                   kfld                    rivTpLiq
414800020423  "  C                   kfld                    rivanno
414900020423  "  C                   kfld                    rivperliq
415000020423  "  C                   kfld                    rivtpreg
415100020423  "  C     k4riv3        klist
415200020423  "  C                   kfld                    rivsoc
415300020423  "  C                   kfld                    rivTpLiq
415400020423  "  C                   kfld                    rivanno
415500020423  "  C                   kfld                    rivperliq
415600020423  "  C     k2riv3        klist
415700020423  "  C                   kfld                    RIVSoc
415800020423  "  C                   kfld                    RIVTpLiq
415900020423C1550 *
416000020423C1550X**   k4riv3        klist
416100020423  "  X**                 kfld                    rivsoc
416200020423  "  X**                 kfld                    rivanno
416300020423  "  X**                 kfld                    rivperliq
416400020423  "  X**                 kfld                    rivtpreg
416500020423  "  X**
416600020423  "  X**   k3riv3        klist
416700020423  "  X**                 kfld                    rivsoc
416800020423  "  X**                 kfld                    rivanno
416900020423C1550X**                 kfld                    rivperliq
417000000720C1396X*
417100000720  "  X*****k2ope1        klist
417200000720  "  X*****              kfld                    opesocieta
417300000720C1396X*****              kfld                    opecausale
417400951113     C*
417500951113     C     k2plf1        klist
417600951113     C                   kfld                    plfsocieta
417700951113     C                   kfld                    plfamcomp
417800000717C1396C*
417900000717  "  C     k05alr01      KList
418000000717  "  C                   KFld                    ALRSocieta
418100000717  "  C                   KFld                    ALRTpReg
418200000717  "  C                   KFld                    ALRLibro
418300000717  "  C                   KFld                    ALRAliq
418400000717C1396C                   KFld                    ALRRifIva
418500001004C1425C*
418600001004  "  C     k03dli01      KList
418700001004  "  C                   KFld                    DLISocieta
418800001004  "  C                   KFld                    DLITpReg
418900001004C1425C                   KFld                    DLILibro
419000040520C1762 * NDIVG02L - Iva di gruppo
419100040520  "  C     K07IVG02      klist
419200040520  "  C                   kfld                    IVGKnraz
419300040520  "  C                   kfld                    IVGTipTotG
419400040520  "  C                   kfld                    IVGSocieta
419500040520  "  C                   kfld                    IVGRegimeS
419600040520  "  C                   kfld                    IVGTpLiqS
419700040520  "  C                   kfld                    IVGAnnoS
419800040520C1762C                   kfld                    IVGMeseS
419900090120C2106 * ANAIV01L
420000090120  "  C     K2AIV1        KLIST
420100090120  "  C                   KFLD                    XSCSOC
420200090120C2106C                   KFLD                    D1CDATT
420300090120C2106C*
420400090120  "  C     K3AIL2        KLIST
420500090120  "  C                   KFLD                    DLISocieta
420600090120  "  C                   KFLD                    DLITpReg
420700090120C2106C                   KFLD                    DLILibro
420800940223     C*
420900940223     C* Variabili per gestione videate
421000940223     C*
421100940223     C                   MOVE      *BLANK        $GEST             2
421200940223     C                   MOVE      *BLANK        $FINE             1
421300940223     C                   MOVE      *BLANK        $INZD1            1
421400950719     C                   MOVE      *BLANK        $INZD2            1
421500991230     C                   MOVE      *BLANK        $INZD4            1
421600020423C1550C                   MOVE      *BLANK        $INZD9            1
421700940223     C                   MOVE      *BLANK        $LASTG            2
421800940223     C                   MOVE      *BLANK        $LASTV            2
421900940223     C*
422000940223     C* Variabili appoggio
422100940223     C*
422200940223     C                   Z-ADD     0             CURR              2 0
422300940223     C                   Z-ADD     0             CURC              2 0
422400940223     C                   MOVE      *ZEROS        WIN              99
422500000717C1396X*
422600000718  "  X******like         define    ivttotale     totacq
422700000720  "  X******like         define    ivttotale     totacqint
422800000718  "  X******like         define    ivttotale     totvendite
422900000717  "  X*
423000000718  "  X******like         define    d2salperpr    salperpr
423100000718  "  X******like         define    d2salintpr    salintpr
423200000718  "  X******like         define    d2salperio    salperio
423300000718  "  X******like         define    d2salnuovo    salnuovo
423400000718  "  X******like         define    d2salintra    salintra
423500000717  "  X*
423600000718  "  X******like         define    d2salperpr    savperpr
423700000718C1396X******like         define    d2salintpr    savintpr
423800940223     C*
423900940223     C* Indici
424000940223     C*
424100940223     C                   Z-ADD     0             X                 3 0
424200940223     C                   Z-ADD     0             Y                 3 0
424300940127     C*
424400940117     C                   ENDSR
424500940509     C/EJECT
424600940509     C************************************************************
424700940509     C* RIEMPIMENTO TASTI FUNZIONE
424800940509     C************************************************************
424900940509     C     RIEKEY        BEGSR
425000940509     C*
425100940509     C* Input :
425200940509     C* - $FX = schiera con tutti i testi disponibili
425300940509     C* - $FC = schiere con i flag di validità associati ai testi
425400940509     C* - $FR = schiere con i flag di REVERSE IMAGE
425500940509     C* - $ULK = ultimo testo caricato
425600940722     C* - $L01 = Lunghezza prima riga
425700940722     C* - $L02 = Lunghezza seconda riga
425800940509     C* Output :
425900940509     C* - $ULK
426000940609     C* - $KEY1= 1° riga di tasti funzione
426100940609     C* - $KEY2= 2° riga di tasti funzione
426200940509     C*
426300950522     C                   CALLB     'XKEYMSG'
426400940509     C* input
426500940509     C                   PARM                    $FX
426600940509     C                   PARM                    $FC
426700940509     C                   PARM                    $FR
426800940509     C                   PARM                    $ULK              3 0
426900940509     C* output
427000940509     C                   PARM                    $KEY1            79
427100940509     C                   PARM                    $KEY2            79
427200940722    >C* input
427300950530    >C                   PARM                    $L01              3 0
427400950530    >C                   PARM                    $L02              3 0
427500940509     C*
427600060224C1913C                   PARM                    $ALLFUNCT       480
427700060224C1913C*
427800940509     C                   ENDSR
427900000821C1396C************************************************************
428000000821  "  C* Controllo causale
428100000821  "  C************************************************************
428200000821C1396C     CallMVC003    BEGSR
428300000821     C*
428400000821     C                   Clear                   ANOPEDS
428500000821     C                   Eval      LenOut = %Size(ANOPEDS)
428600000821     C                   CallB     'NDMVC003'
428700000821     C                   PARM                    XScSoc
428800000821     C                   PARM                    Causale
428900000821     C                   PARM                    *Omit
429000000821     C                   PARM                    Uso
429100000821     C                   PARM      *Off          GesMsg
429200000821     C                   PARM      *Off          Esito
429300000821     C                   PARM      'ANOPE'       StrutturaO
429400000821     C     ANOPEDS       PARM                    OutPut
429500000821     C                   PARM                    LenOut
429600000821     C                   PARM      *Off          Refresh
429700000821     C*
429800000821C1396C                   ENDSR
429900090120C2106C************************************************************
430000090120  "  C* Reperisce dati da ANAIL02L
430100090120  "  C************************************************************
430200090608  "  C     Repanail      BEGSR
430300090120  "  C*
430400090120  "  C     K3ail2        CHAIN     ANAIL02L
430500090120  "  C                   if        not %found(Anail02l)
430600090120  "  C                   EVAL      AILCDATT = *BLANK
430700090120  "  C                   ENDIF
430800090120  "  C*
430900090120C2106C                   ENDSR
431000951113**  Costante x XMSGR
431100981214PROMSG    *LIBL     COS0165    PLAFOND inesistente per il periodo
431200951221PROMSG    *LIBL     COS0166    procedere ugualmente alla stampa ?
431300951221PROMSG    *LIBL     COS0170    procedere al calcolo del plafond
431400981214PROMSG    *LIBL     COS0439    RIEPILOGO inesistente per il periodo
431500940509** TASTI DI FUNZIONE  D1
431600951002PROMSG    *LIBL     KEY0001  01
431700950726PROMSG    *LIBL     KEY0002  02
431800940509PROMSG    *LIBL     KEY0003  03
431900951002PROMSG    *LIBL     KEY0004  04
432000951002PROMSG    *LIBL     KEY0005  05
432100951002PROMSG    *LIBL     KEY0006  06
432200951002PROMSG    *LIBL     KEY0007  07
432300951002PROMSG    *LIBL     KEY0008  08
432400951002PROMSG    *LIBL     KEY0009  09
432500951002PROMSG    *LIBL     KEY0010  10
432600951002PROMSG    *LIBL     KEY0011  11
432700951002PROMSG    *LIBL     KEY0012  12
432800951002PROMSG    *LIBL     KEY0013  13
432900951002PROMSG    *LIBL     KEY0014  14
433000951002PROMSG    *LIBL     KEY0015  15
433100951002PROMSG    *LIBL     KEY0016  16
433200951002PROMSG    *LIBL     KEY0017  17
433300951002PROMSG    *LIBL     KEY0018  18
433400951002PROMSG    *LIBL     KEY0019  19
433500951002PROMSG    *LIBL     KEY0020  20
433600951002PROMSG    *LIBL     KEY0021  21
433700951002PROMSG    *LIBL     KEY0022  22
433800951002PROMSG    *LIBL     KEY0023  23
433900951002PROMSG    *LIBL     KEY0024  24
434000950717**  ABILITAZIONE D1
4341009407200N  01
4342000007180N  02
4343009407201N  03
4344000204291N  04                                                                                 ### C1550 ###
4345009507210N  05
4346009507170N  06
4347009407200N  07
4348009505300N  08
4349009407200N  09
4350009411160N  10
4351009407200N  11
4352009511291N  12
4353009407200N  13
4354009407200N  14
4355009407200N  15
4356009407200N  16
4357009407200N  17
4358009507170N  18
4359009407200N  19
4360000304140N  20
4361000304141N  21
4362009407200N  22
4363009412140N  23   Lasciare sempre impostato a '0' il valore della riga 23
4364009412140N  24   Lasciare sempre impostata a '0' il valore della riga 24
436500950717** TASTI DI FUNZIONE  D2
436600951002PROMSG    *LIBL     KEY0001  01
436700951002PROMSG    *LIBL     KEY0002  02
436800951002PROMSG    *LIBL     KEY0003  03
436900951002PROMSG    *LIBL     KEY0004  04
437000951002PROMSG    *LIBL     KEY0005  05
437100951002PROMSG    *LIBL     KEY0006  06
437200951002PROMSG    *LIBL     KEY0007  07
437300951002PROMSG    *LIBL     KEY0008  08
437400951002PROMSG    *LIBL     KEY0009  09
437500951002PROMSG    *LIBL     KEY0010  10
437600951002PROMSG    *LIBL     KEY0011  11
437700951002PROMSG    *LIBL     KEY0012  12
437800951002PROMSG    *LIBL     KEY0013  13
437900030623PROMSG    *LIBL     KEY0238  14
438000951002PROMSG    *LIBL     KEY0015  15
438100951002PROMSG    *LIBL     KEY0016  16
438200951002PROMSG    *LIBL     KEY0017  17
438300951002PROMSG    *LIBL     KEY0018  18
438400040723PROMSG    *LIBL     KEY0279  19                                                       ###  C1781 ###
438500951002PROMSG    *LIBL     KEY0020  20
438600951002PROMSG    *LIBL     KEY0021  21
438700951002PROMSG    *LIBL     KEY0022  22
438800951002PROMSG    *LIBL     KEY0023  23
438900951002PROMSG    *LIBL     KEY0024  24
439000950717**  ABILITAZIONE D2
4391009505300N  01
4392009505300N  02
4393009505301N  03
4394000012291N  04
4395009507210N  05
4396009507171N  06
4397009505300N  07
4398009505300N  08
4399009505300N  09
4400009505300N  10
4401009505300N  11
4402009505301N  12
4403009505300N  13
4404000306231N  14
4405009505300N  15
4406009505300N  16
4407009505300N  17
4408009912301N  18
4409000407231N  19                                                                                ###  C1781 ###
4410009505300N  20
4411000407270N  21
4412009505300N  22
4413009505300N  23   Lasciare sempre impostato a '0' il valore della riga 23
4414009505300N  24   Lasciare sempre impostata a '0' il valore della riga 24
441500991230** TASTI DI FUNZIONE  D4
441600991230PROMSG    *LIBL     KEY0001  01
441700991230PROMSG    *LIBL     KEY0002  02
441800991230PROMSG    *LIBL     KEY0003  03
441900991230PROMSG    *LIBL     KEY0004  04
442000991230PROMSG    *LIBL     KEY0005  05
442100991230PROMSG    *LIBL     KEY0006  06
442200991230PROMSG    *LIBL     KEY0007  07
442300991230PROMSG    *LIBL     KEY0008  08
442400991230PROMSG    *LIBL     KEY0009  09
442500991230PROMSG    *LIBL     KEY0010  10
442600991230PROMSG    *LIBL     KEY0011  11
442700991230PROMSG    *LIBL     KEY0012  12
442800991230PROMSG    *LIBL     KEY0013  13
442900991230PROMSG    *LIBL     KEY0014  14
443000991230PROMSG    *LIBL     KEY0015  15
443100991230PROMSG    *LIBL     KEY0016  16
443200991230PROMSG    *LIBL     KEY0017  17
443300991230PROMSG    *LIBL     KEY0018  18
443400991230PROMSG    *LIBL     KEY0019  19
443500991230PROMSG    *LIBL     KEY0020  20
443600991230PROMSG    *LIBL     KEY0021  21
443700991230PROMSG    *LIBL     KEY0022  22
443800991230PROMSG    *LIBL     KEY0023  23
443900991230PROMSG    *LIBL     KEY0024  24
444000991230**  ABILITAZIONE D4
4441009912300N  01
4442009912300N  02
4443009912301N  03
4444009912300N  04
4445009912300N  05
4446009912300N  06
4447009912300N  07
4448009912300N  08
4449009912300N  09
4450009912300N  10
4451009912300N  11
4452009912301N  12
4453009912300N  13
4454009912300N  14
4455009912300N  15
4456009912300N  16
4457009912300N  17
4458009912300N  18
4459009912300N  19
4460009912300N  20
4461009912300N  21
4462009912300N  22
4463009912300N  23   Impostare a '1' se si usano le opzioni
4464009912301N  24   Lasciare sempre impostata a '1' il valore della riga 24
446500020423** TASTI DI FUNZIONE  D9                                                              ###  C1550 ###
446600020423PROMSG    *LIBL     KEY0001  01                                                       ###  C1550 ###
446700020423PROMSG    *LIBL     KEY0002  02                                                       ###  C1550 ###
446800020423PROMSG    *LIBL     KEY0003  03                                                       ###  C1550 ###
446900020423PROMSG    *LIBL     KEY0004  04                                                       ###  C1550 ###
447000020423PROMSG    *LIBL     KEY0005  05                                                       ###  C1550 ###
447100020423PROMSG    *LIBL     KEY0006  06                                                       ###  C1550 ###
447200020423PROMSG    *LIBL     KEY0007  07                                                       ###  C1550 ###
447300020423PROMSG    *LIBL     KEY0008  08                                                       ###  C1550 ###
447400020423PROMSG    *LIBL     KEY0009  09                                                       ###  C1550 ###
447500020423PROMSG    *LIBL     KEY0010  10                                                       ###  C1550 ###
447600020423PROMSG    *LIBL     KEY0011  11                                                       ###  C1550 ###
447700020423PROMSG    *LIBL     KEY0012  12                                                       ###  C1550 ###
447800020423PROMSG    *LIBL     KEY0013  13                                                       ###  C1550 ###
447900020423PROMSG    *LIBL     KEY0014  14                                                       ###  C1550 ###
448000020423PROMSG    *LIBL     KEY0015  15                                                       ###  C1550 ###
448100020423PROMSG    *LIBL     KEY0016  16                                                       ###  C1550 ###
448200020423PROMSG    *LIBL     KEY0017  17                                                       ###  C1550 ###
448300020423PROMSG    *LIBL     KEY0018  18                                                       ###  C1550 ###
448400020423PROMSG    *LIBL     KEY0019  19                                                       ###  C1550 ###
448500020423PROMSG    *LIBL     KEY0020  20                                                       ###  C1550 ###
448600020423PROMSG    *LIBL     KEY0021  21                                                       ###  C1550 ###
448700020423PROMSG    *LIBL     KEY0022  22                                                       ###  C1550 ###
448800020423PROMSG    *LIBL     KEY0023  23                                                       ###  C1550 ###
448900020423PROMSG    *LIBL     KEY0024  24                                                       ###  C1550 ###
449000020423**  ABILITAZIONE D9                                                                   ###  C1550 ###
4491000204230N  01                                                                                ###  C1550 ###
4492000204230N  02                                                                                ###  C1550 ###
4493000204231N  03                                                                                ###  C1550 ###
4494000204291N  04                                                                                ###  C1550 ###
4495000204230N  05                                                                                ###  C1550 ###
4496000204230N  06                                                                                ###  C1550 ###
4497000204230N  07                                                                                ###  C1550 ###
4498000204230N  08                                                                                ###  C1550 ###
4499000204230N  09                                                                                ###  C1550 ###
4500000204230N  10                                                                                ###  C1550 ###
4501000204230N  11                                                                                ###  C1550 ###
4502000204231N  12                                                                                ###  C1550 ###
4503000204230N  13                                                                                ###  C1550 ###
4504000204230N  14                                                                                ###  C1550 ###
4505000204230N  15                                                                                ###  C1550 ###
4506000204230N  16                                                                                ###  C1550 ###
4507000204230N  17                                                                                ###  C1550 ###
4508000204230N  18                                                                                ###  C1550 ###
4509000204230N  19                                                                                ###  C1550 ###
4510000204230N  20                                                                                ###  C1550 ###
4511000304140N  21                                                                                ###  C1550 ###
4512000204230N  22                                                                                ###  C1550 ###
4513000204230N  23   Impostare a '1' se si usano le opzioni                                       ###  C1550 ###
4514000204231N  24   Lasciare sempre impostata a '1' il valore della riga 24                      ###  C1550 ###
