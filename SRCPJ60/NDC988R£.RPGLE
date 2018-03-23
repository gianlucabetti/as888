000100140530     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970520     H*PARMS CVTOPT(*NONE)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400941115      *
000500090310    > *                   Gestione impegni
000600061115      *           -------------------------------------
000700940211     F*------------
000800090316     FNDImp01L  UF A E           K DISK
000900090316     FNdPpa01L  IF   E           K DISK
001000140313     FNdPsc01L  IF   E           K DISK
001100140313     F******NdPsc01L  UF A E           K DISK
001200090316     FNdScd01L  IF   E           K DISK
001300131104BRT  FNdRca01L  IF A E           K DISK
001400131209C2226F                                     INFDS(INF001)
001500131104c2156F***NdRca04L  UF A E           K DISK
001600131104  "  FNdRca05L  UF   E           K DISK
001700131104c2156F                                     RENAME(NDRCA000:NDRCA05)
001800090310    >FNDC988D   CF   E             WORKSTN usropn
001900090310    >F                                     RENAME(C988D1:FMTD1)
002000090310    >F                                     RENAME(C988D2:FMTD2)
002100090310    >F                                     RENAME(C988D3:FMTD3)
002200090310    >F                                     RENAME(C988S1:FMTS1)
002300090310    >F                                     RENAME(C988C1:FMTC1)
002400090310    >F                                     RENAME(C988T1:FMTT1)
002500090310    >F                                     RENAME(C988Z1:FMTZ1)
002600090310    >F                                     RENAME(C988Z2:FMTZ2)
002700090310    >F                                     RENAME(C988Z3:FMTZ3)
002800100423C2156F                                     RENAME(C988D4:FMTD4)
002900100423C2156F                                     RENAME(C988Z4:FMTZ4)
003000940131     F                                     SFILE(FMTS1:S1NRR)
003100940201     F                                     INFDS(DSFMT)
003200940128     D*----------------------------------------------------*
003300940302     D* Schiere per la gestione delle opzioni
003400940302     D*
003500960117     D* dimensione della schiera $MS1, da non cambiare
003600960117     D $DIMOP          C                   CONST(30)
003700950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003800950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
003900950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004000950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
004100960117     D* schiera contenente i nomi dei programma richiamati attraverso
004200960117     D* le opzioni personalizzate
004300960117     D $PGO1           S             10    DIM($dimop)
004400941214     D $TX             S             50    DIM(50)
004500941214     D $FL             S              1    DIM(50)
004600940909     D* Schiere generali per la gestione dei tasti di funzione
004700940506     D $FM             S             27    DIM(24)
004800940506     D $FX             S             50    DIM(24)
004900940506     D $FC             S              1    DIM(24)
005000940506     D $FR             S              1    DIM(24)
005100940909     D* Schiere per la gestione dei tasti di funzione
005200940506     D* Video D1
005300940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005400940506     D $FX1            S             50    DIM(24) ALT($FM1)
005500940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005600940506     D $FR1            S              1    DIM(24) ALT($FC1)
005700960202     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005800960202     D* funzione personalizzata S1
005900960202     D $PGF2           S             10    DIM(25)
006000940506     D* Schiere per la gestione dei tasti di funzione
006100940506     D* Video S1
006200940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006300940506     D $FX2            S             50    DIM(24) ALT($FM2)
006400940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006500940506     D $FR2            S              1    DIM(24) ALT($FC2)
006600940916     D* Schiere per la gestione dei tasti di funzione
006700940916     D* Video D2
006800940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006900940916     D $FX3            S             50    DIM(24) ALT($FM3)
007000940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007100940916     D $FR3            S              1    DIM(24) ALT($FC3)
007200100423c2156D* Schiere per la gestione dei tasti di funzione
007300100423  "  D* Video D4
007400100423  "  D $FM4            S             27    DIM(24) CTDATA PERRCD(1)
007500100423  "  D $FX4            S             50    DIM(24) ALT($FM4)
007600100423  "  D $FC4            S              1    DIM(24) CTDATA PERRCD(1)
007700100423c2156D $FR4            S              1    DIM(24) ALT($FC4)
007800950726     D*
007900950726     D $D1L01          S              3  0
008000950726     D $D1L02          S              3  0
008100950726     D $S1L01          S              3  0
008200950726     D $S1L02          S              3  0
008300950906     D $C1Op1          S              3  0
008400950906     D $C1Op2          S              3  0
008500950906     D $D2L01          S              3  0
008600950906     D $D2L02          S              3  0
008700940302     D*
008800960202     D*-------------
008900960202     D* Indice della schiera tasti funzione
009000960202     D PosFun          S              2  0
009100940211     D/EJECT
009200940211     D*-------------
009300940207     D* Valori assunti
009400950524     D XMPEDS        E DS                  INZ
009500950524     D XMPEDS1       E DS                  INZ
009600941111     D DSMPE           DS                  INZ
009700940207     D  $LOP                   1      1  0
009800940831     D  $ORD                   2      3  0
009900940207     D*-------------
010000940211     D* Passaggio Parametri
010100940211     D KPJBA         E DS
010200061115     D*-------------
010300941209     D* Dati di ambiente ottenuti da XSOC
010400061108     D SOC001        E DS                  EXTNAME(XSOC001DS)
010500061115     D    FgO1                        1    overlay(XscFgO)
010600941209     D*-------------
010700941209     D* DS Interna per dati di output di XSOC
010800941209     D XSOCDS          DS          1000
010900940325     D*-------------
011000940325     D* Parametri in ricezione
011100090310    >D NDC988DS      E DS
011200100517C2156D  dtpar988x    E                     EXTFLD(dtpar988)                     *
011300100517  "  D  nrpar988x    E                     EXTFLD(nrpar988)                     *
011400100517C2156D  serpar988x   E                     EXTFLD(serpar988)                    *
011500061113     D*-------------
011600061115     D* Parametri in ricezione ds multipla
011700090312    >D NDC988DS1     E DS                  occurs(1000) inz
011800061115     D*-------------
011900090310     D* Richiamo a XATB
012000090310     D XATBDS        E DS                  INZ
012100090312     D XTABDS        E DS                  INZ
012200090310     D*-------------
012300090310     D* Tabella ESITO
012400090312     D ANG042DS      E DS                  inz
012500090310     D*-------------
012600061115     D* DS Esterna gestione messaggi                                            *
012700061115     D XMSGDS        E DS                  inz                                  *
012800061115     D  STS          E                     EXTFLD(MSGSTS)                       *
012900061115     D                                     DIM(333)                             *
013000061115     D  JBA          E                     EXTFLD(MSGJBA)                       *
013100061115     D                                     DIM(502)                             *
013200061115     D  MSGRSP       E                     INZ('N')
013300061115     D  MSGEMV       E                     INZ('S')
013400061115     D  MSGMSG       E                     INZ(0)
013500061115     D  MSGRGP       E                     INZ(10)
013600061115     D  MSGCNL       E                     INZ('N')
013700061115     D  MSGVID       E                     INZ('N')
013800061115     D  MSGOPE       E                     INZ('N')
013900061115     D  MSGSTP       E                     INZ('N')
014000061115     D  MSGLCK       E                     INZ('N')
014100061113     D*---------------
014200960202     D* campo di salvataggio per la DS d'ingresso durante la chiamata
014300960202     D* alla funzione personalizzata
014400090310    >D $SaveDS         S                   like(NDC988DS)
014500131111C2226D*-------------
014600131111  "  D* F13=selezione multipla per ripetere i dati inseriti
014700131111  "  D  SavImpImp      s                   Like(s1ImpImp)
014800131111  "  D  SavEsito       s                   Like(s1Esito)
014900131111  "  D  SavDtImp       s                   Like(s1DtImp)
015000131111  "  D  SavNota        s                   Like(s1Nota)
015100131111C2226D  Wc1RCD         s                   Like(C1RCD)  inz
015200960202     D*-------------
015300940211     D DSFMT           DS           512
015400940506     D  $TASTO               369    369
015500940211     D  NRG                  370    370
015600940211     D  NCL                  371    371
015700940211     D  SFLNRR               378    379B 0
015800940211     D*-------------
015900940211     D* posizione cursore
016000940211     D CURSOR          DS
016100940211     D  RIRI                   1      2B 0 INZ
016200940211     D  R$$                    2      2
016300940211     D  COCO                   3      4B 0 INZ
016400940211     D  C$$                    4      4
016500940127     D*-------------
016600940127     D* Reperimento nome PGM
016700940127     D STATUS         SDS           333
016800940127     D  DSPGM            *PROC
016900950411     D  PARMS            *PARMS
017000090316     D  SDSUSR               254    263
017100940207     D*-------------
017200940616     D* Costanti
017300940616     D* nr.di righe del sfl (SFLPAG)
017400090310    >D SFLPAG          C                   CONST(15)
017500100423c2156D* ds nota
017600100423  "  D d4nota          DS           400
017700100423  "  D d4nota1                 1     80
017800100423  "  D d4nota2                81    160
017900100423  "  D d4nota3               161    240
018000100423  "  D d4nota4               241    320
018100100423c2156D d4nota5               321    400
018200940616     D*-------------
018300940506     D* Tasti di funzione
018400940506     D F01             C                   CONST(X'31')
018500940506     D F02             C                   CONST(X'32')
018600940506     D F03             C                   CONST(X'33')
018700940506     D F04             C                   CONST(X'34')
018800940506     D F05             C                   CONST(X'35')
018900940506     D F06             C                   CONST(X'36')
019000940506     D F07             C                   CONST(X'37')
019100940506     D F08             C                   CONST(X'38')
019200940506     D F09             C                   CONST(X'39')
019300940506     D F10             C                   CONST(X'3A')
019400940506     D F11             C                   CONST(X'3B')
019500940506     D F12             C                   CONST(X'3C')
019600940506     D F13             C                   CONST(X'B1')
019700940506     D F14             C                   CONST(X'B2')
019800940506     D F15             C                   CONST(X'B3')
019900940506     D F16             C                   CONST(X'B4')
020000940506     D F17             C                   CONST(X'B5')
020100940506     D F18             C                   CONST(X'B6')
020200940506     D F19             C                   CONST(X'B7')
020300940506     D F20             C                   CONST(X'B8')
020400940506     D F21             C                   CONST(X'B9')
020500940506     D F22             C                   CONST(X'BA')
020600940506     D F23             C                   CONST(X'BB')
020700940506     D F24             C                   CONST(X'BC')
020800940506     D ENTER           C                   CONST(X'F1')
020900940506     D ROLDWN          C                   CONST(X'F4')
021000940506     D ROLLUP          C                   CONST(X'F5')
021100961017     D*-------------
021200021010     D* Attributi video
021300021010     D Normale         C                   Const(X'32')
021400021010     D Protetto        C                   Const(X'A0')
021500021010     D*-------------
021600950524     D* Sottotitolo di testata
021700070928     D SOTTIT          C                   CONST(' ')
021800960109     D*-------------
021900950606     D* Variabili per gestione videate
022000950606     D $Gest           S              2A
022100950606     D $Fine           S              1A
022200950606     D $InzD1          S              1A
022300950606     D $InzS1          S              1A
022400950606     D $InzD2          S              1A
022500950606     D $LastG          S              2A
022600950606     D $LastV          S              2A
022700950606     D $EndFile        S              1A
022800950606     D $Esci           S              1A
022900950606     D $RcdOK          S              1A
023000950606     D $Unique         S              1A
023100090511     D $RigheVuote     S              3  0
023200140313     D $F10mult        S              1A
023300950606     D*-------------
023400950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
023500950606     D S1Nrr           S                   like(C1Rcd)
023600950606     D WSfl            S                   like(C1Nrr)
023700950606     D WMax            S                   like(C1Rcd)
023800950606     D WPag            S                   like(C1Rcd)
023900950606     D WInzS1          S                   like($InzS1)
024000090310    >D WOpr            S                   like(Opr988)
024100090310     D CurR            S              3S 0
024200090310     D CurC            S              3S 0
024300950606     D Win             S             99A
024400950606     D W1Pos           S                   like(C1Pos)
024500950606     D WINKR           S              1A
024600970912     D WExf            S              1A
024700061108     D ZDateiso        S               D
024800140311     D WrkData         S               D
024900950606     D*-------------
025000950606     D*  Indici
025100950606     D X               S              3S 0
025200061117     D Y               S              5S 0
025300960117     D* Indice della schiera per le opzioni personalizzate
025400960117     D PosOpz          S              2  0
025500960117     D* numero max opzioni
025600960117     D NumOpz          S              2  0 Inz($DIMOP)
025700960117     D* numero effettive opzioni
025800960117     D OpzLen          S              2  0
025900950606     D*-------------
026000061115     D* Campi per tubero
026100061115     D DataRif         S               D
026200061115     D DataIso         S               D
026300061115     D Esito           S              1
026400061115     D GesMsg          S              1
026500061115     D Output          S           4000
026600061115     D LenOut          S              9B 0
026700061115     D Struttura       S             10
026800090316     D*----------
026900090312     D*  ultimo progressivo righe fittizie (output)
027000090312     D PrgFittizie     S              9S 0
027100090312     D*  totale scaduto (output)
027200090312     D TotScaduto      S             19s 3
027300090312     D*  totale a scadere (output)
027400090312     D TotAScad        S             19s 3
027500090317     D*  totale impegni
027600090317     D TotImpegni      S             19s 3
027700090316     D*----------
027800070927     D* per riga unica tasti
027900070927     D $A              s             79
028000070927     D $B              s             79
028100090312     D*-------------
028200090312     D s1nota          DS           512
028300090312     D  s1nota1                1     30
028400090312     D  s1nota2               31    160
028500090312     D  s1nota3              161    290
028600090312     D*-------------
028700090312     D* DS esterna per dati clienti
028800090312     D DVACLI        E DS                  INZ
028900090312     D*-------------
029000090312     D* Definizioni parametri driver anagrafici
029100090312     DDvaric           S              1
029200090312     DDvaSocieta       S              3
029300090312     DDvaUnita         S              8
029400090312     DDvaStrutt        S             10
029500090312     DDvaDtRif         S               D
029600090312     DDvaLenOut        S              9B 0
029700090312     DDvaSnatura       S              1
029800090312     DDvaCodice        S              8
029900090312     DDvaLineav        S              3
030000090312     DDvaFiliale       S              3  0
030100090312     DDvaSogg          S              8
030200090312     DDvaTpInd         S              2
030300090312     DDvaCdInd         S              3
030400090312     DDvaErrore        S              1
030500090312     DDvaOutput        S           4000
030600090312     D*-------------
030700090312     D* DS per controllo conto
030800090312     D ND002DS       E DS                  INZ
030900100426c2156D Categorie             131    250
031000090316     D*----------
031100100426c2156D* Schiera categorie da ND002DS
031200100426  "  D DsCateg         DS           120
031300100426c2156D SkCateg                        4    dim(30)
031400100426     D*-------------
031500090312     D* Parametri del PGM di reperimento informazioni recupero credito
031600090312     D NDCL99DS      E DS                  inz
031700090316     D*----------------------------------------------------*
031800090316     D* DS per richiesta dati a X20
031900090316     D X20ScadDS     E DS                  inz
032000090316     D* DS parametro esteso se richiesto x20parscd='S'
032100090316     D X20SCADXDS    E DS                  inz
032200090316     D*------------
032300090316     D $Aggiorna       S              1
032400100423c2156D savdtpar        S                   like(s1dtpar)
032500100423c2156D savnrpar        S                   like(s1nrpar)
032600100423c2156D savserpar       S                   like(h1serpar)
032700090316     D*----------
032800090316     D* DS gestione parametri modulo
032900090316     D XPAOUT          DS          2000
033000090316     D anp057ds      E DS
033100090316     C*----------------------------------------------------*
033200061115     D* PROTOTIPAZIONE DELLE PROCEDURE
033300061115     D/COPY *LIBL/SRCCPY,PJX002PR
033400131209      *
033500131209C2226 * DS per UserExit
033600131209  "  D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
033700131209  "  D* DS Numero relativo NDRCA00F
033800131209  "  D Inf001          DS
033900131209C2226D  RRNRCA               397    400B 0
034000131209      *
034100940127     C*----------------------------------------------------*
034200940127     C*                MAIN LINE PROGRAM
034300940127     C*----------------------------------------------------*
034400940127     C*
034500940223     C* inizializzazione variabili
034600940223     C                   EXSR      INZVAR
034700940223     C*
034800940223     C     $FINE         DOWEQ     *OFF
034900940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
035000940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
035100940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
035200941027     C                   ENDCS
035300941027     C                   ENDDO
035400940325     C* fine programma
035500940325     C                   EXSR      ENDPGM
035600940225     C*
035700940325     C/EJECT
035800940325     C************************************************************
035900940325     C* FINE PROGRAMMA
036000940325     C************************************************************
036100940325     C     ENDPGM        BEGSR
036200940325     C*
036300940325     C* passaggio dati al pgm chiamante
036400090407     c                   movel     ndc988ds      kpjbu
036500940325     C*
036600940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
036700090310    >C     OPZ988        IFEQ      *BLANK
036800940325     C                   SETON                                            LR
036900940325     C* altrimenti chiudo in RT
037000940325     C                   ELSE
037100940325     C                   SETON                                            RT
037200940325     C                   ENDIF
037300940616     C                   RETURN
037400940325     C*
037500940325     C                   ENDSR
037600940224     C/EJECT
037700940201     C************************************************************
037800940201     C* RICERCHE
037900940201     C************************************************************
038000940127     C     SEARCH        BEGSR
038100940127     C*
038200940127     C* determino Riga/Colonna del cursore
038300940127     C                   MOVE      NRG           R$$
038400940127     C                   MOVE      NCL           C$$
038500940127     C                   Z-ADD     RIRI          CURR
038600940127     C                   Z-ADD     COCO          CURC
038700940127     C*
038800061114     c                   select
038900071003     c* stessa ricerca per tutti i campi per sfl S1
039000090312     c                   When      H1NmFl = 'S1ESITO'
039100081229     c     C1NRR         CHAIN     FMTS1                              21
039200090312     c*
039300090312     C                   RESET                   XTABDS
039400090312     C                   MOVE      '01'          XTAOPZ
039500090312     C                   MOVE      xscsoc        XTAAZI
039600090312     C                   MOVE      XSCLIN        XTALIN
039700090312     c                   Z-ADD     20            XTARIG
039800090312     C                   Z-ADD     CURC          XTACOL
039900090312     C                   CLEAR                   KPJBU
040000090312     C                   MOVEL     XTABDS        KPJBU
040100090312     C                   CALL      'ANG042R'
040200090312     C                   PARM                    KPJBA
040300090312     C                   MOVEL     KPJBU         XTABDS
040400090312     c*
040500090312     C     XTAret        IFNE      '2'
040600090312     C                   move      XTAKEY        S1ESITO
040700090312     c* decodifica esito
040800090312     C                   MOVE      '1'           XTBRIC
040900090312     C                   MOVE      XSCSOC        XTBAZI
041000090312     C                   MOVE      *ZERO         XTBKEY
041100090312     C                   MOVE      '042'         XTBCOD
041200090312     C                   MOVE      s1esito       XTBKEY
041300090312     C                   CALLb     'XATB'
041400090312     C                   PARM                    XATBDS
041500090312     C     XTBERR        IFEQ      *Off
041600090312     C                   MOVEL     XTBUNI        ang042ds
041700090312     C                   MOVEL     DEs042        s1esitoD
041800090312     c                   endif
041900090312     C                   ENDIF
042000090312     c*
042100090312     c                   move      *on           *in32
042200090312     c                   exsr      Atrs1
042300090312     c                   Update    fmts1
042400090312     c                   move      *off          *in32
042500100426     c*
042600100426c2156c                   When      H1NmFl = 'D4ESITO'
042700100426  "  c*
042800100426  "  C                   RESET                   XTABDS
042900100426  "  C                   MOVE      '01'          XTAOPZ
043000100426  "  C                   MOVE      xscsoc        XTAAZI
043100100426  "  C                   MOVE      XSCLIN        XTALIN
043200100426  "  c                   Z-ADD     20            XTARIG
043300100426  "  C                   Z-ADD     CURC          XTACOL
043400100426  "  C                   CLEAR                   KPJBU
043500100426  "  C                   MOVEL     XTABDS        KPJBU
043600100426  "  C                   CALL      'ANG042R'
043700100426  "  C                   PARM                    KPJBA
043800100426  "  C                   MOVEL     KPJBU         XTABDS
043900100426  "  c*
044000100426  "  C     XTAret        IFNE      '2'
044100100426  "  C                   move      XTAKEY        d4ESITO
044200100426  "  c* decodifica esito
044300100426  "  C                   MOVE      '1'           XTBRIC
044400100426  "  C                   MOVE      XSCSOC        XTBAZI
044500100426  "  C                   MOVE      *ZERO         XTBKEY
044600100426  "  C                   MOVE      '042'         XTBCOD
044700100426  "  C                   MOVE      d4esito       XTBKEY
044800100426  "  C                   CALLb     'XATB'
044900100426  "  C                   PARM                    XATBDS
045000100426  "  C     XTBERR        IFEQ      *Off
045100100426  "  C                   MOVEL     XTBUNI        ang042ds
045200100426  "  C                   MOVEL     DEs042        d4esitoD
045300100426  "  c                   endif
045400100426c2156C                   ENDIF
045500940223     C*
045600940223     C* ricerca non abilitata in questa posizione
045700941118   X1C                   OTHER
045800940929     C                   SETON                                        9899
045900940127     C*
046000941118   E1C                   ENDSL
046100950612     C*
046200950612     C* imposto pos. del cursore
046300061116     C                   MOVE      *HIVAL        $LASTV
046400061116     C                   Z-ADD     CURR          H1RIGA
046500061116     C                   Z-ADD     CURC          H1COLO
046600950612     C*
046700940127     C                   ENDSR
046800071003     C/EJECT
046900940224     C************************************************************
047000940131     C* RIEMPIMENTO OPZIONI
047100940224     C************************************************************
047200940131     C     RIEOPZ        BEGSR
047300940223     C*
047400940223     C* Input :
047500940223     C* - $TX = schiera con tutti i testi disponibili
047600940223     C* - $FL = schiere con i flag di validità associati ai testi
047700940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
047800940223     C*          opzione usato
047900940223     C* - WMXO = variabile contenente il numero massimo di testi
048000940223     C*          opzione usabili
048100940721     C* - $L01 = Lunghezza prima riga
048200940721     C* - $L02 = Lunghezza seconda riga
048300940223     C* Output :
048400940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
048500940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
048600941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
048700940223     C*
048800950522     C                   CALLB     'X04OPMSG'
048900940223     C* input
049000940223     C                   PARM                    $TX
049100940223     C                   PARM                    $FL
049200940225     C                   PARM                    WCOZ              3 0
049300940223     C                   PARM                    WMXO              3 0
049400940223     C* output
049500940223     C                   PARM                    WOPZ1            76
049600940223     C                   PARM                    WOPZ2            76
049700941214     C                   PARM                    FF23              1
049800950301     C* input
049900950726     C                   PARM                    $L01              3 0
050000950726     C                   PARM                    $L02              3 0
050100940223     C*
050200060224     C                   PARM                    $ALLOPT         960
050300060224     C*
050400940131     C                   ENDSR
050500940506     C/EJECT
050600940506     C************************************************************
050700940506     C* RIEMPIMENTO TASTI FUNZIONE
050800940506     C************************************************************
050900940506     C     RIEKEY        BEGSR
051000940506     C*
051100940506     C* Input :
051200940506     C* - $FX = schiera con tutti i testi disponibili
051300940506     C* - $FC = schiere con i flag di validità associati ai testi
051400940506     C* - $FR = schiere con i flag di REVERSE IMAGE
051500940506     C* - $ULK = ultimo testo caricato
051600940721     C* - $L01 = Lunghezza prima riga
051700940721     C* - $L02 = Lunghezza seconda riga
051800940506     C* Output :
051900940506     C* - $ULK
052000940609     C* - $KEY1= 1° riga di tasti funzione
052100940609     C* - $KEY2= 2° riga di tasti funzione
052200940506     C*
052300950522     C                   CALLB     'XKEYMSG'
052400940506     C* input
052500940506     C                   PARM                    $FX
052600940506     C                   PARM                    $FC
052700940506     C                   PARM                    $FR
052800940506     C                   PARM                    $ULK              3 0
052900940506     C* output
053000940506     C                   PARM                    $KEY1            79
053100940506     C                   PARM                    $KEY2            79
053200950301     C* input
053300061108     C                   PARM                    $L01              3 0
053400061108     C                   PARM                    $L02              3 0
053500940506     C*
053600060224     C                   PARM                    $ALLFUNCT       480
053700060224     C*
053800940506     C                   ENDSR
053900940224     C/EJECT
054000940224     C************************************************************
054100940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
054200940224     C************************************************************
054300940224     C     GESPIE        BEGSR
054400940224     C*
054500940224     C                   SELECT
054600940224     C* Parzializzazioni
054700940224     C     $GEST         WHENEQ    'D1'
054800940506     C     $ULKD1        IFEQ      0
054900940506     C* riempimento opzioni
055000940506     C                   EXSR      F24D1
055100940506     C                   ELSE
055200061108     C                   WRITE     FMTZ1
055300940506     C                   ENDIF
055400940506     C* Lista
055500940506     C     $GEST         WHENEQ    'S1'
055600940506     C     $ULKS1        IFEQ      0
055700940506     C                   EXSR      F24S1
055800940506     C                   ELSE
055900131111C2226c* totali
056000131111  "  c                   Z-ADD     TotScaduto    z2TotScad
056100131111  "  c                   Z-ADD     TotAscad      z2TotASca
056200131111C2226c                   Z-ADD     TotImpegni    z2TotImp
056300061108     C                   WRITE     FMTZ2
056400940506     C                   ENDIF
056500940916     C* Modifica valori assunti
056600940916     C     $GEST         WHENEQ    'D2'
056700940916     C     $ULKD2        IFEQ      0
056800940916     C                   EXSR      F24D2
056900940916     C                   ELSE
057000940916     C                   WRITE     FMTZ3
057100940916     C                   ENDIF
057200100423c2156C     $GEST         WHENEQ    'D4'
057300100423  "  C     $ULKD4        IFEQ      0
057400100423  "  C                   EXSR      F24D4
057500100423  "  C                   ELSE
057600100423  "  C                   WRITE     FMTZ4
057700100423c2156C                   ENDIF
057800940506     C                   ENDSL
057900940506     C*
058000940224     C                   ENDSR
058100940224     C/EJECT
058200940224     C************************************************************
058300940224     C* GESTIONE PARZIALIZZAZIONI
058400940224     C************************************************************
058500940224     C     GESD1         BEGSR
058600940224     C* inizializzazione videata
058700940223     C     $INZD1        IFEQ      *ON
058800940127     C                   EXSR      INZD1
058900940223     C                   MOVE      *OFF          $INZD1
059000140313     C                   MOVE      *OFF          $F10mult
059100941027     C                   ENDIF
059200940223     C* emissione piede videata se proveniente da altra
059300940223     C* salvataggio valore formato di provenienza
059400940202     C     $LASTV        IFNE      'D1'
059500940223     C* salvo stato F17 per sapere cosa fare in caso di F12
059600940223     C                   MOVE      *INKR         WINKR
059700140313     c                   clear                   T1tit
059800940202     C                   WRITE     FMTT1
059900940224     C                   EXSR      GESPIE
060000940202     C                   MOVE      $LASTV        $LASTG
060100940202     C                   MOVE      'D1'          $LASTV
060200940707     C                   ENDIF
060300940707     C*
060400940707     C* se esistono errori sulla videata
060500940707     C* emetto la write del formato a indicatori spenti per vedere
060600940707     C* le eventuali decodifiche
060700940707     C     *IN99         IFEQ      *ON
060800940707     C                   MOVEA     *IN           WIN
060900940707     C                   MOVE      *ALL'0'       IN5098           49
061000940707     C                   MOVEA     IN5098        *IN(50)
061100940707     C                   WRITE     FMTD1
061200940707     C                   MOVEA     WIN           *IN
061300940707     C                   ENDIF
061400940131     C*
061500940131     C                   EXFMT     FMTD1
061600960605     C*
061700960605     C* azzero la posizione del cursore
061800960605     C                   Z-ADD     0             H1RIGA
061900960605     C                   Z-ADD     0             H1COLO
062000940506     C*
062100951005     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
062200951005     C* INPUT
062300951005     C* $FC1 = Abilitazioni videata D1
062400951005     C* OUTPUT
062500951005     C* *IN97 + *IN99 Errori
062600951005     C                   MOVEA     $FC1          $FC
062700951005     C                   EXSR      CTRKEY
062800940506     C*
062900940506     C     *IN99         IFEQ      *OFF
063000940131     C*
063100940117     C                   SELECT
063200940916     C* F2=Decodifica
063300940916     C     $TASTO        WHENEQ    F02
063400940916     C                   EXSR      F02D1
063500940131     C* F3=Fine
063600940506     C     $TASTO        WHENEQ    F03
063700940307     C                   EXSR      F03D1
063800940131     C* F4=Ricerca
063900940506     C     $TASTO        WHENEQ    F04
064000940127     C                   EXSR      SEARCH
064100940916     C* F5=Refresh
064200940916     C     $TASTO        WHENEQ    F05
064300940916     C                   EXSR      F05D1
064400941206     C* F8=Successivo
064500941206     C     $TASTO        WHENEQ    F08
064600941206     C                   EXSR      F08D1
064700940131     C* F12=Ritorno
064800940506     C     $TASTO        WHENEQ    F12
064900940307     C                   EXSR      F12D1
065000940131     C* F18=Modifica valori assunti
065100940506     C     $TASTO        WHENEQ    F18
065200940307     C                   EXSR      F18D1
065300940224     C* F24=Altri tasti
065400940506     C     $TASTO        WHENEQ    F24
065500940309     C                   EXSR      F24D1
065600940131     C*
065700940117     C                   OTHER
065800940127     C                   EXSR      CTRD1
065900940127     C     *IN99         IFEQ      *OFF
066000940131     C* definisco l'ordinamento
066100950410     C                   EXSR      DEFORD
066200950410     C* richiamo la lista
066300940127     C                   MOVE      'S1'          $GEST
066400940223     C                   MOVE      *ON           $INZS1
066500940302     C                   MOVE      *BLANK        W1POS
066600940211     C                   ENDIF
066700940224     C*
066800940117     C                   ENDSL
066900940224     C*
067000940506     C                   ENDIF
067100950525     C*
067200940117     C                   ENDSR
067300940224     C/EJECT
067400940224     C************************************************************
067500940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
067600940224     C************************************************************
067700940127     C     INZD1         BEGSR
067800940318     C*
067900940318     C                   CLEAR                   FMTD1
068000941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
068100941013     C*    parzializzazioni
068200941104     C                   SELECT
068300941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
068400950606     C     $Unique       WHENEQ    *ON
068500941104     C* il pgm è richiamato e
068600941104     C* sono stati passati dei valori di parzializzazione ma non
068700941104     C* la chiave univoca
068800090310    >C     OPZ988        WHENNE    *BLANK
068900941104     C* il pgm non è richiamato
069000090310    >C     OPZ988        WHENEQ    *BLANK
069100061117     c* decodifiche
069200061117     c                   exsr      F02D1
069300941115     C                   ENDSL
069400940224     C*
069500940117     C                   ENDSR
069600940916     C/EJECT
069700940916     C************************************************************
069800940916     C* GESTIONE F02 VIDEO D1
069900940916     C************************************************************
070000940916     C     F02D1         BEGSR
070100940916     C*
070200940916     C* per decodificare la videata richiamo la routine dei ctrl
070300940916     C* e poi disattivo gli eventuali messaggi di errore
070400940916     C                   EXSR      CTRD1
070500940916     C*
070600940916     C                   MOVE      *ALL'0'       IN5098
070700940916     C                   MOVEA     IN5098        *IN(50)
070800940916     C*
070900940916     C                   ENDSR
071000940224     C/EJECT
071100940131     C************************************************************
071200940307     C* GESTIONE F03 VIDEO D1
071300940131     C************************************************************
071400940307     C     F03D1         BEGSR
071500940131     C*
071600940307     C                   MOVE      *ON           $FINE
071700090310    >C                   MOVE      '1'           RET988
071800940325     C* fine programma
071900940325     C                   EXSR      ENDPGM
072000940307     C*
072100940307     C                   ENDSR
072200940916     C/EJECT
072300940916     C************************************************************
072400940916     C* GESTIONE F05 VIDEO D1
072500940916     C************************************************************
072600940916     C     F05D1         BEGSR
072700940916     C*
072800940916     C                   MOVE      *ON           $INZD1
072900940916     C*
073000940916     C                   ENDSR
073100941206     C/EJECT
073200941206     C************************************************************
073300941206     C* GESTIONE F08 VIDEO D1
073400941206     C************************************************************
073500941206     C     F08D1         BEGSR
073600941206     C*
073700941206     C                   MOVE      *ON           $FINE
073800090310    >C                   MOVE      '0'           RET988
073900941206     C*
074000941206     C                   ENDSR
074100950301     C/EJECT
074200940307     C************************************************************
074300940307     C* GESTIONE F12 VIDEO D1
074400940307     C************************************************************
074500940307     C     F12D1         BEGSR
074600940307     C*
074700940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
074800940307     C*  per F12 torno alla lista
074900940307     C     $LOP          IFEQ      2
075000940307     C     WINKR         OREQ      *ON
075100940307     C                   MOVE      'S1'          $GEST
075200940307     C*  altrimenti, per F12 torno a menu
075300940307     C                   ELSE
075400940307     C                   MOVE      *ON           $FINE
075500090310    >C                   MOVE      '2'           RET988
075600941027     C                   ENDIF
075700940307     C*
075800940307     C                   ENDSR
075900940307     C/EJECT
076000940307     C************************************************************
076100940307     C* GESTIONE F18 VIDEO D1
076200940307     C************************************************************
076300940307     C     F18D1         BEGSR
076400940307     C*
076500940307     C                   MOVE      'D2'          $GEST
076600940307     C                   MOVEL     *ON           $INZD2
076700940307     C*
076800940307     C                   ENDSR
076900940307     C/EJECT
077000940307     C************************************************************
077100940307     C* GESTIONE F24 VIDEO D1
077200940609     C* input
077300940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
077400940509     C* $FX1 =Testi tasti funzione
077500940509     C* $FC1 = Abilitazioni
077600940509     C* $FR1 = Inversioni di fondo
077700940307     C************************************************************
077800940307     C     F24D1         BEGSR
077900940506     C*
078000940506     C                   MOVEA     $FX1          $FX
078100940506     C                   MOVEA     $FC1          $FC
078200940506     C                   MOVEA     $FR1          $FR
078300940506     C                   Z-ADD     $ULKD1        $ULK
078400950726     C                   Z-ADD     $D1L01        $L01
078500950726     C                   Z-ADD     $D1L02        $L02
078600940506     C                   EXSR      RIEKEY
078700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
078800940720     C                   MOVEA     $FC           $FC1
078900070927     C                   MOVEL     $KEY1         Z1KE
079000061108     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
079100061108     C                   MOVEL     $KEY2         Z1KE2
079200070927     c* riga unica tasti
079300070927     c                   eval      $A = %triml(z1ke)
079400070927     c                   eval      $B = %triml(z1ke2)
079500070927     c     $A            cat(P)    $B:0          z1ke1
079600061108     C                   WRITE     FMTZ1
079700940307     C*
079800940307     C                   ENDSR
079900940307     C/EJECT
080000940307     C************************************************************
080100940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
080200950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
080300960109     C* 93 94 96 97 98
080400940307     C************************************************************
080500940307     C     CTRD1         BEGSR
080600940307     C*
080700940127     C                   SETOFF                                       99
080800940117     C                   ENDSR
080900950410     C/EJECT
081000950410     C************************************************************
081100950410     C* DEFINIZIONE DELL'ORDINAMENTO
081200950410     C************************************************************
081300950410     C     DEFORD        BEGSR
081400950410     C*
081500950410     C* definisco l'ordinamento della lista a seconda dei valori
081600950410     C* impostati nel video di parzializzazione
081700950410     C*
081800950410     C                   Z-ADD     1             $ORD
081900061108     C*
082000950410     C                   ENDSR
082100960202     C/EJECT
082200960202     C************************************************************
082300960202     C* Chiamata programma con tasto personalizzato
082400960202     C************************************************************
082500960202     C     FPerS1        BEGSR
082600960202     C*
082700960202     C* salvo la DS di ingresso
082800090310    >C                   MOVEL     NDC988DS      $SaveDS
082900960202     C*
083000090310    >C                   MOVEL     NDC988DS      KPJBU
083100960202     C                   CALL      $PGF2(PosFun)                        22
083200960202     C                   PARM                    KPJBA
083300960202     C                   PARM                    NOMPGM
083400960202     C*
083500960202     C* ripristino la DS di ingresso
083600090310    >C                   MOVEL     $SaveDS       NDC988DS
083700960202     C*
083800960202     C                   ENDSR
083900940224     C/EJECT
084000940127     C************************************************************
084100940131     C* GESTIONE LISTA
084200940127     C************************************************************
084300940127     C     GESS1         BEGSR
084400940223     C* inizializzazione videata
084500940223     C     $INZS1        IFEQ      *ON
084600940127     C                   EXSR      INZS1
084700940223     C                   MOVE      *OFF          $INZS1
084800940127     C                   ENDIF
084900940223     C* emissione piede videata se proveniente da altra
085000940223     C* salvataggio valore formato di provenienza
085100940202     C     $LASTV        IFNE      'S1'
085200140313     c                   if        $f10mult = *on
085300140313     c                   eval      t1tit = '       --- IMMISSIONE ---'
085400140313     c                   else
085500140313     c                   clear                   T1tit
085600140313     c                   endif
085700940202     C                   WRITE     FMTT1
085800940224     C                   EXSR      GESPIE
085900940202     C                   MOVE      $LASTV        $LASTG
086000940202     C                   MOVE      'S1'          $LASTV
086100941027     C                   ENDIF
086200940223     C*
086300940223     C     WMAX          IFEQ      0
086400940211     C                   WRITE     FMTD3                                        NO RCD
086500940211     C                   ENDIF
086600960202     C* Inizializzazione indice tasti funzione personalizzati
086700960202     C                   Eval      PosFun = 25
086800140313     C*
086900140313     C                   MOVE      *OFF          $F10mult
087000940127     C*
087100061108     C                   Eval      *in09=c1mode
087200940131     C                   EXFMT     FMTC1
087300960605     C*
087400960605     C* azzero la posizione del cursore
087500960605     C                   Z-ADD     0             H1RIGA
087600960605     C                   Z-ADD     0             H1COLO
087700960605     C*
087800940204     C     C1NRR         IFNE      0
087900940204     C                   Z-ADD     C1NRR         WSFL
088000940204     C                   ENDIF
088100951020     C                   IF        SFLNRR > 0
088200951020     C                   Z-ADD     SFLNRR        C1RCD
088300951020     C                   ENDIF
088400960122     C* inizializzazione indice opzioni personalizzate
088500960122     C                   Eval      PosOpz = 0
088600951005     C* CONTROLLO ABILITAZIONE TASTI
088700951005     C* INPUT
088800951005     C* $FC2 = Abilitazioni videata S1
088900951005     C* OUTPUT
089000951005     C* *IN97 + *IN99 Errori
089100951005     C                   MOVEA     $FC2          $FC
089200951005     C                   EXSR      CTRKEY
089300941205     C*
089400940506     C     *IN99         IFEQ      *OFF
089500940127     C*
089600941027     C                   SELECT
089700940127     C* F3=Fine
089800940506     C     $TASTO        WHENEQ    F03
089900940309     C                   EXSR      F03S1
090000061113     C* F4=drill
090100061113     C     $TASTO        WHENEQ    F04
090200061113     C                   EXSR      Search
090300090406     C* F5=Ripristino
090400090406     C     $TASTO        WHENEQ    F05
090500090406     C                   EXSR      F05S1
090600941206     C* F8=Successivo
090700941206     C     $TASTO        WHENEQ    F08
090800941206     C                   EXSR      F08S1
090900100423C2156C* F10=Immissione altri impegni
091000100423  "  C     $TASTO        WHENEQ    F10
091100140313C2156C                   EXSR      F10S1
091200940131     C* F12=Ritorno
091300940506     C     $TASTO        WHENEQ    F12
091400940309     C                   EXSR      F12S1
091500131111C2226C* F13=Selezione multipla
091600131111  "  C     $TASTO        WHENEQ    F13
091700131111C2226C                   EXSR      F13S1
091800940131     C* F17=Parzializzazioni
091900940506     C     $TASTO        WHENEQ    F17
092000940309     C                   EXSR      F17S1
092100940131     C* F18=Modifica valori assunti
092200940506     C     $TASTO        WHENEQ    F18
092300940309     C                   EXSR      F18S1
092400940224     C* F23=Altre opzioni
092500940506     C     $TASTO        WHENEQ    F23
092600940309     C                   EXSR      F23S1
092700940224     C* F24=Altri tasti
092800940506     C     $TASTO        WHENEQ    F24
092900140311     C                   EXSR      F24S1
093000940127     C* ROLL-UP
093100090511     C     $TASTO        WHENEQ    ROLLUP
093200100423c2156x***                if        $righevuote <=2
093300100423  "  x***                EXSR      rollvuote
093400100423c2156x***                endif
093500960202     C* Personalizzata
093600960202     C     $PGF2(PosFun) Whenne    *BLANK
093700960202     C                   EXSR      FperS1
093800940127     C*
093900941027     C                   OTHER
094000940127     C* CONTROLLO DATI
094100940131     C                   EXSR      CTRC1
094200940201     C     *IN99         IFEQ      *OFF
094300940131     C                   EXSR      CTRS1
094400090318     C* ricalcola totale impegni
094500090318     C     *IN99         IFEQ      *OFF
094600090318     C                   EXSR      ClcTotImp
094700090318     c                   endif
094800090316     c* aggiorna
094900090316     C     *IN99         IFEQ      *OFF
095000090316     C     $TASTO        andeq     F06
095100090316     c                   Exsr      AggANA
095200090316     C                   EXSR      GESAGG
095300090316     c                   endif
095400090316     c*
095500941027     C                   ENDIF
095600941205     C*
095700941027     C                   ENDSL
095800940127     C*
095900940506     C                   ENDIF
096000941205     C*
096100940127     C                   ENDSR
096200940224     C/EJECT
096300090316     C************************************************************
096400090316     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
096500090316     C************************************************************
096600090316     C     GESAGG        BEGSR
096700090316     C*
096800090316     C* segnalo al pgm chiamante l'avvenuta conferma
096900090316    >C                   MOVE      *ON           OPR988
097000090316     C* segnalo al pgm chiamante l'aver premuto l'F6
097100090316    >C                   MOVE      '0'           RET988
097200090316     C*
097300090316     C                   MOVE      *ON           $FINE
097400090316     C*
097500090316     C                   ENDSR
097600090316     C/EJECT
097700090316     C************************************************************
097800090316     C* AGGIORNAMENTO ANAGRAFICA
097900090316     C************************************************************
098000090316     C     AGGANA        BEGSR
098100090316     c*
098200090316     C                   DO        *hival        S1NRR
098300090316     C     S1NRR         CHAIN     FMTS1                              21
098400090316     c   21              leave
098500090316     c*
098600140317     c                   if        h1impann = '1'
098700140317     c                   iter
098800140317     c                   endif
098900140317     c*
099000090316     C                   SELECT
099100090316     c* se la partita è valorizzata
099200100426c2156x****               WHEN      S1DtPar  <> 0
099300100426c2156c                   WHEN      h1DtPar  <> *loval
099400090316     C* aggiorno l'impegno per la partita NON fittizia ricevuta
099500090316     C                   EXSR      AggImpegno
099600090316     C* altrimenti
099700090316     C                   OTHER
099800090316     C* aggiorno l'impegno su partita fittizia
099900090316     C                   EXSR      AggImpegnoFitt
100000090316     C                   ENDSL
100100090316     C*
100200090316     C                   ENDDO
100300090316     C*
100400090316     C                   ENDSR
100500090316     C/EJECT
100600131111     C************************************************************
100700131111     C* Controllo data in aggiornamento solo se modificata
100800131111     C************************************************************
100900131111     C     CtrAggDtImp   BEGSR
101000131111     C*
101100131111     C                   EVAL      IMPSocieta = Societa988
101200131111     C                   EVAL      IMPKcc =   kcc988
101300131111     C                   EVAL      IMPKsc =   ksc988
101400131111     C                   EVAL      IMPDtPar =  h1dtpar
101500131111     C                   EVAL      IMPNrPar =  h1nrpar
101600131111     C                   EVAL      IMPSerPar = h1serpar
101700131111     C                   EVAL      IMPDtSca =  h1dtsca
101800131111     c                   eval      ImpProgr =  H1progr
101900131111     C*
102000131111     C     K08IMP01      CHAIN     NDIMP01L                           21
102100131111     C*
102200131111     C                   IF        %found(ndimp01l)
102300131111     c                   if        impDtImp <> %date(S1DtImp:*jobrun)
102400131111     c                             and %date(S1DtImp:*jobrun)< zdateiso
102500131111     c                   seton                                        5199
102600131111     c                   endif
102700131111     C                   ENDIF
102800131111     C*
102900131111     C                   ENDSR
103000090316     C************************************************************
103100090316     C* Aggiorno l'impegno ricevuto
103200090316     C************************************************************
103300090316     C     AggImpegno    BEGSR
103400090316     C*
103500090316     C                   EVAL      IMPSocieta = Societa988
103600090316     C                   EVAL      IMPKcc =   kcc988
103700090316     C                   EVAL      IMPKsc =   ksc988
103800090316     C                   EVAL      IMPDtPar =  h1dtpar
103900090316     C                   EVAL      IMPNrPar =  h1nrpar
104000090316     C                   EVAL      IMPSerPar = h1serpar
104100090316     C                   EVAL      IMPDtSca =  h1dtsca
104200100423c2156c                   eval      ImpProgr =  H1progr
104300090316     C*
104400100423c2156x**** K07IMP01      CHAIN     NDIMP01L                           21
104500100423c2156C     K08IMP01      CHAIN     NDIMP01L                           21
104600090316     C*
104700090316     C* se il rcd è già presente
104800090316b1   C                   IF        *in21 = *off
104900090316     C*
105000090316     C* se l'esito è valorizzato
105100090316b2   C                   IF        s1esito  <> *blank
105200090316     C* modifico
105300090316     C                   EVAL      IMPImporto = S1ImpScad
105400090316     C                   EVAL      IMPImportD = 0
105500090316     C                   EVAL      IMPEsito  = s1esito
105600090316     c                   if        s1dtimp <> 0
105700090316     c     *jobrun       move      s1dtimp       impdtimp
105800090316     c                   else
105900090316     c                   clear                   impdtimp
106000090316     c                   endif
106100090316     C                   EVAL      IMPImpImp = s1impimp
106200090316     C                   EVAL      IMPNota   = s1nota
106300090316     C                   EVAL      IMPUteUlMo = sdsusr
106400090316     C                   EVAL      IMPDtUlMo = ZDateISO
106500090316     C* Aggiorno/scrivo file particolarità scadenze
106600090316     C                   eval      $Aggiorna=*on
106700090316     c                   exsr      AggParSca
106800090316     C                   UPDATE    NDIMP000
106900140313c2156c* aggiorno la fase relativa all'impegno
107000140313c2156c                   exsr      UpdateFase
107100090316     C*
107200090316     C* se l'esite NON è valorizzato
107300090316x2   C                   ELSE
107400090421     C* cancello solo se c'ERA l'esito !!!
107500090421     c                   if        H1Esito <> *blanks
107600090316     C* pulisco flag particolarità scadenze
107700090316     C                   eval      $Aggiorna=*off
107800090316     c                   exsr      AggParSca
107900090316     c*
108000090316     C                   DELETE    NDIMP000
108100100524c2156c* cancello la fase relativa all'impegno
108200100524c2156c                   exsr      CancellaFase
108300090421     c                   endif
108400090316e2   C                   ENDIF
108500090316     C*
108600090316     C* se il rcd NON è presente
108700090316x1   C                   ELSE
108800090316     C*
108900090421     C* se l'esito è valorizzato
109000090316b2   C                   IF        s1esito  <> *blank
109100090316     C* scrivo
109200090316     C                   CLEAR                   NDIMP000
109300090316     C                   EVAL      IMPSocieta = Societa988
109400090421     C                   EVAL      IMPUnita   = H1Unita
109500090316     C                   EVAL      IMPKcc =  Kcc988
109600090316     C                   EVAL      IMPKsc =  ksc988
109700090316     C*                  EVAL      IMPTpInd = %subst(Contatto:1:2)
109800090316     C*                  EVAL      IMPCdInd = %subst(Contatto:3:3)
109900090316     C*                  EVAL      IMPTpCnt = %subst(Contatto:6:2)
110000090316     C*                  EVAL      IMPCdCnt = %subst(Contatto:8:3)
110100090316     C                   EVAL      IMPDtPar =  H1DtPar
110200090316     C                   EVAL      IMPNrPar =  H1NrPar
110300090316     C                   EVAL      IMPSerPar = H1SerPar
110400090316     C                   EVAL      IMPDtSca =  H1DtSca
110500100423c2156c                   eval      ImpProgr =  H1progr
110600090316     C                   EVAL      IMPImporto = S1ImpScad
110700090316     C                   EVAL      IMPImportD = 0
110800090316     C                   EVAL      IMPEsito  = s1esito
110900090316     c                   if        s1dtimp <> 0
111000090316     c     *jobrun       move      s1dtimp       impdtimp
111100090316     c                   else
111200090316     c                   clear                   impdtimp
111300090316     c                   endif
111400090316     C                   EVAL      IMPImpImp = s1impimp
111500090316     C                   EVAL      IMPNota   = s1nota
111600090316     C                   EVAL      IMPDtUlMo = ZDateISO
111700090316     C                   EVAL      IMPUteUlMo = sdsusr
111800090316     C* richiamo routine calcolo scadenzario per ottenere il saldo partita
111900090316     C* al momento
112000090316     C                   EVAL      IMPSaldoP = 0
112100090316     C                   EXSR      CallX20
112200090316     C                   IF        X20Errori = *blanks
112300090316     C                   EVAL      IMPSaldoP = X20TDLD + X20TDLP + X20TDLG -
112400090316     C                                        (X20TALD + X20TALP + X20TALG)
112500090316     C                   ENDIF
112600090316     c*
112700090316      * Scrivo/aggiorno file particolarità scadenze
112800090316     C                   eval      $Aggiorna=*on
112900090316     c                   exsr      AggParSca
113000090316     C*
113100090316     C                   WRITE     NDIMP000
113200100426c2156c* scrivo azione impegno su file attività recupero credito
113300100426  "  c* la scrivo solo se NON prima fase, in quanto non è possibile
113400131111C2516c* e non avrei tutti i dati
113500131111C2226C* Devo reperire nuovamente l'ultima fase
113600131111C2226C                   Exsr      CtrFase
113700131111C2516c                   if        Impegn057=*on AND
113800131111  "  c                             not %eof(ndrca01L)
113900100426  "  c                   exsr      ScriviFase
114000100426c2156c                   endif
114100100426     C*
114200090316     C*
114300090316     C* se l'esite NON è valorizzato
114400090316 x2  C                   ELSE
114500090316     C* non faccio nulla
114600090316 e2  C                   ENDIF
114700090316     C*
114800090316 e1  C                   ENDIF
114900090316     C*
115000090316     C                   ENDSR
115100100426c2156C************************************************************
115200100426     C* scrittura azione impengo su file attività
115300100426     C************************************************************
115400100426c2156C     ScriviFase    BEGSR
115500100426     c*
115600100426     c* tutti i campi di ndrca rimangono impostati giustamente
115700100426     c* dalla setll readpe su ndrca04l
115800100426     c* gli altri li imposto qui
115900100426     c                   eval      RcaSocieta=Societa988
116000100426     c                   eval      RcaKcc    =Kcc988
116100100426     c                   eval      RcaKSc    =Ksc988
116200100426     c                   eval      RcaPosiz  =PosAttr057
116300100426     c                   eval      Rcaprogr  =RcaProgr+1
116400100511     c                   eval      RcaAzione ='98'
116500100426     c                   eval      RcaGrute  =*blanks
116600100426     c                   eval      RcaBlk    =BklSca057
116700100426     c                   eval      Rcadtpar  =h1dtpar
116800131104     c                   eval      RcaUnita  =h1Unita
116900131104     c                   eval      RcaImporto=ImpImporto
117000131104     c                   eval      RcaImportd=ImpImportd
117100100426     c                   eval      RcaNrpar  =h1NrPar
117200100426     c                   eval      RcaSerPar =h1SerPar
117300100426     c                   eval      RcaDtSca  =h1DtSca
117400140311      ****               eval      RcaDtAtt  =Zdateiso
117500140311     c     *DMY          Move      S1DtImp       WrkData
117600140311     c                   eval      RcaDtAtt = WrkData
117700140311      *
117800100426     c                   eval      RcaStato = ' '
117900100524     c                   eval      RcaprogrIm=H1Progr
118000100426     c                   clear                   RcaDtlet
118100100426     c                   clear                   RcaNrlet
118200100426     c                   clear                   RcaUtechiu
118300100426     c                   clear                   RcaDtChiu
118400100426     c                   clear                   RcaAnn
118500100426     c                   clear                   RcaCodAnn
118600100426     c                   clear                   RcaFlg
118700131209     c                   clear                   RcaLibero
118800100426     c                   eval      Rcanota=Impnota
118900100426     c*
119000100426     c                   write     NdRca000
119100131209      *
119200131209  "   * richiama pgm User Exit PROVVISORIO !!!
119300131209  "  C                   MOVEL     '01'          XUKOPZ
119400131209  "  C* imposta chiave univoca  per user exit
119500131209  "  C                   MOVE      XSCSOC        XUKSOC
119600131209  "  C                   MOVE      XSCDUN        XUKDUN
119700131209  "  C                   MOVEL     DSPGM         XUKPGM
119800131209  "  C                   MOVEL (P) RRNRCA        XUKFL1
119900131209  "  C                   MOVEL     XUKDS         KPJBU
120000131209  "  C                   CALL      'NDCL69RU'                           21
120100131209  "  C                   PARM                    KPJBA
120200131209      *
120300100426c2156C                   ENDSR
120400100427c2156C************************************************************
120500100524     C* cancella relativa fase impegno (SOLO SE NON CHIUSA)
120600100427     C************************************************************
120700100524c2156C     CancellaFase  BEGSR
120800100524     C*
120900100427     c                   eval      RcaSocieta=Societa988
121000100427     C                   eval      RcaPosiz=Posattr057
121100100427     c                   eval      RcaKcc    =kcc988
121200100427     c                   eval      RcaKsc    =ksc988
121300100427     c                   eval      Rcadtpar  =h1DtPar
121400100427     c                   eval      RcaNrpar  =h1NrPar
121500100427     c                   eval      RcaSerPar =h1SerPar
121600100427     c                   eval      RcaDtSca  =h1DtSca
121700100524     c                   eval      RcaProgrIm=H1Progr
121800100525     c                   eval      RcaAzione ='98'
121900100525     c     k10Rca05      chain     NdRca05L                             23
122000100524     c                   if        %found(ndrca05l) and *in23=*off
122100100524     c                             and rcastato=' '
122200100524     c                   delete    NDRCA05
122300100525     c                   else
122400100525     c* provo con azione '99'
122500100525     c                   eval      RcaAzione ='99'
122600100525     c     k10Rca05      chain     NdRca05L                             23
122700100525     c                   if        %found(ndrca05l) and *in23=*off
122800100525     c                             and rcastato=' '
122900100525     c                   delete    NDRCA05
123000100427     c                   endif
123100100525     c                   endif
123200100427     C*
123300100427c2156C                   ENDSR
123400140313c2156C************************************************************
123500140313     C* aggiorna relativa fase impegno (SOLO SE NON CHIUSA)
123600140313     C************************************************************
123700140313c2156C     UpdateFase    BEGSR
123800140313     C*
123900140313     c                   eval      RcaSocieta=Societa988
124000140313     C                   eval      RcaPosiz=Posattr057
124100140313     c                   eval      RcaKcc    =kcc988
124200140313     c                   eval      RcaKsc    =ksc988
124300140313     c                   eval      Rcadtpar  =h1DtPar
124400140313     c                   eval      RcaNrpar  =h1NrPar
124500140313     c                   eval      RcaSerPar =h1SerPar
124600140313     c                   eval      RcaDtSca  =h1DtSca
124700140313     c                   eval      RcaProgrIm=H1Progr
124800140313     c                   eval      RcaAzione ='98'
124900140313     c     k10Rca05      chain     NdRca05L                             23
125000140313     c                   if        %found(ndrca05l) and *in23=*off
125100140313     c                             and rcastato=' '
125200140313     c     *DMY          Move      S1DtImp       WrkData
125300140313     c                   eval      RcaDtAtt = WrkData
125400140313     c                   update    NDRCA05
125500140313     c                   else
125600140313     c* provo con azione '99'
125700140313     c                   eval      RcaAzione ='99'
125800140313     c     k10Rca05      chain     NdRca05L                             23
125900140313     c                   if        %found(ndrca05l) and *in23=*off
126000140313     c                             and rcastato=' '
126100140313     c     *DMY          Move      S1DtImp       WrkData
126200140313     c                   eval      RcaDtAtt = WrkData
126300140313     c                   update    NDRCA05
126400140313     c                   endif
126500140313     c                   endif
126600140313     C*
126700140313c2156C                   ENDSR
126800090316     C************************************************************
126900090316     C* Chiamata routine calcolo scadenzario
127000090316     C************************************************************
127100090316     C     Callx20       BEGSR
127200090316     C*
127300090316     C                   RESET                   x20scadds
127400090316     C                   RESET                   x20scadXds
127500090316     C                   eval      x20societa = Societa988
127600090316     C                   eval      x20ctb     = 'CG'
127700090316    >C                   eval      x20def     = *on
127800090316    >C                   eval      x20prov    = *on
127900090316    >C                   eval      x20gest    = *on
128000090316     C                   eval      x20dtregmd = ZDateIso
128100090316     C                   eval      x20dtregmp = *hival
128200090316     C                   eval      x20dtregmg = ZDateIso
128300090316     C                   eval      x20kcc     =  kcc988
128400090316     C                   eval      x20ksc     =  ksc988
128500090316     C                   move      IMPDtPar      X20DtPar
128600090316     C                   move      IMPNrPar      X20NrPar
128700090316     C                   move      IMPSerPar     X20SerPar
128800090316     C                   eval      x20snatura = 'C'
128900090316     C                   eval      x20parsca  = 'P'
129000090316     C                   eval      X20ParScd  = *blank
129100090316     C                   eval      x20elasca  = 'R'
129200090316     C                   eval      x20DtSca   = ZdateIso
129300090316     C                   eval      x20ppa     = 'N'
129400090316     C                   eval      x20lrt     = 'R'
129500090316     C                   CALL      'X20SCADR'
129600090316     C                   PARM                    X20SCADDS
129700090316     C                   PARM                    X20SCADXDS
129800090316     C*
129900090316     C                   ENDSR
130000090316     C************************************************************
130100090316     C* Aggiorno l'impegno legato a una partita fittizia
130200090316     C************************************************************
130300090316     C     AggImpegnoFittBEGSR
130400090316     C*
130500090316     C                   EVAL      IMPSocieta = Societa988
130600090316     C                   EVAL      IMPKcc =  Kcc988
130700090316     C                   EVAL      IMPKsc =  ksc988
130800090316     C                   EVAL      IMPDtPar =  h1dtpar
130900090316     C                   EVAL      IMPNrPar =  h1nrpar
131000090316     C                   EVAL      IMPSerPar = h1serpar
131100090316     C                   EVAL      IMPDtSca =  h1dtsca
131200090316     C*
131300090323     C     K07IMP01      CHAIN     NDIMP01L                           21
131400090316     C*
131500090316     C* se il rcd è già presente
131600090316     C                   IF        *in21 = *off
131700090316     C*
131800090421     C* se l'esito è valorizzato
131900090316     C                   IF        s1esito  <> *blank
132000090316     C* modifico
132100090316     C                   EVAL      IMPEsito  = s1esito
132200090316     c                   if        s1dtimp <> 0
132300090316     c     *jobrun       move      s1dtimp       impdtimp
132400090316     c                   else
132500090316     c                   clear                   impdtimp
132600090316     c                   endif
132700090316     C                   EVAL      IMPImpImp = s1impimp
132800090316     C                   EVAL      IMPNota   = s1nota
132900090316     C                   EVAL      IMPUteUlMo = sdsusr
133000090316     C                   EVAL      IMPDtUlMo = ZDateISO
133100090316     C                   UPDATE    NDIMP000
133200090316     C*
133300090316     C* se l'esite NON è valorizzato
133400090316     C                   ELSE
133500090316     C* cancello
133600090316     C                   DELETE    NDIMP01L
133700090316     C                   ENDIF
133800090316     C*
133900090316     C* se il rcd NON è presente
134000090316     C                   ELSE
134100090316     C*
134200090316     C* se l'esite è valorizzato
134300090316     C                   IF        s1esito  <> *blank
134400090316     C* scrivo
134500090316     C                   CLEAR                   NDIMP000
134600090316     C                   EVAL      IMPFitt = '1'
134700090316     C                   EVAL      IMPSocieta = Societa988
134800090421     C                   EVAL      IMPUnita   = H1Unita
134900090316     C                   EVAL      IMPKcc =  Kcc988
135000090316     C                   EVAL      IMPKsc =  ksc988
135100090316     C*                  EVAL      IMPTpInd = %subst(Contatto:1:2)
135200090316     C*                  EVAL      IMPCdInd = %subst(Contatto:3:3)
135300090316     C*                  EVAL      IMPTpCnt = %subst(Contatto:6:2)
135400090316     C*                  EVAL      IMPCdCnt = %subst(Contatto:8:3)
135500090316     C                   EVAL      IMPDtPar =  h1dtpar
135600090316     C                   EVAL      IMPNrPar =  PrgFittizie + 1
135700090316     c                   eval      prgfittizie = ImpNrPar
135800090316     C                   EVAL      IMPSerPar = h1serpar
135900090316     C                   EVAL      IMPDtSca =  h1dtsca
136000090316     c                   if        s1dtimp <> 0
136100090316     c     *jobrun       move      s1dtimp       impdtimp
136200090316     c                   else
136300090316     c                   clear                   impdtimp
136400090316     c                   endif
136500090316     C                   EVAL      IMPImpImp = s1impimp
136600090316     C                   EVAL      IMPEsito  = s1esito
136700090316     C                   EVAL      IMPNota   = s1nota
136800090316     C                   EVAL      IMPDtUlMo = ZDateISO
136900090316     C                   EVAL      IMPUteUlMo = sdsusr
137000090316     C                   WRITE     NDIMP000
137100090316     C*
137200090316     C* se l'esite NON è valorizzato
137300090316     C                   ELSE
137400090316     C* non faccio nulla
137500090316     C                   ENDIF
137600090316     C*
137700090316     C                   ENDIF
137800090316     C*
137900090316     C                   ENDSR
138000090316     C************************************************************
138100090316     C* Aggiorno file scadenze della partita
138200090316     C************************************************************
138300090316     C     AggParSca     BEGSR
138400090316     C*
138500090316     c                   eval      PpaSocieta=Societa988
138600090316     c                   eval      PpaCtb    ='CG'
138700090316     c                   eval      Ppakcc    =kcc988
138800090316     c                   eval      Ppaksc    =ksc988
138900090316     c                   eval      PpaDtPar=ImpDtpar
139000090316     c                   eval      PpaNrPar=ImpNrPar
139100090316     c                   eval      PpaSerPar=ImpSerPar
139200090316     c     K07Ppa01      chain     ndppa01l
139300090316 B1  c                   if        %found(NdPpa01l)
139400090316     c*
139500090316     c                   eval      PscSys = ppasys
139600090316     c                   eval      PscNrasInt=ppanrasint
139700090316     c                   eval      PscDtSca  =impdtsca
139800090316     c     K03Psc01      chain     NdPsc01l                             22
139900090316 b2  c                   if        %found(NdPsc01l)
140000090316     c* se file esiste
140100090316     c* se aggiornare...
140200090316 b3  c                   if        $Aggiorna=*on
140300090316     c                   eval      PscBkAmm  =BklSca057
140400090316     c                   eval      PscCliSoll=AggBkl057
140500090316     c                   else
140600090316     c* se pulire...
140700090316     c                   clear                   PscBKAmm
140800090316     c                   eval      PscCliSoll=*off
140900090316 e3  c                   endif
141000090316     C*
141100140313     c**********         UPDATE    NDPSC000
141200090316 x2  c                   else
141300090316     c* se file NON esiste
141400090316     c* scrive ndpsc  solo se impostato qualcosa nel par.mod.
141500090316 b3  c                   if        BklSca057 <> *blanks or
141600090316     c                             AggBkl057 =*on
141700090316     c* e solo se NON si deve pulire, ma aggiornare o scrivere
141800090316 b4  c                   if        $Aggiorna=*on
141900090316     c                   eval      PscBkAmm  =BklSca057
142000090316     c                   eval      PscCliSoll=AggBkl057
142100090316     c* reperisco numero rata
142200090316     c                   exsr      RepNrRata
142300140313     c***                WRITE     NDPSC000
142400090316 e4  c                   endif
142500090316 e3  c                   endif
142600090316     c*
142700090316 e2  c                   endif
142800090316     c*
142900090316 e1  c                   endif
143000090316     C*
143100090316     C                   ENDSR
143200090316     C************************************************************
143300090316     C* Reperimento numero rata da scrivere su NDPSC
143400090316     C************************************************************
143500090316     C     RepNrRata     BEGSR
143600090316     C*
143700090316     c                   eval      ScdSocieta=Societa988
143800090316     c                   eval      ScdCtb    ='CG'
143900090316     c                   eval      Scdkcc    = kcc988
144000090316     c                   eval      Scdksc    = ksc988
144100090316     c                   eval      ScdDtPar= impdtpar
144200090316     c                   eval      ScdNrPar= impnrpar
144300090316     c                   eval      SCdSerPar=impserpar
144400090316     c                   eval      SCdDtSca =impdtsca
144500090316     c     K09Scd01      chain     ndscd01l
144600090316     c                   if        %found(ndscd01L)
144700090316     c                   eval      PscNrRata = ScdNrRata
144800090316     c                   else
144900090316     c                   eval      PscNrRata = 1
145000090316     c                   endif
145100090316     C*
145200090316     C                   ENDSR
145300940127     C************************************************************
145400940131     C* INIZIALIZZAZIONE LISTA
145500940127     C************************************************************
145600940127     C     INZS1         BEGSR
145700940302     C* pulizia SFL
145800940128     C                   SETOFF                                         3031
145900940131     C                   WRITE     FMTC1
146000940128     C                   SETON                                          31
146100940127     C*
146200940128     C* riempimento opzioni
146300940223     C                   MOVEA     $TX1          $TX
146400940223     C                   MOVEA     $FL1          $FL
146500940309     C                   Z-ADD     0             WCOZ
146600960117     C                   Z-ADD     0             WMXO
146700940225     C*  equivale al num.di elementi della schiera $TX1
146800960117     C                   Z-add     OpzLen        WMXO
146900950906     C                   Z-ADD     $C1Op1        $L01
147000950906     C                   Z-ADD     $C1Op2        $L02
147100940128     C                   EXSR      RIEOPZ
147200941214     C                   Z-ADD     23            X
147300941214     C                   MOVE      FF23          $FC2(X)
147400940608     C                   MOVEL     WOPZ1         C1OPZ1
147500060224     C                   MOVEL     $ALLOPT       H1ALLOPT
147600940608     C                   MOVEL     WOPZ2         C1OPZ2
147700940225     C                   Z-ADD     WCOZ          C1COZ
147800940128     C*
147900940128     C* CARICAMENTO SFL A PAGINE
148000940128     C*
148100940201     C                   Z-ADD     0             S1NRR
148200940128     C                   Z-ADD     0             WMAX
148300090312     C                   Z-ADD     0             Z2TotSCAD
148400090312     C                   Z-ADD     0             Z2TotASCA
148500090317     C                   Z-ADD     0             Z2TotImp
148600090312     c* CARICAMENTO CONTROL
148700090312     c                   exsr      RieC1
148800940224     C*
148900940224     C* Posizionamento su file pilota
149000940302     C                   CLEAR                   C1POS
149100941214     C                   EXSR      SETANA
149200940302     C* ripristino i valori del posizionamento
149300940302     C                   MOVE      W1POS         C1POS
149400940131     C*
149500061113     C* Lettura file pilota (si legge tutto subito per dare totali)
149600941214     C                   EXSR      REDANA
149700940131     C*
149800970318     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
149900970318     C                   IF        $LastV = 'D1'
150000970318     C                   Z-ADD     SFLPAG        WPAG
150100970318     C                   ENDIF
150200940127     C                   EXSR      ROLS1
150300970318     C* imposto il caricamento sfl del numero massimo di righe per pagina
150400970318     C                   Z-ADD     SFLPAG        WPAG
150500061116     c* sempre prima pagina
150600061116     C                   z-add     1             C1RCD
150700940127     C*
150800940127     C                   ENDSR
150900940127     C/EJECT
151000940127     C************************************************************
151100940131     C* CARICAMENTO PAGINA LISTA
151200940127     C************************************************************
151300940127     C     ROLS1         BEGSR
151400940127     C*
151500940128     C                   SETOFF                                       32
151600940223     C                   Z-ADD     0             Y
151700940127     C                   Z-ADD     WMAX          S1NRR
151800090312     C                   Z-ADD     S1NRR         WMAX                 30
151900940127     C*
152000061113     c* si carica tutto per dare i totali subito
152100940127     C*
152200940131     C                   EXSR      RIES1
152300940127     C*
152400090312     C                   Z-ADD     S1NRR         C1RCD
152500090312     c*
152600090312     C                   Z-ADD     S1NRR         WMAX                 30
152700940127     C*
152800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
152900940127     C*
153000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
153100940127     C                   MVR                     RESTO             3 0
153200940131     C     PAGINE        MULT      SFLPAG        C1RCD
153300941027     C     RESTO         IFGT      0
153400940127     C                   ADD       1             C1RCD
153500941027     C                   ELSE
153600940407     C                   SUB       SFLPAG        C1RCD
153700940407     C                   ADD       1             C1RCD
153800941027     C                   ENDIF
153900090312     C*
154000100423c2156C* alla fine carico ultime righe vuote
154100100423c2156x***                exsr      rollVuote
154200131111C2226C                   Movel     $EndFile      *in33
154300940128     C*
154400940127     C                   ENDSR
154500940224     C/EJECT
154600100423c2156x************************************************************
154700100423  "  x* caricamento righe vuote (solo 4 righe)
154800100423  "  x************************************************************
154900100423  "  x*    Rollvuote     BEGSR
155000100423  "  x*
155100100423  "  x*
155200100423  "  x*                  SETOFF                                       32
155300100423  "  x*
155400100423  "  x*                  Z-ADD     WMAX          S1NRR
155500100423  "  x* calcolo quante righe mancano al riempimento della pagina
155600100423  "  x*    S1Nrr         DIV       SflPag        Resto
155700100423  "  x*                  MVR                     Resto
155800100423  "  x*    SflPag        SUB       Resto         Resto
155900100423  "  x* se non ne manca nessuna significa che devo riempire una nuova pagina
156000100423  "  x*                  IF        Resto = 0
156100100423  "  x*                  Z-ADD     SflPag        Resto
156200100423  "  x*                  ENDIF
156300100423  "  x*                  CLEAR                   FMTS1
156400100423  "  x*
156500100423  "  x* Carico una pagina vuota
156600100423  "  x*    Resto         DOWGT     *zeros
156700100423  "  x*                  if        $righevuote <=2
156800100423  "  x*                  ADD       1             $righevuote
156900100423  "  x*                  ADD       1             S1NRR
157000100423  "  x*                  WRITE     FMTS1
157100100423  "  x* imposto gli attributi del sfl
157200100423  "  x*                  EXSR      AtrS1
157300100423  "  x*                  endif
157400100423  "  x*                  SUB       1             Resto
157500100423  "  x*                  Enddo
157600100423  "  x*
157700100423  "  x*                  Z-ADD     S1NRR         WMAX                 30
157800100423  "  x*                  Z-ADD     S1NRR         C1RCD
157900100423  "  x* inibisco rollup se righevuote > 3
158000100423  "  x*                  if        $righevuote = 3
158100100423  "  x*                  seton                                        33
158200100423  "  x*                  else
158300100423  "  x*                  setoff                                       33
158400100423  "  x*                  endif
158500100423  "  x*
158600100423c2156x*                  ENDSR
158700090312     C************************************************************
158800090312     C* riempimento dati control
158900090312     C************************************************************
159000090312     C     RieC1         BEGSR
159100100426     c*
159200100426c2156c                   clear                   WrkAttr           4
159300090312     c* conto
159400090312     C                   eval      c1kcc=kcc988
159500090312     C                   eval      c1ksc=ksc988
159600090312     C* Richiamo routine per decodifica dati conto
159700090312     C                   Clear                   ND002DS
159800090312     C                   EVAL      LenOut = %Size(ND002DS)
159900090312     C                   CallB     'NDMVC002'
160000090312     C                   PARM                    societa988
160100090312     C                   PARM                    kcc988
160200090312     C                   PARM                    ksc988
160300090312     C                   PARM      *loval        dataiso
160400090312     C                   PARM      *OFF          GesMsg
160500090312     C                   PARM      *OFF          Esito
160600090312     C                   PARM      'ND002DS'     Struttura
160700090312     C                   PARM                    ND002DS
160800090312     C                   PARM                    LenOut
160900090312     C*
161000090312     C     Esito         ifeq      *ON
161100090312     C                   CLEAR                   ND002DS
161200090312     C                   else
161300100426c2156c                   if        PosAttr057 <> *blanks
161400100426  "  c                   movel     Categorie     DsCateg
161500100426  "  c                   eval      WrkAttr  = SkCateg(Posiz)
161600100426c2156c                   endif
161700090312     C                   eval      c1kscd   = descr002
161800090312     c* indirizzo
161900090312     C                   MOVE      '1'           DVARIC
162000090312     C                   MOVE      societa988    DVASOCIETA
162100090312     C                   MOVE      unita988      DVAUnita
162200090312     C                   MOVE      'C'           DVASNATURA
162300090312     C                   MOVE      tpind002      DVATPIND
162400090312     C                   MOVE      cdind002      DVACDIND
162500090312     C                   MOVE      ksc988        DVAcodice
162600090312     C                   MOVEL     'DVACLI'      DVAstrutt
162700090312     C                   EVAL      DVALenOut = %Size(DVACLI)
162800090312     C*
162900090312     C                   CALLB     'NDDVASOG'    PDVASOG
163000090312     C*
163100090312     C                   if        DVAerrore<>*ON
163200090312     C                   eval      %subst(DVACLI:1:DVALENOUT)
163300090312     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
163400090312     C***                eval      d1indriz=dvcindriz
163500090312     C                   eval      c1cap=dvccap
163600090312     C                   eval      c1localit=dvclocalit
163700090312     C                   eval      c1prov =dvcprov
163800090312     c                   endif
163900090312     c                   endif
164000090312     C* dati ultimo sollecito
164100090312     c                   reset                   NDCL99DS
164200090312     c                   eval      societal99=societa988
164300090312     c                   eval      UnitaL99 =unita988
164400090312     c                   eval      ctbl99='CG'
164500090312     c                   eval      kccl99=Kcc988
164600090312     c                   eval      kscl99=ksc988
164700090312     c                   eval      SolleciL99=*off
164800090312     c                   eval      ImpegniL99=*off
164900090312     c                   eval      ContenzL99=*off
165000090312     c                   eval      RatingL99 =*off
165100090312     c                   eval      BlocchiL99=*off
165200090312     c                   eval      UltimoSL99=*on
165300090312     c                   eval      kpjbu=ndcl99ds
165400090312     C*
165500090312     c                   CALL      'NDCL99R'
165600090312     C                   parm                    kpjba
165700090312     c                   eval      ndcl99ds = kpjbu
165800090312     c                   if        UltimoSL99=*on
165900090312     c     *jobrun       move      DtUlSolL99    c1DtSoll
166000090312     c                   z-add     NrUlSolL99    c1Nrsoll
166100090312     c                   move      GravsolL99    c1Gravita
166200090312     c                   z-add     ImpsolL99     c1ImpSoll
166300090312     c                   endif
166400061116     C*
166500090312     C                   ENDSR
166600090312     C/EJECT
166700940224     C************************************************************
166800061115     C* POSIZIONAMENTO lettura ds multipla
166900940224     C************************************************************
167000941214     C     SETANA        BEGSR
167100061108     c*
167200090310     c                   reset                   NDC988Ds1
167300061113     c* pulisco ds multipla
167400090310     c                   do        1000          $x
167500090310     c     $x            occur     ndc988ds1
167600090310     c                   clear                   ndc988ds1
167700061113     c                   enddo
167800061108     c*
167900940224     C                   ENDSR
168000940128     C/EJECT
168100940128     C************************************************************
168200940131     C* LETTURA RCD ARCHIVIO PILOTA
168300940128     C************************************************************
168400941214     C     REDANA        BEGSR
168500940128     C*
168600950606     C                   MOVEL     *OFF          $EndFile
168700970910     C*
168800950606     C     $EndFile      DOUEQ     *ON
168900940131     C*
169000090310     c                   call      'NDC988R1'
169100090310     C                   parm                    NDC988DS
169200090310     C                   parm                    NDC988DS1
169300090310     C                   PARM                    TotScaduto
169400090310     C                   PARM                    TotAScad
169500090317     C                   PARM                    TotImpegni
169600090310     C                   PARM                    PrgFittizie
169700090312     C                   PARM                    Errore            1
169800140313     C                   PARM                    $F10mult
169900090511     C*
170000090511     C                   MOVEL     *ON           $EndFile
170100940131     C*
170200941027     C                   ENDDO
170300940131     C*
170400940131     C                   ENDSR
170500940224     C/EJECT
170600940224     C************************************************************
170700940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
170800940224     C************************************************************
170900940224     C     CLCPAG        BEGSR
171000940224     C* Input :
171100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
171200940224     C*          cursore
171300940224     C* - SFLPAG = numero rcd per pagina sfl
171400940224     C* Output :
171500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
171600940224     C*
171700940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
171800940224     C                   MVR                     RESTO             3 0
171900940224     C     RESTO         IFGT      0
172000940224     C                   ADD       1             PAGINE
172100940224     C                   ENDIF
172200940224     C     PAGINE        MULT      SFLPAG        WPAG
172300940224     C*
172400940224     C                   ENDSR
172500940131     C/EJECT
172600940131     C************************************************************
172700061115     C* RIEMPIMENTO RIGA LISTA da ds multipla
172800940131     C************************************************************
172900940131     C     RIES1         BEGSR
173000940131     C*
173100131111C2226C
173200131111  "  C                   Clear                   savdtpar
173300131111  "  C                   Clear                   savnrpar
173400131111C2226C                   Clear                   savserpar
173500090310     c                   do        1000          $X                5 0
173600090316     C                   CLEAR                   FMTS1
173700090310     c     $x            occur     ndc988ds1
173800090312     c* anche le fittizie hanno un numero
173900090312     c                   if        nrpar988 = 0
174000090312     c                   leave
174100090312     c                   endif
174200090316     c*
174300090323     c                   move      unita1988     h1unita
174400090316     c                   move      dtpar988      h1dtpar
174500090316     c                   z-add     nrpar988      h1nrpar
174600090316     c                   move      serpar988     h1serpar
174700090316     c                   move      dtsca988      h1dtsca
174800140311c2156c                   z-add     progr988      h1progr
174900100426  "  c* in alta intensità la partita selezionata
175000100426  "  c                   if        h1dtpar = dtpar988x and
175100100426  "  c                             h1nrpar = nrpar988x and
175200100426  "  c                             h1serpar= serpar988x
175300100426  "  c                   move      *on           h1in17
175400100426  "  c                   seton                                        17
175500100426c2156c                   endif
175600070928     c*
175700090312     c* partita
175800090312     c                   if        dtpar988 <> *loval
175900090312     c     *jobrun       move      dtpar988      s1dtpar
176000090316     c                   z-add     nrpar988      s1nrpar
176100090312     c                   endif
176200090312     c                   if        dtsca988 <> *loval
176300090312     c     *jobrun       move      dtsca988      s1dtsca
176400090312     c                   endif
176500090316     c                   z-add     impscad988    s1impscad
176600090312     c                   move      gravsol988    s1gravita
176700140317     c* annull.
176800140317     c                   eval      H1ImpAnn = ImpAnn988
176900090312     c* esito
177000090421     c                   eval      H1Esito = esito988
177100090312     c                   if        esito988 <> *blanks
177200090310     c                   eval      s1esito = esito988
177300090312     C                   MOVE      '1'           XTBRIC
177400090312     C                   MOVE      xscsoc        XTBAZI
177500090312     C                   MOVE      *ZERO         XTBKEY
177600090312     C                   MOVE      '042'         XTBCOD
177700090312     C                   MOVE      esito988      XTBKEY
177800090312     C                   CALLb     'XATB'
177900090312     C                   PARM                    XATBDS
178000090312     C     XTBERR        IFEQ      '0'
178100090312     C                   MOVEL     XTBUNI        ang042ds
178200090312     C                   eval      s1esitod =    des042
178300090312     c                   endif
178400090312     c                   endif
178500090312     c* impegno
178600090312     c                   if        dtimp988 <> *loval
178700090312     c     *jobrun       move      dtimp988      s1dtimp
178800090312     c                   endif
178900090312     c                   z-add     impimp988     s1impimp
179000090312     c                   movel     nota988       s1nota
179100100423c2156C* faccio vedere i dati di partita solo alla prima riga
179200100423  "  c                   if        savdtpar = s1dtpar and
179300100423  "  c                             savnrpar = s1nrpar and
179400100423  "  c                             savserpar= h1serpar
179500100423  "  c                   clear                   s1dtpar
179600100423  "  c                   clear                   s1nrpar
179700100423  "  C                   else
179800100423  "  c                   eval      savdtpar=s1dtpar
179900100423  "  c                   eval      savnrpar=s1nrpar
180000100423  "  c                   eval      savserpar=h1serpar
180100100423c2156c                   endif
180200140311      *
180300140313     c                   if        $F10mult = *on
180400140311     c                   add       1             h1progr
180500140311     c                   clear                   s1dtimp
180600140311     c                   clear                   s1impimp
180700140311     c                   clear                   s1nota
180800140311     c                   clear                   s1esito
180900140313     c                   clear                   h1esito
181000140317     c                   clear                   h1impann
181100140317     c                   clear                   s1esitod
181200140311     c                   endif
181300140317     C* imposto quegli attributi del sfl che sono modificabili
181400140317     C* al richiamo del determinato pgm
181500140317     C                   EXSR      ATRS1
181600100423     c*
181700071001     C                   ADD       1             S1NRR
181800071001     C                   ADD       1             Y
181900071001     C*
182000061113     C                   WRITE     FMTS1
182100061113     C*
182200061113     c                   enddo
182300940131     C*
182400940131     C                   ENDSR
182500950301     C/EJECT
182600950301     C************************************************************
182700950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
182800950301     C************************************************************
182900950301     C     ATRS1         BEGSR
183000950301     C*
183100971120     C* in questa routine bisogna attivare indicatori o campi programma
183200971120     C* che gestiscono gli attributi di uno o più campi di questa videata;
183300971120     C* attenzione : non è questo il posto in cui eseguire controlli,
183400971120     C*  dovrebbero già arrivare dei flag
183500100426     C* in alta intensità la partita selezionata
183600100426     C                   MOVE      H1IN17        *IN17
183700970618     C*
183800140317     C                   MOVE      H1impann      *IN18
183900950301     C*
184000950301     C                   ENDSR
184100941206     C/EJECT
184200090406     C************************************************************
184300090406     C* GESTIONE F05 VIDEO S1
184400090406     C************************************************************
184500090406     C     F05S1         BEGSR
184600090406     C*
184700090406     C                   MOVE      *ON           $inzs1
184800090406     C                   MOVE      *HIVAL        $LASTV
184900090406     C*
185000090406     C                   ENDSR
185100090406     C/EJECT
185200941206     C************************************************************
185300941206     C* GESTIONE F08 VIDEO S1
185400941206     C************************************************************
185500941206     C     F08S1         BEGSR
185600941206     C*
185700941206     C                   MOVE      *ON           $FINE
185800090310    >C                   MOVE      '0'           RET988
185900941206     C*
186000941206     C                   ENDSR
186100940309     C/EJECT
186200940309     C************************************************************
186300940309     C* GESTIONE F03 VIDEO S1
186400940309     C************************************************************
186500940309     C     F03S1         BEGSR
186600940309     C*
186700940309     C                   MOVE      *ON           $FINE
186800090310    >C                   MOVE      '1'           RET988
186900940325     C* fine programma
187000940325     C                   EXSR      ENDPGM
187100940309     C*
187200940309     C                   ENDSR
187300940309     C/EJECT
187400100423C2156 ************************************************************
187500100423      * Gestione Immissione altri impegni
187600100423      ************************************************************
187700140313c2156C     F10S1old      BEGSR
187800100423     C*
187900100423     C* determino Riga/Colonna del cursore
188000100423     C                   MOVE      NRG           R$$
188100100423     C                   MOVE      NCL           C$$
188200100423     C                   Z-ADD     RIRI          CURR
188300100423     C                   Z-ADD     COCO          CURC
188400100423     C*
188500100423     C* se sono posizionato sul sfl
188600100423   B1C     H1NMRC        IFEQ      'C988S1'
188700100423     C* leggo il rcd
188800100423     C     C1NRR         CHAIN     FMTS1                              24
188900100423     C*
189000100423     C     *In24         IFEQ      *Off
189100100423     C* imposto gli attributi del sfl
189200100423     C                   EXSR      AtrS1
189300100423     C                   UPDATE    FMTS1
189400100423     C                   MOVE      *OFF          *In32
189500100423     C* gestione videata immissione
189600100427     c                   clear                   $AggiornaI        1
189700100423     c                   move      'D4'          $Gest
189800100423     c                   move      *on           $Inzd4            1
189900100423     c                   move      *off          $FineD4           1
190000100423     C     $FineD4       doweq     *off
190100100423     C                   exsr      GESD4
190200100423     c                   enddo
190300100423     c*
190400100423     c*
190500100423     C* ricarico sfl se digitato F6
190600100427     c                   if        $AggiornaI= *on
190700100423     c                   move      *on           $inzs1
190800100423     c                   move      'S1'          $gest
190900100423     c                   ENDIF
191000100423     c*
191100100423     C                   ENDIF
191200100423     C*
191300100423     C                   ENDIF
191400100423     C*
191500100423     C* imposto pos. del cursore
191600100423     C                   Z-ADD     CURR          H1RIGA
191700100423     C                   Z-ADD     CURC          H1COLO
191800100423     C*
191900140313c2156C                   ENDSR
192000140311C2156 ************************************************************
192100140311      * Gestione Immissione altri impegni
192200140311      ************************************************************
192300140313c2156C     F10S1         BEGSR
192400140311     C*
192500140311     C                   MOVE      *ON           $inzs1
192600140311     C                   MOVE      *HIVAL        $LASTV
192700140313     C                   MOVE      *ON           $F10Mult
192800140311     C*
192900140313c2156C     XF10S1        ENDSR
193000940309     C************************************************************
193100940309     C* GESTIONE F12 VIDEO S1
193200940309     C************************************************************
193300940309     C     F12S1         BEGSR
193400940309     C*
193500940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
193600940309     C     $LOP          IFEQ      1
193700940309     C                   MOVE      'D1'          $GEST
193800940309     C*  se la 1a videata è questa, per F12 torno a menu
193900940309     C                   ELSE
194000940309     C                   MOVE      *ON           $FINE
194100090310    >C                   MOVE      '2'           RET988
194200941027     C                   ENDIF
194300940309     C*
194400940309     C                   ENDSR
194500940309     C/EJECT
194600131111C2226C************************************************************
194700131111  "  C* GESTIONE F13 VIDEO S1
194800131111  "  C************************************************************
194900131111C2226C     F13S1         BEGSR
195000131111     C* Pulisco i campi di salvataggio
195100131111     C                   Clear                   SavImpImp
195200131111     C                   Clear                   SavEsito
195300131111     C                   Clear                   SavDtImp
195400131111     C                   Clear                   SavNota
195500131111     C* il controllo lo faccio solo se il sfl NON è vuoto
195600131111   1bC                   If        WMax    <>    0
195700131111     C* determino Riga/Colonna del cursore
195800131111     C                   MOVE      NRG           R$$
195900131111     C                   MOVE      NCL           C$$
196000131111     C                   Z-ADD     RIRI          CURR
196100131111     C                   Z-ADD     COCO          CURC
196200131111     C*
196300131111     C* se sono posizionato sul sfl
196400131111     C* salvo il numero del record su cui sono posizionato
196500131111     C                   Z-Add     C1Nrr         WC1Rcd
196600131111     C* leggo il rcd
196700131111     C     C1NrR         CHAIN     FMTS1                              21
196800131111     C* salvo i dati su cui sono posizionato
196900131111     C                   If        S1ImpImp  <> 0  or
197000131111     C                             S1Esito   <> *blanks or
197100131111     C                             S1DtImp   <> 0  or
197200131111     C                             S1Nota    <> *blanks
197300131111     C                   Move      S1ImpImp      SavImpImp
197400131111     C                   Movel     S1Esito       SavEsito
197500131111     C                   Move      S1DtImp       SavDtImp
197600131111     C                   Movel     S1Nota        SavNota
197700131111     C*
197800131111     C                   Z-Add     C1Nrr         $X
197900131111     C* attivo SflNxtChg
198000131111     C                   SetOn                                        32
198100131111     C                   exsr      Atrs1
198200131111     C                   UpDate    FmtS1
198300131111     C                   SetOff                                       32
198400131111     C*
198500131111     C* mi posiziono sul record successivo
198600131111     C                   Add       1             $X
198700131111     C*
198800131111     C                   DoW       $X      <=    Wmax
198900131111     C     $X            Chain     FmtS1                              21
199000131111     C   21              Leave
199100131111     C* Riporto i dati solo se non già valorizzati
199200131111     C                   If        S1ImpImp  = 0  and
199300131111     C                             S1Esito   = *blanks and
199400131111     C                             S1DtImp   = 0  and
199500131111     C                             S1Nota    = *blanks
199600131111     C*
199700131111     C                   Move      SavImpImp     S1ImpImp
199800131111     C                   Movel     SavEsito      S1Esito
199900131111     C                   Move      SavDtImp      S1DtImp
200000131111     C                   Movel     SavNota       S1Nota
200100131111     C*
200200131111     C                   Endif
200300131111     C* attivo SflNxtChg
200400131111     C                   SetOn                                        32
200500131111     C                   exsr      Atrs1
200600131111     C                   UpDate    FmtS1
200700131111     C                   SetOff                                       32
200800131111     C*
200900131111     C                   Add       1             $X
201000131111     C                   enddo
201100131111     c*
201200131111     C                   Eval      WPag     =    9999
201300131111     C*
201400131111     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
201500131111     C*
201600131111     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
201700131111     C                   MVR                     RESTO             3 0
201800131111     C     PAGINE        MULT      SFLPAG        C1RCD
201900131111     C     RESTO         IFGT      0
202000131111     C                   ADD       1             C1RCD
202100131111     C                   ELSE
202200131111     C                   SUB       SFLPAG        C1RCD
202300131111     C                   ADD       1             C1RCD
202400131111     C                   ENDIF
202500131111     C*
202600131111     C* reimposto sflpag
202700131111     C                   Eval      WPag     =    SflPag
202800131111     C*
202900131111     C* ripristino la posizione iniziale del record
203000131111     c                   Z-Add     WC1Rcd        C1Rcd
203100131111     C*
203200131111     C                   endif
203300131111     C*
203400131111     C                   endif
203500131111     C*
203600131111     C*
203700131111     C                   ENDSR
203800131111C2226C/EJECT
203900940309     C************************************************************
204000940309     C* GESTIONE F17 VIDEO S1
204100940309     C************************************************************
204200940309     C     F17S1         BEGSR
204300940309     C*
204400940309     C                   MOVE      'D1'          $GEST
204500941104     C* se il pgm è stato richiamato con chiave univoca
204600941104     C* e dopo è stata richiesta una parzializzazione
204700941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
204800941104     C* valore; propongo come ordinamento il 1°)
204900950606     C     $Unique       IFEQ      *ON
205000950606     C                   MOVE      *OFF          $Unique
205100941027     C                   ENDIF
205200940831     C     $ORD          IFEQ      99
205300940831     C                   Z-ADD     1             $ORD
205400941104     C                   MOVE      *ON           $INZD1
205500941027     C                   ENDIF
205600940309     C*
205700940309     C                   ENDSR
205800940309     C/EJECT
205900940309     C************************************************************
206000940309     C* GESTIONE F18 VIDEO S1
206100940309     C************************************************************
206200940309     C     F18S1         BEGSR
206300940309     C*
206400940309     C                   MOVE      'D2'          $GEST
206500940309     C                   MOVEL     *ON           $INZD2
206600940831     C     $ORD          IFEQ      99
206700940831     C                   Z-ADD     1             $ORD
206800941027     C                   ENDIF
206900940309     C*
207000940309     C                   ENDSR
207100940309     C/EJECT
207200940309     C************************************************************
207300940309     C* GESTIONE F23 VIDEO S1
207400940309     C************************************************************
207500940309     C     F23S1         BEGSR
207600940309     C*
207700950906     C                   Z-ADD     $C1Op1        $L01
207800950906     C                   Z-ADD     $C1Op2        $L02
207900940309     C                   EXSR      RIEOPZ
208000941214     C                   Z-ADD     23            X
208100941214     C                   MOVE      FF23          $FC2(X)
208200940608     C                   MOVEL     WOPZ1         C1OPZ1
208300060224     C                   MOVEL     $ALLOPT       H1ALLOPT
208400940608     C                   MOVEL     WOPZ2         C1OPZ2
208500940309     C                   Z-ADD     WCOZ          C1COZ
208600940309     C*
208700940309     C                   ENDSR
208800940309     C/EJECT
208900940309     C************************************************************
209000940309     C* GESTIONE F24 VIDEO S1
209100940309     C************************************************************
209200940309     C     F24S1         BEGSR
209300090423     C*
209400090423     c* totali
209500090423     c                   z-add     TotScaduto    z2TotScad
209600090423     c                   Z-ADD     TotAscad      z2TotASca
209700090423     c                   Z-ADD     TotImpegni    z2TotImp
209800940506     C*
209900940506     C                   MOVEA     $FX2          $FX
210000940506     C                   MOVEA     $FC2          $FC
210100940506     C                   MOVEA     $FR2          $FR
210200940506     C                   Z-ADD     $ULKS1        $ULK
210300950726     C                   Z-ADD     $S1L01        $L01
210400950726     C                   Z-ADD     $S1L02        $L02
210500940506     C                   EXSR      RIEKEY
210600940506     C                   Z-ADD     $ULK          $ULKS1            3 0
210700940720     C                   MOVEA     $FC           $FC2
210800070927     C                   MOVEL     $KEY1         Z2KE
210900061108     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
211000070927     C                   MOVEL     $KEY2         Z2KE2
211100070927     c* riga unica tasti
211200070927     c                   eval      $A = %triml(z2ke)
211300070927     c                   eval      $B = %triml(z2ke2)
211400070927     c     $A            cat(P)    $B:0          z2ke1
211500061108     C                   WRITE     FMTZ2
211600940506     C*
211700940309     C                   ENDSR
211800940128     C/EJECT
211900940128     C************************************************************
212000940131     C* CONTROLLO TESTATA LISTA
212100940128     C************************************************************
212200940131     C     CTRC1         BEGSR
212300940128     C*
212400940201     C                   MOVE      *OFF          *IN99
212500940131     C*
212600940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
212700940202     C* L'INIZIALIZZAZIONE LISTA
212800940202     C*
212900941214     C     C1POS         IFNE      W1POS
213000940202     C                   EXSR      POSS1
213100941027     C                   ENDIF
213200940202     C*
213300940202     C                   ENDSR
213400940202     C/EJECT
213500940202     C************************************************************
213600940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
213700940202     C************************************************************
213800940202     C     POSS1         BEGSR
213900940202     C*
214000940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
214100940202     C*
214200941118   B1C     WMAX          IFGT      0
214300940202     C                   READC     FMTS1                                  21
214400941118   B2C     *IN21         DOWEQ     *OFF
214500940202     C     *IN99         ANDEQ     *OFF
214600940202     C*
214700941118   B3C     S1OPZ         IFNE      0
214800960109     C                   SETON                                        92  99
214900941118   E3C                   ENDIF
215000940202     C* se rilevata opzione attiva, attivo sflnxtchg
215100941118   B3C     *IN99         IFEQ      *ON
215200940202     C                   MOVE      *ON           *IN32
215300950301     C*
215400950301     C* imposto quegli attributi del sfl che sono modificabili
215500950301     C* al richiamo del determinato pgm
215600941118     C                   EXSR      ATRS1
215700941118     C*
215800940202     C                   UPDATE    FMTS1
215900941118   X3C                   ELSE
216000940202     C                   READC     FMTS1                                  21
216100941118   E3C                   ENDIF
216200940202     C*
216300941118   E2C                   ENDDO
216400940202     C*
216500941118   E1C                   ENDIF
216600940202     C*
216700940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
216800940221     C* AL VALORE DI POSIZIONAMENTO :
216900940221     C* se questo è maggiore del valore del rispettivo campo di
217000940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
217100940221     C* con i valori di parzializzazione.
217200940221     C*
217300941118   B1C     *IN99         IFEQ      *OFF
217400940721    >C                   MOVE      *BLANK        W1POS
217500940202     C*
217600950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
217700950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
217800950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
217900950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
218000950525     C* verificata la condizione suddetta avverrà con posizionamento a
218100950525     C* partire dal valore di parzializzazione
218200950525     C*
218300941118   B2C                   SELECT
218400940202     C*
218500940202     C     $ORD          WHENEQ    1
218600061108    >C*    C1POS         IFGT      D1Codice
218700061108    >C*                  MOVEL     C1POS         W1POS
218800061108     C*                  ENDIF
218900950525     C*
219000950525     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
219100950525     C*
219200941118   E2C                   ENDSL
219300950525     C*
219400950525     C* richiedo l'inizializzazione sfl
219500950525     C                   MOVE      *ON           $INZS1
219600940202     C*
219700941118   E1C                   ENDIF
219800940202     C*
219900940131     C                   ENDSR
220000940131     C/EJECT
220100940131     C************************************************************
220200940131     C* CONTROLLO OPZIONI LISTA
220300940131     C************************************************************
220400940131     C     CTRS1         BEGSR
220500940131     C*
220600940202     C                   MOVEL     *OFF          $ESCI
220700940201     C                   SETOFF                                       99
220800940131     C                   Z-ADD     0             S1OPZ
220900940131     C*
221000940127     C* Leggo il sfl solo se ci sono rcd
221100941118   B1C     WMAX          IFGT      0
221200940131     C                   READC     FMTS1                                  21
221300940127     C*
221400940131     C* esce se fine sfl o errore che richiede l'uscita
221500941118   B2C     *IN21         DOWEQ     *OFF
221600940131     C     $ESCI         ANDEQ     *OFF
221700940201     C                   Z-ADD     S1NRR         C1RCD
221800940131     C* ctrl su riga
221900940131     C                   EXSR      RECS1
222000940131     C* gestione opzioni
222100941118   B3C     S1OPZ         IFNE      0
222200940201     C     *IN99         ANDEQ     *OFF
222300940131     C                   EXSR      OPZS1
222400941118   E3C                   ENDIF
222500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
222600941118   B3C     *IN99         IFEQ      *ON
222700940131     C                   MOVE      *ON           *IN32
222800941118   E3C                   ENDIF
222900950301     C*
223000950301     C* imposto quegli attributi del sfl che sono modificabili
223100950301     C* al richiamo del determinato pgm
223200941118     C                   EXSR      ATRS1
223300941118     C*
223400940131     C                   Z-ADD     0             S1OPZ
223500940223     C*
223600940127     C                   UPDATE    FMTS1
223700941125     C                   MOVE      *OFF          *IN32
223800940223     C*
223900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
224000941118   B3C     $ESCI         IFEQ      *OFF
224100940131     C                   READC     FMTS1                                  21
224200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
224300941118   B4C     *IN21         IFEQ      *ON
224400940201     C                   MOVE      WINZS1        $INZS1
224500010620     C                   MOVE      *off          WINZS1
224600980603     C* se è avvenuta almeno una modifica
224700980603   B5C                   IF        $INZS1 = *on
224800940204     C* calcolo pagina a cui deve posizionarsi
224900940728     C     WSFL          IFGT      0
225000940728     C                   EXSR      CLCPAG
225100940728     C                   ELSE
225200940728     C                   Z-ADD     SFLPAG        WPAG
225300940728     C                   ENDIF
225400980603   E5C                   ENDIF
225500941118   E4C                   ENDIF
225600980603   E3C                   ENDIF
225700940131     C*
225800941118   E2C                   ENDDO
225900940127     C*
226000941118   E1C                   ENDIF
226100940131     C*
226200940127     C                   ENDSR
226300940127     C/EJECT
226400940127     C************************************************************
226500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
226600940127     C************************************************************
226700940131     C     RECS1         BEGSR
226800940131     C*
226900940201     C* reset indicatori DSPATR
227000940201     C                   MOVE      *ALL'0'       IN4049           10
227100940201     C                   MOVEA     IN4049        *IN(40)
227200090317     c*
227300100426c2156C* data impegno obbligatoria se inserito l'esito
227400100426  "  c                   if         s1dtimp =  0 and s1esito <> *blanks
227500100426  "  c                   seton                                        5199
227600100426c2156c                   endif
227700090406     c* se metto la data e l'importo impegno è =0, propongo l'importo
227800090317     c* della scadenza
227900090406     c                   if        s1dtimp <> 0
228000090317     c                             and s1impimp = 0
228100090317     c                   eval      s1impimp=s1impscad
228200090317     c                   endif
228300090316     C*
228400090316     C* esito obbligatorio se inserito qualcosa
228500090316     c                   if        (s1dtimp <> 0 or
228600090316     c                              s1impimp<> 0 or
228700090316     c                              s1nota  <> *blanks )
228800090316     C                             AND
228900090316     C                             s1esito = *blanks
229000090316     c                   seton                                        5399
229100090316     c                   endif
229200970424     C*
229300090316     C* esito
229400090316     c                   clear                   s1esitod
229500090316     c                   if        s1esito <> *blanks
229600090316     C                   MOVE      '1'           XTBRIC
229700090316     C                   MOVE      XSCSOC        XTBAZI
229800090316     C                   MOVE      *ZERO         XTBKEY
229900090316     C                   MOVE      '042'         XTBCOD
230000090316     C                   MOVE      s1esito       XTBKEY
230100090316     C                   CALLb     'XATB'
230200090316     C                   PARM                    XATBDS
230300090316     C     XTBERR        IFEQ      *Off
230400090316     C                   MOVEL     XTBUNI        ang042ds
230500090316     C                   MOVEL     DEs042        s1esitoD
230600090316     c                   else
230700090316     c                   seton                                        5099
230800090316     c                   endif
230900090316     C                   ENDIF
231000090316     C*
231100090316     C* data impegno
231200090316     C                   If        s1dtimp <> 0
231300090316     c* controllo validità formale
231400090316     C     *JobRun       Test(D)                 s1dtimp                15
231500090316     C                   If        *in15 = *on
231600090316     c                   seton                                        5199
231700090316     c                   endif
231800090316     c                   endif
231900090316     C*
232000090317     C* data e importo impegno
232100090316     c                   if        (s1dtimp <> 0 and s1impimp=0)
232200090316     c                             OR
232300090316     c                             (s1dtimp =  0 and s1impimp<> 0)
232400090316     c                   seton                                        5299
232500090316     c                   endif
232600100426     C*
232700131030      * se immissione controllo data impegno maggiore/uguale oggi
232800131030     c                   if        h1esito = *blanks
232900131030     c                             and s1esito <> *blanks
233000131030     c                             and S1DTimp <> *ZERO
233100131030     c                             and *IN99 = *OFF
233200131030     c                   if        %date(S1DtImp:*jobrun) < zdateiso
233300131030     c                   seton                                        5199
233400131030     c                   endif
233500131030     c                   endif
233501140530      *
233502140530      * se immissione controllo data attività ENTRO 30 GG.
233503140530     c                   if        h1esito=  *blanks
233504140530     c                             and s1esito<> *blanks
233505140530     c                             and S1DTimp <> *ZERO
233506140530     c                             and *IN99 = *OFF
233507140530     c     zdateiso      adddur    1:*months     wrkdata
233508140530     c                   if        %date(S1DtImp:*jobrun) > wrkdata
233509140530     c                   seton                                        5199
233510140530     c                   endif
233511140530     c                   endif
233600131030     c*
233700131030      * se aggiornamento controllo data impegno maggiore/uguale oggi
233800131030     c                   if        h1esito <> *blanks
233900131030     c                             and s1esito <> *blanks
234000131030     c                             and S1DTimp <> *ZERO
234100131030     c                             and *IN99 = *OFF
234200131030     c                   exsr      CtrAggDtImp
234300131030     c                   endif
234400131203      *
234500131203      * controlli se fase chiusa in aggiornamento
234600131203brt  c                   if        *in99 = *off
234700131203brt  c                   exsr      CtrAggFase
234800131203brt  c                   endif
234900131203      *
235000131203c2156c* controlli sulla fase delle attività solo immissione!!
235100100426  "  c                   if        h1esito =  *blanks and
235200100426  "  c                             s1esito <> *blanks and impegn057=*on
235300100426  "  c                   exsr      CtrFase
235400100426c2156c                   endif
235500090316     c
235600090316      * ripristino attributi campi sfl
235700090316     C*
235800090316     C     *IN99         ifeq      *on
235900090316     C     $esci         oreq      *on
236000090316     C                   Z-ADD     s1nrr         C1RCD
236100090316     C                   move      *on           $esci
236200090316     C                   endif
236300090316     C*
236400940131     C                   ENDSR
236500090318     C************************************************************
236600090318     C* Calcola totale impegni
236700090318     C************************************************************
236800090318     C     ClcTotImp     BEGSR
236900090318     c*
237000090318     c                   clear                   $lastv
237100090318     c                   clear                   z2TotImp
237200090318     c*
237300090318     C                   DO        *hival        S1NRR
237400090318     C     S1NRR         CHAIN     FMTS1                              21
237500090318     c   21              leave
237600090318     c*
237700090318     C                   if        S1esito  <> *blanks
237800090318     c                   add       s1impimp      z2totimp
237900090318     C                   endif
238000090318     C*
238100090318     C                   ENDDO
238200090318     C*
238300090318     C                   ENDSR
238400090318     C/EJECT
238500941118     C/EJECT
238600941118     C************************************************************
238700941118     C* GESTIONE OPZIONI LISTA
238800941118     C************************************************************
238900941118     C     OPZS1         BEGSR
239000950301     C*
239100950301     C                   Z-ADD     RIRI          CURR
239200950301     C                   Z-ADD     COCO          CURC
239300941118     C*
239400941027     C                   SELECT
239500940225     C*
239600950620     C* se opz=Selezione
239700950620     C* passo la chiave al pgm chiamante
239800940225     C     S1OPZ         WHENEQ    1
239900090310    >C                   MOVE      S1OPZ         OPZ988
240000090310     C* passare nei campi appositi della DS NDC988DS il valore
240100950301     C* della chiave della VL univoca (se c'è)
240200940225     C                   MOVE      *ON           $ESCI
240300940225     C                   MOVE      *ON           $FINE
240400940909     C*
240500940909     C                   MOVE      *HIVAL        $LASTV
240600940225     C*
240700950620     C* se altre opzioni
240800950620     C* richiamo pgm gestione anagrafica
240900941027     C                   OTHER
241000940225     C*
241100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
241200941027     C     *IN99         IFEQ      *ON
241300940223     C                   SETON                                        40
241400941027     C                   ENDIF
241500940131     C*
241600940506     C                   MOVE      *HIVAL        $LASTV
241700941027     C                   ENDSL
241800940225     C*
241900940131     C                   ENDSR
242000940223     C/EJECT
242100940131     C************************************************************
242200940131     C* GESTIONE VALORI ASSUNTI
242300940131     C************************************************************
242400940131     C     GESD2         BEGSR
242500940131     C*
242600941118   B1C     $INZD2        IFEQ      *ON
242700940131     C                   EXSR      INZD2
242800940223     C                   MOVE      *OFF          $INZD2
242900941118   E1C                   ENDIF
243000940202     C*
243100941118   B1C     $LASTV        IFNE      'D2'
243200940923     C                   EXSR      GESPIE
243300961023   B1C     $LASTV        IFNE      *HIVAL
243400940202     C                   MOVE      $LASTV        $LASTG
243500961023     C                   ENDIF
243600940202     C                   MOVE      'D2'          $LASTV
243700941118   E1C                   ENDIF
243800940131     C*
243900940707     C* se esistono errori sulla videata
244000940707     C* emetto la write del formato a indicatori spenti per vedere
244100940707     C* le eventuali decodifiche
244200941118   B1C     *IN99         IFEQ      *ON
244300940707     C                   MOVEA     *IN           WIN
244400940707     C                   MOVE      *ALL'0'       IN5098           49
244500940707     C                   MOVEA     IN5098        *IN(50)
244600940707     C                   WRITE     FMTD2
244700940707     C                   MOVEA     WIN           *IN
244800941118   E1C                   ENDIF
244900940707     C*
245000940131     C                   EXFMT     FMTD2
245100960605     C*
245200960605     C* azzero la posizione del cursore
245300960605     C                   Z-ADD     0             H1RIGA
245400960605     C                   Z-ADD     0             H1COLO
245500941205     C*
245600951005     C* CONTROLLO ABILITAZIONE TASTI
245700951005     C* INPUT
245800951005     C* $FC3 = Abilitazioni videata D2
245900951005     C* OUTPUT
246000951005     C* *IN97 + *IN99 Errori
246100951005     C                   MOVEA     $FC3          $FC
246200951005     C                   EXSR      CTRKEY
246300941205     C*
246400941205   B1C     *IN99         IFEQ      *OFF
246500941205     C*
246600941205   B2C                   SELECT
246700940131     C* F3=Fine
246800940506     C     $TASTO        WHENEQ    F03
246900940309     C                   EXSR      F03D2
247000061116     C* F4=Ricerca
247100061116     C     $TASTO        WHENEQ    F04
247200061116     C                   EXSR      Search
247300940131     C* F12=Ritorno
247400940506     C     $TASTO        WHENEQ    F12
247500940309     C                   EXSR      F12D2
247600940916     C* F24=Altri tasti
247700940916     C     $TASTO        WHENEQ    F24
247800940916     C                   EXSR      F24D2
247900940131     C*
248000940131     C                   OTHER
248100940131     C                   EXSR      CTRD2
248200941205   B3C     *IN99         IFEQ      *OFF
248300940211     C                   MOVE      $LASTG        $GEST
248400941205   B4C                   SELECT
248500940202     C     $LASTG        WHENEQ    'S1'
248600940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
248700941205   B5C     $ORD          IFNE      D2ORD
248800940223     C                   MOVE      *ON           $INZS1
248900940302     C                   MOVE      *BLANK        W1POS
249000941205   E5C                   ENDIF
249100940202     C     $LASTG        WHENEQ    'D1'
249200940318     C* per ora niente
249300941205   E4C                   ENDSL
249400940131     C* definisco i nuovi valori di default
249500940201     C                   Z-ADD     D2LOP         $LOP
249600940131     C                   Z-ADD     D2ORD         $ORD
249700950322     C*
249800950322     C* aggiorno valori assunti
249900950328     C* se richiesto il salvataggio
250000950328   B4C     D2BSV         IFEQ      1
250100950322     C                   EXSR      AGGMPE
250200950322   E4C                   ENDIF
250300950322     C*
250400941205   E3C                   ENDIF
250500940131     C*
250600941205   E2C                   ENDSL
250700940131     C*
250800941205   E1C                   ENDIF
250900941205     C*
251000940131     C                   ENDSR
251100940223     C/EJECT
251200940131     C************************************************************
251300940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
251400940131     C************************************************************
251500940201     C     INZD2         BEGSR
251600940131     C*
251700940131     C                   CLEAR                   FMTD2
251800021010     C                   EVAL      D2BSVA = Normale
251900950328     C* se sono entrato con chiave univoca
252000950328     C* impedisco il salvataggio valori assunti
252100950606     C     $Unique       IFEQ      *ON
252200021010     C                   EVAL      D2BSVA = Protetto
252300950328     C                   ENDIF
252400940201     C                   MOVEL     $LOP          D2LOP
252500940831     C                   MOVE      $ORD          D2ORD
252600940131     C*
252700940131     C                   ENDSR
252800940309     C/EJECT
252900940309     C************************************************************
253000940309     C* GESTIONE F03 VIDEO D2
253100940309     C************************************************************
253200940309     C     F03D2         BEGSR
253300940309     C*
253400940309     C                   MOVE      *ON           $FINE
253500940325     C* fine programma
253600940325     C                   EXSR      ENDPGM
253700940309     C*
253800940309     C                   ENDSR
253900940309     C/EJECT
254000940309     C************************************************************
254100940309     C* GESTIONE F12 VIDEO D2
254200940309     C************************************************************
254300940309     C     F12D2         BEGSR
254400940309     C*
254500940309     C                   MOVE      $LASTG        $GEST
254600940309     C*
254700940309     C                   ENDSR
254800940916     C/EJECT
254900940916     C************************************************************
255000940916     C* GESTIONE F24 VIDEO D2
255100940916     C************************************************************
255200940916     C     F24D2         BEGSR
255300940916     C*
255400940916     C                   MOVEA     $FX3          $FX
255500940916     C                   MOVEA     $FC3          $FC
255600940916     C                   MOVEA     $FR3          $FR
255700940916     C                   Z-ADD     $ULKD2        $ULK
255800950726     C                   Z-ADD     $D2L01        $L01
255900950726     C                   Z-ADD     $D2L02        $L02
256000940916     C                   EXSR      RIEKEY
256100940916     C                   Z-ADD     $ULK          $ULKD2            3 0
256200940916     C                   MOVEA     $FC           $FC3
256300940916     C                   MOVEL     $KEY1         Z3KE1
256400060224     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
256500940916     C                   WRITE     FMTZ3
256600940916     C*
256700940916     C                   ENDSR
256800940223     C/EJECT
256900940131     C************************************************************
257000940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
257100940131     C************************************************************
257200940131     C     CTRD2         BEGSR
257300940131     C*
257400940131     C                   SETOFF                                       99
257500940131     C*
257600940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
257700940201     C     D2LOP         IFEQ      0
257800940131     C                   SETON                                          5099
257900941027     C                   ENDIF
258000940131     C*
258100940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
258200940131     C     D2ORD         IFEQ      0
258300940131     C                   SETON                                          5199
258400941027     C                   ENDIF
258500070927     C*
258600940131     C                   ENDSR
258700941205     C/EJECT
258800941205     C************************************************************
258900941205     C* CONTROLLO ABILITAZIONE TASTI
259000941205     C* INPUT
259100941205     C* $TASTO =Tasto funzione premuto
259200941205     C* $FC = Tasti abilitati
259300941205     C* OUTPUT
259400941205     C* IN99 = ERrore
259500941205     C************************************************************
259600941205     C     CTRKEY        BEGSR
259700941205     C*
259800951005     C                   SETOFF                                       99
259900951005     C*
260000941205     C                   SELECT
260100941205     C     $TASTO        WHENEQ    F01
260200960202     C                   z-add     01            posfun
260300941205     C     $FC(1)        IFEQ      '0'
260400951005     C                   SETON                                        9997
260500941205     C                   ENDIF
260600941205     C     $TASTO        WHENEQ    F02
260700960202     C                   z-add     02            posfun
260800941205     C     $FC(2)        IFEQ      '0'
260900951005     C                   SETON                                        9997
261000941205     C                   ENDIF
261100941205     C     $TASTO        WHENEQ    F03
261200960202     C                   z-add     03            posfun
261300941205     C     $FC(3)        IFEQ      '0'
261400951005     C                   SETON                                        9997
261500941205     C                   ENDIF
261600941205     C     $TASTO        WHENEQ    F04
261700960202     C                   z-add     04            posfun
261800941205     C     $FC(4)        IFEQ      '0'
261900951005     C                   SETON                                        9997
262000941205     C                   ENDIF
262100941205     C     $TASTO        WHENEQ    F05
262200960202     C                   z-add     05            posfun
262300941205     C     $FC(5)        IFEQ      '0'
262400951005     C                   SETON                                        9997
262500941205     C                   ENDIF
262600941205     C     $TASTO        WHENEQ    F06
262700960202     C                   z-add     06            posfun
262800941205     C     $FC(6)        IFEQ      '0'
262900951005     C                   SETON                                        9997
263000941205     C                   ENDIF
263100941205     C     $TASTO        WHENEQ    F07
263200960202     C                   z-add     07            posfun
263300941205     C     $FC(7)        IFEQ      '0'
263400951005     C                   SETON                                        9997
263500941205     C                   ENDIF
263600941205     C     $TASTO        WHENEQ    F08
263700960202     C                   z-add     08            posfun
263800941205     C     $FC(8)        IFEQ      '0'
263900951005     C                   SETON                                        9997
264000941205     C                   ENDIF
264100941205     C     $TASTO        WHENEQ    F09
264200960202     C                   z-add     09            posfun
264300941205     C     $FC(09)       IFEQ      '0'
264400951005     C                   SETON                                        9997
264500941205     C                   ENDIF
264600941205     C     $TASTO        WHENEQ    F10
264700960202     C                   z-add     10            posfun
264800941205     C     $FC(10)       IFEQ      '0'
264900951005     C                   SETON                                        9997
265000941205     C                   ENDIF
265100941205     C     $TASTO        WHENEQ    F11
265200960202     C                   z-add     11            posfun
265300941205     C     $FC(11)       IFEQ      '0'
265400951005     C                   SETON                                        9997
265500941205     C                   ENDIF
265600941205     C     $TASTO        WHENEQ    F12
265700960202     C                   z-add     12            posfun
265800941205     C     $FC(12)       IFEQ      '0'
265900951005     C                   SETON                                        9997
266000941205     C                   ENDIF
266100941205     C     $TASTO        WHENEQ    F13
266200960202     C                   z-add     13            posfun
266300941205     C     $FC(13)       IFEQ      '0'
266400951005     C                   SETON                                        9997
266500941205     C                   ENDIF
266600941205     C     $TASTO        WHENEQ    F14
266700960202     C                   z-add     14            posfun
266800941205     C     $FC(14)       IFEQ      '0'
266900951005     C                   SETON                                        9997
267000941205     C                   ENDIF
267100941205     C     $TASTO        WHENEQ    F15
267200960202     C                   z-add     15            posfun
267300941205     C     $FC(15)       IFEQ      '0'
267400951005     C                   SETON                                        9997
267500941205     C                   ENDIF
267600941205     C     $TASTO        WHENEQ    F16
267700960202     C                   z-add     16            posfun
267800941205     C     $FC(16)       IFEQ      '0'
267900951005     C                   SETON                                        9997
268000941205     C                   ENDIF
268100941205     C     $TASTO        WHENEQ    F17
268200960202     C                   z-add     17            posfun
268300941205     C     $FC(17)       IFEQ      '0'
268400951005     C                   SETON                                        9997
268500941205     C                   ENDIF
268600941205     C     $TASTO        WHENEQ    F18
268700960202     C                   z-add     18            posfun
268800941205     C     $FC(18)       IFEQ      '0'
268900951005     C                   SETON                                        9997
269000941205     C                   ENDIF
269100941205     C     $TASTO        WHENEQ    F19
269200960202     C                   z-add     19            posfun
269300941205     C     $FC(19)       IFEQ      '0'
269400951005     C                   SETON                                        9997
269500941205     C                   ENDIF
269600941205     C     $TASTO        WHENEQ    F20
269700960202     C                   z-add     20            posfun
269800941205     C     $FC(20)       IFEQ      '0'
269900951005     C                   SETON                                        9997
270000941205     C                   ENDIF
270100941205     C     $TASTO        WHENEQ    F21
270200960202     C                   z-add     21            posfun
270300941205     C     $FC(21)       IFEQ      '0'
270400951005     C                   SETON                                        9997
270500941205     C                   ENDIF
270600941205     C     $TASTO        WHENEQ    F22
270700960202     C                   z-add     22            posfun
270800941205     C     $FC(22)       IFEQ      '0'
270900951005     C                   SETON                                        9997
271000941205     C                   ENDIF
271100941205     C     $TASTO        WHENEQ    F23
271200960202     C                   z-add     23            posfun
271300941205     C     $FC(23)       IFEQ      '0'
271400951005     C                   SETON                                        9997
271500941205     C                   ENDIF
271600941205     C     $TASTO        WHENEQ    F24
271700960202     C                   z-add     24            posfun
271800941205     C     $FC(24)       IFEQ      '0'
271900951005     C                   SETON                                        9997
272000941205     C                   ENDIF
272100941205     C                   ENDSL
272200941205     C*
272300941205     C                   ENDSR
272400940223     C/EJECT
272500940201     C************************************************************
272600940201     C* AGGIORNAMENTO VALORI ASSUNTI
272700940201     C************************************************************
272800940201     C     AGGMPE        BEGSR
272900940201     C*
273000950524     C* preparo i parametri di chiamata al driver
273100950524     C                   CLEAR                   XMPEDS
273200950524     C                   EVAL      TpChiamata = '03'
273300950524     C                   EVAL      NChiavi = 5
273400950524     C                   EVAL      VLogica = 2
273500950524     C*
273600950524     C* preparo le chiavi per la chiamata al driver
273700950524     C                   CLEAR                   XMPEDS1
273800950524     C                   MOVEL     XSCSOC        DSMPESOC
273900950524     C                   MOVEL     DSPGM         DSMPEPGM
274000950524     C                   MOVEL     KNMUS         DSMPEPRF
274100941013     C*
274200950322     C* Si memorizzano i dati di default a seconda delle modalità con
274300950322     C* cui il pgm è usato
274400950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
274500950322     C* MPETIP = 'PV'  pgm richiamato da menù
274600950322     C* MPETIP = 'PB'  x lancio Batch
274700950322     C*
274800941013     C                   SELECT
274900090310    >C     OPZ988        WHENEQ    '01'
275000950524     C                   MOVEL     'PX'          DSMPETIP
275100941013     C                   OTHER
275200950524     C                   MOVEL     'PV'          DSMPETIP
275300941013     C                   ENDSL
275400950524     C* dati
275500950524     C                   MOVEL     DSMPE         DSMPEPAR
275600950524     C*
275700950524     C                   CALLB     'XMPE'
275800950524     C                   PARM                    XMPEDS
275900950524     C                   PARM                    XMPEDS1
276000940201     C*
276100940201     C                   ENDSR
276200940223     C/EJECT
276300940131     C************************************************************
276400940131     C* OPERAZIONI INIZIALI
276500940131     C************************************************************
276600940131     C     *INZSR        BEGSR
276700061108     C     *DMY          Move      Udate         ZDateISO
276800061108     C     *JobRun       Move      ZDateISO      ZDate             6 0
276900940131     C*
277000940127     C* Reperimento parametri
277100940127     C*
277200940117     C     *ENTRY        PLIST
277300940117     C                   PARM                    KPJBA
277400951206     C*
277500951206     C* Reperimento dati società
277600951206     C*
277700061108     C                   MOVEL     'SOC001'      TIPXSC
277800970424     C                   MOVEL     *BLANK        SOCXSC
277900951206     C                   EXSR      REPSOC
278000960409     C*
278100960409     C* Controlli dati società
278200960409     C*
278300960409     C     RTNXSC        IFNE      '1'
278400061108     C                   MOVEL     XSOCDS        SOC001
278500960409     C                   ELSE
278600960409     C                   MOVE      *ON           $FINE
278700090310    >C                   MOVE      '1'           Err988
278800960409     C                   EXSR      ENDPGM
278900960409     C                   ENDIF
279000061115     C*
279100090310     c                   open      ndc988d                              13
279200061115     c* emetto messaggio per video non a 132
279300061115     c                   if        *in13=*on
279400061115     c                   move      'COS2651'     XmsgCd1
279500061115     C                   CALL      'XMSGR'
279600061115     C                   PARM                    XMSGDS
279700061115     C                   PARM                    XMStxt          135
279800061115     C                   PARM                    XMSGinz           1
279900061115     C                   PARM                    XMSGCd1           7
280000061115     C                   PARM                    XMSGCd2           7
280100061115     C                   PARM                    XMSGCd3           7
280200061115     C                   PARM                    XMSGSn1           7
280300061115     C                   PARM                    XMSGCdTxT        63
280400061115     C                   PARM                    MSGCdTit         14
280500061115     C                   PARM                    MSGClPrtf         1
280600061115     C                   PARM                    VarMsg1         100
280700061115     C                   PARM                    VarMsg2         100
280800061115     C                   PARM                    VarMsg3         100
280900090310    >C                   MOVE      '1'           Err988
281000061115     c                   exsr      endpgm
281100061115     c                   endif
281200950726     C*
281300950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
281400070927     C                   EVAL      $D1L01 = %Size(Z1Ke)
281500061108     C                   EVAL      $D1L02 = %Size(Z1Ke2)
281600070927     C                   EVAL      $S1L01 = %Size(Z2Ke)
281700070927     C                   EVAL      $S1L02 = %Size(Z2Ke2)
281800950926     C                   EVAL      $C1Op1 = %Size(C1Opz1)
281900950926     C                   EVAL      $C1Op2 = %Size(C1Opz2)
282000950906     C                   EVAL      $D2L01 = %Size(Z3Ke1)
282100950906     C                   EVAL      $D2L02 = 0
282200090316     C*
282300090316     c* reperimento dati parametro modulo solleciti
282400090316     C                   movel     'NDCL1S'      xpapar
282500090316     C                   movel     *blank        xparic
282600090316     C                   CALLb     'XPAR'
282700090316     C                   PARM                    xscsoc
282800090316     C                   PARM                    XPAPAR            8
282900090316     C                   PARM                    XPAOUT
283000090316     C                   PARM                    XPAERR            1
283100090316     C                   PARM                    XPARIC            1
283200090316     C     XPAERR        IFEQ      '0'
283300090316     C                   movel(p)  XpaOut        Anp057Ds
283400100426c2156c                   move      posattr057    posiz             2 0
283500090316     c                   else
283600090316     c                   seton                                        lr
283700090316     c                   return
283800090316     C                   endif
283900940225     C*
284000940223     C* Reperimento opzioni
284100940127     C*
284200940128     C                   EXSR      REPOPZ
284300950406     C*
284400940506     C* Reperimento tasti di funzione
284500940506     C*
284600940506     C                   EXSR      REPKEY
284700960117     C* Chiamata a routine per il reperimento delle opzioni
284800960117     C* personalizzate
284900960117     C                   CAllb     'X46USREXT'
285000960117     C                   PARM                    $OP1
285100960117     C                   PARM                    $TX1
285200960117     C                   PARM                    $FL1
285300960117     C                   PARM                    $PGO1
285400960117     C                   PARM      $dimOp        NumOpz
285500960202     C                   PARM                    $FX2
285600960202     C                   PARM                    $FC2
285700960202     C                   PARM                    $PGF2
285800960117     C                   PARM                    DsPgm
285900960117     C                   PARM                    xsccut
286000960117     C                   PARM                    xscgrp
286100960117     C*
286200960118     C                   Z-add     1             OPZLEN
286300960118     C     *blank        Lookup    $TX1(opzlen)                           21
286400960117     C                   if        *in21 = *on
286500960117     C                   sub       1             OPZLEN
286600960117     C                   else
286700960117     C                   Z-add     $DimOp        OPZLEN
286800960117     C                   endif
286900940127     C*
287000940117     C                   ENDSR
287100940223     C/EJECT
287200940223     C************************************************************
287300940223     C* INIZIALIZZAZIONE VARIABILI
287400940223     C************************************************************
287500940223     C     INZVAR        BEGSR
287600941215     C*
287700941215     C* Dati da PGM chiamante
287800941215     C*
287900090312     c                   movel     kpjbu         ndc988ds
288000951206     C*
288100951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
288200951206     C* reperimento dati società
288300061108     C                   MOVEL     'SOC001'      TIPXSC
288400070927    >C                   MOVEL     *blanks       SOCXSC
288500951122     C                   EXSR      REPSOC
288600951010     C     RTNXSC        IFNE      '1'
288700061108     C                   MOVEL     XSOCDS        SOC001
288800951010     C                   ELSE
288900951010     C                   MOVE      *ON           $FINE
289000090310    >C                   MOVE      '1'           Err988
289100951010     C                   EXSR      ENDPGM
289200951010     C                   ENDIF
289300061108     C* visualizzo il sfl troncato
289400061108     C                   Eval      c1mode=*on
289500941026     C*
289600941026     C* Reperimento valori default utente
289700941026     C*
289800941026     C                   EXSR      REPDFT
289900940506     C*
290000940506     C* Definizione delle opzioni utilizzabili a pgm
290100940506     C*
290200940506     C                   EXSR      DEFOPZ
290300950330     C*
290400940223     C* Variabili per gestione videate
290500940223     C*
290600940223     C                   MOVE      *OFF          $FINE                          fine pgm
290700940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
290800940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
290900940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
291000100423c2156C                   MOVE      *OFF          $INZD4                         inizializzione D2
291100100423c2156C                   MOVE      *OFF          $FINED4           1            fine pgm
291200950606     C                   MOVE      *OFF          $EndFile                       memo fine file
291300940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
291400940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
291500940509     C                   MOVE      *HIVAL        $LASTV
291600090511     C                   Z-ADD     0             $RigheVuote                    piede da emett. D1
291700940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
291800940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
291900940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
292000960605     C* azzero la posizione del cursore
292100960605     C                   Z-ADD     0             H1RIGA
292200960605     C                   Z-ADD     0             H1COLO
292300940223     C*
292400940318     C                   MOVE      *ON           $INZD1
292500940318     C                   MOVE      *ON           $INZS1
292600100423c2156C                   MOVE      *ON           $INZD4
292700940831     C*
292800940831     C* se pgm richiamato con chiave univoca impostata,
292900940831     C* forzo la emissione della lista e imposto l'ordinamento
293000090312    >C*    Opz988        IFNE      *BLANK
293100090312     C*                  MOVE      *ON           $Unique
293200090312     C*                  Z-ADD     2             $LOP
293300090312     C*                  Z-ADD     99            $ORD
293400090312     C*                  ELSE
293500090312     C                   MOVE      *OFF          $Unique
293600090312     C*                  ENDIF
293700950330     C*
293800950330     C* Variabili appoggio
293900950330     C*
294000950330     C                   Z-ADD     SFLPAG        WPAG
294100950330     C                   MOVEL     *OFF          WOPR
294200950515     C                   CLEAR                   W1POS
294300941104     C*
294400941104     C* definisco qual'è la prima videata
294500950410     C* (se la prima videata risulta la lista, devo comunque caricare
294600950410     C*  la videata di parzializzazione e gestire la definizione
294700950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
294800950410     C*  passati dei dati, può variare il caricamento)
294900940223     C                   SELECT
295000940223     C     $LOP          WHENEQ    1
295100940223     C                   MOVE      'D1'          $GEST
295200940223     C     $LOP          WHENEQ    2
295300940223     C                   MOVE      'S1'          $GEST
295400941104     C                   EXSR      INZD1
295500941104     C                   MOVE      *OFF          $INZD1
295600950410     C                   EXSR      DEFORD
295700941104     C                   ENDSL
295800940223     C*
295900940223     C* Valorizzazione campi univoci testate
296000940223     C*
296100940223     C                   CLEAR                   FMTT1
296200940223     C                   MOVEL     KNSIF         NOMSIF
296300941109     C                   MOVEL     XSCDSI        NOMDIT
296400941027     C                   MOVEL     DSPGM         NOMPGM
296500061113     C                   ENDSR
296600061113     c*
296700941209     C************************************************************
296800941209     C* REPERIMENTO DATI SOCIETA'
296900941209     C************************************************************
297000941209     C     REPSOC        BEGSR
297100941209     C*
297200950522     C                   CALLB     'XSOC'
297300941209     C                   PARM                    TIPXSC            6
297400941209     C                   PARM                    SOCXSC            3
297500941209     C                   PARM                    CDSXSC            9 0
297600941209     C                   PARM                    MODXSC            3
297700941209     C                   PARM      *BLANKS       RTNXSC            1
297800941209     C                   PARM                    XSOCDS
297900941209     C                   PARM                    KPJBA
298000941209     C*
298100941209     C                   ENDSR
298200940223     C/EJECT
298300940127     C************************************************************
298400940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
298500940127     C************************************************************
298600940131     C     REPDFT        BEGSR
298700940127     C*
298800950524     C* preparo i parametri di chiamata al driver
298900950524     C                   CLEAR                   XMPEDS
299000950524     C                   EVAL      TpChiamata = '01'
299100950524     C                   EVAL      NChiavi = 5
299200950524     C                   EVAL      VLogica = 2
299300950524     C*
299400950524     C* preparo le chiavi per la chiamata al driver
299500950524     C                   CLEAR                   XMPEDS1
299600950524     C                   MOVEL     XSCSOC        DSMPESOC
299700950524     C                   MOVEL     DSPGM         DSMPEPGM
299800950524     C                   MOVEL     KNMUS         DSMPEPRF
299900950524     C* Si reperiscono i dati di default a seconda delle modalità con
300000950524     C* cui il pgm è usato
300100950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
300200950524     C* MPETIP = 'PV'  pgm richiamato da menù
300300950524     C* MPETIP = 'PB'  x lancio Batch
300400941013     C                   SELECT
300500090310    >C     OPZ988        WHENEQ    '01'
300600950524     C                   MOVEL     'PX'          DSMPETIP
300700941013     C                   OTHER
300800950524     C                   MOVEL     'PV'          DSMPETIP
300900941013     C                   ENDSL
301000950524     C*
301100950524     C                   CALLB     'XMPE'
301200950524     C                   PARM                    XMPEDS
301300950524     C                   PARM                    XMPEDS1
301400950524     C*
301500950524     C     Risultato     IFEQ      '0'
301600950524     C                   MOVEL     DSMPEPAR      DSMPE
301700940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
301800940131     C* i suoi valori di default vengono decisi a pgm
301900940131     C                   ELSE
302000070928     C                   Z-ADD     2             $LOP
302100070928     C                   Z-ADD     1             $ORD
302200941027     C                   ENDIF
302300940131     C*
302400940131     C                   ENDSR
302500940223     C/EJECT
302600940131     C************************************************************
302700940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
302800940131     C************************************************************
302900940225     C     DEFOPZ        BEGSR
303000940131     C*
303100940225     C* Questa routine deve valorizzare la schiera $FL1
303200940225     C* con S o N a seconda della utilizzabilità o meno
303300940225     C* dell'opzione correlata
303400940225     C*
303500950301     C* Inizialmente si considerano tutte le opzioni come valide
303600940317     C     1             DO        $DIMOP        X
303700061108     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
303800960117     C                   If        $TX1(X)  <> *Blank
303900940225     C                   MOVE      'S'           $FL1(X)
304000960117     C                   Else
304100960117     C                   Leave
304200960117     C                   EndIf
304300941027     C                   ENDDO
304400940715     C*
304500940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
304600941018     C* con 1 o 0 a seconda della utilizzabilità o meno
304700940715     C* del tasto funzione correlato
304800940715     C*
304900940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
305000940715     C* di compilazione
305100940715     C                   RESET                   $FC1
305200940715     C                   RESET                   $FR1
305300940715     C                   RESET                   $FC2
305400940715     C                   RESET                   $FR2
305500940919     C                   RESET                   $FC3
305600940919     C                   RESET                   $FR3
305700100423C2156C                   RESET                   $FC4
305800100423C2156C                   RESET                   $FR4
305900940225     C*
306000940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
306100940225     C* invalido alcune opzioni
306200940225     C                   SELECT
306300940225     C* Se pgm richiamato in ricerca disattivo :
306400090310    >C     OPZ988        WHENEQ    '01'
306500940603     C* F3=Fine
306600940603     C                   Z-ADD     3             X
306700941216     C                   MOVE      '0'           $FC1(X)
306800941216     C                   MOVE      '0'           $FC2(X)
306900941216     C                   MOVE      '0'           $FC3(X)
307000950116     C*
307100950301     C*
307200940225     C* Se pgm richiamato in interrogazione disattivo :
307300090310    >C     OPZ988        WHENEQ    '05'
307400940307     C* F10=Immissione
307500940615     C                   Z-ADD     10            X
307600950301     C                   MOVE      '0'           $FC1(X)
307700950301     C                   MOVE      '0'           $FC2(X)
307800940225     C* 1=Selezione
307900940225     C                   Z-ADD     1             X
308000940225     C     01            LOOKUP    $OP1(X)                                21
308100940225     C     *IN21         IFEQ      *ON
308200940225     C                   MOVE      'N'           $FL1(X)
308300940225     C                   ENDIF
308400940225     C* 2=Modifica
308500940225     C                   Z-ADD     1             X
308600940225     C     02            LOOKUP    $OP1(X)                                21
308700940225     C     *IN21         IFEQ      *ON
308800940225     C                   MOVE      'N'           $FL1(X)
308900940225     C                   ENDIF
309000940225     C* 3=Copia
309100940225     C                   Z-ADD     1             X
309200940225     C     03            LOOKUP    $OP1(X)                                21
309300940225     C     *IN21         IFEQ      *ON
309400940225     C                   MOVE      'N'           $FL1(X)
309500940225     C                   ENDIF
309600940225     C* 4=Annullamento
309700940225     C                   Z-ADD     1             X
309800940225     C     04            LOOKUP    $OP1(X)                                21
309900940225     C     *IN21         IFEQ      *ON
310000940225     C                   MOVE      'N'           $FL1(X)
310100940225     C                   ENDIF
310200940225     C*
310300090310    >C     OPZ988        WHENEQ    *BLANKS
310400940225     C* Se pgm non richiamato, disattivo subito :
310500941124     C* F8=Selezione seguente
310600941124     C                   Z-ADD     8             X
310700941206     C                   MOVE      '0'           $FC1(X)
310800941206     C                   MOVE      '0'           $FC2(X)
310900940225     C* 1=Selezione
311000940225     C                   Z-ADD     1             X
311100940225     C     01            LOOKUP    $OP1(X)                                21
311200940225     C     *IN21         IFEQ      *ON
311300940225     C                   MOVE      'N'           $FL1(X)
311400940225     C                   ENDIF
311500940225     C*
311600950301     C* In tutti gli altri casi, disattivo subito :
311700941228     C                   OTHER
311800941228     C* 1=Selezione
311900941228     C                   Z-ADD     1             X
312000941228     C     01            LOOKUP    $OP1(X)                                21
312100941228     C     *IN21         IFEQ      *ON
312200941228     C                   MOVE      'N'           $FL1(X)
312300941228     C                   ENDIF
312400941228     C*
312500940225     C                   ENDSL
312600941118     C*
312700941118     C* poi, disattivo a seconda delle autorizzazioni :
312800061108    >C*                  IF        AUT___ <> *on
312900061108    >C*                  Z-ADD     1             X
313000061108    >C*    XX            LOOKUP    $OP1(X)                                21
313100061108    >C*    *IN21         IFEQ      *ON
313200061108    >C*                  MOVE      'N'           $FL1(X)
313300061108    >C*                  ENDIF
313400061108    >C*                  ENDIF
313500940225     C*
313600940225     C                   ENDSR
313700940225     C/EJECT
313800940225     C************************************************************
313900940225     C* REPERIMENTO OPZIONI
314000940225     C************************************************************
314100940225     C     REPOPZ        BEGSR
314200940225     C*
314300940314     C     1             DO        $DIMOP        X
314400940128     C*
314500940223     C     $MS1(X)       IFNE      *BLANK
314600940128     C*
314700940223     C                   MOVEL     $MS1(X)       IDMSG
314800950522     C                   CALLB     'XRTVM'
314900940201     C                   PARM                    IDMSG            27
315000940201     C                   PARM                    TXTMSG          100
315100940201     C                   PARM                    ERRMSG            1
315200940128     C*
315300940128     C     ERRMSG        IFNE      *ON
315400940223     C                   MOVEL     TXTMSG        $TX1(X)
315500940128     C                   ELSE
315600940223     C                   MOVEL     *ALL'?'       $TX1(X)
315700941027     C                   ENDIF
315800940128     C*
315900941027     C                   ENDIF
316000940128     C*
316100941027     C                   ENDDO
316200940128     C*
316300940128     C                   ENDSR
316400940506     C/EJECT
316500940506     C************************************************************
316600940506     C* REPERIMENTO TASTI DI FUNZIONE
316700940506     C************************************************************
316800940506     C     REPKEY        BEGSR
316900940506     C                   MOVEA     $FM1          $FM
317000950721     C                   MOVEA     $FC1          $FC
317100940506     C                   EXSR      REPKKK
317200940506     C                   MOVEA     $FX           $FX1
317300940506     C*
317400940506     C                   MOVEA     $FM2          $FM
317500950721     C                   MOVEA     $FC2          $FC
317600940506     C                   EXSR      REPKKK
317700940506     C                   MOVEA     $FX           $FX2
317800940919     C*
317900940919     C                   MOVEA     $FM3          $FM
318000950721     C                   MOVEA     $FC3          $FC
318100940919     C                   EXSR      REPKKK
318200940919     C                   MOVEA     $FX           $FX3
318300100423     c*
318400100423c2156C                   MOVEA     $FM4          $FM
318500100423  "  C                   MOVEA     $FC4          $FC
318600100423  "  C                   EXSR      REPKKK
318700100423c2156C                   MOVEA     $FX           $FX4
318800940919     C                   ENDSR
318900940506     C/EJECT
319000940506     C************************************************************
319100940506     C* REPERIMENTO TASTI DI FUNZIONE
319200940506     C************************************************************
319300940506     C     REPKKK        BEGSR
319400940506     C*
319500940506     C                   CLEAR                   $FX
319600940506     C     1             DO        24            X
319700940506     C*
319800940506     C     $FM(X)        IFNE      *BLANK
319900950721     C     $Fc(X)        andne     '0'
320000940506     C*
320100940506     C                   MOVEL     $FM(X)        IDMSG
320200950522     C                   CALLB     'XRTVM'
320300940506     C                   PARM                    IDMSG            27
320400940506     C                   PARM                    TXTMSG          100
320500940506     C                   PARM                    ERRMSG            1
320600940506     C*
320700940506     C     ERRMSG        IFNE      *ON
320800940506     C                   MOVEL     TXTMSG        $FX(X)
320900940506     C                   ELSE
321000940506     C                   MOVEL     *ALL' '       $FX(X)
321100941027     C                   ENDIF
321200940506     C*
321300941027     C                   ENDIF
321400940506     C*
321500941027     C                   ENDDO
321600940506     C*
321700940506     C                   ENDSR
321800940223     C/EJECT
321900940128     C************************************************************
322000950606     C* DEFINIZIONE KLIST
322100940128     C************************************************************
322200950606     C     DefKlist      BEGSR
322300940128     C*
322400940223     C* klist
322500090312     C* PLIST per NDVASOG
322600090312     C     PDVASOG       PLIST
322700090312     C                   PARM                    DVARIC
322800090312     C                   PARM                    DVASOCIETA
322900090312     C                   PARM                    DVAUNITA
323000090312     C                   PARM                    DVASTRUTT
323100090312     C                   PARM                    DVADTRIF
323200090312     C                   PARM                    DVALENOUT
323300090312     C* Se si accede con il codice cli/for impostare i campi seguenti
323400090312     C* se si accede con il soggetto ometterli e impostare gli altri
323500090312     C                   PARM                    DVASNATURA
323600090312     C                   PARM                    DVACODICE
323700090312     C                   PARM                    DVALINEAV
323800090312     C                   PARM                    DVAFILIALE
323900090312     C                   PARM                    *omit
324000090312     C                   PARM                    DVATPIND
324100090312     C                   PARM                    DVACDIND
324200090312     C                   PARM                    DVAERRORE
324300090312     C                   PARM                    DVAOUTPUT
324400090316     C* NDIMP01L
324500100423     C     K07IMP01      KLIST
324600090316     C                   KFLD                    IMPSocieta
324700090316     C                   KFLD                    IMPKcc
324800090316     C                   KFLD                    IMPKsc
324900090316     C                   KFLD                    IMPDtPar
325000090316     C                   KFLD                    IMPNrPar
325100090316     C                   KFLD                    IMPSerPar
325200090316     C                   KFLD                    IMPDtSca
325300100423     C*
325400100423c2156c     K08IMP01      KLIST
325500100423  "  C                   KFLD                    IMPSocieta
325600100423  "  C                   KFLD                    IMPKcc
325700100423  "  C                   KFLD                    IMPKsc
325800100423  "  C                   KFLD                    IMPDtPar
325900100423  "  C                   KFLD                    IMPNrPar
326000100423  "  C                   KFLD                    IMPSerPar
326100100423  "  C                   KFLD                    IMPDtSca
326200100423c2156C                   KFLD                    IMPProgr
326300090316     C* NDPPA01L
326400090316     C     K07PPA01      KLIST
326500090316     C                   KFLD                    PPASocieta
326600090316     C                   KFLD                    PPACtb
326700090316     C                   KFLD                    PPAKcc
326800090316     C                   KFLD                    PPAKsc
326900090316     C                   KFLD                    PPADtPar
327000090316     C                   KFLD                    PPANrPar
327100090316     C                   KFLD                    PPASerPar
327200090316     C*
327300090316     C     K03PSc01      KLIST
327400090316     C                   KFLD                    PSCSys
327500090316     C                   KFLD                    PSCNrasInt
327600090316     C                   KFLD                    PSCDtSca
327700090316     C* NDSCD01L
327800090316     C     K09Scd01      KLIST
327900090316     C                   KFLD                    ScdSocieta
328000090316     C                   KFLD                    ScdCtb
328100090316     C                   KFLD                    ScdTpScad
328200090316     C                   KFLD                    ScdKcc
328300090316     C                   KFLD                    SCdKsc
328400090316     C                   KFLD                    SCdDtPar
328500090316     C                   KFLD                    SCdNrPar
328600090316     C                   KFLD                    SCdSerPar
328700090316     C                   KFLD                    SCdDtSca
328800090316     c                   eval      ScdCtb = 'CG'
328900090316     c                   eval      ScdTpScad='0'
329000100426c2156C* ndRCA04l
329100100426  " >C     K09Rca04      KLIST
329200100426  " >C                   KFLD                    RcaSocieta
329300100426  " >C                   KFLD                    RcaPosiz
329400100426  " >C                   KFLD                    RcaKCC
329500100426  " >C                   KFLD                    RcaKSC
329600100426  " >C                   KFLD                    RcaDtpar
329700100426  " >C                   KFLD                    RcaNrpar
329800100426  " >C                   KFLD                    RcaSerPar
329900100426  " >C                   KFLD                    RcaDtSca
330000100426  " >C                   KFLD                    RcaDtAtt
330100100426  " >C     K08Rca04      KLIST
330200100426  " >C                   KFLD                    RcaSocieta
330300100426  " >C                   KFLD                    RcaPosiz
330400100426  " >C                   KFLD                    RcaKCC
330500100426  " >C                   KFLD                    RcaKSC
330600100426  " >C                   KFLD                    RcaDtpar
330700100426  " >C                   KFLD                    RcaNrpar
330800100426  " >C                   KFLD                    RcaSerPar
330900100426c2156C                   KFLD                    RcaDtSca
331000131104     C     K08Rca01      KLIST
331100131104     C                   KFLD                    RcaSocieta
331200131104     C                   KFLD                    RcaPosiz
331300131104     C                   KFLD                    RcaKCC
331400131104     C                   KFLD                    RcaKSC
331500131104     C                   KFLD                    RcaDtpar
331600131104     C                   KFLD                    RcaNrpar
331700131104     C                   KFLD                    RcaSerPar
331800131104     C                   KFLD                    RcaDtSca
331900100524c2156C* ndRCA05l
332000100525  " >C     K10Rca05      KLIST
332100100524  " >C                   KFLD                    RcaSocieta
332200100524  " >C                   KFLD                    RcaPosiz
332300100524  " >C                   KFLD                    RcaKCC
332400100524  " >C                   KFLD                    RcaKSC
332500100524  " >C                   KFLD                    RcaDtpar
332600100524  " >C                   KFLD                    RcaNrpar
332700100524  " >C                   KFLD                    RcaSerPar
332800100524  " >C                   KFLD                    RcaDtSca
332900100524c2156C                   KFLD                    RcaProgrIm
333000100525c2156C                   KFLD                    RcaAzione
333100090316     C                   ENDSR
333200090316     c*
333300100423c2156C************************************************************
333400100423     C* GESTIONE tassi per scaglioni
333500100423     C************************************************************
333600100423c2156C     GESD4         BEGSR
333700100423     C* inizializzazione videata
333800100423     C     $INZD4        IFEQ      *ON
333900100423     C                   EXSR      INZD4
334000100423     C                   MOVE      *OFF          $INZD4
334100100423     C                   ENDIF
334200100423     C* emissione piede videata se proveniente da altra
334300100423     C* salvataggio valore formato di provenienza
334400100423     C     $LASTV        IFNE      'D4'
334500100423     C* salvo stato F17 per sapere cosa fare in caso di F12
334600100423     C                   MOVE      *INKR         WINKR             1
334700100423     C*******            WRITE     FMTT1
334800100423     C                   EXSR      GESPIE
334900100423     C                   MOVE      $LASTV        $LASTG
335000100423     C                   MOVE      'D4'          $LASTV
335100100423     C                   ENDIF
335200100423     C*
335300100423     C* se esistono errori sulla videata
335400100423     C* emetto la write del formato a indicatori spenti per vedere
335500100423     C* le eventuali decodifiche
335600100423     C     *IN99         IFEQ      *ON
335700100423     C                   MOVEA     *IN           WIN
335800100423     C                   MOVE      *ALL'0'       IN5098           49
335900100423     C                   MOVEA     IN5098        *IN(50)
336000100423     C                   WRITE     FMTD4
336100100423     C                   MOVEA     WIN           *IN
336200100423     C                   ENDIF
336300100423     C*
336400100423     C                   EXFMT     FMTD4
336500100423     C* controllo abilitazione tasti funzione
336600100423     C*
336700100423     C                   EXSR      CTRKD4
336800100423     C*
336900100423     C     *IN99         IFEQ      *OFF
337000100423     C*
337100100423     C                   SELECT
337200100423     C* F3=Fine
337300100423     C     $TASTO        WHENEQ    F03
337400100423     C                   EXSR      F03D1
337500100426     C* F4=Ricerca
337600100426     C     $TASTO        WHENEQ    F04
337700100426     C                   EXSR      SEARCH
337800100423     C* F5=Refresh
337900100423     C     $TASTO        WHENEQ    F05
338000100423     C                   EXSR      F05D4
338100100423     C* F12=Ritorno
338200100423     C     $TASTO        WHENEQ    F12
338300100423     C                   EXSR      F12D4
338400100423     C* F24=Altri tasti
338500100423     C     $TASTO        WHENEQ    F24
338600100423     C                   EXSR      F24D4
338700100423     C*
338800100423     C                   OTHER
338900100423     C                   EXSR      CTRD4
339000100423     c*
339100100423   B4C     $TASTO        IFEQ      F06
339200100423     C     *IN99         ANDEQ     *OFF
339300100423     c                   exsr      AggAnaD4
339400100427     C                   EXSR      GESAGG
339500100423     c                   move      *on           $FineD4
339600100427     c                   move      *on           $AggiornaI        1
339700100423   E6C                   ENDIF
339800100423     C*
339900100423     C                   ENDSL
340000100423     C*
340100100423     C                   ENDIF
340200100423c2156C                   ENDSR
340300100423     C/EJECT
340400100423c2156C************************************************************
340500100423     C* aggiornamento da videata d4
340600100423     C************************************************************
340700100423c2156C     AggAnaD4      BEGSR
340800100423     C                   EVAL      s1esito  =  d4esito
340900100423     C                   EVAL      s1nota   =  d4nota
341000100423     C                   EVAL      s1impimp =  d4impimp
341100100525     C                   EVAL      s1dtimp  =  d4dtimp
341200100423c2156c                   eval      h1progr  =  h4progr
341300100423     c*
341400100423     c                   exsr      AggImpegno
341500100423     C*
341600100423c2156C                   ENDSR
341700100423c2156C************************************************************
341800100426     C* INIZIALIZZAZIONE VIDEATA immissione impegno
341900100423     C************************************************************
342000100423c2156C     INZD4         BEGSR
342100100423     C*
342200100423     C                   CLEAR                   FMTD4
342300100423     C*
342400100423     c                   move      c1kcc         d4kcc
342500100423     c                   move      c1ksc         d4ksc
342600100423     c                   move      c1kscd        d4Descr
342700100426     C     *jobrun       Move      h1DtPar       D4DtPar
342800100426     C                   z-add     h1nrpar       D4NrPar
342900100423     c                   move      h1serpar      d4Serpar
343000100423     C                   Move      s1DtSca       D4DtSca
343100100423     c                   z-add     s1ImpScad     D4ImpScad
343200100423     c*
343300100423     C* reperisco ultimo progressivo della scadenza
343400100423     C                   EVAL      IMPSocieta = Societa988
343500100423     C                   EVAL      IMPKcc =   kcc988
343600100423     C                   EVAL      IMPKsc =   ksc988
343700100426     C                   move      h1dtpar       impdtpar
343800100426     C                   EVAL      IMPNrPar =  h1nrpar
343900100423     C                   EVAL      IMPserpar=  h1serpar
344000100423     C     *jobrun       move      s1dtsca       impdtsca
344100100423     c                   eval      ImpProgr =  *hival
344200100423     C     K08IMP01      setgt     NDIMP01L
344300100525     C     K07IMP01      readpe(N) NDIMP01L                               21
344400100423     c* lo trova sempre perchè il primo va inserito sul sfl
344500100423     c  N21impprogr      add       1             h4progr
344600100423     c*
344700100423     C                   ENDSR
344800100423c2156C/EJECT
344900100423c2156C************************************************************
345000100423     C* CONTROLLO VIDEATA x scaglioni
345100100423     C************************************************************
345200100423c2156C     CTRD4         BEGSR
345300100423     C*
345400100423     c                   setoff                                       99
345500100423     C*
345600100423     C* esito
345700100423     c                   clear                   d4esitod
345800100423     C                   MOVE      '1'           XTBRIC
345900100423     C                   MOVE      XSCSOC        XTBAZI
346000100423     C                   MOVE      *ZERO         XTBKEY
346100100423     C                   MOVE      '042'         XTBCOD
346200100423     C                   MOVE      d4esito       XTBKEY
346300100423     C                   CALLb     'XATB'
346400100423     C                   PARM                    XATBDS
346500100423     C     XTBERR        IFEQ      *Off
346600100423     C                   MOVEL     XTBUNI        ang042ds
346700100423     C                   MOVEL     DEs042        d4esitoD
346800100423     c                   else
346900100423     c                   seton                                        6099
347000100423     c                   endif
347100100423     C*
347200100423     C* data impegno
347300100423     C                   If        d4dtimp <> 0
347400100423     c* controllo validità formale
347500100423     C     *JobRun       Test(D)                 d4dtimp                15
347600100423     C                   If        *in15 = *on
347700100423     c                   seton                                        6199
347800100423     c                   endif
347900100427     c                   else
348000100427     c                   seton                                        6199
348100100423     c                   endif
348200100423     C*
348300100426c21563* controlli sulla fase delle attività
348400100426  "  c                   if        impegn057=*on
348500100426  "  c                   exsr      CtrFase
348600100426c2156c                   endif
348700100423     C*
348800100423c2156C                   ENDSR
348900131203brt  C************************************************************
349000131203brt  C* CONTROLLO sulla fase delle attività
349100131203brt  C************************************************************
349200131203     C     CtrAggFase    BEGSR
349300131203     C* controlli sulla fase delle attività
349400131203     c                   eval      RcaSocieta=Societa988
349500131203     C                   eval      RcaPosiz=Posattr057
349600131203     c                   eval      RcaKcc    =kcc988
349700131203     c                   eval      RcaKsc    =ksc988
349800131203     c                   eval      Rcadtpar  =h1DtPar
349900131203     c                   eval      RcaNrpar  =h1NrPar
350000131203     c                   eval      RcaSerPar =h1SerPar
350100131203     c                   eval      RcaDtSca  =H1DtSca
350200131203     c                   eval      RcaProgrIm=H1Progr
350300131203     C                   eval      RcaAzione='98'
350400131203     c     k10Rca05      chain(N)  NdRca05L                             23
350500131203     c                   if        %found(ndrca05l) and *in23=*off
350600131203     c                   if        rcastato<>' '
350700131203     c                   seton                                        5599
350800131203     c                   endif
350900131203     c                   endif
351000131203     c*
351100131203     c                   endsr
351200100426c2156C************************************************************
351300100426     C* CONTROLLO sulla fase delle attività
351400100426     C************************************************************
351500100426c2156C     CtrFase       BEGSR
351600100426     C* controlli sulla fase delle attività
351700100426     c                   eval      RcaSocieta=Societa988
351800100426     C                   eval      RcaPosiz=Posattr057
351900100426     c                   eval      RcaKcc    =kcc988
352000100426     c                   eval      RcaKsc    =ksc988
352100100426     c                   eval      Rcadtpar  =h1DtPar
352200100426     c                   eval      RcaNrpar  =h1NrPar
352300100426     c                   eval      RcaSerPar =h1SerPar
352400100426     c                   eval      RcaDtSca  =h1DtSca
352500131104     c***                eval      RcaDtAtt  = *hival
352600131104     c***     k09Rca04      setll     NdRca04L
352700131104     c***     k08Rca04      reade(n)  NdRca04L
352800131104     c     k08Rca01      setgt     NdRca01L
352900131104     c     k08Rca01      readpe(n) NdRca01L
353000100521     c* se c'è una fase aperta NON impegno, errore
353100131104     c***                if        not %eof(NdRca04l)
353200131104     c                   if        not %eof(NdRca01l)
353300100426     c                             and rcastato = ' '
353400100521     c                             and RcaAzione <> '98'
353500100521     c                             and RcaAzione <> '99'
353600100426     c                   seton                                        5499
353700100426     c                   endif
353800100426     c*
353900100426c2156c                   endsr
354000100426     C************************************************************
354100100423     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D4
354200100423     C* INPUT
354300100423     C* $FC1 = Abilitazioni videata D1
354400100423     C* OUTPUT
354500100423     C* *IN97 *IN99 Errori
354600100423     C************************************************************
354700100423c2156C     CTRKD4        BEGSR
354800100423     C                   SETOFF                                       99
354900100423     C                   MOVEA     $FC4          $FC
355000100423     C                   EXSR      CTRKEY
355100100423     C     *IN99         IFEQ      *ON
355200100423     C                   SETON                                        97
355300100423     C                   ENDIF
355400100423c2156C                   ENDSR
355500100423     C/EJECT
355600100423c2156C************************************************************
355700100423     C* F05
355800100423     C************************************************************
355900100423c2156C     F05D4         BEGSR
356000100423     C*
356100100423     C                   MOVE      *ON           $INZD4
356200100423     C*
356300100423c2156C                   ENDSR
356400100423c2156C************************************************************
356500100423     C* F12
356600100423     C************************************************************
356700100423c2156C     F12D4         BEGSR
356800100423     C*  si pulisce tutto perchè si aggiornano i tassi per scaglioni
356900100423     c*  solo da questa videata con F6 dalla D4
357000100423     C                   CLEAR                   FMTD4
357100100423     C                   move      'S1'          $gest
357200100423     C                   move      *on           $FineD4
357300100423     C*
357400100423c2156C                   ENDSR
357500100423c2156C************************************************************
357600100423     C* GESTIONE F24 VIDEO D4
357700100423     C* input
357800100423     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
357900100423     C* $FX1 =Testi tasti funzione
358000100423     C* $FC1 = Abilitazioni
358100100423     C* $FR1 = Inversioni di fondo
358200100423     C************************************************************
358300100423     C     F24D4         BEGSR
358400100423     C*
358500100423     C                   MOVEA     $FX4          $FX
358600100423     C                   MOVEA     $FC4          $FC
358700100423     C                   MOVEA     $FR4          $FR
358800100423     C                   Z-ADD     $ULKD4        $ULK
358900100423     C                   Z-ADD     79            $L01
359000100423     C                   Z-ADD     79            $L02
359100100423     C                   EXSR      RIEKEY
359200100423     C                   Z-ADD     $ULK          $ULKD4            3 0
359300100423     C                   MOVEA     $FC           $FC4
359400100423     C                   MOVEL     $KEY1         Z4KE1
359500100423     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
359600100423     C                   MOVEL     $KEY2         Z4KE2
359700100423     C                   WRITE     FMTZ4
359800100423     C*
359900100423c2156C                   ENDSR
360000940223     C/EJECT
360100980319** $MS1 / $TX1
360200061108PROMSG    *LIBL     OPZ0001
360300980319** $OP1 / $FL1
36040094022501S      <Selezione
360500980319** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
360600061108PROMSG    *LIBL     KEY0001  01
360700940506PROMSG    *LIBL     KEY0002  02
360800940506PROMSG    *LIBL     KEY0003  03
360900940506PROMSG    *LIBL     KEY0004  04
361000940506PROMSG    *LIBL     KEY0005  05
361100940506PROMSG    *LIBL     KEY0006  06
361200940506PROMSG    *LIBL     KEY0007  07
361300940506PROMSG    *LIBL     KEY0008  08
361400940506PROMSG    *LIBL     KEY0009  09
361500061108PROMSG    *LIBL     KEY0010  10
361600061108PROMSG    *LIBL     KEY0011  11
361700061108PROMSG    *LIBL     KEY0012  12
361800061108PROMSG    *LIBL     KEY0013  13
361900061108PROMSG    *LIBL     KEY0014  14
362000061108PROMSG    *LIBL     KEY0015  15
362100061108PROMSG    *LIBL     KEY0016  16
362200061108PROMSG    *LIBL     KEY0017  17
362300061108PROMSG    *LIBL     KEY0018  18
362400061108PROMSG    *LIBL     KEY0019  19
362500940506PROMSG    *LIBL     KEY0020  20
362600940506PROMSG    *LIBL     KEY0021  21
362700940506PROMSG    *LIBL     KEY0022  22
362800940506PROMSG    *LIBL     KEY0023  23
362900940506PROMSG    *LIBL     KEY0024  24
363000980319**  ABILITAZIONE D1 : $FC1 / $FR1
3631009407200N  01
3632009507031N  02
3633009407201N  03
3634009407201N  04
3635009407200N  05
3636009407200N  06
3637009407200N  07
3638009412061N  08
3639009407200N  09
3640000611080N  10
3641009407200N  11
3642009407201N  12
3643001311110N  13                                        *C2226*
3644009407200N  14
3645009407200N  15
3646009407200N  16
3647009407200N  17
3648000903120N  18
3649009407200N  19
3650009407200N  20
3651009407200N  21
3652009407200N  22
3653009507260N  23   Impostare a '1' se si utilizzano opzioni
3654009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
365500980319** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
365600061108PROMSG    *LIBL     KEY0001  01
365700940916PROMSG    *LIBL     KEY0002  02
365800940916PROMSG    *LIBL     KEY0003  03
365900090312PROMSG    *LIBL     KEY0004  04
366000940916PROMSG    *LIBL     KEY0005  05
366100940916PROMSG    *LIBL     KEY0006  06
366200940916PROMSG    *LIBL     KEY0007  07
366300940916PROMSG    *LIBL     KEY0008  08
366400940916PROMSG    *LIBL     KEY0009  09
366500140319YBARMSG   *LIBL     KEY0115  10                               c2156
366600061108PROMSG    *LIBL     KEY0011  11
366700061108PROMSG    *LIBL     KEY0012  12
366800061108PROMSG    *LIBL     KEY0013  13
366900081229PROMSG    *LIBL     KEY0345  14                               c2110
367000061108PROMSG    *LIBL     KEY0015  15
367100061108PROMSG    *LIBL     KEY0016  16
367200061108PROMSG    *LIBL     KEY0017  17
367300061108PROMSG    *LIBL     KEY0018  18
367400061108PROMSG    *LIBL     KEY0019  19
367500940916PROMSG    *LIBL     KEY0020  20
367600940916PROMSG    *LIBL     KEY0021  21
367700940916PROMSG    *LIBL     KEY0022  22
367800940916PROMSG    *LIBL     KEY0023  23
367900940916PROMSG    *LIBL     KEY0024  24
368000980319**  ABILITAZIONE S1 : $FC2 / $FR2
3681009409160N  01
3682009409160N  02
3683009409161N  03
3684000611131N  04
3685000904061N  05
3686000903121N  06
3687009409160N  07
3688000709270N  08
3689000903121N  09
3690001004231N  10
3691001403110N  11
3692009409161N  12
3693001311111N  13                                                       C2226
3694000903120N  14                                                       c2110
3695009409160N  15
3696009409160N  16
3697000709060N  17                                                       D2254
3698000709270N  18
3699009409160N  19
3700009409160N  20
3701009409160N  21
3702009409160N  22
3703009507261N  23   Impostare a '1' se si utilizzano opzioni
3704009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
370500980319** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
370600061108PROMSG    *LIBL     KEY0001  01
370700940506PROMSG    *LIBL     KEY0002  02
370800940506PROMSG    *LIBL     KEY0003  03
370900940506PROMSG    *LIBL     KEY0004  04
371000940506PROMSG    *LIBL     KEY0005  05
371100940506PROMSG    *LIBL     KEY0006  06
371200940506PROMSG    *LIBL     KEY0007  07
371300940506PROMSG    *LIBL     KEY0008  08
371400940506PROMSG    *LIBL     KEY0009  09
371500061108PROMSG    *LIBL     KEY0010  10
371600061108PROMSG    *LIBL     KEY0011  11
371700061108PROMSG    *LIBL     KEY0012  12
371800061108PROMSG    *LIBL     KEY0013  13
371900061108PROMSG    *LIBL     KEY0014  14
372000061108PROMSG    *LIBL     KEY0015  15
372100061108PROMSG    *LIBL     KEY0016  16
372200061108PROMSG    *LIBL     KEY0017  17
372300061108PROMSG    *LIBL     KEY0018  18
372400061108PROMSG    *LIBL     KEY0019  19
372500940506PROMSG    *LIBL     KEY0020  20
372600940506PROMSG    *LIBL     KEY0021  21
372700940506PROMSG    *LIBL     KEY0022  22
372800940506PROMSG    *LIBL     KEY0023  23
372900940506PROMSG    *LIBL     KEY0024  24
373000980319**  ABILITAZIONE D3 : $FC3 / $FR3
3731009407200N  01
3732009407200N  02
3733009407201N  03
3734000611151N  04
3735009407200N  05
3736009407200N  06
3737009407200N  07
3738009407200N  08
3739009407200N  09
3740009409160N  10
3741009407200N  11
3742009407201N  12
3743009407200N  13
3744009407200N  14
3745009407200N  15
3746009407200N  16
3747009409160N  17
3748009409160N  18
3749009407200N  19
3750009407200N  20
3751009407200N  21
3752009407200N  22
3753009507260N  23   Impostare a '1' se si utilizzano opzioni
3754009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
375500100423** TASTI DI FUNZIONE  D4                      c2156
375600100423PROMSG    *LIBL     KEY0001  01
375700100423PROMSG    *LIBL     KEY0002  02
375800100423PROMSG    *LIBL     KEY0003  03
375900100423PROMSG    *LIBL     KEY0004  04
376000100423PROMSG    *LIBL     KEY0005  05
376100100423PROMSG    *LIBL     KEY0006  06
376200100423PROMSG    *LIBL     KEY0007  07
376300100423PROMSG    *LIBL     KEY0008  08
376400100423PROMSG    *LIBL     KEY0009  09
376500100423PROMSG    *LIBL     KEY0010  10
376600100423PROMSG    *LIBL     KEY0011  11
376700100423PROMSG    *LIBL     KEY0012  12
376800100423PROMSG    *LIBL     KEY0013  13
376900100423PROMSG    *LIBL     KEY0014  14
377000100423PROMSG    *LIBL     KEY0015  15
377100100423PROMSG    *LIBL     KEY0016  16
377200100423PROMSG    *LIBL     KEY0017  17
377300100423PROMSG    *LIBL     KEY0018  18
377400100423PROMSG    *LIBL     KEY0019  19
377500100423PROMSG    *LIBL     KEY0020  20
377600100423PROMSG    *LIBL     KEY0021  21
377700100423PROMSG    *LIBL     KEY0022  22
377800100423PROMSG    *LIBL     KEY0023  23
377900100423PROMSG    *LIBL     KEY0024  24
378000100423**  ABILITAZIONE D4                           c2156
3781001004230N  01
3782001004230N  02
3783001004231N  03
3784001004261N  04
3785001004231N  05
3786001004231N  06
3787001004230N  07
3788001004230N  08
3789001004230N  09
3790001004230N  10
3791001004230N  11
3792001004231N  12
3793001004230N  13
3794001004230N  14
3795001004230N  15
3796001004230N  16
3797001004230N  17
3798001004230N  18
3799001004230N  19
3800001004230N  20
3801001004230N  21
3802001004230N  22
3803001004230N  23   Impostare a '1' se si utilizzano opzioni
3804001004231N  24   Lasciare sempre impostata a '1' il valore della riga 24
