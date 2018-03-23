000100980721     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970520     H*PARMS CVTOPT(*NONE)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400941115      *
000500130915    > *                  Gestione attività manuali
000600061115      *           -------------------------------------
000700940211     F*------------
000800130915     FNDAtt01L  UF A E           K DISK
000900131009     F***NdRca04L  UF A E           K DISK
001000131009     FNdRca01L  UF A E           K DISK
001100131206C2226F                                     INFDS(INF001)
001200130922     FNdRca05L  UF   E           K DISK
001300130922     F                                     RENAME(NDRCA000:NDRCA05)
001400130915    >FNDC980D   CF   E             WORKSTN usropn
001500130915    >F                                     RENAME(C980D1:FMTD1)
001600130915    >F                                     RENAME(C980D2:FMTD2)
001700130915    >F                                     RENAME(C980D3:FMTD3)
001800130915    >F                                     RENAME(C980S1:FMTS1)
001900130915    >F                                     RENAME(C980C1:FMTC1)
002000130915    >F                                     RENAME(C980T1:FMTT1)
002100130915    >F                                     RENAME(C980Z1:FMTZ1)
002200130915    >F                                     RENAME(C980Z2:FMTZ2)
002300130915    >F                                     RENAME(C980Z3:FMTZ3)
002400130915     F                                     RENAME(C980D4:FMTD4)
002500130915     F                                     RENAME(C980Z4:FMTZ4)
002600940131     F                                     SFILE(FMTS1:S1NRR)
002700940201     F                                     INFDS(DSFMT)
002800940128     D*----------------------------------------------------*
002900940302     D* Schiere per la gestione delle opzioni
003000940302     D*
003100960117     D* dimensione della schiera $MS1, da non cambiare
003200960117     D $DIMOP          C                   CONST(30)
003300950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003400950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
003500950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003600950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
003700960117     D* schiera contenente i nomi dei programma richiamati attraverso
003800960117     D* le opzioni personalizzate
003900960117     D $PGO1           S             10    DIM($dimop)
004000941214     D $TX             S             50    DIM(50)
004100941214     D $FL             S              1    DIM(50)
004200940909     D* Schiere generali per la gestione dei tasti di funzione
004300940506     D $FM             S             27    DIM(24)
004400940506     D $FX             S             50    DIM(24)
004500940506     D $FC             S              1    DIM(24)
004600940506     D $FR             S              1    DIM(24)
004700940909     D* Schiere per la gestione dei tasti di funzione
004800940506     D* Video D1
004900940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005000940506     D $FX1            S             50    DIM(24) ALT($FM1)
005100940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005200940506     D $FR1            S              1    DIM(24) ALT($FC1)
005300960202     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005400960202     D* funzione personalizzata S1
005500960202     D $PGF2           S             10    DIM(25)
005600940506     D* Schiere per la gestione dei tasti di funzione
005700940506     D* Video S1
005800940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005900940506     D $FX2            S             50    DIM(24) ALT($FM2)
006000940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006100940506     D $FR2            S              1    DIM(24) ALT($FC2)
006200940916     D* Schiere per la gestione dei tasti di funzione
006300940916     D* Video D2
006400940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006500940916     D $FX3            S             50    DIM(24) ALT($FM3)
006600940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006700940916     D $FR3            S              1    DIM(24) ALT($FC3)
006800130915     D* Schiere per la gestione dei tasti di funzione
006900130915     D* Video D4
007000130915     D $FM4            S             27    DIM(24) CTDATA PERRCD(1)
007100130915     D $FX4            S             50    DIM(24) ALT($FM4)
007200130915     D $FC4            S              1    DIM(24) CTDATA PERRCD(1)
007300130915     D $FR4            S              1    DIM(24) ALT($FC4)
007400130915     D*
007500130915     D $D1L01          S              3  0
007600950726     D $D1L02          S              3  0
007700950726     D $S1L01          S              3  0
007800950726     D $S1L02          S              3  0
007900950906     D $C1Op1          S              3  0
008000950906     D $C1Op2          S              3  0
008100950906     D $D2L01          S              3  0
008200950906     D $D2L02          S              3  0
008300940302     D*
008400960202     D*-------------
008500960202     D* Indice della schiera tasti funzione
008600960202     D PosFun          S              2  0
008700940211     D/EJECT
008800940211     D*-------------
008900940207     D* Valori assunti
009000950524     D XMPEDS        E DS                  INZ
009100950524     D XMPEDS1       E DS                  INZ
009200941111     D DSMPE           DS                  INZ
009300940207     D  $LOP                   1      1  0
009400940831     D  $ORD                   2      3  0
009500940207     D*-------------
009600940211     D* Passaggio Parametri
009700940211     D KPJBA         E DS
009800061115     D*-------------
009900941209     D* Dati di ambiente ottenuti da XSOC
010000061108     D SOC001        E DS                  EXTNAME(XSOC001DS)
010100061115     D    FgO1                        1    overlay(XscFgO)
010200941209     D*-------------
010300941209     D* DS Interna per dati di output di XSOC
010400941209     D XSOCDS          DS          1000
010500940325     D*-------------
010600940325     D* Parametri in ricezione
010700130915    >D NDC980DS      E DS
010800130915     D  dtpar980x    E                     EXTFLD(dtpar980)                     *
010900130915     D  nrpar980x    E                     EXTFLD(nrpar980)                     *
011000130915     D  serpar980x   E                     EXTFLD(serpar980)                    *
011100130915     D*-------------
011200130915     D* Parametri in ricezione ds multipla
011300130915    >D NDC980DS1     E DS                  occurs(1000) inz
011400061115     D*-------------
011500090310     D* Richiamo a XATB
011600090310     D XATBDS        E DS                  INZ
011700090312     D XTABDS        E DS                  INZ
011800090310     D*-------------
011900130915     D* Tabella Azione
012000130915     D ANG047DS      E DS                  inz
012100130915     D* Tabella gruppi utenti
012200130915     D ANG048DS      E DS                  inz
012300090310     D*-------------
012400061115     D* DS Esterna gestione messaggi                                            *
012500061115     D XMSGDS        E DS                  inz                                  *
012600061115     D  STS          E                     EXTFLD(MSGSTS)                       *
012700061115     D                                     DIM(333)                             *
012800061115     D  JBA          E                     EXTFLD(MSGJBA)                       *
012900061115     D                                     DIM(502)                             *
013000061115     D  MSGRSP       E                     INZ('N')
013100061115     D  MSGEMV       E                     INZ('S')
013200061115     D  MSGMSG       E                     INZ(0)
013300061115     D  MSGRGP       E                     INZ(10)
013400061115     D  MSGCNL       E                     INZ('N')
013500061115     D  MSGVID       E                     INZ('N')
013600061115     D  MSGOPE       E                     INZ('N')
013700061115     D  MSGSTP       E                     INZ('N')
013800061115     D  MSGLCK       E                     INZ('N')
013900061113     D*---------------
014000960202     D* campo di salvataggio per la DS d'ingresso durante la chiamata
014100960202     D* alla funzione personalizzata
014200130915    >D $SaveDS         S                   like(NDC980DS)
014300131025     D*-------------
014400131025     D* F13=selezione multipla per ripetere i dati inseriti
014500131025     D  SavAzione      s                   Like(S1Azione) inz
014600131025C2226D  SavGrute       s                   Like(S1Grute) inz
014700131025C2226D  SavDtAtt       s                   Like(S1DtAtt) inz
014800131025C2226D  SavNota        s                   Like(S1Nota)  inz
014900131025C2226D  Wc1RCD         s                   Like(C1RCD)  inz
015000960202     D*-------------
015100940211     D DSFMT           DS           512
015200940506     D  $TASTO               369    369
015300940211     D  NRG                  370    370
015400940211     D  NCL                  371    371
015500940211     D  SFLNRR               378    379B 0
015600940211     D*-------------
015700940211     D* posizione cursore
015800940211     D CURSOR          DS
015900940211     D  RIRI                   1      2B 0 INZ
016000940211     D  R$$                    2      2
016100940211     D  COCO                   3      4B 0 INZ
016200940211     D  C$$                    4      4
016300940127     D*-------------
016400940127     D* Reperimento nome PGM
016500940127     D STATUS         SDS           333
016600940127     D  DSPGM            *PROC
016700950411     D  PARMS            *PARMS
016800090316     D  SDSUSR               254    263
016900940207     D*-------------
017000940616     D* Costanti
017100940616     D* nr.di righe del sfl (SFLPAG)
017200090310    >D SFLPAG          C                   CONST(15)
017300130915     D* ds nota
017400130915     D d4nota          DS           400
017500130915     D d4nota1                 1     80
017600130915     D d4nota2                81    160
017700130915     D d4nota3               161    240
017800130915     D d4nota4               241    320
017900130915     D d4nota5               321    400
018000940616     D*-------------
018100940506     D* Tasti di funzione
018200940506     D F01             C                   CONST(X'31')
018300940506     D F02             C                   CONST(X'32')
018400940506     D F03             C                   CONST(X'33')
018500940506     D F04             C                   CONST(X'34')
018600940506     D F05             C                   CONST(X'35')
018700940506     D F06             C                   CONST(X'36')
018800940506     D F07             C                   CONST(X'37')
018900940506     D F08             C                   CONST(X'38')
019000940506     D F09             C                   CONST(X'39')
019100940506     D F10             C                   CONST(X'3A')
019200940506     D F11             C                   CONST(X'3B')
019300940506     D F12             C                   CONST(X'3C')
019400940506     D F13             C                   CONST(X'B1')
019500940506     D F14             C                   CONST(X'B2')
019600940506     D F15             C                   CONST(X'B3')
019700940506     D F16             C                   CONST(X'B4')
019800940506     D F17             C                   CONST(X'B5')
019900940506     D F18             C                   CONST(X'B6')
020000940506     D F19             C                   CONST(X'B7')
020100940506     D F20             C                   CONST(X'B8')
020200940506     D F21             C                   CONST(X'B9')
020300940506     D F22             C                   CONST(X'BA')
020400940506     D F23             C                   CONST(X'BB')
020500940506     D F24             C                   CONST(X'BC')
020600940506     D ENTER           C                   CONST(X'F1')
020700940506     D ROLDWN          C                   CONST(X'F4')
020800940506     D ROLLUP          C                   CONST(X'F5')
020900961017     D*-------------
021000021010     D* Attributi video
021100021010     D Normale         C                   Const(X'32')
021200021010     D Protetto        C                   Const(X'A0')
021300021010     D*-------------
021400950524     D* Sottotitolo di testata
021500070928     D SOTTIT          C                   CONST(' ')
021600960109     D*-------------
021700950606     D* Variabili per gestione videate
021800950606     D $Gest           S              2A
021900950606     D $Fine           S              1A
022000950606     D $InzD1          S              1A
022100950606     D $InzS1          S              1A
022200950606     D $InzD2          S              1A
022300950606     D $LastG          S              2A
022400950606     D $LastV          S              2A
022500950606     D $EndFile        S              1A
022600950606     D $Esci           S              1A
022700950606     D $RcdOK          S              1A
022800950606     D $Unique         S              1A
022900090511     D $RigheVuote     S              3  0
023000140313     D $F10mult        S              1A
023100950606     D*-------------
023200950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
023300950606     D S1Nrr           S                   like(C1Rcd)
023400950606     D WSfl            S                   like(C1Nrr)
023500950606     D WMax            S                   like(C1Rcd)
023600950606     D WPag            S                   like(C1Rcd)
023700950606     D WInzS1          S                   like($InzS1)
023800130915    >D WOpr            S                   like(Opr980)
023900090310     D CurR            S              3S 0
024000090310     D CurC            S              3S 0
024100950606     D Win             S             99A
024200950606     D W1Pos           S                   like(C1Pos)
024300950606     D WINKR           S              1A
024400970912     D WExf            S              1A
024500061108     D ZDateiso        S               D
024600131001     D WrkData         S               D
024700950606     D*-------------
024800950606     D*  Indici
024900950606     D X               S              3S 0
025000061117     D Y               S              5S 0
025100960117     D* Indice della schiera per le opzioni personalizzate
025200960117     D PosOpz          S              2  0
025300960117     D* numero max opzioni
025400960117     D NumOpz          S              2  0 Inz($DIMOP)
025500960117     D* numero effettive opzioni
025600960117     D OpzLen          S              2  0
025700950606     D*-------------
025800061115     D* Campi per tubero
025900061115     D DataRif         S               D
026000061115     D DataIso         S               D
026100061115     D Esito           S              1
026200061115     D GesMsg          S              1
026300061115     D Output          S           4000
026400061115     D LenOut          S              9B 0
026500061115     D Struttura       S             10
026600090316     D*----------
026700090312     D*  ultimo progressivo righe fittizie (output)
026800090312     D PrgFittizie     S              9S 0
026900090312     D*  totale scaduto (output)
027000090312     D TotScaduto      S             19s 3
027100090316     D*----------
027200070927     D* per riga unica tasti
027300070927     D $A              s             79
027400070927     D $B              s             79
027500090312     D*-------------
027600090312     D s1nota          DS           512
027700130915     D  s1nota1                1     44
027800130915     D  s1nota2               45    174
027900130916     D  s1nota3              175    290
028000090312     D*-------------
028100090312     D* DS esterna per dati clienti
028200090312     D DVACLI        E DS                  INZ
028300090312     D*-------------
028400090312     D* Definizioni parametri driver anagrafici
028500090312     DDvaric           S              1
028600090312     DDvaSocieta       S              3
028700090312     DDvaUnita         S              8
028800090312     DDvaStrutt        S             10
028900090312     DDvaDtRif         S               D
029000090312     DDvaLenOut        S              9B 0
029100090312     DDvaSnatura       S              1
029200090312     DDvaCodice        S              8
029300090312     DDvaLineav        S              3
029400090312     DDvaFiliale       S              3  0
029500090312     DDvaSogg          S              8
029600090312     DDvaTpInd         S              2
029700090312     DDvaCdInd         S              3
029800090312     DDvaErrore        S              1
029900090312     DDvaOutput        S           4000
030000090312     D*-------------
030100090312     D* DS per controllo conto
030200090312     D ND002DS       E DS                  INZ
030300130915     D Categorie             131    250
030400090316     D*----------
030500130915     D* Schiera categorie da ND002DS
030600130915     D DsCateg         DS           120
030700130915     D SkCateg                        4    dim(30)
030800100426     D*-------------
030900090312     D* Parametri del PGM di reperimento informazioni recupero credito
031000090312     D NDCL99DS      E DS                  inz
031100090316     D*----------------------------------------------------*
031200090316     D* DS per richiesta dati a X20
031300090316     D X20ScadDS     E DS                  inz
031400090316     D* DS parametro esteso se richiesto x20parscd='S'
031500090316     D X20SCADXDS    E DS                  inz
031600090316     D*------------
031700090316     D $Aggiorna       S              1
031800130915     D savdtpar        S                   like(s1dtpar)
031900130915     D savnrpar        S                   like(s1nrpar)
032000130915     D savserpar       S                   like(h1serpar)
032100090316     D*----------
032200090316     D* DS gestione parametri modulo
032300090316     D XPAOUT          DS          2000
032400090316     D anp057ds      E DS
032500090316     C*----------------------------------------------------*
032600061115     D* PROTOTIPAZIONE DELLE PROCEDURE
032700061115     D/COPY *LIBL/SRCCPY,PJX002PR
032800131206      *
032900131206C2226 * DS per UserExit
033000131206  "  D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
033100131206  "  D* DS Numero relativo NDRCA00F
033200131206  "  D Inf001          DS
033300131206C2226D  RRNRCA               397    400B 0
033400131206      *
033500940127     C*----------------------------------------------------*
033600940127     C*                MAIN LINE PROGRAM
033700940127     C*----------------------------------------------------*
033800940127     C*
033900940223     C* inizializzazione variabili
034000940223     C                   EXSR      INZVAR
034100940223     C*
034200940223     C     $FINE         DOWEQ     *OFF
034300940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
034400940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
034500940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
034600941027     C                   ENDCS
034700941027     C                   ENDDO
034800940325     C* fine programma
034900940325     C                   EXSR      ENDPGM
035000940225     C*
035100940325     C/EJECT
035200940325     C************************************************************
035300940325     C* FINE PROGRAMMA
035400940325     C************************************************************
035500940325     C     ENDPGM        BEGSR
035600940325     C*
035700940325     C* passaggio dati al pgm chiamante
035800130915     c                   movel     ndc980ds      kpjbu
035900940325     C*
036000940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
036100130915    >C     OPZ980        IFEQ      *BLANK
036200940325     C                   SETON                                            LR
036300940325     C* altrimenti chiudo in RT
036400940325     C                   ELSE
036500940325     C                   SETON                                            RT
036600940325     C                   ENDIF
036700940616     C                   RETURN
036800940325     C*
036900940325     C                   ENDSR
037000940224     C/EJECT
037100940201     C************************************************************
037200940201     C* RICERCHE
037300940201     C************************************************************
037400940127     C     SEARCH        BEGSR
037500940127     C*
037600940127     C* determino Riga/Colonna del cursore
037700940127     C                   MOVE      NRG           R$$
037800940127     C                   MOVE      NCL           C$$
037900940127     C                   Z-ADD     RIRI          CURR
038000940127     C                   Z-ADD     COCO          CURC
038100940127     C*
038200061114     c                   select
038300071003     c* stessa ricerca per tutti i campi per sfl S1
038400130915     C* Azione
038500130915     c                   When      H1NmFl = 'S1AZIONE'
038600081229     c     C1NRR         CHAIN     FMTS1                              21
038700090312     c*
038800090312     C                   RESET                   XTABDS
038900090312     C                   MOVE      '01'          XTAOPZ
039000090312     C                   MOVE      xscsoc        XTAAZI
039100090312     C                   MOVE      XSCLIN        XTALIN
039200090312     c                   Z-ADD     20            XTARIG
039300090312     C                   Z-ADD     CURC          XTACOL
039400090312     C                   CLEAR                   KPJBU
039500090312     C                   MOVEL     XTABDS        KPJBU
039600130915     C                   CALL      'ANG047R'
039700090312     C                   PARM                    KPJBA
039800090312     C                   MOVEL     KPJBU         XTABDS
039900090312     c*
040000090312     C     XTAret        IFNE      '2'
040100130915     C                   move      XTAKEY        S1Azione
040200130915     c* decodifica azione
040300090312     C                   MOVE      '1'           XTBRIC
040400090312     C                   MOVE      XSCSOC        XTBAZI
040500090312     C                   MOVE      *ZERO         XTBKEY
040600130915     C                   MOVE      '047'         XTBCOD
040700130915     C                   MOVE      S1Azione      XTBKEY
040800090312     C                   CALLb     'XATB'
040900090312     C                   PARM                    XATBDS
041000090312     C     XTBERR        IFEQ      *Off
041100130915     C                   MOVEL     XTBUNI        Ang047ds
041200130916     C                   MOVEL     Des047        S1AzioneD
041300090312     c                   endif
041400090312     C                   ENDIF
041500090312     c*
041600090312     c                   move      *on           *in32
041700090312     c                   exsr      Atrs1
041800090312     c                   Update    fmts1
041900090312     c                   move      *off          *in32
042000130915     C*
042100130915     C                   When      H1NmFl = 'D4AZIONE'
042200130915     C*
042300130915     C                   RESET                   XTABDS
042400130915     C                   MOVE      '01'          XTAOPZ
042500130915     C                   MOVE      xscsoc        XTAAZI
042600130915     C                   MOVE      XSCLIN        XTALIN
042700130915     c                   Z-ADD     20            XTARIG
042800130915     C                   Z-ADD     CURC          XTACOL
042900130915     C                   CLEAR                   KPJBU
043000130915     C                   MOVEL     XTABDS        KPJBU
043100130915     C                   CALL      'ANG047R'
043200130915     C                   PARM                    KPJBA
043300130915     C                   MOVEL     KPJBU         XTABDS
043400130915     C*
043500130915     C     XTAret        IFNE      '2'
043600130915     C                   move      XTAKEY        D4AZIONE
043700130915     C* decodifica azione
043800130915     C                   MOVE      '1'           XTBRIC
043900130915     C                   MOVE      XSCSOC        XTBAZI
044000130915     C                   MOVE      *ZERO         XTBKEY
044100130915     C                   MOVE      '047'         XTBCOD
044200130915     C                   MOVE      D4Azione      XTBKEY
044300130915     C                   CALLb     'XATB'
044400130915     C                   PARM                    XATBDS
044500130915     C     XTBERR        IFEQ      *Off
044600130916     C                   MOVEL     XTBUNI        ang047ds
044700130915     C                   MOVEL     Des047        d4Azioned
044800130915     C                   endif
044900130915     C                   ENDIF
045000130915     C* Gruppo utenti destinatari
045100130923     C                   When      H1NmFl = 'S1GRUTE'
045200130915     C*
045300130915     C                   RESET                   XTABDS
045400130915     C                   MOVE      '01'          XTAOPZ
045500130915     C                   MOVE      XSCSOC        XTAAZI
045600130915     C                   MOVE      XSCLIN        XTALIN
045700130915     C                   Z-ADD     CURR          XTARIG
045800130915     C                   Z-ADD     CURC          XTACOL
045900130915     C                   CLEAR                   KPJBU
046000130915     C                   MOVEL     XTABDS        KPJBU
046100130915     C                   CALL      'ANG048R'
046200130915     C                   PARM                    KPJBA
046300130915     C                   MOVEL     KPJBU         XTABDS
046400130915     C     XTAret        IFNE      '2'
046500130915     C                   MOVE      XTAKEY        S1GRUTE
046600130915     C                   ENDIF
046700130930     C                   move      *on           *in32
046800130930     c                   exsr      Atrs1
046900130930     c                   Update    fmts1
047000130930     c                   move      *off          *in32
047100130930     C*
047200130923     C*
047300130923     C                   When      H1NmFl = 'D4GRUTE'
047400130923     C*
047500130923     C                   RESET                   XTABDS
047600130923     C                   MOVE      '01'          XTAOPZ
047700130923     C                   MOVE      XSCSOC        XTAAZI
047800130923     C                   MOVE      XSCLIN        XTALIN
047900130923     C                   Z-ADD     CURR          XTARIG
048000130923     C                   Z-ADD     CURC          XTACOL
048100130923     C                   CLEAR                   KPJBU
048200130923     C                   MOVEL     XTABDS        KPJBU
048300130923     C                   CALL      'ANG048R'
048400130923     C                   PARM                    KPJBA
048500130923     C                   MOVEL     KPJBU         XTABDS
048600130923     C     XTAret        IFNE      '2'
048700130923     C                   MOVE      XTAKEY        D4GRUTE
048800130923     C                   ENDIF
048900940223     C*
049000940223     C* ricerca non abilitata in questa posizione
049100941118   X1C                   OTHER
049200940929     C                   SETON                                        9899
049300940127     C*
049400941118   E1C                   ENDSL
049500130923     C*
049600950612     C*
049700950612     C* imposto pos. del cursore
049800061116     C                   MOVE      *HIVAL        $LASTV
049900061116     C                   Z-ADD     CURR          H1RIGA
050000061116     C                   Z-ADD     CURC          H1COLO
050100950612     C*
050200940127     C                   ENDSR
050300071003     C/EJECT
050400940224     C************************************************************
050500940131     C* RIEMPIMENTO OPZIONI
050600940224     C************************************************************
050700940131     C     RIEOPZ        BEGSR
050800940223     C*
050900940223     C* Input :
051000940223     C* - $TX = schiera con tutti i testi disponibili
051100940223     C* - $FL = schiere con i flag di validità associati ai testi
051200940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
051300940223     C*          opzione usato
051400940223     C* - WMXO = variabile contenente il numero massimo di testi
051500940223     C*          opzione usabili
051600940721     C* - $L01 = Lunghezza prima riga
051700940721     C* - $L02 = Lunghezza seconda riga
051800940223     C* Output :
051900940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
052000940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
052100941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
052200940223     C*
052300950522     C                   CALLB     'X04OPMSG'
052400940223     C* input
052500940223     C                   PARM                    $TX
052600940223     C                   PARM                    $FL
052700940225     C                   PARM                    WCOZ              3 0
052800940223     C                   PARM                    WMXO              3 0
052900940223     C* output
053000940223     C                   PARM                    WOPZ1            76
053100940223     C                   PARM                    WOPZ2            76
053200941214     C                   PARM                    FF23              1
053300950301     C* input
053400950726     C                   PARM                    $L01              3 0
053500950726     C                   PARM                    $L02              3 0
053600940223     C*
053700060224     C                   PARM                    $ALLOPT         960
053800060224     C*
053900940131     C                   ENDSR
054000940506     C/EJECT
054100940506     C************************************************************
054200940506     C* RIEMPIMENTO TASTI FUNZIONE
054300940506     C************************************************************
054400940506     C     RIEKEY        BEGSR
054500940506     C*
054600940506     C* Input :
054700940506     C* - $FX = schiera con tutti i testi disponibili
054800940506     C* - $FC = schiere con i flag di validità associati ai testi
054900940506     C* - $FR = schiere con i flag di REVERSE IMAGE
055000940506     C* - $ULK = ultimo testo caricato
055100940721     C* - $L01 = Lunghezza prima riga
055200940721     C* - $L02 = Lunghezza seconda riga
055300940506     C* Output :
055400940506     C* - $ULK
055500940609     C* - $KEY1= 1° riga di tasti funzione
055600940609     C* - $KEY2= 2° riga di tasti funzione
055700940506     C*
055800950522     C                   CALLB     'XKEYMSG'
055900940506     C* input
056000940506     C                   PARM                    $FX
056100940506     C                   PARM                    $FC
056200940506     C                   PARM                    $FR
056300940506     C                   PARM                    $ULK              3 0
056400940506     C* output
056500940506     C                   PARM                    $KEY1            79
056600940506     C                   PARM                    $KEY2            79
056700950301     C* input
056800061108     C                   PARM                    $L01              3 0
056900061108     C                   PARM                    $L02              3 0
057000940506     C*
057100060224     C                   PARM                    $ALLFUNCT       480
057200060224     C*
057300940506     C                   ENDSR
057400940224     C/EJECT
057500940224     C************************************************************
057600940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
057700940224     C************************************************************
057800940224     C     GESPIE        BEGSR
057900940224     C*
058000940224     C                   SELECT
058100940224     C* Parzializzazioni
058200940224     C     $GEST         WHENEQ    'D1'
058300940506     C     $ULKD1        IFEQ      0
058400940506     C* riempimento opzioni
058500940506     C                   EXSR      F24D1
058600940506     C                   ELSE
058700061108     C                   WRITE     FMTZ1
058800940506     C                   ENDIF
058900940506     C* Lista
059000940506     C     $GEST         WHENEQ    'S1'
059100940506     C     $ULKS1        IFEQ      0
059200940506     C                   EXSR      F24S1
059300940506     C                   ELSE
059400131025     C* totali
059500131025     C                   z-add     TotScaduto    z2TotScad
059600061108     C                   WRITE     FMTZ2
059700940506     C                   ENDIF
059800940916     C* Modifica valori assunti
059900940916     C     $GEST         WHENEQ    'D2'
060000940916     C     $ULKD2        IFEQ      0
060100940916     C                   EXSR      F24D2
060200940916     C                   ELSE
060300940916     C                   WRITE     FMTZ3
060400940916     C                   ENDIF
060500130915     C     $GEST         WHENEQ    'D4'
060600130915     C     $ULKD4        IFEQ      0
060700130915     C                   EXSR      F24D4
060800130915     C                   ELSE
060900130915     C                   WRITE     FMTZ4
061000130915     C                   ENDIF
061100940506     C                   ENDSL
061200940506     C*
061300940224     C                   ENDSR
061400940224     C/EJECT
061500940224     C************************************************************
061600940224     C* GESTIONE PARZIALIZZAZIONI
061700940224     C************************************************************
061800940224     C     GESD1         BEGSR
061900940224     C* inizializzazione videata
062000940223     C     $INZD1        IFEQ      *ON
062100940127     C                   EXSR      INZD1
062200940223     C                   MOVE      *OFF          $INZD1
062300140313     C                   MOVE      *OFF          $F10mult
062400941027     C                   ENDIF
062500940223     C* emissione piede videata se proveniente da altra
062600940223     C* salvataggio valore formato di provenienza
062700940202     C     $LASTV        IFNE      'D1'
062800940223     C* salvo stato F17 per sapere cosa fare in caso di F12
062900940223     C                   MOVE      *INKR         WINKR
063000061113     c                   clear                   T1tit
063100940202     C                   WRITE     FMTT1
063200940224     C                   EXSR      GESPIE
063300940202     C                   MOVE      $LASTV        $LASTG
063400940202     C                   MOVE      'D1'          $LASTV
063500940707     C                   ENDIF
063600940707     C*
063700940707     C* se esistono errori sulla videata
063800940707     C* emetto la write del formato a indicatori spenti per vedere
063900940707     C* le eventuali decodifiche
064000940707     C     *IN99         IFEQ      *ON
064100940707     C                   MOVEA     *IN           WIN
064200940707     C                   MOVE      *ALL'0'       IN5098           49
064300940707     C                   MOVEA     IN5098        *IN(50)
064400940707     C                   WRITE     FMTD1
064500940707     C                   MOVEA     WIN           *IN
064600940707     C                   ENDIF
064700940131     C*
064800940131     C                   EXFMT     FMTD1
064900960605     C*
065000960605     C* azzero la posizione del cursore
065100960605     C                   Z-ADD     0             H1RIGA
065200960605     C                   Z-ADD     0             H1COLO
065300940506     C*
065400951005     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
065500951005     C* INPUT
065600951005     C* $FC1 = Abilitazioni videata D1
065700951005     C* OUTPUT
065800951005     C* *IN97 + *IN99 Errori
065900951005     C                   MOVEA     $FC1          $FC
066000951005     C                   EXSR      CTRKEY
066100940506     C*
066200940506     C     *IN99         IFEQ      *OFF
066300940131     C*
066400940117     C                   SELECT
066500940916     C* F2=Decodifica
066600940916     C     $TASTO        WHENEQ    F02
066700940916     C                   EXSR      F02D1
066800940131     C* F3=Fine
066900940506     C     $TASTO        WHENEQ    F03
067000940307     C                   EXSR      F03D1
067100940131     C* F4=Ricerca
067200940506     C     $TASTO        WHENEQ    F04
067300940127     C                   EXSR      SEARCH
067400940916     C* F5=Refresh
067500940916     C     $TASTO        WHENEQ    F05
067600940916     C                   EXSR      F05D1
067700941206     C* F8=Successivo
067800941206     C     $TASTO        WHENEQ    F08
067900941206     C                   EXSR      F08D1
068000940131     C* F12=Ritorno
068100940506     C     $TASTO        WHENEQ    F12
068200940307     C                   EXSR      F12D1
068300940131     C* F18=Modifica valori assunti
068400940506     C     $TASTO        WHENEQ    F18
068500940307     C                   EXSR      F18D1
068600940224     C* F24=Altri tasti
068700940506     C     $TASTO        WHENEQ    F24
068800940309     C                   EXSR      F24D1
068900940131     C*
069000940117     C                   OTHER
069100940127     C                   EXSR      CTRD1
069200940127     C     *IN99         IFEQ      *OFF
069300940131     C* definisco l'ordinamento
069400950410     C                   EXSR      DEFORD
069500950410     C* richiamo la lista
069600940127     C                   MOVE      'S1'          $GEST
069700940223     C                   MOVE      *ON           $INZS1
069800940302     C                   MOVE      *BLANK        W1POS
069900940211     C                   ENDIF
070000940224     C*
070100940117     C                   ENDSL
070200940224     C*
070300940506     C                   ENDIF
070400950525     C*
070500940117     C                   ENDSR
070600940224     C/EJECT
070700940224     C************************************************************
070800940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
070900940224     C************************************************************
071000940127     C     INZD1         BEGSR
071100940318     C*
071200940318     C                   CLEAR                   FMTD1
071300941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
071400941013     C*    parzializzazioni
071500941104     C                   SELECT
071600941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
071700950606     C     $Unique       WHENEQ    *ON
071800941104     C* il pgm è richiamato e
071900941104     C* sono stati passati dei valori di parzializzazione ma non
072000941104     C* la chiave univoca
072100130915    >C     OPZ980        WHENNE    *BLANK
072200941104     C* il pgm non è richiamato
072300130915    >C     OPZ980        WHENEQ    *BLANK
072400061117     c* decodifiche
072500061117     c                   exsr      F02D1
072600941115     C                   ENDSL
072700940224     C*
072800940117     C                   ENDSR
072900940916     C/EJECT
073000940916     C************************************************************
073100940916     C* GESTIONE F02 VIDEO D1
073200940916     C************************************************************
073300940916     C     F02D1         BEGSR
073400940916     C*
073500940916     C* per decodificare la videata richiamo la routine dei ctrl
073600940916     C* e poi disattivo gli eventuali messaggi di errore
073700940916     C                   EXSR      CTRD1
073800940916     C*
073900940916     C                   MOVE      *ALL'0'       IN5098
074000940916     C                   MOVEA     IN5098        *IN(50)
074100940916     C*
074200940916     C                   ENDSR
074300940224     C/EJECT
074400940131     C************************************************************
074500940307     C* GESTIONE F03 VIDEO D1
074600940131     C************************************************************
074700940307     C     F03D1         BEGSR
074800940131     C*
074900940307     C                   MOVE      *ON           $FINE
075000130915    >C                   MOVE      '1'           RET980
075100940325     C* fine programma
075200940325     C                   EXSR      ENDPGM
075300940307     C*
075400940307     C                   ENDSR
075500940916     C/EJECT
075600940916     C************************************************************
075700940916     C* GESTIONE F05 VIDEO D1
075800940916     C************************************************************
075900940916     C     F05D1         BEGSR
076000940916     C*
076100940916     C                   MOVE      *ON           $INZD1
076200940916     C*
076300940916     C                   ENDSR
076400941206     C/EJECT
076500941206     C************************************************************
076600941206     C* GESTIONE F08 VIDEO D1
076700941206     C************************************************************
076800941206     C     F08D1         BEGSR
076900941206     C*
077000941206     C                   MOVE      *ON           $FINE
077100130915    >C                   MOVE      '0'           RET980
077200941206     C*
077300941206     C                   ENDSR
077400950301     C/EJECT
077500940307     C************************************************************
077600940307     C* GESTIONE F12 VIDEO D1
077700940307     C************************************************************
077800940307     C     F12D1         BEGSR
077900940307     C*
078000940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
078100940307     C*  per F12 torno alla lista
078200940307     C     $LOP          IFEQ      2
078300940307     C     WINKR         OREQ      *ON
078400940307     C                   MOVE      'S1'          $GEST
078500940307     C*  altrimenti, per F12 torno a menu
078600940307     C                   ELSE
078700940307     C                   MOVE      *ON           $FINE
078800130915    >C                   MOVE      '2'           RET980
078900941027     C                   ENDIF
079000940307     C*
079100940307     C                   ENDSR
079200940307     C/EJECT
079300940307     C************************************************************
079400940307     C* GESTIONE F18 VIDEO D1
079500940307     C************************************************************
079600940307     C     F18D1         BEGSR
079700940307     C*
079800940307     C                   MOVE      'D2'          $GEST
079900940307     C                   MOVEL     *ON           $INZD2
080000940307     C*
080100940307     C                   ENDSR
080200940307     C/EJECT
080300940307     C************************************************************
080400940307     C* GESTIONE F24 VIDEO D1
080500940609     C* input
080600940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
080700940509     C* $FX1 =Testi tasti funzione
080800940509     C* $FC1 = Abilitazioni
080900940509     C* $FR1 = Inversioni di fondo
081000940307     C************************************************************
081100940307     C     F24D1         BEGSR
081200940506     C*
081300940506     C                   MOVEA     $FX1          $FX
081400940506     C                   MOVEA     $FC1          $FC
081500940506     C                   MOVEA     $FR1          $FR
081600940506     C                   Z-ADD     $ULKD1        $ULK
081700950726     C                   Z-ADD     $D1L01        $L01
081800950726     C                   Z-ADD     $D1L02        $L02
081900940506     C                   EXSR      RIEKEY
082000940506     C                   Z-ADD     $ULK          $ULKD1            3 0
082100940720     C                   MOVEA     $FC           $FC1
082200070927     C                   MOVEL     $KEY1         Z1KE
082300061108     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
082400061108     C                   MOVEL     $KEY2         Z1KE2
082500070927     c* riga unica tasti
082600070927     c                   eval      $A = %triml(z1ke)
082700070927     c                   eval      $B = %triml(z1ke2)
082800070927     c     $A            cat(P)    $B:0          z1ke1
082900061108     C                   WRITE     FMTZ1
083000940307     C*
083100940307     C                   ENDSR
083200940307     C/EJECT
083300940307     C************************************************************
083400940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
083500950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
083600960109     C* 93 94 96 97 98
083700940307     C************************************************************
083800940307     C     CTRD1         BEGSR
083900940307     C*
084000940127     C                   SETOFF                                       99
084100940117     C                   ENDSR
084200950410     C/EJECT
084300950410     C************************************************************
084400950410     C* DEFINIZIONE DELL'ORDINAMENTO
084500950410     C************************************************************
084600950410     C     DEFORD        BEGSR
084700950410     C*
084800950410     C* definisco l'ordinamento della lista a seconda dei valori
084900950410     C* impostati nel video di parzializzazione
085000950410     C*
085100950410     C                   Z-ADD     1             $ORD
085200061108     C*
085300950410     C                   ENDSR
085400960202     C/EJECT
085500960202     C************************************************************
085600960202     C* Chiamata programma con tasto personalizzato
085700960202     C************************************************************
085800960202     C     FPerS1        BEGSR
085900960202     C*
086000960202     C* salvo la DS di ingresso
086100130915    >C                   MOVEL     NDC980DS      $SaveDS
086200960202     C*
086300130915    >C                   MOVEL     NDC980DS      KPJBU
086400960202     C                   CALL      $PGF2(PosFun)                        22
086500960202     C                   PARM                    KPJBA
086600960202     C                   PARM                    NOMPGM
086700960202     C*
086800960202     C* ripristino la DS di ingresso
086900130915    >C                   MOVEL     $SaveDS       NDC980DS
087000960202     C*
087100960202     C                   ENDSR
087200940224     C/EJECT
087300940127     C************************************************************
087400940131     C* GESTIONE LISTA
087500940127     C************************************************************
087600940127     C     GESS1         BEGSR
087700940223     C* inizializzazione videata
087800940223     C     $INZS1        IFEQ      *ON
087900940127     C                   EXSR      INZS1
088000940223     C                   MOVE      *OFF          $INZS1
088100940127     C                   ENDIF
088200940223     C* emissione piede videata se proveniente da altra
088300940223     C* salvataggio valore formato di provenienza
088400940202     C     $LASTV        IFNE      'S1'
088500140313     c                   if        $f10mult = *on
088600140313     c                   eval      t1tit = '       --- IMMISSIONE ---'
088700140313     c                   else
088800140313     c                   clear                   T1tit
088900140313     c                   endif
089000940202     C                   WRITE     FMTT1
089100940224     C                   EXSR      GESPIE
089200940202     C                   MOVE      $LASTV        $LASTG
089300940202     C                   MOVE      'S1'          $LASTV
089400941027     C                   ENDIF
089500940223     C*
089600940223     C     WMAX          IFEQ      0
089700940211     C                   WRITE     FMTD3                                        NO RCD
089800940211     C                   ENDIF
089900960202     C* Inizializzazione indice tasti funzione personalizzati
090000960202     C                   Eval      PosFun = 25
090100140313      *
090200140313     C                   MOVE      *OFF          $F10mult
090300940127     C*
090400061108     C                   Eval      *in09=c1mode
090500940131     C                   EXFMT     FMTC1
090600960605     C*
090700960605     C* azzero la posizione del cursore
090800960605     C                   Z-ADD     0             H1RIGA
090900960605     C                   Z-ADD     0             H1COLO
091000960605     C*
091100940204     C     C1NRR         IFNE      0
091200940204     C                   Z-ADD     C1NRR         WSFL
091300940204     C                   ENDIF
091400951020     C                   IF        SFLNRR > 0
091500951020     C                   Z-ADD     SFLNRR        C1RCD
091600951020     C                   ENDIF
091700960122     C* inizializzazione indice opzioni personalizzate
091800960122     C                   Eval      PosOpz = 0
091900951005     C* CONTROLLO ABILITAZIONE TASTI
092000951005     C* INPUT
092100951005     C* $FC2 = Abilitazioni videata S1
092200951005     C* OUTPUT
092300951005     C* *IN97 + *IN99 Errori
092400951005     C                   MOVEA     $FC2          $FC
092500951005     C                   EXSR      CTRKEY
092600941205     C*
092700940506     C     *IN99         IFEQ      *OFF
092800940127     C*
092900941027     C                   SELECT
093000940127     C* F3=Fine
093100940506     C     $TASTO        WHENEQ    F03
093200940309     C                   EXSR      F03S1
093300061113     C* F4=drill
093400061113     C     $TASTO        WHENEQ    F04
093500061113     C                   EXSR      Search
093600090406     C* F5=Ripristino
093700090406     C     $TASTO        WHENEQ    F05
093800090406     C                   EXSR      F05S1
093900941206     C* F8=Successivo
094000941206     C     $TASTO        WHENEQ    F08
094100941206     C                   EXSR      F08S1
094200130915     C* F10=Immissione altre azioni per la stessa scadenza
094300130915     C     $TASTO        WHENEQ    F10
094400130915     C                   EXSR      F10S1
094500940131     C* F12=Ritorno
094600940506     C     $TASTO        WHENEQ    F12
094700940309     C                   EXSR      F12S1
094800131025     C* F13=Selezione multipla
094900131025     C     $TASTO        WHENEQ    F13
095000131025     C                   EXSR      F13S1
095100940131     C* F17=Parzializzazioni
095200940506     C     $TASTO        WHENEQ    F17
095300940309     C                   EXSR      F17S1
095400940131     C* F18=Modifica valori assunti
095500940506     C     $TASTO        WHENEQ    F18
095600940309     C                   EXSR      F18S1
095700940224     C* F23=Altre opzioni
095800940506     C     $TASTO        WHENEQ    F23
095900940309     C                   EXSR      F23S1
096000940224     C* F24=Altri tasti
096100940506     C     $TASTO        WHENEQ    F24
096200940309     C                   EXSR      F24S1
096300940127     C* ROLL-UP
096400090511     C     $TASTO        WHENEQ    ROLLUP
096500960202     C* Personalizzata
096600960202     C     $PGF2(PosFun) Whenne    *BLANK
096700960202     C                   EXSR      FperS1
096800940127     C*
096900941027     C                   OTHER
097000940127     C* CONTROLLO DATI
097100940131     C                   EXSR      CTRC1
097200940201     C     *IN99         IFEQ      *OFF
097300940131     C                   EXSR      CTRS1
097400090316     c* aggiorna
097500090316     C     *IN99         IFEQ      *OFF
097600090316     C     $TASTO        andeq     F06
097700090316     c                   Exsr      AggANA
097800090316     C                   EXSR      GESAGG
097900090316     c                   endif
098000090316     c*
098100941027     C                   ENDIF
098200941205     C*
098300941027     C                   ENDSL
098400940127     C*
098500940506     C                   ENDIF
098600941205     C*
098700940127     C                   ENDSR
098800940224     C/EJECT
098900090316     C************************************************************
099000090316     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
099100090316     C************************************************************
099200090316     C     GESAGG        BEGSR
099300090316     C*
099400090316     C* segnalo al pgm chiamante l'avvenuta conferma
099500130915    >C                   MOVE      *ON           OPR980
099600090316     C* segnalo al pgm chiamante l'aver premuto l'F6
099700130915    >C                   MOVE      '0'           RET980
099800090316     C*
099900090316     C                   MOVE      *ON           $FINE
100000090316     C*
100100090316     C                   ENDSR
100200090316     C/EJECT
100300090316     C************************************************************
100400090316     C* AGGIORNAMENTO ANAGRAFICA
100500090316     C************************************************************
100600090316     C     AGGANA        BEGSR
100700090316     c*
100800090316     C                   DO        *hival        S1NRR
100900090316     C     S1NRR         CHAIN     FMTS1                              21
101000090316     c   21              leave
101100090316     c*
101200090316     C                   SELECT
101300090316     c* se la partita è valorizzata
101400130915     c                   WHEN      h1DtPar  <> *loval
101500130915     C* aggiorno l'attività per la partita NON fittizia ricevuta
101600130915     C                   EXSR      AggAtti
101700090316     C* altrimenti
101800090316     C                   OTHER
101900130915     C* aggiorno l'attivitù su partita fittizia
102000130915     C                   EXSR      AggAttiFitt
102100090316     C                   ENDSL
102200090316     C*
102300090316     C                   ENDDO
102400090316     C*
102500090316     C                   ENDSR
102600090316     C/EJECT
102700090316     C************************************************************
102800130915     C* Aggiorno l'attività ricevuta
102900090316     C************************************************************
103000130915     C     AggAtti       BEGSR
103100090316     C*
103200130915     C                   EVAL      ATTSocieta = Societa980
103300130915     C                   EVAL      ATTKcc =   kcc980
103400130915     C                   EVAL      ATTKsc =   ksc980
103500130915     C                   EVAL      ATTDtPar =  h1dtpar
103600130915     C                   EVAL      ATTNrPar =  h1nrpar
103700130915     C                   EVAL      ATTSerPar = h1serpar
103800130915     C                   EVAL      ATTDtSca =  h1dtsca
103900130915     C                   EVAL      ATTProgr =  H1progr
104000090316     C*
104100130915     C     K08ATT01      CHAIN     NDATT01L                           21
104200090316     C*
104300090316     C* se il rcd è già presente
104400090316b1   C                   IF        *in21 = *off
104500090316     C*
104600130915     C* se l'azione è valorizzata
104700130915b2   C                   IF        s1azione <> *blank
104800090316     C* modifico
104900130915     C                   EVAL      ATTImporto = S1ImpScad
105000130915     C                   EVAL      ATTImportD = 0
105100130915     C                   EVAL      AttAzione = S1Azione
105200130915     C                   EVAL      AttGrute  = S1Grute
105300130915     c                   if        S1DtAtt <> 0
105400130915     c     *jobrun       move      S1DtAtt       AttDtAtt
105500090316     c                   else
105600130915     c                   clear                   AttDtAtt
105700090316     c                   endif
105800130915     C                   EVAL      ATTNota   = s1nota
105900130915     C                   EVAL      ATTUteUlMo = sdsusr
106000130915     C                   EVAL      ATTDtUlMo = ZDateISO
106100130915     C                   UPDATE    NDATT000
106200131028     C* Aggiorno RCA
106300131028     C                   exsr      AggiornaFase
106400090316     C*
106500130916     C* se l'azione NON è valorizzata
106600090316x2   C                   ELSE
106700130915     C* cancello solo se c'ERA l'azione!!!
106800130915     C                   if        H1Azione<> *blanks
106900130915     C*
107000130915     C                   DELETE    NDATT000
107100130922     c* cancello la fase relativa all'impegno
107200130922     c                   exsr      CancellaFase
107300130915     C                   EndIf
107400130916     C                   ENDIF
107500090316     C*
107600090316     C* se il rcd NON è presente
107700090316x1   C                   ELSE
107800090316     C*
107900130915     C* se l'azione è valorizzata
108000130915b2   C                   IF        S1Azione <> *blank
108100090316     C* scrivo
108200130915     C                   CLEAR                   NDATT000
108300130915     C                   EVAL      ATTSocieta = Societa980
108400130915     C                   EVAL      ATTUnita   = H1Unita
108500130915     C                   EVAL      ATTKcc =  Kcc980
108600130915     C                   EVAL      ATTKsc =  ksc980
108700130915     C                   EVAL      ATTDtPar =  H1DtPar
108800130915     C                   EVAL      ATTNrPar =  H1NrPar
108900130915     C                   EVAL      ATTSerPar = H1SerPar
109000130915     C                   EVAL      ATTDtSca =  H1DtSca
109100130915     c                   eval      ATTProgr =  H1progr
109200130915     C                   EVAL      ATTImporto = S1ImpScad
109300130915     C                   EVAL      ATTImportD = 0
109400130915     C                   EVAL      ATTAzione = S1azione
109500130915     C                   EVAL      ATTGrute  = S1grute
109600130915     c                   if        S1DtAtt <> 0
109700130915     c     *jobrun       move      S1DtAtt       AttDtAtt
109800090316     c                   else
109900130915     c                   clear                   AttDtAtt
110000090316     c                   endif
110100130915     C                   EVAL      ATTNota   = s1nota
110200130915     C                   EVAL      ATTDtUlMo = ZDateISO
110300130915     C                   EVAL      ATTUteUlMo = sdsusr
110400090316     C*
110500130915     C                   WRITE     NDATT000
110600130922     C* scrivo azione altre attività sul file delle attività di recupero
110700130922     C* credto solo se NON prima fase, in quanto non è possibile
110800130922     C* e non avrei tutti i dati
110900131009     C****                   If        not %eof(ndrca04L)
111000131028     C                   exsr      ctrfase
111100131009     C                   If        not %eof(ndrca01L)
111200130922     C                   exsr      ScriviFase
111300130922     C                   endif
111400100426     C*
111500130915     C* se l'azione NON è valorizzata
111600090316 x2  C                   ELSE
111700090316     C* non faccio nulla
111800090316 e2  C                   ENDIF
111900090316     C*
112000090316 e1  C                   ENDIF
112100090316     C*
112200090316     C                   ENDSR
112300090316     C************************************************************
112400130915     C* Aggiorno l'attività legata a una partita fittizia
112500090316     C************************************************************
112600130915     C     AggAttiFitt   BEGSR
112700090316     C*
112800130915     C                   EVAL      ATTSocieta = Societa980
112900130915     C                   EVAL      ATTKcc =  Kcc980
113000130915     C                   EVAL      ATTKsc =  ksc980
113100130915     C                   EVAL      ATTDtPar =  h1dtpar
113200130915     C                   EVAL      ATTNrPar =  h1nrpar
113300130915     C                   EVAL      ATTSerPar = h1serpar
113400130915     C                   EVAL      AttDtSca =  h1dtsca
113500090316     C*
113600130915     C     K07ATT01      CHAIN     NDATT01L                           21
113700090316     C*
113800090316     C* se il rcd è già presente
113900090316     C                   IF        *in21 = *off
114000090316     C*
114100090421     C* se l'esito è valorizzato
114200130915     C                   IF        S1Azione <> *blank
114300090316     C* modifico
114400130915     C                   EVAL      ATTAzione = S1Azione
114500130915     C                   EVAL      ATTGrute = S1Grute
114600130915     c                   If        S1DtAtt <> 0
114700130915     c     *jobrun       move      s1DtAtt       AttDtAtt
114800090316     c                   else
114900130915     c                   clear                   AttDtAtt
115000090316     c                   endif
115100130915     C                   EVAL      ATTNota   = s1nota
115200130915     C                   EVAL      ATTUteUlMo = sdsusr
115300130915     C                   EVAL      ATTDtUlMo = ZDateISO
115400130915     C                   UPDATE    NDATT000
115500090316     C*
115600130915     C* se l'azione NON è valorizzata
115700090316     C                   ELSE
115800090316     C* cancello
115900130915     C                   DELETE    NDATT01L
116000090316     C                   ENDIF
116100090316     C*
116200090316     C* se il rcd NON è presente
116300090316     C                   ELSE
116400090316     C*
116500130915     C* se l'azione è valorizzato
116600130915     C                   IF        S1Azione <> *blank
116700090316     C* scrivo
116800130915     C                   CLEAR                   NDATT000
116900130915     C                   EVAL      ATTFitt = '1'
117000130915     C                   EVAL      ATTSocieta = Societa980
117100130915     C                   EVAL      ATTUnita   = H1Unita
117200130915     C                   EVAL      ATTKcc =  Kcc980
117300130915     C                   EVAL      ATTKsc =  ksc980
117400130915     C                   EVAL      ATTDtPar =  h1dtpar
117500130915     C                   EVAL      ATTNrPar =  PrgFittizie + 1
117600130915     C                   EVAL      prgfittizie = ATTNrPar
117700130915     C                   EVAL      ATTSerPar = h1serpar
117800130915     C                   EVAL      ATTDtSca =  h1dtsca
117900130915     C                   if        S1dtAtt <> 0
118000130916     C     *jobrun       move      S1dtAtt       ATTdtAtt
118100130915     C                   else
118200130916     C                   clear                   ATTdtAtt
118300130915     C                   endif
118400130915     C                   EVAL      ATTAzione = S1Azione
118500130915     C                   EVAL      ATTGrute  = S1Grute
118600130915     C                   EVAL      ATTNota   = S1nota
118700130915     C                   EVAL      ATTDtUlMo = ZDateISO
118800130915     C                   EVAL      ATTUteUlMo = sdsusr
118900130915     C                   WRITE     NDATT000
119000090316     C*
119100090316     C* se l'esite NON è valorizzato
119200090316     C                   ELSE
119300090316     C* non faccio nulla
119400090316     C                   ENDIF
119500090316     C*
119600090316     C                   ENDIF
119700090316     C*
119800090316     C                   ENDSR
119900131028     C************************************************************
120000131028     C* CONTROLLO sulla fase delle attività
120100131028     C************************************************************
120200131028     C     CtrAggFase    BEGSR
120300131028     C* controlli sulla fase delle attività
120400131028     c                   eval      RcaSocieta=Societa980
120500131028     C                   eval      RcaPosiz=Posattr057
120600131028     c                   eval      RcaKcc    =kcc980
120700131028     c                   eval      RcaKsc    =ksc980
120800131028     c                   eval      Rcadtpar  =h1DtPar
120900131028     c                   eval      RcaNrpar  =h1NrPar
121000131028     c                   eval      RcaSerPar =h1SerPar
121100131028     c                   eval      RcaDtSca  =H1DtSca
121200131028     c                   eval      RcaProgrIm=H1Progr
121300131028     C                   eval      RcaAzione=H1Azione
121400140319     c     k10Rca05      chain(n)  NdRca05L                             23
121500131028     c                   if        %found(ndrca05l) and *in23=*off
121600131029     c                   if        rcaAzione <> S1Azione
121700131029     c                             or rcagrute <> S1grute
121800131029     c                             or rcadtatt <> %date(S1DtAtt:*jobrun)
121900131029     c                             or rcanota <> S1nota
122000131028     c                   if        rcastato<>' '
122100131029     c                   seton                                        5799
122200131028     c                   endif
122300131029     c                   endif
122400131029     c                   else
122500131029     c                   seton                                        5899
122600131028     c                   endif
122700131028     c*
122800131028     c                   endsr
122900140319     C************************************************************
123000140319     C* reperisco stato fase
123100140319     C************************************************************
123200140319     C     RepStatoFase  BEGSR
123300140319     C*
123400140319     c                   eval      RcaSocieta=Societa980
123500140319     C                   eval      RcaPosiz=Posattr057
123600140319     c                   eval      RcaKcc    =kcc980
123700140319     c                   eval      RcaKsc    =ksc980
123800140319     c                   eval      Rcadtpar  =h1DtPar
123900140319     c                   eval      RcaNrpar  =h1NrPar
124000140319     c                   eval      RcaSerPar =h1SerPar
124100140319     c                   eval      RcaDtSca  =H1DtSca
124200140319     c                   eval      RcaProgrIm=H1Progr
124300140319     C                   eval      RcaAzione=H1Azione
124400140319     c     k10Rca05      chain(n)  NdRca05L                             23
124500140319     c                   if        %found(ndrca05l) and *in23=*off
124600140319     c                   move      rcastato      h1stato
124700140319     c                   else
124800140319     c                   clear                   h1stato
124900140319     c                   endif
125000140319     c*
125100140319     c                   endsr
125200130922     C************************************************************
125300130922     C* CONTROLLO sulla fase delle attività
125400130922     C************************************************************
125500130922     C     CtrFase       BEGSR
125600130922     C* controlli sulla fase delle attività
125700130922     c                   eval      RcaSocieta=Societa980
125800130922     C                   eval      RcaPosiz=Posattr057
125900130922     c                   eval      RcaKcc    =kcc980
126000130922     c                   eval      RcaKsc    =ksc980
126100130922     c                   eval      Rcadtpar  =h1DtPar
126200130922     c                   eval      RcaNrpar  =h1NrPar
126300130922     c                   eval      RcaSerPar =h1SerPar
126400130922     c                   eval      RcaDtSca  =h1DtSca
126500130922     c                   eval      RcaDtAtt  = *hival
126600131009     c***     k09Rca04      setll     NdRca04L
126700131009     c***     k08Rca04      reade(n)  NdRca04L
126800131009     c     k08Rca01      setgt     NdRca01L
126900131009     c     k08Rca01      readpe(n) NdRca01L
127000130922     c* se c'è una fase aperta errore
127100131009     c                   if        not %eof(NdRca01l)
127200130922     c                             and rcastato = ' '
127300130922     c                   seton                                        5699
127400130922     c                   endif
127500130922     c*
127600130922     c                   endsr
127700131028     C************************************************************
127800131009     C* CONTROLLO sulla fase delle attività  Video D4
127900131009     C************************************************************
128000131009     C     CtrFaseD4     BEGSR
128100131009     C* controlli sulla fase delle attività
128200131009     c                   eval      RcaSocieta=Societa980
128300131009     C                   eval      RcaPosiz=Posattr057
128400131009     c                   eval      RcaKcc    =kcc980
128500131009     c                   eval      RcaKsc    =ksc980
128600131009     c     *DMY          Move      D4DtPar       WrkData
128700131009     c                   eval      RcaDtPar = WrkData
128800131009     c                   eval      RcaNrpar  =d4NrPar
128900131009     c                   eval      RcaSerPar =d4SerPar
129000131009     c     *DMY          Move      D4DtSca       WrkData
129100131009     c                   eval      RcaDtSca = WrkData
129200131009     c********           eval      RcaDtAtt  = *hival
129300131009     c                   eval      RcaFase   = *hival
129400131009     c                   eval      RcaProgr  = *hival
129500131009      *
129600131009     c     k08Rca01      setgt     NdRca01L
129700131009     c     k08Rca01      readpe(n) NdRca01L
129800131009     c* se c'è una fase aperta errore
129900131009     c                   if        not %eof(NdRca01l)
130000131009     c                             and rcastato = ' '
130100131009     c                   seton                                        5699
130200131009     c                   endif
130300131009      *
130400131009     c                   endsr
130500130922     C*************************************************************
130600130922     C* scrittura azione attività manuali su file attività rec.cred
130700130922     C*************************************************************
130800130922     C     ScriviFase    BEGSR
130900130922     c*
131000130922     c* tutti i campi di ndrca rimangono impostati giustamente
131100130922     c* dalla setll readpe su ndrca04l
131200130922     c* gli altri li imposto qui
131300130922     c                   eval      RcaSocieta=Societa980
131400131009     c                   eval      RcaUnita   = H1Unita
131500130922     c                   eval      RcaKcc    =Kcc980
131600130922     c                   eval      RcaKSc    =Ksc980
131700130922     c                   eval      RcaPosiz  =PosAttr057
131800130922     c                   eval      Rcaprogr  =RcaProgr+1
131900130922     c                   eval      RcaAzione =AttAzione
132000130922     c                   eval      RcaGrute  =AttGrute
132100130922     c                   eval      RcaBlk    =BklSca057
132200130922     c                   eval      Rcadtpar  =h1dtpar
132300131001     c                   eval      RcaNrpar  =h1NrPar
132400130922     c                   eval      RcaSerPar =h1SerPar
132500130922     c                   eval      RcaDtSca  =h1DtSca
132600131001     c****               eval      RcaDtAtt  =Zdateiso
132700131001     c     *DMY          Move      S1Dtatt       WrkData
132800131001     c                   eval      RcaDtAtt = WrkData
132900130922     c                   eval      RcaStato = ' '
133000130922     c                   eval      RcaprogrIm=H1Progr
133100130922     c                   clear                   RcaDtlet
133200130922     c                   clear                   RcaNrlet
133300130922     c                   clear                   RcaUtechiu
133400130922     c                   clear                   RcaDtChiu
133500130922     c                   clear                   RcaAnn
133600130922     c                   clear                   RcaCodAnn
133700130922     c                   clear                   RcaFlg
133800131209     c                   clear                   RcaLibero
133900130922     c                   eval      Rcanota=AttNota
134000130922     c*
134100130922     c                   write     NdRca000
134200131206      *
134300131206  "   * richiama pgm User Exit PROVVISORIO !!!
134400131206  "  C                   MOVEL     '01'          XUKOPZ
134500131206  "  C* imposta chiave univoca  per user exit
134600131206  "  C                   MOVE      XSCSOC        XUKSOC
134700131206  "  C                   MOVE      XSCDUN        XUKDUN
134800131206  "  C                   MOVEL     DSPGM         XUKPGM
134900131206  "  C                   MOVEL (P) RRNRCA        XUKFL1
135000131206  "  C                   MOVEL     XUKDS         KPJBU
135100131206  "  C                   CALL      'NDCL69RU'                           21
135200131206  "  C                   PARM                    KPJBA
135300131206      *
135400130922     C                   ENDSR
135500130922     C************************************************************
135600130922     C* cancella relativa fase impegno (SOLO SE NON CHIUSA)
135700130922     C************************************************************
135800130922     C     CancellaFase  BEGSR
135900130922     C*
136000130922     c                   eval      RcaSocieta=Societa980
136100130922     C                   eval      RcaPosiz=Posattr057
136200130922     c                   eval      RcaKcc    =kcc980
136300130922     c                   eval      RcaKsc    =ksc980
136400130922     c                   eval      Rcadtpar  =h1DtPar
136500130922     c                   eval      RcaNrpar  =h1NrPar
136600130922     c                   eval      RcaSerPar =h1SerPar
136700130922     c                   eval      RcaDtSca  =H1DtSca
136800130922     c                   eval      RcaProgrIm=H1Progr
136900130922     C                   eval      RcaAzione=AttAzione
137000130922     c     k10Rca05      chain     NdRca05L                             23
137100130922     c                   if        %found(ndrca05l) and *in23=*off
137200130922     c                             and rcastato=' '
137300130922     c                   delete    NDRCA05
137400130922     c                   endif
137500130922     C*
137600130922     C                   ENDSR
137700131028     C************************************************************
137800131028     C* aggiorna relativa fase impegno (SOLO SE NON CHIUSA)
137900131028     C************************************************************
138000131028     C     AggiornaFase  BEGSR
138100131028     C*
138200131028     c                   eval      RcaSocieta=Societa980
138300131028     C                   eval      RcaPosiz=Posattr057
138400131028     c                   eval      RcaKcc    =kcc980
138500131028     c                   eval      RcaKsc    =ksc980
138600131028     c                   eval      Rcadtpar  =h1DtPar
138700131028     c                   eval      RcaNrpar  =h1NrPar
138800131028     c                   eval      RcaSerPar =h1SerPar
138900131028     c                   eval      RcaDtSca  =H1DtSca
139000131028     c                   eval      RcaProgrIm=H1Progr
139100131028     C                   eval      RcaAzione=H1Azione
139200131028     c     k10Rca05      chain     NdRca05L                             23
139300131028     c                   eval      RcaAzione =S1Azione
139400131028     c                   if        %found(ndrca05l) and *in23=*off
139500131028     c                             and rcastato=' '
139600131028     c     *DMY          Move      S1Dtatt       WrkData
139700131028     c                   eval      RcaDtAtt = WrkData
139800131029     c                   eval      RcaNota = S1Nota
139900131029     c                   eval      RcaGrute = S1Grute
140000131028     c                   update    NDRCA05
140100131028     c                   endif
140200131028     C*
140300131028     C                   Endsr
140400131028     C************************************************************
140500131028     C
140600940131     C* INIZIALIZZAZIONE LISTA
140700940127     C************************************************************
140800940127     C     INZS1         BEGSR
140900940302     C* pulizia SFL
141000940128     C                   SETOFF                                         3031
141100940131     C                   WRITE     FMTC1
141200940128     C                   SETON                                          31
141300940127     C*
141400940128     C* riempimento opzioni
141500940223     C                   MOVEA     $TX1          $TX
141600940223     C                   MOVEA     $FL1          $FL
141700940309     C                   Z-ADD     0             WCOZ
141800960117     C                   Z-ADD     0             WMXO
141900940225     C*  equivale al num.di elementi della schiera $TX1
142000960117     C                   Z-add     OpzLen        WMXO
142100950906     C                   Z-ADD     $C1Op1        $L01
142200950906     C                   Z-ADD     $C1Op2        $L02
142300940128     C                   EXSR      RIEOPZ
142400941214     C                   Z-ADD     23            X
142500941214     C                   MOVE      FF23          $FC2(X)
142600940608     C                   MOVEL     WOPZ1         C1OPZ1
142700060224     C                   MOVEL     $ALLOPT       H1ALLOPT
142800940608     C                   MOVEL     WOPZ2         C1OPZ2
142900940225     C                   Z-ADD     WCOZ          C1COZ
143000940128     C*
143100940128     C* CARICAMENTO SFL A PAGINE
143200940128     C*
143300940201     C                   Z-ADD     0             S1NRR
143400940128     C                   Z-ADD     0             WMAX
143500090312     C                   Z-ADD     0             Z2TotSCAD
143600090312     c* CARICAMENTO CONTROL
143700090312     c                   exsr      RieC1
143800940224     C*
143900940224     C* Posizionamento su file pilota
144000940302     C                   CLEAR                   C1POS
144100941214     C                   EXSR      SETANA
144200940302     C* ripristino i valori del posizionamento
144300940302     C                   MOVE      W1POS         C1POS
144400940131     C*
144500061113     C* Lettura file pilota (si legge tutto subito per dare totali)
144600941214     C                   EXSR      REDANA
144700940131     C*
144800970318     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
144900970318     C                   IF        $LastV = 'D1'
145000970318     C                   Z-ADD     SFLPAG        WPAG
145100970318     C                   ENDIF
145200940127     C                   EXSR      ROLS1
145300970318     C* imposto il caricamento sfl del numero massimo di righe per pagina
145400970318     C                   Z-ADD     SFLPAG        WPAG
145500061116     c* sempre prima pagina
145600061116     C                   z-add     1             C1RCD
145700940127     C*
145800940127     C                   ENDSR
145900940127     C/EJECT
146000940127     C************************************************************
146100940131     C* CARICAMENTO PAGINA LISTA
146200940127     C************************************************************
146300940127     C     ROLS1         BEGSR
146400940127     C*
146500940128     C                   SETOFF                                       32
146600940223     C                   Z-ADD     0             Y
146700940127     C                   Z-ADD     WMAX          S1NRR
146800090312     C                   Z-ADD     S1NRR         WMAX                 30
146900940127     C*
147000061113     c* si carica tutto per dare i totali subito
147100940127     C*
147200940131     C                   EXSR      RIES1
147300940127     C*
147400090312     C                   Z-ADD     S1NRR         C1RCD
147500090312     c*
147600090312     C                   Z-ADD     S1NRR         WMAX                 30
147700940127     C*
147800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
147900940127     C*
148000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
148100940127     C                   MVR                     RESTO             3 0
148200940131     C     PAGINE        MULT      SFLPAG        C1RCD
148300941027     C     RESTO         IFGT      0
148400940127     C                   ADD       1             C1RCD
148500941027     C                   ELSE
148600940407     C                   SUB       SFLPAG        C1RCD
148700940407     C                   ADD       1             C1RCD
148800941027     C                   ENDIF
148900130917     C*
149000130917     C                   Movel     $EndFile      *in33
149100130917     C*
149200940127     C                   ENDSR
149300940224     C/EJECT
149400090312     C************************************************************
149500090312     C* riempimento dati control
149600090312     C************************************************************
149700090312     C     RieC1         BEGSR
149800100426     c*
149900130915     c                   clear                   WrkAttr           4
150000090312     c* conto
150100130915     C                   eval      c1kcc=kcc980
150200130915     C                   eval      c1ksc=ksc980
150300090312     C* Richiamo routine per decodifica dati conto
150400090312     C                   Clear                   ND002DS
150500090312     C                   EVAL      LenOut = %Size(ND002DS)
150600090312     C                   CallB     'NDMVC002'
150700130915     C                   PARM                    societa980
150800130915     C                   PARM                    kcc980
150900130915     C                   PARM                    ksc980
151000090312     C                   PARM      *loval        dataiso
151100090312     C                   PARM      *OFF          GesMsg
151200090312     C                   PARM      *OFF          Esito
151300090312     C                   PARM      'ND002DS'     Struttura
151400090312     C                   PARM                    ND002DS
151500090312     C                   PARM                    LenOut
151600090312     C*
151700090312     C     Esito         ifeq      *ON
151800090312     C                   CLEAR                   ND002DS
151900090312     C                   else
152000130915     c                   if        PosAttr057 <> *blanks
152100130915     c                   movel     Categorie     DsCateg
152200130915     c                   eval      WrkAttr  = SkCateg(Posiz)
152300130915     c                   endif
152400090312     C                   eval      c1kscd   = descr002
152500090312     c                   endif
152600090312     C* dati ultimo sollecito
152700090312     c                   reset                   NDCL99DS
152800130915     c                   eval      societal99=societa980
152900130915     c                   eval      UnitaL99 =unita980
153000090312     c                   eval      ctbl99='CG'
153100130915     c                   eval      kccl99=Kcc980
153200130915     c                   eval      kscl99=ksc980
153300090312     c                   eval      SolleciL99=*off
153400090312     c                   eval      ImpegniL99=*off
153500090312     c                   eval      ContenzL99=*off
153600090312     c                   eval      RatingL99 =*off
153700090312     c                   eval      BlocchiL99=*off
153800090312     c                   eval      UltimoSL99=*on
153900090312     c                   eval      kpjbu=ndcl99ds
154000090312     C*
154100090312     c                   CALL      'NDCL99R'
154200090312     C                   parm                    kpjba
154300090312     c                   eval      ndcl99ds = kpjbu
154400090312     c                   if        UltimoSL99=*on
154500090312     c     *jobrun       move      DtUlSolL99    c1DtSoll
154600090312     c                   z-add     NrUlSolL99    c1Nrsoll
154700090312     c                   move      GravsolL99    c1Gravita
154800090312     c                   z-add     ImpsolL99     c1ImpSoll
154900090312     c                   endif
155000061116     C*
155100090312     C                   ENDSR
155200090312     C/EJECT
155300940224     C************************************************************
155400061115     C* POSIZIONAMENTO lettura ds multipla
155500940224     C************************************************************
155600941214     C     SETANA        BEGSR
155700061108     c*
155800130915     c                   reset                   NDC980Ds1
155900061113     c* pulisco ds multipla
156000090310     c                   do        1000          $x
156100130915     c     $x            occur     ndc980ds1
156200130915     c                   clear                   ndc980ds1
156300061113     c                   enddo
156400061108     c*
156500940224     C                   ENDSR
156600940128     C/EJECT
156700940128     C************************************************************
156800940131     C* LETTURA RCD ARCHIVIO PILOTA
156900940128     C************************************************************
157000941214     C     REDANA        BEGSR
157100940128     C*
157200950606     C                   MOVEL     *OFF          $EndFile
157300970910     C*
157400950606     C     $EndFile      DOUEQ     *ON
157500940131     C*
157600130915     c                   call      'NDC980R1'
157700130915     C                   parm                    NDC980DS
157800130915     C                   parm                    NDC980DS1
157900090310     C                   PARM                    TotScaduto
158000090310     C                   PARM                    PrgFittizie
158100090312     C                   PARM                    Errore            1
158200140313     C                   PARM                    $F10mult
158300090511     C*
158400090511     C                   MOVEL     *ON           $EndFile
158500940131     C*
158600941027     C                   ENDDO
158700940131     C*
158800940131     C                   ENDSR
158900940224     C/EJECT
159000940224     C************************************************************
159100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
159200940224     C************************************************************
159300940224     C     CLCPAG        BEGSR
159400940224     C* Input :
159500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
159600940224     C*          cursore
159700940224     C* - SFLPAG = numero rcd per pagina sfl
159800940224     C* Output :
159900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
160000940224     C*
160100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
160200940224     C                   MVR                     RESTO             3 0
160300940224     C     RESTO         IFGT      0
160400940224     C                   ADD       1             PAGINE
160500940224     C                   ENDIF
160600940224     C     PAGINE        MULT      SFLPAG        WPAG
160700940224     C*
160800940224     C                   ENDSR
160900940131     C/EJECT
161000940131     C************************************************************
161100061115     C* RIEMPIMENTO RIGA LISTA da ds multipla
161200940131     C************************************************************
161300940131     C     RIES1         BEGSR
161400940131     C*
161500131010     C                   clear                   savnrpar
161600131010     C                   clear                   savdtpar
161700131010     C                   clear                   savserpar
161800131010     C*
161900090310     c                   do        1000          $X                5 0
162000090316     C                   CLEAR                   FMTS1
162100130915     c     $x            occur     ndc980ds1
162200090312     c* anche le fittizie hanno un numero
162300130915     c                   if        nrpar980 = 0
162400090312     c                   leave
162500090312     c                   endif
162600090316     c*
162700130915     c                   move      unita1980     h1unita
162800130915     c                   move      dtpar980      h1dtpar
162900130915     c                   z-add     nrpar980      h1nrpar
163000130915     c                   move      serpar980     h1serpar
163100130915     c                   move      dtsca980      h1dtsca
163200130915     c                   z-add     progr980      h1progr
163300130915     c* in alta intensità la partita selezionata
163400130915     c                   if        h1dtpar = dtpar980x and
163500130915     c                             h1nrpar = nrpar980x and
163600130915     c                             h1serpar= serpar980x
163700130915     c                   move      *on           h1in17
163800130915     c                   seton                                        17
163900130915     c                   endif
164000070928     c*
164100090312     c* partita
164200130915     c                   if        dtpar980 <> *loval
164300130915     c     *jobrun       move      dtpar980      s1dtpar
164400130915     c                   z-add     nrpar980      s1nrpar
164500090312     c                   endif
164600130915     c                   if        dtsca980 <> *loval
164700130915     c     *jobrun       move      dtsca980      s1dtsca
164800090312     c                   endif
164900130916     c                   z-add     impscad980    s1impscad
165000130915     c* azione
165100130915     c                   eval      H1Azione = azione980
165200130915     c                   if        azione980 <> *blanks
165300130915     c                   eval      S1azione = azione980
165400090312     C                   MOVE      '1'           XTBRIC
165500090312     C                   MOVE      xscsoc        XTBAZI
165600090312     C                   MOVE      *ZERO         XTBKEY
165700130915     C                   MOVE      '047'         XTBCOD
165800130915     C                   MOVE      azione980     XTBKEY
165900090312     C                   CALLb     'XATB'
166000090312     C                   PARM                    XATBDS
166100090312     C     XTBERR        IFEQ      '0'
166200130915     C                   MOVEL     XTBUNI        ang047ds
166300130915     C                   eval      S1AzioneD=    des047
166400130915     C                   endif
166500130915     C                   endif
166600130915     C* Gruppo utenti destinatari
166700130915     C                   eval      S1Grute  = Grute980
166800130915     C* Data attività
166900130915     C                   if        DtAtt980 <> *loval
167000130915     C     *jobrun       move      DtAtt980      S1DtAtt
167100130915     C                   endif
167200130915     C                   movel     Nota980       S1Nota
167300130915     C* faccio vedere i dati di partita solo alla prima riga
167400130915     C                   if        savdtpar = s1dtpar and
167500130915     C                             savnrpar = s1nrpar and
167600130915     C                             savserpar= h1serpar
167700130915     C                   clear                   s1dtpar
167800130915     C                   clear                   s1nrpar
167900130915     C                   else
168000130915     C                   eval      savdtpar=s1dtpar
168100130915     C                   eval      savnrpar=s1nrpar
168200130915     C                   eval      savserpar=h1serpar
168300130915     C                   endif
168400140313      *
168500140313     c                   if        $F10mult = *on
168600140313     c                   add       1             h1progr
168700140313     c                   clear                   s1dtatt
168800140313     c                   clear                   s1azione
168900140313     c                   clear                   h1azione
169000140313     c                   clear                   s1azioned
169100140313     c                   clear                   s1nota
169200140313     c                   endif
169300130915     C*
169400071001     C                   ADD       1             S1NRR
169500071001     C                   ADD       1             Y
169600140319      *
169700140319     C                   exsr      RepStatoFase
169800140319      *
169900140319     C* imposto quegli attributi del sfl che sono modificabili
170000140319     C* al richiamo del determinato pgm
170100140319     C                   EXSR      ATRS1
170200071001     C*
170300061113     C                   WRITE     FMTS1
170400130915     C                   enddo
170500940131     C*
170600940131     C                   ENDSR
170700950301     C/EJECT
170800950301     C************************************************************
170900950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
171000950301     C************************************************************
171100950301     C     ATRS1         BEGSR
171200950301     C*
171300971120     C* in questa routine bisogna attivare indicatori o campi programma
171400971120     C* che gestiscono gli attributi di uno o più campi di questa videata;
171500971120     C* attenzione : non è questo il posto in cui eseguire controlli,
171600971120     C*  dovrebbero già arrivare dei flag
171700100426     C* in alta intensità la partita selezionata
171800100426     C                   MOVE      H1IN17        *IN17
171900140319     C                   if        H1Stato <> *blank
172000140319     C                   move      *on           *in18
172100140319     c                   endif
172200950301     C*
172300950301     C                   ENDSR
172400941206     C/EJECT
172500090406     C************************************************************
172600090406     C* GESTIONE F05 VIDEO S1
172700090406     C************************************************************
172800090406     C     F05S1         BEGSR
172900090406     C*
173000090406     C                   MOVE      *ON           $inzs1
173100090406     C                   MOVE      *HIVAL        $LASTV
173200090406     C*
173300090406     C                   ENDSR
173400090406     C/EJECT
173500941206     C************************************************************
173600941206     C* GESTIONE F08 VIDEO S1
173700941206     C************************************************************
173800941206     C     F08S1         BEGSR
173900941206     C*
174000941206     C                   MOVE      *ON           $FINE
174100130915    >C                   MOVE      '0'           RET980
174200941206     C*
174300941206     C                   ENDSR
174400940309     C/EJECT
174500940309     C************************************************************
174600940309     C* GESTIONE F03 VIDEO S1
174700940309     C************************************************************
174800940309     C     F03S1         BEGSR
174900940309     C*
175000940309     C                   MOVE      *ON           $FINE
175100130915    >C                   MOVE      '1'           RET980
175200940325     C* fine programma
175300940325     C                   EXSR      ENDPGM
175400940309     C*
175500940309     C                   ENDSR
175600940309     C/EJECT
175700140313C2156 ************************************************************
175800140313      * Gestione Immissione altri impegni
175900140313      ************************************************************
176000140313c2156C     F10S1         BEGSR
176100140313     C*
176200140313     C                   MOVE      *ON           $inzs1
176300140313     C                   MOVE      *HIVAL        $LASTV
176400140313     C                   MOVE      *ON           $F10Mult
176500140313     C*
176600140313c2156C     XF11S1        ENDSR
176700130915      ************************************************************
176800130915      * Gestione Immissione altra attività
176900100423      ************************************************************
177000140313     C     F10S1old      BEGSR
177100100423     C*
177200100423     C* determino Riga/Colonna del cursore
177300100423     C                   MOVE      NRG           R$$
177400100423     C                   MOVE      NCL           C$$
177500100423     C                   Z-ADD     RIRI          CURR
177600100423     C                   Z-ADD     COCO          CURC
177700100423     C*
177800100423     C* se sono posizionato sul sfl
177900130915   B1C     H1NMRC        IFEQ      'C980S1'
178000100423     C* leggo il rcd
178100100423     C     C1NRR         CHAIN     FMTS1                              24
178200100423     C*
178300100423     C     *In24         IFEQ      *Off
178400100423     C* imposto gli attributi del sfl
178500100423     C                   EXSR      AtrS1
178600100423     C                   UPDATE    FMTS1
178700100423     C                   MOVE      *OFF          *In32
178800100423     C* gestione videata immissione
178900100427     c                   clear                   $AggiornaI        1
179000100423     c                   move      'D4'          $Gest
179100100423     c                   move      *on           $Inzd4            1
179200130915     C                   move      *off          $FineD4           1
179300100423     C     $FineD4       doweq     *off
179400100423     C                   exsr      GESD4
179500130915     C                   enddo
179600130915     C*
179700130915     C* ricarico sfl se digitato F6
179800130915     C                   if        $AggiornaI= *on
179900130915     C                   move      *on           $inzs1
180000130915     C                   move      'S1'          $gest
180100130915     C                   ENDIF
180200130915     C*
180300130915     C                   ENDIF
180400100423     C*
180500100423     C                   ENDIF
180600100423     C*
180700100423     C* imposto pos. del cursore
180800100423     C                   Z-ADD     CURR          H1RIGA
180900100423     C                   Z-ADD     CURC          H1COLO
181000100423     C*
181100130915     C     XF10S1        ENDSR
181200940309     C************************************************************
181300940309     C* GESTIONE F12 VIDEO S1
181400940309     C************************************************************
181500940309     C     F12S1         BEGSR
181600940309     C*
181700940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
181800940309     C     $LOP          IFEQ      1
181900940309     C                   MOVE      'D1'          $GEST
182000940309     C*  se la 1a videata è questa, per F12 torno a menu
182100940309     C                   ELSE
182200940309     C                   MOVE      *ON           $FINE
182300130915    >C                   MOVE      '2'           RET980
182400941027     C                   ENDIF
182500131025     C*
182600131025     C                   ENDSR
182700131025     C/EJECT
182800131025     C************************************************************
182900131025     C* GESTIONE F13 VIDEO S1
183000131025     C************************************************************
183100131025     C     F13S1         BEGSR
183200131025     C* Pulisco i campi di salvataggio
183300131025C2226C                   Clear                   SavAzione
183400131025C2226C                   Clear                   SavGrUte
183500131025C2226C                   Clear                   SavDtAtt
183600131025C2226C                   Clear                   SavNota
183700131025     C* il controllo lo faccio solo se il sfl NON è vuoto
183800131025   1bC                   If        WMax    <>    0
183900131025     C* determino Riga/Colonna del cursore
184000131025     C                   MOVE      NRG           R$$
184100131025     C                   MOVE      NCL           C$$
184200131025     C                   Z-ADD     RIRI          CURR
184300131025     C                   Z-ADD     COCO          CURC
184400131025     C*
184500131025     C* se sono posizionato sul sfl
184600131025     C* salvo il numero del record su cui sono posizionato
184700131025     C                   Z-Add     C1Nrr         WC1Rcd
184800131025     C* leggo il rcd
184900131025     C     C1NrR         CHAIN     FMTS1                              21
185000131025     C* salvo i dati su cui sono posizionato
185100131025     C                   If        S1Azione <> ' ' or
185200131025     C                             S1DtAtt  <> 0   or
185300131025     C                             S1Grute  <> *blanks or
185400131025     C                             S1Nota   <> *blanks
185500131025     C                   Movel     S1Azione      SavAzione
185600131025     C                   Move      S1DtAtt       SavDtAtt
185700131025     C                   Move      S1Grute       SavGrUte
185800131025     c                   Move      S1Nota        SavNota
185900131025     c                   Z-Add     C1Nrr         $X
186000131025     C* attivo SflNxtChg
186100131025     C                   SetOn                                        32
186200131025     c                   exsr      Atrs1
186300131025     C                   UpDate    FmtS1
186400131025     C                   SetOff                                       32
186500131025     C*
186600131025     C* mi posiziono sul record successivo
186700131025     C                   Add       1             $X
186800131025     C*
186900131025     C                   DoW       $X      <=    Wmax
187000131025     C     $X            Chain     FmtS1                              21
187100131025     C   21              Leave
187200131025     C* Riporto i dati solo se non già valorizzati
187300131025     C                   if        S1Azione = *blanks
187400131025     C                             and S1GrUte  = *blanks
187500131025     C                             and S1DtAtt  = 0
187600131025     C                             and S1Nota   = *blanks
187700131025     C                   Movel     SavAzione     S1azione
187800131025     C                   Movel     SavGrUte      S1GrUte
187900131025     C                   Move      SavDtAtt      S1DtAtt
188000131025     C                   Movel     SavNota       S1Nota
188100131025     C                   Endif
188200131025     C* imposto l'opzione solo se è possibile eseguirla
188300131025     c*
188400131025     C* attivo SflNxtChg
188500131025     C                   SetOn                                        32
188600131025     C                   exsr      Atrs1
188700131025     C                   UpDate    FmtS1
188800131025     C                   SetOff                                       32
188900131025     C*
189000131025     C                   Add       1             $X
189100131025     C                   enddo
189200131025     c*
189300131025     C                   Eval      WPag     =    9999
189400131025     C*
189500131025     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
189600131025     C*
189700131025     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
189800131025     C                   MVR                     RESTO             3 0
189900131025     C     PAGINE        MULT      SFLPAG        C1RCD
190000131025     C     RESTO         IFGT      0
190100131025     C                   ADD       1             C1RCD
190200131025     C                   ELSE
190300131025     C                   SUB       SFLPAG        C1RCD
190400131025     C                   ADD       1             C1RCD
190500131025     C                   ENDIF
190600131025     C*
190700131025     C* reimposto sflpag
190800131025     C                   Eval      WPag     =    SflPag
190900131025     C*
191000131025     C* ripristino la posizione iniziale del record
191100131025     c                   Z-Add     WC1Rcd        C1Rcd
191200131025     C*
191300131025     C                   endif
191400131025     C*
191500131025     C                   endif
191600131025     C*
191700940309     C*
191800940309     C                   ENDSR
191900940309     C/EJECT
192000940309     C************************************************************
192100940309     C* GESTIONE F17 VIDEO S1
192200940309     C************************************************************
192300940309     C     F17S1         BEGSR
192400940309     C*
192500940309     C                   MOVE      'D1'          $GEST
192600941104     C* se il pgm è stato richiamato con chiave univoca
192700941104     C* e dopo è stata richiesta una parzializzazione
192800941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
192900941104     C* valore; propongo come ordinamento il 1°)
193000950606     C     $Unique       IFEQ      *ON
193100950606     C                   MOVE      *OFF          $Unique
193200941027     C                   ENDIF
193300940831     C     $ORD          IFEQ      99
193400940831     C                   Z-ADD     1             $ORD
193500941104     C                   MOVE      *ON           $INZD1
193600941027     C                   ENDIF
193700940309     C*
193800940309     C                   ENDSR
193900940309     C/EJECT
194000940309     C************************************************************
194100940309     C* GESTIONE F18 VIDEO S1
194200940309     C************************************************************
194300940309     C     F18S1         BEGSR
194400940309     C*
194500940309     C                   MOVE      'D2'          $GEST
194600940309     C                   MOVEL     *ON           $INZD2
194700940831     C     $ORD          IFEQ      99
194800940831     C                   Z-ADD     1             $ORD
194900941027     C                   ENDIF
195000940309     C*
195100940309     C                   ENDSR
195200940309     C/EJECT
195300940309     C************************************************************
195400940309     C* GESTIONE F23 VIDEO S1
195500940309     C************************************************************
195600940309     C     F23S1         BEGSR
195700940309     C*
195800950906     C                   Z-ADD     $C1Op1        $L01
195900950906     C                   Z-ADD     $C1Op2        $L02
196000940309     C                   EXSR      RIEOPZ
196100941214     C                   Z-ADD     23            X
196200941214     C                   MOVE      FF23          $FC2(X)
196300940608     C                   MOVEL     WOPZ1         C1OPZ1
196400060224     C                   MOVEL     $ALLOPT       H1ALLOPT
196500940608     C                   MOVEL     WOPZ2         C1OPZ2
196600940309     C                   Z-ADD     WCOZ          C1COZ
196700940309     C*
196800940309     C                   ENDSR
196900940309     C/EJECT
197000940309     C************************************************************
197100940309     C* GESTIONE F24 VIDEO S1
197200940309     C************************************************************
197300940309     C     F24S1         BEGSR
197400090423     C*
197500090423     c* totali
197600090423     c                   z-add     TotScaduto    z2TotScad
197700940506     C*
197800940506     C                   MOVEA     $FX2          $FX
197900940506     C                   MOVEA     $FC2          $FC
198000940506     C                   MOVEA     $FR2          $FR
198100940506     C                   Z-ADD     $ULKS1        $ULK
198200950726     C                   Z-ADD     $S1L01        $L01
198300950726     C                   Z-ADD     $S1L02        $L02
198400940506     C                   EXSR      RIEKEY
198500940506     C                   Z-ADD     $ULK          $ULKS1            3 0
198600940720     C                   MOVEA     $FC           $FC2
198700070927     C                   MOVEL     $KEY1         Z2KE
198800061108     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
198900070927     C                   MOVEL     $KEY2         Z2KE2
199000070927     c* riga unica tasti
199100070927     c                   eval      $A = %triml(z2ke)
199200070927     c                   eval      $B = %triml(z2ke2)
199300070927     c     $A            cat(P)    $B:0          z2ke1
199400061108     C                   WRITE     FMTZ2
199500940506     C*
199600940309     C                   ENDSR
199700940128     C/EJECT
199800940128     C************************************************************
199900940131     C* CONTROLLO TESTATA LISTA
200000940128     C************************************************************
200100940131     C     CTRC1         BEGSR
200200940128     C*
200300940201     C                   MOVE      *OFF          *IN99
200400940131     C*
200500940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
200600940202     C* L'INIZIALIZZAZIONE LISTA
200700940202     C*
200800941214     C     C1POS         IFNE      W1POS
200900940202     C                   EXSR      POSS1
201000941027     C                   ENDIF
201100940202     C*
201200940202     C                   ENDSR
201300940202     C/EJECT
201400940202     C************************************************************
201500940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
201600940202     C************************************************************
201700940202     C     POSS1         BEGSR
201800940202     C*
201900940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
202000940202     C*
202100941118   B1C     WMAX          IFGT      0
202200940202     C                   READC     FMTS1                                  21
202300941118   B2C     *IN21         DOWEQ     *OFF
202400940202     C     *IN99         ANDEQ     *OFF
202500940202     C*
202600941118   B3C     S1OPZ         IFNE      0
202700960109     C                   SETON                                        92  99
202800941118   E3C                   ENDIF
202900940202     C* se rilevata opzione attiva, attivo sflnxtchg
203000941118   B3C     *IN99         IFEQ      *ON
203100940202     C                   MOVE      *ON           *IN32
203200950301     C*
203300950301     C* imposto quegli attributi del sfl che sono modificabili
203400950301     C* al richiamo del determinato pgm
203500941118     C                   EXSR      ATRS1
203600941118     C*
203700940202     C                   UPDATE    FMTS1
203800941118   X3C                   ELSE
203900940202     C                   READC     FMTS1                                  21
204000941118   E3C                   ENDIF
204100940202     C*
204200941118   E2C                   ENDDO
204300940202     C*
204400941118   E1C                   ENDIF
204500940202     C*
204600940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
204700940221     C* AL VALORE DI POSIZIONAMENTO :
204800940221     C* se questo è maggiore del valore del rispettivo campo di
204900940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
205000940221     C* con i valori di parzializzazione.
205100940221     C*
205200941118   B1C     *IN99         IFEQ      *OFF
205300940721    >C                   MOVE      *BLANK        W1POS
205400940202     C*
205500950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
205600950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
205700950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
205800950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
205900950525     C* verificata la condizione suddetta avverrà con posizionamento a
206000950525     C* partire dal valore di parzializzazione
206100950525     C*
206200941118   B2C                   SELECT
206300940202     C*
206400940202     C     $ORD          WHENEQ    1
206500950525     C*
206600950525     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
206700950525     C*
206800941118   E2C                   ENDSL
206900950525     C*
207000950525     C* richiedo l'inizializzazione sfl
207100950525     C                   MOVE      *ON           $INZS1
207200940202     C*
207300941118   E1C                   ENDIF
207400940202     C*
207500940131     C                   ENDSR
207600940131     C/EJECT
207700940131     C************************************************************
207800940131     C* CONTROLLO OPZIONI LISTA
207900940131     C************************************************************
208000940131     C     CTRS1         BEGSR
208100940131     C*
208200940202     C                   MOVEL     *OFF          $ESCI
208300940201     C                   SETOFF                                       99
208400940131     C                   Z-ADD     0             S1OPZ
208500940131     C*
208600940127     C* Leggo il sfl solo se ci sono rcd
208700941118   B1C     WMAX          IFGT      0
208800940131     C                   READC     FMTS1                                  21
208900940127     C*
209000940131     C* esce se fine sfl o errore che richiede l'uscita
209100941118   B2C     *IN21         DOWEQ     *OFF
209200940131     C     $ESCI         ANDEQ     *OFF
209300940201     C                   Z-ADD     S1NRR         C1RCD
209400940131     C* ctrl su riga
209500940131     C                   EXSR      RECS1
209600940131     C* gestione opzioni
209700941118   B3C     S1OPZ         IFNE      0
209800940201     C     *IN99         ANDEQ     *OFF
209900940131     C                   EXSR      OPZS1
210000941118   E3C                   ENDIF
210100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
210200941118   B3C     *IN99         IFEQ      *ON
210300940131     C                   MOVE      *ON           *IN32
210400941118   E3C                   ENDIF
210500950301     C*
210600950301     C* imposto quegli attributi del sfl che sono modificabili
210700950301     C* al richiamo del determinato pgm
210800941118     C                   EXSR      ATRS1
210900941118     C*
211000940131     C                   Z-ADD     0             S1OPZ
211100940223     C*
211200940127     C                   UPDATE    FMTS1
211300941125     C                   MOVE      *OFF          *IN32
211400940223     C*
211500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
211600941118   B3C     $ESCI         IFEQ      *OFF
211700940131     C                   READC     FMTS1                                  21
211800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
211900941118   B4C     *IN21         IFEQ      *ON
212000940201     C                   MOVE      WINZS1        $INZS1
212100010620     C                   MOVE      *off          WINZS1
212200980603     C* se è avvenuta almeno una modifica
212300980603   B5C                   IF        $INZS1 = *on
212400940204     C* calcolo pagina a cui deve posizionarsi
212500940728     C     WSFL          IFGT      0
212600940728     C                   EXSR      CLCPAG
212700940728     C                   ELSE
212800940728     C                   Z-ADD     SFLPAG        WPAG
212900940728     C                   ENDIF
213000980603   E5C                   ENDIF
213100941118   E4C                   ENDIF
213200980603   E3C                   ENDIF
213300940131     C*
213400941118   E2C                   ENDDO
213500940127     C*
213600941118   E1C                   ENDIF
213700940131     C*
213800940127     C                   ENDSR
213900940127     C/EJECT
214000940127     C************************************************************
214100940131     C* CONTROLLO CAMPI I/O RIGA LISTA
214200940127     C************************************************************
214300940131     C     RECS1         BEGSR
214400940131     C*
214500940201     C* reset indicatori DSPATR
214600940201     C                   MOVE      *ALL'0'       IN4049           10
214700940201     C                   MOVEA     IN4049        *IN(40)
214800130915     C*
214900130915     C* data impegno obbligatoria se inserita l'azione
215000130915     C                   if         S1DtAtt =  0 and S1Azione<> *blanks
215100130915     C                   seton                                        5199
215200130915     C                   endif
215300090316     C*
215400130915     C* azione obbligatoria se inserito qualcosa
215500130915     c                   if        (S1DtAtt <> 0 or
215600130915     c                              S1Grute <> *blanks or
215700130915     c                              S1nota  <> *blanks )
215800090316     C                             AND
215900130915     C                             S1Azione = *blanks
216000090316     c                   seton                                        5399
216100090316     c                   endif
216200970424     C*
216300130915     C* Azione
216400130915     c                   Clear                   S1AzioneD
216500130915     c                   if        S1Azione <> *blanks
216600090316     C                   MOVE      '1'           XTBRIC
216700090316     C                   MOVE      XSCSOC        XTBAZI
216800090316     C                   MOVE      *ZERO         XTBKEY
216900130915     C                   MOVE      '047'         XTBCOD
217000130915     C                   MOVE      S1Azione      XTBKEY
217100090316     C                   CALLb     'XATB'
217200090316     C                   PARM                    XATBDS
217300090316     C     XTBERR        IFEQ      *Off
217400130915     C                   MOVEL     XTBUNI        ang047ds
217500130915     C                   MOVEL     DEs047        S1AzioneD
217600130915     C* solo tipo blank (azione libera)
217700130915     C                   if        TipAtt047 <> *blanks
217800130915     C                   seton                                        5599
217900130915     C                   endif
218000130915     C                   else
218100130915     C                   seton                                        5099
218200130915     C                   endif
218300090316     C                   ENDIF
218400090316     C*
218500130923     C* destinatari
218600130923     C                   If        S1Grute <> *blanks
218700130923     C                   MOVE      '1'           XTBRIC
218800130923     C                   MOVE      XSCSOC        XTBAZI
218900130923     C                   MOVE      *ZERO         XTBKEY
219000130923     C                   MOVE      '048'         XTBCOD
219100130923     C                   MOVE      S1Grute       XTBKEY
219200130923     C                   CALLb     'XATB'
219300130923     C                   PARM                    XATBDS
219400130923     C     XTBERR        IFEQ      *Off
219500130923     C                   MOVEL     XTBUNI        ang048ds
219600130923     C                   else
219700130923     C                   seton                                        5499
219800130923     C                   endif
219900130923     C                   endif
220000130915     C* data attività
220100130915     C                   If        S1DtAtt <> 0
220200090316     c* controllo validità formale
220300130915     C     *JobRun       Test(D)                 S1DtAtt                15
220400090316     C                   If        *in15 = *on
220500090316     c                   seton                                        5199
220600090316     c                   endif
220700090316     c                   endif
220800131024      * se immissione controllo data attività maggiore/uguale oggi
220900131024     c                   if        h1azione=  *blanks
221000131024     c                             and s1azione<> *blanks
221100131024     c                             and S1DTATT <> *ZERO
221200131024     c                             and *IN99 = *OFF
221300131024     c                   if        %date(S1DtAtt:*jobrun) < zdateiso
221400131024     c                   seton                                        5199
221500131024     c                   endif
221600131024     c                   endif
221601140530      *
221602140530      * se immissione controllo data attività ENTRO 30 GG.
221603140530     c                   if        h1azione=  *blanks
221605140530     c                             and s1azione<> *blanks
221606140530     c                             and S1DTATT <> *ZERO
221607140530     c                             and *IN99 = *OFF
221608140530     c     zdateiso      adddur    1:*months     wrkdata
221609140530     c                   if        %date(S1DtAtt:*jobrun) > wrkdata
221610140530     c                   seton                                        5199
221614140530     c                   endif
221615140530     c                   endif
221700131028      *
221800131028      * controlli sulla fase delle attività solo udate!!
221900131029     c                   if        h1azione<>*blanks
222000131029     c                             and *in99 = *off
222100131028     c                   exsr      CtrAggFase
222200131029     c                   endif
222300131028      *
222400131024      * controlli sulla fase delle attività solo immissione!!
222500130922     c                   if        h1azione=  *blanks and
222600130922     c                             s1azione<> *blanks
222700131029     c                             and *in99 = *off
222800130922     c                   exsr      CtrFase
222900130922     c                   endif
223000131021      *
223100100426     C*
223200090316      * ripristino attributi campi sfl
223300090316     C*
223400090316     C     *IN99         ifeq      *on
223500090316     C     $esci         oreq      *on
223600090316     C                   Z-ADD     s1nrr         C1RCD
223700090316     C                   move      *on           $esci
223800090316     C                   endif
223900090316     C*
224000940131     C                   ENDSR
224100941118     C/EJECT
224200941118     C************************************************************
224300941118     C* GESTIONE OPZIONI LISTA
224400941118     C************************************************************
224500941118     C     OPZS1         BEGSR
224600950301     C*
224700950301     C                   Z-ADD     RIRI          CURR
224800950301     C                   Z-ADD     COCO          CURC
224900941118     C*
225000941027     C                   SELECT
225100940225     C*
225200950620     C* se opz=Selezione
225300950620     C* passo la chiave al pgm chiamante
225400940225     C     S1OPZ         WHENEQ    1
225500130915    >C                   MOVE      S1OPZ         OPZ980
225600130915     C* passare nei campi appositi della DS NDC980DS il valore
225700950301     C* della chiave della VL univoca (se c'è)
225800940225     C                   MOVE      *ON           $ESCI
225900940225     C                   MOVE      *ON           $FINE
226000940909     C*
226100940909     C                   MOVE      *HIVAL        $LASTV
226200940225     C*
226300950620     C* se altre opzioni
226400950620     C* richiamo pgm gestione anagrafica
226500941027     C                   OTHER
226600940225     C*
226700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
226800941027     C     *IN99         IFEQ      *ON
226900940223     C                   SETON                                        40
227000941027     C                   ENDIF
227100940131     C*
227200940506     C                   MOVE      *HIVAL        $LASTV
227300941027     C                   ENDSL
227400940225     C*
227500940131     C                   ENDSR
227600940223     C/EJECT
227700940131     C************************************************************
227800940131     C* GESTIONE VALORI ASSUNTI
227900940131     C************************************************************
228000940131     C     GESD2         BEGSR
228100940131     C*
228200941118   B1C     $INZD2        IFEQ      *ON
228300940131     C                   EXSR      INZD2
228400940223     C                   MOVE      *OFF          $INZD2
228500941118   E1C                   ENDIF
228600940202     C*
228700941118   B1C     $LASTV        IFNE      'D2'
228800940923     C                   EXSR      GESPIE
228900961023   B1C     $LASTV        IFNE      *HIVAL
229000940202     C                   MOVE      $LASTV        $LASTG
229100961023     C                   ENDIF
229200940202     C                   MOVE      'D2'          $LASTV
229300941118   E1C                   ENDIF
229400940131     C*
229500940707     C* se esistono errori sulla videata
229600940707     C* emetto la write del formato a indicatori spenti per vedere
229700940707     C* le eventuali decodifiche
229800941118   B1C     *IN99         IFEQ      *ON
229900940707     C                   MOVEA     *IN           WIN
230000940707     C                   MOVE      *ALL'0'       IN5098           49
230100940707     C                   MOVEA     IN5098        *IN(50)
230200940707     C                   WRITE     FMTD2
230300940707     C                   MOVEA     WIN           *IN
230400941118   E1C                   ENDIF
230500940707     C*
230600940131     C                   EXFMT     FMTD2
230700960605     C*
230800960605     C* azzero la posizione del cursore
230900960605     C                   Z-ADD     0             H1RIGA
231000960605     C                   Z-ADD     0             H1COLO
231100941205     C*
231200951005     C* CONTROLLO ABILITAZIONE TASTI
231300951005     C* INPUT
231400951005     C* $FC3 = Abilitazioni videata D2
231500951005     C* OUTPUT
231600951005     C* *IN97 + *IN99 Errori
231700951005     C                   MOVEA     $FC3          $FC
231800951005     C                   EXSR      CTRKEY
231900941205     C*
232000941205   B1C     *IN99         IFEQ      *OFF
232100941205     C*
232200941205   B2C                   SELECT
232300940131     C* F3=Fine
232400940506     C     $TASTO        WHENEQ    F03
232500940309     C                   EXSR      F03D2
232600061116     C* F4=Ricerca
232700061116     C     $TASTO        WHENEQ    F04
232800061116     C                   EXSR      Search
232900940131     C* F12=Ritorno
233000940506     C     $TASTO        WHENEQ    F12
233100940309     C                   EXSR      F12D2
233200940916     C* F24=Altri tasti
233300940916     C     $TASTO        WHENEQ    F24
233400940916     C                   EXSR      F24D2
233500940131     C*
233600940131     C                   OTHER
233700940131     C                   EXSR      CTRD2
233800941205   B3C     *IN99         IFEQ      *OFF
233900940211     C                   MOVE      $LASTG        $GEST
234000941205   B4C                   SELECT
234100940202     C     $LASTG        WHENEQ    'S1'
234200940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
234300941205   B5C     $ORD          IFNE      D2ORD
234400940223     C                   MOVE      *ON           $INZS1
234500940302     C                   MOVE      *BLANK        W1POS
234600941205   E5C                   ENDIF
234700940202     C     $LASTG        WHENEQ    'D1'
234800940318     C* per ora niente
234900941205   E4C                   ENDSL
235000940131     C* definisco i nuovi valori di default
235100940201     C                   Z-ADD     D2LOP         $LOP
235200940131     C                   Z-ADD     D2ORD         $ORD
235300950322     C*
235400950322     C* aggiorno valori assunti
235500950328     C* se richiesto il salvataggio
235600950328   B4C     D2BSV         IFEQ      1
235700950322     C                   EXSR      AGGMPE
235800950322   E4C                   ENDIF
235900950322     C*
236000941205   E3C                   ENDIF
236100940131     C*
236200941205   E2C                   ENDSL
236300940131     C*
236400941205   E1C                   ENDIF
236500941205     C*
236600940131     C                   ENDSR
236700940223     C/EJECT
236800940131     C************************************************************
236900940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
237000940131     C************************************************************
237100940201     C     INZD2         BEGSR
237200940131     C*
237300940131     C                   CLEAR                   FMTD2
237400021010     C                   EVAL      D2BSVA = Normale
237500950328     C* se sono entrato con chiave univoca
237600950328     C* impedisco il salvataggio valori assunti
237700950606     C     $Unique       IFEQ      *ON
237800021010     C                   EVAL      D2BSVA = Protetto
237900950328     C                   ENDIF
238000940201     C                   MOVEL     $LOP          D2LOP
238100940831     C                   MOVE      $ORD          D2ORD
238200940131     C*
238300940131     C                   ENDSR
238400940309     C/EJECT
238500940309     C************************************************************
238600940309     C* GESTIONE F03 VIDEO D2
238700940309     C************************************************************
238800940309     C     F03D2         BEGSR
238900940309     C*
239000940309     C                   MOVE      *ON           $FINE
239100940325     C* fine programma
239200940325     C                   EXSR      ENDPGM
239300940309     C*
239400940309     C                   ENDSR
239500940309     C/EJECT
239600940309     C************************************************************
239700940309     C* GESTIONE F12 VIDEO D2
239800940309     C************************************************************
239900940309     C     F12D2         BEGSR
240000940309     C*
240100940309     C                   MOVE      $LASTG        $GEST
240200940309     C*
240300940309     C                   ENDSR
240400940916     C/EJECT
240500940916     C************************************************************
240600940916     C* GESTIONE F24 VIDEO D2
240700940916     C************************************************************
240800940916     C     F24D2         BEGSR
240900940916     C*
241000940916     C                   MOVEA     $FX3          $FX
241100940916     C                   MOVEA     $FC3          $FC
241200940916     C                   MOVEA     $FR3          $FR
241300940916     C                   Z-ADD     $ULKD2        $ULK
241400950726     C                   Z-ADD     $D2L01        $L01
241500950726     C                   Z-ADD     $D2L02        $L02
241600940916     C                   EXSR      RIEKEY
241700940916     C                   Z-ADD     $ULK          $ULKD2            3 0
241800940916     C                   MOVEA     $FC           $FC3
241900940916     C                   MOVEL     $KEY1         Z3KE1
242000060224     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
242100940916     C                   WRITE     FMTZ3
242200940916     C*
242300940916     C                   ENDSR
242400940223     C/EJECT
242500940131     C************************************************************
242600940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
242700940131     C************************************************************
242800940131     C     CTRD2         BEGSR
242900940131     C*
243000940131     C                   SETOFF                                       99
243100940131     C*
243200940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
243300940201     C     D2LOP         IFEQ      0
243400940131     C                   SETON                                          5099
243500941027     C                   ENDIF
243600940131     C*
243700940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
243800940131     C     D2ORD         IFEQ      0
243900940131     C                   SETON                                          5199
244000941027     C                   ENDIF
244100070927     C*
244200940131     C                   ENDSR
244300941205     C/EJECT
244400941205     C************************************************************
244500941205     C* CONTROLLO ABILITAZIONE TASTI
244600941205     C* INPUT
244700941205     C* $TASTO =Tasto funzione premuto
244800941205     C* $FC = Tasti abilitati
244900941205     C* OUTPUT
245000941205     C* IN99 = ERrore
245100941205     C************************************************************
245200941205     C     CTRKEY        BEGSR
245300941205     C*
245400951005     C                   SETOFF                                       99
245500951005     C*
245600941205     C                   SELECT
245700941205     C     $TASTO        WHENEQ    F01
245800960202     C                   z-add     01            posfun
245900941205     C     $FC(1)        IFEQ      '0'
246000951005     C                   SETON                                        9997
246100941205     C                   ENDIF
246200941205     C     $TASTO        WHENEQ    F02
246300960202     C                   z-add     02            posfun
246400941205     C     $FC(2)        IFEQ      '0'
246500951005     C                   SETON                                        9997
246600941205     C                   ENDIF
246700941205     C     $TASTO        WHENEQ    F03
246800960202     C                   z-add     03            posfun
246900941205     C     $FC(3)        IFEQ      '0'
247000951005     C                   SETON                                        9997
247100941205     C                   ENDIF
247200941205     C     $TASTO        WHENEQ    F04
247300960202     C                   z-add     04            posfun
247400941205     C     $FC(4)        IFEQ      '0'
247500951005     C                   SETON                                        9997
247600941205     C                   ENDIF
247700941205     C     $TASTO        WHENEQ    F05
247800960202     C                   z-add     05            posfun
247900941205     C     $FC(5)        IFEQ      '0'
248000951005     C                   SETON                                        9997
248100941205     C                   ENDIF
248200941205     C     $TASTO        WHENEQ    F06
248300960202     C                   z-add     06            posfun
248400941205     C     $FC(6)        IFEQ      '0'
248500951005     C                   SETON                                        9997
248600941205     C                   ENDIF
248700941205     C     $TASTO        WHENEQ    F07
248800960202     C                   z-add     07            posfun
248900941205     C     $FC(7)        IFEQ      '0'
249000951005     C                   SETON                                        9997
249100941205     C                   ENDIF
249200941205     C     $TASTO        WHENEQ    F08
249300960202     C                   z-add     08            posfun
249400941205     C     $FC(8)        IFEQ      '0'
249500951005     C                   SETON                                        9997
249600941205     C                   ENDIF
249700941205     C     $TASTO        WHENEQ    F09
249800960202     C                   z-add     09            posfun
249900941205     C     $FC(09)       IFEQ      '0'
250000951005     C                   SETON                                        9997
250100941205     C                   ENDIF
250200941205     C     $TASTO        WHENEQ    F10
250300960202     C                   z-add     10            posfun
250400941205     C     $FC(10)       IFEQ      '0'
250500951005     C                   SETON                                        9997
250600941205     C                   ENDIF
250700941205     C     $TASTO        WHENEQ    F11
250800960202     C                   z-add     11            posfun
250900941205     C     $FC(11)       IFEQ      '0'
251000951005     C                   SETON                                        9997
251100941205     C                   ENDIF
251200941205     C     $TASTO        WHENEQ    F12
251300960202     C                   z-add     12            posfun
251400941205     C     $FC(12)       IFEQ      '0'
251500951005     C                   SETON                                        9997
251600941205     C                   ENDIF
251700941205     C     $TASTO        WHENEQ    F13
251800960202     C                   z-add     13            posfun
251900941205     C     $FC(13)       IFEQ      '0'
252000951005     C                   SETON                                        9997
252100941205     C                   ENDIF
252200941205     C     $TASTO        WHENEQ    F14
252300960202     C                   z-add     14            posfun
252400941205     C     $FC(14)       IFEQ      '0'
252500951005     C                   SETON                                        9997
252600941205     C                   ENDIF
252700941205     C     $TASTO        WHENEQ    F15
252800960202     C                   z-add     15            posfun
252900941205     C     $FC(15)       IFEQ      '0'
253000951005     C                   SETON                                        9997
253100941205     C                   ENDIF
253200941205     C     $TASTO        WHENEQ    F16
253300960202     C                   z-add     16            posfun
253400941205     C     $FC(16)       IFEQ      '0'
253500951005     C                   SETON                                        9997
253600941205     C                   ENDIF
253700941205     C     $TASTO        WHENEQ    F17
253800960202     C                   z-add     17            posfun
253900941205     C     $FC(17)       IFEQ      '0'
254000951005     C                   SETON                                        9997
254100941205     C                   ENDIF
254200941205     C     $TASTO        WHENEQ    F18
254300960202     C                   z-add     18            posfun
254400941205     C     $FC(18)       IFEQ      '0'
254500951005     C                   SETON                                        9997
254600941205     C                   ENDIF
254700941205     C     $TASTO        WHENEQ    F19
254800960202     C                   z-add     19            posfun
254900941205     C     $FC(19)       IFEQ      '0'
255000951005     C                   SETON                                        9997
255100941205     C                   ENDIF
255200941205     C     $TASTO        WHENEQ    F20
255300960202     C                   z-add     20            posfun
255400941205     C     $FC(20)       IFEQ      '0'
255500951005     C                   SETON                                        9997
255600941205     C                   ENDIF
255700941205     C     $TASTO        WHENEQ    F21
255800960202     C                   z-add     21            posfun
255900941205     C     $FC(21)       IFEQ      '0'
256000951005     C                   SETON                                        9997
256100941205     C                   ENDIF
256200941205     C     $TASTO        WHENEQ    F22
256300960202     C                   z-add     22            posfun
256400941205     C     $FC(22)       IFEQ      '0'
256500951005     C                   SETON                                        9997
256600941205     C                   ENDIF
256700941205     C     $TASTO        WHENEQ    F23
256800960202     C                   z-add     23            posfun
256900941205     C     $FC(23)       IFEQ      '0'
257000951005     C                   SETON                                        9997
257100941205     C                   ENDIF
257200941205     C     $TASTO        WHENEQ    F24
257300960202     C                   z-add     24            posfun
257400941205     C     $FC(24)       IFEQ      '0'
257500951005     C                   SETON                                        9997
257600941205     C                   ENDIF
257700941205     C                   ENDSL
257800941205     C*
257900941205     C                   ENDSR
258000940223     C/EJECT
258100940201     C************************************************************
258200940201     C* AGGIORNAMENTO VALORI ASSUNTI
258300940201     C************************************************************
258400940201     C     AGGMPE        BEGSR
258500940201     C*
258600950524     C* preparo i parametri di chiamata al driver
258700950524     C                   CLEAR                   XMPEDS
258800950524     C                   EVAL      TpChiamata = '03'
258900950524     C                   EVAL      NChiavi = 5
259000950524     C                   EVAL      VLogica = 2
259100950524     C*
259200950524     C* preparo le chiavi per la chiamata al driver
259300950524     C                   CLEAR                   XMPEDS1
259400950524     C                   MOVEL     XSCSOC        DSMPESOC
259500950524     C                   MOVEL     DSPGM         DSMPEPGM
259600950524     C                   MOVEL     KNMUS         DSMPEPRF
259700941013     C*
259800950322     C* Si memorizzano i dati di default a seconda delle modalità con
259900950322     C* cui il pgm è usato
260000950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
260100950322     C* MPETIP = 'PV'  pgm richiamato da menù
260200950322     C* MPETIP = 'PB'  x lancio Batch
260300950322     C*
260400941013     C                   SELECT
260500130915    >C     OPZ980        WHENEQ    '01'
260600950524     C                   MOVEL     'PX'          DSMPETIP
260700941013     C                   OTHER
260800950524     C                   MOVEL     'PV'          DSMPETIP
260900941013     C                   ENDSL
261000950524     C* dati
261100950524     C                   MOVEL     DSMPE         DSMPEPAR
261200950524     C*
261300950524     C                   CALLB     'XMPE'
261400950524     C                   PARM                    XMPEDS
261500950524     C                   PARM                    XMPEDS1
261600940201     C*
261700940201     C                   ENDSR
261800940223     C/EJECT
261900940131     C************************************************************
262000940131     C* OPERAZIONI INIZIALI
262100940131     C************************************************************
262200940131     C     *INZSR        BEGSR
262300061108     C     *DMY          Move      Udate         ZDateISO
262400061108     C     *JobRun       Move      ZDateISO      ZDate             6 0
262500940131     C*
262600940127     C* Reperimento parametri
262700940127     C*
262800940117     C     *ENTRY        PLIST
262900940117     C                   PARM                    KPJBA
263000951206     C*
263100951206     C* Reperimento dati società
263200951206     C*
263300061108     C                   MOVEL     'SOC001'      TIPXSC
263400970424     C                   MOVEL     *BLANK        SOCXSC
263500951206     C                   EXSR      REPSOC
263600960409     C*
263700960409     C* Controlli dati società
263800960409     C*
263900960409     C     RTNXSC        IFNE      '1'
264000061108     C                   MOVEL     XSOCDS        SOC001
264100960409     C                   ELSE
264200960409     C                   MOVE      *ON           $FINE
264300130915    >C                   MOVE      '1'           Err980
264400960409     C                   EXSR      ENDPGM
264500960409     C                   ENDIF
264600061115     C*
264700130915     c                   open      ndc980d                              13
264800061115     c* emetto messaggio per video non a 132
264900061115     c                   if        *in13=*on
265000061115     c                   move      'COS2651'     XmsgCd1
265100061115     C                   CALL      'XMSGR'
265200061115     C                   PARM                    XMSGDS
265300061115     C                   PARM                    XMStxt          135
265400061115     C                   PARM                    XMSGinz           1
265500061115     C                   PARM                    XMSGCd1           7
265600061115     C                   PARM                    XMSGCd2           7
265700061115     C                   PARM                    XMSGCd3           7
265800061115     C                   PARM                    XMSGSn1           7
265900061115     C                   PARM                    XMSGCdTxT        63
266000061115     C                   PARM                    MSGCdTit         14
266100061115     C                   PARM                    MSGClPrtf         1
266200061115     C                   PARM                    VarMsg1         100
266300061115     C                   PARM                    VarMsg2         100
266400061115     C                   PARM                    VarMsg3         100
266500130915    >C                   MOVE      '1'           Err980
266600061115     c                   exsr      endpgm
266700061115     c                   endif
266800950726     C*
266900950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
267000070927     C                   EVAL      $D1L01 = %Size(Z1Ke)
267100061108     C                   EVAL      $D1L02 = %Size(Z1Ke2)
267200070927     C                   EVAL      $S1L01 = %Size(Z2Ke)
267300070927     C                   EVAL      $S1L02 = %Size(Z2Ke2)
267400950926     C                   EVAL      $C1Op1 = %Size(C1Opz1)
267500950926     C                   EVAL      $C1Op2 = %Size(C1Opz2)
267600950906     C                   EVAL      $D2L01 = %Size(Z3Ke1)
267700950906     C                   EVAL      $D2L02 = 0
267800090316     C*
267900090316     c* reperimento dati parametro modulo solleciti
268000090316     C                   movel     'NDCL1S'      xpapar
268100090316     C                   movel     *blank        xparic
268200090316     C                   CALLb     'XPAR'
268300090316     C                   PARM                    xscsoc
268400090316     C                   PARM                    XPAPAR            8
268500090316     C                   PARM                    XPAOUT
268600090316     C                   PARM                    XPAERR            1
268700090316     C                   PARM                    XPARIC            1
268800090316     C     XPAERR        IFEQ      '0'
268900090316     C                   movel(p)  XpaOut        Anp057Ds
269000130915     c                   move      posattr057    posiz             2 0
269100090316     c                   else
269200090316     c                   seton                                        lr
269300090316     c                   return
269400090316     C                   endif
269500940225     C*
269600940223     C* Reperimento opzioni
269700940127     C*
269800940128     C                   EXSR      REPOPZ
269900950406     C*
270000940506     C* Reperimento tasti di funzione
270100940506     C*
270200940506     C                   EXSR      REPKEY
270300960117     C* Chiamata a routine per il reperimento delle opzioni
270400960117     C* personalizzate
270500960117     C                   CAllb     'X46USREXT'
270600960117     C                   PARM                    $OP1
270700960117     C                   PARM                    $TX1
270800960117     C                   PARM                    $FL1
270900960117     C                   PARM                    $PGO1
271000960117     C                   PARM      $dimOp        NumOpz
271100960202     C                   PARM                    $FX2
271200960202     C                   PARM                    $FC2
271300960202     C                   PARM                    $PGF2
271400960117     C                   PARM                    DsPgm
271500960117     C                   PARM                    xsccut
271600960117     C                   PARM                    xscgrp
271700960117     C*
271800960118     C                   Z-add     1             OPZLEN
271900960118     C     *blank        Lookup    $TX1(opzlen)                           21
272000960117     C                   if        *in21 = *on
272100960117     C                   sub       1             OPZLEN
272200960117     C                   else
272300960117     C                   Z-add     $DimOp        OPZLEN
272400960117     C                   endif
272500940127     C*
272600940117     C                   ENDSR
272700940223     C/EJECT
272800940223     C************************************************************
272900940223     C* INIZIALIZZAZIONE VARIABILI
273000940223     C************************************************************
273100940223     C     INZVAR        BEGSR
273200941215     C*
273300941215     C* Dati da PGM chiamante
273400941215     C*
273500130915     c                   movel     kpjbu         ndc980ds
273600951206     C*
273700951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
273800951206     C* reperimento dati società
273900061108     C                   MOVEL     'SOC001'      TIPXSC
274000070927    >C                   MOVEL     *blanks       SOCXSC
274100951122     C                   EXSR      REPSOC
274200951010     C     RTNXSC        IFNE      '1'
274300061108     C                   MOVEL     XSOCDS        SOC001
274400951010     C                   ELSE
274500951010     C                   MOVE      *ON           $FINE
274600130915    >C                   MOVE      '1'           Err980
274700951010     C                   EXSR      ENDPGM
274800951010     C                   ENDIF
274900061108     C* visualizzo il sfl troncato
275000061108     C                   Eval      c1mode=*on
275100941026     C*
275200941026     C* Reperimento valori default utente
275300941026     C*
275400941026     C                   EXSR      REPDFT
275500940506     C*
275600940506     C* Definizione delle opzioni utilizzabili a pgm
275700940506     C*
275800940506     C                   EXSR      DEFOPZ
275900950330     C*
276000940223     C* Variabili per gestione videate
276100940223     C*
276200940223     C                   MOVE      *OFF          $FINE                          fine pgm
276300940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
276400940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
276500940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
276600130915     C                   MOVE      *OFF          $INZD4                         inizializzione D2
276700130915     C                   MOVE      *OFF          $FINED4           1            fine pgm
276800950606     C                   MOVE      *OFF          $EndFile                       memo fine file
276900940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
277000940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
277100940509     C                   MOVE      *HIVAL        $LASTV
277200090511     C                   Z-ADD     0             $RigheVuote                    piede da emett. D1
277300940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
277400940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
277500940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
277600960605     C* azzero la posizione del cursore
277700960605     C                   Z-ADD     0             H1RIGA
277800960605     C                   Z-ADD     0             H1COLO
277900940223     C*
278000940318     C                   MOVE      *ON           $INZD1
278100940318     C                   MOVE      *ON           $INZS1
278200130915     C                   MOVE      *ON           $INZD4
278300940831     C*
278400940831     C* se pgm richiamato con chiave univoca impostata,
278500940831     C* forzo la emissione della lista e imposto l'ordinamento
278600130915    >C*    Opz980        IFNE      *BLANK
278700090312     C*                  MOVE      *ON           $Unique
278800090312     C*                  Z-ADD     2             $LOP
278900090312     C*                  Z-ADD     99            $ORD
279000090312     C*                  ELSE
279100090312     C                   MOVE      *OFF          $Unique
279200090312     C*                  ENDIF
279300950330     C*
279400950330     C* Variabili appoggio
279500950330     C*
279600950330     C                   Z-ADD     SFLPAG        WPAG
279700950330     C                   MOVEL     *OFF          WOPR
279800950515     C                   CLEAR                   W1POS
279900941104     C*
280000941104     C* definisco qual'è la prima videata
280100950410     C* (se la prima videata risulta la lista, devo comunque caricare
280200950410     C*  la videata di parzializzazione e gestire la definizione
280300950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
280400950410     C*  passati dei dati, può variare il caricamento)
280500940223     C                   SELECT
280600940223     C     $LOP          WHENEQ    1
280700940223     C                   MOVE      'D1'          $GEST
280800940223     C     $LOP          WHENEQ    2
280900940223     C                   MOVE      'S1'          $GEST
281000941104     C                   EXSR      INZD1
281100941104     C                   MOVE      *OFF          $INZD1
281200950410     C                   EXSR      DEFORD
281300941104     C                   ENDSL
281400940223     C*
281500940223     C* Valorizzazione campi univoci testate
281600940223     C*
281700940223     C                   CLEAR                   FMTT1
281800940223     C                   MOVEL     KNSIF         NOMSIF
281900941109     C                   MOVEL     XSCDSI        NOMDIT
282000941027     C                   MOVEL     DSPGM         NOMPGM
282100061113     C                   ENDSR
282200061113     c*
282300941209     C************************************************************
282400941209     C* REPERIMENTO DATI SOCIETA'
282500941209     C************************************************************
282600941209     C     REPSOC        BEGSR
282700941209     C*
282800950522     C                   CALLB     'XSOC'
282900941209     C                   PARM                    TIPXSC            6
283000941209     C                   PARM                    SOCXSC            3
283100941209     C                   PARM                    CDSXSC            9 0
283200941209     C                   PARM                    MODXSC            3
283300941209     C                   PARM      *BLANKS       RTNXSC            1
283400941209     C                   PARM                    XSOCDS
283500941209     C                   PARM                    KPJBA
283600941209     C*
283700941209     C                   ENDSR
283800940223     C/EJECT
283900940127     C************************************************************
284000940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
284100940127     C************************************************************
284200940131     C     REPDFT        BEGSR
284300940127     C*
284400950524     C* preparo i parametri di chiamata al driver
284500950524     C                   CLEAR                   XMPEDS
284600950524     C                   EVAL      TpChiamata = '01'
284700950524     C                   EVAL      NChiavi = 5
284800950524     C                   EVAL      VLogica = 2
284900950524     C*
285000950524     C* preparo le chiavi per la chiamata al driver
285100950524     C                   CLEAR                   XMPEDS1
285200950524     C                   MOVEL     XSCSOC        DSMPESOC
285300950524     C                   MOVEL     DSPGM         DSMPEPGM
285400950524     C                   MOVEL     KNMUS         DSMPEPRF
285500950524     C* Si reperiscono i dati di default a seconda delle modalità con
285600950524     C* cui il pgm è usato
285700950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
285800950524     C* MPETIP = 'PV'  pgm richiamato da menù
285900950524     C* MPETIP = 'PB'  x lancio Batch
286000941013     C                   SELECT
286100130915    >C     OPZ980        WHENEQ    '01'
286200950524     C                   MOVEL     'PX'          DSMPETIP
286300941013     C                   OTHER
286400950524     C                   MOVEL     'PV'          DSMPETIP
286500941013     C                   ENDSL
286600950524     C*
286700950524     C                   CALLB     'XMPE'
286800950524     C                   PARM                    XMPEDS
286900950524     C                   PARM                    XMPEDS1
287000950524     C*
287100950524     C     Risultato     IFEQ      '0'
287200950524     C                   MOVEL     DSMPEPAR      DSMPE
287300940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
287400940131     C* i suoi valori di default vengono decisi a pgm
287500940131     C                   ELSE
287600070928     C                   Z-ADD     2             $LOP
287700070928     C                   Z-ADD     1             $ORD
287800941027     C                   ENDIF
287900940131     C*
288000940131     C                   ENDSR
288100940223     C/EJECT
288200940131     C************************************************************
288300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
288400940131     C************************************************************
288500940225     C     DEFOPZ        BEGSR
288600940131     C*
288700940225     C* Questa routine deve valorizzare la schiera $FL1
288800940225     C* con S o N a seconda della utilizzabilità o meno
288900940225     C* dell'opzione correlata
289000940225     C*
289100950301     C* Inizialmente si considerano tutte le opzioni come valide
289200940317     C     1             DO        $DIMOP        X
289300061108     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
289400960117     C                   If        $TX1(X)  <> *Blank
289500940225     C                   MOVE      'S'           $FL1(X)
289600960117     C                   Else
289700960117     C                   Leave
289800960117     C                   EndIf
289900941027     C                   ENDDO
290000940715     C*
290100940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
290200941018     C* con 1 o 0 a seconda della utilizzabilità o meno
290300940715     C* del tasto funzione correlato
290400940715     C*
290500940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
290600940715     C* di compilazione
290700940715     C                   RESET                   $FC1
290800940715     C                   RESET                   $FR1
290900940715     C                   RESET                   $FC2
291000940715     C                   RESET                   $FR2
291100940919     C                   RESET                   $FC3
291200940919     C                   RESET                   $FR3
291300130915     C                   RESET                   $FC4
291400130915     C                   RESET                   $FR4
291500940225     C*
291600940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
291700940225     C* invalido alcune opzioni
291800940225     C                   SELECT
291900940225     C* Se pgm richiamato in ricerca disattivo :
292000130915    >C     OPZ980        WHENEQ    '01'
292100940603     C* F3=Fine
292200940603     C                   Z-ADD     3             X
292300941216     C                   MOVE      '0'           $FC1(X)
292400941216     C                   MOVE      '0'           $FC2(X)
292500941216     C                   MOVE      '0'           $FC3(X)
292600950116     C*
292700950301     C*
292800940225     C* Se pgm richiamato in interrogazione disattivo :
292900130915    >C     OPZ980        WHENEQ    '05'
293000940307     C* F10=Immissione
293100940615     C                   Z-ADD     10            X
293200950301     C                   MOVE      '0'           $FC1(X)
293300950301     C                   MOVE      '0'           $FC2(X)
293400940225     C* 1=Selezione
293500940225     C                   Z-ADD     1             X
293600940225     C     01            LOOKUP    $OP1(X)                                21
293700940225     C     *IN21         IFEQ      *ON
293800940225     C                   MOVE      'N'           $FL1(X)
293900940225     C                   ENDIF
294000940225     C* 2=Modifica
294100940225     C                   Z-ADD     1             X
294200940225     C     02            LOOKUP    $OP1(X)                                21
294300940225     C     *IN21         IFEQ      *ON
294400940225     C                   MOVE      'N'           $FL1(X)
294500940225     C                   ENDIF
294600940225     C* 3=Copia
294700940225     C                   Z-ADD     1             X
294800940225     C     03            LOOKUP    $OP1(X)                                21
294900940225     C     *IN21         IFEQ      *ON
295000940225     C                   MOVE      'N'           $FL1(X)
295100940225     C                   ENDIF
295200940225     C* 4=Annullamento
295300940225     C                   Z-ADD     1             X
295400940225     C     04            LOOKUP    $OP1(X)                                21
295500940225     C     *IN21         IFEQ      *ON
295600940225     C                   MOVE      'N'           $FL1(X)
295700940225     C                   ENDIF
295800940225     C*
295900130915    >C     OPZ980        WHENEQ    *BLANKS
296000940225     C* Se pgm non richiamato, disattivo subito :
296100941124     C* F8=Selezione seguente
296200941124     C                   Z-ADD     8             X
296300941206     C                   MOVE      '0'           $FC1(X)
296400941206     C                   MOVE      '0'           $FC2(X)
296500940225     C* 1=Selezione
296600940225     C                   Z-ADD     1             X
296700940225     C     01            LOOKUP    $OP1(X)                                21
296800940225     C     *IN21         IFEQ      *ON
296900940225     C                   MOVE      'N'           $FL1(X)
297000940225     C                   ENDIF
297100940225     C*
297200950301     C* In tutti gli altri casi, disattivo subito :
297300941228     C                   OTHER
297400941228     C* 1=Selezione
297500941228     C                   Z-ADD     1             X
297600941228     C     01            LOOKUP    $OP1(X)                                21
297700941228     C     *IN21         IFEQ      *ON
297800941228     C                   MOVE      'N'           $FL1(X)
297900941228     C                   ENDIF
298000941228     C*
298100940225     C                   ENDSL
298200941118     C*
298300941118     C* poi, disattivo a seconda delle autorizzazioni :
298400061108    >C*                  IF        AUT___ <> *on
298500061108    >C*                  Z-ADD     1             X
298600061108    >C*    XX            LOOKUP    $OP1(X)                                21
298700061108    >C*    *IN21         IFEQ      *ON
298800061108    >C*                  MOVE      'N'           $FL1(X)
298900061108    >C*                  ENDIF
299000061108    >C*                  ENDIF
299100940225     C*
299200940225     C                   ENDSR
299300940225     C/EJECT
299400940225     C************************************************************
299500940225     C* REPERIMENTO OPZIONI
299600940225     C************************************************************
299700940225     C     REPOPZ        BEGSR
299800940225     C*
299900940314     C     1             DO        $DIMOP        X
300000940128     C*
300100940223     C     $MS1(X)       IFNE      *BLANK
300200940128     C*
300300940223     C                   MOVEL     $MS1(X)       IDMSG
300400950522     C                   CALLB     'XRTVM'
300500940201     C                   PARM                    IDMSG            27
300600940201     C                   PARM                    TXTMSG          100
300700940201     C                   PARM                    ERRMSG            1
300800940128     C*
300900940128     C     ERRMSG        IFNE      *ON
301000940223     C                   MOVEL     TXTMSG        $TX1(X)
301100940128     C                   ELSE
301200940223     C                   MOVEL     *ALL'?'       $TX1(X)
301300941027     C                   ENDIF
301400940128     C*
301500941027     C                   ENDIF
301600940128     C*
301700941027     C                   ENDDO
301800940128     C*
301900940128     C                   ENDSR
302000940506     C/EJECT
302100940506     C************************************************************
302200940506     C* REPERIMENTO TASTI DI FUNZIONE
302300940506     C************************************************************
302400940506     C     REPKEY        BEGSR
302500940506     C                   MOVEA     $FM1          $FM
302600950721     C                   MOVEA     $FC1          $FC
302700940506     C                   EXSR      REPKKK
302800940506     C                   MOVEA     $FX           $FX1
302900940506     C*
303000940506     C                   MOVEA     $FM2          $FM
303100950721     C                   MOVEA     $FC2          $FC
303200940506     C                   EXSR      REPKKK
303300940506     C                   MOVEA     $FX           $FX2
303400940919     C*
303500940919     C                   MOVEA     $FM3          $FM
303600950721     C                   MOVEA     $FC3          $FC
303700940919     C                   EXSR      REPKKK
303800940919     C                   MOVEA     $FX           $FX3
303900100423     c*
304000130915     C                   MOVEA     $FM4          $FM
304100130915     C                   MOVEA     $FC4          $FC
304200130915     C                   EXSR      REPKKK
304300130915     C                   MOVEA     $FX           $FX4
304400940919     C                   ENDSR
304500940506     C/EJECT
304600940506     C************************************************************
304700940506     C* REPERIMENTO TASTI DI FUNZIONE
304800940506     C************************************************************
304900940506     C     REPKKK        BEGSR
305000940506     C*
305100940506     C                   CLEAR                   $FX
305200940506     C     1             DO        24            X
305300940506     C*
305400940506     C     $FM(X)        IFNE      *BLANK
305500950721     C     $Fc(X)        andne     '0'
305600940506     C*
305700940506     C                   MOVEL     $FM(X)        IDMSG
305800950522     C                   CALLB     'XRTVM'
305900940506     C                   PARM                    IDMSG            27
306000940506     C                   PARM                    TXTMSG          100
306100940506     C                   PARM                    ERRMSG            1
306200940506     C*
306300940506     C     ERRMSG        IFNE      *ON
306400940506     C                   MOVEL     TXTMSG        $FX(X)
306500940506     C                   ELSE
306600940506     C                   MOVEL     *ALL' '       $FX(X)
306700941027     C                   ENDIF
306800940506     C*
306900941027     C                   ENDIF
307000940506     C*
307100941027     C                   ENDDO
307200940506     C*
307300940506     C                   ENDSR
307400940223     C/EJECT
307500940128     C************************************************************
307600950606     C* DEFINIZIONE KLIST
307700940128     C************************************************************
307800950606     C     DefKlist      BEGSR
307900940128     C*
308000940223     C* klist
308100090312     C* PLIST per NDVASOG
308200090312     C     PDVASOG       PLIST
308300090312     C                   PARM                    DVARIC
308400090312     C                   PARM                    DVASOCIETA
308500090312     C                   PARM                    DVAUNITA
308600090312     C                   PARM                    DVASTRUTT
308700090312     C                   PARM                    DVADTRIF
308800090312     C                   PARM                    DVALENOUT
308900090312     C* Se si accede con il codice cli/for impostare i campi seguenti
309000090312     C* se si accede con il soggetto ometterli e impostare gli altri
309100090312     C                   PARM                    DVASNATURA
309200090312     C                   PARM                    DVACODICE
309300090312     C                   PARM                    DVALINEAV
309400090312     C                   PARM                    DVAFILIALE
309500090312     C                   PARM                    *omit
309600090312     C                   PARM                    DVATPIND
309700090312     C                   PARM                    DVACDIND
309800090312     C                   PARM                    DVAERRORE
309900090312     C                   PARM                    DVAOUTPUT
310000130915     C* NDATT01L
310100130915     C     K07ATT01      KLIST
310200130915     C                   KFLD                    ATTSocieta
310300130915     C                   KFLD                    ATTKcc
310400130915     C                   KFLD                    ATTKsc
310500130915     C                   KFLD                    ATTDtPar
310600130915     C                   KFLD                    ATTNrPar
310700130915     C                   KFLD                    ATTSerPar
310800130915     C                   KFLD                    ATTDtSca
310900100423     C*
311000130915     c     K08ATT01      KLIST
311100130915     C                   KFLD                    ATTSocieta
311200130915     C                   KFLD                    ATTKcc
311300130915     C                   KFLD                    ATTKsc
311400130915     C                   KFLD                    ATTDtPar
311500130915     C                   KFLD                    ATTNrPar
311600130915     C                   KFLD                    ATTSerPar
311700130915     C                   KFLD                    ATTDtSca
311800130915     C                   KFLD                    ATTProgr
311900130922     C* ndRCA04l
312000130922     C     K09Rca04      KLIST
312100130922     C                   KFLD                    RcaSocieta
312200130922     C                   KFLD                    RcaPosiz
312300130922     C                   KFLD                    RcaKCC
312400130922     C                   KFLD                    RcaKSC
312500130922     C                   KFLD                    RcaDtpar
312600130922     C                   KFLD                    RcaNrpar
312700130922     C                   KFLD                    RcaSerPar
312800130922     C                   KFLD                    RcaDtSca
312900130922     C                   KFLD                    RcaDtAtt
313000130922     C     K08Rca04      KLIST
313100130922     C                   KFLD                    RcaSocieta
313200130922     C                   KFLD                    RcaPosiz
313300130922     C                   KFLD                    RcaKCC
313400130922     C                   KFLD                    RcaKSC
313500130922     C                   KFLD                    RcaDtpar
313600130922     C                   KFLD                    RcaNrpar
313700130922     C                   KFLD                    RcaSerPar
313800130922     C                   KFLD                    RcaDtSca
313900131009     C* ndRCA01l
314000131009     C     K10Rca01      KLIST
314100131009     C                   KFLD                    RcaSocieta
314200131009     C                   KFLD                    RcaPosiz
314300131009     C                   KFLD                    RcaKCC
314400131009     C                   KFLD                    RcaKSC
314500131009     C                   KFLD                    RcaDtpar
314600131009     C                   KFLD                    RcaNrpar
314700131009     C                   KFLD                    RcaSerPar
314800131009     C                   KFLD                    RcaDtSca
314900131009     C                   KFLD                    RcaFase
315000131009     C                   KFLD                    RcaProgr
315100131009     C     K08Rca01      KLIST
315200131009     C                   KFLD                    RcaSocieta
315300131009     C                   KFLD                    RcaPosiz
315400131009     C                   KFLD                    RcaKCC
315500131009     C                   KFLD                    RcaKSC
315600131009     C                   KFLD                    RcaDtpar
315700131009     C                   KFLD                    RcaNrpar
315800131009     C                   KFLD                    RcaSerPar
315900131009     C                   KFLD                    RcaDtSca
316000130922     C* ndRCA05l
316100130922     C     K10Rca05      KLIST
316200130922     C                   KFLD                    RcaSocieta
316300130922     C                   KFLD                    RcaPosiz
316400130922     C                   KFLD                    RcaKCC
316500130922     C                   KFLD                    RcaKSC
316600130922     C                   KFLD                    RcaDtpar
316700130922     C                   KFLD                    RcaNrpar
316800130922     C                   KFLD                    RcaSerPar
316900130922     C                   KFLD                    RcaDtSca
317000130922     C                   KFLD                    RcaProgrIm
317100130922     C                   KFLD                    RcaAzione
317200090316     C                   ENDSR
317300090316     c*
317400130915     C************************************************************
317500130915     C* GESTIONE ulteriore record
317600100423     C************************************************************
317700130915     C     GESD4         BEGSR
317800100423     C* inizializzazione videata
317900100423     C     $INZD4        IFEQ      *ON
318000100423     C                   EXSR      INZD4
318100100423     C                   MOVE      *OFF          $INZD4
318200100423     C                   ENDIF
318300100423     C* emissione piede videata se proveniente da altra
318400100423     C* salvataggio valore formato di provenienza
318500100423     C     $LASTV        IFNE      'D4'
318600100423     C* salvo stato F17 per sapere cosa fare in caso di F12
318700100423     C                   MOVE      *INKR         WINKR             1
318800100423     C                   EXSR      GESPIE
318900100423     C                   MOVE      $LASTV        $LASTG
319000100423     C                   MOVE      'D4'          $LASTV
319100100423     C                   ENDIF
319200100423     C*
319300100423     C* se esistono errori sulla videata
319400100423     C* emetto la write del formato a indicatori spenti per vedere
319500100423     C* le eventuali decodifiche
319600100423     C     *IN99         IFEQ      *ON
319700100423     C                   MOVEA     *IN           WIN
319800100423     C                   MOVE      *ALL'0'       IN5098           49
319900100423     C                   MOVEA     IN5098        *IN(50)
320000100423     C                   WRITE     FMTD4
320100100423     C                   MOVEA     WIN           *IN
320200100423     C                   ENDIF
320300100423     C*
320400100423     C                   EXFMT     FMTD4
320500100423     C* controllo abilitazione tasti funzione
320600100423     C*
320700100423     C                   EXSR      CTRKD4
320800100423     C*
320900100423     C     *IN99         IFEQ      *OFF
321000100423     C*
321100100423     C                   SELECT
321200100423     C* F3=Fine
321300100423     C     $TASTO        WHENEQ    F03
321400100423     C                   EXSR      F03D1
321500100426     C* F4=Ricerca
321600100426     C     $TASTO        WHENEQ    F04
321700100426     C                   EXSR      SEARCH
321800100423     C* F5=Refresh
321900100423     C     $TASTO        WHENEQ    F05
322000100423     C                   EXSR      F05D4
322100100423     C* F12=Ritorno
322200100423     C     $TASTO        WHENEQ    F12
322300100423     C                   EXSR      F12D4
322400100423     C* F24=Altri tasti
322500100423     C     $TASTO        WHENEQ    F24
322600100423     C                   EXSR      F24D4
322700100423     C*
322800100423     C                   OTHER
322900100423     C                   EXSR      CTRD4
323000100423     c*
323100100423   B4C     $TASTO        IFEQ      F06
323200100423     C     *IN99         ANDEQ     *OFF
323300100423     c                   exsr      AggAnaD4
323400131025     C                   EXSR      GESAGGD4
323500100423   E6C                   ENDIF
323600100423     C*
323700100423     C                   ENDSL
323800100423     C*
323900100423     C                   ENDIF
324000130915     C                   ENDSR
324100100423     C/EJECT
324200130915     C************************************************************
324300100423     C* aggiornamento da videata d4
324400100423     C************************************************************
324500130915     C     AggAnaD4      BEGSR
324600130915     C                   EVAL      S1Azione =  d4azione
324700130915     C                   EVAL      S1Grute  =  d4Grute
324800130915     C                   EVAL      S1nota   =  d4nota
324900130915     C                   EVAL      s1dtatt  =  d4dtatt
325000130915     C                   eval      h1progr  =  h4progr
325100130915     C*
325200130916     C                   exsr      AggAtti
325300100423     C*
325400130915     C                   ENDSR
325500131025     C/EJECT
325600131025     C************************************************************
325700131025     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
325800131025     C************************************************************
325900131025     C     GESAGGD4      BEGSR
326000131025     C*
326100131025     C* segnalo al pgm chiamante l'avvenuta conferma
326200131025    >C                   MOVE      *ON           OPR980
326300131025     C* segnalo al pgm chiamante l'aver premuto l'F6
326400131025    >C                   MOVE      '0'           RET980
326500131025     C*
326600131025     C                   MOVE      *ON           $FINED4
326700131025     c                   move      *on           $AggiornaI        1
326800131025     C*
326900131025     C                   ENDSR
327000131025     C/EJECT
327100130915     C************************************************************
327200130915     C* INIZIALIZZAZIONE VIDEATA immissione attività
327300100423     C************************************************************
327400130915     C     INZD4         BEGSR
327500100423     C*
327600100423     C                   CLEAR                   FMTD4
327700100423     C*
327800100423     c                   move      c1kcc         d4kcc
327900100423     c                   move      c1ksc         d4ksc
328000100423     c                   move      c1kscd        d4Descr
328100100426     C     *jobrun       Move      h1DtPar       D4DtPar
328200100426     C                   z-add     h1nrpar       D4NrPar
328300100423     c                   move      h1serpar      d4Serpar
328400100423     C                   Move      s1DtSca       D4DtSca
328500100423     c                   z-add     s1ImpScad     D4ImpScad
328600100423     c*
328700100423     C* reperisco ultimo progressivo della scadenza
328800130916     C                   EVAL      AttSocieta = Societa980
328900130916     C                   EVAL      AttKcc =   kcc980
329000130916     C                   EVAL      AttKsc =   ksc980
329100130916     C                   move      h1dtpar       Attdtpar
329200130916     C                   EVAL      ATTNrPar =  h1nrpar
329300130916     C                   EVAL      ATTserpar=  h1serpar
329400130916     C     *jobrun       move      s1dtsca       Attdtsca
329500130915     c                   eval      AttProgr =  *hival
329600130915     C     K08Att01      setgt     NDAtt01L
329700130915     C     K07Att01      readpe(N) NDAtt01L                               21
329800100423     c* lo trova sempre perchè il primo va inserito sul sfl
329900130915     c  N21Attprogr      add       1             h4progr
330000100423     c*
330100100423     C                   ENDSR
330200130915     C/EJECT
330300130915     C************************************************************
330400130915     C* CONTROLLO VIDEATA
330500100423     C************************************************************
330600130915     C     CTRD4         BEGSR
330700100423     C*
330800100423     c                   setoff                                       99
330900100423     C*
331000130915     C* azione
331100130915     c                   clear                   D4AzioneD
331200100423     C                   MOVE      '1'           XTBRIC
331300100423     C                   MOVE      XSCSOC        XTBAZI
331400100423     C                   MOVE      *ZERO         XTBKEY
331500130915     C                   MOVE      '047'         XTBCOD
331600130915     C                   MOVE      D4Azione      XTBKEY
331700100423     C                   CALLb     'XATB'
331800100423     C                   PARM                    XATBDS
331900100423     C     XTBERR        IFEQ      *Off
332000130915     C                   MOVEL     XTBUNI        ang047ds
332100130915     C                   MOVEL     DEs047        D4AzioneD
332200130915     C     Tipatt047     Ifne      *blanks
332300130915     C                   seton                                        6399
332400130915     C                   Endif
332500130915     C                   else
332600130915     C                   seton                                        6099
332700130915     C                   endif
332800100423     C*
332900130915     C* destinatari
333000130915     C                   If        D4Grute <> *blanks
333100130915     C                   MOVE      '1'           XTBRIC
333200130915     C                   MOVE      XSCSOC        XTBAZI
333300130915     C                   MOVE      *ZERO         XTBKEY
333400130915     C                   MOVE      '048'         XTBCOD
333500130915     C                   MOVE      D4Grute       XTBKEY
333600130915     C                   CALLb     'XATB'
333700130915     C                   PARM                    XATBDS
333800130915     C     XTBERR        IFEQ      *Off
333900130915     C                   MOVEL     XTBUNI        ang048ds
334000130915     C                   else
334100130915     C                   seton                                        6299
334200130915     C                   endif
334300130916     C                   endif
334400100423     C* data impegno
334500131029     C                   If        d4dtatt <> 0
334600100423     c* controllo validità formale
334700130915     C     *JobRun       Test(D)                 d4dtatt                15
334800100423     C                   If        *in15 = *on
334900100423     c                   seton                                        6199
335000100423     c                   endif
335100100427     c                   else
335200100427     c                   seton                                        6199
335300100423     c                   endif
335400131029      *
335500131029      * se immissione controllo data attività maggiore/uguale oggi
335600131029     c                   if        *in61 = *off
335700131029     c                   if        %date(d4DtAtt:*jobrun) < zdateiso
335800131029     c                   seton                                        6199
335900131029     c                   endif
336000131029     c                   endif
336100100423     C*
336200131009     c                   exsr      CtrFaseD4
336300100423     C*
336400130915     C                   ENDSR
336500100426     C************************************************************
336600100423     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D4
336700100423     C* INPUT
336800100423     C* $FC1 = Abilitazioni videata D1
336900100423     C* OUTPUT
337000100423     C* *IN97 *IN99 Errori
337100100423     C************************************************************
337200130915     C     CTRKD4        BEGSR
337300100423     C                   SETOFF                                       99
337400100423     C                   MOVEA     $FC4          $FC
337500100423     C                   EXSR      CTRKEY
337600100423     C     *IN99         IFEQ      *ON
337700100423     C                   SETON                                        97
337800100423     C                   ENDIF
337900130915     C                   ENDSR
338000100423     C/EJECT
338100130915     C************************************************************
338200100423     C* F05
338300100423     C************************************************************
338400130915     C     F05D4         BEGSR
338500100423     C*
338600100423     C                   MOVE      *ON           $INZD4
338700100423     C*
338800130915     C                   ENDSR
338900130915     C************************************************************
339000130915     C* F12
339100100423     C************************************************************
339200130915     C     F12D4         BEGSR
339300130915     C*  si pulisce tutto perchè si aggiornano i tassi per scaglioni
339400130915     c*  solo da questa videata con F6 dalla D4
339500100423     C                   CLEAR                   FMTD4
339600100423     C                   move      'S1'          $gest
339700100423     C                   move      *on           $FineD4
339800100423     C*
339900130915     C                   ENDSR
340000130915     C************************************************************
340100130915     C* GESTIONE F24 VIDEO D4
340200100423     C* input
340300100423     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
340400100423     C* $FX1 =Testi tasti funzione
340500100423     C* $FC1 = Abilitazioni
340600100423     C* $FR1 = Inversioni di fondo
340700100423     C************************************************************
340800100423     C     F24D4         BEGSR
340900100423     C*
341000100423     C                   MOVEA     $FX4          $FX
341100100423     C                   MOVEA     $FC4          $FC
341200100423     C                   MOVEA     $FR4          $FR
341300100423     C                   Z-ADD     $ULKD4        $ULK
341400100423     C                   Z-ADD     79            $L01
341500100423     C                   Z-ADD     79            $L02
341600100423     C                   EXSR      RIEKEY
341700100423     C                   Z-ADD     $ULK          $ULKD4            3 0
341800100423     C                   MOVEA     $FC           $FC4
341900100423     C                   MOVEL     $KEY1         Z4KE1
342000100423     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
342100100423     C                   MOVEL     $KEY2         Z4KE2
342200100423     C                   WRITE     FMTZ4
342300100423     C*
342400130915     C                   ENDSR
342500940223     C/EJECT
342600980319** $MS1 / $TX1
342700061108PROMSG    *LIBL     OPZ0001
342800980319** $OP1 / $FL1
34290094022501S      <Selezione
343000980319** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
343100061108PROMSG    *LIBL     KEY0001  01
343200940506PROMSG    *LIBL     KEY0002  02
343300940506PROMSG    *LIBL     KEY0003  03
343400940506PROMSG    *LIBL     KEY0004  04
343500940506PROMSG    *LIBL     KEY0005  05
343600940506PROMSG    *LIBL     KEY0006  06
343700940506PROMSG    *LIBL     KEY0007  07
343800940506PROMSG    *LIBL     KEY0008  08
343900940506PROMSG    *LIBL     KEY0009  09
344000061108PROMSG    *LIBL     KEY0010  10
344100061108PROMSG    *LIBL     KEY0011  11
344200061108PROMSG    *LIBL     KEY0012  12
344300061108PROMSG    *LIBL     KEY0013  13
344400061108PROMSG    *LIBL     KEY0014  14
344500061108PROMSG    *LIBL     KEY0015  15
344600061108PROMSG    *LIBL     KEY0016  16
344700061108PROMSG    *LIBL     KEY0017  17
344800061108PROMSG    *LIBL     KEY0018  18
344900061108PROMSG    *LIBL     KEY0019  19
345000940506PROMSG    *LIBL     KEY0020  20
345100940506PROMSG    *LIBL     KEY0021  21
345200940506PROMSG    *LIBL     KEY0022  22
345300940506PROMSG    *LIBL     KEY0023  23
345400940506PROMSG    *LIBL     KEY0024  24
345500980319**  ABILITAZIONE D1 : $FC1 / $FR1
3456009407200N  01
3457009507031N  02
3458009407201N  03
3459009407201N  04
3460009407200N  05
3461009407200N  06
3462009407200N  07
3463009412061N  08
3464009407200N  09
3465000611080N  10
3466009407200N  11
3467009407201N  12
3468009407200N  13
3469009407200N  14
3470009407200N  15
3471009407200N  16
3472009407200N  17
3473000903120N  18
3474009407200N  19
3475009407200N  20
3476009407200N  21
3477009407200N  22
3478009507260N  23   Impostare a '1' se si utilizzano opzioni
3479009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
348000980319** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
348100061108PROMSG    *LIBL     KEY0001  01
348200940916PROMSG    *LIBL     KEY0002  02
348300940916PROMSG    *LIBL     KEY0003  03
348400090312PROMSG    *LIBL     KEY0004  04
348500940916PROMSG    *LIBL     KEY0005  05
348600940916PROMSG    *LIBL     KEY0006  06
348700940916PROMSG    *LIBL     KEY0007  07
348800940916PROMSG    *LIBL     KEY0008  08
348900940916PROMSG    *LIBL     KEY0009  09
349000140319YBARMSG   *LIBL     KEY0114  10
349100130915PROMSG    *LIBL     KEY0011  11
349200130915PROMSG    *LIBL     KEY0012  12
349300061108PROMSG    *LIBL     KEY0013  13
349400130915PROMSG    *LIBL     KEY0345  14
349500130915PROMSG    *LIBL     KEY0015  15
349600130915PROMSG    *LIBL     KEY0016  16
349700061108PROMSG    *LIBL     KEY0017  17
349800061108PROMSG    *LIBL     KEY0018  18
349900061108PROMSG    *LIBL     KEY0019  19
350000940916PROMSG    *LIBL     KEY0020  20
350100940916PROMSG    *LIBL     KEY0021  21
350200940916PROMSG    *LIBL     KEY0022  22
350300940916PROMSG    *LIBL     KEY0023  23
350400940916PROMSG    *LIBL     KEY0024  24
350500980319**  ABILITAZIONE S1 : $FC2 / $FR2
3506009409160N  01
3507009409160N  02
3508009409161N  03
3509000611131N  04
3510000904061N  05
3511000903121N  06
3512009409160N  07
3513000709270N  08
3514000903121N  09
3515001004231N  10
3516009409160N  11
3517009409161N  12
3518001310251N  13
3519001309150N  14
3520001309150N  15
3521001309150N  16
3522001309150N  17
3523001309150N  18
3524001309150N  19
3525009409160N  20
3526009409160N  21
3527009409160N  22
3528009507261N  23   Impostare a '1' se si utilizzano opzioni
3529009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
353000980319** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
353100061108PROMSG    *LIBL     KEY0001  01
353200940506PROMSG    *LIBL     KEY0002  02
353300940506PROMSG    *LIBL     KEY0003  03
353400940506PROMSG    *LIBL     KEY0004  04
353500940506PROMSG    *LIBL     KEY0005  05
353600940506PROMSG    *LIBL     KEY0006  06
353700940506PROMSG    *LIBL     KEY0007  07
353800940506PROMSG    *LIBL     KEY0008  08
353900940506PROMSG    *LIBL     KEY0009  09
354000061108PROMSG    *LIBL     KEY0010  10
354100061108PROMSG    *LIBL     KEY0011  11
354200061108PROMSG    *LIBL     KEY0012  12
354300061108PROMSG    *LIBL     KEY0013  13
354400061108PROMSG    *LIBL     KEY0014  14
354500061108PROMSG    *LIBL     KEY0015  15
354600061108PROMSG    *LIBL     KEY0016  16
354700061108PROMSG    *LIBL     KEY0017  17
354800061108PROMSG    *LIBL     KEY0018  18
354900061108PROMSG    *LIBL     KEY0019  19
355000940506PROMSG    *LIBL     KEY0020  20
355100940506PROMSG    *LIBL     KEY0021  21
355200940506PROMSG    *LIBL     KEY0022  22
355300940506PROMSG    *LIBL     KEY0023  23
355400940506PROMSG    *LIBL     KEY0024  24
355500980319**  ABILITAZIONE D3 : $FC3 / $FR3
3556009407200N  01
3557009407200N  02
3558009407201N  03
3559000611151N  04
3560009407200N  05
3561009407200N  06
3562009407200N  07
3563009407200N  08
3564009407200N  09
3565009409160N  10
3566009407200N  11
3567009407201N  12
3568009407200N  13
3569009407200N  14
3570009407200N  15
3571009407200N  16
3572009409160N  17
3573009409160N  18
3574009407200N  19
3575009407200N  20
3576009407200N  21
3577009407200N  22
3578009507260N  23   Impostare a '1' se si utilizzano opzioni
3579009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
358000130915** TASTI DI FUNZIONE  D4
358100130915PROMSG    *LIBL     KEY0001  01
358200130915PROMSG    *LIBL     KEY0002  02
358300100423PROMSG    *LIBL     KEY0003  03
358400100423PROMSG    *LIBL     KEY0004  04
358500100423PROMSG    *LIBL     KEY0005  05
358600100423PROMSG    *LIBL     KEY0006  06
358700100423PROMSG    *LIBL     KEY0007  07
358800100423PROMSG    *LIBL     KEY0008  08
358900100423PROMSG    *LIBL     KEY0009  09
359000100423PROMSG    *LIBL     KEY0010  10
359100100423PROMSG    *LIBL     KEY0011  11
359200100423PROMSG    *LIBL     KEY0012  12
359300100423PROMSG    *LIBL     KEY0013  13
359400100423PROMSG    *LIBL     KEY0014  14
359500100423PROMSG    *LIBL     KEY0015  15
359600100423PROMSG    *LIBL     KEY0016  16
359700100423PROMSG    *LIBL     KEY0017  17
359800100423PROMSG    *LIBL     KEY0018  18
359900100423PROMSG    *LIBL     KEY0019  19
360000100423PROMSG    *LIBL     KEY0020  20
360100100423PROMSG    *LIBL     KEY0021  21
360200100423PROMSG    *LIBL     KEY0022  22
360300100423PROMSG    *LIBL     KEY0023  23
360400100423PROMSG    *LIBL     KEY0024  24
360500130915**  ABILITAZIONE D4
3606001309150N  01
3607001309150N  02
3608001004231N  03
3609001004261N  04
3610001004231N  05
3611001004231N  06
3612001004230N  07
3613001004230N  08
3614001004230N  09
3615001004230N  10
3616001004230N  11
3617001004231N  12
3618001004230N  13
3619001004230N  14
3620001004230N  15
3621001004230N  16
3622001004230N  17
3623001004230N  18
3624001004230N  19
3625001004230N  20
3626001004230N  21
3627001004230N  22
3628001004230N  23   Impostare a '1' se si utilizzano opzioni
3629001004231N  24   Lasciare sempre impostata a '1' il valore della riga 24
