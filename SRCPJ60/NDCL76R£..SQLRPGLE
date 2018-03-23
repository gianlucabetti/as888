000100070524     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200070524     H*PARMS BNDDIR(PJXBND)
000300070524     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000400070524     H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE)
000500140310     H DFTACTGRP(*NO) ACTGRP('QILE') DATFMT(*ISO) BNDDIR('PJXBND')
000600140310     H DECEDIT('0,') DATEDIT(*DMY.)
000700941115      *
000800090323    > *                Lista Attività Recupero credito
000900090323      *           -------------------------------------
001000090324     FAnUni01L  IF   E           K DISK
001100130923C2226FNdAtt01L  IF   E           K DISK
001200100413     FNdRca01L  IF   E           K DISK
001300101209c2176FNdRca04L  UF   E           K DISK
001400101209c2176F                                     RENAME(NDRCA000:NDRCA04)
001500940211     F*------------
001600090323    >FNDCL76D   CF   E             WORKSTN usropn
001700090323    >F                                     RENAME(CL76D1:FMTD1)
001800090323    >F                                     RENAME(CL76D2:FMTD2)
001900090323    >F                                     RENAME(CL76D3:FMTD3)
002000090323    >F                                     RENAME(CL76S1:FMTS1)
002100090323    >F                                     RENAME(CL76C1:FMTC1)
002200130911C2226F                                     RENAME(CL76C2:FMTC2)
002300130911C2226F                                     RENAME(CL76S2:FMTS2)
002400090323    >F                                     RENAME(CL76T1:FMTT1)
002500090323    >F                                     RENAME(CL76Z1:FMTZ1)
002600090323    >F                                     RENAME(CL76Z2:FMTZ2)
002700090323    >F                                     RENAME(CL76Z3:FMTZ3)
002800110412D2680F                                     RENAME(CL76D5:FMTD5)
002900940131     F                                     SFILE(FMTS1:S1NRR)
003000130911C2226F                                     SFILE(FMTS2:S2NRR)
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
004100130912C2226D $OP2            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004200130912C2226D $FL2            S              1    DIM($dimop) ALT($OP2)
004300960117     D* schiera contenente i nomi dei programma richiamati attraverso
004400960117     D* le opzioni personalizzate
004500960117     D $PGO1           S             10    DIM($dimop)
004600941214     D $TX             S             50    DIM(50)
004700941214     D $FL             S              1    DIM(50)
004800940909     D* Schiere generali per la gestione dei tasti di funzione
004900940506     D $FM             S             27    DIM(24)
005000940506     D $FX             S             50    DIM(24)
005100940506     D $FC             S              1    DIM(24)
005200940506     D $FR             S              1    DIM(24)
005300940909     D* Schiere per la gestione dei tasti di funzione
005400940506     D* Video D1
005500940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005600940506     D $FX1            S             50    DIM(24) ALT($FM1)
005700940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005800940506     D $FR1            S              1    DIM(24) ALT($FC1)
005900960202     D* Schiera contente i nomi dei programmi da richiamare con i tasti
006000960202     D* funzione personalizzata S1
006100960202     D $PGF2           S             10    DIM(25)
006200940506     D* Schiere per la gestione dei tasti di funzione
006300940506     D* Video S1
006400940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006500940506     D $FX2            S             50    DIM(24) ALT($FM2)
006600940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006700940506     D $FR2            S              1    DIM(24) ALT($FC2)
006800940916     D* Schiere per la gestione dei tasti di funzione
006900940916     D* Video D2
007000940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
007100940916     D $FX3            S             50    DIM(24) ALT($FM3)
007200940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007300940916     D $FR3            S              1    DIM(24) ALT($FC3)
007400090325     D*-------------
007500090325     D* Video S1
007600090325     D WC1Rcd          S                   like(c1nrr) inz
007700090325     D $OpzOk          S              1
007800090325     D WOpz            S                   like(S1Opz) inz
007900090325     D $X              S                   like(c1nrr) inz
008000950726     D*
008100130911C2226D* Video S2
008200130911  "  D WC2Rcd          S                   like(c2nrr) inz
008300130911  "  D $OpzOkS2        S              1
008400130911  "  D WOpzS2          S                   like(S2Opz) inz
008500130911  "  D $XS2            S                   like(c2nrr) inz
008600130911C2226D*
008700950726     D $D1L01          S              3  0
008800950726     D $D1L02          S              3  0
008900950726     D $S1L01          S              3  0
009000950726     D $S1L02          S              3  0
009100130911C2226D $S2L01          S              3  0
009200130911C2226D $S2L02          S              3  0
009300950906     D $C1Op1          S              3  0
009400950906     D $C1Op2          S              3  0
009500950906     D $D2L01          S              3  0
009600950906     D $D2L02          S              3  0
009700130911C2226D $C2Op1          S              3  0
009800130911C2226D $C2Op2          S              3  0
009900940302     D*
010000960202     D*-------------
010100960202     D* Indice della schiera tasti funzione
010200960202     D PosFun          S              2  0
010300940211     D/EJECT
010400940128     D*----------------------------------------------------*
010500100402     DDsFetch          Ds
010600100407     D DsSocieta                           like(RcaSocieta)
010700100402     D DsPosiz                             like(RcaPosiz)
010800100407     D DsAttrib                            like(RcaAttrib)
010900100402     D DsKcc                               like(RcaKcc)
011000100402     D DsKsc                               like(RcaKsc)
011100100402     D DsDtPar                             like(RcaDtPar)
011200100402     D DsNrPar                             like(RcaNrPar)
011300100402     D DsSerPar                            like(RcaSerPar)
011400100419     D Dsdivisa                            like(RcaDivisa)
011500100407     D DskscAge                            like(RcakscAge)
011600130910C2226DDsFetchPre       Ds
011700130910  "  D Ds2Societa                          like(RcaSocieta)
011800130918  "  D Ds2Unita                            like(RcaUnita)
011900130918  "  D Ds2Kcc                              like(RcaKcc)
012000130910  "  D Ds2CdFisc                     20
012100130910  "  D Ds2Partiva                    20
012200130918  "  D Ds2Sogg                        8
012300130919  "  D Ds2descr                      44
012400130918  "  D Ds2Importo                    20  4
012500130918  "  D*
012600130919  "  D DsfetchTab      DS
012700130918  "  D KccPre                              like(H2kcc)
012800130918  "  D Partivapre                          like(H2partiva)
012900130918  "  D CdfiscPre                           like(H2CdFisc)
013000130918  "  D SoggPre                             like(H2Sogg)
013100130918  "  D DescrPre                            like(S2descr)
013200130919  "  D ImpPre                        19  3
013300130918  "  D*
013400130918  "  D $PKcc           S                   Like(RcaKcc)
013500130919  "  D $PSogg          S                   Like(H2Sogg)
013600130918  "  D $PPartiva       S                   Like(H2Partiva)
013700130918  "  D $PCdFisc        S                   Like(H2CdFisc)
013800130919  "  D $First          S              1    inz
013900130918C2226D*
014000940211     D*-------------
014100940207     D* Valori assunti
014200950524     D XMPEDS        E DS                  INZ
014300950524     D XMPEDS1       E DS                  INZ
014400941111     D DSMPE           DS                  INZ
014500940207     D  $LOP                   1      1  0
014600940831     D  $ORD                   2      3  0
014700130912C2226D  $PRE                   4      4
014800130912C2226D  $RGRPRE                5      5
014900940207     D*-------------
015000940211     D* Passaggio Parametri
015100940211     D KPJBA         E DS
015200940211     D*-------------
015300940225     D* Reperimento Autorizzazioni
015400941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015500070531     D*-------------
015600070531     D* Dati per pgm XNOAUT
015700070531     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
015800941124     D*-------------
015900950410     D* Passaggio Autorizzazioni
016000950410     D DSAUT           DS           512
016100090325     D*-------------
016200090325     D* Maschera dati autorizzazioni
016300100409    >D CONRECDS      E DS
016400941209     D*-------------
016500130909C2226D* Autorizzazioni alla unità
016600130909  "  D XAUTUNIDS     E DS
016700130909C2226D*-------------
016800941209     D* Dati di ambiente ottenuti da XSOC
016900970424     D SOC001        E DS                  EXTNAME(XSOC001DS)
017000090324     D    FgO1                        1    overlay(XscFgO)
017100941209     D*-------------
017200941209     D* DS Interna per dati di output di XSOC
017300941209     D XSOCDS          DS          1000
017400070629     D*-------------
017500070629     D* DS gestione parametri modulo
017600070629     D XPAOUT          DS          2000
017700090324     D anp057ds      E DS
017800940325     D*-------------
017900940325     D* Parametri in ricezione
018000090323    >D NDCL76DS      E DS
018100090330     D*-------------
018200090330      * Parametri per scheda cliente
018300090330     D NDCL77DS      E DS                  inz
018400090324     D*-------------
018500090324      * Parametri per ricerca unita
018600090324     Dana009ds       E DS                  inz
018700090323     D*-------------
018800090323     D* Ricerca capoconti
018900090323     D A200DS        E DS                  EXTNAME(ANA200DS) INZ
019000090323     D*-------------
019100090323     D* Ricerca sottoconti
019200090323     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
019300090324     D*-------------
019400090324     D XATBDS        E DS                  INZ
019500090324     D*-------------
019600090324     D* Parametri per ricerca codice ANATB00F
019700090324     D XTABDS        E DS                  INZ
019800090416     D*-------------
019900100407     D* Tabella azioni
020000100407     D Ang047DS      E DS                  Inz
020100100407     D*-------------
020200100407     D* Tabella destinatari
020300100401     D Ang048DS      E DS                  Inz
020400090409     D*-------------------------------------
020500090409     D*  ds  attributi contabili
020600090409     D Ang0A2ds      E DS                  INZ
020700090409     D Ang0A1ds      E DS                  INZ
020800090409     D*-------------------------------------
020900090324     D* Ricerca legende
021000090324     D Ana964ds      E DS                  INZ
021100090324     D*-------------
021200090324     D* Controlli validità legende
021300090324     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
021400090324     D*-------------
021500090324     D* Ricerca profili
021600090324     D Ana001ds      E DS                  inz
021700090324     D*----------------------------------------------------*
021800090324     D* Reperimento profilo
021900090324     D parmok          s              1
022000090324     D* parametro utente
022100090324     D ANUTNParm       s                   like(ANUTN00F)
022200090324     D* lunghezza parametro
022300090324     D LungUTN         S              5s 0
022400090324     D*
022500090324     D Anutn00f      e ds
022600070524     D*-------------
022700070524     D* Parametri del PGM di interrogazione conti
022800070524    >D ANA399DS      E DS                  inz
022900070524    >D  A39RET       E                     INZ('0')
023000070524    >D  A39ERRORI    E                     INZ('0')
023100130919C2226D*-------------
023200130919  "  D* Parametri del PGM di interrogazione conti
023300130919C2226D NDC981DS      E DS                  inz
023400090406     D*-------------
023500070525     D* Controllo conti
023600070525     D ND002DS       E DS
023700090409     D Categorie             131    250
023800100402     D*-------------
023900100402     D* Ricerca agenti
024000100402     D A400DS        E DS                  EXTNAME(ANA400DS) INZ
024100070525     D*-------------
024200090409     D* Schiera categorie da ND002DS
024300090409     D DsCateg         DS           120
024400090409     D SkCateg                        4    dim(30)
024500090409     D*-------------
024600070525     D* Definizioni parametri standard chiamate moduli
024700070525     D$Esito           S              1
024800070525     D$gesmsg          S              1
024900070525     D$lenout          S              9B 0
025000070525     D$struttura       S             10
025100070525     D$kcc             S              6
025200070525     D$ksc             S              8
025300090323     D*-------------
025400960202     D* campo di salvataggio per la DS d'ingresso durante la chiamata
025500960202     D* alla funzione personalizzata
025600090323    >D $SaveDS         S                   like(NDCL76DS)
025700090325     D*-------------
025800090325     D*tasto precedente = F13=selezione multipla
025900090325     D  $TastoSav      s                   Like($Tasto) inz
026000130911C2226D  $TastoSav2     s                   Like($Tasto) inz
026100090325     D*emetto al window di conf.canc. solo x il 1° rec.da cancellare
026200090325     D  PrimaVolta     s              1  0 inz(0)
026300960202     D*-------------
026400940211     D DSFMT           DS           512
026500940506     D  $TASTO               369    369
026600940211     D  NRG                  370    370
026700940211     D  NCL                  371    371
026800940211     D  SFLNRR               378    379B 0
026900090324      * riga cursore window
027000090324     D  NRgWin               382    382
027100090324      * colonna cursore window
027200090324     D  NClWin               383    383
027300940211     D*-------------
027400940211     D* posizione cursore
027500940211     D CURSOR          DS
027600940211     D  RIRI                   1      2B 0 INZ
027700940211     D  R$$                    2      2
027800940211     D  COCO                   3      4B 0 INZ
027900940211     D  C$$                    4      4
028000940127     D*-------------
028100940127     D* Reperimento nome PGM
028200940127     D STATUS         SDS           333
028300940127     D  DSPGM            *PROC
028400950411     D  PARMS            *PARMS
028500101209c2176D  SDSUSR               254    263
028600940207     D*-------------
028700940616     D* Costanti
028800940616     D* nr.di righe del sfl (SFLPAG)
028900090330     D SFLPAG          C                   CONST(16)
029000940616     D*-------------
029100940506     D* Tasti di funzione
029200940506     D F01             C                   CONST(X'31')
029300940506     D F02             C                   CONST(X'32')
029400940506     D F03             C                   CONST(X'33')
029500940506     D F04             C                   CONST(X'34')
029600940506     D F05             C                   CONST(X'35')
029700940506     D F06             C                   CONST(X'36')
029800940506     D F07             C                   CONST(X'37')
029900940506     D F08             C                   CONST(X'38')
030000940506     D F09             C                   CONST(X'39')
030100940506     D F10             C                   CONST(X'3A')
030200940506     D F11             C                   CONST(X'3B')
030300940506     D F12             C                   CONST(X'3C')
030400940506     D F13             C                   CONST(X'B1')
030500940506     D F14             C                   CONST(X'B2')
030600940506     D F15             C                   CONST(X'B3')
030700940506     D F16             C                   CONST(X'B4')
030800940506     D F17             C                   CONST(X'B5')
030900940506     D F18             C                   CONST(X'B6')
031000940506     D F19             C                   CONST(X'B7')
031100940506     D F20             C                   CONST(X'B8')
031200940506     D F21             C                   CONST(X'B9')
031300940506     D F22             C                   CONST(X'BA')
031400940506     D F23             C                   CONST(X'BB')
031500940506     D F24             C                   CONST(X'BC')
031600940506     D ENTER           C                   CONST(X'F1')
031700940506     D ROLDWN          C                   CONST(X'F4')
031800940506     D ROLLUP          C                   CONST(X'F5')
031900961017     D*-------------
032000021010     D* Attributi video
032100021010     D Normale         C                   Const(X'32')
032200021010     D Protetto        C                   Const(X'A0')
032300021010     D*-------------
032400950524     D* Sottotitolo di testata
032500070524     D SOTTIT          C                   CONST(' ')
032600070524     D*SOTTIT          C                   CONST('PROMSG    *LIBL-
032700070524     D*                                         COSxxxx')
032800960109     D*-------------
032900950606     D* Variabili per gestione videate
033000950606     D $Gest           S              2A
033100950606     D $Fine           S              1A
033200950606     D $InzD1          S              1A
033300950606     D $InzS1          S              1A
033400950606     D $InzD2          S              1A
033500130911C2226D $InzS2          S              1A
033600950606     D $LastG          S              2A
033700950606     D $LastV          S              2A
033800950606     D $EndFile        S              1A
033900130911C2226D $EndFilePre     S              1A
034000950606     D $Esci           S              1A
034100950606     D $RcdOK          S              1A
034200130911C2226D $RcdOKPre       S              1A
034300950606     D $Unique         S              1A
034400950606     D*-------------
034500950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
034600950606     D S1Nrr           S                   like(C1Rcd)
034700130911C2226D S2Nrr           S                   like(C2Rcd)
034800950606     D WSfl            S                   like(C1Nrr)
034900130911C2226D WSfl2           S                   like(C1Nrr)
035000950606     D WMax            S                   like(C1Rcd)
035100130911C2226D WMaxS2          S                   like(C2Rcd)
035200950606     D WPag            S                   like(C1Rcd)
035300130911C2226D WPag2           S                   like(C1Rcd)
035400950606     D WInzS1          S                   like($InzS1)
035500130911C2226D WInzS2          S                   like($InzS2)
035600090323    >D WOpr            S                   like(OprL76)
035700090323     D CurR            S              3S 0
035800090323     D CurC            S              3S 0
035900950606     D Win             S             99A
036000950606     D W1Pos           S                   like(C1Pos)
036100130911C2226D W2Pos           S                   like(C2Pos)
036200950606     D WINKR           S              1A
036300970912     D WExf            S              1A
036400070711     D ZDateISO        S               D
036500070525     D*-------------
036600070525     D* Variabili specifiche del programma
036700950606     D*-------------
036800100420     D WDtParDa        S                   like(Zdateiso)
036900100420     D WDtParA         S                   like(Zdateiso)
037000100401     D WDtScaDa        S                   like(Zdateiso)
037100100401     D WDtScaA         S                   like(Zdateiso)
037200100401     D WDtAttDa        S                   like(Zdateiso)
037300100401     D WDtAttA         S                   like(Zdateiso)
037400140319     D WDtChiDa        S                   like(Zdateiso)
037500140319     D WDtChiA         S                   like(Zdateiso)
037600100416     D WDtSolDa        S                   like(Zdateiso)
037700100416     D WDtSolA         S                   like(Zdateiso)
037800070525     D Wdate           S               D
037900130918C2226D WInUni          S           4000
038000130913  "  D WExi            S           1024
038100130913  "  D* ds nota di chiusura fase
038200130913  "  D d5nota          DS           400
038300130913  "  D d5nota1                 1     36
038400130913  "  D d5nota2                37     88
038500130913  "  D d5nota3                89    140
038600130913  "  D d5nota4               141    192
038700130913  "  D d5nota5               193    244
038800130913  "  D d5nota6               245    296
038900130913  "  D d5nota7               297    348
039000130913C2226D d5nota8               349    400
039100070525     D*-------------
039200950606     D*  Indici
039300070524     D X               S              5S 0
039400061116     D Y               S              5S 0
039500960117     D* Indice della schiera per le opzioni personalizzate
039600960117     D PosOpz          S              2  0
039700960117     D* numero max opzioni
039800960117     D NumOpz          S              2  0 Inz($DIMOP)
039900960117     D* numero effettive opzioni
040000960117     D OpzLen          S              2  0
040100070525     D*-------------
040200070525     D* Stringa per SQL
040300130919C2226X*** SQ              DS          5000
040400130919C2226D SQ              DS          5000
040500070531     D*-------------
040600070531     D $A              s             79
040700070531     D $B              s             79
040800070725     D*-------------
040900070725     D* DS Esterna gestione messaggi                                            *
041000070725     D XMSGDS        E DS                  inz                                  *
041100070725     D  STS          E                     EXTFLD(MSGSTS)                       *
041200070725     D                                     DIM(333)                             *
041300070725     D  JBA          E                     EXTFLD(MSGJBA)                       *
041400070725     D                                     DIM(502)                             *
041500070725     D  MSGRSP       E                     INZ('N')
041600070725     D  MSGEMV       E                     INZ('S')
041700070725     D  MSGMSG       E                     INZ(0)
041800070725     D  MSGRGP       E                     INZ(10)
041900070725     D  MSGCNL       E                     INZ('N')
042000070725     D  MSGVID       E                     INZ('N')
042100070725     D  MSGOPE       E                     INZ('N')
042200070725     D  MSGSTP       E                     INZ('N')
042300070725     D  MSGLCK       E                     INZ('N')
042400070725     D*---------------
042500090409    9DPuntini          C                   const('...................')
042600100407     D*  bottone presente
042700100407     D PRESENTE        C                   CONST(X'A1')
042800100407     D PRESERED        C                   CONST(X'A9')
042900100407     D PRESEPNK        C                   CONST(X'B9')
043000100408     D PRESEBLU        C                   CONST(X'BB')
043100100412     D PRESEHIG        C                   CONST(X'23')
043200100511     D PRESETRQ        C                   CONST(X'B1')
043300110412D2680D WWMSG           s                   like(Txtmsg)
043400110412  "  D* schiera costanti
043500110412D2680D MSG             S             27    DIM(10) CTDATA PERRCD(1)             messaggi possibili
043600940127     C*----------------------------------------------------*
043700940127     C*                MAIN LINE PROGRAM
043800940127     C*----------------------------------------------------*
043900940127     C*
044000940223     C* inizializzazione variabili
044100940223     C                   EXSR      INZVAR
044200940223     C*
044300940223     C     $FINE         DOWEQ     *OFF
044400940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
044500940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
044600940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
044700130911C2226C     $GEST         CASEQ     'S2'          GESS2                          Lista
044800941027     C                   ENDCS
044900941027     C                   ENDDO
045000940325     C* fine programma
045100940325     C                   EXSR      ENDPGM
045200940225     C*
045300940325     C/EJECT
045400940325     C************************************************************
045500940325     C* FINE PROGRAMMA
045600940325     C************************************************************
045700940325     C     ENDPGM        BEGSR
045800940325     C*
045900940325     C* passaggio dati al pgm chiamante
046000090323    >C                   MOVEL     WOPR          OPRL76                                     ne S1
046100090323    >C                   MOVEL     NDCL76DS      KPJBU
046200940325     C*
046300940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
046400090323    >C     OPZL76        IFEQ      *BLANK
046500940325     C                   SETON                                            LR
046600940325     C* altrimenti chiudo in RT
046700940325     C                   ELSE
046800940325     C                   SETON                                            RT
046900940325     C                   ENDIF
047000940616     C                   RETURN
047100940325     C*
047200940325     C                   ENDSR
047300940224     C/EJECT
047400940201     C************************************************************
047500940201     C* RICERCHE
047600940201     C************************************************************
047700940127     C     SEARCH        BEGSR
047800940127     C*
047900090324     C* determino Riga/Colonna del cursore
048000090324     c                   if        $lastV='D2'
048100090324     C                   MOVE      NRGWIN        R$$
048200090324     C                   MOVE      NCLWIN        C$$
048300090324     C                   else
048400090324     C                   MOVE      NRG           R$$
048500090324     C                   MOVE      NCL           C$$
048600090324     c                   endif
048700940127     C                   Z-ADD     RIRI          CURR
048800940127     C                   Z-ADD     COCO          CURC
048900090323     C*
049000140318brt  C     D1KCC         IFeq      *BLANKS
049100140318brt  C                   evaL      D1KCC = 'C'
049200140318brt  C                   ENDIF
049300140318     C*
049400090323     C     D1KCC         IFNE      *BLANKS
049500090323     C     D1KCC         ANDNE     'C     '
049600090323     C     D1KCC         ANDNE     'F     '
049700090323     C                   CALL      'XALLINEA'
049800090323     C                   PARM                    D1KCC
049900090323     C                   PARM      6             LEN               5 0
050000090323     C                   PARM                    XSCINP
050100090323     C                   ENDIF
050200090323     C*
050300090323     C     D1KSC         IFNE      *BLANKS
050400090323     C                   CALL      'XALLINEA'
050500090323     C                   PARM                    D1KSC
050600090323     C                   PARM      8             LEN               5 0
050700090323     C                   PARM                    XSCINP
050800090323     C                   ENDIF
050900100402     C*
051000100402     C* agente recupero crediti
051100100402     C     D1KSCAGE      IFNE      *BLANKS
051200100402     C                   CALLB     'XALLINEA'
051300100402     C                   PARM                    D1KSCAGE
051400100402     C                   PARM      8             LEN               5 0
051500100402     C                   PARM                    XSCINP
051600100402     C                   ENDIF
051700090323     C*
051800090323   B1C                   SELECT
051900090324    >C* Ricerca unita
052000090324     C     H1NMFL        wheneq    'D1UNITA'
052100090324     C                   RESET                   Ana009ds
052200090324     C                   MOVE      '01'          opz009
052300090324     C                   MOVE      xscsoc        soc009
052400090324     C                   MOVEL     Ana009ds      KPJBU
052500090324     C                   CALL      'ANA009R'
052600090324     C                   PARM                    KPJBA
052700090324     C                   MOVEL     KPJBU         Ana009ds
052800090324     C*
052900090324     C     uni009        IFNE      *BLANK
053000090324     C                   MOVE      uni009        d1unita
053100090324     C                   ENDIF
053200090324     C* Ricerca capoconto
053300090323     C     H1NMFL        WHENEQ    'D1KCC'
053400090323     C                   RESET                   A200DS
053500090323     C                   MOVEL     '01'          OPZ200
053600090323     C                   MOVE      D1KCC         KCC200
053700090323     C                   MOVEL     A200DS        KPJBU
053800090323     C                   CALL      'ANA200R'
053900090323     C                   PARM                    KPJBA
054000090323     C                   MOVEL     KPJBU         A200DS
054100090323     C*
054200090323     C     KCC200        IFNE      *BLANK
054300090323     C                   MOVEL     KCC200        D1KCC
054400090323     C                   ENDIF
054500090323     C*
054600090324     C* Ricerca sottoconto
054700090323     C     H1NMFL        WHENEQ    'D1KSC'
054800090323     C                   RESET                   A201DS
054900090323     C                   MOVE      XSCSOC        SOC201
055000090323     C                   MOVE      D1KCC         KCC201
055100090323     C                   MOVE      D1KsC         KSC201
055200090323     C                   MOVEL     A201DS        KPJBU
055300090323     C                   CALL      'ANA201R'
055400090323     C                   PARM                    KPJBA
055500090323     C                   MOVEL     KPJBU         A201DS
055600090323     C*
055700090323     C     KSC201        IFNE      *BLANK
055800090323     C                   MOVEL     KCC201        D1KCC
055900090323     C                   MOVEL     KSC201        D1KSC
056000090323     C                   ENDIF
056100140317     c*
056200140317     C* Fasi selezione tipo
056300140317     C     H1NMFL        WHENEQ    'D1FASIAPE'
056400140317     C                   RESET                   ANA964DS
056500140317     C                   MOVE      '01'          OPZ964
056600140317     C                   MOVEL     'ANDIZ000'    TRC964
056700140317     C                   MOVEL     'DSPSELFASI'  CAM964
056800140317     C                   MOVEL     *ON           ALL964
056900140317     C                   CLEAR                   KPJBU
057000140317     C                   MOVEL     ANA964DS      KPJBU
057100140317     C                   CALL      'ANA964R'
057200140317     C                   PARM                    KPJBA
057300140317     C                   MOVEL     KPJBU         ANA964DS
057400140317     C     Ret964        IFNE      '2'
057500140317     C                   MOVEL     Val964        D1FASIAPE
057600140317     C                   MOVEL     DES964        D1FASIAPED
057700140317     C                   ENDIF
057800100402     C*  Attributo
057900100402     C     H1NMFL        WHENEQ    'D1ATTRIB'
058000100402     C                   RESET                   XTABDS
058100100402     C                   MOVE      '01'          XTAOPZ
058200100402     C                   MOVE      XSCSOC        XTAAZI
058300100402     C                   MOVE      XSCLIN        XTALIN
058400100402     C                   Z-ADD     CURR          XTARIG
058500100402     C                   Z-ADD     CURC          XTACOL
058600100402     c                   move      posattr057    xtapos
058700100402     C                   CLEAR                   KPJBU
058800100402     C                   MOVEL     XTABDS        KPJBU
058900100402     C                   CALL      'ANG0A1R'
059000100402     C                   PARM                    KPJBA
059100100402     C                   MOVEL     KPJBU         XTABDS
059200100402     C     XTAret        IFNE      '2'
059300100402     C                   MOVE      XTAKEY        D1Attrib
059400100402     C                   ENDIF
059500100402     C* - Agente
059600100402     C     H1NMFL        WHENEQ    'D1KSCAGE'
059700100402     C                   RESET                   A400DS
059800100402     C                   MOVEL     '01'          OPZ400
059900100402     C                   MOVE      '3'           AGE400
060000100402     C                   MOVEL     A400DS        KPJBU
060100100402     C                   CALL      'ANA400R'
060200100402     C                   PARM                    KPJBA
060300100402     C                   MOVEL     KPJBU         A400DS
060400100402     C*
060500100402     C     KSC400        IFNE      *BLANK
060600100402     C                   MOVEL     KSC400        D1KSCAGE
060700100402     C                   ENDIF
060800090324     C*
060900090324     C* Ricerca destinatario
061000090324     C     H1NMFL        WHENEQ    'D1DESTIN '
061100090324    >C                   RESET                   ANA001DS
061200090324    >C                   MOVEL     '01'          OPZ001
061300090324    >C                   MOVEL     ANA001DS      KPJBU
061400090324    >C                   CALL      'ANA001R'
061500090324     C                   PARM                    KPJBA
061600090324    >C                   MOVEL     KPJBU         ANA001DS
061700090324     C     RET001        IFNE      '2'
061800090324     c                   eval      D1Destin = Prf001
061900090324     C                   ENDIF
062000100407     C*  Azione
062100100407     C     H1NMFL        WHENEQ    'D1AZIONE'
062200100407     C                   RESET                   XTABDS
062300100407     C                   MOVE      '01'          XTAOPZ
062400100407     C                   MOVE      XSCSOC        XTAAZI
062500100407     C                   MOVE      XSCLIN        XTALIN
062600100407     C                   Z-ADD     CURR          XTARIG
062700100407     C                   Z-ADD     CURC          XTACOL
062800100407     C                   CLEAR                   KPJBU
062900100407     C                   MOVEL     XTABDS        KPJBU
063000100407     C                   CALL      'ANG047R'
063100100407     C                   PARM                    KPJBA
063200100407     C                   MOVEL     KPJBU         XTABDS
063300100407     C     XTAret        IFNE      '2'
063400100407     C                   MOVE      XTAKEY        D1AZIONE
063500100407     C                   ENDIF
063600090324     c*
063700090324     C* Ordinamento
063800090324     C     H1NMFL        WHENEQ    'D2ORD'
063900090324     C                   RESET                   ANA964DS
064000090324     C                   MOVE      '01'          OPZ964
064100090324     C                   MOVEL     'ANDIZ000'    TRC964
064200100401     C                   MOVEL     'DSPORDRCA'   CAM964
064300090324     C                   MOVEL     *oFF          ALL964
064400090324     C                   CLEAR                   KPJBU
064500090324     C                   MOVEL     ANA964DS      KPJBU
064600090324     C                   CALL      'ANA964R'
064700090324     C                   PARM                    KPJBA
064800090324     C                   MOVEL     KPJBU         ANA964DS
064900090324     C     Ret964        IFNE      '2'
065000090324     C                   MOVEL     Val964        D2ORD
065100090324     C                   MOVEL     DES964        D2ORDD
065200090324     C                   ENDIF
065300130912C2226C* Raggruppamento
065400130912  "  C     H1NMFL        WHENEQ    'D2RGRPRE'
065500130912  "  C                   RESET                   ANA964DS
065600130912  "  C                   MOVE      '01'          OPZ964
065700130912  "  C                   MOVEL     'ANDIZ000'    TRC964
065800130912  "  C                   MOVEL     'DSPRGRSCL'   CAM964
065900130912  "  C                   MOVEL     *oFF          ALL964
066000130912  "  C                   CLEAR                   KPJBU
066100130912  "  C                   MOVEL     ANA964DS      KPJBU
066200130912  "  C                   CALL      'ANA964R'
066300130912  "  C                   PARM                    KPJBA
066400130912  "  C                   MOVEL     KPJBU         ANA964DS
066500130912  "  C     Ret964        IFNE      '2'
066600130912  "  C                   MOVEL     Val964        D2RGRPRE
066700130912  "  C                   MOVEL     DES964        D2RGRPRED
066800130912C2226C                   ENDIF
066900090323     C*
067000940223     C* ricerca non abilitata in questa posizione
067100941118   X1C                   OTHER
067200940929     C                   SETON                                        9899
067300940127     C*
067400941118   E1C                   ENDSL
067500950612     C*
067600950612     C* imposto pos. del cursore
067700950612     C                   Z-ADD     CURR          H1RIGA
067800950612     C                   Z-ADD     CURC          H1COLO
067900940506     C                   MOVE      *HIVAL        $LASTV
068000950612     C*
068100940127     C                   ENDSR
068200940131     C/EJECT
068300940224     C************************************************************
068400940131     C* RIEMPIMENTO OPZIONI
068500940224     C************************************************************
068600940131     C     RIEOPZ        BEGSR
068700940223     C*
068800940223     C* Input :
068900940223     C* - $TX = schiera con tutti i testi disponibili
069000940223     C* - $FL = schiere con i flag di validità associati ai testi
069100940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
069200940223     C*          opzione usato
069300940223     C* - WMXO = variabile contenente il numero massimo di testi
069400940223     C*          opzione usabili
069500940721     C* - $L01 = Lunghezza prima riga
069600940721     C* - $L02 = Lunghezza seconda riga
069700940223     C* Output :
069800940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
069900940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
070000941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
070100940223     C*
070200950522     C                   CALLB     'X04OPMSG'
070300940223     C* input
070400940223     C                   PARM                    $TX
070500940223     C                   PARM                    $FL
070600940225     C                   PARM                    WCOZ              3 0
070700940223     C                   PARM                    WMXO              3 0
070800940223     C* output
070900940223     C                   PARM                    WOPZ1            76
071000940223     C                   PARM                    WOPZ2            76
071100941214     C                   PARM                    FF23              1
071200950301     C* input
071300950726     C                   PARM                    $L01              3 0
071400950726     C                   PARM                    $L02              3 0
071500940223     C*
071600060224     C                   PARM                    $ALLOPT         960
071700060224     C*
071800940131     C                   ENDSR
071900940506     C/EJECT
072000940506     C************************************************************
072100940506     C* RIEMPIMENTO TASTI FUNZIONE
072200940506     C************************************************************
072300940506     C     RIEKEY        BEGSR
072400940506     C*
072500940506     C* Input :
072600940506     C* - $FX = schiera con tutti i testi disponibili
072700940506     C* - $FC = schiere con i flag di validità associati ai testi
072800940506     C* - $FR = schiere con i flag di REVERSE IMAGE
072900940506     C* - $ULK = ultimo testo caricato
073000940721     C* - $L01 = Lunghezza prima riga
073100940721     C* - $L02 = Lunghezza seconda riga
073200940506     C* Output :
073300940506     C* - $ULK
073400940609     C* - $KEY1= 1° riga di tasti funzione
073500940609     C* - $KEY2= 2° riga di tasti funzione
073600940506     C*
073700950522     C                   CALLB     'XKEYMSG'
073800940506     C* input
073900940506     C                   PARM                    $FX
074000940506     C                   PARM                    $FC
074100940506     C                   PARM                    $FR
074200940506     C                   PARM                    $ULK              3 0
074300940506     C* output
074400940506     C                   PARM                    $KEY1            79
074500940506     C                   PARM                    $KEY2            79
074600950301     C* input
074700950726     C                   PARM                    $L01              3 0
074800950726     C                   PARM                    $L02              3 0
074900940506     C*
075000060224     C                   PARM                    $ALLFUNCT       480
075100060224     C*
075200940506     C                   ENDSR
075300940224     C/EJECT
075400940224     C************************************************************
075500940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
075600940224     C************************************************************
075700940224     C     GESPIE        BEGSR
075800940224     C*
075900940224     C                   SELECT
076000940224     C* Parzializzazioni
076100940224     C     $GEST         WHENEQ    'D1'
076200940506     C     $ULKD1        IFEQ      0
076300940506     C* riempimento opzioni
076400940506     C                   EXSR      F24D1
076500940506     C                   ELSE
076600940506     C                   WRITE     FMTZ1
076700940506     C                   ENDIF
076800130911C2226C* PreLista
076900130911  "  C     $GEST         WHENEQ    'S2'
077000130911  "  C     $ULKS2        IFEQ      0
077100130911  "  C                   EXSR      F24S2
077200130911  "  C                   ELSE
077300130911  "  C                   WRITE     FMTZ2
077400130911C2226C                   ENDIF
077500940506     C* Lista
077600940506     C     $GEST         WHENEQ    'S1'
077700940506     C     $ULKS1        IFEQ      0
077800940506     C                   EXSR      F24S1
077900940506     C                   ELSE
078000940506     C                   WRITE     FMTZ2
078100940506     C                   ENDIF
078200940916     C* Modifica valori assunti
078300940916     C     $GEST         WHENEQ    'D2'
078400940916     C     $ULKD2        IFEQ      0
078500940916     C                   EXSR      F24D2
078600940916     C                   ELSE
078700940916     C                   WRITE     FMTZ3
078800940916     C                   ENDIF
078900940506     C                   ENDSL
079000940506     C*
079100940224     C                   ENDSR
079200940224     C/EJECT
079300940224     C************************************************************
079400940224     C* GESTIONE PARZIALIZZAZIONI
079500940224     C************************************************************
079600940224     C     GESD1         BEGSR
079700940224     C* inizializzazione videata
079800940223     C     $INZD1        IFEQ      *ON
079900940127     C                   EXSR      INZD1
080000940223     C                   MOVE      *OFF          $INZD1
080100941027     C                   ENDIF
080200940223     C* emissione piede videata se proveniente da altra
080300940223     C* salvataggio valore formato di provenienza
080400940202     C     $LASTV        IFNE      'D1'
080500940223     C* salvo stato F17 per sapere cosa fare in caso di F12
080600940223     C                   MOVE      *INKR         WINKR
080700940202     C                   WRITE     FMTT1
080800940224     C                   EXSR      GESPIE
080900940202     C                   MOVE      $LASTV        $LASTG
081000940202     C                   MOVE      'D1'          $LASTV
081100940707     C                   ENDIF
081200940707     C*
081300940707     C* se esistono errori sulla videata
081400940707     C* emetto la write del formato a indicatori spenti per vedere
081500940707     C* le eventuali decodifiche
081600940707     C     *IN99         IFEQ      *ON
081700940707     C                   MOVEA     *IN           WIN
081800940707     C                   MOVE      *ALL'0'       IN5098           49
081900940707     C                   MOVEA     IN5098        *IN(50)
082000940707     C                   WRITE     FMTD1
082100940707     C                   MOVEA     WIN           *IN
082200940707     C                   ENDIF
082300940131     C*
082400940131     C                   EXFMT     FMTD1
082500960605     C*
082600960605     C* azzero la posizione del cursore
082700960605     C                   Z-ADD     0             H1RIGA
082800960605     C                   Z-ADD     0             H1COLO
082900940506     C*
083000951005     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
083100951005     C* INPUT
083200951005     C* $FC1 = Abilitazioni videata D1
083300951005     C* OUTPUT
083400951005     C* *IN97 + *IN99 Errori
083500951005     C                   MOVEA     $FC1          $FC
083600951005     C                   EXSR      CTRKEY
083700940506     C*
083800940506     C     *IN99         IFEQ      *OFF
083900940131     C*
084000940117     C                   SELECT
084100940916     C* F2=Decodifica
084200940916     C     $TASTO        WHENEQ    F02
084300940916     C                   EXSR      F02D1
084400940131     C* F3=Fine
084500940506     C     $TASTO        WHENEQ    F03
084600940307     C                   EXSR      F03D1
084700940131     C* F4=Ricerca
084800940506     C     $TASTO        WHENEQ    F04
084900940127     C                   EXSR      SEARCH
085000940916     C* F5=Refresh
085100940916     C     $TASTO        WHENEQ    F05
085200940916     C                   EXSR      F05D1
085300941206     C* F8=Successivo
085400941206     C     $TASTO        WHENEQ    F08
085500941206     C                   EXSR      F08D1
085600940131     C* F12=Ritorno
085700940506     C     $TASTO        WHENEQ    F12
085800940307     C                   EXSR      F12D1
085900940131     C* F18=Modifica valori assunti
086000940506     C     $TASTO        WHENEQ    F18
086100940307     C                   EXSR      F18D1
086200940224     C* F24=Altri tasti
086300940506     C     $TASTO        WHENEQ    F24
086400940309     C                   EXSR      F24D1
086500940131     C*
086600940117     C                   OTHER
086700940127     C                   EXSR      CTRD1
086800940127     C     *IN99         IFEQ      *OFF
086900940131     C* definisco l'ordinamento
087000950410     C                   EXSR      DEFORD
087100950410     C* richiamo la lista
087200130912C2226C                   If        $Pre  = *on
087300130911  "  C                   MOVE      'S2'          $GEST
087400130911  "  C                   MOVE      *ON           $INZS2
087500130911  "  C                   MOVE      *BLANK        W2POS
087600130911C2226C                   Else
087700940127     C                   MOVE      'S1'          $GEST
087800940223     C                   MOVE      *ON           $INZS1
087900940302     C                   MOVE      *BLANK        W1POS
088000130911C2226C                   ENDIF
088100940211     C                   ENDIF
088200940224     C*
088300940117     C                   ENDSL
088400940224     C*
088500940506     C                   ENDIF
088600950525     C*
088700940117     C                   ENDSR
088800940224     C/EJECT
088900940224     C************************************************************
089000940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
089100940224     C************************************************************
089200940127     C     INZD1         BEGSR
089300940318     C*
089400940318     C                   CLEAR                   FMTD1
089500131001BRT  c                   move      xscon         d1on3
089600131001BRT  c                   move      xscoff        d1off3
089700140310BRT  c                   move      xscon         d1on4
089800140310BRT  c                   move      xscoff        d1off4
089900140314     c***********        move      xscon         d1stato
090000140314     c                   move      *blank        d1FasiApe
090100131001BRT  c                   move      xscon         d1EsclImp
090200140310BRT  c                   move      xscoff        d1Insoluti
090300090324     c*
090400130905C2226X**Impostazione indicatore 11 per gestione campo unità nei solleciti
090500130905C2226X****               if        Gesuni057='D'
090600130905C2226C* Impostazione indicatore 11 per gestione campo unità recupero cred.
090700130905C2226C                   if        GesuniR057='D'
090800090324     C                   eval      *in11=*on
090900130910C2226C                   exsr      CarUniAbi
091000090324     C                   else
091100090324     C                   eval      *in11=*off
091200090324     C                   endif
091300090409      * Impostazione indicatore 17 per gestione campo attributo contabile
091400090409     c                   if        posattr057 <> *blanks
091500090409     C                   eval      *in17=*on
091600090409     C                   else
091700090409     C                   eval      *in17=*off
091800090409     C                   endif
091900941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
092000941013     C*    parzializzazioni
092100941104     C                   SELECT
092200941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
092300950606     C     $Unique       WHENEQ    *ON
092400070524    >C****               MOVE      UQ1001        D1KCC
092500941104     C* il pgm è richiamato e
092600941104     C* sono stati passati dei valori di parzializzazione ma non
092700941104     C* la chiave univoca
092800090323    >C     OPZL76        WHENNE    *BLANK
092900950403     C* imposto i valori passati sui campi a video
093000090323    >C                   MOVE      UnitaL76      D1Unita
093100090323    >C                   MOVE      KccL76        D1kcc
093200090323    >C                   MOVE      KScL76        D1Ksc
093300100401     c                   move      AttribL76     D1Attrib
093400100401     c                   move      KscAgeL76     D1kscAge
093500100420     c                   if        DtParDaL76 <> *loval
093600100420     c     *jobrun       move      DtParDaL76    D1DtParDA
093700100420     c                   endif
093800100420     c                   if        DtParAL76  <> *loval
093900100420     c     *jobrun       move      DtParAL76     D1DtParA
094000100420     c                   endif
094100100401     c                   if        DtScadDL76 <> *loval
094200100401     c     *jobrun       move      DtScadDL76    D1DtScaDA
094300100401     c                   endif
094400100401     c                   if        DtScadAL76 <> *loval
094500100401     c     *jobrun       move      DtScadAL76    D1DtScaA
094600100401     c                   endif
094700100416     c                   if        DtSolDaL76 <> *loval
094800100416     c     *jobrun       move      DtSolDaL76    D1DtSolDA
094900100416     c                   endif
095000100416     c                   if        DtSolAL76  <> *loval
095100100416     c     *jobrun       move      DtSolAL76     D1DtSolA
095200100416     c                   endif
095300100401     c                   move      FaseDAL76     D1FaseDa
095400100401     c                   move      FaseAL76      D1FaseA
095500090323     c                   move      DestinL76     d1Destin
095600100401     c                   if        DtAttDaL76 <> *loval
095700100401     c     *jobrun       move      DtAttDaL76    D1dtAttDa
095800090323     c                   endif
095900100401     c                   if        DtAttAl76  <> *loval
096000100401     c     *jobrun       move      DtAttAL76     D1dtAttA
096100090323     c                   endif
096200941104     C* il pgm non è richiamato
096300090323    >C     OPZL76        WHENEQ    *BLANK
096400950403     C* imposto dei valori di default
096500100401     c                   move      '9'           D1FaseA
096600131018     c                   move      udate         D1dtAttA
096700941115     C                   ENDSL
096800090409     c*
096900090409     C* decodifica posizione attributo
097000090409     c                   if        PosAttr057 <> *blanks
097100090409     C                   MOVE      *ALL'0'       XTBKEY
097200090409    >C                   MOVE      PosAttr057    XTBKEY
097300090409     C                   MOVE      '0'           XTBALC
097400090409     C                   MOVE      '1'           XTBRIC
097500090409     C                   MOVE      '0A2'         XTBCOD
097600090409     C                   CALLB     'XATB'
097700090409     C                   PARM                    XATBDS
097800090409     C     XTBERR        IFEQ      '0'
097900090409     C                   MOVEL     XTBUNI        Ang0a2ds
098000090409     C     DES0A2        CAT       Puntini:1     D1posattrD
098100090409     c                   move      ':'           D1PosAttrD
098200090409     c                   endif
098300090409     c                   endif
098400131129C2232C* Intestazione campo libero se presente nel parametro modulo
098500131129  "  C                   If        LiberoI057 <> *blanks
098600131129  "  C                   Movel(P)  LiberoI057    D1LiberoI
098700131129  "  C                   Else
098800131129  "  C                   Eval      D1LiberoI = 'Campo libero................:'
098900131129C2232C                   Endif
099000940224     C*
099100940117     C                   ENDSR
099200940916     C************************************************************
099300940916     C* GESTIONE F02 VIDEO D1
099400940916     C************************************************************
099500940916     C     F02D1         BEGSR
099600940916     C*
099700940916     C* per decodificare la videata richiamo la routine dei ctrl
099800940916     C* e poi disattivo gli eventuali messaggi di errore
099900940916     C                   EXSR      CTRD1
100000940916     C*
100100940916     C                   MOVE      *ALL'0'       IN5098
100200940916     C                   MOVEA     IN5098        *IN(50)
100300940916     C*
100400940916     C                   ENDSR
100500940224     C/EJECT
100600940131     C************************************************************
100700940307     C* GESTIONE F03 VIDEO D1
100800940131     C************************************************************
100900940307     C     F03D1         BEGSR
101000940131     C*
101100940307     C                   MOVE      *ON           $FINE
101200090323    >C                   MOVE      '1'           RETL76
101300940325     C* fine programma
101400940325     C                   EXSR      ENDPGM
101500940307     C*
101600940307     C                   ENDSR
101700940916     C/EJECT
101800940916     C************************************************************
101900940916     C* GESTIONE F05 VIDEO D1
102000940916     C************************************************************
102100940916     C     F05D1         BEGSR
102200940916     C*
102300940916     C                   MOVE      *ON           $INZD1
102400940916     C*
102500940916     C                   ENDSR
102600941206     C************************************************************
102700941206     C* GESTIONE F08 VIDEO D1
102800941206     C************************************************************
102900941206     C     F08D1         BEGSR
103000941206     C*
103100941206     C                   MOVE      *ON           $FINE
103200090323    >C                   MOVE      '0'           RETL76
103300941206     C*
103400941206     C                   ENDSR
103500950301     C/EJECT
103600940307     C************************************************************
103700940307     C* GESTIONE F12 VIDEO D1
103800940307     C************************************************************
103900940307     C     F12D1         BEGSR
104000940307     C*
104100940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
104200940307     C*  per F12 torno alla lista
104300940307     C     $LOP          IFEQ      2
104400940307     C     WINKR         OREQ      *ON
104500130923C2226X**                 MOVE      'S1'          $GEST
104600130923C2226C                   MOVE      $LastG        $GEST
104700940307     C*  altrimenti, per F12 torno a menu
104800940307     C                   ELSE
104900940307     C                   MOVE      *ON           $FINE
105000090323    >C                   MOVE      '2'           RETL76
105100941027     C                   ENDIF
105200940307     C*
105300940307     C                   ENDSR
105400940307     C/EJECT
105500940307     C************************************************************
105600940307     C* GESTIONE F18 VIDEO D1
105700940307     C************************************************************
105800940307     C     F18D1         BEGSR
105900940307     C*
106000940307     C                   MOVE      'D2'          $GEST
106100940307     C                   MOVEL     *ON           $INZD2
106200940307     C*
106300940307     C                   ENDSR
106400940307     C/EJECT
106500940307     C************************************************************
106600940307     C* GESTIONE F24 VIDEO D1
106700940609     C* input
106800940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
106900940509     C* $FX1 =Testi tasti funzione
107000940509     C* $FC1 = Abilitazioni
107100940509     C* $FR1 = Inversioni di fondo
107200940307     C************************************************************
107300940307     C     F24D1         BEGSR
107400940506     C*
107500940506     C                   MOVEA     $FX1          $FX
107600940506     C                   MOVEA     $FC1          $FC
107700940506     C                   MOVEA     $FR1          $FR
107800940506     C                   Z-ADD     $ULKD1        $ULK
107900950726     C                   Z-ADD     $D1L01        $L01
108000950726     C                   Z-ADD     $D1L02        $L02
108100940506     C                   EXSR      RIEKEY
108200940506     C                   Z-ADD     $ULK          $ULKD1            3 0
108300940720     C                   MOVEA     $FC           $FC1
108400070618     C                   MOVEL     $KEY1         Z1KE
108500060224     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
108600940506     C                   MOVEL     $KEY2         Z1KE2
108700070618     c* riga unica tasti
108800070618     c                   eval      $A = %triml(z1ke)
108900070618     c                   eval      $B = %triml(z1ke2)
109000070618     c     $A            cat(P)    $B:0          z1ke1
109100940506     C                   WRITE     FMTZ1
109200940307     C*
109300940307     C                   ENDSR
109400940307     C/EJECT
109500940307     C************************************************************
109600940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
109700950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
109800960109     C* 93 94 96 97 98
109900940307     C************************************************************
110000940307     C     CTRD1         BEGSR
110100940307     C*
110200940127     C                   SETOFF                                       99
110300070525     C* non imposto pos. del cursore
110400070525     C                   Z-ADD     0             H1RIGA
110500070525     C                   Z-ADD     0             H1COLO
110600940131     C*
110700090324     C* Unità
110800090324     C                   exsr      CtrUnita
110900090324     C* Conto
111000090324     C                   exsr      CtrConto
111100100402     C* Attributo contabile
111200100402     C                   exsr      CtrAttr
111300100402     C* Agente
111400100402     C                   exsr      CtrkscAge
111500100420     C* Date partita
111600100420     C                   exsr      CtrDatePar
111700100402     C* Date scadenza
111800100402     C                   exsr      CtrDateSca
111900100416     C* Date sollecito
112000100416     C                   exsr      CtrDateSol
112100100402     C* Fasi
112200100402     C                   exsr      CtrFasi
112300100407     C* Azione
112400100407     C                   exsr      CtrAzione
112500090324     C* Destinatario
112600090324     C                   exsr      CtrDestin
112700100402     C* Date attività
112800100402     C                   exsr      CtrDateAtt
112900140319     C* Data chiusura attività
113000140319     C                   exsr      CtrDateChiu
113100140317     C* fasi aperte/CHIUSE/TUTTE
113200140317     C                   exsr      CtrSELFASI
113300100413     C* partite aperte
113400140314     c*                  if        D1Stato <> xscon and
113500140314     c*                            D1Stato <> xscoff
113600140314     c*                  seton                                        5799
113700140314     c*                  endif
113800131001BRT  C* esclusione impegni (98)
113900131001 "   c                   if        D1EsclImp <> xscon and
114000131001 "   c                             D1EsclImp <> xscoff
114100131001 "   c                   seton                                        5899
114200131001BRT  c                   endif
114300070531     c*
114400140310BRT  C* Solo Insoluti
114500140310 "   c                   if        D1Insoluti <> xscon and
114600140310 "   c                             D1Insoluti <> xscoff
114700140310 "   c                   seton                                        6699
114800140310BRT  c                   endif
114900140310     c*
115000940117     C                   ENDSR
115100140317     C*----------------------------------------------------------
115200140317     C* Selezione fasi
115300140317     C*----------------------------------------------------------
115400140317     C     CtrSelFasi    BEGSR
115500140317     C*
115600140317     C                   RESET                   X07DS
115700140317     C                   MOVE      '1'           X07RIC
115800140317     C                   MOVEL     'ANDIZ000'    X07TRC
115900140317     C                   MOVEL     'DSPSELFASI'  X07CAM
116000140317     C                   MOVEL     D1FASIAPE     X07VAL
116100140317     C                   MOVE      *ON           X07ALL
116200140317     C                   CALLB     'X07VALR'
116300140317     C                   PARM                    X07DS
116400140317   B3C     X07ERR        IFEQ      '0'
116500140317     C                   MOVEL     X07DES        D1fasiaped
116600140317     c                   else
116700140317     C                   SETON                                        6199
116800140317     C                   ENDIF
116900140317     c*
117000140317     C                   ENDSR
117100090324     C*----------------------------------------------------------
117200090324     C* Controllo unita
117300090324     C*----------------------------------------------------------
117400090324     C     CtrUnita      BEGSR
117500090324     C*
117600090324     C                   CLEAR                   D1UnitaD
117700090324     C     D1Unita       IFNE      *BLANKS
117800090324     C     *in11         ANDEQ     *on
117900090324     C                   MOVE      xscsoc        UNISOCieta
118000090324     C                   MOVE      D1Unita       UNIUNIta
118100090324     C     K02Uni01      CHAIN     ANUNI01L                           21
118200090324     C     *IN21         IFEQ      *ON
118300090324     C                   SETON                                        6299
118400090324     C                   ELSE
118500090324     C                   MOVEL     UNIDESbrev    D1UnitaD
118600090324     C                   ENDIF
118700130909C2226C* Controllo autorizzazione all'unità
118800130909  "  C     *in62         Ifeq      *off
118900130909  "  C                   RESET                   XAutUniDS
119000130909  "  C                   EVAL      XAUPrf = KNmUs
119100130909  "  C                   EVAL      XAUSoc = XscSoc
119200130909  "  C                   EVAL      XAUUni = d1unita
119300130909  "  C                   CALL      'XAUTUNI'
119400130909  "  C                   PARM                    XAutUniDS
119500130909  "  C     XAUAbl        IFEQ      *OFF
119600130909  "  C                   SETON                                        7599
119700130909  "  C                   ENDIF
119800130909C2226C                   endif
119900090324     C                   ENDIF
120000090324     c*
120100090324     C                   ENDSR
120200090324     C*----------------------------------------------------------
120300090324     C* Controllo conto
120400090324     C*----------------------------------------------------------
120500090324     C     CtrConto      BEGSR
120600090324     C*
120700090324     C* Controllo del conto (niente obbligatorio)
120800090324     C                   CLEAR                   D1Descr
120900140317BRT  C                   CLEAR                   D1Kcc
121000090324 B1  c                   if        D1Kcc <> *blanks or
121100090324     c                             D1Ksc <> *blanks
121200090324     c*
121300090324     c                   if        d1kcc=*blanks
121400090324     c                   eval      d1kcc='C'
121500090324     c                   endif
121600090324     C     D1kcc         IFNE      'C'
121700090324     C     D1kcc         ANDNE     'F'
121800090324     C                   CALL      'XALLINEA'
121900090324     C                   PARM                    D1kcc
122000090324     C                   PARM      6             LEN               5 0
122100090324     C                   PARM                    XSCINP
122200090324     C                   ENDIF
122300090324     C*
122400090324     C                   CALL      'XALLINEA'
122500090324     C                   PARM                    D1KSC
122600090324     C                   PARM      8             LEN               5 0
122700090324     C                   PARM                    XSCINP
122800090324     C*
122900090324     C                   MOVE      D1KCC         $kcc
123000090324     C                   MOVE      D1KSC         $ksc
123100090324     C*
123200090324     C* reperisco capoconto se cliente/fornitore
123300090324     C     D1KCC         IFEQ      'C       '
123400090324     C     D1KCC         OREQ      'F       '
123500090324     C                   MOVE      XSCSOC        XCAPSOCIET        3
123600090324     C                   MOVE      D1KCC         XCAPKCC           6
123700090324     C                   MOVE      D1KSC         XCAPKSC           8
123800090324     C                   CALLB     'XCAPCLIFOR'
123900090324     C                   PARM                    XCAPSOCIET
124000090324     C                   PARM                    XCAPKCC
124100090324     C                   PARM                    XCAPKSC
124200090324     C                   MOVE      XCAPKCC       D1KCC
124300090324     C                   MOVE      XCAPKCC       $kcc
124400090324     C                   MOVE      XCAPKsC       $ksc
124500090324     C                   ENDIF
124600090324     C*
124700090324     C* Richiamo modulo controllo conto
124800090324     C                   EXSR      CALLMVC002
124900090324     C* Errore
125000090324     C     $Esito        IFEQ      *ON
125100090324     c                   if        d1ksc = *blanks
125200090324     C                   SETON                                        5099
125300090324     c                   else
125400090324     C                   SETON                                        505199
125500090324     c                   endif
125600090324     c*
125700090324     C                   ELSE
125800090324     C                   MOVEL     Descr002      D1Descr
125900100402     c* solo clienti
126000100402     c                   if        Snatura002 <> 'C'
126100100402     c                   if        d1ksc = *blanks
126200100402     C                   SETON                                        5399
126300100402     c                   else
126400100402     C                   SETON                                        535499
126500100402     c                   endif
126600100402     c                   endif
126700090324     C                   ENDIF
126800090324     C*
126900090324 e1  C                   ENDIF
127000090324     c*
127100090324     C                   ENDSR
127200070525     C*----------------------------------------------------------
127300070525     C* Chiamata NDMVC002
127400070525     C*----------------------------------------------------------
127500070525     C     CallMvc002    BEGSR
127600070525     C*
127700070525     C* Controllo conto
127800070525     C                   Clear                   ND002DS
127900070525     C                   EVAL      $LenOut = %Size(ND002DS)
128000070525     C                   CallB     'NDMVC002'
128100070525     C                   PARM                    xscsoc
128200070525     C                   PARM                    $kcc
128300070525     C                   PARM                    $ksc
128400100521     C                   PARM                    Wdate
128500070525     C                   PARM      *OFF          $GesMsg
128600070525     C                   PARM      *OFF          $Esito
128700070525     C                   PARM      'ND002DS'     $Struttura
128800070525     C                   PARM                    ND002DS
128900070525     C                   PARM                    $LenOut
129000070525     C*
129100070525     C                   ENDSR
129200100402     C*----------------------------------------------------------
129300100402     C* Controllo attributo contabile
129400100402     C*----------------------------------------------------------
129500100402     C     CtrAttr       BEGSR
129600100402     C*
129700100402     c* controllo nel control il codice attributo
129800100402     C                   clear                   d1attribd
129900100402     c                   if        D1attrib <> *blanks
130000100402     C                   MOVE      *ALL'0'       XTBKEY
130100100402     C                   MOVEL     PosAttr057    WRK6              6
130200100402     C                   MOVE      d1Attrib      WRK6
130300100402    >C                   MOVE      wrk6          XTBKEY
130400100402     C                   MOVE      '0'           XTBALC
130500100402     C                   MOVE      '1'           XTBRIC
130600100402     C                   MOVE      '0A1'         XTBCOD
130700100402     C                   CALLB     'XATB'
130800100402     C                   PARM                    XATBDS
130900100402     C     XTBERR        IFEQ      '0'
131000100402     C                   MOVEL     XTBUNI        Ang0a1ds
131100100402     C                   MOVEL     DES0a1        D1AttribD
131200100402     c                   else
131300100402     c                   seton                                        6899
131400100402     c                   endif
131500100402     c                   endif
131600100402     c*
131700100402     c                   ENDSR
131800100402     C/EJECT
131900100402     C*----------------------------------------------------------
132000100402     C* Controllo Agente
132100100402     C*----------------------------------------------------------
132200100402     C     CtrKscAge     BEGSR
132300100402     C*
132400100402     c                   clear                   d1kscageD
132500100402   1 C     D1kscage      IFNE      *blanks
132600100402     C                   CALLb     'XALLINEA'
132700100402     C                   PARM                    D1kscage
132800100402     C                   PARM      8             LEN               5 0
132900100402     C                   PARM                    XSCINP
133000100402     C* reperisco capoconto agente
133100100402     C                   EVAL      Xcapkcc  = 'F     '
133200100402     C                   MOVE      XSCSOC        XCAPSOCIET
133300100402     C                   MOVE      d1kscage      XCAPKSC
133400100402     C                   CALLB     'XCAPCLIFOR'
133500100402     C                   PARM                    XCAPSOCIET        3
133600100402     C                   PARM                    XCAPKCC           6
133700100402     C                   PARM                    XCAPKSC           8
133800100402     C* Richiamo modulo controllo conto
133900100402     c                   move      Xcapkcc       $kcc
134000100402     c                   move      Xcapksc       $ksc
134100100402     C                   EXSR      CALLMVC002
134200100402   2 C     $Esito        IFEQ      *ON
134300100402     C                   SETON                                        5599
134400100402   2xC                   ELSE
134500100402     C                   MOVEL     DESBREV002    D1kscageD
134600100402   2-C                   ENDIF
134700100402   1-C                   ENDIF
134800100402     C*
134900100402     C                   ENDSR
135000100402     c*
135100100416     c*EJECT
135200100420     C*----------------------------------------------------------
135300100420     C* Controllo DATE partita
135400100420     C*----------------------------------------------------------
135500100420     C     CtrDatePar    BEGSR
135600100420     C*
135700100420      * Data partita da a
135800100420     c                   Eval      WdtParDa= *Loval
135900100420 b1  C                   if        D1DtParDa<>   *Zeros
136000100420     C     *JobRun       Test(d)                 D1DtParDa              21
136100100420    >C     *In21         ifeq      *On
136200100420     c* non corretta
136300100420    >C                   SetOn                                        7299
136400100420     c                   else
136500100420     c* è corretta,
136600100420     c     *JobRun       move      D1DtParDa     WdtParDA
136700100420    >C                   endif
136800100420E1  >C                   endif
136900100420     C*
137000100420      * Data scadenza a
137100100420     c                   Eval      WdtParA = *hival
137200100420 b1  C                   if        D1DtParA  <>   *Zeros
137300100420     C     *JobRun       Test(d)                 D1DtParA               21
137400100420    >C     *In21         ifeq      *On
137500100420     c* non corretta
137600100420    >C                   SetOn                                        7399
137700100420     c                   else
137800100420     c* è corretta,
137900100420     c     *JobRun       move      D1DtParA      WdtParA
138000100420    >C                   endif
138100100420E1  >C                   endif
138200100420     c*
138300100420     c* limiti errati
138400100420     c                   if        WdtParDa > WdtParA
138500100420    >C                   SetOn                                        7499
138600100420     c                   endif
138700100420     c
138800100420     C                   ENDSR
138900100420     c*EJECT
139000100402     C*----------------------------------------------------------
139100100402     C* Controllo DATE scadenza
139200100402     C*----------------------------------------------------------
139300100402     C     CtrDateSca    BEGSR
139400100402     C*
139500100402      * Data scadenza  da
139600100402     c                   Eval      WdtScaDa= *Loval
139700100402 b1  C                   if        D1DtScaDa<>   *Zeros
139800100402     C     *JobRun       Test(d)                 D1DtScaDa              21
139900100402    >C     *In21         ifeq      *On
140000100402     c* non corretta
140100100402    >C                   SetOn                                        6999
140200100402     c                   else
140300100402     c* è corretta,
140400100402     c     *JobRun       move      D1DtScaDa     WdtScaDA
140500100402    >C                   endif
140600100402E1  >C                   endif
140700100402     C*
140800100402      * Data scadenza a
140900100402     c                   Eval      WdtScaA = *hival
141000100402 b1  C                   if        D1DtScaA  <>   *Zeros
141100100402     C     *JobRun       Test(d)                 D1DtScaA               21
141200100402    >C     *In21         ifeq      *On
141300100402     c* non corretta
141400100402    >C                   SetOn                                        7099
141500100402     c                   else
141600100402     c* è corretta,
141700100402     c     *JobRun       move      D1DtScaA      WdtScaA
141800100402    >C                   endif
141900100402E1  >C                   endif
142000100402     c*
142100100402     c* limiti errati
142200100402     c                   if        WdtScaDa > WdtScaA
142300100402    >C                   SetOn                                        7199
142400100402     c                   endif
142500100402     c
142600100402     C                   ENDSR
142700100416     c*EJECT
142800100416     C*----------------------------------------------------------
142900100416     C* Controllo DATE sollecito
143000100416     C*----------------------------------------------------------
143100100416     C     CtrDateSol    BEGSR
143200100416     C*
143300100416      * Data sollecito da
143400100416     c                   Eval      WdtSolDa= *Loval
143500100416 b1  C                   if        D1DtSolDa<>   *Zeros
143600100416     C     *JobRun       Test(d)                 D1DtSolDa              21
143700100416    >C     *In21         ifeq      *On
143800100416     c* non corretta
143900100416    >C                   SetOn                                        5899
144000100416     c                   else
144100100416     c* è corretta,
144200100416     c     *JobRun       move      D1DtSolDa     WdtSolDA
144300100416    >C                   endif
144400100416E1  >C                   endif
144500100416     C*
144600100416      * Data sollecito a
144700100416     c                   Eval      WdtSolA = *hival
144800100416 b1  C                   if        D1DtSolA  <>   *Zeros
144900100416     C     *JobRun       Test(d)                 D1DtSolA               21
145000100416    >C     *In21         ifeq      *On
145100100416     c* non corretta
145200100416    >C                   SetOn                                        5999
145300100416     c                   else
145400100416     c* è corretta,
145500100416     c     *JobRun       move      D1DtSolA      WdtSolA
145600100416    >C                   endif
145700100416E1  >C                   endif
145800100416     c*
145900100416     c* limiti errati
146000100416     c                   if        WdtSolDa > WdtSolA
146100100416    >C                   SetOn                                        6099
146200100416     c                   endif
146300100416     c
146400100416     C                   ENDSR
146500100402     C*----------------------------------------------------------
146600100402     C* Controllo fasi
146700100402     C*----------------------------------------------------------
146800100402     C     CtrFasi       BEGSR
146900100402     C*
147000100402     c                   if        D1FaseDa > d1FaseA
147100100402     c                   seton                                        6799
147200100402     c                   endif
147300100402     c*
147400100402     C                   ENDSR
147500100407     C*----------------------------------------------------------
147600100407     C* Controllo azione
147700100407     C*----------------------------------------------------------
147800100407     C     CtrAzione     BEGSR
147900100407     C*
148000100407     c                   clear                   D1AzioneD
148100100407     c                   if        d1Azione <> *blanks
148200100407     C                   MOVE      *ALL'0'       XTBKEY
148300100407    >C                   MOVE      D1Azione      XTBKEY
148400100407     C                   MOVE      '0'           XTBALC
148500100407     C                   MOVE      '1'           XTBRIC
148600100407     C                   MOVE      '047'         XTBCOD
148700100407     C                   CALLB     'XATB'
148800100407     C                   PARM                    XATBDS
148900100407     C     XTBERR        IFEQ      '0'
149000100407     C                   MOVEL     XTBUNI        Ang047ds
149100100407     c                   movel     Des047        D1AzioneD
149200100407     c                   else
149300100407     c                   seton                                        5699
149400100407     c                   endif
149500100407     c                   endif
149600100407     c*
149700100407     C                   ENDSR
149800090324     C*----------------------------------------------------------
149900090324     C* Controllo destinatario
150000090324     C*----------------------------------------------------------
150100090324     C     CtrDestin     BEGSR
150200090324     C*
150300090324      * Controllo codice destinatario
150400090324     c                   if        d1destin  <> *blanks
150500090324     C                   CLEAR                   AUTDS
150600090324     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
150700090324     C                   Z-ADD     0             XCAGRP                         GRUPPO
150800090324     C                   MOVEL     d1Destin      XCAPRF                         PROFILO
150900090324    >C                   MOVE      'CU'          XCATCK                         TIPO CTRL
151000090324    >C                   MOVE      ' '           XCANAU                         RICH.XNOAUT
151100090324     C                   EVAL      Lungutn= %Size(AnUtn00f)
151200090324     C                   CALLB     'XCHKAUT'
151300090324     C                   PARM                    AUTDS
151400090324     C                   PARM                    PARMOK
151500090324     C                   PARM                    ANUTNParm
151600090324     C                   PARM      71            LungUTN
151700090324b1   C     XCARTN        IFne      0
151800090324     c                   seton                                        6599
151900090324     C                   ENDIF
152000090324     c                   endif
152100090324     C*
152200090324     C                   ENDSR
152300090324     C*----------------------------------------------------------
152400100402     C* Controllo data attività
152500090324     C*----------------------------------------------------------
152600100402     C     CtrDateAtt    BEGSR
152700090324     C*
152800100402      * Data attività da
152900100401     c                   Eval      WdtAttda= *Loval
153000100401 b1  C                   if        D1dtAttDa<>   *Zeros
153100100401     C     *JobRun       Test(d)                 D1dtAttDa              21
153200090324    >C     *In21         ifeq      *On
153300090324     c* non corretta
153400090324    >C                   SetOn                                        5299
153500090324     c                   else
153600090324     c* è corretta,
153700100401     c     *JobRun       move      D1dtAttDA     WdtAttDa
153800090324    >C                   endif
153900090324E1  >C                   endif
154000090324     C*
154100100402      * Data attività a
154200100401     c                   Eval      WdtAttA = *hival
154300100401 b1  C                   if        D1dtAttA <>   *Zeros
154400100401     C     *JobRun       Test(d)                 D1dtAttA               21
154500090324    >C     *In21         ifeq      *On
154600090324     c* non corretta
154700090324    >C                   SetOn                                        6399
154800090324     c                   else
154900090324     c* è corretta,
155000100401     c     *JobRun       move      D1dtAttA      WdtAttA
155100090324    >C                   endif
155200090324E1  >C                   endif
155300090324     c*
155400090324     c* limiti errati
155500100401     c                   if        WdtAttDa > WdtAttA
155600090324    >C                   SetOn                                        6499
155700090324     c                   endif
155800090324     c*
155900090324     C                   ENDSR
156000140319     C*----------------------------------------------------------
156100140319     C* Controllo data chiusura
156200140319     C*----------------------------------------------------------
156300140319     C     CtrDateChiu   BEGSR
156400140319     C*
156500140319      * Data chiusura da
156600140319     c                   Eval      WdtChida= *Loval
156700140319 b1  C                   if        D1dtChiDa<>   *Zeros
156800140319     C     *JobRun       Test(d)                 D1dtChiDa              21
156900140319    >C     *In21         ifeq      *On
157000140319     c* non corretta
157100140319    >C                   SetOn                                        7699
157200140319     c                   else
157300140319     c* è corretta,
157400140319     c     *JobRun       move      D1dtChiDA     WdtChiDa
157500140319    >C                   endif
157600140319E1  >C                   endif
157700140319     C*
157800140319      * Data attività a
157900140319     c                   Eval      WdtChiA = *hival
158000140319 b1  C                   if        D1dtChiA <>   *Zeros
158100140319     C     *JobRun       Test(d)                 D1dtChiA               21
158200140319    >C     *In21         ifeq      *On
158300140319     c* non corretta
158400140319    >C                   SetOn                                        7799
158500140319     c                   else
158600140319     c* è corretta,
158700140319     c     *JobRun       move      D1dtChiA      WdtChiA
158800140319    >C                   endif
158900140319E1  >C                   endif
159000140319     c*
159100140319     c* limiti errati
159200140319     c                   if        WdtChiDa > WdtChiA
159300140319    >C                   SetOn                                        7899
159400140319     c                   endif
159500140319     c*
159600140319     C                   ENDSR
159700090409     C*
159800950410     C************************************************************
159900950410     C* DEFINIZIONE DELL'ORDINAMENTO
160000950410     C************************************************************
160100950410     C     DEFORD        BEGSR
160200950410     C*
160300950410     C* definisco l'ordinamento della lista a seconda dei valori
160400950410     C* impostati nel video di parzializzazione
160500950410     C*
160600950410     C                   Z-ADD     1             $ORD
160700070524     C*
160800950410     C                   ENDSR
160900960202     C/EJECT
161000960202     C************************************************************
161100960202     C* Chiamata programma con tasto personalizzato
161200960202     C************************************************************
161300960202     C     FPerS1        BEGSR
161400960202     C*
161500960202     C* salvo la DS di ingresso
161600090323    >C                   MOVEL     NDCL76DS      $SaveDS
161700960202     C*
161800090323    >C                   MOVEL     NDCL76DS      KPJBU
161900960202     C                   CALL      $PGF2(PosFun)                        22
162000960202     C                   PARM                    KPJBA
162100960202     C                   PARM                    NOMPGM
162200960202     C*
162300960202     C* ripristino la DS di ingresso
162400090323    >C                   MOVEL     $SaveDS       NDCL76DS
162500960202     C*
162600960202     C                   ENDSR
162700130911C2226C/EJECT
162800130911  "  C************************************************************
162900130911  "  C* Chiamata programma con tasto personalizzato
163000130911  "  C************************************************************
163100130911C2226C     FPerS2        BEGSR
163200130911     C*
163300130911     C* salvo la DS di ingresso
163400130911    >C                   MOVEL     NDCL76DS      $SaveDS
163500130911     C*
163600130911    >C                   MOVEL     NDCL76DS      KPJBU
163700130911     C                   CALL      $PGF2(PosFun)                        22
163800130911     C                   PARM                    KPJBA
163900130911     C                   PARM                    NOMPGM
164000130911     C*
164100130911     C* ripristino la DS di ingresso
164200130911    >C                   MOVEL     $SaveDS       NDCL76DS
164300130911     C*
164400130911C2226C                   ENDSR
164500940224     C/EJECT
164600940127     C************************************************************
164700940131     C* GESTIONE LISTA
164800940127     C************************************************************
164900940127     C     GESS1         BEGSR
165000940223     C* inizializzazione videata
165100940223     C     $INZS1        IFEQ      *ON
165200940127     C                   EXSR      INZS1
165300940223     C                   MOVE      *OFF          $INZS1
165400940127     C                   ENDIF
165500940223     C* emissione piede videata se proveniente da altra
165600940223     C* salvataggio valore formato di provenienza
165700940202     C     $LASTV        IFNE      'S1'
165800940202     C                   WRITE     FMTT1
165900940224     C                   EXSR      GESPIE
166000940202     C                   MOVE      $LASTV        $LASTG
166100940202     C                   MOVE      'S1'          $LASTV
166200941027     C                   ENDIF
166300940223     C*
166400940223     C     WMAX          IFEQ      0
166500940211     C                   WRITE     FMTD3                                        NO RCD
166600940211     C                   ENDIF
166700960202     C* Inizializzazione indice tasti funzione personalizzati
166800960202     C                   Eval      PosFun = 25
166900940127     C*
167000940131     C                   EXFMT     FMTC1
167100960605     C*
167200960605     C* azzero la posizione del cursore
167300960605     C                   Z-ADD     0             H1RIGA
167400960605     C                   Z-ADD     0             H1COLO
167500970912     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
167600970912     C* di refresh dello sfondo
167700970912     C                   MOVE      '1'           WExf
167800960605     C*
167900940204     C     C1NRR         IFNE      0
168000940204     C                   Z-ADD     C1NRR         WSFL
168100940204     C                   ENDIF
168200951020     C                   IF        SFLNRR > 0
168300951020     C                   Z-ADD     SFLNRR        C1RCD
168400951020     C                   ENDIF
168500960122     C* inizializzazione indice opzioni personalizzate
168600960122     C                   Eval      PosOpz = 0
168700951005     C* CONTROLLO ABILITAZIONE TASTI
168800951005     C* INPUT
168900951005     C* $FC2 = Abilitazioni videata S1
169000951005     C* OUTPUT
169100951005     C* *IN97 + *IN99 Errori
169200951005     C                   MOVEA     $FC2          $FC
169300951005     C                   EXSR      CTRKEY
169400941205     C*
169500940506     C     *IN99         IFEQ      *OFF
169600940127     C*
169700941027     C                   SELECT
169800940127     C* F3=Fine
169900940506     C     $TASTO        WHENEQ    F03
170000940309     C                   EXSR      F03S1
170100130924C2226C* F5=Refresh
170200130924  "  C     $TASTO        WHENEQ    F05
170300130924C2226C                   EXSR      F05S1
170400941206     C* F8=Successivo
170500941206     C     $TASTO        WHENEQ    F08
170600941206     C                   EXSR      F08S1
170700940131     C* F12=Ritorno
170800940506     C     $TASTO        WHENEQ    F12
170900940309     C                   EXSR      F12S1
171000090325     C* F13=Selezione multipla
171100090325     C     $TASTO        WHENEQ    F13
171200130924C2226C                   Clear                   $tastoSav2
171300090325     C                   Eval      $TastoSav=$Tasto
171400130924C2226C                   Eval      PrimaVolta = 0
171500090325     C                   EXSR      F13S1
171600940131     C* F17=Parzializzazioni
171700940506     C     $TASTO        WHENEQ    F17
171800940309     C                   EXSR      F17S1
171900940131     C* F18=Modifica valori assunti
172000940506     C     $TASTO        WHENEQ    F18
172100940309     C                   EXSR      F18S1
172200940224     C* F23=Altre opzioni
172300940506     C     $TASTO        WHENEQ    F23
172400940309     C                   EXSR      F23S1
172500940224     C* F24=Altri tasti
172600940506     C     $TASTO        WHENEQ    F24
172700940309     C                   EXSR      F24S1
172800940127     C* ROLL-UP
172900090330     C     $TASTO        WHENEQ    ROLLUP
173000090330     C                   EXSR      ROLS1
173100960202     C* Personalizzata
173200960202     C     $PGF2(PosFun) Whenne    *BLANK
173300960202     C                   EXSR      FperS1
173400940127     C*
173500941027     C                   OTHER
173600940127     C* CONTROLLO DATI
173700940131     C                   EXSR      CTRC1
173800940201     C     *IN99         IFEQ      *OFF
173900940131     C                   EXSR      CTRS1
174000941027     C                   ENDIF
174100941205     C*
174200941027     C                   ENDSL
174300940127     C*
174400940506     C                   ENDIF
174500941205     C*
174600940127     C                   ENDSR
174700940224     C/EJECT
174800940127     C************************************************************
174900940131     C* INIZIALIZZAZIONE LISTA
175000940127     C************************************************************
175100940127     C     INZS1         BEGSR
175200090325     C*appoggio x F13
175300090325     C                   Clear                   $TastoSav
175400090325     C                   eval      PrimaVolta=0
175500130924     C                   Clear                   $TastoSav2
175600940302     C* pulizia SFL
175700940128     C                   SETOFF                                         3031
175800940131     C                   WRITE     FMTC1
175900940128     C                   SETON                                          31
176000940127     C*
176100940128     C* riempimento opzioni
176200940223     C                   MOVEA     $TX1          $TX
176300940223     C                   MOVEA     $FL1          $FL
176400940309     C                   Z-ADD     0             WCOZ
176500960117     C                   Z-ADD     0             WMXO
176600940225     C*  equivale al num.di elementi della schiera $TX1
176700960117     C                   Z-add     OpzLen        WMXO
176800950906     C                   Z-ADD     $C1Op1        $L01
176900950906     C                   Z-ADD     $C1Op2        $L02
177000940128     C                   EXSR      RIEOPZ
177100941214     C                   Z-ADD     23            X
177200941214     C                   MOVE      FF23          $FC2(X)
177300130911     C                   MOVEL     WOPZ1         C1OPZ
177400060224     C                   MOVEL     $ALLOPT       H1ALLOPT
177500940608     C                   MOVEL     WOPZ2         C1OPZ2
177600940225     C                   Z-ADD     WCOZ          C1COZ
177700070531     c* riga unica opzioni
177800070531     c                   eval      $A = %triml(c1opz)
177900070531     c                   eval      $B = %triml(c1opz2)
178000070531     c     $A            cat(P)    $B:2          C1opz1
178100100402     c*
178200100402     c                   eval      C1Unita   =D1Unita
178300100402     c                   eval      C1kcc     =D1kcc
178400100402     c                   eval      C1PosAttrD=D1PosAttrD
178500100402     c                   eval      C1attrib=D1Attrib
178600100402     c                   eval      C1attribD=D1AttribD
178700130911C2226c                   eval      C1Importo=0
178800130912  "  C                   If        $pre = *on
178900130911  "  c                   eval      C1Importo=S2Importo
179000130911C2226C                   Endif
179100940128     C*
179200940128     C* CARICAMENTO SFL A PAGINE
179300940128     C*
179400940201     C                   Z-ADD     0             S1NRR
179500940128     C                   Z-ADD     0             WMAX
179600940224     C*
179700940224     C* Posizionamento su file pilota
179800940302     C                   CLEAR                   C1POS
179900941214     C                   EXSR      SETANA
180000940302     C* ripristino i valori del posizionamento
180100940302     C                   MOVE      W1POS         C1POS
180200940131     C*
180300940201     C* Lettura file pilota
180400941214     C                   EXSR      REDANA
180500940131     C*
180600970318     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
180700970318     C                   IF        $LastV = 'D1'
180800970318     C                   Z-ADD     SFLPAG        WPAG
180900970318     C                   ENDIF
181000940127     C                   EXSR      ROLS1
181100970318     C* imposto il caricamento sfl del numero massimo di righe per pagina
181200970318     C                   Z-ADD     SFLPAG        WPAG
181300940127     C*
181400940127     C                   ENDSR
181500940127     C/EJECT
181600940127     C************************************************************
181700940131     C* CARICAMENTO PAGINA LISTA
181800940127     C************************************************************
181900940127     C     ROLS1         BEGSR
182000940127     C*
182100940128     C                   SETOFF                                       32
182200940223     C                   Z-ADD     0             Y
182300940127     C                   Z-ADD     WMAX          S1NRR
182400940127     C*
182500940127     C* Leggo dal file anagrafico per caricare la lista
182600940127     C*
182700950606     C     $EndFile      DOWEQ     *OFF
182800940201     C     Y             ANDLT     WPAG
182900090325     c     S1nrr         andlt     9999
183000940127     C*
183100940131     C                   EXSR      RIES1
183200940127     C*
183300940127     C                   ADD       1             S1NRR
183400940127     C                   ADD       1             Y
183500940127     C*
183600940131     C                   WRITE     FMTS1
183700940131     C*
183800941214     C                   EXSR      REDANA
183900940128     C*
184000941027     C                   ENDDO
184100940127     C*
184200940223     C                   Z-ADD     S1NRR         WMAX                 30
184300940127     C*
184400940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
184500940127     C*
184600940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
184700940127     C                   MVR                     RESTO             3 0
184800940131     C     PAGINE        MULT      SFLPAG        C1RCD
184900941027     C     RESTO         IFGT      0
185000940127     C                   ADD       1             C1RCD
185100941027     C                   ELSE
185200940407     C                   SUB       SFLPAG        C1RCD
185300940407     C                   ADD       1             C1RCD
185400941027     C                   ENDIF
185500940128     C*
185600940127     C                   ENDSR
185700940224     C/EJECT
185800940224     C************************************************************
185900940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
186000940224     C************************************************************
186100941214     C     SETANA        BEGSR
186200940224     C*
186300070524     c                   exsr      InzSql
186400940224     C*
186500940224     C                   ENDSR
186600940128     C/EJECT
186700940128     C************************************************************
186800940131     C* LETTURA RCD ARCHIVIO PILOTA
186900940128     C************************************************************
187000941214     C     REDANA        BEGSR
187100940128     C*
187200950606     C                   MOVEL     *OFF          $EndFile
187300940131     C                   MOVEL     *OFF          $RCDOK
187400970910     C*
187500950606     C     $EndFile      DOUEQ     *ON
187600940131     C     $RCDOK        OREQ      *ON
187700070525     C*
187800070525     c                   exsr      FetchSql
187900070525     c*
188000070525     C                   if        SqlCod<0 or SqlCod=100
188100070525     C                   MOVE      *ON           *IN21
188200070525     C                   ELSE
188300070525     C                   MOVE      *OFF          *IN21
188400070525     C                   END
188500940131     C*
188600950606     C                   MOVE      *IN21         $EndFile
188700940131     C*
188800950606     C     $EndFile      IFEQ      *OFF
188900100407     C                   EXSR      SELANA
189000941027     C                   ENDIF
189100940131     C*
189200941027     C                   ENDDO
189300940131     C*
189400000208     C                   MOVE      $EndFile      *IN33                          SflEnd
189500940131     C*
189600940131     C                   ENDSR
189700940224     C/EJECT
189800130911C2226C************************************************************
189900130911  "  C* GESTIONE PRELISTA
190000130911  "  C************************************************************
190100130911C2226C     GESS2         BEGSR
190200130911     C* inizializzazione videata
190300130911     C     $INZS2        IFEQ      *ON
190400130911     C                   EXSR      INZS2
190500130911     C                   MOVE      *OFF          $INZS2
190600130911     C                   ENDIF
190700130911     C* emissione piede videata se proveniente da altra
190800130911     C* salvataggio valore formato di provenienza
190900130911     C     $LASTV        IFNE      'S2'
191000130911     C                   WRITE     FMTT1
191100130911     C                   EXSR      GESPIE
191200130911     C                   MOVE      $LASTV        $LASTG
191300130911     C                   MOVE      'S2'          $LASTV
191400130911     C                   ENDIF
191500130911     C*
191600130911     C     WMAXS2        IFEQ      0
191700130911     C                   WRITE     FMTD3                                        NO RCD
191800130911     C                   ENDIF
191900130911     C* Inizializzazione indice tasti funzione personalizzati
192000130911     C                   Eval      PosFun = 25
192100130911     C*
192200130911     C                   EXFMT     FMTC2
192300130911     C*
192400130911     C* azzero la posizione del cursore
192500130911     C                   Z-ADD     0             H1RIGA
192600130911     C                   Z-ADD     0             H1COLO
192700130911     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
192800130911     C* di refresh dello sfondo
192900130911     C                   MOVE      '1'           WExf
193000130911     C*
193100130911     C     C2NRR         IFNE      0
193200130911     C                   Z-ADD     C2NRR         WSFL2
193300130911     C                   ENDIF
193400130911     C                   IF        SFLNRR > 0
193500130911     C                   Z-ADD     SFLNRR        C2RCD
193600130911     C                   ENDIF
193700130911     C* inizializzazione indice opzioni personalizzate
193800130911     C                   Eval      PosOpz = 0
193900130911     C* CONTROLLO ABILITAZIONE TASTI
194000130911     C* INPUT
194100130911     C* $FC2 = Abilitazioni videata S1
194200130911     C* OUTPUT
194300130911     C* *IN97 + *IN99 Errori
194400130911     C                   MOVEA     $FC2          $FC
194500130911     C                   EXSR      CTRKEY
194600130911     C*
194700130911     C     *IN99         IFEQ      *OFF
194800130911     C*
194900130911     C                   SELECT
195000130911     C* F3=Fine
195100130911     C     $TASTO        WHENEQ    F03
195200130911     C                   EXSR      F03S2
195300130911     C* F8=Successivo
195400130911     C     $TASTO        WHENEQ    F08
195500130911     C                   EXSR      F08S2
195600130911     C* F12=Ritorno
195700130911     C     $TASTO        WHENEQ    F12
195800130911     C                   EXSR      F12S2
195900130911     C* F13=Selezione multipla
196000130911     C     $TASTO        WHENEQ    F13
196100130911     C                   EXSR      F13S2
196200130911     C* F17=Parzializzazioni
196300130911     C     $TASTO        WHENEQ    F17
196400130911     C                   EXSR      F17S2
196500130911     C* F18=Modifica valori assunti
196600130911     C     $TASTO        WHENEQ    F18
196700130911     C                   EXSR      F18S2
196800130911     C* F23=Altre opzioni
196900130911     C     $TASTO        WHENEQ    F23
197000130911     C                   EXSR      F23S2
197100130911     C* F24=Altri tasti
197200130911     C     $TASTO        WHENEQ    F24
197300130911     C                   EXSR      F24S2
197400130911     C* ROLL-UP
197500130911     C     $TASTO        WHENEQ    ROLLUP
197600130911     C                   EXSR      ROLS2
197700130911     C* Personalizzata
197800130911     C     $PGF2(PosFun) Whenne    *BLANK
197900130911     C                   EXSR      FperS2
198000130911     C*
198100130911     C                   OTHER
198200130911     C* CONTROLLO DATI
198300130911     C                   EXSR      CTRC2
198400130911     C     *IN99         IFEQ      *OFF
198500130911     C                   EXSR      CTRS2
198600130911     C                   ENDIF
198700130911     C*
198800130911     C                   ENDSL
198900130911     C*
199000130911     C                   ENDIF
199100130911     C*
199200130911C2226C                   ENDSR
199300130911C2226C/EJECT
199400130911  "  C************************************************************
199500130911  "  C* INIZIALIZZAZIONE PRELISTA
199600130911  "  C************************************************************
199700130911C2226C     INZS2         BEGSR
199800130911     C* pulizia SFL
199900130911     C                   SETOFF                                         3031
200000130911     C                   WRITE     FMTC2
200100130911     C                   SETON                                          31
200200130911     C*
200300130911     C* riempimento opzioni
200400130911     C                   MOVEA     $TX1          $TX
200500130912     C                   MOVEA     $FL2          $FL
200600130911     C                   Z-ADD     0             WCOZ
200700130911     C                   Z-ADD     0             WMXO
200800130911     C*  equivale al num.di elementi della schiera $TX1
200900130911     C                   Z-add     OpzLen        WMXO
201000130911     C                   Z-ADD     $C2Op1        $L01
201100130911     C                   Z-ADD     $C2Op2        $L02
201200130911     C                   EXSR      RIEOPZ
201300130911     C                   Z-ADD     23            X
201400130911     C                   MOVE      FF23          $FC2(X)
201500130912     C                   MOVEL     WOPZ1         C2OPZ
201600130912     C                   MOVEL     $ALLOPT       H1ALLOPT
201700130912     C                   MOVEL     WOPZ2         C2OPZ2
201800130912     C                   Z-ADD     WCOZ          C2COZ
201900130912     C* riga unica opzioni
202000130912     C                   eval      $A = %triml(c2opz)
202100130912     C                   eval      $B = %triml(c2opz2)
202200130912     C     $A            cat(P)    $B:2          C2opz1
202300130912     C*
202400130912     C                   eval      C2Unita   =D1Unita
202500130912     C                   eval      C2kcc     =D1kcc
202600130912     C                   eval      C2PosAttrD=D1PosAttrD
202700130912     C                   eval      C2attrib=D1Attrib
202800130912     C                   eval      C2attribD=D1AttribD
202900130912     C* Intestazione a seconda del raggruppamento
203000130912     C                   If        $RgrPre = 'P'
203100130912     C                   Eval      C2CodiceD ='P.Iva/Codice fiscale'
203200130912     C                   Else
203300130912     C                   Eval      C2CodiceD ='Soggetto            '
203400130912     C                   Endif
203500130912     C*
203600130912     C* CARICAMENTO SFL A PAGINE
203700130912     C*
203800130912     C                   Z-ADD     0             S2NRR
203900130912     C                   Z-ADD     0             WMAXS2
204000130912     C*
204100130911     C* Posizionamento su file pilota
204200130911     C                   CLEAR                   C2POS
204300130911     C                   EXSR      SETANAPRE
204400130911     C* ripristino i valori del posizionamento
204500130911     C                   MOVE      W2POS         C2POS
204600130911     C*
204700130911     C* Lettura file pilota
204800130911     C                   EXSR      REDANAPRE
204900130911     C*
205000130911     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
205100130911     C                   IF        $LastV = 'D1'
205200130911     C                   Z-ADD     SFLPAG        WPAG2
205300130911     C                   ENDIF
205400130911     C                   EXSR      ROLS2
205500130911     C* imposto il caricamento sfl del numero massimo di righe per pagina
205600130911     C                   Z-ADD     SFLPAG        WPAG2
205700130911     C*
205800130911C2226C                   ENDSR
205900130911  "  C/EJECT
206000130911  "  C************************************************************
206100130911  "  C* CARICAMENTO PAGINA LISTA
206200130911  "  C************************************************************
206300130912C2226C     ROLS2         BEGSR
206400130912     C*
206500130912     C                   SETOFF                                       32
206600130912     C                   Z-ADD     0             Y
206700130912     C                   Z-ADD     WMAXS2        S2NRR
206800130912     C*
206900130912     C* Leggo dal file anagrafico per caricare la lista
207000130912     C*
207100130912     C     $EndFilePre   DOWEQ     *OFF
207200130912     C     Y             ANDLT     WPAG2
207300130912     C     S2nrr         andlt     9999
207400130912     C*
207500130912     C                   EXSR      RIES2
207600130912     C*
207700130912     C                   ADD       1             S2NRR
207800130912     C                   ADD       1             Y
207900130912     C*
208000130912     C                   WRITE     FMTS2
208100130912     C*
208200130912     C                   EXSR      REDANAPRE
208300130911     C*
208400130911     C                   ENDDO
208500130911     C*
208600130911     C                   Z-ADD     S2NRR         WMAXS2               30
208700130911     C*
208800130911     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
208900130911     C*
209000130911     C     S2NRR         DIV       SFLPAG        PAGINE            4 0
209100130911     C                   MVR                     RESTO             3 0
209200130911     C     PAGINE        MULT      SFLPAG        C2RCD
209300130911     C     RESTO         IFGT      0
209400130911     C                   ADD       1             C2RCD
209500130911     C                   ELSE
209600130911     C                   SUB       SFLPAG        C2RCD
209700130911     C                   ADD       1             C2RCD
209800130911     C                   ENDIF
209900130911     C*
210000130911C2226C                   ENDSR
210100130911  "  C/EJECT
210200130911  "  C************************************************************
210300130911  "  C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
210400130911  "  C************************************************************
210500130911C2226C     SETANAPRE     BEGSR
210600130911     C*
210700130911     C                   exsr      InzSqlpre
210800130918     C*
210900130919     C                   exsr      Cartab
211000130919     C*
211100130919     C                   exsr      InzSqlTab
211200130919     C*
211300130918C2226C                   ENDSR
211400130918  "  C/EJECT
211500130918  "  C************************************************************
211600130919  "  C* CARICAMENTO tabella
211700130918  "  C************************************************************
211800130919C2226C     CARTAB        BEGSR
211900130920      *
212000130920     C                   exsr      clocurtab
212100130920      *
212200130920     C/EXEC SQL
212300130920     C+ DROP TABLE qtemp/NDRCATEMP
212400130920     C/END-EXEC
212500130920      *
212600130920     C                   if        SqlCod<0
212700130920     C                             and SqlCod<>-204
212800130920     C                   dump(a)
212900130920     C                   exsr      endpgm
213000130920     C                   endif
213100130919     C/EXEC SQL
213200130919     C+ CREATE TABLE QTEMP/NDRCATEMP (KCCPRE CHAR ( 6) NOT NULL WITH
213300130919     C+ DEFAULT, PARTIVAPRE CHAR ( 20) NOT NULL WITH DEFAULT, CDFISCPRE
213400130919     C+ CHAR ( 20) NOT NULL WITH DEFAULT, SOGGPRE CHAR ( 8) NOT NULL WITH
213500130919     C+ DEFAULT, DESCRPRE CHAR ( 44) NOT NULL WITH DEFAULT, IMPPRE DEC (
213600130919     C+ 19, 3) NOT NULL WITH DEFAULT)
213700130919     C/END-EXEC
213800130919     C                   if        SqlCod<0
213900130919     C                   dump(a)
214000130919     C                   exsr      endpgm
214100130919     C                   endif
214200130918     C*
214300130919     C                   movel     *off          $First
214400130918     C                   Clear                   $PKcc
214500130918     C                   Clear                   $PPartiva
214600130918     C                   Clear                   $PCdFisc
214700130918     C                   Clear                   $PSogg
214800130919     C                   Clear                   ImpPre
214900130919     C                   Clear                   DescrPre
215000130918     C*
215100130918     C                   DO        *hival
215200130918     C*
215300130918     C                   exsr      FetchSqlPre
215400130918     C*
215500130918     C                   If        SqlCod<0 or SqlCod=100
215600130918     C                   LEAVE
215700130918     C                   End
215800130918     C*
215900130918     C                   Reset                   NDC981DS
216000130919     C                   Eval      Societa981 = ds2Societa
216100130919     C                   Eval      Kcc981 = ds2Kcc
216200130919     C                   Eval      UniTa981 = ds2Unita
216300130918     C                   Eval      Partiva981 =  Ds2Partiva
216400130918     C                   Eval      Cdfisc981  =  Ds2CdFisc
216500130918     C                   Eval      Sogg981    =  Ds2Sogg
216600130919     C                   Call      'NDC981R'
216700130919     C                   parm                    Ndc981DS
216800130918     C*
216900130919     C                   If        Errore981 =  *On or
217000130919     C                             impscad981 = 0
217100130918     C                   Iter
217200130918     C                   Endif
217300131021      * Nuovo Filtro su importo scaduto
217400131022     C***                If        impscad981 < d1impda or
217500131022     C***                          d1impa > *zeros and
217600131022     C***                          impscad981 > d1impa
217700131022     C***                Iter
217800131022     C***                Endif
217900130919     C*
218000130919     C                   If        $First = *off
218100130919     C                   Movel     Ds2Kcc        $PKcc
218200130919     C                   Movel     Ds2Partiva    $PPartiva
218300130919     C                   Movel     Ds2CdFisc     $PCdFisc
218400130919     C                   Movel     Ds2Sogg       $PSogg
218500130919     C                   Eval      $First = *on
218600130919     C                   Endif
218700130918     C*
218800130919     C                   Select
218900130919     C*
219000130918     C                   When      $RgrPre = 'P'
219100130919     C*
219200130918     C                   IF        Ds2Kcc + Ds2Partiva + Ds2CdFisc  <>
219300130918     C                             $PKcc + $PPartiva  + $PCdFisc
219400130919     C                   Exsr      InsertTab
219500130919     C                   Movel     Ds2Kcc        $PKcc
219600130918     C                   Movel     Ds2Partiva    $PPartiva
219700130918     C                   Movel     Ds2CdFisc     $PCdFisc
219800130919     C                   Movel     Ds2Sogg       $PSogg
219900130919     C                   Clear                   Imppre
220000130919     C                   Clear                   descrpre
220100130918     C                   Endif
220200130918     C*
220300130918     C                   Other
220400130919     C*
220500130918     C                   IF        Ds2Kcc + Ds2Sogg <>
220600130918     C                             $PKcc + $PSogg
220700130919     C                   Exsr      InsertTab
220800130919     C                   Movel     Ds2Kcc        $PKcc
220900130918     C                   Movel     Ds2Partiva    $PPartiva
221000130918     C                   Movel     Ds2CdFisc     $PCdFisc
221100130919     C                   movel     Ds2Sogg       $PSogg
221200130919     C                   Clear                   Imppre
221300130919     C                   Clear                   descrpre
221400130919     C                   Endif
221500130918     C*
221600130918     C                   EndSl
221700130918     C*
221800130918     C                   Add       ImpScad981    ImpPre
221900130919     C                   movel     Ds2descr      descrpre
222000130918     C*
222100130918     C                   ENDDO
222200130920     C* Anche l'ultimo record
222300130920     C                   If        $First = *on
222400130920     C                   Exsr      InsertTab
222500130920     C                   Endif
222600130918     C*
222700130911C2226C                   ENDSR
222800130911  "  C/EJECT
222900130911  "  C************************************************************
223000130919  "  C* Inserimento tabella
223100130911  "  C************************************************************
223200130919C2226C     InsertTab     BEGSR
223300130919     C*
223400130919     C/EXEC SQL
223500130919     C+ INSERT INTO QTEMP/NDRCATEMP VALUES(:$pKcc,:$pPartiva,
223600130919     C+ :$pcdfisc, :$pSogg, :Descrpre, :imppre)
223700130919     C/END-EXEC
223800130919     C                   if        SqlCod<0
223900130919     C                   dump(a)
224000130919     C                   exsr      endpgm
224100130919     C                   endif
224200130919C2226C                   ENDSR
224300130919  "  C/EJECT
224400130919  "  C************************************************************
224500130919  "  C* LETTURA RCD ARCHIVIO PILOTA
224600130919  "  C************************************************************
224700130919C2226C     REDANAPRE     BEGSR
224800130911     C*
224900130911     C                   MOVEL     *OFF          $EndFilePre
225000130911     C                   MOVEL     *OFF          $RCDOKPre
225100130911     C*
225200130911     C     $EndFilePre   DOUEQ     *ON
225300130911     C     $RCDOKPRE     OREQ      *ON
225400130919     C*
225500130919     C                   exsr      FetchSqlTab
225600130919     C*
225700130919     C                   if        SqlCod<0 or SqlCod=100
225800130919     C                   MOVE      *ON           *IN21
225900130919     C                   ELSE
226000130919     C                   MOVE      *OFF          *IN21
226100130919     C                   END
226200130919     C*
226300130919     C                   MOVE      *IN21         $EndFilePre
226400130912     C*
226500130912     C                   If        $EndFilePre = *off
226600130919     C                   Eval      $rcdOkPre = *on
226700130919     C                   Endif
226800130912     C*
226900130912     C                   ENDDO
227000130912     C*
227100130912     C                   MOVE      $EndFilePre   *IN34                          SflEnd
227200130912     C*
227300130912C2226C                   ENDSR
227400130912  "  C/EJECT
227500130911  "  C************************************************************
227600130911  "  C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL S2
227700130911  "  C************************************************************
227800130911C2262C     CLCPAG2       BEGSR
227900130911     C* Input :
228000130911     C* - WSFL2= numero dell'ultimo rcd su cui era POSIZIONATO il
228100130911     C*          cursore
228200130911     C* - SFLPAG = numero rcd per pagina sfl
228300130911     C* Output :
228400130911     C* - WPAG2= pagina fino a cui deve essere ricaricato il sfl
228500130911     C*
228600130911     C     WSFL2         DIV       SFLPAG        PAGINE            4 0
228700130911     C                   MVR                     RESTO             3 0
228800130911     C     RESTO         IFGT      0
228900130911     C                   ADD       1             PAGINE
229000130911     C                   ENDIF
229100130911     C     PAGINE        MULT      SFLPAG        WPAG2
229200130911     C*
229300130911C2226C                   ENDSR
229400130911C2226C/EJECT
229500940224     C************************************************************
229600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
229700940224     C************************************************************
229800940224     C     CLCPAG        BEGSR
229900940224     C* Input :
230000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
230100940224     C*          cursore
230200940224     C* - SFLPAG = numero rcd per pagina sfl
230300940224     C* Output :
230400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
230500940224     C*
230600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
230700940224     C                   MVR                     RESTO             3 0
230800940224     C     RESTO         IFGT      0
230900940224     C                   ADD       1             PAGINE
231000940224     C                   ENDIF
231100940224     C     PAGINE        MULT      SFLPAG        WPAG
231200940224     C*
231300940224     C                   ENDSR
231400940131     C/EJECT
231500940131     C************************************************************
231600940131     C* SELEZIONI SULLA RIGA LETTA
231700940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
231800940314     C* non rientra nei parametri di parzializzazione
231900940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
232000940314     C* superiore stabilito nei parametri
232100940314     C*
232200940131     C************************************************************
232300941214     C     SELANA        BEGSR
232400940131     C*
232500940202     C                   MOVEL     *ON           $RCDOK
232600070119     C                   MOVE      *hival        ALL9             15
232700100407     c* aggancio file per partita per reperire altri dati
232800100407     C                   clear                   SAvv              1
232900100407     C                   clear                   Ssol              1
233000100407     C                   clear                   SImp              1
233100100407     C                   clear                   SImpSCa           1
233200100511     C                   clear                   STel              1
233300100511     C                   clear                   Smail             1
233400130923C2226C                   clear                   SAtt              1
233500130923C2226C                   clear                   SAttSCa           1
233600100407     c                   exsr      LeggiRCa
233700970515     C*
233800940202     C                   ENDSR
233900940131     C/EJECT
234000100407     C************************************************************
234100100407     C* leggo rca per la partita in lettura
234200100407     C************************************************************
234300100407     C     LeggiRca      BEGSR
234400100407     c*
234500100407     c                   eval      RcaSocieta=DsSocieta
234600100407     c                   eval      RcaPosiz  =DsPosiz
234700100407     c                   eval      RcaKcc    =DsKcc
234800100407     c                   eval      RcaKsc    =DsKsc
234900100407     c                   eval      RcaDtPar  =DsDtPar
235000100407     c                   eval      RcaNrPar  =DsNrPar
235100100407     c                   eval      RcaSerPar =DsSerPar
235200100413     c     K07Rca01      setll     NdRca01L
235300100407     c                   do        *hival
235400100413     c     K07Rca01      reade     NdRca01L
235500100413     c                   if        %eof(NdRca01L)
235600100407     c                   leave
235700100407     c                   endif
235800100511     c* vedo quale è il tipo di avvitivà da svolgere
235900100511     C                   MOVE      *ALL'0'       XTBKEY
236000100511    >C                   MOVE      RcaAzione     XTBKEY
236100100511     C                   MOVE      '0'           XTBALC
236200100511     C                   MOVE      '1'           XTBRIC
236300100511     C                   MOVE      '047'         XTBCOD
236400100511     C                   CALLB     'XATB'
236500100511     C                   PARM                    XATBDS
236600100511     C     XTBERR        IFEQ      '0'
236700100511     C                   MOVEL     XTBUNI        Ang047ds
236800100407     c*
236900100511     c* avviso stampato
237000100520     c                   if        Attivit47='A'  and RcaStato<> *blanks
237100100407     c                   eval      SAvv  = *on
237200100407     c                   endif
237300100511     c* sollecito stampato
237400100520     c                   if        Attivit47='S'  and RcaStato <> *blanks
237500100407     c                   eval      SSol  = *on
237600100407     c                   endif
237700100511     c* telefonata fatta
237800100511     c                   if        Attivit47='T'  and RcaStato <> *blanks
237900100511     c                   eval      STel  = *on
238000100511     c                   endif
238100100511     c* lettera o e-mail fatte
238200100511     c                   if        Attivit47='L'  and RcaStato <> *blanks
238300100511     c                             or
238400100511     c                             (Attivit47='E'  and RcaStato <> *blanks)
238500100511     c                   eval      SMail = *on
238600100511     c                   endif
238700100524     c* impegno presente NON chiuso, attivo
238800100524     c                   if        Attivit47='I' and RcaStato = *blanks
238900100407     c                   eval      SImp  = *on
239000100407     c                   endif
239100100524     c* impegno scaduto presente, non chiuso
239200100524     c                   if        Attivit47='X' and RcaStato = *blanks
239300100524     c                   eval      SImpSca = *on
239400100407     c                   endif
239500130923C2226c* se attività libera ne verifico la presenza nel file
239600130923  "  C* ndatt00f
239700130923  "  C                   eval      ATTSocieta = RcaSocieta
239800130923  "  C                   eval      ATTKcc     = RcaKcc
239900130923  "  C                   eval      ATTKsc     = RcaKsc
240000130923  "  C                   eval      ATTDtPar   = RcaDtPar
240100130923  "  C                   eval      ATTNrPar   = RcaNrPar
240200130923  "  C                   eval      ATTSerPar  = RcaSerPar
240300130923  "  C                   eval      ATTDtSca   = RcaDtSca
240400130923  "  C                   eval      ATTProgr   = *Hival
240500130923  "  C     K08Att01      setgt     NdATT01l
240600130923  "  C     K07Att01      ReadPe    NdATT01l
240700130923  "  C                   If        Not %eof(NdAtt01l)
240800130923  "  C                   If        AttDtAtt > zdateIso
240900130923  "  C                   Eval      SAtt = *on
241000130923  "  C                   Else
241100130923  "  C                   eval      SAttSca = *on
241200130923  "  C                   Endif
241300130923C2226C                   Endif
241400100407     c*
241500100511     c                   endif
241600100407     c                   enddo
241700100407     c*
241800100407     c                   ENDSR
241900940131     C************************************************************
242000940131     C* RIEMPIMENTO RIGA LISTA
242100940131     C************************************************************
242200940131     C     RIES1         BEGSR
242300940131     C*
242400940131     C                   CLEAR                   FMTS1
242500100407     c                   move      SAvv          H1Avv
242600100407     c                   move      SSol          H1Sol
242700100407     c                   move      SImp          H1Imp
242800100407     c                   move      SImpSca       H1ImpSca
242900100511     c                   move      STel          H1Tel
243000100512     c                   move      Smail         H1in15
243100100419     c                   move      DsDivisa      H1Divisa
243200130923C2226c                   move      SAtt          H1Att
243300130923C2226c                   move      SAttSca       H1AttSca
243400950301     C*
243500950301     C* riempio i campi del sfl, compresi gli hidden con i valori
243600950301     C* del rcd letto
243700100407    >C                   MOVEL     DsKcc         H1Kcc
243800100407    >C                   MOVEL     DsKsc         S1Ksc
243900100402     C* Richiamo modulo controllo conto
244000100407     c                   move      Dskcc         $kcc
244100100407     c                   move      Dsksc         $ksc
244200100402     C                   EXSR      CALLMVC002
244300100402     C     $Esito        IFne      *ON
244400100402     C                   MOVEL     DEScr002      s1descr
244500100521     c                   if        DtAnn002 <=Zdateiso and
244600100521     c                             DtAnn002 <> *loval
244700100521     c                   move      *on           H1in18
244800100521    -C                   endif
244900100402    -C                   ENDIF
245000100402     c*
245100100407     c                   if        DsDtPar    <> *loval
245200100407     c     *jobrun       move      DsDtPar       s1dtPar
245300100402     c                   endif
245400100407     c                   z-add     DsNrPar       S1NrPar
245500100407     c                   move      DsSerPar      S1SerPar
245600100407     c*
245700100407     c*
245800100407     c                   move      DskscAge      s1kscAge
245900100402   1 C     s1kscage      IFNE      *blanks
246000100402     C                   CALLb     'XALLINEA'
246100100402     C                   PARM                    s1kscage
246200100402     C                   PARM      8             LEN               5 0
246300100402     C                   PARM                    XSCINP
246400100402     C* reperisco capoconto agente
246500100402     C                   EVAL      Xcapkcc  = 'F     '
246600100402     C                   MOVE      XSCSOC        XCAPSOCIET
246700100402     C                   MOVE      s1kscage      XCAPKSC
246800100402     C                   CALLB     'XCAPCLIFOR'
246900100402     C                   PARM                    XCAPSOCIET        3
247000100402     C                   PARM                    XCAPKCC           6
247100100402     C                   PARM                    XCAPKSC           8
247200100402     C* Richiamo modulo controllo conto
247300100402     c                   move      Xcapkcc       $kcc
247400100402     c                   move      Xcapksc       $ksc
247500100402     C                   EXSR      CALLMVC002
247600100402   2 C     $Esito        IFne      *ON
247700100402     C                   MOVEL     DESBREV002    s1kscageD
247800100402   2-C                   ENDIF
247900100402   1-C                   ENDIF
248000950301     C*
248100090325     C                   Eval      S1opz    =    Wopz
248200090325     C                   if        S1opz     <>  0
248300090325     C                   SetOn                                        32
248400090325     C                   endif
248500100407     c*
248600090325     c*
248700950301     C* imposto quegli attributi del sfl che sono modificabili
248800950301     C* al richiamo del determinato pgm
248900950301     C                   EXSR      ATRS1
249000940131     C*
249100940131     C                   ENDSR
249200950301     C/EJECT
249300950301     C************************************************************
249400950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
249500950301     C************************************************************
249600950301     C     ATRS1         BEGSR
249700950301     C*
249800971120     C* in questa routine bisogna attivare indicatori o campi programma
249900971120     C* che gestiscono gli attributi di uno o più campi di questa videata;
250000971120     C* attenzione : non è questo il posto in cui eseguire controlli,
250100971120     C*  dovrebbero già arrivare dei flag
250200970618     C*
250300100407     C                   MOVEL     X'A0'         H1AvvA
250400100407     C                   MOVEL     X'A0'         H1SolA
250500100407     C                   MOVEL     X'A0'         H1ImpA
250600100407     C                   MOVEL     X'A0'         H1ImpScaA
250700100511     C                   MOVEL     X'A0'         H1TelA
250800130923C2226C                   MOVEL     X'A0'         H1AttA
250900130923C2226C                   MOVEL     X'A0'         H1AttScaA
251000100412     c*
251100100407     c                   if        H1Avv=*on
251200100407     c                   move      Presente      H1AvvA
251300100407     c                   endif
251400950301     C*
251500100407     c                   if        H1Sol=*on
251600100408     c                   move      PreseBlu      H1SolA
251700100407     c                   endif
251800100511     C*
251900100511     c                   if        H1Tel=*on
252000100511     c                   move      PreseHig      H1TelA
252100100511     c                   endif
252200100511     C*
252300100512     c                   move      H1in15        *in15
252400100407     C*
252500100407     c                   if        H1Imp=*on
252600100407     c                   move      PresePnk      H1ImpA
252700100407     c                   endif
252800100407     C*
252900100407     c                   if        H1ImpSca=*on
253000100407     c                   move      PreseREd      H1ImpScaA
253100100407     c                   endif
253200100521     c* cliente annullato
253300100521     c                   move      H1in18        *in18
253400130923C2226C*
253500130923  "  C* Presenza di attività manuali in essere o scadute
253600130923  "  C                   if        H1Att=*on
253700130923  "  C                   move      PresePnk      H1AttA
253800130923  "  C                   endif
253900130923  "  C*
254000130923  "  C                   if        H1AttSca=*on
254100130923  "  C                   move      PreseREd      H1AttScaA
254200130923C2226c                   endif
254300100407     C*
254400950301     C                   ENDSR
254500941206     C/EJECT
254600941206     C************************************************************
254700941206     C* GESTIONE F08 VIDEO S1
254800941206     C************************************************************
254900941206     C     F08S1         BEGSR
255000941206     C*
255100941206     C                   MOVE      *ON           $FINE
255200090323    >C                   MOVE      '0'           RETL76
255300941206     C*
255400941206     C                   ENDSR
255500940309     C/EJECT
255600940309     C************************************************************
255700940309     C* GESTIONE F03 VIDEO S1
255800940309     C************************************************************
255900940309     C     F03S1         BEGSR
256000940309     C*
256100940309     C                   MOVE      *ON           $FINE
256200090323    >C                   MOVE      '1'           RETL76
256300940325     C* fine programma
256400940325     C                   EXSR      ENDPGM
256500940309     C*
256600940309     C                   ENDSR
256700130924C2226C************************************************************
256800130924  "  C* GESTIONE F05 VIDEO S1
256900130924  "  C************************************************************
257000130924C2226C     F05S1         BEGSR
257100130924     C*
257200130924     C                   MOVE      *ON           $INZS1
257300130924     C                   MOVE      *ON           WINZS1
257400130924     C*
257500130924C2226C                   ENDSR
257600940309     C************************************************************
257700940309     C* GESTIONE F12 VIDEO S1
257800940309     C************************************************************
257900940309     C     F12S1         BEGSR
258000940309     C*
258100940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
258200940309     C     $LOP          IFEQ      1
258300130912C2226C                   IF        $Pre = *on
258400130911  "  C                   MOVE      'S2'          $GEST
258500130923  "  C                   move      *ON           $INZS2                         iniz.sfl fine ciclo
258600130923  "  c                   move      *on           Winzs2
258700130911C2226C                   Else
258800940309     C                   MOVE      'D1'          $GEST
258900130911C2226C                   Endif
259000940309     C*  se la 1a videata è questa, per F12 torno a menu
259100940309     C                   ELSE
259200130912C2226C                   IF        $Pre = *on
259300130911  "  C                   MOVE      'S2'          $GEST
259400130911C2226C                   Else
259500940309     C                   MOVE      *ON           $FINE
259600090323    >C                   MOVE      '2'           RETL76
259700130911C2226C                   ENDIF
259800941027     C                   ENDIF
259900940309     C*
260000940309     C                   ENDSR
260100940309     C/EJECT
260200090325     C************************************************************
260300090325     C* GESTIONE F13 VIDEO S1
260400090325     C************************************************************
260500090325     C     F13S1         BEGSR
260600090325     C*
260700090325     C* E' simile alla funzione F13 del PDM: riporta l'opzione della
260800090325     C* riga del sfl su cui è posizionato il cursore dalla riga
260900090325     C* successiva fino alla fine del sfl stesso,
261000090325     C* senza sovrapporsi alle opzioni preesistenti.
261100090325     C* il controllo lo faccio solo se il sfl NON è vuoto
261200090325   1bC                   If        WMax    <>    0
261300090325     C* determino Riga/Colonna del cursore
261400090325     C                   MOVE      NRG           R$$
261500090325     C                   MOVE      NCL           C$$
261600090325     C                   Z-ADD     RIRI          CURR
261700090325     C                   Z-ADD     COCO          CURC
261800090325     C*
261900090325     C* se sono posizionato sul sfl
262000090325     C* salvo il numero del record su cui sono posizionato
262100090325     c                   Z-Add     C1Nrr         WC1Rcd
262200090325     C* leggo il rcd
262300090325     C     C1NrR         CHAIN     FMTS1                              21
262400090325     C* salvo l'opzione e il numero relativo
262500090325     C                   If        S1Opz   >     0
262600090325     C                   Z-Add     S1Opz         WOpz              2 0
262700090325     c                   Z-Add     C1Nrr         $X
262800090325     C* attivo SflNxtChg
262900090325     C                   SetOn                                        32
263000090325     C                   UpDate    FmtS1
263100090325     C                   SetOff                                       32
263200090325     C*
263300090325     C* mi posiziono sul record successivo
263400090325     C                   Add       1             $X
263500090325     c*
263600090325     C                   DoW       $X      <=    WMax
263700090325     C     $X            Chain     FmtS1                              21
263800090325     C   21              Leave
263900090325     C                   if        S1Opz    =    0
264000090325     C* imposto l'opzione solo se è possibile eseguirla
264100090325     c*
264200090325     C                   Z-Add     WOpz          S1Opz
264300090325     C                   endif
264400090325     C*                  endif
264500090325     C* attivo SflNxtChg
264600090325     C                   SetOn                                        32
264700090325     C                   UpDate    FmtS1
264800090325     C                   SetOff                                       32
264900090325     C*
265000090325     C                   Add       1             $X
265100090325     C                   enddo
265200090325     c*
265300090325     C                   Eval      WPag     =    9999
265400090325     C*
265500090325     C                   ExSr      RolS1
265600090325     C* reimposto sflpag
265700090325     C                   Eval      WPag     =    SflPag
265800090325     C*
265900090325     C* ripristino la posizione iniziale del record
266000090325     c                   Z-Add     WC1Rcd        C1Rcd
266100090325     C* ripristino il campo di salvataggio dell'opzione per non sporca
266200090325     C* re il campo S1Opz in caso di ricaricamento del sfl
266300090325     C                   Z-Add     0             WOpz
266400090325     C*
266500090325     C                   endif
266600090325     C*
266700090325     C                   endif
266800090325     C*
266900090325     C*
267000090325     C                   ENDSR
267100090325     C/EJECT
267200940309     C************************************************************
267300940309     C* GESTIONE F17 VIDEO S1
267400940309     C************************************************************
267500940309     C     F17S1         BEGSR
267600940309     C*
267700940309     C                   MOVE      'D1'          $GEST
267800941104     C* se il pgm è stato richiamato con chiave univoca
267900941104     C* e dopo è stata richiesta una parzializzazione
268000941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
268100941104     C* valore; propongo come ordinamento il 1°)
268200950606     C     $Unique       IFEQ      *ON
268300950606     C                   MOVE      *OFF          $Unique
268400090323    >C***                MOVE      *BLANK        UQ1L76
268500941027     C                   ENDIF
268600940831     C     $ORD          IFEQ      99
268700940831     C                   Z-ADD     1             $ORD
268800941104     C                   MOVE      *ON           $INZD1
268900941027     C                   ENDIF
269000940309     C*
269100940309     C                   ENDSR
269200940309     C/EJECT
269300940309     C************************************************************
269400940309     C* GESTIONE F18 VIDEO S1
269500940309     C************************************************************
269600940309     C     F18S1         BEGSR
269700940309     C*
269800940309     C                   MOVE      'D2'          $GEST
269900940309     C                   MOVEL     *ON           $INZD2
270000940831     C     $ORD          IFEQ      99
270100940831     C                   Z-ADD     1             $ORD
270200941027     C                   ENDIF
270300940309     C*
270400940309     C                   ENDSR
270500940309     C/EJECT
270600940309     C************************************************************
270700940309     C* GESTIONE F23 VIDEO S1
270800940309     C************************************************************
270900940309     C     F23S1         BEGSR
271000940309     C*
271100950906     C                   Z-ADD     $C1Op1        $L01
271200950906     C                   Z-ADD     $C1Op2        $L02
271300940309     C                   EXSR      RIEOPZ
271400941214     C                   Z-ADD     23            X
271500941214     C                   MOVE      FF23          $FC2(X)
271600070531     C                   MOVEL     WOPZ1         C1OPz
271700060224     C                   MOVEL     $ALLOPT       H1ALLOPT
271800940608     C                   MOVEL     WOPZ2         C1OPZ2
271900940309     C                   Z-ADD     WCOZ          C1COZ
272000070531     c* riga unica opzioni
272100070531     c                   eval      $A = %triml(c1opz)
272200070531     c                   eval      $B = %triml(c1opz2)
272300070531     c     $A            cat(P)    $B:2          C1opz1
272400940309     C*
272500940309     C                   ENDSR
272600940309     C/EJECT
272700940309     C************************************************************
272800940309     C* GESTIONE F24 VIDEO S1
272900940309     C************************************************************
273000940309     C     F24S1         BEGSR
273100940506     C*
273200940506     C                   MOVEA     $FX2          $FX
273300940506     C                   MOVEA     $FC2          $FC
273400940506     C                   MOVEA     $FR2          $FR
273500940506     C                   Z-ADD     $ULKS1        $ULK
273600950726     C                   Z-ADD     $S1L01        $L01
273700950726     C                   Z-ADD     $S1L02        $L02
273800940506     C                   EXSR      RIEKEY
273900940506     C                   Z-ADD     $ULK          $ULKS1            3 0
274000940720     C                   MOVEA     $FC           $FC2
274100070618     C                   MOVEL     $KEY1         Z2KE
274200060224     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
274300940506     C                   MOVEL     $KEY2         Z2KE2
274400070618     c* riga unica tasti
274500070618     c                   eval      $A = %triml(z2ke)
274600070618     c                   eval      $B = %triml(z2ke2)
274700070618     c     $A            cat(P)    $B:0          z2ke1
274800940506     C                   WRITE     FMTZ2
274900940506     C*
275000940309     C                   ENDSR
275100940128     C/EJECT
275200940128     C************************************************************
275300940131     C* CONTROLLO TESTATA LISTA
275400940128     C************************************************************
275500940131     C     CTRC1         BEGSR
275600940128     C*
275700940201     C                   MOVE      *OFF          *IN99
275800940131     C*
275900940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
276000940202     C* L'INIZIALIZZAZIONE LISTA
276100940202     C*
276200941214     C     C1POS         IFNE      W1POS
276300940202     C                   EXSR      POSS1
276400941027     C                   ENDIF
276500940202     C*
276600940202     C                   ENDSR
276700940202     C/EJECT
276800940202     C************************************************************
276900940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
277000940202     C************************************************************
277100940202     C     POSS1         BEGSR
277200940202     C*
277300940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
277400940202     C*
277500941118   B1C     WMAX          IFGT      0
277600940202     C                   READC     FMTS1                                  21
277700941118   B2C     *IN21         DOWEQ     *OFF
277800940202     C     *IN99         ANDEQ     *OFF
277900940202     C*
278000941118   B3C     S1OPZ         IFNE      0
278100960109     C                   SETON                                        92  99
278200941118   E3C                   ENDIF
278300940202     C* se rilevata opzione attiva, attivo sflnxtchg
278400941118   B3C     *IN99         IFEQ      *ON
278500940202     C                   MOVE      *ON           *IN32
278600950301     C*
278700950301     C* imposto quegli attributi del sfl che sono modificabili
278800950301     C* al richiamo del determinato pgm
278900941118     C                   EXSR      ATRS1
279000941118     C*
279100940202     C                   UPDATE    FMTS1
279200941118   X3C                   ELSE
279300940202     C                   READC     FMTS1                                  21
279400941118   E3C                   ENDIF
279500940202     C*
279600941118   E2C                   ENDDO
279700940202     C*
279800941118   E1C                   ENDIF
279900940202     C*
280000940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
280100940221     C* AL VALORE DI POSIZIONAMENTO :
280200940221     C* se questo è maggiore del valore del rispettivo campo di
280300940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
280400940221     C* con i valori di parzializzazione.
280500940221     C*
280600941118   B1C     *IN99         IFEQ      *OFF
280700940721    >C                   MOVE      *BLANK        W1POS
280800940202     C*
280900950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
281000950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
281100950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
281200950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
281300950525     C* verificata la condizione suddetta avverrà con posizionamento a
281400950525     C* partire dal valore di parzializzazione
281500950525     C*
281600941118   B2C                   SELECT
281700940202     C*
281800940202     C     $ORD          WHENEQ    1
281900070524    >C     C1POS         IFGT      D1KSC
282000951005    >C                   MOVEL     C1POS         W1POS
282100070524    >C                   CALLB     'XALLINEA'
282200070524    >C                   PARM                    W1Pos
282300070524    >C                   PARM      8             LEN               5 0
282400070524    >C                   PARM                    XSCINP
282500941027     C                   ENDIF
282600950525     C*
282700950525     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
282800950525     C*
282900941118   E2C                   ENDSL
283000950525     C*
283100950525     C* richiedo l'inizializzazione sfl
283200950525     C                   MOVE      *ON           $INZS1
283300940202     C*
283400941118   E1C                   ENDIF
283500940202     C*
283600940131     C                   ENDSR
283700940131     C/EJECT
283800940131     C************************************************************
283900940131     C* CONTROLLO OPZIONI LISTA
284000940131     C************************************************************
284100940131     C     CTRS1         BEGSR
284200940131     C*
284300940202     C                   MOVEL     *OFF          $ESCI
284400940201     C                   SETOFF                                       99
284500940131     C                   Z-ADD     0             S1OPZ
284600940131     C*
284700940127     C* Leggo il sfl solo se ci sono rcd
284800941118   B1C     WMAX          IFGT      0
284900940131     C                   READC     FMTS1                                  21
285000940127     C*
285100940131     C* esce se fine sfl o errore che richiede l'uscita
285200941118   B2C     *IN21         DOWEQ     *OFF
285300940131     C     $ESCI         ANDEQ     *OFF
285400940201     C                   Z-ADD     S1NRR         C1RCD
285500940131     C* ctrl su riga
285600940131     C                   EXSR      RECS1
285700940131     C* gestione opzioni
285800941118   B3C     S1OPZ         IFNE      0
285900940201     C     *IN99         ANDEQ     *OFF
286000940131     C                   EXSR      OPZS1
286100941118   E3C                   ENDIF
286200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
286300941118   B3C     *IN99         IFEQ      *ON
286400940131     C                   MOVE      *ON           *IN32
286500941118   E3C                   ENDIF
286600950301     C*
286700950301     C* imposto quegli attributi del sfl che sono modificabili
286800950301     C* al richiamo del determinato pgm
286900941118     C                   EXSR      ATRS1
287000941118     C*
287100940131     C                   Z-ADD     0             S1OPZ
287200940223     C*
287300940127     C                   UPDATE    FMTS1
287400941125     C                   MOVE      *OFF          *IN32
287500940223     C*
287600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
287700941118   B3C     $ESCI         IFEQ      *OFF
287800940131     C                   READC     FMTS1                                  21
287900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
288000941118   B4C     *IN21         IFEQ      *ON
288100940201     C                   MOVE      WINZS1        $INZS1
288200010620     C                   MOVE      *off          WINZS1
288300980603     C* se è avvenuta almeno una modifica
288400980603   B5C                   IF        $INZS1 = *on
288500940204     C* calcolo pagina a cui deve posizionarsi
288600940728     C     WSFL          IFGT      0
288700940728     C                   EXSR      CLCPAG
288800940728     C                   ELSE
288900940728     C                   Z-ADD     SFLPAG        WPAG
289000940728     C                   ENDIF
289100980603   E5C                   ENDIF
289200941118   E4C                   ENDIF
289300980603   E3C                   ENDIF
289400940131     C*
289500941118   E2C                   ENDDO
289600940127     C*
289700941118   E1C                   ENDIF
289800940131     C*
289900940127     C                   ENDSR
290000940127     C/EJECT
290100940127     C************************************************************
290200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
290300940127     C************************************************************
290400940131     C     RECS1         BEGSR
290500940131     C*
290600940201     C* reset indicatori DSPATR
290700940201     C                   MOVE      *ALL'0'       IN4049           10
290800940201     C                   MOVEA     IN4049        *IN(40)
290900940201     C*
291000940131     C* Controllo se Utente Autorizzato alla scelta
291100940202     C     S1OPZ         IFNE      0
291200940131     C                   Z-ADD     1             X
291300940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
291400960117     C* mi salvo l'indice dell' opzione
291500960117     C   21              Z-add     X             PosOpz            2 0
291600940211     C     *IN21         IFEQ      *OFF
291700940223     C     $FL1(X)       OREQ      'N'
291800940131     C                   MOVE      *ON           $ESCI
291900960109     C                   SETON                                        409199
292000940131     C                   ENDIF
292100940202     C                   ENDIF
292200940131     C*
292300970424     C*
292400940131     C                   ENDSR
292500941118     C/EJECT
292600941118     C************************************************************
292700941118     C* GESTIONE OPZIONI LISTA
292800941118     C************************************************************
292900941118     C     OPZS1         BEGSR
293000950301     C*
293100950301     C                   Z-ADD     RIRI          CURR
293200950301     C                   Z-ADD     COCO          CURC
293300941118     C*
293400941027     C                   SELECT
293500090330     C* se opz=Scheda cliente
293600090330     C* richiamo il pgm di gestione
293700090330     C     S1OPZ         WHENEQ    1
293800090331     c                   move      '02'          opzL77
293900090401     c                   move      xscsoc        SocietaL77
294000100402    >C                   MOVE      C1Unita       UnitaL77
294100090330    >C                   MOVE      h1kcc         kccl77
294200090330    >C                   MOVE      s1ksc         kscl77
294300100408     c     *jobrun       move      s1dtpar       dtParl77
294400100408     c                   z-add     s1nrpar       NrparL77
294500100408     c                   move      s1serpar      SerParL77
294600100419     c                   move      H1divisa      DivisaL77
294700140314brt  c**                 if        d1stato = xscon
294800140314brt  c**                 eval      StatoL77 = *on
294900140314brt  c**                 else
295000140314brt  c**                 eval      StatoL77 = *off
295100140314brt  c**                 endif
295200090330     C                   movel     ndcl77ds      kpjbu
295300090330     C                   CALL      'NDCL77R'                            21
295400090330     C                   PARM                    KPJBA
295500100426c2156C                   movel     kpjbu         ndcl77ds
295600100426c2156c                   exsr      RTNL77
295700940831     C*
295800101209c2176C* se opz=Chiusura scadenze partita
295900101209  "  C     S1OPZ         WHENEQ    15
296000101209c2176c                   exsr      AggRca
296100070531     C* se opz=Interrogazioni
296200070531     C* richiamo il pgm di gestione
296300090402     C     S1OPZ         WHENEQ    16
296400070531     C                   reset                   ana399ds
296500070531     C                   eval      A39societa = XSCSOC
296600070531     C                   eval      A39unita = *blanks
296700070531     C                   eval      A39ctb = 'CG'
296800070531     C                   eval      A39kcc = h1kcc
296900070531     C                   eval      A39ksc = s1ksc
297000070531     C                   movel     ana399ds      kpjbu
297100070531     C                   CALL      'ANA399R'                            21
297200070531     C                   PARM                    KPJBA
297300070531     C     A39ERRORI     IFEQ      '0'
297400070531     C                   endif
297500130912C2226C*
297600130912  "  C* se selezionata opzione personalizzata, richiamo il programma
297700130912  "  C* corrispondente
297800130912  "  C     $PGO1(PosOpz) Whenne    *Blank
297900130912  " >C                   RESET                   NDCL77DS
298000130912  "  c                   move      '02'          opzL77
298100130912  "  c                   move      xscsoc        SocietaL77
298200130912  " >C                   MOVE      C1Unita       UnitaL77
298300130912  " >C                   MOVE      h1kcc         kccl77
298400130912  " >C                   MOVE      s1ksc         kscl77
298500130912  "  c     *jobrun       move      s1dtpar       dtParl77
298600130912  "  c                   z-add     s1nrpar       NrparL77
298700130912  "  c                   move      s1serpar      SerParL77
298800130912  "  c                   move      H1divisa      DivisaL77
298900140314br"t c**                 if        d1stato = xscon
299000140314br"t c**                 eval      StatoL77 = *on
299100140314br"t c**                 else
299200140314br"t c**                 eval      StatoL77 = *off
299300140314br"t c**                 endif
299400130912  "  C                   movel     ndcl77ds      kpjbu
299500130912  "  C*
299600130912  "  C                   CALL      $PGO1(PosOpz)                        22
299700130912  "  C                   PARM                    KPJBA
299800130912  "  C                   PARM                    NOMPGM
299900130912  "  C*
300000130912  " >C                   MOVEL     KPJBU         NDCL77DS
300100130912  "  C* ritorno da PGM
300200130912  " >C                   EXSR      RTNL77
300300130912C2226C*
300400070531     C                   MOVE      *HIVAL        $LASTV
300500940225     C*
300600950620     C* se altre opzioni
300700941027     C                   OTHER
300800940225     C*
300900941027     C                   ENDSL
301000940225     C*
301100940131     C                   ENDSR
301200130911C2226C************************************************************
301300130911  "  C* RIEMPIMENTO RIGA PRELISTA
301400130911  "  C************************************************************
301500130911C2226C     RIES2         BEGSR
301600130911     C*
301700130911     C                   CLEAR                   FMTS2
301800130911     C*
301900130911     C* riempio i campi del sfl, compresi gli hidden con i valori
302000130911     C* del rcd letto
302100130911     C*
302200130912     C                   Select
302300130912     C                   When      $RgrPre = 'P'
302400130918     C                   Eval      H2Kcc = KccPre
302500130918     C                   Eval      H2PartIva = PartIvaPre
302600130918     C                   Eval      H2CdFisc  = CdFiscPre
302700130918     C                   If        PartivaPre <> *blanks
302800130918     C                   Movel     PartIvaPre    S2Codice
302900130911     C                   Else
303000130918     C                   Movel     CdFiscPre     S2Codice
303100130911     C                   Endif
303200130918     C                   Movel     DescrPre      S2Descr
303300130918     C                   Z-ADD     ImpPre        S2Importo
303400130912     C                   Other
303500130919     C                   Eval      H2Kcc   = KccPre
303600130919     C                   Eval      H2Sogg  = SoggPre
303700130918     C                   Movel     H2Sogg        S2Codice
303800130918     C                   Movel     DescrPre      S2Descr
303900130918     C                   Z-ADD     ImpPre        S2Importo
304000130912     C                   EndSl
304100130911     C*
304200130911     C                   Eval      S2opz    =    Wopz
304300130911     C                   if        S2opz     <>  0
304400130911     C                   SetOn                                        32
304500130911     C                   endif
304600130911     c*
304700130911     c*
304800130911     C* imposto quegli attributi del sfl che sono modificabili
304900130911     C* al richiamo del determinato pgm
305000130911     C                   EXSR      ATRS2
305100130911     C*
305200130911     C                   ENDSR
305300130911C2226C/EJECT
305400130911  "  C************************************************************
305500130911  "  C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
305600130911  "  C************************************************************
305700130911C2226C     ATRS2         BEGSR
305800130911     C*
305900130911     C* in questa routine bisogna attivare indicatori o campi programma
306000130911     C* che gestiscono gli attributi di uno o più campi di questa videata;
306100130911     C* attenzione : non è questo il posto in cui eseguire controlli,
306200130911     C*  dovrebbero già arrivare dei flag
306300130911     C*
306400130911     C                   ENDSR
306500130911C2226C/EJECT
306600130911  "  C************************************************************
306700130911  "  C* GESTIONE F08 VIDEO S2
306800130911  "  C************************************************************
306900130911C2226C     F08S2         BEGSR
307000130911     C*
307100130911     C                   MOVE      *ON           $FINE
307200130911    >C                   MOVE      '0'           RETL76
307300130911     C*
307400130911C2226C                   ENDSR
307500130911  "  C/EJECT
307600130911  "  C************************************************************
307700130911  "  C* GESTIONE F03 VIDEO S2
307800130911  "  C************************************************************
307900130911C2226C     F03S2         BEGSR
308000130911     C*
308100130911     C                   MOVE      *ON           $FINE
308200130911    >C                   MOVE      '1'           RETL76
308300130911     C* fine programma
308400130911     C                   EXSR      ENDPGM
308500130911     C*
308600130911C2226C                   ENDSR
308700130911  "  C************************************************************
308800130911  "  C* GESTIONE F12 VIDEO S2
308900130911  "  C************************************************************
309000130911C2226C     F12S2         BEGSR
309100130911     C*
309200130911     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
309300130911     C     $LOP          IFEQ      1
309400130911     C                   MOVE      'D1'          $GEST
309500130911     C*  se la 1a videata è questa, per F12 torno a menu
309600130911     C                   ELSE
309700130911     C                   MOVE      *ON           $FINE
309800130911    >C                   MOVE      '2'           RETL76
309900130911     C                   ENDIF
310000130911     C*
310100130911C2226C                   ENDSR
310200130911  "  C/EJECT
310300130911  "  C************************************************************
310400130911  "  C* GESTIONE F13 VIDEO S1
310500130911  "  C************************************************************
310600130911C2226C     F13S2         BEGSR
310700130911     C*
310800130911     C* E' simile alla funzione F13 del PDM: riporta l'opzione della
310900130911     C* riga del sfl su cui è posizionato il cursore dalla riga
311000130911     C* successiva fino alla fine del sfl stesso,
311100130911     C* senza sovrapporsi alle opzioni preesistenti.
311200130911     C* il controllo lo faccio solo se il sfl NON è vuoto
311300130911   1bC                   If        WMaxS2  <>    0
311400130911     C* determino Riga/Colonna del cursore
311500130911     C                   MOVE      NRG           R$$
311600130911     C                   MOVE      NCL           C$$
311700130911     C                   Z-ADD     RIRI          CURR
311800130911     C                   Z-ADD     COCO          CURC
311900130911     C*
312000130911     C* se sono posizionato sul sfl
312100130911     C* salvo il numero del record su cui sono posizionato
312200130911     c                   Z-Add     C2Nrr         WC2Rcd
312300130911     C* leggo il rcd
312400130911     C     C2NrR         CHAIN     FMTS2                              21
312500130911     C* salvo l'opzione e il numero relativo
312600130911     C                   If        S2Opz   >     0
312700130911     C                   Z-Add     S2Opz         WOpzS2            2 0
312800130911     c                   Z-Add     C2Nrr         $XS2
312900130911     C* attivo SflNxtChg
313000130911     C                   SetOn                                        32
313100130911     C                   UpDate    FmtS2
313200130911     C                   SetOff                                       32
313300130911     C*
313400130911     C* mi posiziono sul record successivo
313500130911     C                   Add       1             $X
313600130911     c*
313700130911     C                   DoW       $X      <=    WMaxS2
313800130911     C     $XS2          Chain     FmtS2                              21
313900130911     C   21              Leave
314000130911     C                   if        S2Opz    =    0
314100130911     C* imposto l'opzione solo se è possibile eseguirla
314200130911     c*
314300130911     C                   Z-Add     WOpzS2        S2Opz
314400130911     C                   endif
314500130911     C*                  endif
314600130911     C* attivo SflNxtChg
314700130911     C                   SetOn                                        32
314800130911     C                   UpDate    FmtS2
314900130911     C                   SetOff                                       32
315000130911     C*
315100130911     C                   Add       1             $XS2
315200130911     C                   enddo
315300130911     c*
315400131129     C                   Eval      WPag2    =    9999
315500130911     C*
315600130911     C                   ExSr      RolS2
315700130911     C* reimposto sflpag
315800131129     C                   Eval      WPag2    =    SflPag
315900130911     C*
316000130911     C* ripristino la posizione iniziale del record
316100130911     c                   Z-Add     WC2Rcd        C2Rcd
316200130911     C* ripristino il campo di salvataggio dell'opzione per non sporca
316300130911     C* re il campo S1Opz in caso di ricaricamento del sfl
316400130911     C                   Z-Add     0             WOpzS2
316500130911     C*
316600130911     C                   endif
316700130911     C*
316800130911     C                   endif
316900130911     C*
317000130911     C*
317100130911C2226C                   ENDSR
317200130911  "  C/EJECT
317300130911  "  C************************************************************
317400130911  "  C* GESTIONE F17 VIDEO S2
317500130911  "  C************************************************************
317600130911C2226C     F17S2         BEGSR
317700130911     C*
317800130911     C                   MOVE      'D1'          $GEST
317900130911     C* se il pgm è stato richiamato con chiave univoca
318000130911     C* e dopo è stata richiesta una parzializzazione
318100130911     C* tolgo la gestione della chiave univoca (azzere/abblenco il
318200130911     C* valore; propongo come ordinamento il 1°)
318300130911     C     $Unique       IFEQ      *ON
318400130911     C                   MOVE      *OFF          $Unique
318500130911    >C***                MOVE      *BLANK        UQ1L76
318600130911     C                   ENDIF
318700130911     C     $ORD          IFEQ      99
318800130911     C                   Z-ADD     1             $ORD
318900130911     C                   MOVE      *ON           $INZD1
319000130911     C                   ENDIF
319100130911     C*
319200130911C2226C                   ENDSR
319300130911  "  C/EJECT
319400130911  "  C************************************************************
319500130911  "  C* GESTIONE F18 VIDEO S2
319600130911  "  C************************************************************
319700130911C2226C     F18S2         BEGSR
319800130911     C*
319900130911     C                   MOVE      'D2'          $GEST
320000130911     C                   MOVEL     *ON           $INZD2
320100130911     C     $ORD          IFEQ      99
320200130911     C                   Z-ADD     1             $ORD
320300130911     C                   ENDIF
320400130911     C*
320500130911C2226C                   ENDSR
320600130911  "  C/EJECT
320700130911  "  C************************************************************
320800130911  "  C* GESTIONE F23 VIDEO S2
320900130911  "  C************************************************************
321000130911C2226C     F23S2         BEGSR
321100130911     C*
321200130911     C                   Z-ADD     $C2Op1        $L01
321300130911     C                   Z-ADD     $C2Op2        $L02
321400130911     C                   EXSR      RIEOPZ
321500130911     C                   Z-ADD     23            X
321600130911     C                   MOVE      FF23          $FC2(X)
321700130911     C                   MOVEL     WOPZ1         C2OPz
321800130911     C                   MOVEL     $ALLOPT       H1ALLOPT
321900130911     C                   MOVEL     WOPZ2         C2OPZ2
322000130911     C                   Z-ADD     WCOZ          C2COZ
322100130911     c* riga unica opzioni
322200130911     c                   eval      $A = %triml(c2opz)
322300130911     c                   eval      $B = %triml(c2opz2)
322400130911     c     $A            cat(P)    $B:2          C2opz1
322500130911     C*
322600130911     C                   ENDSR
322700130911C2226C/EJECT
322800130911  "  C************************************************************
322900130911  "  C* GESTIONE F24 VIDEO S2
323000130911  "  C************************************************************
323100130911C2226C     F24S2         BEGSR
323200130911     C*
323300130911     C                   MOVEA     $FX2          $FX
323400130911     C                   MOVEA     $FC2          $FC
323500130911     C                   MOVEA     $FR2          $FR
323600130911     C                   Z-ADD     $ULKS2        $ULK
323700130911     C                   Z-ADD     $S2L01        $L01
323800130911     C                   Z-ADD     $S2L02        $L02
323900130911     C                   EXSR      RIEKEY
324000130911     C                   Z-ADD     $ULK          $ULKS2            3 0
324100130911     C                   MOVEA     $FC           $FC2
324200130911     C                   MOVEL     $KEY1         Z2KE
324300130911     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
324400130911     C                   MOVEL     $KEY2         Z2KE2
324500130911     c* riga unica tasti
324600130911     c                   eval      $A = %triml(z2ke)
324700130911     c                   eval      $B = %triml(z2ke2)
324800130911     c     $A            cat(P)    $B:0          z2ke1
324900130911     C                   WRITE     FMTZ2
325000130911     C*
325100130911     C                   ENDSR
325200130911C2226C/EJECT
325300130911  "  C************************************************************
325400130911  "  C* CONTROLLO TESTATA PRELISTA
325500130911  "  C************************************************************
325600130911C2226C     CTRC2         BEGSR
325700130911     C*
325800130911     C                   MOVE      *OFF          *IN99
325900130911     C*
326000130911     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
326100130911     C* L'INIZIALIZZAZIONE LISTA
326200130911     C*
326300130911     C     C2POS         IFNE      W2POS
326400130911     C                   EXSR      POSS2
326500130911     C                   ENDIF
326600130911     C*
326700130911     C                   ENDSR
326800130911C2226C/EJECT
326900130911  "  C************************************************************
327000130911  "  C* CONTROLLI PER POSIZIONAMENTO SU PRELISTA
327100130911  "  C************************************************************
327200130911C2226C     POSS2         BEGSR
327300130911     C*
327400130911     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
327500130911     C*
327600130911   B1C     WMAXS2        IFGT      0
327700130911     C                   READC     FMTS2                                  21
327800130911   B2C     *IN21         DOWEQ     *OFF
327900130911     C     *IN99         ANDEQ     *OFF
328000130911     C*
328100130911   B3C     S2OPZ         IFNE      0
328200130911     C                   SETON                                        92  99
328300130911   E3C                   ENDIF
328400130911     C* se rilevata opzione attiva, attivo sflnxtchg
328500130911   B3C     *IN99         IFEQ      *ON
328600130911     C                   MOVE      *ON           *IN32
328700130911     C*
328800130911     C* imposto quegli attributi del sfl che sono modificabili
328900130911     C* al richiamo del determinato pgm
329000130911     C                   EXSR      ATRS2
329100130911     C*
329200130911     C                   UPDATE    FMTS2
329300130911   X3C                   ELSE
329400130911     C                   READC     FMTS2                                  21
329500130911   E3C                   ENDIF
329600130911     C*
329700130911   E2C                   ENDDO
329800130911     C*
329900130911   E1C                   ENDIF
330000130911     C*
330100130911     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
330200130911     C* AL VALORE DI POSIZIONAMENTO :
330300130911     C* se questo è maggiore del valore del rispettivo campo di
330400130911     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
330500130911     C* con i valori di parzializzazione.
330600130911     C*
330700130911   B1C     *IN99         IFEQ      *OFF
330800130911    >C                   MOVE      *BLANK        W2POS
330900130911     C*
331000130911     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
331100130911     C* è maggiore del relativo valore di parzializzazione, nel qual caso
331200130911     C* imposto la variabile del posizionamento (W2Pos) al nuovo valore;
331300130911     C* richiedo comunque un ricaricamento del sfl che, se non si è
331400130911     C* verificata la condizione suddetta avverrà con posizionamento a
331500130911     C* partire dal valore di parzializzazione
331600130911     C*
331700130911    >C     C2POS         IFGT      *blanks
331800130911    >C                   MOVEL     C2POS         W2POS
331900130911    >C                   CALLB     'XALLINEA'
332000130911    >C                   PARM                    W2Pos
332100130911    >C                   PARM      8             LEN               5 0
332200130911    >C                   PARM                    XSCINP
332300130911     C                   ENDIF
332400130911     C*
332500130911     C* richiedo l'inizializzazione sfl
332600130911     C                   MOVE      *ON           $INZS2
332700130911     C*
332800130911   E1C                   ENDIF
332900130911     C*
333000130911     C                   ENDSR
333100130911C2226C/EJECT
333200130911  "  C************************************************************
333300130911  "  C* CONTROLLO OPZIONI LISTA
333400130911  "  C************************************************************
333500130911C2226C     CTRS2         BEGSR
333600130911     C*
333700130911     C                   MOVEL     *OFF          $ESCI
333800130911     C                   SETOFF                                       99
333900130911     C                   Z-ADD     0             S2OPZ
334000130911     C*
334100130911     C* Leggo il sfl solo se ci sono rcd
334200130911   B1C     WMAXS2        IFGT      0
334300130911     C                   READC     FMTS2                                  21
334400130911     C*
334500130911     C* esce se fine sfl o errore che richiede l'uscita
334600130911   B2C     *IN21         DOWEQ     *OFF
334700130911     C     $ESCI         ANDEQ     *OFF
334800130911     C                   Z-ADD     S2NRR         C2RCD
334900130911     C* ctrl su riga
335000130911     C                   EXSR      RECS2
335100130911     C* gestione opzioni
335200130911   B3C     S2OPZ         IFNE      0
335300130911     C     *IN99         ANDEQ     *OFF
335400130911     C                   EXSR      OPZS2
335500130911   E3C                   ENDIF
335600130911     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
335700130911   B3C     *IN99         IFEQ      *ON
335800130911     C                   MOVE      *ON           *IN32
335900130911   E3C                   ENDIF
336000130911     C*
336100130911     C* imposto quegli attributi del sfl che sono modificabili
336200130911     C* al richiamo del determinato pgm
336300130911     C                   EXSR      ATRS2
336400130911     C*
336500130911     C                   Z-ADD     0             S2OPZ
336600130911     C*
336700130911     C                   UPDATE    FMTS2
336800130911     C                   MOVE      *OFF          *IN32
336900130911     C*
337000130911     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
337100130911   B3C     $ESCI         IFEQ      *OFF
337200130911     C                   READC     FMTS2                                  21
337300130911     C* a fine ciclo ripristino il flag richiesta iniz.sfl
337400130911   B4C     *IN21         IFEQ      *ON
337500130911     C                   MOVE      WINZS2        $INZS2
337600130911     C                   MOVE      *off          WINZS2
337700130911     C* se è avvenuta almeno una modifica
337800130911   B5C                   IF        $INZS2 = *on
337900130911     C* calcolo pagina a cui deve posizionarsi
338000130911     C     WSFL2         IFGT      0
338100130911     C                   EXSR      CLCPAG
338200130911     C                   ELSE
338300130911     C                   Z-ADD     SFLPAG        WPAG2
338400130911     C                   ENDIF
338500130911   E5C                   ENDIF
338600130911   E4C                   ENDIF
338700130911   E3C                   ENDIF
338800130911     C*
338900130911   E2C                   ENDDO
339000130911     C*
339100130911   E1C                   ENDIF
339200130911     C*
339300130911     C                   ENDSR
339400130911C2226C/EJECT
339500130911  "  C************************************************************
339600130911  "  C* CONTROLLO CAMPI I/O RIGA PRELISTA
339700130911  "  C************************************************************
339800130911C2226C     RECS2         BEGSR
339900130911     C*
340000130911     C* reset indicatori DSPATR
340100130911     C                   MOVE      *ALL'0'       IN4049           10
340200130911     C                   MOVEA     IN4049        *IN(40)
340300130911     C*
340400130911     C* Controllo se Utente Autorizzato alla scelta
340500130911     C     S2OPZ         IFNE      0
340600130911     C                   Z-ADD     1             X
340700130912     C     S2OPZ         LOOKUP    $OP2(X)                                21
340800130911     C* mi salvo l'indice dell' opzione
340900130911     C   21              Z-add     X             PosOpz            2 0
341000130911     C     *IN21         IFEQ      *OFF
341100130912     C     $FL2(X)       OREQ      'N'
341200130911     C                   MOVE      *ON           $ESCI
341300130911     C                   SETON                                        409199
341400130911     C                   ENDIF
341500130911     C                   ENDIF
341600130911     C*
341700130911     C*
341800130911     C                   ENDSR
341900130911C2226C/EJECT
342000130911  "  C************************************************************
342100130911  "  C* GESTIONE OPZIONI LISTA
342200130911  "  C************************************************************
342300130911C2226C     OPZS2         BEGSR
342400130911     C*
342500130911     C                   Z-ADD     RIRI          CURR
342600130911     C                   Z-ADD     COCO          CURC
342700130911     C*
342800130911     C                   SELECT
342900130911     C* se opz=Lista
343000130911     C* Proseguo con il pgm di lista
343100130911     C     S2OPZ         WHENEQ    1
343200130911     C                   MOVE      'S1'          $GEST
343300130911     C                   MOVE      *ON           $INZS1
343400130911     C                   MOVE      *BLANK        W1POS
343500130911     C                   EXSR      GESS1
343600130911     C*
343700130911     C                   MOVE      *HIVAL        $LASTV
343800130911     C*
343900130911     C* se altre opzioni
344000130911     C                   OTHER
344100130911     C*
344200130911     C                   ENDSL
344300130911     C*
344400130911     C                   ENDSR
344500101209C2176 ************************************************************
344600110412  "   * Aggiornam. x chiusura fasi (tutte le fasi aperte della partita)
344700101209  "   ************************************************************
344800101209C2176C     AggRca        Begsr
344900110412D2680C* emetto richiesta data chiusura
345000130924C2226C                   If        (PrimaVolta = 0 and
345100130924  "  C                             $Tastosav = F13) or
345200130924C2226C                             $Tastosav <> F13
345300131129D2680c                   eval      D5DtChiu   =udate
345400131129C2226c                   eval      D5DtPar    =S1DtPar
345500130924  "  c                   eval      D5NrPar    =S1NrPar
345600131129C2226c                   eval      D5SerPar   =S1SerPar
345700130923D2680c                   clear                   WWMSG
345800130923C2226c                   clear                   D5Nota
345900130923D2680c                   eval      Idmsg= Msg(1)
346000110412  "  c                   exsr      PrendiMsg
346100130924D2680c                   eval      Z5Key=txtmsg
346200130924C2226c                   eval      Idmsg= Msg(2)
346300130924  "  c                   exsr      PrendiMsg
346400130924C2226c                   eval      Z5Key=%trim(Z5Key) + ' ' +%trim(Txtmsg)
346500130924D2680c     emetti        tag
346600131129D2680c                   exfmt     Fmtd5
346700130924C2226C                   Eval      Primavolta = 1
346800130923D2680c                   if        $tasto=F12
346900130924C2226C                   If        $tastosav = F13
347000131129  "  C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
347100131129  "  c                   move      *on           Winzs1
347200130924  "  C                   Eval      $esci = *on
347300130924C2226C                   Endif
347400130924D2680c                   leavesr
347500110412  "  c                   endif
347600110412  "  C                   if        D5DtChiu <>   *Zeros
347700110412  "  C     *JobRun       Test(d)                 D5DtChiu               21
347800110412  " >C     *In21         ifeq      *On
347900110412  "  c* non corretta
348000110412  " >C                   SetOn                                        6699
348100110412  "  c                   goto      emetti
348200110412  " >C                   endif
348300110412  "  c                   else
348400110412  "  c* non immessa
348500110412  " >C                   SetOn                                        6699
348600110412  "  c                   goto      emetti
348700110412D2680C                   endif
348800131129C2226C                   If        $tasto <> F06
348900130924  "  C                   goto      emetti
349000130924  "  C                   Else
349100130924  "  C                   Eval      $tastoSav2 = F06
349200130924  "  C                   ENdif
349300131129  "  C                   ENdif
349400130924C2226C                   If        $tastosav2 = F06
349500101209     C                   eval      Rcasocieta = xscsoc
349600101209     C                   eval      RcaPosiz   = posattr057
349700101209     C                   eval      Rcakcc     = h1kcc
349800101209     C                   eval      rcaksc     = s1ksc
349900101209     C     *jobrun       move      s1dtpar       RcaDtPar
350000101209     C                   eval      rcanrpar   = s1nrpar
350100101209     C                   eval      rcaserpar  = s1serpar
350200130924     C*
350300101209    >C     K07Rca04      Setll     NDRCA04L
350400101209     c                   do        *hival
350500101209    >C     K07Rca04      reade     NDRCA04                              2221
350600101209     c   21              leave
350700101209     C*
350800101209     C* Gestione RCD allocato
3509001012093    C     *IN22         IFEQ      *ON
351000101209    >C                   MOVEL     'NDRCA00F'    MSGFIL
351100101209     C                   EXSR      MSGPGM
351200101209     c                   if        Msgrtn='C'
351300101209     c                   leave
351400101209     c                   else
351500101209     c* mi riposiziono per riprovare con la stessa scadenza
351600101209    >C     K08Rca04      Setll     NDRCA04L
351700101209     c                   iter
351800101209     c                   endif
3519001012093-   C                   ENDIF
352000101209     c*
352100101210     c* non chiude le fasi impegno
352200101210     c                   if        RcaAzione='98'or
352300101210     c                             RcaAzione='99'
352400101210     c                   iter
352500101210     c                   endif
352600101210     c*
352700101209     C                   if        *in21=*off
352800101209     c                             and RcaDtChiu=*loval
352900110412D2680x*************      eval      RcaDtChiu=Zdateiso
353000110412D2680c     *jobrun       move      D5DtChiu      RcaDtChiu
353100101209     c                   eval      RcaUteChiu=sdsusr
353200101209     c                   eval      RcaStato='1'
353300130913C2226C                   If        D5Nota <> *blanks
353400130913  "  C                   eval      RcaNota = D5Nota
353500130913C2226C                   Endif
353600101209     c                   update    NdRCA04
353700101209     C                   move      *ON           $INZS1                         iniz.sfl fine ciclo
353800101209     c                   move      *on           Winzs1
353900101209     c                   ENDIF
354000101209     c*
354100101209     c                   enddo
354200130924C2226c                   ENDIF
354300101209     c*
354400101209c2176c                   endsr
354500940223     C/EJECT
354600101209c2176C******************************************************
354700101209  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
354800101209  "  C******************************************************
354900101209c2176C     MSGPGM        BEGSR
355000101209     C*
355100101209     C                   Z-ADD     2             MSGMSG
355200101209     C                   MOVEL     'S'           MSGRSP
355300101209     C                   MOVEL     'CR'          MSGTPR
355400101209     C                   MOVEL     'S'           MSGEMV
355500101209     C                   Z-ADD     10            MSGRGP
355600101209     C                   MOVEL     'S'           MSGLCK
355700101209     C                   MOVEA     STATUS        STS
355800101209     C                   MOVEL     'N'           MSGCNL
355900101209     C                   MOVEL     'N'           MSGVID
356000101209     C                   MOVEL     'N'           MSGOPE
356100101209     C                   MOVEL     'N'           MSGSTP
356200101209     C                   CALL      'XMSGR'
356300101209     C                   PARM                    XMSGDS
356400101209     C*
356500101209c2176C                   ENDSR
356600101209     C/EJECT
356700100426c2156C************************************************************
356800100426     C* GESTIONE RITORNO DA PGM DI GESTIONE
356900100426     C************************************************************
357000100426c2156C     RTNL77        BEGSR
357100100426     C*
357200100426     C* modo di ritorno
357300100426     C*
357400100426     C                   SELECT
357500100426     C* questi modi di utilizzo dei valori di ritorno dal
357600100426     C* pgm di manutenzione rcd di anagrafica sono delle
357700100426     C* proposte, normalmente sempre valide, ma modificabili
357800100426     C* per situazioni particolari
357900100426     C* 1 = F3
358000100426    >C     RETL77        WHENEQ    '1'
358100100426     C                   MOVE      *ON           $ESCI                          sospese READC
358200100426     C                   MOVE      *ON           $FINE                          fine pgm
358300100426    >C                   MOVE      '1'           RETL76
358400100426     C* 2 = F12
358500100426    >C     RETL77        WHENEQ    '2'
358600100426     C* se il pgm di manutenzione ha prima una guida, è necessario anche
358700100426     c* questo test, altrimenti prevale il ret002 e il sfl non viene
358800100426     C* ricaricato
358900100426    >C     OPRL77        ANDNE     '1'
359000100426     C                   MOVE      *ON           $ESCI                          sospese READC
359100100426     C*
359200100426     C                   ENDSL
359300100426     C*
359400100426     C* operazione eseguite dal pgm chiamato
359500100426     C*
359600100426     C                   SELECT
359700100426     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
359800100426    >C     OPRL77        WHENEQ    '1'
359900100426     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
360000100426     C*
360100100426     C                   ENDSL
360200100426     C*
360300100426     C* funzione non eseguibile per errore :
360400100426     C*
360500100426     C                   ENDSR
360600100426     C/EJECT
360700950301     C************************************************************
360800950301     C* AGGIORNAMENTO RIGA LISTA
360900950301     C************************************************************
361000950301     C     AGGS1         BEGSR
361100950301     C*
361200950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
361300950301     C* di manutenzione dopo una opzione 02
361400950301     C*
361500950301     C*
361600950301     C                   ENDSR
361700941124     C/EJECT
361800940131     C************************************************************
361900940131     C* GESTIONE VALORI ASSUNTI
362000940131     C************************************************************
362100940131     C     GESD2         BEGSR
362200940131     C*
362300941118   B1C     $INZD2        IFEQ      *ON
362400940131     C                   EXSR      INZD2
362500940223     C                   MOVE      *OFF          $INZD2
362600941118   E1C                   ENDIF
362700940202     C*
362800941118   B1C     $LASTV        IFNE      'D2'
362900940923     C                   EXSR      GESPIE
363000961023   B1C     $LASTV        IFNE      *HIVAL
363100940202     C                   MOVE      $LASTV        $LASTG
363200961023     C                   ENDIF
363300940202     C                   MOVE      'D2'          $LASTV
363400941118   E1C                   ENDIF
363500940131     C*
363600940707     C* se esistono errori sulla videata
363700940707     C* emetto la write del formato a indicatori spenti per vedere
363800940707     C* le eventuali decodifiche
363900941118   B1C     *IN99         IFEQ      *ON
364000940707     C                   MOVEA     *IN           WIN
364100940707     C                   MOVE      *ALL'0'       IN5098           49
364200940707     C                   MOVEA     IN5098        *IN(50)
364300940707     C                   WRITE     FMTD2
364400940707     C                   MOVEA     WIN           *IN
364500941118   E1C                   ENDIF
364600940707     C*
364700940131     C                   EXFMT     FMTD2
364800960605     C*
364900960605     C* azzero la posizione del cursore
365000960605     C                   Z-ADD     0             H1RIGA
365100960605     C                   Z-ADD     0             H1COLO
365200941205     C*
365300951005     C* CONTROLLO ABILITAZIONE TASTI
365400951005     C* INPUT
365500951005     C* $FC3 = Abilitazioni videata D2
365600951005     C* OUTPUT
365700951005     C* *IN97 + *IN99 Errori
365800951005     C                   MOVEA     $FC3          $FC
365900951005     C                   EXSR      CTRKEY
366000941205     C*
366100941205   B1C     *IN99         IFEQ      *OFF
366200941205     C*
366300941205   B2C                   SELECT
366400090324     C* F2=Decodifica
366500090324     C     $TASTO        WHENEQ    F02
366600090324     C                   EXSR      F02D2
366700940131     C* F3=Fine
366800940506     C     $TASTO        WHENEQ    F03
366900940309     C                   EXSR      F03D2
367000090324     C* F4=Ricerca
367100090324     C     $TASTO        WHENEQ    F04
367200090324     C                   EXSR      SEARCH
367300940131     C* F12=Ritorno
367400940506     C     $TASTO        WHENEQ    F12
367500940309     C                   EXSR      F12D2
367600940916     C* F24=Altri tasti
367700940916     C     $TASTO        WHENEQ    F24
367800940916     C                   EXSR      F24D2
367900940131     C*
368000940131     C                   OTHER
368100940131     C                   EXSR      CTRD2
368200941205   B3C     *IN99         IFEQ      *OFF
368300940211     C                   MOVE      $LASTG        $GEST
368400941205   B4C                   SELECT
368500940202     C     $LASTG        WHENEQ    'S1'
368600940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
368700941205   B5C     $ORD          IFNE      D2ORD
368800940223     C                   MOVE      *ON           $INZS1
368900940302     C                   MOVE      *BLANK        W1POS
369000941205   E5C                   ENDIF
369100130912C2226C     $LASTG        WHENEQ    'S2'
369200130912  "  C* se è stato modificato il raggruppamento ricarico prelista
369300130912  "  C     $RGRPRE       IFNE      D2RGRPRE
369400130912  "  C                   MOVE      *ON           $INZS2
369500130912  "  C                   MOVE      *BLANK        W2POS
369600130912C2226C                   ENDIF
369700940202     C     $LASTG        WHENEQ    'D1'
369800940318     C* per ora niente
369900941205   E4C                   ENDSL
370000940131     C* definisco i nuovi valori di default
370100940201     C                   Z-ADD     D2LOP         $LOP
370200940131     C                   Z-ADD     D2ORD         $ORD
370300130912C2226C                   If        D2Pre = XscOn
370400130912  "  C                   Movel     *on           $Pre
370500130910  "  C                   Else
370600130912  "  C                   Movel     *off          $Pre
370700130912  "  C                   Endif
370800130912C2226C                   Movel     D2RgrPre      $RgrPre
370900950322     C*
371000950322     C* aggiorno valori assunti
371100950328     C* se richiesto il salvataggio
371200950328   B4C     D2BSV         IFEQ      1
371300950322     C                   EXSR      AGGMPE
371400950322   E4C                   ENDIF
371500950322     C*
371600941205   E3C                   ENDIF
371700940131     C*
371800941205   E2C                   ENDSL
371900940131     C*
372000941205   E1C                   ENDIF
372100941205     C*
372200940131     C                   ENDSR
372300940223     C/EJECT
372400940131     C************************************************************
372500940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
372600940131     C************************************************************
372700940201     C     INZD2         BEGSR
372800940131     C*
372900940131     C                   CLEAR                   FMTD2
373000130910C2226C                   EVAL      D2On1 = Xscon
373100130912C2226C                   EVAL      D2Off1= Xscoff
373200021010     C                   EVAL      D2BSVA = Normale
373300950328     C* se sono entrato con chiave univoca
373400950328     C* impedisco il salvataggio valori assunti
373500950606     C     $Unique       IFEQ      *ON
373600021010     C                   EVAL      D2BSVA = Protetto
373700950328     C                   ENDIF
373800940201     C                   MOVEL     $LOP          D2LOP
373900940831     C                   MOVE      $ORD          D2ORD
374000130924C2226C                   If        $pre = ' '
374100130924  "  C                   Movel     *on           $pre
374200130924  "  C                   Movel     'P'           $RgrPre
374300130924  "  C                   Endif
374400130924  "  C                   If        $Pre = *on
374500130912  "  C                   Movel     XscOn         D2Pre
374600130910  "  C                   Else
374700130912  "  C                   Movel     XscOff        D2Pre
374800130920  "  C                   Endif
374900130924C2226C                   Movel     $RgrPre       D2RgrPre
375000090324     c* decodifica
375100090324     C                   EXSR      CTRD2
375200090324     C*
375300090324     C                   MOVE      *ALL'0'       IN5098
375400090324     C                   MOVEA     IN5098        *IN(50)
375500090324     C*
375600940131     C*
375700940131     C                   ENDSR
375800940309     C/EJECT
375900090324     C************************************************************
376000090324     C* GESTIONE F02 VIDEO D1
376100090324     C************************************************************
376200090324     C     F02D2         BEGSR
376300090324     C*
376400090324     C* per decodificare la videata richiamo la routine dei ctrl
376500090324     C* e poi disattivo gli eventuali messaggi di errore
376600090324     C                   EXSR      CTRD2
376700090324     C*
376800090324     C                   MOVE      *ALL'0'       IN5098
376900090324     C                   MOVEA     IN5098        *IN(50)
377000090324     C*
377100090324     C                   ENDSR
377200090324     C/EJECT
377300940309     C************************************************************
377400940309     C* GESTIONE F03 VIDEO D2
377500940309     C************************************************************
377600940309     C     F03D2         BEGSR
377700940309     C*
377800940309     C                   MOVE      *ON           $FINE
377900940325     C* fine programma
378000940325     C                   EXSR      ENDPGM
378100940309     C*
378200940309     C                   ENDSR
378300940309     C/EJECT
378400940309     C************************************************************
378500940309     C* GESTIONE F12 VIDEO D2
378600940309     C************************************************************
378700940309     C     F12D2         BEGSR
378800940309     C*
378900940309     C                   MOVE      $LASTG        $GEST
379000940309     C*
379100940309     C                   ENDSR
379200940916     C/EJECT
379300940916     C************************************************************
379400940916     C* GESTIONE F24 VIDEO D2
379500940916     C************************************************************
379600940916     C     F24D2         BEGSR
379700940916     C*
379800940916     C                   MOVEA     $FX3          $FX
379900940916     C                   MOVEA     $FC3          $FC
380000940916     C                   MOVEA     $FR3          $FR
380100940916     C                   Z-ADD     $ULKD2        $ULK
380200950726     C                   Z-ADD     $D2L01        $L01
380300950726     C                   Z-ADD     $D2L02        $L02
380400940916     C                   EXSR      RIEKEY
380500940916     C                   Z-ADD     $ULK          $ULKD2            3 0
380600940916     C                   MOVEA     $FC           $FC3
380700940916     C                   MOVEL     $KEY1         Z3KE1
380800060224     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
380900090324     C                   MOVEL     $KEY2         Z3KE2
381000940916     C                   WRITE     FMTZ3
381100940916     C*
381200940916     C                   ENDSR
381300940223     C/EJECT
381400940131     C************************************************************
381500940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
381600940131     C************************************************************
381700940131     C     CTRD2         BEGSR
381800940131     C*
381900940131     C                   SETOFF                                       99
382000940131     C*
382100940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
382200940201     C     D2LOP         IFEQ      0
382300940131     C                   SETON                                          5099
382400941027     C                   ENDIF
382500940131     C*
382600940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
382700940131     C     D2ORD         IFEQ      0
382800940131     C                   SETON                                          5199
382900941027     C                   ENDIF
383000090324     c* Controllo ordinamento
383100090324     C                   RESET                   X07DS
383200090324     C                   MOVE      '1'           X07RIC
383300090324     C                   MOVEL     'ANDIZ000'    X07TRC
383400100401     C                   MOVEL     'DSPORDRCA'   X07CAM
383500090324     C                   MOVEL     D2ORD         X07VAL
383600090324     C                   MOVE      *OFF          X07ALL
383700090324     C                   CALLB     'X07VALR'
383800090324     C                   PARM                    X07DS
383900090324   B3C     X07ERR        IFEQ      '0'
384000090324     C                   MOVEL     X07DES        D2ORDD
384100090324     c                   else
384200090324     C                   SETON                                        5299
384300090324     C                   ENDIF
384400130910C2226C* CONTROLLO FLAG PRELISTA
384500130912  "  c                   if        D2Pre <> XscOn and
384600130912  "  c                             D2Pre <> XscOff
384700130910  "  c                   seton                                        5399
384800130912  "  c                   endif
384900130912  "  C* CONTROLLO FLAG Raggruppamento obbligatorio
385000130912  "  c                   if        D2RgrPre = ' ' and
385100130912  "  c                             D2Pre = XscOn
385200130912  "  c                   seton                                        5499
385300130912  "  c                   endif
385400130912  "  c* Controllo values raggruppamento
385500130912  "  C                   RESET                   X07DS
385600130912  "  C                   MOVE      '1'           X07RIC
385700130912  "  C                   MOVEL     'ANDIZ000'    X07TRC
385800130912  "  C                   MOVEL     'DSPRGRSCL'   X07CAM
385900130912  "  C                   MOVEL     D2RGRPre      X07VAL
386000130912  "  C                   MOVE      *OFF          X07ALL
386100130912  "  C                   CALLB     'X07VALR'
386200130912  "  C                   PARM                    X07DS
386300130912  "  C     X07ERR        IFEQ      '0'
386400130912  "  C                   MOVEL     X07DES        D2RgrPreD
386500130912  "  c                   else
386600130912  "  C                   SETON                                        5599
386700130912C2226C                   ENDIF
386800940131     C*
386900940131     C                   ENDSR
387000941205     C/EJECT
387100941205     C************************************************************
387200941205     C* CONTROLLO ABILITAZIONE TASTI
387300941205     C* INPUT
387400941205     C* $TASTO =Tasto funzione premuto
387500941205     C* $FC = Tasti abilitati
387600941205     C* OUTPUT
387700941205     C* IN99 = ERrore
387800941205     C************************************************************
387900941205     C     CTRKEY        BEGSR
388000941205     C*
388100951005     C                   SETOFF                                       99
388200951005     C*
388300941205     C                   SELECT
388400941205     C     $TASTO        WHENEQ    F01
388500960202     C                   z-add     01            posfun
388600941205     C     $FC(1)        IFEQ      '0'
388700951005     C                   SETON                                        9997
388800941205     C                   ENDIF
388900941205     C     $TASTO        WHENEQ    F02
389000960202     C                   z-add     02            posfun
389100941205     C     $FC(2)        IFEQ      '0'
389200951005     C                   SETON                                        9997
389300941205     C                   ENDIF
389400941205     C     $TASTO        WHENEQ    F03
389500960202     C                   z-add     03            posfun
389600941205     C     $FC(3)        IFEQ      '0'
389700951005     C                   SETON                                        9997
389800941205     C                   ENDIF
389900941205     C     $TASTO        WHENEQ    F04
390000960202     C                   z-add     04            posfun
390100941205     C     $FC(4)        IFEQ      '0'
390200951005     C                   SETON                                        9997
390300941205     C                   ENDIF
390400941205     C     $TASTO        WHENEQ    F05
390500960202     C                   z-add     05            posfun
390600941205     C     $FC(5)        IFEQ      '0'
390700951005     C                   SETON                                        9997
390800941205     C                   ENDIF
390900941205     C     $TASTO        WHENEQ    F06
391000960202     C                   z-add     06            posfun
391100941205     C     $FC(6)        IFEQ      '0'
391200951005     C                   SETON                                        9997
391300941205     C                   ENDIF
391400941205     C     $TASTO        WHENEQ    F07
391500960202     C                   z-add     07            posfun
391600941205     C     $FC(7)        IFEQ      '0'
391700951005     C                   SETON                                        9997
391800941205     C                   ENDIF
391900941205     C     $TASTO        WHENEQ    F08
392000960202     C                   z-add     08            posfun
392100941205     C     $FC(8)        IFEQ      '0'
392200951005     C                   SETON                                        9997
392300941205     C                   ENDIF
392400941205     C     $TASTO        WHENEQ    F09
392500960202     C                   z-add     09            posfun
392600941205     C     $FC(09)       IFEQ      '0'
392700951005     C                   SETON                                        9997
392800941205     C                   ENDIF
392900941205     C     $TASTO        WHENEQ    F10
393000960202     C                   z-add     10            posfun
393100941205     C     $FC(10)       IFEQ      '0'
393200951005     C                   SETON                                        9997
393300941205     C                   ENDIF
393400941205     C     $TASTO        WHENEQ    F11
393500960202     C                   z-add     11            posfun
393600941205     C     $FC(11)       IFEQ      '0'
393700951005     C                   SETON                                        9997
393800941205     C                   ENDIF
393900941205     C     $TASTO        WHENEQ    F12
394000960202     C                   z-add     12            posfun
394100941205     C     $FC(12)       IFEQ      '0'
394200951005     C                   SETON                                        9997
394300941205     C                   ENDIF
394400941205     C     $TASTO        WHENEQ    F13
394500960202     C                   z-add     13            posfun
394600941205     C     $FC(13)       IFEQ      '0'
394700951005     C                   SETON                                        9997
394800941205     C                   ENDIF
394900941205     C     $TASTO        WHENEQ    F14
395000960202     C                   z-add     14            posfun
395100941205     C     $FC(14)       IFEQ      '0'
395200951005     C                   SETON                                        9997
395300941205     C                   ENDIF
395400941205     C     $TASTO        WHENEQ    F15
395500960202     C                   z-add     15            posfun
395600941205     C     $FC(15)       IFEQ      '0'
395700951005     C                   SETON                                        9997
395800941205     C                   ENDIF
395900941205     C     $TASTO        WHENEQ    F16
396000960202     C                   z-add     16            posfun
396100941205     C     $FC(16)       IFEQ      '0'
396200951005     C                   SETON                                        9997
396300941205     C                   ENDIF
396400941205     C     $TASTO        WHENEQ    F17
396500960202     C                   z-add     17            posfun
396600941205     C     $FC(17)       IFEQ      '0'
396700951005     C                   SETON                                        9997
396800941205     C                   ENDIF
396900941205     C     $TASTO        WHENEQ    F18
397000960202     C                   z-add     18            posfun
397100941205     C     $FC(18)       IFEQ      '0'
397200951005     C                   SETON                                        9997
397300941205     C                   ENDIF
397400941205     C     $TASTO        WHENEQ    F19
397500960202     C                   z-add     19            posfun
397600941205     C     $FC(19)       IFEQ      '0'
397700951005     C                   SETON                                        9997
397800941205     C                   ENDIF
397900941205     C     $TASTO        WHENEQ    F20
398000960202     C                   z-add     20            posfun
398100941205     C     $FC(20)       IFEQ      '0'
398200951005     C                   SETON                                        9997
398300941205     C                   ENDIF
398400941205     C     $TASTO        WHENEQ    F21
398500960202     C                   z-add     21            posfun
398600941205     C     $FC(21)       IFEQ      '0'
398700951005     C                   SETON                                        9997
398800941205     C                   ENDIF
398900941205     C     $TASTO        WHENEQ    F22
399000960202     C                   z-add     22            posfun
399100941205     C     $FC(22)       IFEQ      '0'
399200951005     C                   SETON                                        9997
399300941205     C                   ENDIF
399400941205     C     $TASTO        WHENEQ    F23
399500960202     C                   z-add     23            posfun
399600941205     C     $FC(23)       IFEQ      '0'
399700951005     C                   SETON                                        9997
399800941205     C                   ENDIF
399900941205     C     $TASTO        WHENEQ    F24
400000960202     C                   z-add     24            posfun
400100941205     C     $FC(24)       IFEQ      '0'
400200951005     C                   SETON                                        9997
400300941205     C                   ENDIF
400400941205     C                   ENDSL
400500941205     C*
400600941205     C                   ENDSR
400700940223     C/EJECT
400800940201     C************************************************************
400900940201     C* AGGIORNAMENTO VALORI ASSUNTI
401000940201     C************************************************************
401100940201     C     AGGMPE        BEGSR
401200940201     C*
401300950524     C* preparo i parametri di chiamata al driver
401400950524     C                   CLEAR                   XMPEDS
401500950524     C                   EVAL      TpChiamata = '03'
401600950524     C                   EVAL      NChiavi = 5
401700950524     C                   EVAL      VLogica = 2
401800950524     C*
401900950524     C* preparo le chiavi per la chiamata al driver
402000950524     C                   CLEAR                   XMPEDS1
402100950524     C                   MOVEL     XSCSOC        DSMPESOC
402200950524     C                   MOVEL     DSPGM         DSMPEPGM
402300950524     C                   MOVEL     KNMUS         DSMPEPRF
402400941013     C*
402500950322     C* Si memorizzano i dati di default a seconda delle modalità con
402600950322     C* cui il pgm è usato
402700950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
402800950322     C* MPETIP = 'PV'  pgm richiamato da menù
402900950322     C* MPETIP = 'PB'  x lancio Batch
403000950322     C*
403100941013     C                   SELECT
403200090323    >C     OPZL76        WHENEQ    '01'
403300950524     C                   MOVEL     'PX'          DSMPETIP
403400941013     C                   OTHER
403500950524     C                   MOVEL     'PV'          DSMPETIP
403600941013     C                   ENDSL
403700950524     C* dati
403800950524     C                   MOVEL     DSMPE         DSMPEPAR
403900950524     C*
404000950524     C                   CALLB     'XMPE'
404100950524     C                   PARM                    XMPEDS
404200950524     C                   PARM                    XMPEDS1
404300940201     C*
404400940201     C                   ENDSR
404500940223     C/EJECT
404600940131     C************************************************************
404700940131     C* OPERAZIONI INIZIALI
404800940131     C************************************************************
404900940131     C     *INZSR        BEGSR
405000070711     C     *DMY          Move      Udate         ZDateISO
405100940131     C*
405200940127     C* Reperimento parametri
405300940127     C*
405400940117     C     *ENTRY        PLIST
405500940117     C                   PARM                    KPJBA
405600950410     C                   PARM                    DSAUT
405700950410     C*
405800951010     C* testo se sono stati passati i dati di autorizzazione
405900951010     C*
406000950410     C     PARMS         IFLT      2
406100950410     C                   MOVE      *BLANK        AUTDS
406200950410     C                   ELSE
406300950410     C                   MOVEL     DSAUT         AUTDS
406400950410     C                   ENDIF
406500951206     C*
406600951206     C* Reperimento dati società
406700951206     C*
406800970424     C                   MOVEL     'SOC001'      TIPXSC
406900970424     C                   MOVEL     *BLANK        SOCXSC
407000951206     C                   EXSR      REPSOC
407100960409     C*
407200960409     C* Controlli dati società
407300960409     C*
407400960409     C     RTNXSC        IFNE      '1'
407500970424     C                   MOVEL     XSOCDS        SOC001
407600960409     C                   ELSE
407700960409     C                   MOVE      *ON           $FINE
407800090323    >C                   MOVE      '1'           ErrL76
407900960409     C                   EXSR      ENDPGM
408000960409     C                   ENDIF
408100070725     c*
408200090323     c                   open      ndcL76d                              13
408300070725     c* emetto messaggio per video non a 132
408400070725     c                   if        *in13=*on
408500070725     c                   move      'COS2651'     XmsgCd1
408600070725     C                   CALL      'XMSGR'
408700070725     C                   PARM                    XMSGDS
408800070725     C                   PARM                    XMStxt          135
408900070725     C                   PARM                    XMSGinz           1
409000070725     C                   PARM                    XMSGCd1           7
409100070725     C                   PARM                    XMSGCd2           7
409200070725     C                   PARM                    XMSGCd3           7
409300070725     C                   PARM                    XMSGSn1           7
409400070725     C                   PARM                    XMSGCdTxT        63
409500070725     C                   PARM                    MSGCdTit         14
409600070725     C                   PARM                    MSGClPrtf         1
409700070725     C                   PARM                    VarMsg1         100
409800070725     C                   PARM                    VarMsg2         100
409900070725     C                   PARM                    VarMsg3         100
410000090323    >C                   MOVE      '1'           ErrL76
410100070725     c                   exsr      endpgm
410200070725     c                   endif
410300950726     C*
410400950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
410500070618     C                   EVAL      $D1L01 = %Size(Z1Ke)
410600950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
410700070618     C                   EVAL      $S1L01 = %Size(Z2Ke)
410800950726     C                   EVAL      $S1L02 = %Size(Z2Ke2)
410900070531     C                   EVAL      $C1Op1 = %Size(C1Opz)
411000950926     C                   EVAL      $C1Op2 = %Size(C1Opz2)
411100950906     C                   EVAL      $D2L01 = %Size(Z3Ke1)
411200090324     C                   EVAL      $D2L02 = %size(z3ke2)
411300130911C2226C                   EVAL      $S2L01 = %Size(Z2Ke)
411400130911  "  C                   EVAL      $S2L02 = %Size(Z2Ke2)
411500130911  "  C                   EVAL      $C2Op1 = %Size(C2Opz)
411600130911C2226C                   EVAL      $C2Op2 = %Size(C2Opz2)
411700940225     C*
411800940223     C* Reperimento opzioni
411900940127     C*
412000940128     C                   EXSR      REPOPZ
412100950406     C*
412200940506     C* Reperimento tasti di funzione
412300940506     C*
412400940506     C                   EXSR      REPKEY
412500960117     C* Chiamata a routine per il reperimento delle opzioni
412600960117     C* personalizzate
412700960117     C                   CAllb     'X46USREXT'
412800960117     C                   PARM                    $OP1
412900960117     C                   PARM                    $TX1
413000960117     C                   PARM                    $FL1
413100960117     C                   PARM                    $PGO1
413200960117     C                   PARM      $dimOp        NumOpz
413300960202     C                   PARM                    $FX2
413400960202     C                   PARM                    $FC2
413500960202     C                   PARM                    $PGF2
413600960117     C                   PARM                    DsPgm
413700960117     C                   PARM                    xsccut
413800960117     C                   PARM                    xscgrp
413900960117     C*
414000960118     C                   Z-add     1             OPZLEN
414100960118     C     *blank        Lookup    $TX1(opzlen)                           21
414200960117     C                   if        *in21 = *on
414300960117     C                   sub       1             OPZLEN
414400960117     C                   else
414500960117     C                   Z-add     $DimOp        OPZLEN
414600960117     C                   endif
414700940127     C*
414800940117     C                   ENDSR
414900940223     C/EJECT
415000940223     C************************************************************
415100940223     C* INIZIALIZZAZIONE VARIABILI
415200940223     C************************************************************
415300940223     C     INZVAR        BEGSR
415400941215     C*
415500941215     C* Dati da PGM chiamante
415600941215     C*
415700090323    >C                   MOVEL     KPJBU         NdcL76DS
415800951206     C*
415900951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
416000951206     C* reperimento dati società
416100090323    >C                   IF        SocietaL76 <> xscsoc  and
416200090323    >C                             OpzL76 <>*blank
416300960119     C                   MOVEL     'SOC001'      TIPXSC
416400090323    >C                   MOVEL     SocietaL76    SOCXSC
416500951122     C                   EXSR      REPSOC
416600951010     C     RTNXSC        IFNE      '1'
416700970424     C                   MOVEL     XSOCDS        SOC001
416800951010     C                   ELSE
416900951010     C                   MOVE      *ON           $FINE
417000090323    >C                   MOVE      '1'           ErrL76
417100951010     C                   EXSR      ENDPGM
417200951010     C                   ENDIF
417300960410     C                   ENDIF
417400090325     C*
417500090325     C* Reperimento autorizzazioni
417600090325     C*
417700090325     C* le autorizzazioni potrebbero essere già state passate,
417800090325     C* nel qual caso non vanno reperite
417900090325    >C     AUTDS         IFEQ      *BLANK
418000090325    >C                   EXSR      REPAUT
418100090325    >C                   ELSE
418200100409    >C                   MOVEL     XCADDS        CONRECDS
418300090325     C* Se non autorizzati alla funzione oppure                              ed esco
418400100409    >C     VisATT        IFNE      *ON
418500090325     C                   MOVE      XCAFNC        XNOFNC
418600090325     C                   MOVE      XCAPRF        XNOPRF
418700090325     C                   MOVE      XCACUT        XNOCUT
418800090325     C                   MOVE      XCATCK        XNOTCK
418900090325     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
419000090325     C                   CALL      'XNOAUTR'
419100090325     C                   PARM                    XNOADS
419200090325    >C                   MOVE      '2'           ErrL76
419300090325     C                   MOVE      *ON           $fine
419400090325     C                   EXSR      ENDPGM
419500090325     C                   ENDIF
419600090325    >C                   ENDIF
419700090324     C*
419800090324     c* reperimento dati parametro modulo solleciti
419900090324     C                   movel     'NDCL1S'      xpapar
420000090324     C                   movel     *blank        xparic
420100090324     C                   CALLb     'XPAR'
420200090324     C                   PARM                    xscsoc
420300090324     C                   PARM                    XPAPAR            8
420400090324     C                   PARM                    XPAOUT
420500090324     C                   PARM                    XPAERR            1
420600090324     C                   PARM                    XPARIC            1
420700090324     C     XPAERR        IFEQ      '0'
420800090324     C                   movel(p)  XpaOut        Anp057Ds
420900090324     c                   else
421000090324     c                   seton                                        lr
421100090324     c                   return
421200090324     C                   endif
421300941026     C*
421400941026     C* Reperimento valori default utente
421500941026     C*
421600941026     C                   EXSR      REPDFT
421700940506     C*
421800940506     C* Definizione delle opzioni utilizzabili a pgm
421900940506     C*
422000940506     C                   EXSR      DEFOPZ
422100950330     C*
422200940223     C* Variabili per gestione videate
422300940223     C*
422400940223     C                   MOVE      *OFF          $FINE                          fine pgm
422500940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
422600940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
422700130911C2226C                   MOVE      *OFF          $INZS2                         inizializzione S1
422800940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
422900950606     C                   MOVE      *OFF          $EndFile                       memo fine file
423000130911C2226C                   MOVE      *OFF          $EndFilePre                    memo fine file
423100940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
423200940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
423300130911C2226C                   MOVE      *OFF          $RCDOKPre                      rcd letto valido
423400940509     C                   MOVE      *HIVAL        $LASTV
423500940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
423600940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
423700940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
423800130911C2226C                   Z-ADD     0             $ULKS2            3 0          piede da emett. S1
423900960605     C* azzero la posizione del cursore
424000960605     C                   Z-ADD     0             H1RIGA
424100960605     C                   Z-ADD     0             H1COLO
424200940223     C*
424300940318     C                   MOVE      *ON           $INZD1
424400940318     C                   MOVE      *ON           $INZS1
424500130911C2226C                   MOVE      *ON           $INZS2
424600940831     C*
424700940831     C* se pgm richiamato con chiave univoca impostata,
424800940831     C* forzo la emissione della lista e imposto l'ordinamento
424900090323    >C*    OpzL76        IFNE      *BLANK
425000070524    >C*    UQ1001        IFNE      *BLANK
425100070524     C*                  MOVE      *ON           $Unique
425200070524     C*                  Z-ADD     2             $LOP
425300070524     C*                  Z-ADD     99            $ORD
425400070524     C*                  ELSE
425500070524     C*                  MOVE      *OFF          $Unique
425600070524     C*                  ENDIF
425700070524     C*                  ENDIF
425800950330     C*
425900950330     C* Variabili appoggio
426000950330     C*
426100950330     C                   Z-ADD     SFLPAG        WPAG
426200130924C2226C                   Z-ADD     SFLPAG        WPAG2
426300950330     C                   MOVEL     *OFF          WOPR
426400950515     C                   CLEAR                   W1POS
426500941104     C*
426600941104     C* definisco qual'è la prima videata
426700950410     C* (se la prima videata risulta la lista, devo comunque caricare
426800950410     C*  la videata di parzializzazione e gestire la definizione
426900950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
427000950410     C*  passati dei dati, può variare il caricamento)
427100940223     C                   SELECT
427200940223     C     $LOP          WHENEQ    1
427300940223     C                   MOVE      'D1'          $GEST
427400940223     C     $LOP          WHENEQ    2
427500130912C2226C                   IF        $Pre = *on
427600130911  "  C                   MOVE      'S2'          $GEST
427700130911C2226C                   Else
427800940223     C                   MOVE      'S1'          $GEST
427900130911C2226C                   Endif
428000941104     C                   EXSR      INZD1
428100090325     C                   EXSR      CTRD1
428200090325     C                   MOVE      *ALL'0'       IN5098
428300090325     C                   MOVEA     IN5098        *IN(50)
428400941104     C                   MOVE      *OFF          $INZD1
428500950410     C                   EXSR      DEFORD
428600941104     C                   ENDSL
428700940223     C*
428800940223     C* Valorizzazione campi univoci testate
428900940223     C*
429000940223     C                   CLEAR                   FMTT1
429100940223     C                   MOVEL     KNSIF         NOMSIF
429200941109     C                   MOVEL     XSCDSI        NOMDIT
429300941027     C                   MOVEL     DSPGM         NOMPGM
429400950331     C* recupero il sottotitolo (se c'è)
429500950331     C                   MOVEL     *BLANK        T1TIT
429600950331     C     SOTTIT        IFNE      *BLANK
429700950331     C                   MOVEL     SOTTIT        IDMSG
429800950522     C                   CALLB     'XRTVM'
429900950331     C                   PARM                    IDMSG            27
430000950331     C                   PARM                    TXTMSG          100
430100950331     C                   PARM                    ERRMSG            1
430200950331     C                   PARM      *ON           CTRMSG            1            centratura
430300950331     C                   PARM      30            LENMSG            3 0          lun output.
430400950331     C     ERRMSG        IFNE      *ON
430500950331     C                   MOVEL     TXTMSG        T1TIT
430600950331     C                   ELSE
430700950331     C                   MOVEL     *ALL'?'       T1TIT
430800950331     C                   ENDIF
430900950331     C                   ENDIF
431000940223     C*
431100940223     C                   ENDSR
431200941209     C/EJECT
431300070525     C************************************************************
431400070525     C* Composizione stringa sql
431500070525     C************************************************************
431600070525     C     InzSql        BEGSR
431700130912     C*
431800100407     C                   EVAL      SQ='Select distinct RcaSocieta,           +
431900100407     C                                 RcaPosiz, RcaAttrib, Rcakcc, Rcaksc,  +
432000130912     C                                 RcaDtPar, RcaNrPar, RcaSerPar, RcaDivisa+
432100130912     C                                 ,RcaKscAge from NDRCA00F'
432200130912C2226C                   If        $Pre = *on
432300140310  "  C                   Eval      SQ=%trimr(SQ) + ',ANRCO00F,ANSOG00F '
432400140310      *
432500140310     C*                  if        d1insoluti = xscon
432600140310     C*                  Eval      SQ=%trimr(SQ) + ' inner join ndscd00f on +
432700140310     C*                            scdsocieta=rcasocieta and +
432800140310     C*                            scdctb = ''CG'' and scdtpscad=''0'' +
432900140310     C*                            scdkcc=rcakcc and +
433000140310     C*                            scdksc=rcaksc and +
433100140310     C*                            scddtpar=rcadtpar and +
433200140310     C*                            scdnrpar=rcanrpar and +
433300140310     C*                            scdserpar=rcaserpar and +
433400140310     C*                            scddtsca=rcadtsca '
433500140310     C*                  endif
433600140310      *
433700140310     C                   Eval      SQ=%trimr(SQ) +
433800140310  "  C                             ' Where RcaKcc=RcoKcc and RcaKsc=RcoKsc and +
433900140310  "  C                             RcoSogg = SogSogg and '
434000130918  "  C                   If        $RgrPre = 'P'
434100130911  "  C                   Cat       'SogPartiva':1Sq
434200130911  "  C                   Cat       '='         :1Sq
434300130911  "  C                   Cat       ''''        :0Sq
434400130911  "  C                   Cat       H2Partiva   :0Sq
434500130911  "  C                   Cat       ''''        :0Sq
434600130911  "  C                   Cat       'and'       :1Sq
434700130911  "  C                   Cat       'SogCdFisc' :1Sq
434800130911  "  C                   Cat       '='         :1Sq
434900130911  "  C                   Cat       ''''        :0Sq
435000130911  "  C                   Cat       H2CdFisc    :0Sq
435100130912  "  C                   Cat       ''''        :0Sq
435200130912  "  C                   Cat       'and'       :1Sq
435300130912  "  C                   Else
435400130912  "  C                   Cat       'SogSogg'   :1Sq
435500130912  "  C                   Cat       '='         :1Sq
435600130912  "  C                   Cat       ''''        :0Sq
435700130912  "  C                   Cat       H2Sogg      :0Sq
435800130912  "  C                   Cat       ''''        :0Sq
435900130912  "  C                   Cat       'and'       :1Sq
436000130912  "  C                   Endif
436100130912C2226C                   Else
436200130912     C*
436300070525     C                   Cat       'Where'     :1Sq
436400130911C2226C                   Endif
436500100401     c* CAMPI OBBLIGATORI
436600070525     c* Societa
436700100401     C                   Cat       'Rcasocieta':1Sq
436800100401     C                   Cat       '='         :1Sq
436900100401     C                   Cat       ''''        :0Sq
437000070525     C                   Cat       xscsoc      :0Sq
437100100401     C                   Cat       ''''        :0Sq
437200130910C2226X*** unita (se gestita dai solleciti)
437300130910C2226C* unita (se gestita nella attività di recupero credito)
437400100401     c                   if        D1Unita<> *blanks
437500100401     C                   Cat       'and'       :1Sq
437600100401     C                   Cat       'RcaUnita'  :1Sq
437700100401     C                   Cat       '='         :1Sq
437800100401     C                   Cat       ''''        :0Sq
437900100401     C                   Cat       D1Unita     :0Sq
438000100401     C                   Cat       ''''        :0Sq
438100130910C2226C                   Else
438200130912  "  C                   if        GesuniR057='D'
438300130910  "  C                   Cat       'and'       :1Sq
438400130910  "  C                   Cat       'RcaUnita'  :1Sq
438500130910  "  C                   If        WinUni=' '
438600130910  "  C                   Cat       '='         :1Sq
438700130910  "  C                   Cat       ''''        :0Sq
438800130910  "  C                   Cat       ''''        :0Sq
438900130910  "  C                   Else
439000130910  "  C                   Cat       WinUni      :1Sq
439100130910  "  C                   Endif
439200130910C2226C                   Endif
439300130911     C                   endif
439400100401     c* capoconto
439500100407     c                   if        D1kcc  <> *blanks
439600100401     C                   Cat       'and'       :1Sq
439700100401     C                   Cat       'RcaKcc'    :1Sq
439800100401     C                   Cat       '='         :1Sq
439900100401     C                   Cat       ''''        :0Sq
440000100401     C                   Cat       D1kcc       :0Sq
440100100401     C                   Cat       ''''        :0Sq
440200130911C2226C                   else
440300130912  "  C                   If        $Pre = *on
440400130911  "  C                   Cat       'and'       :1Sq
440500130911  "  C                   Cat       'RcaKcc'    :1Sq
440600130911  "  C                   Cat       '='         :1Sq
440700130911  "  C                   Cat       ''''        :0Sq
440800130911  "  C                   Cat       H2kcc       :0Sq
440900130911  "  C                   Cat       ''''        :0Sq
441000130911C2226C                   endif
441100130911     C                   endif
441200100401     c*
441300100401     c* posizione attributo = a quella del parametro mod. solleciti
441400100401     C                   Cat       'and'       :1Sq
441500100401     C                   Cat       'RcaPosiz'  :1Sq
441600100401     C                   Cat       '='         :1Sq
441700100401     C                   Cat       ''''        :0Sq
441800100401     C                   Cat       PosAttr057  :0Sq
441900100401     C                   Cat       ''''        :0Sq
442000100401     c* CAMPI FACOLTATIVI
442100100401     c* attributo
442200100401     c                   if        D1attrib <> *blanks
442300100401     C                   Cat       'and'       :1Sq
442400100401     C                   Cat       'RcaAttrib' :1Sq
442500100401     C                   Cat       '='         :1Sq
442600100401     C                   Cat       ''''        :0Sq
442700100401     C                   Cat       d1Attrib    :0Sq
442800100401     C                   Cat       ''''        :0Sq
442900100401     c                   endif
443000100401     c* ksc
443100100401     c                   if        D1ksc <> *blanks
443200100401     C                   Cat       'and'       :1Sq
443300100401     C                   Cat       'Rcaksc'    :1Sq
443400100401     C                   Cat       '='         :1Sq
443500100401     C                   Cat       ''''        :0Sq
443600100401     C                   Cat       d1ksc       :0Sq
443700100401     C                   Cat       ''''        :0Sq
443800100401     c                   endif
443900100401     c* agente
444000100401     c                   if        D1kscAge <> *blanks
444100090409     C                   Cat       'and'       :1Sq
444200100401     C                   Cat       'RcakscAge' :1Sq
444300090409     C                   Cat       '='         :1Sq
444400100401     C                   Cat       ''''        :0Sq
444500100401     C                   Cat       d1kscage    :0Sq
444600100401     C                   Cat       ''''        :0Sq
444700090409     c                   endif
444800100512     c* parzializzazione fasi
444900140314     c                   select
445000140314     c                   when      D1FasiApe= ' '
445100140314     C                   Eval      Sq = %trim(sq) +
445200140314     C                             ' and RcaStato='' '''
445300140314     c                   when      D1FasiApe= '1'
445400140314     C                   Eval      Sq = %trim(sq) +
445500140314     C                             ' and RcaStato=''1'''
445600140314     c                   when      D1FasiApe= '2'
445700140314     C                   Eval      Sq = %trim(sq) +
445800140314     C                             ' and RcaStato=''2'''
445900140314     c                   endsl
446000140314      ***
446100140314     C***                if        d1fasiape <> *blanks
446200140314     C***                Cat       'and'       :1Sq
446300140314     C***                Cat       'RcaUteChiu':1Sq
446400140314     c**aperte
446500140314     c***                if        D1FasiApe=xscon
446600140314     C***                Cat       '='         :1Sq
446700140314     C***                Cat       ''''        :0Sq
446800140314     C***                Cat       ' '         :0Sq
446900140314     C***                Cat       ''''        :0Sq
447000140314     c***                else
447100140314     C***                Cat       '<>'        :1Sq
447200140314     C***                Cat       ''''        :0Sq
447300140314     C***                Cat       ' '         :0Sq
447400140314     C***                Cat       ''''        :0Sq
447500140314     c***                endif
447600140314     c***                endif
447700100413     c* parzializzazione partite
447800140314     C*                  Cat       'and'       :1Sq
447900140314     C*                  Cat       'RcaStato'  :1Sq
448000100413     c* aperte
448100140314     c*                  if        D1Stato=xscon
448200140314     C*                  Cat       '<>'        :1Sq
448300140314     C*                  Cat       ''''        :0Sq
448400140314     C*                  Cat       '2'         :0Sq
448500140314     C*                  Cat       ''''        :0Sq
448600140314     c*                  else
448700140314     C*                  Cat       '='         :1Sq
448800140314     C*                  Cat       ''''        :0Sq
448900140314     C*                  Cat       '2'         :0Sq
449000140314     C*                  Cat       ''''        :0Sq
449100140314     c*                  endif
449200140310      * Solo insoluti
449300140310     C**                 if        d1insoluti = xscon
449400140310     C**                 Eval      Sq = %trim(sq) +
449500140310     C**                           ' and substr(scdflg , 3 , 1)=''S'''
449600140310     C**                 endif
449700100407     c*
449800100420     c* parzializzazione data partita
449900100420     c                   exsr      ParzDtPar
450000100401     c* parzializzazione data scadenza
450100100401     c                   exsr      ParzDtSca
450200100416     c* parzializzazione data sollecito
450300100416     c                   exsr      ParzDtSol
450400100401     c* parzializzazione fase
450500100401     c                   if        D1FaseDa <> *blanks
450600100401     c                   move      D1FaseDa      FaseDa            1
450700100401     c                   move      D1FaseA       FaseA             1
450800100401     C                   Cat       'and'       :1Sq
450900100401     C                   Cat       'RcaFase'   :1Sq
451000100407     C                   Cat       'between'   :1Sq
451100100401     C                   Cat       FaseDa      :1Sq
451200100401     C                   Cat       'and'       :1Sq
451300100401     C                   Cat       FaseA       :1Sq
451400100401     c                   endif
451500100407     c* parzializzazione azione
451600100407     c                   if        D1Azione <> *blanks
451700100407     C                   Cat       'and'       :1Sq
451800100407     C                   Cat       'RcaAzione' :1Sq
451900100407     C                   Cat       '='         :1Sq
452000100407     C                   Cat       ''''        :0Sq
452100100407     C                   Cat       d1Azione    :0Sq
452200100407     C                   Cat       ''''        :0Sq
452300100407     c                   endif
452400131001BRT  C* esclusione impegni (98)
452500131001 "   c                   if        d1EsclImp = xscon
452600131001 "   C                   Cat       'and'       :1Sq
452700131001 "   C                   Cat       'RcaAzione' :1Sq
452800131001 "   C                   Cat       '<>'        :0Sq
452900131001 "   C                   Cat       '''98'''    :0Sq
453000131001BRT  C                   endif
453100131001BRT  C* parzializzazione fasi aperte BRT
453200140314     c                   select
453300140314     C                   when      d1fasiape = '1'
453400140314BRT  C                   exsr      ParzFasiBRT1
453500140314     C                   when      d1fasiape = '2'
453600140314brt  C                   exsr      ParzFasiBRT2
453700140314brt  c                   endsl
453800131129C2232C* parzializzazione campo libero
453900131129  "  C                   if        D1Libero <> *blanks
454000131129  "  C                   Cat       'and'       :1Sq
454100131129  "  C                   Cat       'RCALibero' :1Sq
454200131203  "  C                   Cat       'like ''%'  :1Sq
454300131205     C                   Eval      Sq = %trim(sq) +
454400131205     C                              %trim(%scanrpl('''':'''''':d1libero))
454500131205  "  C                   Cat       '%'''       :0Sq
454600131129C2232C                   endif
454700100401     c* parzializzazione destinatario
454800100401     c                   exsr      ParzDest
454900100401     c*
455000100401     c* parzializzazione data attività
455100100401     c                   exsr      ParzDtAtt
455200100401     C* ORDINAMENTO
455300100401     c                   cat       'Order by'  :1Sq
455400100401     c                   select
455500100401     c                   when      $ord = 1
455600100402     c                   cat       'RcaKsc,'   :1Sq
455700100402     c                   cat       'RcaDtPar'  :1Sq
455800100401     c                   when      $ord = 2
455900100402     c                   cat       'RcaDtPar,' :1Sq
456000100402     c                   cat       'RcaKsc'    :1Sq
456100100401     c                   when      $ord = 3
456200100402     c                   cat       'RcaKscAge,':1Sq
456300100401     c                   cat       'RcaKsc'    :1Sq
456400100401     c                   endsl
456500070525     C*
456600070525     C* Esecuzione SQL
456700070525     C                   Exsr      ExeSQL
456800070525     C*
456900070525     C                   ENDSR
457000070525     C/EJECT
457100130910C2226C************************************************************
457200130910  "  C* Composizione stringa sql di prelista
457300130910  "  C************************************************************
457400130910C2226C     InzSqlPre     BEGSR
457500130918     C* Selezione
457600130919     C                   EVAL      SQ='Select +
457700130918     C                                 RcaSocieta,RcaUnita,RcaKcc,SogCdFisc,+
457800130919     C                                 SogPartIva,SogSogg,Min(SogDes)  +
457900140310     C                                 from NDRCA00F'
458000140310      *
458100140310     C                   if        d1insoluti = xscon
458200140310     C                   Eval      SQ=%trimr(SQ) + ' inner join ndscd00f on +
458300140310     C                             scdsocieta=rcasocieta and +
458400140310     C                             scdctb = ''CG'' and scdtpscad=''0'' +
458500140310     C                             and scdkcc=rcakcc and +
458600140310     C                             scdksc=rcaksc and +
458700140310     C                             scddtpar=rcadtpar and +
458800140310     C                             scdnrpar=rcanrpar and +
458900140310     C                             scdserpar=rcaserpar and +
459000140310     C                             scddtsca=rcadtsca '
459100140310     C                   endif
459200130912     C*
459300140310     C                   EVAL      SQ=%trimr(SQ) + ' ,ANRCO00F,ANSOG00F'
459400140310      *
459500130912     C                   Cat       'Where'     :1Sq
459600130912     C* CAMPI OBBLIGATORI
459700130912     C* Societa
459800130918     C                   Cat       'RcaSocieta':1Sq
459900130912     C                   Cat       '='         :1Sq
460000130912     C                   Cat       ''''        :0Sq
460100130912     C                   Cat       xscsoc      :0Sq
460200130912     C                   Cat       ''''        :0Sq
460300130912     C* unita (se gestita nella attività di recupero credito)
460400130912     C                   if        D1Unita<> *blanks
460500130918     C                   Cat       'and'       :1Sq
460600130918     C                   Cat       'RcaUnita'  :1Sq
460700130918     C                   Cat       '='         :1Sq
460800130918     C                   Cat       ''''        :0Sq
460900130918     C                   Cat       D1Unita     :0Sq
461000130918     C                   Cat       ''''        :0Sq
461100130910     C                   Else
461200130918     C* unita se gestita ma vuota, include tutte le unità
461300130910     C* abilitate per il profilo.
461400130910     C                   if        GesuniR057='D'
461500130910     C                   Cat       'and'       :1Sq
461600130918     C                   Cat       'RcaUnita'  :1Sq
461700130910     C                   If        WinUni=' '
461800130910     C                   Cat       '='         :1Sq
461900130910     C                   Cat       ''''        :0Sq
462000130910     C                   Cat       ''''        :0Sq
462100130910     C                   Else
462200130910     C                   Cat       WinUni      :1Sq
462300130910     C                   Endif
462400130910     C                   Endif
462500130912     C                   Endif
462600130912     C* capoconto
462700130912     C                   if        D1kcc  <> *blanks
462800130912     C                   Cat       'and'       :1Sq
462900130918     C                   Cat       'RcaKcc'    :1Sq
463000130912     C                   Cat       '='         :1Sq
463100130912     C                   Cat       ''''        :0Sq
463200130912     C                   Cat       D1kcc       :0Sq
463300130912     C                   Cat       ''''        :0Sq
463400130912     C                   endif
463500130918     c* posizione attributo = a quella del parametro mod. solleciti
463600130918     C                   Cat       'and'       :1Sq
463700130918     C                   Cat       'RcaPosiz'  :1Sq
463800130918     C                   Cat       '='         :1Sq
463900130918     C                   Cat       ''''        :0Sq
464000130918     C                   Cat       PosAttr057  :0Sq
464100130918     C                   Cat       ''''        :0Sq
464200130912     C*
464300130912     C* join con i file anagrafici(anrco00f/ansog00f)
464400130912     C* per reperire la partita Iva
464500130910     C                   Cat       'and'       :1Sq
464600130918     C                   Cat       'RcaSocieta':1Sq
464700130910     C                   Cat       '='         :1Sq
464800130910     C                   Cat       'RcoSocieta':0Sq
464900130910     C*
465000130910     C                   Cat       'and'       :1Sq
465100130918     C                   Cat       'RcaKcc'    :1Sq
465200130910     C                   Cat       '='         :1Sq
465300130910     C                   Cat       'RcoKcc'    :0Sq
465400130910     C*
465500130910     C                   Cat       'and'       :1Sq
465600130918     C                   Cat       'RcaKsc'    :1Sq
465700130910     C                   Cat       '='         :1Sq
465800130918     C                   Cat       'RcoKsc'    :1Sq
465900130910     C*
466000130910     C                   Cat       'and'       :1Sq
466100130910     C                   Cat       'RcoSogg'   :1Sq
466200130910     C                   Cat       '='         :1Sq
466300130910     C                   Cat       'SogSogg'   :1Sq
466400130918     C* CAMPI FACOLTATIVI
466500130918     C* attributo
466600130918     C                   if        D1attrib <> *blanks
466700130918     C                   Cat       'and'       :1Sq
466800130918     C                   Cat       'RcaAttrib' :1Sq
466900130918     C                   Cat       '='         :1Sq
467000130918     C                   Cat       ''''        :0Sq
467100130918     C                   Cat       d1Attrib    :0Sq
467200130918     C                   Cat       ''''        :0Sq
467300130918     C                   endif
467400130918     C* ksc
467500130918     C                   if        D1ksc <> *blanks
467600130918     C                   Cat       'and'       :1Sq
467700130918     C                   Cat       'Rcaksc'    :1Sq
467800130918     C                   Cat       '='         :1Sq
467900130918     C                   Cat       ''''        :0Sq
468000130918     C                   Cat       d1ksc       :0Sq
468100130918     C                   Cat       ''''        :0Sq
468200130918     C                   endif
468300130918     C* agente
468400130918     C                   if        D1kscAge <> *blanks
468500130918     C                   Cat       'and'       :1Sq
468600130918     C                   Cat       'RcakscAge' :1Sq
468700130918     C                   Cat       '='         :1Sq
468800130918     C                   Cat       ''''        :0Sq
468900130918     C                   Cat       d1kscage    :0Sq
469000130918     C                   Cat       ''''        :0Sq
469100130918     C                   endif
469200130918     C* parzializzazione fasi
469300140314     c                   select
469400140314     c                   when      D1FasiApe= ' '
469500140314     C                   Eval      Sq = %trim(sq) +
469600140314     C                             ' and RcaStato='' '''
469700140314     c                   when      D1FasiApe= '1'
469800140314     C                   Eval      Sq = %trim(sq) +
469900140314     C                             ' and RcaStato=''1'''
470000140314     c                   when      D1FasiApe= '2'
470100140314     C                   Eval      Sq = %trim(sq) +
470200140314     C                             ' and RcaStato=''2'''
470300140314     c                   endsl
470400140314     c**                 if        D1FasiApe=XSCON
470500140314     C**                 Cat       'and'       :1Sq
470600140314     C***                Cat       'RcaUteChiu':1Sq
470700140314     C**                 Cat       'RcaStato':1  Sq
470800140314      * aperte
470900140314     c**                 if        D1FasiApe=xscon
471000140314     C**                 Cat       '='         :1Sq
471100140314     C**                 Cat       ''''        :0Sq
471200140314     C**                 Cat       ' '         :0Sq
471300140314     C**                 Cat       ''''        :0Sq
471400140314     c**                 else
471500140314     C**                 Cat       '<>'        :1Sq
471600140314     C**                 Cat       ''''        :0Sq
471700140314     C**                 Cat       ' '         :0Sq
471800140314     C**                 Cat       ''''        :0Sq
471900140314     C**                 endif
472000140314     C**                 endif
472100130918     C* parzializzazione partite
472200140314     C**                 Cat       'and'       :1Sq
472300140314     C**                 Cat       'RcaStato'  :1Sq
472400140314     C**aperte
472500140314     C**                 select
472600140314     C**                 when      D1Stato=xscon
472700140314     C**                 Cat       '<>'        :1Sq
472800140314     C**                 Cat       ''''        :0Sq
472900140314     C**                 Cat       '2'         :0Sq
473000140314     C**                 Cat       ''''        :0Sq
473100140314     C**                 when      D1Stato=xscoff
473200140314     C**                 Cat       '='         :1Sq
473300140314     C**                 Cat       ''''        :0Sq
473400140314     C**                 Cat       '2'         :0Sq
473500140314     C**                 Cat       ''''        :0Sq
473600140314     C**                 endsl
473700140310      * Solo insoluti
473800140310     C                   if        d1insoluti = xscon
473900140310     C                   Eval      Sq = %trim(sq) +
474000140310     C                             ' and substr(scdflg , 3 , 1)=''S'''
474100140310     C                   endif
474200131129C2232c* parzializzazione campo libero
474300131129  "  c                   if        D1Libero <> *blanks
474400131129  "  C                   Cat       'and'       :1Sq
474500131129  "  C                   Cat       'RCALibero' :1Sq
474600131203  "  C                   Cat       'like ''%'  :1Sq
474700131205     C                   Eval      Sq = %trim(sq) +
474800131205     C                             %trim(%scanrpl('''':'''''':d1libero))
474900131203  "  C                   Cat       '%'''       :0Sq
475000131129C2232C                   endif
475100130918     C* parzializzazione data partita
475200130918     C                   exsr      ParzDtPar
475300130918     C* parzializzazione data scadenza
475400130918     C                   exsr      ParzDtSca
475500130918     C* parzializzazione data sollecito
475600130918     C                   exsr      ParzDtSol
475700130918     C* parzializzazione fase
475800130918     C                   if        D1FaseDa <> *blanks
475900130918     C                   move      D1FaseDa      FaseDa            1
476000130918     C                   move      D1FaseA       FaseA             1
476100130918     C                   Cat       'and'       :1Sq
476200130918     C                   Cat       'RcaFase'   :1Sq
476300130918     C                   Cat       'between'   :1Sq
476400130918     C                   Cat       FaseDa      :1Sq
476500130918     C                   Cat       'and'       :1Sq
476600130918     C                   Cat       FaseA       :1Sq
476700130918     C                   endif
476800130918     C* parzializzazione azione
476900130918     c                   if        D1Azione <> *blanks
477000130918     C                   Cat       'and'       :1Sq
477100130918     C                   Cat       'RcaAzione' :1Sq
477200130918     C                   Cat       '='         :1Sq
477300130918     C                   Cat       ''''        :0Sq
477400130918     C                   Cat       d1Azione    :0Sq
477500130918     C                   Cat       ''''        :0Sq
477600130918     C                   endif
477700131001BRT  C* esclusione impegni (98)
477800131001 "   c                   if        d1EsclImp = xscon
477900131001 "   C                   Cat       'and'       :1Sq
478000131001 "   C                   Cat       'RcaAzione' :1Sq
478100131001 "   C                   Cat       '<>'        :0Sq
478200131001 "   C                   Cat       '''98'''    :0Sq
478300131001BRT  C                   endif
478400131001BRT  C* parzializzazione fasi aperte BRT
478500140314     c                   select
478600140314     C                   when      d1fasiape = '1'
478700140314BRT  C                   exsr      ParzFasiBRT1
478800140314     C                   when      d1fasiape = '2'
478900140314brt  C                   exsr      ParzFasiBRT2
479000140314brt  c                   endsl
479100130918     C* parzializzazione destinatario
479200130918     C                   exsr      ParzDest
479300130918     C*
479400130918     C* parzializzazione data attività
479500130918     C                   exsr      ParzDtAtt
479600140318     C* parzializzazione data chiusura
479700140318     C                   exsr      ParzDtChiu
479800130919     C* Raggruppamento
479900130919     C                   cat       'Group by'  :1Sq
480000130919     C                   cat       'RcaSocieta':1Sq
480100130919     C                   cat       ','         :0Sq
480200130919     C                   cat       'RcaUnita'  :0Sq
480300130919     C                   cat       ','         :0Sq
480400130919     C                   cat       'RcaKcc'    :0Sq
480500130919     C                   cat       ','         :0Sq
480600130919     C                   cat       'SogCdFisc' :0Sq
480700130919     C                   cat       ','         :0Sq
480800130919     C                   cat       'SogPartIva':0Sq
480900130919     C                   cat       ','         :0Sq
481000130919     C                   cat       'SogSogg'   :0Sq
481100130919     C* ordinamento
481200130919     C                   cat       'Order by'  :1Sq
481300130919     C                   if        $RgrPre = 'P'
481400130919     C                   cat       'RcaKcc'    :1Sq
481500130919     C                   cat       ','         :0Sq
481600130919     C                   cat       'SogCdFisc' :0Sq
481700130919     C                   cat       ','         :0Sq
481800130919     C                   cat       'SogPartIva':0Sq
481900130919     C                   Else
482000130919     C                   cat       'RCaKcc'    :1Sq
482100130919     C                   cat       ','         :0Sq
482200130919     C                   cat       'SogSogg'   :0Sq
482300130919     C                   Endif
482400130912     C*
482500130910     C* Esecuzione SQL
482600130910     C                   Exsr      ExeSQLPre
482700130918C2226C*
482800130918  "  C                   ENDSR
482900130918C2226C/EJECT
483000130919C2226C************************************************************
483100130919  "  C* Composizione stringa sql di tabella in qtemp
483200130919  "  C************************************************************
483300130919C2226C     InzSqltab     BEGSR
483400130919     C* Selezione
483500131022     C                   EVAL      SQ='Select * from Qtemp/NDrcaTemp'
483600131022      *
483700131022     C                   if        d1impda > 0 or
483800131022     C                             d1impa > 0
483900131022     C                   if        d1impa = 0
484000131022     C                   eval      d1impa=*all'9'
484100131022     C                   ENDIF
484200131022      *
484300131022     C                   eval      SQ=%trim(SQ) +
484400131022     C                             ' where imppre between ' +
484500131022     C                             %char(d1impda) +
484600131022     C                             ' and ' + %char(d1impa)
484700131022     C                   ENDIF
484800131022     C
484900130919     C* ordinamento
485000130919     C                   cat       'Order by'  :1Sq
485100130919     C                   cat       'ImpPre'    :1Sq
485200130919     C                   cat       'Desc'      :1Sq
485300130919     C*
485400130919     C* Esecuzione SQL
485500130919     C                   Exsr      ExeSQLTab
485600130919C2226C*
485700130919  "  C                   ENDSR
485800130919C2226C/EJECT
485900131001BRT  C************************************************************
486000140314 "   C* SubSelect scadenze chiuse
486100131001 "   C************************************************************
486200140314 "   C     ParzFasiBRT2  BEGSR
486300131001 "    /free
486400140314 "          if d1FasiApe = '2';
486500131001 "         eval sq = %trim(sq) +
486600131001 "       ' and not exists +
486700131001 "       (select rcaksc from ndrca00f as b  +
486800140314 "       //  where b.rcautechiu ='' '' +
486900140314 "       where b.rcastato <> ''2'' +
487000131001 "       and ndrca00f.rcasocieta = b.rcasocieta +
487100131001 "       and ndrca00f.rcaposiz = b.rcaposiz +
487200131001 "       and ndrca00f.rcaunita = b.rcaunita +
487300131001 "       and ndrca00f.rcakcc = b.rcakcc +
487400131001 "       and ndrca00f.rcaksc = b.rcaksc +
487500131001 "       and ndrca00f.rcadtsca = b.rcadtsca +
487600131001 "       and ndrca00f.rcadtpar = b.rcadtpar +
487700131001 "       and ndrca00f.rcanrpar = b.rcanrpar +
487800131001 "       and ndrca00f.rcaserpar = b.rcaserpar) ';
487900131001 "       endif;
488000131001 "    /end-free
488100131001BRT  C                   ENDSR
488200140314BRT  C************************************************************
488300140314 "   C* SubSelect fasi chiuse manualmente
488400140314 "   C************************************************************
488500140314 "   C     ParzFasiBRT1  BEGSR
488600140314 "    /free
488700140314 "          if d1FasiApe = '1';
488800140314 "         eval sq = %trim(sq) +
488900140314 "       ' and not exists +
489000140314 "       (select rcaksc from ndrca00f as b  +
489100140314 "       //  where b.rcautechiu ='' '' +
489200140314 "       where b.rcastato = '' '' +
489300140314 "       and ndrca00f.rcasocieta = b.rcasocieta +
489400140314 "       and ndrca00f.rcaposiz = b.rcaposiz +
489500140314 "       and ndrca00f.rcaunita = b.rcaunita +
489600140314 "       and ndrca00f.rcakcc = b.rcakcc +
489700140314 "       and ndrca00f.rcaksc = b.rcaksc +
489800140314 "       and ndrca00f.rcadtsca = b.rcadtsca +
489900140314 "       and ndrca00f.rcadtpar = b.rcadtpar +
490000140314 "       and ndrca00f.rcanrpar = b.rcanrpar +
490100140314 "       and ndrca00f.rcaserpar = b.rcaserpar) ';
490200140314 "       endif;
490300140314 "    /end-free
490400140314BRT  C                   ENDSR
490500100420     C******************************************************
490600100420     C* Parzializzazioni data partita
490700100420     C******************************************************
490800100420     C     ParzDtPar     BEGSR
490900100420     c*
491000131001BRT  c                   if        D1DtParDa <> 0
491100131001BRT  c                             or D1DtParA <> 0
491200100420     c                   move      WdtParDa      DtPar10DA        10
491300100420     c                   move      WdtParA       DtPar10A         10
491400100420     C                   Cat       'and'       :1Sq
491500100420     C                   Cat       '(RcaDtPar' :1Sq
491600100420     C                   Cat       '>='        :1Sq
491700100420     C                   Cat       '''':0        Sq
491800100420     C                   Cat       DtPar10DA   :0Sq
491900100420     C                   Cat       '''':0        Sq
492000100420     C                   Cat       'and'       :1Sq
492100100420     C                   Cat       'RcaDtPar'  :1Sq
492200100420     C                   Cat       '<='        :1Sq
492300100420     C                   Cat       '''':0        Sq
492400100420     C                   Cat       DtPar10A    :0Sq
492500100420     C                   Cat       '''':0        Sq
492600100420     C                   Cat       ')'         :1Sq
492700131001BRT  C                   Endif
492800100420     C*
492900100420     C                   ENDSR
493000090325     C******************************************************
493100100401     C* Parzializzazioni data scadenza
493200090325     C******************************************************
493300100401     C     ParzDtSca     BEGSR
493400090325     c*
493500131001BRT  c                   if        D1DtScaDa <> 0
493600131001BRT  c                             or D1DtScaA <> 0
493700100401     c                   move      WdtScaDa      DtSca10DA        10
493800100401     c                   move      WdtScaA       DtSca10A         10
493900090325     C                   Cat       'and'       :1Sq
494000100401     C                   Cat       '(RcaDtSca' :1Sq
494100090325     C                   Cat       '>='        :1Sq
494200090325     C                   Cat       '''':0        Sq
494300100401     C                   Cat       DtSca10DA   :0Sq
494400090325     C                   Cat       '''':0        Sq
494500090325     C                   Cat       'and'       :1Sq
494600100401     C                   Cat       'RcaDtSca'  :1Sq
494700090325     C                   Cat       '<='        :1Sq
494800090325     C                   Cat       '''':0        Sq
494900100401     C                   Cat       DtSca10A    :0Sq
495000090325     C                   Cat       '''':0        Sq
495100090325     C                   Cat       ')'         :1Sq
495200131001BRT  C                   Endif
495300090325     C*
495400090325     C                   ENDSR
495500100416     C******************************************************
495600100416     C* Parzializzazioni data sollecito
495700100416     C******************************************************
495800100416     C     ParzDtSol     BEGSR
495900100416     c*
496000131001BRT  c                   if        D1DtSolDa <> 0
496100131001BRT  c                             or D1DtSolA <> 0
496200100416     c                   move      WdtSolDa      DtSol10DA        10
496300100416     c                   move      WdtSolA       DtSol10A         10
496400100416     C                   Cat       'and'       :1Sq
496500100416     C                   Cat       '(RcaDtLet' :1Sq
496600100416     C                   Cat       '>='        :1Sq
496700100416     C                   Cat       '''':0        Sq
496800100416     C                   Cat       DtSol10DA   :0Sq
496900100416     C                   Cat       '''':0        Sq
497000100416     C                   Cat       'and'       :1Sq
497100100416     C                   Cat       'RcaDtLet'  :1Sq
497200100416     C                   Cat       '<='        :1Sq
497300100416     C                   Cat       '''':0        Sq
497400100416     C                   Cat       DtSol10A    :0Sq
497500100416     C                   Cat       '''':0        Sq
497600100416     C                   Cat       ')'         :1Sq
497700131001BRT  C                   Endif
497800100416     C*
497900100416     C                   ENDSR
498000100401     C******************************************************
498100100401     C* Parzializzazioni destinatario
498200100401     C******************************************************
498300100401     C     ParzDest      BEGSR
498400100401     c*
498500100401     c                   if        D1Destin <> *blanks
498600100401     C                   Cat       'and'       :1Sq
498700100401     C                   Cat       'RcaGrute'  :1Sq
498800100402     C                   Cat       'in('       :1Sq
498900100401     C*
499000100401     C*   leggo tutta la tabella 048 per caricare i gruppi del
499100100401     c*   destinatario
499200100401     C*
499300100401     C                   MOVE      '2'           XTBRIC
499400100401     C                   MOVE      XSCSOC        XTBAZI
499500100401     C                   MOVE      '048'         XTBCOD
499600100401     C                   MOVE      *BLANKS       XTBKEY
499700100401     C                   CALLb     'XATB'
499800100401     C                   PARM                    XATBDS
499900100401     C     XTBERR        DOWEQ     *OFF
500000100401     C                   MOVEL     XTBUNI        ang048ds
500100100401     c                   move      xtbkey        Gruppo            3
500200100401     c                   select
500300100401     c                   when      d1Destin=usr1048
500400100401     C                   Cat       ''''        :0Sq
500500100402     c                   cat       Gruppo      :0Sq
500600100401     C                   Cat       ''''        :0Sq
500700100402     C                   Cat       ','         :1Sq
500800100401     c                   when      d1Destin=usr2048
500900100401     C                   Cat       ''''        :0Sq
501000100402     c                   cat       Gruppo      :0Sq
501100100401     C                   Cat       ''''        :0Sq
501200100402     C                   Cat       ','         :1Sq
501300100401     c                   when      d1Destin=usr3048
501400100401     C                   Cat       ''''        :0Sq
501500100402     c                   cat       Gruppo      :0Sq
501600100401     C                   Cat       ''''        :0Sq
501700100402     C                   Cat       ','         :1Sq
501800100401     c                   when      d1Destin=usr4048
501900100401     C                   Cat       ''''        :0Sq
502000100402     c                   cat       Gruppo      :0Sq
502100100401     C                   Cat       ''''        :0Sq
502200100402     C                   Cat       ','         :1Sq
502300100401     c                   when      d1Destin=usr5048
502400100401     C                   Cat       ''''        :0Sq
502500100402     c                   cat       Gruppo      :0Sq
502600100401     C                   Cat       ''''        :0Sq
502700100402     C                   Cat       ','         :1Sq
502800100401     c                   when      d1Destin=usr6048
502900100401     C                   Cat       ''''        :0Sq
503000100402     c                   cat       Gruppo      :0Sq
503100100401     C                   Cat       ''''        :0Sq
503200100402     C                   Cat       ','         :1Sq
503300100401     c                   when      d1Destin=usr7048
503400100401     C                   Cat       ''''        :0Sq
503500100402     c                   cat       Gruppo      :0Sq
503600100401     C                   Cat       ''''        :0Sq
503700100402     C                   Cat       ','         :1Sq
503800100401     c                   when      d1Destin=usr8048
503900100401     C                   Cat       ''''        :0Sq
504000100402     c                   cat       Gruppo      :0Sq
504100100401     C                   Cat       ''''        :0Sq
504200100402     C                   Cat       ','         :1Sq
504300100401     c                   endsl
504400100401     C*
504500100401     C                   MOVE      '3'           XTBRIC
504600100401     C                   MOVE      XSCSOC        XTBAZI
504700100401     C                   MOVE      *ZERO         XTBKEY
504800100401     C                   MOVE      '048'         XTBCOD
504900100401     C                   CALLb     'XATB'
505000100401     C                   PARM                    XATBDS
505100100401     C*
505200100401     C                   ENDDO                                                  *
505300100401     c* chiudo stringa sql
505400100402     C                   Cat       ''''        :1Sq
505500100402     C                   Cat       ' '         :1Sq
505600100402     C                   Cat       ''''        :1Sq
505700100402     C                   Cat       ')'         :1Sq
505800100401     c
505900100401     c                   endif
506000100401     C*
506100100401     C                   ENDSR
506200100401     C******************************************************
506300100401     C* Parzializzazioni data attività
506400100401     C******************************************************
506500100401     C     ParzDtAtt     BEGSR
506600100401     c*
506700100401     c                   move      WdtAttDa      DtAtt10DA        10
506800100401     c                   move      WdtAttA       DtAtt10A         10
506900100401     C                   Cat       'and'       :1Sq
507000100401     C                   Cat       '(RcaDtAtt' :1Sq
507100100401     C                   Cat       '>='        :1Sq
507200100401     C                   Cat       '''':0        Sq
507300100401     C                   Cat       DtAtt10DA   :0Sq
507400100401     C                   Cat       '''':0        Sq
507500100401     C                   Cat       'and'       :1Sq
507600100401     C                   Cat       'RcaDtAtt'  :1Sq
507700100401     C                   Cat       '<='        :1Sq
507800100401     C                   Cat       '''':0        Sq
507900100401     C                   Cat       DtAtt10A    :0Sq
508000100401     C                   Cat       '''':0        Sq
508100100401     C                   Cat       ')'         :1Sq
508200100401     C*
508300100401     C                   ENDSR
508400140318     C******************************************************
508500140318     C* Parzializzazioni data chiusura
508600140318     C******************************************************
508700140318     C     ParzDtChiu    BEGSR
508800140318     c*
508900140318     c                   move      WdtChiDa      DtChi10DA        10
509000140318     c                   move      WdtChiA       DtChi10A         10
509100140318     C                   Cat       'and'       :1Sq
509200140319     C                   Cat       '(RcaDtChiu':1Sq
509300140318     C                   Cat       '>='        :1Sq
509400140318     C                   Cat       '''':0        Sq
509500140318     C                   Cat       DtChi10DA   :0Sq
509600140318     C                   Cat       '''':0        Sq
509700140318     C                   Cat       'and'       :1Sq
509800140319     C                   Cat       'RcaDtChiu' :1Sq
509900140318     C                   Cat       '<='        :1Sq
510000140318     C                   Cat       '''':0        Sq
510100140318     C                   Cat       DtChi10A    :0Sq
510200140318     C                   Cat       '''':0        Sq
510300140318     C                   Cat       ')'         :1Sq
510400140318     C*
510500140318     C                   ENDSR
510600070525     C******************************************************
510700070525     C* ESECUZIONE SQL
510800070525     C******************************************************
510900070525     C     EXESQL        BEGSR
511000070525     C*
511100070525     C                   EXSR      CLOCUR
511200070525     C*
511300070525     C/EXEC SQL
511400070525     C+     PREPARE S1 FROM :SQ
511500070525     C/END-EXEC
511600070525     C*
511700070525     C/EXEC SQL
511800070525     C+     DECLARE A1 CURSOR FOR S1
511900070525     C/END-EXEC
512000070525     C*
512100070525     C/EXEC SQL
512200070525     C+     OPEN A1
512300070525     C/END-EXEC
512400070525     c*
512500070525     C                   MOVE      *ON           $OPEN             1
512600070525     c*
512700070525     C                   ENDSR
512800070525     C*
512900070525     C************************************************************
513000070525     C* CHIUSURA CURSORE
513100070525     C************************************************************
513200070525     C     CLOCUR        BEGSR
513300070525     C*
513400070525     C/EXEC SQL
513500070525     C+     CLOSE A1
513600070525     C/END-EXEC
513700070525     C*
513800070525     C                   MOVE      *OFF          $OPEN
513900070525     C*
514000070525     C                   ENDSR
514100070525     C/EJECT
514200070525     C************************************************************
514300070525     C* fetch
514400070525     C************************************************************
514500070525     C     FetchSql      BEGSR
514600070525     C*
514700070525     C/EXEC SQL
514800100402     C+     FETCH A1 INTO :DsFetch
514900070525     C/END-EXEC
515000070525     c*
515100070525     C                   ENDSR
515200130910C2226C******************************************************
515300130910  "  C* ESECUZIONE SQL
515400130910  "  C******************************************************
515500130910C2226C     EXESQLPRE     BEGSR
515600130910     C*
515700130910     C                   EXSR      CLOCURPRE
515800130910     C*
515900130910     C/EXEC SQL
516000130910     C+     PREPARE S2 FROM :SQ
516100130910     C/END-EXEC
516200130910     C*
516300130910     C/EXEC SQL
516400130910     C+     DECLARE A2 CURSOR FOR S2
516500130910     C/END-EXEC
516600130910     C*
516700130910     C/EXEC SQL
516800130910     C+     OPEN A2
516900130910     C/END-EXEC
517000130910     c*
517100130910     C                   MOVE      *ON           $OPENPre          1
517200130910     c*
517300130910C2226C                   ENDSR
517400130910  "  C*
517500130910  "  C************************************************************
517600130910  "  C* CHIUSURA CURSORE
517700130910  "  C************************************************************
517800130910C2226C     CLOCURPRE     BEGSR
517900130910     C*
518000130910     C/EXEC SQL
518100130910     C+     CLOSE A2
518200130910     C/END-EXEC
518300130910     C*
518400130910     C                   MOVE      *OFF          $OPENPre
518500130910     C*
518600130910C2226C                   ENDSR
518700130910  "  C/EJECT
518800130910  "  C************************************************************
518900130910  "  C* fetch
519000130910  "  C************************************************************
519100130910C2226C     FetchSqlPre   BEGSR
519200130910     C*
519300130910     C/EXEC SQL
519400130910     C+     FETCH A2 INTO :DsFetchPre
519500130910     C/END-EXEC
519600130910     c*
519700130912C2226C                   ENDSR
519800130919C2226C******************************************************
519900130919  "  C* ESECUZIONE SQL
520000130919  "  C******************************************************
520100130919C2226C     EXESQLTAB     BEGSR
520200130919     C*
520300130919     C                   EXSR      CLOCURTAB
520400130919     C*
520500130919     C/EXEC SQL
520600130919     C+     PREPARE S3 FROM :SQ
520700130919     C/END-EXEC
520800130919     C*
520900130919     C/EXEC SQL
521000130919     C+     DECLARE A3 CURSOR FOR S3
521100130919     C/END-EXEC
521200130919     C*
521300130919     C/EXEC SQL
521400130919     C+     OPEN A3
521500130919     C/END-EXEC
521600130919     c*
521700130919     C                   MOVE      *ON           $OPENTab          1
521800130919     c*
521900130919C2226C                   ENDSR
522000130919  "  C*
522100130919  "  C************************************************************
522200130919  "  C* CHIUSURA CURSORE
522300130919  "  C************************************************************
522400130919C2226C     CLOCURTAB     BEGSR
522500130919     C*
522600130919     C/EXEC SQL
522700130919     C+     CLOSE A3
522800130919     C/END-EXEC
522900130919     C*
523000130919     C                   MOVE      *OFF          $OPENTAB
523100130919     C*
523200130919C2226C                   ENDSR
523300130919  "  C/EJECT
523400130919  "  C************************************************************
523500130919  "  C* fetch
523600130919  "  C************************************************************
523700130919C2226C     FetchSqlTAB   BEGSR
523800130919     C*
523900130919     C/EXEC SQL
524000130919     C+     FETCH A3 INTO :DsFetchTab
524100130919     C/END-EXEC
524200130919     c*
524300130919C2226C                   ENDSR
524400070525     C*
524500941209     C************************************************************
524600941209     C* REPERIMENTO DATI SOCIETA'
524700941209     C************************************************************
524800941209     C     REPSOC        BEGSR
524900941209     C*
525000950522     C                   CALLB     'XSOC'
525100941209     C                   PARM                    TIPXSC            6
525200941209     C                   PARM                    SOCXSC            3
525300941209     C                   PARM                    CDSXSC            9 0
525400941209     C                   PARM                    MODXSC            3
525500941209     C                   PARM      *BLANKS       RTNXSC            1
525600941209     C                   PARM                    XSOCDS
525700941209     C                   PARM                    KPJBA
525800941209     C*
525900941209     C                   ENDSR
526000940223     C/EJECT
526100090325     C************************************************************
526200090325     C* REPERIMENTO AUTORIZZAZIONI UTENTE
526300090325     C************************************************************
526400090325     C     REPAUT        BEGSR
526500090325     C*
526600090325     C                   CLEAR                   AUTDS
526700090325     C*
526800090325     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
526900090325     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
527000090325     C                   Z-ADD     0             XCAGRP                         GRUPPO
527100090325     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
527200100409    >C                   MOVEL     'CONREC'      XCAFNC                         FUNZIONE
527300090325    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
527400090325     c                   MOVE      'REC'         XCAVFU
527500090511    >C                   MOVE      'CC'          XCATCK                         TIPO CTRL
527600090325    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
527700090325     C*
527800090325     C                   CALLB     'XCHKAUT'
527900090325     C                   PARM                    AUTDS
528000090325     C*
528100090325     C* errori/non abilitazione
528200090325     C     XCARTN        IFNE      0
528300090325    >C                   MOVE      '2'           ErrL76
528400090325     C                   EXSR      ENDPGM
528500090325     C                   ELSE
528600100409    >C                   MOVEL     XCADDS        CONRECDS
528700090325     C* Se non autorizzati alla funzione oppure                              ed esco
528800100409    >C     visATT        IFNE      *ON
528900090325     C                   MOVE      XCAFNC        XNOFNC
529000090325     C                   MOVE      XCAPRF        XNOPRF
529100090325     C                   MOVE      XCACUT        XNOCUT
529200090325     C                   MOVE      XCATCK        XNOTCK
529300090325     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
529400090325     C                   CALL      'XNOAUTR'
529500090325     C                   PARM                    XNOADS
529600090325     C                   MOVE      '2'           ErrL76
529700090325     C                   MOVE      *ON           $fine
529800090325     C                   EXSR      ENDPGM
529900090325     C                   ENDIF
530000090325     C                   ENDIF
530100090325     C*
530200090325     C                   ENDSR
530300090325     C/EJECT
530400940127     C************************************************************
530500940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
530600940127     C************************************************************
530700940131     C     REPDFT        BEGSR
530800940127     C*
530900950524     C* preparo i parametri di chiamata al driver
531000950524     C                   CLEAR                   XMPEDS
531100950524     C                   EVAL      TpChiamata = '01'
531200950524     C                   EVAL      NChiavi = 5
531300950524     C                   EVAL      VLogica = 2
531400950524     C*
531500950524     C* preparo le chiavi per la chiamata al driver
531600950524     C                   CLEAR                   XMPEDS1
531700950524     C                   MOVEL     XSCSOC        DSMPESOC
531800950524     C                   MOVEL     DSPGM         DSMPEPGM
531900950524     C                   MOVEL     KNMUS         DSMPEPRF
532000950524     C* Si reperiscono i dati di default a seconda delle modalità con
532100950524     C* cui il pgm è usato
532200950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
532300950524     C* MPETIP = 'PV'  pgm richiamato da menù
532400950524     C* MPETIP = 'PB'  x lancio Batch
532500941013     C                   SELECT
532600090323    >C     OPZL76        WHENEQ    '01'
532700950524     C                   MOVEL     'PX'          DSMPETIP
532800941013     C                   OTHER
532900950524     C                   MOVEL     'PV'          DSMPETIP
533000941013     C                   ENDSL
533100950524     C*
533200950524     C                   CALLB     'XMPE'
533300950524     C                   PARM                    XMPEDS
533400950524     C                   PARM                    XMPEDS1
533500950524     C*
533600950524     C     Risultato     IFEQ      '0'
533700950524     C                   MOVEL     DSMPEPAR      DSMPE
533800940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
533900940131     C* i suoi valori di default vengono decisi a pgm
534000940131     C                   ELSE
534100090323     C                   Z-ADD     2             $LOP
534200940131     C                   Z-ADD     1             $ORD
534300130924C2226C                   eval      $PRE    = *on
534400130924C2226C                   eval      $RGRPRE = 'P'
534500941027     C                   ENDIF
534600940131     C*
534700940131     C                   ENDSR
534800940223     C/EJECT
534900940131     C************************************************************
535000940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
535100940131     C************************************************************
535200940225     C     DEFOPZ        BEGSR
535300940131     C*
535400940225     C* Questa routine deve valorizzare la schiera $FL1
535500940225     C* con S o N a seconda della utilizzabilità o meno
535600940225     C* dell'opzione correlata
535700940225     C*
535800950301     C* Inizialmente si considerano tutte le opzioni come valide
535900940317     C     1             DO        $DIMOP        X
536000960119     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
536100960117     C                   If        $TX1(X)  <> *Blank
536200940225     C                   MOVE      'S'           $FL1(X)
536300960117     C                   Else
536400960117     C                   Leave
536500960117     C                   EndIf
536600941027     C                   ENDDO
536700940715     C*
536800940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
536900941018     C* con 1 o 0 a seconda della utilizzabilità o meno
537000940715     C* del tasto funzione correlato
537100940715     C*
537200940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
537300940715     C* di compilazione
537400940715     C                   RESET                   $FC1
537500940715     C                   RESET                   $FR1
537600940715     C                   RESET                   $FC2
537700940715     C                   RESET                   $FR2
537800940919     C                   RESET                   $FC3
537900940919     C                   RESET                   $FR3
538000940225     C*
538100940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
538200940225     C* invalido alcune opzioni
538300940225     C                   SELECT
538400940225     C* Se pgm richiamato in ricerca disattivo :
538500090323    >C     OPZL76        WHENEQ    '01'
538600940603     C* F3=Fine
538700940603     C                   Z-ADD     3             X
538800941216     C                   MOVE      '0'           $FC1(X)
538900941216     C                   MOVE      '0'           $FC2(X)
539000941216     C                   MOVE      '0'           $FC3(X)
539100950116     C*
539200940225     C* Se pgm richiamato in interrogazione disattivo :
539300090323    >C     OPZL76        WHENEQ    '05'
539400940225     C* 1=Selezione
539500940225     C                   Z-ADD     1             X
539600940225     C     01            LOOKUP    $OP1(X)                                21
539700940225     C     *IN21         IFEQ      *ON
539800940225     C                   MOVE      'N'           $FL1(X)
539900940225     C                   ENDIF
540000940225     C* 2=Modifica
540100940225     C                   Z-ADD     1             X
540200940225     C     02            LOOKUP    $OP1(X)                                21
540300940225     C     *IN21         IFEQ      *ON
540400940225     C                   MOVE      'N'           $FL1(X)
540500940225     C                   ENDIF
540600940225     C* 3=Copia
540700940225     C                   Z-ADD     1             X
540800940225     C     03            LOOKUP    $OP1(X)                                21
540900940225     C     *IN21         IFEQ      *ON
541000940225     C                   MOVE      'N'           $FL1(X)
541100940225     C                   ENDIF
541200940225     C* 4=Annullamento
541300940225     C                   Z-ADD     1             X
541400940225     C     04            LOOKUP    $OP1(X)                                21
541500940225     C     *IN21         IFEQ      *ON
541600940225     C                   MOVE      'N'           $FL1(X)
541700940225     C                   ENDIF
541800940225     C*
541900090323    >C     OPZL76        WHENEQ    *BLANKS
542000940225     C* Se pgm non richiamato, disattivo subito :
542100941124     C* F8=Selezione seguente
542200941124     C                   Z-ADD     8             X
542300941206     C                   MOVE      '0'           $FC1(X)
542400941206     C                   MOVE      '0'           $FC2(X)
542500940225     C*
542600950301     C* In tutti gli altri casi, disattivo subito :
542700941228     C                   OTHER
542800941228     C*
542900940225     C                   ENDSL
543000941118     C*
543100941118     C* poi, disattivo a seconda delle autorizzazioni :
543200070524    >C*                  IF        AUT___ <> *on
543300070524    >C*                  Z-ADD     1             X
543400070524    >C*    XX            LOOKUP    $OP1(X)                                21
543500070524    >C*    *IN21         IFEQ      *ON
543600070524    >C*                  MOVE      'N'           $FL1(X)
543700070524    >C*                  ENDIF
543800070524    >C*                  ENDIF
543900940225     C*
544000940225     C                   ENDSR
544100940225     C/EJECT
544200940225     C************************************************************
544300940225     C* REPERIMENTO OPZIONI
544400940225     C************************************************************
544500940225     C     REPOPZ        BEGSR
544600940225     C*
544700940314     C     1             DO        $DIMOP        X
544800940128     C*
544900940223     C     $MS1(X)       IFNE      *BLANK
545000940128     C*
545100940223     C                   MOVEL     $MS1(X)       IDMSG
545200950522     C                   CALLB     'XRTVM'
545300940201     C                   PARM                    IDMSG            27
545400940201     C                   PARM                    TXTMSG          100
545500940201     C                   PARM                    ERRMSG            1
545600940128     C*
545700940128     C     ERRMSG        IFNE      *ON
545800940223     C                   MOVEL     TXTMSG        $TX1(X)
545900940128     C                   ELSE
546000940223     C                   MOVEL     *ALL'?'       $TX1(X)
546100941027     C                   ENDIF
546200940128     C*
546300941027     C                   ENDIF
546400940128     C*
546500941027     C                   ENDDO
546600940128     C*
546700940128     C                   ENDSR
546800940506     C/EJECT
546900940506     C************************************************************
547000940506     C* REPERIMENTO TASTI DI FUNZIONE
547100940506     C************************************************************
547200940506     C     REPKEY        BEGSR
547300940506     C                   MOVEA     $FM1          $FM
547400950721     C                   MOVEA     $FC1          $FC
547500940506     C                   EXSR      REPKKK
547600940506     C                   MOVEA     $FX           $FX1
547700940506     C*
547800940506     C                   MOVEA     $FM2          $FM
547900950721     C                   MOVEA     $FC2          $FC
548000940506     C                   EXSR      REPKKK
548100940506     C                   MOVEA     $FX           $FX2
548200940919     C*
548300940919     C                   MOVEA     $FM3          $FM
548400950721     C                   MOVEA     $FC3          $FC
548500940919     C                   EXSR      REPKKK
548600940919     C                   MOVEA     $FX           $FX3
548700940919     C                   ENDSR
548800940506     C/EJECT
548900940506     C************************************************************
549000940506     C* REPERIMENTO TASTI DI FUNZIONE
549100940506     C************************************************************
549200940506     C     REPKKK        BEGSR
549300940506     C*
549400940506     C                   CLEAR                   $FX
549500940506     C     1             DO        24            X
549600940506     C*
549700940506     C     $FM(X)        IFNE      *BLANK
549800950721     C     $Fc(X)        andne     '0'
549900940506     C*
550000940506     C                   MOVEL     $FM(X)        IDMSG
550100950522     C                   CALLB     'XRTVM'
550200940506     C                   PARM                    IDMSG            27
550300940506     C                   PARM                    TXTMSG          100
550400940506     C                   PARM                    ERRMSG            1
550500940506     C*
550600940506     C     ERRMSG        IFNE      *ON
550700940506     C                   MOVEL     TXTMSG        $FX(X)
550800940506     C                   ELSE
550900940506     C                   MOVEL     *ALL' '       $FX(X)
551000941027     C                   ENDIF
551100940506     C*
551200941027     C                   ENDIF
551300940506     C*
551400941027     C                   ENDDO
551500940506     C*
551600940506     C                   ENDSR
551700940223     C/EJECT
551800940128     C************************************************************
551900950606     C* DEFINIZIONE KLIST
552000940128     C************************************************************
552100950606     C     DefKlist      BEGSR
552200940128     C*
552300940223     C* klist
552400090324    >C* AnUni01L
552500090324    >C     K02Uni01      KLIST
552600090324    >C                   KFLD                    UNISocieta
552700090324    >C                   KFLD                    UNIUnita
552800100413    >C* NdRCa01L
552900100413    >C     K07Rca01      KLIST
553000100402    >C                   KFLD                    RcaSocieta
553100100402    >C                   KFLD                    RcaPosiz
553200100402    >C                   KFLD                    RcaKCC
553300100402    >C                   KFLD                    RcaKSC
553400100402    >C                   KFLD                    RcaDtpar
553500100402    >C                   KFLD                    RcaNrpar
553600100402    >C                   KFLD                    RcaSerPar
553700101209c2176c*
553800101209  " >C     K07Rca04      KLIST
553900101209  " >C                   KFLD                    RcaSocieta
554000101209  " >C                   KFLD                    RcaPosiz
554100101209  " >C                   KFLD                    RcaKCC
554200101209  " >C                   KFLD                    RcaKSC
554300101209  " >C                   KFLD                    RcaDtpar
554400101209  " >C                   KFLD                    RcaNrpar
554500101209  "  C                   KFLD                    RcaSerPar
554600101209  "  C*
554700101209  " >C     K08Rca04      KLIST
554800101209  " >C                   KFLD                    RcaSocieta
554900101209  " >C                   KFLD                    RcaPosiz
555000101209  " >C                   KFLD                    RcaKCC
555100101209  " >C                   KFLD                    RcaKSC
555200101209  " >C                   KFLD                    RcaDtpar
555300101209  " >C                   KFLD                    RcaNrpar
555400101209  "  C                   KFLD                    RcaSerPar
555500101209c2176C                   KFLD                    RcaDtSca
555600130923C2226C* Attività Manuali
555700131129  "  C     K08Att01      KLIST
555800130923  "  C                   KFLD                    AttSocieta
555900130923  "  C                   KFLD                    AttKCC
556000130923  "  C                   KFLD                    AttKSC
556100130923  "  C                   KFLD                    AttDtpar
556200130923  "  C                   KFLD                    AttNrpar
556300130923  "  C                   KFLD                    AttSerPar
556400130923  "  C                   KFLD                    AttDtSca
556500130923  "  C                   KFLD                    AttProgr
556600130923  "  C*
556700130923  "  C     K07Att01      KLIST
556800130923  "  C                   KFLD                    AttSocieta
556900130923  "  C                   KFLD                    AttKCC
557000130923  "  C                   KFLD                    AttKSC
557100130923  "  C                   KFLD                    AttDtpar
557200130923  "  C                   KFLD                    AttNrpar
557300130923  "  C                   KFLD                    AttSerPar
557400130923  "  C                   KFLD                    AttDtSca
557500130923C2226C*
557600110412     C                   ENDSR
557700110412D2680C************************************************************
557800110412  "  C* Chiamata a xrtvm
557900110412  "  C************************************************************
558000110412D2680C     PrendiMsg     BEGSR
558100110412     C*
558200110412     C                   CALLB     'XRTVM'
558300110412     C                   PARM                    IDMSG            27
558400110412     C                   PARM                    TXTMSG          100
558500110412     C                   PARM                    ERRMSG            1
558600110412     C                   PARM      *off          CTRMSG            1            centratura
558700110412     C                   PARM                    LENMSG            3 0          lun output.
558800110412     C                   PARM                    MSgDta          100            lun output.
558900110412     C                   PARM                    TxtMsg30         30            lun output.
559000110412     C                   PARM                    TxtMsg3000     3000            lun output.
559100110412     C     ERRMSG        IFNE      *ON
559200110412     C                   MOVEL     TxTMsg        WWMSG           100            *
559300110412     C                   ELSE
559400110412     C                   MOVEL     *ALL'?'       WWMSG
559500110412     C                   ENDIF
559600110412     C*
559700110412D2680C                   ENDSR
559800130910C2226C************************************************************
559900130910  "  C* Reperisco le unità abilititate all'utente
560000130910  "  C************************************************************
560100130910C2226C     CarUniAbi     BEGSR
560200130910     C                   Clear                   WinUni
560300130910     C     XscSoc        setll     AnUni01l
560400130910     c                   do        *hival
560500130910     c     XscSoc        reade     AnUni01l
560600130910     c                   If        %eof(AnUni01l)
560700130910     c                   Leave
560800130910     c                   Endif
560900130910     C* Controllo autorizzazione all'unità
561000130910     C                   RESET                   XAutUniDS
561100130910     C                   EVAL      XAUPrf = KNmUs
561200130910     C                   EVAL      XAUSoc = XscSoc
561300130910     C                   EVAL      XAUUni = UniUnita
561400130910     C                   CALL      'XAUTUNI'
561500130910     C                   PARM                    XAutUniDS
561600130910     C     XAUAbl        IFNE      *OFF
561700130910     C                   If        WinUni = *blanks
561800130910     C                   Eval      WInUni='in (''' + %trimr(UniUnita)
561900130910     C                             + ''''
562000130910     C                   Else
562100130910     C                   Eval      WInUni=%trimr(WinUni) + ',' +''''
562200130910     C                             + %trimr(UniUnita)
562300130910     C                             + ''''
562400130910     C                   Endif
562500130910     C                   ENDIF
562600130910     C                   Enddo
562700130910     C                   If        WinUni <> *blanks
562800130910     C                   Eval      WInUni=%trimr(WinUni) +')'
562900130910     C                   Endif
563000130910     C*
563100130910C2226C                   ENDSR
563200940223     C/EJECT
563300980319** $MS1 / $TX1
563400090323PROMSG    *LIBL     OPZ0001
563500101209PROMSG    *LIBL     OPZ0458                                          C2176
563600090406PROMSG    *LIBL     OPZ0642
563700980319** $OP1 / $FL1
56380009032301S      <Scelta
56390010120915S      <Chiusura fasi partita                                      C2176
56400009040216S      <Interrogazioni
564100130912** $OP2 / $FL2                                                       C2226
56420013091201S      <Scelta                                                     C2226
564300980319** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
564400940506PROMSG    *LIBL     KEY0001  01
564500940506PROMSG    *LIBL     KEY0002  02
564600940506PROMSG    *LIBL     KEY0003  03
564700940506PROMSG    *LIBL     KEY0004  04
564800940506PROMSG    *LIBL     KEY0005  05
564900070531PROMSG    *LIBL     KEY0323  06
565000940506PROMSG    *LIBL     KEY0007  07
565100940506PROMSG    *LIBL     KEY0008  08
565200940506PROMSG    *LIBL     KEY0009  09
565300940506PROMSG    *LIBL     KEY0010  10
565400940506PROMSG    *LIBL     KEY0011  11
565500940506PROMSG    *LIBL     KEY0012  12
565600940506PROMSG    *LIBL     KEY0013  13
565700940506PROMSG    *LIBL     KEY0014  14
565800940506PROMSG    *LIBL     KEY0015  15
565900940506PROMSG    *LIBL     KEY0016  16
566000940506PROMSG    *LIBL     KEY0017  17
566100940506PROMSG    *LIBL     KEY0018  18
566200940506PROMSG    *LIBL     KEY0019  19
566300940506PROMSG    *LIBL     KEY0020  20
566400940506PROMSG    *LIBL     KEY0021  21
566500940506PROMSG    *LIBL     KEY0022  22
566600940506PROMSG    *LIBL     KEY0023  23
566700940506PROMSG    *LIBL     KEY0024  24
566800980319**  ABILITAZIONE D1 : $FC1 / $FR1
5669009407200N  01
5670009507031N  02
5671009407201N  03
5672009407201N  04
5673009407200N  05
5674000903230N  06
5675009407200N  07
5676009412061N  08
5677009407200N  09
5678000705250N  10
5679009407200N  11
5680009407201N  12
5681009407200N  13
5682009407200N  14
5683009407200N  15
5684009407200N  16
5685009407200N  17
5686000705311N  18
5687009407200N  19
5688009407200N  20
5689009407200N  21
5690009407200N  22
5691009507260N  23   Impostare a '1' se si utilizzano opzioni
5692009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
569300980319** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
569400940916PROMSG    *LIBL     KEY0001  01
569500940916PROMSG    *LIBL     KEY0002  02
569600940916PROMSG    *LIBL     KEY0003  03
569700940916PROMSG    *LIBL     KEY0004  04
569800940916PROMSG    *LIBL     KEY0005  05
569900070531PROMSG    *LIBL     KEY0323  06
570000940916PROMSG    *LIBL     KEY0007  07
570100940916PROMSG    *LIBL     KEY0008  08
570200940916PROMSG    *LIBL     KEY0009  09
570300940916PROMSG    *LIBL     KEY0010  10
570400940916PROMSG    *LIBL     KEY0011  11
570500940916PROMSG    *LIBL     KEY0012  12
570600090323PROMSG    *LIBL     KEY0061  13
570700940916PROMSG    *LIBL     KEY0014  14
570800940916PROMSG    *LIBL     KEY0015  15
570900940916PROMSG    *LIBL     KEY0016  16
571000940916PROMSG    *LIBL     KEY0017  17
571100940916PROMSG    *LIBL     KEY0018  18
571200940916PROMSG    *LIBL     KEY0019  19
571300940916PROMSG    *LIBL     KEY0020  20
571400940916PROMSG    *LIBL     KEY0021  21
571500940916PROMSG    *LIBL     KEY0022  22
571600940916PROMSG    *LIBL     KEY0023  23
571700940916PROMSG    *LIBL     KEY0024  24
571800980319**  ABILITAZIONE S1 : $FC2 / $FR2
5719009409160N  01
5720009409160N  02
5721009409161N  03
5722009409160N  04
5723001309241N  05                                       *C2226*
5724000903230N  06
5725009409160N  07
5726009411241N  08
5727009409160N  09
5728000705250N  10
5729009409160N  11
5730009409161N  12
5731000903231N  13
5732009409160N  14
5733009409160N  15
5734009409160N  16
5735009409161N  17
5736000705311N  18
5737009409160N  19
5738009409160N  20
5739009409160N  21
5740009409160N  22
5741009507261N  23   Impostare a '1' se si utilizzano opzioni
5742009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
574300980319** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
574400940506PROMSG    *LIBL     KEY0001  01
574500940506PROMSG    *LIBL     KEY0002  02
574600940506PROMSG    *LIBL     KEY0003  03
574700940506PROMSG    *LIBL     KEY0004  04
574800940506PROMSG    *LIBL     KEY0005  05
574900940506PROMSG    *LIBL     KEY0006  06
575000940506PROMSG    *LIBL     KEY0007  07
575100940506PROMSG    *LIBL     KEY0008  08
575200940506PROMSG    *LIBL     KEY0009  09
575300940506PROMSG    *LIBL     KEY0010  10
575400940506PROMSG    *LIBL     KEY0011  11
575500940506PROMSG    *LIBL     KEY0012  12
575600940506PROMSG    *LIBL     KEY0013  13
575700940506PROMSG    *LIBL     KEY0014  14
575800940506PROMSG    *LIBL     KEY0015  15
575900940506PROMSG    *LIBL     KEY0016  16
576000940506PROMSG    *LIBL     KEY0017  17
576100940506PROMSG    *LIBL     KEY0018  18
576200940506PROMSG    *LIBL     KEY0019  19
576300940506PROMSG    *LIBL     KEY0020  20
576400940506PROMSG    *LIBL     KEY0021  21
576500940506PROMSG    *LIBL     KEY0022  22
576600940506PROMSG    *LIBL     KEY0023  23
576700940506PROMSG    *LIBL     KEY0024  24
576800980319**  ABILITAZIONE D3 : $FC3 / $FR3
5769009407200N  01
5770000903241N  02
5771009407201N  03
5772000903241N  04
5773009407200N  05
5774009407200N  06
5775009407200N  07
5776009407200N  08
5777009407200N  09
5778009409160N  10
5779009407200N  11
5780009407201N  12
5781009407200N  13
5782009407200N  14
5783009407200N  15
5784009407200N  16
5785009409160N  17
5786009409160N  18
5787009407200N  19
5788009407200N  20
5789009407200N  21
5790009407200N  22
5791009507260N  23   Impostare a '1' se si utilizzano opzioni
5792009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
579300110412** MSG
579400110412PROMSG    *LIBL     KEY0012    F12=Ritorno                D2680
579500130924PROMSG    *LIBL     KEY0006    F06=Conferma               C2226
