000100051115     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200051114     H*PARMS BNDDIR(PJXBND PJCBND)
000300051114     H*PARMS CVTOPT(*none)
000400051114     H DECEDIT('0,') DATEDIT(*DMY.)
000500041126      **********************************************************************
000600041126      *
000700150909      *            -------------------------------------
000800150909      *            Collegamento fatture a bolla doganale
000900150909      *            -------------------------------------
001000140701      *
001100140701      **********************************************************************
001200160408R2212X***SdgLRC02l IF   E           k DISK    RENAME(SDGLRC000:SDGLRC002)
001300160408R2212FSdgLRC02l UF   E           k DISK    RENAME(SDGLRC000:SDGLRC002)
001400150925     FNdIVA01l  IF   E           k DISK
001500150929     FNdReg01l  IF   E           k DISK
001600150925     FSdgLRC01l UF A E           k DISK
001700140701      *------------
001800150924     FSDGSD25D  CF   E             WORKSTN
001900150812     F                                     RENAME(SD25D1:FMTD1)
002000150812     F                                     RENAME(SD25D2:FMTD2)
002100150812     F                                     RENAME(SD25D3:FMTD3)
002200150812     F                                     RENAME(SD25S1:FMTS1)
002300150812     F                                     RENAME(SD25C1:FMTC1)
002400150812     F                                     RENAME(SD25D4:FMTD4)
002500150812     F                                     RENAME(SD25S2:FMTS2)
002600150812     F                                     RENAME(SD25C2:FMTC2)
002700150812     F                                     RENAME(SD25T1:FMTT1)
002800150812     F                                     RENAME(SD25Z1:FMTZ1)
002900150812     F                                     RENAME(SD25Z2:FMTZ2)
003000150812     F                                     RENAME(SD25Z3:FMTZ3)
003100160125     F                                     RENAME(SD25D5:FMTD5)
003200160125     F                                     RENAME(SD25Z5:FMTZ5)
003300041213     F                                     SFILE(FMTS1:S1NRR)
003400041214     F                                     SFILE(FMTS2:S2NRR)
003500041213     F                                     INFDS(DSFMT)
003600041213      *----------------------------------------------------*
003700041213      *Schiere opzioni
003800041213     D $DIMOP          C                   CONST(30)
003900041213     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
004000041213     D $TX1            S             50    DIM($dimop) ALT($MS1)
004100041213     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004200041213     D $FL1            S              1    DIM($dimop) ALT($OP1)
004300041213     D $PGO1           S             10    DIM($dimop)
004400041213      *----------------------------------------------------*
004500041213     D $TX             S             50    DIM(50)
004600041213     D $FL             S              1    DIM(50)
004700041213      *----------------------------------------------------*
004800041213      *Schiere tasti funzione generiche
004900041213     D $FM             S             27    DIM(24)
005000041213     D $FX             S             50    DIM(24)
005100041213     D $FC             S              1    DIM(24)
005200041213     D $FR             S              1    DIM(24)
005300041213      *----------------------------------------------------*
005400041213      * Tasti funzione D1
005500041213     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005600041213     D $FX1            S             50    DIM(24) ALT($FM1)
005700041213     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005800041213     D $FR1            S              1    DIM(24) ALT($FC1)
005900041213      *----------------------------------------------------*
006000041213      * Tasti funzione S1
006100041213     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006200041213     D $FX2            S             50    DIM(24) ALT($FM2)
006300041213     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006400041213     D $FR2            S              1    DIM(24) ALT($FC2)
006500041213      *----------------------------------------------------*
006600041213      * Tasti funzione D2
006700041213     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006800041213     D $FX3            S             50    DIM(24) ALT($FM3)
006900041213     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007000041213     D $FR3            S              1    DIM(24) ALT($FC3)
007100041213      *----------------------------------------------------*
007200041213      * schiera nomi pgm richiamati con tasti funzione personalizzati S1
007300041213     D $PGF2           S             10    DIM(25)
007400041213      *----------------------------------------------------*
007500041213      *contiene la dimensione del campo a video che conterrà
007600041213     D $D1L01          S              3  0
007700041213     D $D1L02          S              3  0
007800041213     D $S1L01          S              3  0
007900041213     D $S1L02          S              3  0
008000041213     D $D2L01          S              3  0
008100041213     D $D2L02          S              3  0
008200041213     D $C1Op1          S              3  0
008300041213     D $C1Op2          S              3  0
008400041213      *----------------------------------------------------*
008500041213      * Indice della schiera tasti funzione
008600041213     D PosFun          S              2  0
008700140708     D*-------------
008800041213      *----------------------------------------------------*
008900041213      * Valori assunti
009000041213     D XMPEDS        E DS                  INZ
009100051115     D XMPEDS1       E DS                  INZ
009200041213     D DSMPE           DS                  INZ
009300041213     D  $LOP                   1      1  0
009400041213     D  $ORD                   2      3  0
009500051115     D  $val                   4      4
009600160407R2212X**$DocLeg                            like(D1DocLeg  )
009700160407R2212D  $DocLeg                       1A
009800150910     D  $Sogg                              like(D1Sogg    )
009900150910     D  $KscDog                            like(D1KscDog  )
010000150910     D  $DtRegDa                           like(D1DtRegDa )
010100150910     D  $DtRega                            like(D1DtRegA  )
010200150910     D  $DtDocDa                           like(D1DtDocDa )
010300150910     D  $DtDoca                            like(D1DtDocA  )
010400150923     D  $KscFatt                           like(D1Ksc     )
010500150910     D  $DtRe2Da                           like(D1DtRe2Da )
010600150910     D  $DtRe2a                            like(D1DtRe2A  )
010700150910     D  $DtDo2Da                           like(D1DtDo2Da )
010800150910     D  $DtDo2a                            like(D1DtDo2A  )
010900160407R2212D  $BolLeg                            like(D1BolLeg  )
011000160407R2212D  $FatLeg                            like(D1FatLeg  )
011100041213      *----------------------------------------------------*
011200041213      * Passaggio Parametri
011300041213     D KPJBA         E DS
011400041213      *----------------------------------------------------*
011500041213      * Reperimento Autorizzazioni
011600041213     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
011700041213      *----------------------------------------------------*
011800041213      * Passaggio Autorizzazioni
011900041213     D DSAUT           DS           512
012000051116     D*-------------
012100150923     D* DS gestione parametri modulo
012200051116     D XPAOUT          DS          2000
012300160122     D*-------------
012400170814R258 X**** Parametri modulo SDBLACK2
012500170814R258 X**** SDGPA11DS     E DS
012600041213      *----------------------------------------------------*
012700041213      * Dati di ambiente ottenuti da XSOC
012800041213     D SOC001        E DS                  EXTNAME(XSOC001DS)
012900041222     D    XscDare                     1    overlay(xscFgo:7)
013000041222     D    XscAvere                    1    overlay(xscFgo:8)
013100041213      *------
013200041213      * DS Interna per dati di output di XSOC
013300041213     D XSOCDS          DS          1000
013400041213      *----------------------------------------------------*
013500140714     D* Parametri in ricezione
013600150812     D SDGSD25DS     E DS
013700140701     D*-------------
013800150924     D* Parametri per sql
013900150812     D SDGSd26DS     E DS
014000150924     D  Errd26       E                     INZ('0')
014100160708R2321D*-------------
014200160705 "   D* Chiamata ricorsiva
014300160705 "   D X48CHPGMDS    E DS                  INZ
014400160705 "   D*-------------
014500160705 "   D* Parametri per gestione p.nota interattiva
014600160705 "   D NDC001DS      E DS                  INZ
014700160705 "   D  RET001       E                     INZ('0')
014800160705 "   D  OPR001       E                     INZ('0')
014900160705 "   D  ERR001       E                     INZ('0')
015000160708R2321D*-------------
015100160708R2322D* Parametri del PGM di modifica cod.soggetto SDGSD44R
015200160705  "  D SDGSD44DS     E DS                  INZ
015300160705  "  D  RETD44       E                     INZ('0')
015400160705  "  D  OPRD44       E                     INZ('0')
015500160705  "  D  ERRD44       E                     INZ('0')
015600160705R2322D*-------------
015700150923     D* Ds ndmov
015800150923     D NDMOV00F      E DS
015900160125     D NDMOVSav      E DS                  Extname(NDMOV00F)
016000160125     D                                     Prefix(MOS:3)
016100160125     D* Ds ndreg
016200160125     D NDREG00F      E DS
016300160125     D NDREGSav      E DS                  Extname(NDREG00F)
016400160125     D                                     Prefix(RES:3)
016500160125     D*---------
016600160125     D* Definizioni parametri standard chiamate moduli
016700160125     D $Sys            S                   Like(MOVSys)
016800160125     D $NrAsReg        S                   Like(MOVNrAsReg)
016900160125     D $NrRigaM        s                   like(MOVNrRigaM) inz
017000160125     D $Esito          S              1
017100160125     D $GesMsg         S              1
017200160125     D $LenOut         S              9B 0
017300160125     D $Struttura      S             10
017400160125     D $CodOper        S              1
017500160125     D $Output         S           4000
017600160125     D $Refresh        S              1
017700160125     D $Uso            S              1
017800051115     D*-----------
017900051115     D* Definizioni parametri standard chiamate moduli
018000051115     D Esito           S              1
018100140701     D GesMsg          S              1
018200051115     D LenOut          S              9B 0
018300051115     D OutPut          S           4000
018400051115     D StrutturaO      S             10
018500051115     D Operazione      S              2
018600051115     D Societa         S              3
018700140701     D*-------------
018800140701     D* DS esterna per decodifica conto
018900140701     D ND002DS       E DS
019000051117     D*-------------
019100150929     D* Ds per reperimento dati su causali operative contabili
019200150929     D ANOPE         E DS                  ExtName(ANOPE00F) Inz
019300041213      *----------------------------------------------------*
019400041213      * campo di salvataggio per la DS d'ingresso durante la chiamata
019500041213      * alla funzione personalizzata
019600150812     D $SaveDS         S                   like(SDGSD25DS)
019700041213      *----------------------------------------------------*
019800041213     D DSFMT           DS           512
019900041213     D  $TASTO               369    369
020000041213     D  NRG                  370    370
020100041213     D  NCL                  371    371
020200041222     D**SFLNUM               376    377B 0
020300041213     D  SFLNRR               378    379B 0
020400041222     D**ACTCUR               382    383
020500041223      *----------------------------------------------------*
020600041223      * DS per UserExit
020700041223     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
020800041213      *----------------------------------------------------*
020900041213      * posizione cursore
021000041213     D CURSOR          DS
021100041213     D  RIRI                   1      2B 0 INZ
021200041213     D  R$$                    2      2
021300041213     D  COCO                   3      4B 0 INZ
021400041213     D  C$$                    4      4
021500041213      *----------------------------------------------------*
021600041213      * Reperimento nome PGM
021700041213     D STATUS         SDS           333
021800041213     D  DSPGM            *PROC
021900041213     D  PARMS            *PARMS
022000160705B0325D  Job_Num              264    269S 0
022100041213      *----------------------------------------------------*
022200041213      * Costanti
022300041213      * nr.di righe del sfl1(SFLPAG)
022400041217      * impostato al nr. max delle righe del sfl per fare
022500041217      * il caricamento sfl non a pagine
022600041217     D SflPagS1        C                   CONST(9999)
022700041217      * nr.di righe del sfl2(SFLPAG)
022800041217      * impostato al nr. max delle righe del sfl per fare
022900041217      * il caricamento sfl non a pagine
023000041217     D SflPagS2        C                   CONST(9999)
023100041213      *----------------------------------------------------*
023200041213      * Tasti di funzione
023300041213     D F01             C                   CONST(X'31')
023400041213     D F02             C                   CONST(X'32')
023500041213     D F03             C                   CONST(X'33')
023600041213     D F04             C                   CONST(X'34')
023700041213     D F05             C                   CONST(X'35')
023800041213     D F06             C                   CONST(X'36')
023900041213     D F07             C                   CONST(X'37')
024000041213     D F08             C                   CONST(X'38')
024100041213     D F09             C                   CONST(X'39')
024200041213     D F10             C                   CONST(X'3A')
024300041213     D F11             C                   CONST(X'3B')
024400041213     D F12             C                   CONST(X'3C')
024500041213     D F13             C                   CONST(X'B1')
024600041213     D F14             C                   CONST(X'B2')
024700041213     D F15             C                   CONST(X'B3')
024800041213     D F16             C                   CONST(X'B4')
024900041213     D F17             C                   CONST(X'B5')
025000041213     D F18             C                   CONST(X'B6')
025100041213     D F19             C                   CONST(X'B7')
025200041213     D F20             C                   CONST(X'B8')
025300041213     D F21             C                   CONST(X'B9')
025400041213     D F22             C                   CONST(X'BA')
025500041213     D F23             C                   CONST(X'BB')
025600041213     D F24             C                   CONST(X'BC')
025700041213     D ENTER           C                   CONST(X'F1')
025800041213     D ROLDWN          C                   CONST(X'F4')
025900041213     D ROLLUP          C                   CONST(X'F5')
026000041213      *----------------------------------------------------*
026100051111      * Sottotitolo di testata.
026200140701     D SOTTIT          C                   CONST('               -
026300140701     D                                                 ')
026400140701     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
026500140701     D                                          SDC0001')
026600041213      *----------------------------------------------------*
026700041213      * Variabili per gestione videate
026800041213     D $Gest           S              2A
026900050103      * indica quale sfl gestire in base agli errori oppure al cursore
027000041221     D WGest           S                   like($Gest) inz
027100050103      * indica su quale sfl si è verificato un errore specifico per quel sfl
027200050103     D $SflErr         S                   like($Gest) inz
027300041213     D $Fine           S              1A
027400041213     D $InzD1          S              1A
027500041213     D $InzS1          S              1A
027600041217     D $InzS2          S              1A
027700041213     D $InzD2          S              1A
027800041213     D $LastG          S              2A
027900041213     D $LastV          S              2A
028000041213     D $EndFile        S              1A
028100041213     D $Esci           S              1A
028200041213     D $RcdOK          S              1A
028300041213     D $Unique         S              1A
028400041213      *----------------------------------------------------*
028500041213      * Variabili appoggio sempre presenti in tutte le anagrafiche
028600041221     D WC1Rcd          S                   like(C1Rcd)
028700041221     D WC2Rcd          S                   like(C2Rcd)
028800041221     D S1Nrr           S                   like(C1Rcd)
028900041217     D S2Nrr           S                   like(C2Rcd)
029000041217     D WSflS1          S                   like(C1Nrr)
029100041217     D WSflS2          S                   like(C2Nrr)
029200041217     D WMaxS1          S                   like(C1Rcd)
029300041217     D WMaxS2          S                   like(C2Rcd)
029400041217     D WPagS1          S                   like(C1Rcd)
029500041217     D WPagS2          S                   like(C2Rcd)
029600041213     D WInzS1          S                   like($InzS1)
029700041217     D WInzS2          S                   like($InzS2)
029800150925     D WOpr            S                   like(OprD25)
029900160413R2212D WOprS1          S                   like(OprD25)
030000160413R2212D WOprS2          S                   like(OprD25)
030100041213     D CurR            S              2S 0
030200041213     D CurC            S              2S 0
030300041213     D Win             S             99A
030400041213     D W1Pos           S                   like(C1Pos)
030500041213     D WINKR           S              1A
030600041213     D WExf            S              1A
030700150922     D* Variabili di comodo
030800140701     D $Kcc            S              6
030900140701     D $Ksc            S              8
031000151009     D $LegBol         S              1A
031100160122     D $FoundLRC       S              1A
031200151009     D $SoggFor        S              8
031300150922     D $LegFat         S              1A
031400150922     D $NdIva          S              1A
031500150929     D $NdReg          S              1A
031600150929     D $Anope          S              1A
031700150929     D $Causale        S                   like(MovCausTes)
031800150922     D $imponib        S                   like(IvaImponiB)
031900150922     D $iva            S                   like(IvaImporto)
032000150922     D $Aliq           S                   like(IvaAliq)
032100150922     D $RifIva         S                   like(IvaRifIva)
032200160418R2212D $FineLRC        S              1A
032300160421R2212D $TastoSav       S                   like($Tasto   )
032400160421R2212D $TastoSav2      S                   like($Tasto   )
032500041213      *----------------------------------------------------*
032600041213      * Variabili appoggio specifiche della singola anagrafica
032700041213      *  campi per parzializzazioni su rcd (solo alfanumerici)
032800041213      *----------------------------------------------------*
032900041213      *  Indici
033000160301     D X               S              4S 0
033100160301     D Y               S              4S 0
033200041213      *----------------------------------------------------*
033300041213      * Indice della schiera per le opzioni personalizzate
033400041213     D PosOpz          S              2  0
033500041213      * numero max opzioni
033600041213     D NumOpz          S              2  0 Inz($DIMOP)
033700041213      * numero effettive opzioni
033800041213     D OpzLen          S              2  0
033900140701     D*-------------
034000150911     D* ricerca soggetti
034100150911     D ANA040DS      E DS                  INZ
034200150911     D*-------------
034300150911     D* DS esterna per dati soggetti
034400150911     D DVASOG        E DS                  INZ
034500150911     D*-------------
034600150911     D* Definizioni parametri driver anagrafici
034700150911     D Dvaric          S              1
034800150911     D DvaSocieta      S              3
034900150911     D DvaUnita        S              8
035000150911     D DvaStrutt       S             10
035100150911     D DvaDtRif        S               D
035200150911     D DvaLenOut       S              9B 0
035300150911     D DvaSnatura      S              1
035400150911     D DvaCodice       S              8
035500150911     D DvaLineav       S              3
035600150911     D DvaFiliale      S              3  0
035700150911     D DvaSogg         S              8
035800150911     D DvaTpInd        S              2
035900150911     D DvaCdInd        S              3
036000150911     D DvaErrore       S              1
036100150911     D DvaOutput       S           4000
036200150911     D*-------------
036300140701     D* Ricerca  clienti
036400140701     D Ana300Ds      E DS
036500140701     D  Ret300       E                     INZ('0')
036600140701     D  Opr300       E                     INZ('0')
036700140701     D  Err300       E                     INZ('0')
036800150908     D*-------------
036900150908     D* Ricerca  fornitori
037000150908     D Ana400Ds      E DS
037100150908     D  Ret400       E                     INZ('0')
037200150908     D  Opr400       E                     INZ('0')
037300150908     D  Err400       E                     INZ('0')
037400140701      *-------------------------------------------------------*
037500051115      * Ricerca legende
037600051115     D Ana964DS      E DS                  inz
037700041213      *-------------------------------------------------------*
037800041213      * Controlli validità legende
037900051115     D X07ValDS      E DS                  inz
038000041213      *----------------------------------------------------*
038100041213      * Richiamo a XATB
038200051115     D XAtbDS        E DS                  inz
038300041213      *----------------------------------------------------*
038400041213      * Parametri per ricerca codice ANATB00F
038500041213     D XTabDS        E DS                  inz
038600051115      *----------------------------------------------------*
038700041213     D Refresh         S              1
038800150915     D*----------------------------------------------------*
038900150915     D Dt0             C                   CONST('0001-01-01')
039000150915     D Dt9             C                   CONST('9999-12-31')
039100150915     D*----------------------------------------------------*
039200041213     D $DtIso1         s               D
039300041213      *----------------------------------------------------*
039400041213      * Attributi video
039500041213     D Normale         C                   Const(X'32')
039600041213     D Protetto        C                   Const(X'A0')
039700150911      * Data di 10 x controlli
039800150925     D DTRegDAamg      S               d
039900150925     D DTRegAamg       S               d
040000150925     D DTRe2DAamg      S               d
040100150925     D DTRe2Aamg       S               d
040200150925     D DTDocDAamg      S               d
040300150925     D DTDocAamg       S               d
040400150925     D DTDo2DAamg      S               d
040500150925     D DTDo2Aamg       S               d
040600041222      * DS Esterna gestione messaggi
040700041222     D XMSGDS        E DS
040800041222     D  STS          E                     EXTFLD(MSGSTS)
040900041222     D                                     DIM(333)
041000041222     D  JBA          E                     EXTFLD(MSGJBA)
041100041222     D                                     DIM(502)
041200140701     D  MSGRSP       E                     INZ('N')
041300140701     D  MSGEMV       E                     INZ('S')
041400140701     D  MSGMSG       E                     INZ(0)
041500140701     D  MSGRGP       E                     INZ(10)
041600140701     D  MSGCNL       E                     INZ('N')
041700140701     D  MSGVID       E                     INZ('N')
041800140701     D  MSGOPE       E                     INZ('N')
041900140701     D  MSGSTP       E                     INZ('N')
042000140701     D  MSGLCK       E                     INZ('N')
042100041222      *----------------------------------------------------*
042200051115      * campi di lavoro
042300160705R2322D Job_Num_A       S              6A
042400160705R2322D USName          S             10A
042500041222     D XMsgInz         s              1a   inz('1')
042600051115     D ZdateIso        s               D
042700051115     D DtopDa          S               D
042800051115     D DtopA           S               D
042900051115     D $DataIOK        S              1A   Inz('1')
043000051115     D $DataFOK        S              1A   Inz('1')
043100051116     D NrRicMov        S              9  0 inz
043200051116     D NrRicDa         S              9  0 inz
043300051116     D NrRicA          S              9  0 inz
043400140714     D Savopz01        S              1  0
043500140714     D Savopz02        S              1  0
043600160408R2212D SavDel01        S              1  0
043700160408R2212D SavDel02        S              1  0
043800160408     D Savdocleg01     S              1
043900160408     D Savdocleg02     S              1
044000150923     D SavSys01        S                   Like(LrcSys)
044100150923     D SavNrAs01       S                   like(LrcNrAsReg)
044200150923     D SavSys02        S                   Like(LrcSysL)
044300150923     D SavNrAs02       S                   like(LrcNrAsL)
044400150925     D SavSogg02       S                   like(Sogg002)
044500150925     D Wsogg           S                   like(D1Sogg)
044600141007     D datmin          C                   const('0001-01-01')
044700141007     D* flag di NO controllo scostamento cambio/importo
044800141007     D NoCtrlScos      S              1A
044900141007      *----------------------------------------------------*
045000041222      *  Parametro da passare a XNMR
045100041222     D XNMDat          s               D
045200041222     D XNMDag          s               D
045300141007     D*------------
045400160122     D* Campi di comodo per gestione date
045500160122     D Comodo4         S              4S 0
045600160122     D AAALFA          S              4A
045700160122     D DataAlfa        S             10A
045800160122     D $DataIso        S               D
045900041213      ******************************************************
046000041213      *                MAIN LINE PROGRAM
046100041213      ******************************************************
046200041213      *
046300041213      * inizializzazione variabili
046400041213     C                   EXSR      INZVAR
046500041213      *
046600041213     C     $FINE         DOWEQ     *OFF
046700041213     C     $GEST         CASEQ     'D1'          GESD1
046800041213     C     $GEST         CASEQ     'S1'          GESS1
046900041217     C     $GEST         CASEQ     'S2'          GESS2
047000041213     C     $GEST         CASEQ     'D2'          GESD2
047100041213     C                   ENDCS
047200041213     C                   ENDDO
047300041213      * fine programma
047400041213     C                   EXSR      ENDPGM
047500041213      *
047600041213      ************************************************************
047700041213      * FINE PROGRAMMA
047800041213      ************************************************************
047900041213     C     ENDPGM        BEGSR
048000060103     c*
048100060103     c* se non vi sono stati errori iniziali
048200150910     c                   if        ErrD25 <> '1'
048300051115      *
048400051115     C                   COMMIT
048500060103      *
048600060103     c                   endif
048700060103      *
048800060103      * passaggio dati al pgm chiamante
048900150910     C                   MOVEL     WOPR          OPRD25
049000150812     C                   MOVEL     SDGSD25DS     KPJBU
049100041213      *
049200050126      * Se pgm Guida non è stato richiamato posso chiudere in LR
049300150910     C     OPZD25        IFEQ      *BLANK
049400050126     C                   SETON                                            LR
049500050126      **altrimenti chiudo in RT
049600050126     C                   ELSE
049700050126     C                   SETON                                            RT
049800050126     C                   ENDIF
049900041213     C                   RETURN
050000041213      *
050100041213     C     XEndPgm       ENDSR
050200041213      ************************************************************
050300041213      * RICERCHE
050400041213      ************************************************************
050500041213     C     SEARCH        BEGSR
050600041213      *
050700041213      * determino Riga/Colonna del cursore
050800041213     C                   MOVE      NRG           R$$
050900041213     C                   MOVE      NCL           C$$
051000041213     C                   Z-ADD     RIRI          CURR
051100041213     C                   Z-ADD     COCO          CURC
051200041213      *
051300140701     C     D1Ksc         IFNE      *BLANK
051400140701     C                   CALLb     'XALLINEA'
051500140701     C                   PARM                    D1Ksc
051600140701     C                   PARM      8             LEN               5 0
051700140701     C                   PARM                    XSCINP
051800140701     C                   ENDIF
051900150911      *
052000150911     C     D1KscDog      IFNE      *BLANK
052100150911     C                   CALLb     'XALLINEA'
052200150911     C                   PARM                    D1KscDog
052300150911     C                   PARM      8             LEN               5 0
052400150911     C                   PARM                    XSCINP
052500150911     C                   ENDIF
052600140701      *
052700150911     C     D1Sogg        IFNE      *BLANK
052800150911     C                   CALLb     'XALLINEA'
052900150911     C                   PARM                    D1Sogg
053000150911     C                   PARM      8             LEN               5 0
053100150911     C                   PARM                    XSCINP
053200150911     C                   ENDIF
053300041213     C                   SELECT
053400150911     C*
053500150911     C* FMTD1 - Soggetto
053600150911     C     H1NMFL        WHENEQ    'D1SOGG  '
053700150911     C*
053800150911     C                   RESET                   ANA040DS
053900150911     C*
054000150911     C                   MOVEL     '01'          OPZ040
054100150911     C                   MoveL     D1Sogg        Cod040
054200150911     C                   MOVEL     ANA040DS      KPJBU
054300150911     C                   CALL      'ANA040R'
054400150911     C                   PARM                    KPJBA
054500150911     C*
054600150911     C                   MOVEL     KPJBU         ANA040DS
054700150911     C*
054800150911     C                   if        Ret040     <> '2'
054900150911     C                   MoveL     Cod040        D1Sogg
055000150911     C                   endif
055100160407R2212C*
055200160407 "   C* FMTD1 - Bolle legate
055300160407 "   C     H1NMFL        WHENEQ    'D1BOLLEG'
055400160407 "   C                   EXSR      F4_BolLeg
055500160407 "   C*
055600160407 "   C* FMTD1 - Fatture legate
055700160407 "   C     H1NMFL        WHENEQ    'D1FATLEG'
055800160407R2212C                   EXSR      F4_FatLeg
055900140701     C*
056000150911     C* FMTD1 - Ricerca fornitore (dogana)
056100150911     C     H1NMFL        WHENEQ    'D1KSCDOG'
056200150911     C                   Reset                   Ana400DS
056300150911     C                   Movel     '01'          Opz400
056400150911     C                   Eval      Ksc400   =    D1KscDog
056500150911     C                   Movel     Ana400Ds      KPJBU
056600150911     C                   Call      'ANA400R'
056700150911     C                   Parm                    KPJBA
056800150911     C                   Movel     KPJBU         Ana400DS
056900150911     C*
057000150911     C     Ksc400        IFNE      *BLANK
057100150911     C                   MOVEL     Ksc400        D1KscDog
057200150911     C                   ENDIF
057300150908      *
057400150911     C* FMTD1  - Codice Fornitore
057500150908     C     H1NMFL        WHENEQ    'D1KSC'
057600150908     C                   Reset                   Ana400DS
057700150908     C                   Movel     '01'          Opz400
057800150908     C                   eval      Ksc400   =    D1Ksc
057900150908     C                   Movel     Ana400Ds      KPJBU
058000150908     C                   Call      'ANA400R'
058100150908     C                   Parm                    KPJBA
058200150908     C                   Movel     KPJBU         Ana400DS
058300150908     C*
058400150908     C     Ksc400        IFNE      *BLANK
058500150908     C                   MOVEL     Ksc400        D1Ksc
058600150908     C                   ENDIF
058700051115     C*
058800041213      * ricerca non abilitata in questa posizione
058900041213     C                   OTHER
059000041213     C                   SETON                                        9899
059100041213      *
059200041213     C                   ENDSL
059300041213      *
059400041213      * imposto pos. del cursore
059500041213     C                   Z-ADD     CURR          H1RIGA
059600041213     C                   Z-ADD     CURC          H1COLO
059700041213      *
059800041213     C                   MOVE      *HIVAL        $LASTV
059900041213      *
060000041213     C     XSearch       ENDSR
060100160407R2212C************************************************************
060200160407 "   C* Ricerca Bolla legate
060300160407 "   C************************************************************
060400160407R2212C     F4_BolLeg     BEGSR
060500160407     C*
060600160407     C                   Reset                   ANA964DS
060700160407     C*
060800160407     C                   MoveL     '01'          Opz964
060900160407     C                   MoveL     'SDGSD25S '   Trc964
061000160407     C                   MoveL     'BOLLEGD25'   Cam964
061100160407     C                   Move      *On           All964
061200160407     C*
061300160407     C                   MoveL     ANA964DS      KPJBU
061400160407     C                   Call      'ANA964R'
061500160407     C                   Parm                    KPJBA
061600160407     C                   MoveL     KPJBU         ANA964DS
061700160407     C*
061800160407     C                   if        Ret964     <> '2'
061900160407     C                   MoveL     Val964        D1BolLeg
062000160407     C                   endif
062100160407     C*
062200160407R2212C                   ENDSR
062300160407R2212 /EJECT
062400160407R2212C************************************************************
062500160407 "   C* Ricerca fatture legate
062600160407 "   C************************************************************
062700160407R2212C     F4_FatLeg     BEGSR
062800160407     C*
062900160407     C                   Reset                   ANA964DS
063000160407     C*
063100160407     C                   MoveL     '01'          Opz964
063200160407     C                   MoveL     'SDGSD25S '   Trc964
063300160407     C                   MoveL     'FATLEGD25'   Cam964
063400160407     C                   Move      *On           All964
063500160407     C*
063600160407     C                   MoveL     ANA964DS      KPJBU
063700160407     C                   Call      'ANA964R'
063800160407     C                   Parm                    KPJBA
063900160407     C                   MoveL     KPJBU         ANA964DS
064000160407     C*
064100160407     C                   if        Ret964     <> '2'
064200160407     C                   MoveL     Val964        D1FatLeg
064300160407     C                   endif
064400160407     C*
064500160407R2212C                   ENDSR
064600160407R2212 /EJECT
064700041213      ************************************************************
064800041213      * RIEMPIMENTO OPZIONI
064900041213      ************************************************************
065000041213     C     RIEOPZ        BEGSR
065100041213      *
065200041213      * Input :
065300041213      * - $TX = schiera con tutti i testi disponibili
065400041213      * - $FL = schiere con i flag di validità associati ai testi
065500041213      * - WCOZ = variabile contenente l'indice dell'ultimo testo
065600041213      *          opzione usato
065700041213      * - WMXO = variabile contenente il numero massimo di testi
065800041213      *          opzione usabili
065900041213      * - $L01 = Lunghezza prima riga
066000041213      * - $L02 = Lunghezza seconda riga
066100041213      * Output :
066200041213      * - WOPZ1 = variabile contenente la prima riga opzioni
066300041213      * - WOPZ2 = variabile contenente la seconda riga opzioni
066400041213      * - FF23  = flag che indica se c'è bisogno del tasto F23
066500041213      *
066600041213     C                   CALLB     'X04OPMSG'
066700041213      * input
066800041213     C                   PARM                    $TX
066900041213     C                   PARM                    $FL
067000041213     C                   PARM                    WCOZ              3 0
067100041213     C                   PARM                    WMXO              3 0
067200041213      * output
067300041213     C                   PARM                    WOPZ1            76
067400041213     C                   PARM                    WOPZ2            76
067500041213     C                   PARM                    FF23              1
067600041213      * input
067700041213     C                   PARM                    $L01              3 0
067800041213     C                   PARM                    $L02              3 0
067900041213      *
068000150910     C                   PARM                    $ALLOPT         960
068100150910     C*
068200041213     C     XRieOpz       ENDSR
068300041213      ************************************************************
068400041213      * RIEMPIMENTO TASTI FUNZIONE
068500041213      ************************************************************
068600041213     C     RIEKEY        BEGSR
068700041213      *
068800041213      * Input :
068900041213      * - $FX = schiera con tutti i testi disponibili
069000041213      * - $FC = schiere con i flag di validità associati ai testi
069100041213      * - $FR = schiere con i flag di REVERSE IMAGE
069200041213      * - $ULK = ultimo testo caricato
069300041213      * - $L01 = Lunghezza prima riga
069400041213      * - $L02 = Lunghezza seconda riga
069500041213      * Output :
069600041213      * - $ULK
069700041213      * - $KEY1= 1° riga di tasti funzione
069800041213      * - $KEY2= 2° riga di tasti funzione
069900041213      *
070000041213     C                   CALLB     'XKEYMSG'
070100041213      * input
070200041213     C                   PARM                    $FX
070300041213     C                   PARM                    $FC
070400041213     C                   PARM                    $FR
070500041213     C                   PARM                    $ULK              3 0
070600041213      * output
070700041213     C                   PARM                    $KEY1            79
070800041213     C                   PARM                    $KEY2            79
070900041213      * input
071000041213     C                   PARM                    $L01              3 0
071100041213     C                   PARM                    $L02              3 0
071200041213      *
071300150910     C                   PARM                    $ALLFUNCT       480
071400150910     C*
071500041213     C     XRieKey       ENDSR
071600041213      ************************************************************
071700041213      * GESTIONE EMISSIONE TASTI FUNZIONE
071800041213      ************************************************************
071900041213     C     GESPIE        BEGSR
072000041213      *
072100041213     C                   SELECT
072200041213      * Parzializzazioni
072300041213     C     $GEST         WHENEQ    'D1'
072400041213     C     $ULKD1        IFEQ      0
072500041213      * riempimento opzioni
072600041213     C                   EXSR      F24D1
072700041213     C                   ELSE
072800041213     C                   WRITE     FMTZ1
072900041213     C                   ENDIF
073000041213      * Lista
073100041217     C                   when      $Gest='S1' or $Gest='S2'
073200041217     C                   if        $UlkS1=0
073300041217     C                   exsr      F24S1
073400041217     C                   else
073500041213     C                   WRITE     FMTZ2
073600041217     C                   endif
073700041213      * Modifica valori assunti
073800041213     C     $GEST         WHENEQ    'D2'
073900041213     C     $ULKD2        IFEQ      0
074000041213     C                   EXSR      F24D2
074100041213     C                   ELSE
074200041213     C                   WRITE     FMTZ3
074300041213     C                   ENDIF
074400041213     C                   ENDSL
074500041213      *
074600041213     C     XGesPie       ENDSR
074700041213      ************************************************************
074800041213      * GESTIONE PARZIALIZZAZIONI
074900041213      ************************************************************
075000041213     C     GESD1         BEGSR
075100160414R2212C*
075200160414 "   C                   MOVEL     *OFF          WOPRS1
075300160414R2212C                   MOVEL     *OFF          WOPRS2
075400041213      * inizializzazione videata
075500041213     C     $INZD1        IFEQ      *ON
075600041213     C                   EXSR      INZD1
075700041213     C                   MOVE      *OFF          $INZD1
075800041213     C                   ENDIF
075900041213      * emissione piede videata se proveniente da altra
076000041213      * salvataggio valore formato di provenienza
076100041213     C     $LASTV        IFNE      'D1'
076200140701     C* recupero il sottotitolo (se c'è)
076300140701     C                   Clear                   D1TIT
076400140701     C     SOTTITD1      IFNE      *BLANK
076500140701     C                   MoveL     SOTTITD1      IDMSG
076600140701     C                   CallB     'XRTVM'
076700140701     C                   Parm                    IDMSG            27
076800140701     C                   Parm                    TXTMSG          100
076900140701     C                   Parm                    ERRMSG            1
077000140701     C                   Parm      *Off          CTRMSG            1            centratura
077100140701     C                   Parm      15            LENMSG            3 0          lun output.
077200140701     C     ERRMSG        IFNE      *ON
077300140701     C                   MoveL     TXTMSG        D1TIT
077400140701     C                   ElSe
077500140701     C                   MoveL     *ALL'?'       D1TIT
077600140701     C                   EndIf
077700140701     C                   EndIf
077800041213      * salvo stato F17 per sapere cosa fare in caso di F12
077900041213     C                   MOVE      *INKR         WINKR
078000041213     C                   WRITE     FMTT1
078100041213     C                   EXSR      GESPIE
078200041213     C                   MOVE      $LASTV        $LASTG
078300041213     C                   MOVE      'D1'          $LASTV
078400041213     C                   ENDIF
078500041213      *
078600041213      * se esistono errori sulla videata
078700041213      * emetto la write del formato a indicatori spenti per vedere
078800041213      * le eventuali decodifiche
078900041213     C     *IN99         IFEQ      *ON
079000041213     C                   MOVEA     *IN           WIN
079100041213     C                   MOVE      *ALL'0'       IN5098           49
079200041213     C                   MOVEA     IN5098        *IN(50)
079300041213     C                   WRITE     FMTD1
079400041213     C                   MOVEA     WIN           *IN
079500041213     C                   ENDIF
079600041213      *
079700041213     C                   EXFMT     FMTD1
079800041213      *
079900041213      * azzero la posizione del cursore
080000041213     C                   Z-ADD     0             H1RIGA
080100041213     C                   Z-ADD     0             H1COLO
080200041213      *
080300041213      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
080400041213      * INPUT
080500041213      * $FC1 = Abilitazioni videata D1
080600041213      * OUTPUT
080700041213      * *IN97 + *IN99 Errori
080800041213     C                   MOVEA     $FC1          $FC
080900041213     C                   EXSR      CTRKEY
081000041213      *
081100041213     C     *IN99         IFEQ      *OFF
081200041213      *
081300041213     C                   SELECT
081400041213      * F2=Decodifica
081500041213     C     $TASTO        WHENEQ    F02
081600041213     C                   EXSR      F02D1
081700041213      * F3=Fine
081800041213     C     $TASTO        WHENEQ    F03
081900041213     C                   EXSR      F03D1
082000041213      * F4=Ricerca
082100041213     C     $TASTO        WHENEQ    F04
082200041213     C                   EXSR      SEARCH
082300041213      * F5=Refresh
082400041213     C     $TASTO        WHENEQ    F05
082500041213     C                   EXSR      F05D1
082600041213      * F8=Successivo
082700041213     C     $TASTO        WHENEQ    F08
082800041213     C                   EXSR      F08D1
082900041213      * F12=Ritorno
083000041213     C     $TASTO        WHENEQ    F12
083100041213     C                   EXSR      F12D1
083200041213      * F18=Modifica valori assunti
083300041213     C     $TASTO        WHENEQ    F18
083400041213     C                   EXSR      F18D1
083500041213      * F24=Altri tasti
083600041213     C     $TASTO        WHENEQ    F24
083700041213     C                   EXSR      F24D1
083800041213      *
083900041213     C                   OTHER
084000041213     C                   EXSR      CTRD1
084100041213     C     *IN99         IFEQ      *OFF
084200041213      * definisco l'ordinamento
084300041213     C                   EXSR      DEFORD
084400041213      * richiamo la lista
084500041213     C                   MOVE      'S1'          $GEST
084600041213     C                   MOVE      *ON           $INZS1
084700041213     C                   MOVE      *BLANK        W1POS
084800041217     C                   MOVE      *ON           $INZS2
084900051115      * aggiorno valori assunti sempre
085000051115     C                   EXSR      AGGMPE
085100041213     C                   ENDIF
085200041213      *
085300041213     C                   ENDSL
085400041213      *
085500041213     C                   ENDIF
085600041213      *
085700041213     C     XGesD1        ENDSR
085800041213      ************************************************************
085900041213      * INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
086000041213      ************************************************************
086100041213     C     INZD1         BEGSR
086200041213      *
086300041213     C                   CLEAR                   FMTD1
086400160407R2212X****               Eval      D1On1 = XscOn
086500160407 "   X****               Eval      D1Off1 = XscOff
086600160407R2212X****               Eval      D1DocLeg = XscOff
086700160407R2212C*
086800160407 "   C                   Eval      D1BolLeg  = '0'
086900160407R2212C                   Eval      D1FatLeg  = '0'
087000160122     C*
087100160122     C                   if        TPDTRIFA11 = 'R'
087200160125     C                   Eval      *In15      = *On
087300160122     C                   else
087400160125     C                   Eval      *In16      = *On
087500160122     C                   endif
087600150911     C*
087700150911     C* imposto i valori da quelli memorizzati
087800150911     C                   if        $val = xscon
087900160407R2212X****               eval      D1Docleg  = $Docleg
088000160407R2212C                   eval      D1BolLeg  = $BolLeg
088100160407R2212C                   eval      D1FatLeg  = $FatLeg
088200150911     C                   eval      D1Sogg    = $Sogg
088300150911     C                   eval      D1KscDog  = $KscDog
088400150911     C                   eval      D1DtRegDa = $DtRegDa
088500150911     C                   eval      D1DtRegA  = $DtRegA
088600150911     C                   eval      D1DtDocDa = $DtDocDa
088700150911     C                   eval      D1DtDocA  = $DtDocA
088800150923     C                   eval      D1Ksc     = $KscFatt
088900150911     C                   eval      D1DtRe2Da = $DtRe2Da
089000150911     C                   eval      D1DtRe2A  = $DtRe2A
089100150911     C                   eval      D1DtDo2Da = $DtDo2Da
089200150911     C                   eval      D1DtDo2A  = $DtDo2A
089300160122     C                   else
089400160122     C                   Eval      Comodo4     = *Year - 1
089500160122     C                   Move      Comodo4       AAAlfa
089600160122     C                   Eval      DataAlfa    = AAAlfa + '-' +
089700160122     C                                           '01'   + '-' +
089800160122     C                                           '01'   + '-'
089900160122     C                   Move      DataAlfa      $DataIso
090000160122      *
090100160122     C                   if        TPDTRIFA11  = 'R'
090200160122     C     *Jobrun       Move      $DataIso      D1DtRegDa
090300160122     C     *Jobrun       Move      $DataIso      D1DtRe2Da
090400160122     C                   else
090500160122     C     *Jobrun       Move      $DataIso      D1DtDocDa
090600160122     C     *Jobrun       Move      $DataIso      D1DtDo2Da
090700160122     C                   endif
090800160122      *
090900160122      *
091000160122     C                   Eval      DataAlfa   =  AAAlfa + '-' +
091100160122     C                                           '12'   + '-' +
091200160122     C                                           '31'   + '-'
091300160122     C                   Move      DataAlfa      $DataIso
091400160122      *
091500160122     C                   if        TPDTRIFA11  = 'R'
091600160122     C     *Jobrun       Move      $DataIso      D1DtRegA
091700160122     C     *Jobrun       Move      $DataIso      D1DtRe2A
091800160122     C                   else
091900160122     C     *Jobrun       Move      $DataIso      D1DtDocA
092000160122     C     *Jobrun       Move      $DataIso      D1DtDo2A
092100150911     C                   endif
092200160122     C                   endif
092300041213      *
092400041213      * Se pgm  richiamato con parametri forniti , li imposto nelle
092500041213      *    parzializzazioni
092600041213     C                   SELECT
092700041213      * il pgm è richiamato e
092800041213      * sono stati passati dei valori di parzializzazione ma non
092900041213      * la chiave univoca
093000150910     C     OPZD25        WHENNE    *BLANK
093100160407R2212X****               If        DocLegD25 = *on
093200160407 "   X****               Eval      D1DocLeg = XscOn
093300160407R2212X****               Endif
093400160407R2212C*
093500160407 "   C                   Eval      D1BolLeg    = BolLegD25
093600160407 "   C                   Eval      D1FatLeg    = FatLegD25
093700160407R2212C*
093800150911     C                   Eval      D1Sogg = SoggD25
093900150924     C                   Eval      D1KscDog = KscDogD25
094000150911     C                   Eval      D1Ksc    = KscD25
094100160118     C*
094200160701R2313C*
094300160701 "   C* richiamo con passaggio numero assoluto di registrazione
094400160701 "   C* della bolla per recupero dati della bolla da impostare
094500160701 "   C* nel filtro (es. da NDC039R filtro di PN IVA)
094600160701 "   C                   if        OPZD25      = 'A1'
094700160624 "   C                   EXSR      RepNDMOV
094800160701 "   C                   else
094900160701R2313C                   if        DtRegDaD25 <> *Loval
095000160118     C     *Jobrun       Move      DtRegDaD25    D1DtRegDa
095100160701R2313C                   endif
095200160701R2313C                   if        DtRegAD25  <> *Loval
095300160118     C     *Jobrun       Move      DtRegAD25     D1DtRegA
095400160701R2313C                   endif
095500160701 "   C                   if        DtDocDaD25 <> *Loval
095600160701 "   C     *Jobrun       Move      DtDocDaD25    D1DtRegDa
095700160701 "   C                   endif
095800160701 "   C                   if        DtDocAD25  <> *Loval
095900160701 "   C     *Jobrun       Move      DtDocAD25     D1DtRegA
096000160701 "   C                   endif
096100160701 "   C*
096200160701 "   C                   endif
096300160701 "   C*
096400160701R2313C                   if        DtRe2DaD25 <> *Loval
096500160118     C     *JobRun       Move      DtRe2DaD25    D1DtRe2Da
096600160701R2313C                   endif
096700160701R2313C                   if        DtRe2AD25  <> *Loval
096800160118     C     *JobRun       Move      DtRe2AD25     D1DtRe2A
096900160701R2313C                   endif
097000160701R2313C*
097100160701R2313X**** *Jobrun       Move      DtDocDaD25    D1DtRegDa
097200160701R2313X**** *Jobrun       Move      DtDocAD25     D1DtRegA
097300160701R2313C*
097400160701R2313C                   if        DtDo2DaD25 <> *Loval
097500160624     C     *JobRun       Move      DtDo2DaD25    D1DtRe2Da
097600160701R2313C                   endif
097700160701R2313C                   if        DtDo2AD25  <> *Loval
097800160118     C     *JobRun       Move      DtDo2AD25     D1DtRe2A
097900160701R2313C                   endif
098000150911     C*
098100041213      *
098200041213      * il pgm non è richiamato
098300150910     C     OPZD25        WHENEQ    *BLANK
098400150911     C                   if        $val <> xscon
098500150911     C                   endif
098600050110      *
098700041213     C                   ENDSL
098800041213      *
098900041213      * imposto quei campi che comunque hanno bisogno di un default
099000041213      *
099100041213      * Attributi video
099200041213     C                   exsr      AtrD1
099300041213      *
099400041213     C     XInzD1        endsr
099500160701R2313C************************************************************
099600160701 "   C* REPERISCO il ksc del fornitore e la data DI REGISTRAZIONE
099700160701 "   C************************************************************
099800160701R2313C     RepNDMOV      Begsr
099900160624     C*
100000160624     C* salvo i dati del movimento che ho in linea
100100160624     C                   MoveL     NDMOV00F      NDMOVSAV
100200160624     C*
100300160624     C                   Eval      $Sys        = SySD25
100400160624     C                   Eval      $NrAsReg    = NrAsRegD25
100500160624     C*
100600160624     C                   EXSR      GetNDMOV
100700160624     C                   if        $CodOper   <> '9'
100800160701     C                   if        TPDTRIFA11 = 'R'
100900160701     C     *Jobrun       Move      MOVDtReg      D1DtRegDa
101000160701     C     *Jobrun       Move      MOVDtReg      D1DtRegA
101100160701     C                   else
101200160701     C     *Jobrun       Move      MOVDtDoc      D1DtDocDa
101300160701     C     *Jobrun       Move      MOVDtDoc      D1DtDocA
101400160701     C                   endif
101500160624     C                   Eval      D1KscDog    = MOVKsc
101600160624     C                   endif
101700160624     C*
101800160624     C* ripristino i dati del movimento che ho in linea
101900160624     C                   MoveL     NDMOVSAV      NDMOV00F
102000160701R2313C*
102100160701R2313C                   ENDSR
102200041213      ************************************************************
102300041213      * ATTRIBUTI D1
102400041213      ************************************************************
102500041213     C     AtrD1         Begsr
102600041213      *
102700041213     C     XAtrD1        Endsr
102800041213      ************************************************************
102900150812      * GESTIONE F02 VIDEO D1
103000041213      ************************************************************
103100041213     C     F02D1         BEGSR
103200041213      *
103300041213      * per decodificare la videata richiamo la routine dei ctrl
103400041213      * e poi disattivo gli eventuali messaggi di errore
103500041213     C                   EXSR      CTRD1
103600041213      *
103700041213     C                   MOVE      *ALL'0'       IN5098
103800041213     C                   MOVEA     IN5098        *IN(50)
103900041213      *
104000041213     C     XF02D1        ENDSR
104100041213      ************************************************************
104200041213      * GESTIONE F03 VIDEO D1
104300041213      ************************************************************
104400041213     C     F03D1         BEGSR
104500041213      *
104600041213     C                   MOVE      *ON           $FINE
104700150910     C                   MOVE      '1'           RETD25
104800041213      * fine programma
104900041213     C                   EXSR      ENDPGM
105000041213      *
105100041213     C     XF03D1        ENDSR
105200041213      ************************************************************
105300041213      * GESTIONE F05 VIDEO D1
105400041213      ************************************************************
105500041213     C     F05D1         BEGSR
105600041213      *
105700041213     C                   MOVE      *ON           $INZD1
105800041213      *
105900041213     C     XF05D1        ENDSR
106000041213      ************************************************************
106100041213      * GESTIONE F08 VIDEO D1
106200041213      ************************************************************
106300041213     C     F08D1         BEGSR
106400041213      *
106500041213     C                   MOVE      *ON           $FINE
106600150910     C                   MOVE      '0'           RETD25
106700041213      *
106800041213     C     XF08D1        ENDSR
106900041213      ************************************************************
107000041213      * GESTIONE F12 VIDEO D1
107100041213      ************************************************************
107200041213     C     F12D1         BEGSR
107300041213      *
107400041213      *  se la 1a videata è la lista oppure sono arrivato qui con F17,
107500041213      *  per F12 torno alla lista
107600041213     C     $LOP          IFEQ      2
107700041213     C     WINKR         OREQ      *ON
107800041213     C                   exsr      CtrD1
107900041213     C                   if        *in99 = *off
108000041213     C                   MOVE      'S1'          $GEST
108100050201     C                   move      *on           $WriteC1
108200050201     C                   move      *on           $WriteC2
108300041213     C                   endif
108400041213      *  altrimenti, per F12 torno a menu
108500041213     C                   ELSE
108600041213     C                   MOVE      *ON           $FINE
108700150910     C                   MOVE      '2'           RETD25
108800041213     C                   ENDIF
108900041213      *
109000041213     C     XF12D1        ENDSR
109100041213      ************************************************************
109200041213      * GESTIONE F18 VIDEO D1
109300041213      ************************************************************
109400041213     C     F18D1         BEGSR
109500041213      *
109600041213     C                   MOVE      'D2'          $GEST
109700041213     C                   MOVEL     *ON           $INZD2
109800041213      *
109900041213     C     XF18D1        ENDSR
110000041213      ************************************************************
110100041213      * GESTIONE F24 VIDEO D1
110200041213      * input
110300041213      * $ULKD1 = Ultimo tasto funzione emesso per video D1
110400041213      * $FX1 =Testi tasti funzione
110500041213      * $FC1 = Abilitazioni
110600041213      * $FR1 = Inversioni di fondo
110700041213      ************************************************************
110800041213     C     F24D1         BEGSR
110900041213      *
111000041213     C                   MOVEA     $FX1          $FX
111100041213     C                   MOVEA     $FC1          $FC
111200041213     C                   MOVEA     $FR1          $FR
111300041213     C                   Z-ADD     $ULKD1        $ULK
111400041213     C                   Z-ADD     $D1L01        $L01
111500041213     C                   Z-ADD     $D1L02        $L02
111600041213     C                   EXSR      RIEKEY
111700041213     C                   Z-ADD     $ULK          $ULKD1            3 0
111800041213     C                   MOVEA     $FC           $FC1
111900041213     C                   MOVEL     $KEY1         Z1KE1
112000150910     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
112100041213     C                   MOVEL     $KEY2         Z1KE2
112200041213     C                   WRITE     FMTZ1
112300041213      *
112400041213      *
112500041213     C     XF24D1        ENDSR
112600041213      ************************************************************
112700041213      * CONTROLLO VIDEATA PARZIALIZZAZIONI
112800041213      * non usare i seguenti indicatori (perchè relativi a D1MSG):
112900041213      * 93 94 96 97 98
113000041213      ************************************************************
113100041213     C     CTRD1         BEGSR
113200140701     C*
113300041213     C                   SETOFF                                       99
113400140701     C*
113500150911     C* Documenti legati
113600160407R2212X****               If        D1DocLeg <> Xscon  and
113700160407 "   X****                         D1DocLeg <> Xscoff
113800160407 "   X****               SetOn                                        5099
113900160407R2212X****               ENDIF
114000160407R2212C*
114100160407 "   C                   EXSR      CtrBolLeg
114200160407R2212C                   EXSR      CtrFatLeg
114300150911     C* Soggetto
114400150911     C                   EXSR      CtrSogg
114500150911     C*
114600150925     C* Inizializzo
114700150925     C                   Eval      DTRegDAamg  =  *loval
114800160118     C                   Eval      DTRegAamg   =  *loval
114900150925     C                   Eval      DTRe2DAamg  =  *loval
115000160118     C                   Eval      DTRe2Aamg   =  *loval
115100150925     C                   Eval      DTDocDAamg  =  *loval
115200160118     C                   Eval      DTDocAamg   =  *loval
115300150925     C                   Eval      DTDo2DAamg  =  *loval
115400160118     C                   Eval      DTDo2Aamg   =  *loval
115500160122     C*
115600160122     C                   if        TPDTRIFA11  =  'R'
115700150915     C* Controllo data Registrazione DA
115800150911     C                   IF        D1DtRegDa >0
115900150925     C*
116000160118     C     *JobRun       TEST(D)                 D1DtRegDa              53
116100150925     C* data corretta
116200150925     C                   if        *In53       = *Off
116300160118     C     *JobRun       Move      D1DtRegDa     DtRegDaamg
116400150925     C*
116500150925     C                   else
116600150925     C*
116700150925     C                   SetOn                                          99
116800150925     C                   endif
116900150925     C                   else
117000150925     C*
117100150925     C                   Seton                                        6399
117200150925     C*
117300150925     C                   endif
117400150911     C*
117500150915     C* Controllo data Registrazione A
117600150915     C                   IF        D1DtRegA > 0
117700160118     C     *JobRun       TEST(D)                 D1DtRega               54
117800150925     C* data corretta
117900150925     C                   if        *In54       = *Off
118000160118     C     *JobRun       Move      D1DtRegA      DtRegaamg
118100150925     C*
118200150925     C                   else
118300150925     C*
118400150925     C                   SetOn                                          99
118500150925     C                   endif
118600150925     C*
118700150925     C                   endif
118800150911     C*
118900150915     C* Controllo congruenza data Registrazione DA-A
119000150915     C                   IF        DtRegDAAMG > DtRegAAMG
119100150915     C                   SETON                                        6499
119200150911     C                   ENDIF
119300160122     C*
119400160122     C*
119500160122     C                   else
119600160122     C*
119700150915     C* Controllo data documento DA
119800150915     C                   IF        D1DtDocDa >0
119900160118     C*
120000160118     C     *JobRun       TEST(D)                 D1DtDocDa              55
120100150925     C* data corretta
120200150925     C                   if        *In55       = *Off
120300160118     C     *JobRun       Move      D1DtDocDa     DtDocDaamg
120400150925     C*
120500150925     C                   else
120600150925     C*
120700150925     C                   SetOn                                          99
120800150925     C                   endif
120900150925     C                   else
121000150925     C*
121100150925     C                   Seton                                        6599
121200150925     C*
121300150925     C                   endif
121400150915     C*
121500150915     C* Controllo data documento A
121600150915     C                   IF        D1DtDocA > 0
121700150925     C*
121800160118     C     *JobRun       TEST(D)                 D1DtDocA               56
121900150925     C* data corretta
122000150925     C                   if        *In56       = *Off
122100160118     C     *JobRun       Move      D1DtDocA      DtDocaamg
122200150925     C*
122300150925     C                   else
122400150925     C*
122500150925     C                   SetOn                                          99
122600150925     C                   endif
122700150925     C*
122800150925     C                   endif
122900150915     C*
123000150915     C* Controllo congruenza data documento DA-A
123100150915     C                   IF        DtDocDAAMG > DtDocAAMG
123200150915     C                   SETON                                        6699
123300150915     C                   ENDIF
123400160122     C*
123500160122     C                   endif
123600150925     C***
123700160122     C*
123800160122     C                   if        TPDTRIFA11  =  'R'
123900150925     C* Controllo data Registrazione Fatture DA
124000150925     C                   IF        D1DtRe2Da >0
124100160118     C     *JobRun       TEST(D)                 D1DtRe2Da              58
124200150925     C* data corretta
124300150925     C                   if        *In58       = *Off
124400160118     C     *JobRun       Move      D1DtRe2Da     DtRe2Daamg
124500150925     C*
124600150925     C                   else
124700150925     C*
124800150925     C                   SetOn                                          99
124900150925     C                   endif
125000150925     C                   else
125100150925     C*
125200150925     C                   Seton                                        6899
125300150925     C*
125400150925     C                   endif
125500150925     C
125600150925     C*
125700150929     C* Controllo data Registrazione Fattura A
125800150925     C                   IF        D1DtRe2A > 0
125900160118     C     *JobRun       TEST(D)                 D1DtRe2a               59
126000150925     C* data corretta
126100150925     C                   if        *In59       = *Off
126200160118     C     *JobRun       Move      D1DtRe2A      DtRe2aamg
126300150925     C*
126400150925     C                   else
126500150925     C*
126600150925     C                   SetOn                                          99
126700150925     C                   endif
126800150925     C*
126900150925     C                   endif
127000150925     C*
127100150925     C* Controllo congruenza data Registrazione DA-A
127200150925     C                   IF        DtRe2DAAMG > DtRe2AAMG
127300150925     C                   SETON                                        6999
127400150925     C                   ENDIF
127500160122     C*
127600160122     C*
127700160122     C                   else
127800160122     C*
127900150925     C* Controllo data documento DA
128000150925     C                   IF        D1DtDo2Da >0
128100150925     C*
128200160118     C     *JobRun       TEST(D)                 D1DtDo2Da              60
128300150925     C* data corretta
128400150925     C                   if        *In60       = *Off
128500160118     C*
128600160118     C     *JobRun       Move      D1DtDo2Da     DtDo2Daamg
128700150925     C*
128800150925     C                   else
128900150925     C*
129000150925     C                   SetOn                                          99
129100150925     C                   endif
129200150925     C                   else
129300150925     C*
129400150925     C                   Seton                                        7099
129500150925     C*
129600150925     C                   endif
129700150925     C*
129800150925     C* Controllo data documento A
129900150925     C                   IF        D1DtDo2A > 0
130000150925     C*
130100160118     C     *JobRun       TEST(D)                 D1DtDo2A               61
130200150925     C* data corretta
130300150925     C                   if        *In61       = *Off
130400160118     C*
130500160118     C     *JobRun       Move      D1DtDo2A      DtDo2aamg
130600150925     C*
130700150925     C                   else
130800150925     C*
130900150925     C                   SetOn                                          99
131000150925     C                   endif
131100150925     C*
131200150925     C                   endif
131300150925     C*
131400150925     C* Controllo congruenza data documento DA-A
131500150925     C                   IF        DtDo2DAAMG > DtDo2AAMG
131600150925     C                   SETON                                        7199
131700150925     C                   ENDIF
131800160122     C*
131900160122     C                   endif
132000150925     C***
132100150911     C* Fornitore / dogana
132200150911     C                   EXSR      CtrDogana
132300150911     C*
132400150911     C* Fornitore
132500150911     C                   EXSR      CtrFornitore
132600150911     C*
132700051111     C                   Endsr
132800150911     C/EJECT
132900160407R2212C***********************************************************
133000160407 "   C* Controllo bolle legate
133100160407 "   C***********************************************************
133200160407R2212C     CtrBolLeg     BEGSR
133300160407     C*
133400160407     C                   Clear                   D1BolLegD
133500160407     C                   Clear                   X07ValDs
133600160407     C*
133700160407     C                   Move      '1'           X07Ric
133800160407     C                   Move      '1'           X07All
133900160407     C                   Movel     'SDGSD25S  '  X07TRc
134000160407     C                   Movel     'BOLLEGD25 '  X07Cam
134100160407     C                   Movel     D1BolLeg      X07VAL
134200160407     C*
134300160407     C                   Callb     'X07VALR'
134400160407     C                   Parm                    X07ValDs
134500160407     C*
134600160407     C     X07ERR        IFEQ      *Off
134700160407     C                   MOVEL     X07DES        D1BolLegD
134800160407     C                   else
134900160407     C                   SetOn                                        5099
135000160407     C                   endif
135100160407     C*
135200160407R2212C                   ENDSR
135300160407R2212C/EJECT
135400160407R2212C***********************************************************
135500160407 "   C* Controllo fatture legate
135600160407 "   C***********************************************************
135700160407R2212C     CtrFatLeg     BEGSR
135800160407     C*
135900160407     C                   Clear                   D1FatLegD
136000160407     C                   Clear                   X07ValDs
136100160407     C*
136200160407     C                   Move      '1'           X07Ric
136300160407     C                   Move      '1'           X07All
136400160407     C                   Movel     'SDGSD25S  '  X07TRc
136500160407     C                   Movel     'FATLEGD25 '  X07Cam
136600160407     C                   Movel     D1FatLeg      X07VAL
136700160407     C*
136800160407     C                   Callb     'X07VALR'
136900160407     C                   Parm                    X07ValDs
137000160407     C*
137100160407     C     X07ERR        IFEQ      *Off
137200160407     C                   MOVEL     X07DES        D1FatLegD
137300160407     C                   else
137400160407     C                   SetOn                                        7499
137500160407     C                   endif
137600160407     C*
137700160407R2212C                   ENDSR
137800160407R2212C/EJECT
137900150911     C***********************************************************
138000150911     C* Controllo codice soggetto
138100150911     C***********************************************************
138200150911     C     CtrSogg       BEGSR
138300150911     C*
138400150911     C                   Clear                   D1SoggD
138500151009     C**
138600151009     C**   D1SOGG        IFNE      *BLANK
138700151009     C**   D1DOCLEG      ANDNE     XscOn
138800151009     C**                 SetOn                                        6799
138900151009     C**                 ELSE
139000151009     C**
139100150911     C     D1SOGG        IFNE      *BLANK
139200150911     C                   CALLb     'XALLINEA'
139300150911     C                   PARM                    D1SOGG
139400150911     C                   PARM      8             LEN               5 0
139500150911     C                   PARM                    XSCINP
139600150911     C*
139700150911     C                   MoveL     D1Sogg        WSogg
139800150911     C                   EXSR      CallDvaSog
139900150911     C*
140000150911     C                   if        DVAErrore  <> '1'
140100150911     C*
140200150911     C                   Eval      %subst(DVASOG:1:DVALENOUT)
140300150911     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
140400150911     C*
140500150911     C                   MoveL     DVSDes        D1SoggD
140600150911     C*
140700150911     C                   else
140800150911     C*
140900150911     C                   SetOn                                        5199
141000150911     C*
141100150911     C                   endif
141200150911     C*
141300150929     c                   ENDIF
141400150929     C*
141500151009     C***                ENDIF
141600150929     C*
141700150911     C                   ENDSR
141800150911     C/EJECT
141900150911     C************************************************************
142000150911     C* Controllo conto Dogana
142100140701     C************************************************************
142200150911     C     CtrDogana     BEGSR
142300140701     C*
142400150911     C                   Clear                   D1KscDogD
142500140701     C*
142600150911     C                   exsr      AllineaKscDog
142700140701     C*
142800150911     C                   if        D1KscDog   <> *Blank
142900140701     C*
143000150929     C                   Move      XCapKccDog    $Kcc
143100150911     C                   Move      D1KscDog      $Ksc
143200140701     C                   exsr      CALLMVC002
143300150916     C
143400140701     C* se errore o
143500140701     C                   if        Esito      = *On
143600150911     C                   SetOn                                        5299
143700140701     C*
143800140701     C                   else
143900150911     C                   MoveL     DesBrev002    D1KscDogD
144000150916     C* non è un conto dogana
144100150929     C                   if        AlleIva002 <> '02'
144200150916     C                   seton                                        6299
144300140701     C                   endif
144400140701     C*
144500150911     C                   endif
144600160418R219 C*
144700160418 "   C                   Else
144800160418R219 C                   Seton                                        7299
144900150925     C                   Endif
145000140701     C*
145100140701     C                   ENDSR
145200140701     C/EJECT
145300140701     C************************************************************
145400150911     C* Allinea conto dogana
145500140701     C************************************************************
145600150911     C     AllineaKscDog BEGSR
145700140701     C*
145800150911     C     D1KscDog      IFNE      *BLANK
145900140701     C                   CALLB     'XALLINEA'
146000150911     C                   PARM                    D1KscDog
146100140701     C                   PARM      8             LEN               5 0
146200140701     C                   PARM                    XSCINP
146300140701     C*
146400150925     C                   move      'F     '      XCapKccDog
146500150910     C*
146600140701     C                   CALLB     'XCAPCLIFOR'
146700140701     C                   PARM                    XScSoc
146800150925     C                   PARM                    XCapKccDog        6
146900150911     C                   PARM                    D1KscDog
147000140701     C                   ENDIF
147100140701     C*
147200140701     C                   ENDSR
147300140701     C/EJECT
147400150911     C************************************************************
147500150911     C* Controllo conto fornitore
147600150911     C************************************************************
147700150911     C     CtrFornitore  BEGSR
147800150911     C*
147900150911     C                   Clear                   D1KscD
148000150911     C*
148100150911     C                   exsr      AllineaKsc
148200150911     C*
148300150911     C                   if        D1Ksc      <> *Blank
148400150911     C*
148500150911     C                   Move      XCapKcc       $Kcc
148600150911     C                   Move      D1Ksc         $Ksc
148700150911     C                   exsr      CALLMVC002
148800150911     C* se errore o
148900150911     C* manca il sottoconto
149000150911     C                   if        Esito      = *On
149100150911     C                   SetOn                                        5799
149200150911     C*
149300150911     C                   else
149400150911     C                   MoveL     DesBrev002    D1KscD
149500150911     C*
149600150911     C                   endif
149700160418R219 C*
149800160418 "   C                   Else
149900160418R219 C                   Seton                                        7399
150000150911     C*
150100150911     C                   endif
150200150911     C*
150300150911     C                   ENDSR
150400150911     C/EJECT
150500150911     C************************************************************
150600150911     C* Allinea conto fornitore
150700150911     C************************************************************
150800150911     C     AllineaKsc    BEGSR
150900150911     C*
151000150911     C     D1Ksc         IFNE      *BLANK
151100150911     C                   CALLB     'XALLINEA'
151200150911     C                   PARM                    D1Ksc
151300150911     C                   PARM      8             LEN               5 0
151400150911     C                   PARM                    XSCINP
151500150911     C*
151600150911     C                   move      'F     '      XCapKcc
151700150911     C*
151800150911     C                   CALLB     'XCAPCLIFOR'
151900150911     C                   PARM                    XScSoc
152000150911     C                   PARM                    XCapKcc           6
152100150911     C                   PARM                    D1Ksc
152200150911     C                   ENDIF
152300150911     C*
152400150911     C                   ENDSR
152500150911     C/EJECT
152600140701     C************************************************************
152700140701     C* DECODIFICA CONTO
152800140701     C************************************************************
152900140701     C     CallMVC002    BEGSR
153000140701     C*
153100140701     C                   Clear                   ND002DS
153200140701     C                   EVAL      LenOut = %Size(ND002DS)
153300140701     C                   CallB     'NDMVC002'
153400140701     C                   PARM                    XScSoc
153500140701     C                   PARM                    $Kcc
153600150929     C                   PARM                    $Ksc
153700140701     C                   PARM                    *Omit
153800140701     C                   PARM      *OFF          GesMsg
153900150911     C                   PARM      *OFF          Esito
154000140701     C                   PARM      'ND002DS'     StrutturaO
154100140701     C                   PARM                    ND002DS
154200140701     C                   PARM                    LenOut
154300140701     C*
154400140701     C                   ENDSR
154500150911     C/EJECT
154600150929R091 C/EJECT
154700150929     C************************************************************
154800150929     C* Rerperimento dati di causale operativa
154900150929     C************************************************************
155000150929     C     CallMVC003    BEGSR
155100150929     C*
155200150929     C                   Clear                   ANOPE
155300150929     C*
155400150929     C                   Eval      LenOut     = %Size(ANOPE)
155500150929     C                   CallB     'NDMVC003'
155600150929     C                   Parm                    XScSoc
155700150929     C                   Parm                    $Causale
155800150929     C                   Parm                    ZDateIso
155900150929     C                   Parm      '0'           Uso               1
156000150929     C                   Parm      *OFF          GesMsg
156100150929     C                   Parm      *ON           Esito
156200150929     C                   Parm      'ANOPE     '  StrutturaO
156300150929     C                   Parm                    ANOPE
156400150929     C                   Parm                    LenOut
156500150929     C*
156600150929     C                   ENDSR
156700150929      /EJECT
156800150911     C************************************************************
156900150911     C* Chiamata al driver dei soggetti
157000150911     C************************************************************
157100150911     C     CallDvaSog    BEGSR
157200150911     C*
157300150911     C                   EVAL      DVALenOut = %Size(DVASOG)
157400150911     C*
157500150911     C                   CALLB     'NDDVASOG'
157600150911     C                   PARM      '1'           DVARIC
157700150911     C                   PARM      XScSoc        DVASOCIETA
157800150911     C                   PARM                    DVAUNITA
157900150911     C                   PARM      'DVASOG    '  DVASTRUTT
158000150911     C                   PARM                    DVADTRIF
158100150911     C                   PARM                    DVALENOUT
158200150911     C* Se si accede con il codice cli/for impostare i campi seguenti
158300150911     C* se si accede con il soggetto ometterli e impostare gli altri
158400150911     C*****              PARM                    DVASNATURA
158500150911     C*****              PARM                    DVACODICE
158600150911     C*****              PARM                    DVALINEAV
158700150911     C*****              PARM                    DVAFILIALE
158800150911     C                   PARM                    *omit
158900150911     C                   PARM                    *omit
159000150911     C                   PARM                    *omit
159100150911     C                   PARM                    *omit
159200150911     C                   PARM      WSogg         DVSSOGG
159300150911     C                   PARM      *Blank        DVSTPIND
159400150911     C                   PARM      *Blank        DVSCDIND
159500150911     C                   PARM                    DVAERRORE
159600150911     C                   PARM                    DVAOUTPUT
159700150911     C*
159800150911     C                   ENDSR
159900150911     C/EJECT
160000041213      ************************************************************
160100041213      * DEFINIZIONE DELL'ORDINAMENTO
160200041213      ************************************************************
160300041213     C     DEFORD        BEGSR
160400041213      *
160500041213     C                   Z-ADD     1             $ORD
160600041213      *
160700041213     C     XDefOrd       ENDSR
160800041213      ************************************************************
160900041213      * GESTIONE LISTA
161000041213      ************************************************************
161100041213     C     GESS1         BEGSR
161200041213      * inizializzazione videata
161300041213     C     $INZS1        IFEQ      *ON
161400041213     C                   EXSR      INZS1
161500041213     C                   MOVE      *OFF          $INZS1
161600041213     C                   ENDIF
161700041217     C     $INZS2        IFEQ      *ON
161800041217     C                   EXSR      INZS2
161900041217     C                   MOVE      *OFF          $INZS2
162000041217     C                   ENDIF
162100041213      * emissione piede videata se proveniente da altra
162200041213      * salvataggio valore formato di provenienza
162300041217     C                   if        $LastV <> 'S1' and
162400041217     C                             $LastV <> 'S2'
162500041213     C                   WRITE     FMTT1
162600041213     C                   EXSR      GESPIE
162700041213     C                   MOVE      $LASTV        $LASTG
162800041213     C                   MOVE      'S1'          $LASTV
162900041213     C                   ENDIF
163000160711     C*
163100160711     C* flag per close/open finestre SDGSD44R
163200160711     C                   Move      '1'           WrkWdwD44         1
163300041213      *
163400041217      * S2
163500041217     C                   if        WMaxS2 = 0
163600041217     C                   write     FmtD4
163700041217     C                   endif
163800041223     C                   if        $writec2=*on
163900041221     C                   write     FmtC2
164000041223     C                   eval      $writec2=*off
164100041222     C                   endif
164200041217      * S1
164300041217     C                   if        WMaxS1 = 0
164400041217     C                   write     FmtD3
164500041217     C                   endif
164600041213      * Inizializzazione indice tasti funzione personalizzati
164700041213     C                   Eval      PosFun = 25
164800041217      *
164900041213     C                   EXFMT     FMTC1
165000041213      *
165100041213      * azzero la posizione del cursore
165200041213     C                   Z-ADD     0             H1RIGA
165300041213     C                   Z-ADD     0             H1COLO
165400041213      * Serve per avvertire i pgm di sola finestra di eseguire le operazioni
165500041213      * di refresh dello sfondo
165600041213     C                   MOVE      '1'           WExf
165700041213      *
165800041213     C     C1NRR         IFNE      0
165900041217     C                   Z-ADD     C1NRR         WSflS1
166000041213     C                   ENDIF
166100041213     C                   IF        SFLNRR > 0
166200041213     C                   Z-ADD     SFLNRR        C1RCD
166300041213     C                   ENDIF
166400041213      * inizializzazione indice opzioni personalizzate
166500041213     C                   Eval      PosOpz = 0
166600041213      * CONTROLLO ABILITAZIONE TASTI
166700041213      * INPUT
166800041213      * $FC2 = Abilitazioni videata S1
166900041213      * OUTPUT
167000041213      * *IN97 + *IN99 Errori
167100041213     C                   MOVEA     $FC2          $FC
167200041213     C                   EXSR      CTRKEY
167300041213      *
167400041213     C     *IN99         IFEQ      *OFF
167500041213      *
167600041213     C                   SELECT
167700041213      * F3=Fine
167800041213     C     $TASTO        WHENEQ    F03
167900041213     C                   EXSR      F03S1
168000041213      * F8=Successivo
168100041213     C     $TASTO        WHENEQ    F08
168200041213     C                   EXSR      F08S1
168300041213      * F12=Ritorno
168400041213     C     $TASTO        WHENEQ    F12
168500041213     C                   EXSR      F12S1
168600041213      * F17=Parzializzazioni
168700041213     C     $TASTO        WHENEQ    F17
168800041213     C                   EXSR      F17S1
168900041213      * F18=Modifica valori assunti
169000041213     C     $TASTO        WHENEQ    F18
169100041213     C                   EXSR      F18S1
169200140701      * F23=Altre opzioni
169300041213     C     $TASTO        WHENEQ    F23
169400041213     C                   EXSR      F23S1
169500041213      * F24=Altri tasti
169600041213     C     $TASTO        WHENEQ    F24
169700041213     C                   EXSR      F24S1
169800041213      * ROLL-UP
169900041213     C     $TASTO        WHENEQ    ROLLUP
170000041213     C                   EXSR      ROLS1
170100041213      * Personalizzata
170200041213     C     $PGF2(PosFun) Whenne    *BLANK
170300041213     C                   EXSR      FperS1
170400041213      *
170500041213     C                   OTHER
170600041221      *
170700041221      * CONTROLLI su entrambi i sfl
170800041222      *   il prossimo sfl da gestire sarà quello in errore
170900041222      *   oppure quello sul quale è il cursore
171000041221     C                   eval      WGest = $Gest
171100041221     C                   exsr      CtrS
171200041221     C                   eval      $Gest = WGest
171300041221      *
171400041221     C                   ENDSL
171500041221      *
171600041213     C                   ENDIF
171700041213      *
171800041213     C     XGesS1        ENDSR
171900041213      ************************************************************
172000041213      * INIZIALIZZAZIONE LISTA
172100041213      ************************************************************
172200041213     C     INZS1         BEGSR
172300041213      * pulizia SFL
172400041213     C                   SETOFF                                         3031
172500041213     C                   WRITE     FMTC1
172600041213     C                   SETON                                          31
172700041213      *
172800140701     C* pulizia del sottotitolo
172900140701     C                   Clear                   D1TIT
173000141007     C                   move      *ALL'0'       IN5098           49
173100141007     C                   movea     IN5098        *IN(50)
173200050104      * Ctl
173300050104     C                   exsr      RieC1
173400160125      *
173500160407R2212X***** Ulteriore definizione delle opzioni utilizzabili a pgm
173600160407R2212X****               EXSR      ULTOPZ
173700041213      *
173800041213      * opzioni
173900041217     C                   exsr      InzS1_RieOpz
174000041213      *
174100041213      * CARICAMENTO SFL A PAGINE
174200041217     C* (essendo SFLPAG=9999, il caricamento viene fatto x tutto il sfl)
174300041217     C                   Z-ADD     0             S1Nrr
174400041217     C                   Z-ADD     0             WMaxS1
174500041213      *
174600041213      * Posizionamento su file pilota
174700041217     C                   clear                   C1Pos
174800041217     C                   exsr      SetAnaS1
174900041213      * ripristino i valori del posizionamento
175000041217     C                   MOVE      W1Pos         C1POS
175100041213      * Lettura file pilota
175200041217     C                   EXSR      RedAnaS1
175300140718     C                   if        $rcdok=*on
175400041213      *
175500041213      * se arrivo dalla parzializzazione devo caricare solo la prima pagina
175600041217     C                   if        $LastV = 'D1'
175700041217     C                   z-add     SflPagS1      WPagS1
175800041217     C                   endif
175900041217     C                   exsr      RolS1
176000041213      * imposto il caricamento sfl del numero massimo di righe per pagina
176100041217     C                   z-add     SflPagS1      WPagS1
176200140718     c                   endif
176300050103      *
176400050103     C                   move      *on           $writeC1
176500041213      *
176600041213     C     XInzS1        ENDSR
176700050104      ************************************************************
176800050104      * Caricamento C1
176900050104      ************************************************************
177000050104     C     RieC1         Begsr
177100050104      *
177200050104      *
177300050104     C     XRieC1        Endsr
177400160407R2212X************************************************************
177500160407 "   X* ULTERIORI DEFINIZIONE OPZIONI UTILIZZABILI A PGM
177600160407 "   X************************************************************
177700160407 "   X**** ULTOPZ        BEGSR
177800160407 "   X****
177900160407 "   X**** Questa routine deve eseguire una ulteriore definizione
178000160407 "   X**** delle opzioni a seconda che stia vedendo i documenti collegati
178100160407 "   X**** o meno
178200160407 "   X****
178300160407 "   X**** 15=Documento legato
178400160407 "   X****               Z-ADD     1             X
178500160407 "   X**** 15            LOOKUP    $OP1(X)                                21
178600160407 "   X**** *IN21         IFEQ      *ON
178700160407 "   X****
178800160407 "   X****               if        D1DocLeg    = XScOn
178900160407 "   X****               MOVE      'S'           $FL1(X)
179000160407 "   X****               else
179100160407 "   X****               MOVE      'N'           $FL1(X)
179200160407 "   X****               endif
179300160407 "   X****
179400160407 "   X****               EndIf
179500160407 "   X****
179600160407 "   X**** XUltOpz       ENDSR
179700160407R2212X****/EJECT
179800041217      ************************************************************
179900041217      * Caricamento opzioni S1
180000041217      ************************************************************
180100041217     C     InzS1_RieOpz  Begsr
180200041217      *
180300041217     C                   MOVEA     $TX1          $TX
180400041217     C                   MOVEA     $FL1          $FL
180500041217     C                   Z-ADD     0             WCOZ
180600041217     C                   Z-ADD     0             WMXO
180700041217      *  equivale al num.di elementi della schiera $TX1
180800041217     C                   Z-add     OpzLen        WMXO
180900041217     C                   Z-ADD     $C1Op1        $L01
181000041217     C                   Z-ADD     $C1Op2        $L02
181100041217     C                   EXSR      RIEOPZ
181200041217     C                   Z-ADD     23            X
181300041217     C                   MOVE      FF23          $FC2(X)
181400041217     C                   MOVEL     WOPZ1         C1OPZ1
181500150910     C                   MOVEL     $ALLOPT       H1ALLOPT
181600041217     C                   MOVEL     WOPZ2         C1OPZ2
181700041217     C                   Z-ADD     WCOZ          C1COZ
181800041217      *
181900041217     C     XInzS1_RieOpz Endsr
182000041213      ************************************************************
182100041213      * CARICAMENTO PAGINA LISTA
182200041213      ************************************************************
182300041213     C     ROLS1         BEGSR
182400041213      *
182500041213      *SflNxtChg inizializzato
182600041213      *(ripristino l'attributo di 'riga cambiata')
182700041213     C                   SETOFF                                       32
182800041213     C                   Z-ADD     0             Y
182900041213      *mi posiziono sull'ultima riga caricata
183000041217     C                   Z-ADD     WMaxS1        S1Nrr
183100041213      *
183200041217      * Carico il sfl fino a quando
183300150925      * esistono Bolle doganali valide in base ai parametri scelti
183400041217      * e fino al max della capienza del sfl
183500041213     C     $EndFile      DOWEQ     *OFF
183600041217     C     Y             ANDLT     WPagS1
183700041213      *
183800041217     C                   exsr      RieS1
183900041213      *
184000041213     C                   ADD       1             S1NRR
184100041213     C                   ADD       1             Y
184200160413R2212C* Salvo il numero relativo di record
184300160413R2212C                   Z-Add     S1NRR         H1Nrr
184400041213      *
184500041213     C                   WRITE     FMTS1
184600041213      *
184700150925      *
184800041217     C                   EXSR      RedAnaS1
184900041213      *
185000041213     C                   ENDDO
185100041213      *
185200041213      *memorizzo quante righe ho caricato
185300041217     C                   Z-ADD     S1NRR         WMaxS1               30
185400041213      *
185500041213      *
185600160413R2212C* Se aggiornamento mi posiziono sulla pagina aggiornata
185700160413  "  C                   If        WOprS1= *on
185800160413  "  C                   Exsr      PosPagAS1
185900160413  "  C                   movel     *off          woprs1
186000160413R2212C                   Else
186100041217      * Posizionamemto al 1° RCD della pagina
186200160414     C                   Exsr      PosPag1S1
186300160413R2212C                   Endif
186400041213      *
186500041217     C     XRolS1        Endsr
186600041217      ************************************************************
186700041217      * Posizionamemto al 1° RCD della pagina S1
186800041217      ************************************************************
186900160413     C     PosPag1S1     Begsr
187000041217      *
187100041217     C     S1Nrr         DIV       SflPagS1      PAGINE            4 0
187200041217     C                   MVR                     RESTO             3 0
187300041217     C     PAGINE        MULT      SflPagS1      C1RCD
187400041217     C     RESTO         IFGT      0
187500041217     C                   ADD       1             C1RCD
187600041217     C                   ELSE
187700041217     C                   SUB       SflPagS1      C1RCD
187800041217     C                   ADD       1             C1RCD
187900041217     C                   ENDIF
188000041217      *
188100041217     C     XPosPag1S1    Endsr
188200160413R2212 ************************************************************
188300160413  "   * Posizionamemto al RCD aggiornato delle pagina S1
188400160413  "   ************************************************************
188500160413R2212C     PosPagAs1     Begsr
188501170911R266 C* Se opzione di cancellazione / scelta e filtro
188502170912  "  C* correggo il posizionamento.
188503170912  "  C                   IF        D1BolLeg = '1'   and
188504170911  "  C                             SavDel01 >0      or
188505170911  "  C                             D1BolLeg = '0'   and
188506170911  "  C                             SavOpz01 >0
188507170911  "  C                   Sub       1             WC1Rcd
188508170911  "  C                   IF        Wc1Rcd <= 0
188509170911  "  C                   Z-add     1             Wc1Rcd
188510170911  "  C                   Endif
188511170911R266 C                   Endif
188600160413      *
188700160413     C     WC1RCD        DIV       2             PAGINE            4 0
188800160413     C                   MVR                     RESTO             3 0
188900160413     C     PAGINE        MULT      2             C1RCD
189000160413     C     RESTO         IFGT      0
189100160413     C                   ADD       1             C1RCD
189200160413     C                   ELSE
189300160413     C                   SUB       2             C1RCD
189400160413     C                   ADD       1             C1RCD
189500160413     C                   ENDIF
189600160413      *
189700160413R2212C     XPosPagAS1    Endsr
189800041217      ************************************************************
189900041217      * Chiamata programma con tasto personalizzato
190000041217      ************************************************************
190100041217     C     FPerS1        BEGSR
190200041217      *
190300041217      * salvo la DS di ingresso
190400150812     C                   MOVEL     SDGSD25DS     $SaveDS
190500041217      *
190600150812     C                   MOVEL     SDGSD25DS     KPJBU
190700041217     C                   CALL      $PGF2(PosFun)                        22
190800041217     C                   PARM                    KPJBA
190900140701     C****               PARM                    NOMPGM
191000041217      *
191100041217      * ripristino la DS di ingresso
191200150812     C                   MOVEL     $SaveDS       SDGSD25DS
191300041217      *
191400041217     C     XFPerS1       ENDSR
191500041213      ************************************************************
191600041213      * POSIZIONAMENTO RCD ARCHIVIO PILOTA
191700041213      ************************************************************
191800041217     C     SetAnaS1      BEGSR
191900150917      *
192000150917     C                   CLEAR                   SDGSD26DS
192100150924     C* Devo estrarre le bolle doganali
192200150924     C                   Movel     *on           SiBollD26
192300150924     C* passo i campi di parzializzazione alla DS del PGM SQL SDGSD26R
192400150917     C* che mi seleziona i record
192500150917     C                   MOVEL     XscSoc        SocietaD26
192600150917     C                   MOVEL     'CG'          CtbD26
192700150917     C                   MOVEL     XcapKccDog    KccD26
192800150917     C                   MOVEL     D1KscDog      KscD26
192900150918     C*
193000160125     C                   Eval      TpDtRifD26  = TpDtRifA11
193100160125     C*
193200160125     C                   if        TPDTRIFA11  = 'R'
193300160118     C     *JobRun       Move      D1DtRegDa     DtRegDaD26
193400160118     C     *JobRun       Move      D1DtRegA      DtRegAD26
193500160125     C                   else
193600160118     C     *JobRun       Move      D1DtDocDa     DtDocDaD26
193700160118     C     *JobRun       Move      D1DtDocA      DtDocAD26
193800160125     C                   endif
193900150917     C*
194000150918     C                   MOVE      1             ORDD26
194100150917     C* richiedo al pgm di lettura il 1° rcd con la conseguente
194200150917     C* inizializzazione delle variabili SQL
194300150918     C                   MOVE      '0'           RICD26
194400150917      *
194500150917     C     XSetAnaS1     ENDSR
194600041213      *
194700041213      ************************************************************
194800041213      * LETTURA RCD ARCHIVIO PILOTA
194900041213      ************************************************************
195000041217     C     RedAnaS1      BEGSR
195100041213      *
195200041213      *
195300041213     C                   movel     *off          $EndFile
195400041213     C                   movel     *off          $RcdOk
195500041213      *
195600041213     C                   dou       $EndFile = *on or $RcdOk = *on
195700041213      *
195800041213      * LETTURA
195900150918      *
196000150918     C                   CALL      'SDGSD26R'
196100150918     C                   PARM                    SDGSD26DS
196200150918     C                   PARM                    NDMOV00F
196300150918      *
196400150918     C                   MOVE      ENDD26        $EndFile
196500150929      * richiedo al pgm di lettura il rcd successivo
196600150929     C                   MOVE      '1'           RICD26
196700041213      *
196800041213      * SELEZIONE
196900041213     C                   if        $EndFile = *off
197000150922     C                   exsr      RepLegBol
197100150929     C                   exsr      RepReg
197200150929     C                   exsr      RepCausale
197300150922     C                   exsr      RepIva
197400041217     C                   exsr      SelAnaS1
197500041213     C                   endif
197600041213      *
197700041213     C                   enddo
197800041213      *
197900041213     C                   move      $EndFile      *IN33
198000041213      *
198100041217     C     XRedAnaS1     ENDSR
198200150922      ************************************************************
198300150922      * REPERISCO IL LEGAME DELLA BOLLA
198400150922      ************************************************************
198500150922     C     RepLegBol     Begsr
198600150922      *
198700160408R2212 * Recupera il primo legame che trova
198800160408 "    * Si assume che il soggetto e il codice fornitore sia
198900160408R2212 * sempre lo stesso
199000150922     C                   movel     *off          $LegBol
199100160122     C                   Movel     *off          $FoundLRC
199200151009     C                   Clear                   $SoggFor
199300150922      *
199400150922     C                   Eval      LRCSocieta = Xscsoc
199500150922     C                   Eval      LRCSys = MovSys
199600150922     C                   Eval      LRCNrasReg = MovNrAsReg
199700150922      *
199800160418R2212X**** K03LRC01      chain(N)  SdgLRC01l
199900160418R2212C*
200000160418 "   C     K03LRC01      SETLL     SDGLRC01L
200100160418 "   C     K03LRC01      READE(N)  SDGLRC01L                              21
200200160418 "   C                   Eval      $FineLRC    = *In21
200300160418 "   C*
200400160418R2212C                   DoW       $FineLRC    = *Off
200500150922      *
200600160418R2212X****               If        %Found(SdgLRC01l)
200700160122     C                   Movel     LRCSogg       $SoggFor
200800160122     C                   Movel     *on           $FoundLRC
200900150929     C                   If        LRCNRASL<> 0
201000150922     C                   Movel     *on           $LegBol
201100160418R2212C                   Movel     *on           $FineLRC
201200150922     C                   Else
201300150922     C                   Movel     *Off          $LegBol
201400150922     C                   Endif
201500160418R2212X****               Endif
201600160418R2212C*
201700160418 "   C* continuo a cercare se è presente un legame effettivo
201800160418 "   C                   if        $FineLRC   <> *On
201900160516R2212C     K03LRC01      READE(N)  SDGLRC01L                              21
202000160516R2251C                   Eval      $FineLRC    = *In21
202100160516R2212C                   endif
202200160418R2212C                   endDo
202300150922      *
202400150922     C                   Endsr
202500150924      ************************************************************
202600150924      * REPERISCO IL LEGAME DELLA Fattura
202700150924      ************************************************************
202800150924     C     RepLegFat     Begsr
202900150924      *
203000150924     C                   movel     *off          $LegFat
203100150924      *
203200150924     C                   Eval      LRCSocieta = Xscsoc
203300150924     C                   Eval      LRCSysL= MovSys
203400150924     C                   Eval      LRCNrasL   = MovNrAsReg
203500150924      *
203600160418R2212X**** K03LRC02      chain     SdgLRC02l
203700160418R2212C     K03LRC02      chain(N)  SdgLRC02l
203800150924      *
203900150924     C                   If        %Found(SdgLRC02l)
204000150924     C                   Movel     *on           $LegFat
204100150924     C                   Endif
204200150924      *
204300150924     C                   Endsr
204400150922      ************************************************************
204500150922      * REPERISCO L'IMPORTO DELL'IVA
204600150922      ************************************************************
204700150922     C     RepIva        Begsr
204800150922      *
204900150922     C                   movel     *off          $NdIva
205000150922     C                   Clear                   $imponib
205100150922     C                   Clear                   $iva
205200150922     C                   Clear                   $Aliq
205300150922     C                   Clear                   $Rifiva
205400150922      *
205500150922     C                   Eval      IVASys     = MoVSyS
205600150922     C                   Eval      IVANrAsReg = MovNrAsReg
205700150922      *
205800150922     C     K02Iva01      Setll     Ndiva01l
205900150922      *
206000150922     C                   Do        *Hival
206100150922     C     K02Iva01      reade     Ndiva01l
206200150922      *
206300150924     C                   If        %eof(Ndiva01l)
206400150922     C                   Leave
206500150922     C                   Endif
206600150929      *
206700150929     C                   movel     *on           $NdIva
206800150922      *
206900150922     C                   add       IvaImponib    $imponib
207000150922     C                   add       IvaImporto    $iva
207100150922      *
207200150922     C                   If        IvaImporto <> 0
207300150922     C                   movel     IvaAliq       $Aliq
207400150922     C                   movel     IvaRifIva     $RifIva
207500150922     C                   Endif
207600150922      *
207700150922     C                   endDo
207800150922     C*
207900150922     C                   Endsr
208000150929      ************************************************************
208100150929      * REPERISCO IL NUMERO DI REGISTRAZIONE
208200150929      ************************************************************
208300150929     C     RepREG        Begsr
208400150929      *
208500150929     C                   movel     *off          $NdReg
208600150929      *
208700150929     C                   Eval      REGSys     = MoVSyS
208800150929     C                   Eval      REGNrAsReg = MovNrAsReg
208900150929      *
209000150929     C     K02Reg01      Chain     NdREG01l
209100150929     C*
209200150929     C                   If        %Found(NdREG01l)
209300150929     C                   Movel     *on           $NdReg
209400150929     C                   Endif
209500150929     C*
209600150929     C                   ENDSR
209700150929      ************************************************************
209800150929      * REPERISCO DATI CAUSALE
209900150929      ************************************************************
210000150929     C     RepCausale    Begsr
210100150929      *
210200150929     C                   movel     *off          $Anope
210300150929     C                   Eval      $causale = MOVCausTes
210400150929      *
210500150929     C                   EXSR      CallMVC003
210600150929      *
210700150929     C                   if        Esito      <>*On
210800150929     C                   movel     *on           $Anope
210900150929     C                   Endif
211000150929     C*
211100150929     C                   ENDSR
211200150929      *
211300041213      ************************************************************
211400041213      * SELEZIONI SULLA RIGA LETTA
211500041213      ************************************************************
211600041217     C     SelAnaS1      Begsr
211700041213      *
211800150922     C                   movel     *on           $RcdOk
211900150918     C*
212000150918     C                   Select
212100150929     C*
212200150929     C                   When      $NdReg   = *Off
212300150929     C                   movel     *off          $RcdOk
212400150929     C*
212500150929     C                   When      $Anope   = *Off or
212600150929     C                             ($anope = *On and
212700150929     C                             (Opestp <> 'F' or
212800150929     C                             OpeTpc <> '1' or
212900150929     C                             OpeTpDoci = '8'))
213000150929     C                   movel     *off          $RcdOk
213100150918     C*
213200150922     C                   When      $LegBol  = *Off  and
213300160407R2212X****                         D1DocLeg = XscOn or
213400160407R2212C                             D1BolLeg = '1'   or
213500150922     C                             $LegBol  = *On and
213600160407R2212X****                         D1DocLeg = XscOff
213700160407R2212C                             D1BolLeg = '0'
213800150918     C                   movel     *off          $RcdOk
213900150918     C*
214000160122     C                   When      D1Sogg <> *blanks
214100160122     C                   If        $FoundLRC = *Off   or
214200160125     C                             ($FoundLRC = *On   and
214300160122     C                              LRCSogg <> D1Sogg)
214400150918     C                   movel     *off          $RcdOk
214500150918     C                   Endif
214600150918     C*
214700150918     C                   EndSl
214800140708      *
214900041217     C     XSelAnaS1     Endsr
215000041213      ************************************************************
215100041213      * RIEMPIMENTO RIGA LISTA
215200041213      ************************************************************
215300041213     C     RIES1         BEGSR
215400041213      *
215500041213     C                   clear                   FMTS1
215600041213      *
215700150925     C                   eval      H1Sys     = MoVSys
215800150925     C                   eval      H1NrAsReg = MoVNrAsReg
215900160418     C                   eval      H1DocLeg  = $LegBol
216000150922     C                   eval      S1Ksc     = MovKsc
216100151009     C                   eval      H1Kcc     = MovKcc
216200151009     C* Decodifico il conto
216300151009     C                   Move      H1Kcc         $Kcc
216400150924     C                   Move      S1Ksc         $Ksc
216500150924     C                   exsr      CALLMVC002
216600150924     C* se nessun errore
216700150924     C                   if        Esito      <> *On
216800150924     C                   MoveL     DesBrev002    S1KscD
216900150924     C                   endif
217000151009     C* Decodifico il soggetto fornitore reperito dal legame
217100151009     C*
217200151009     C                   MoveL     $SoggFor      S1SoggFor
217300151009     C*
217400151009     C                   If        $SoggFor <> *blanks
217500151009     C                   MoveL     $SoggFor      WSogg
217600151009     C                   EXSR      CallDvaSog
217700151009     C*
217800151009     C                   if        DVAErrore  <> '1'
217900151009     C*
218000151009     C                   Eval      %subst(DVASOG:1:DVALENOUT)
218100151009     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
218200151009     C*
218300151009     C                   MoveL     DVSDes        S1SoggForD
218400151009     C*
218500151009     C                   endif
218600151009     C                   endif
218700140701      *
218800150925     C     *jobrun       Move      MovDtDoc      S1DtDoc
218900150929     C                   Movel     MovNrDoc      S1NrDoc
219000150925     C     *jobrun       Move      MovDtReg      S1DtReg
219100150929     C                   Movel     RegNrReg      S1NrReg
219200150922     C*
219300041213      *
219400150922     C                   If        $NdIva = *on
219500150922     C                   eval      S1Importo = $Imponib
219600150922     C                   eval      S1Impiva  = $Iva
219700150922     C                   eval      S1Aliq    = $Aliq
219800150922     C                   eval      S1RifIva  = $RifIva
219900150922     C                   Else
220000150922     C                   eval      S1Importo = MovImporto
220100150922     C                   Endif
220200150922      *
220300150925     C                   eval      S1Causale = MovCausTes
220400140708      *
220500041213      * attributi sfl
220600041213     C                   exsr      AtrS1
220700041213      *
220800041213     C     XRieS1        Endsr
220900041213      ************************************************************
221000041213      * ATTRIBUTI DELLA RIGA DEL SFL
221100041213      ************************************************************
221200041213     C     AtrS1         Begsr
221300050103      *
221400160418     C                   If        H1DocLeg =  *on
221500160418     C                   seton                                        12
221600160707R2322C                   Eval      *In14      = *Off
221700160418     C                   Else
221800160418     C                   setoff                                       12
221900160707R2322C*
222000160707 "   C* se presente solo il soggetto, lo metto in alta intensità
222100160707 "   C                   if        S1SoggFor <> *Blank
222200160707 "   C                   Eval      *In14      = *On
222300160707 "   C                   else
222400160707 "   C                   Eval      *In14      = *Off
222500160707 "   C                   endif
222600160707R2322C*
222700160418     C                   Endif
222800041213      *
222900041213     C     XAtrS1        Endsr
223000041213      ************************************************************
223100041213      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
223200041213      ************************************************************
223300041217     C     ClcPagS1      Begsr
223400041213      *
223500041213      * Input :
223600041213      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
223700041213      *          cursore
223800041213      * - SFLPAG = numero rcd per pagina sfl
223900041213      * Output :
224000041213      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
224100041213      *
224200041217     C     WSflS1        div       SflPagS1      Pagine            4 0
224300041213     C                   mvr                     Resto             3 0
224400041213     C                   if        Resto > 0
224500041213     C                   add       1             Pagine
224600041213     C                   endif
224700041217     C     Pagine        mult      SflPagS1      WPagS1
224800041213      *
224900041217     C     XClcPagS1     Endsr
225000041213      ************************************************************
225100041213      * GESTIONE F08 VIDEO S1
225200041213      ************************************************************
225300041213     C     F08S1         BEGSR
225400041213      *
225500041213     C                   MOVE      *ON           $FINE
225600150910     C                   MOVE      '0'           RETD25
225700041213      *
225800041213     C     XF08S1        ENDSR
225900041213      ************************************************************
226000041213      * GESTIONE F03 VIDEO S1
226100041213      ************************************************************
226200041213     C     F03S1         BEGSR
226300041213      *
226400041213     C                   MOVE      *ON           $FINE
226500150910     C                   MOVE      '1'           RETD25
226600041213      * fine programma
226700041213     C                   EXSR      ENDPGM
226800041213      *
226900041213     C     XF03S1        ENDSR
227000041213      ************************************************************
227100041213      * GESTIONE F12 VIDEO S1
227200041213      ************************************************************
227300041213     C     F12S1         BEGSR
227400041213      *
227500041213      *  se la 1a videata sono le parzializzazioni, per F12 torno a queste
227600041213     C     $LOP          IFEQ      1
227700041213     C                   MOVE      'D1'          $GEST
227800041213      *  se la 1a videata è questa, per F12 torno a menu
227900041213     C                   ELSE
228000041213     C                   MOVE      *ON           $FINE
228100150910     C                   MOVE      '2'           RETD25
228200041213     C                   ENDIF
228300041213      *
228400041213     C     XF12S1        ENDSR
228500041213      ************************************************************
228600041213      * GESTIONE F17 VIDEO S1
228700041213      ************************************************************
228800041213     C     F17S1         BEGSR
228900041213      *
229000041213     C                   MOVE      'D1'          $GEST
229100041213      * se il pgm è stato richiamato con chiave univoca
229200041213      * e dopo è stata richiesta una parzializzazione
229300041213      * tolgo la gestione della chiave univoca (azzere/abblenco il
229400041213      * valore; propongo come ordinamento il 1°)
229500041213     C     $Unique       IFEQ      *ON
229600041213     C                   MOVE      *OFF          $Unique
229700041213     C                   ENDIF
229800041213     C     $ORD          IFEQ      99
229900041213     C                   Z-ADD     1             $ORD
230000041213     C                   MOVE      *ON           $INZD1
230100041213     C                   ENDIF
230200050126      *
230300050126     C                   move      *on           $WriteC1
230400050126     C                   move      *on           $WriteC2
230500041213      *
230600041213     C     XF17S1        ENDSR
230700041213      ************************************************************
230800041213      * GESTIONE F18 VIDEO S1
230900041213      ************************************************************
231000041213     C     F18S1         BEGSR
231100041213      *
231200041213     C                   MOVE      'D2'          $GEST
231300041213     C                   MOVEL     *ON           $INZD2
231400041213     C     $ORD          IFEQ      99
231500041213     C                   Z-ADD     1             $ORD
231600041213     C                   ENDIF
231700041213      *
231800041213     C     XF18S1        ENDSR
231900041213      ************************************************************
232000041213      * GESTIONE F23 VIDEO S1
232100041213      ************************************************************
232200041213     C     F23S1         BEGSR
232300041213      *
232400041213     C                   Z-ADD     $C1Op1        $L01
232500041213     C                   Z-ADD     $C1Op2        $L02
232600041213     C                   EXSR      RIEOPZ
232700041213     C                   Z-ADD     23            X
232800041213     C                   MOVE      FF23          $FC2(X)
232900041213     C                   MOVEL     WOPZ1         C1OPZ1
233000150910     C                   MOVEL     $ALLOPT       H1ALLOPT
233100041213     C                   MOVEL     WOPZ2         C1OPZ2
233200041213     C                   Z-ADD     WCOZ          C1COZ
233300041213      *
233400041213     C     XF23S1        ENDSR
233500041213      ************************************************************
233600041213      * GESTIONE F24 VIDEO S1
233700041213      ************************************************************
233800041213     C     F24S1         BEGSR
233900041213      *
234000041213     C                   MOVEA     $FX2          $FX
234100041213     C                   MOVEA     $FC2          $FC
234200041213     C                   MOVEA     $FR2          $FR
234300041213     C                   Z-ADD     $ULKS1        $ULK
234400041213     C                   Z-ADD     $S1L01        $L01
234500041213     C                   Z-ADD     $S1L02        $L02
234600041213     C                   EXSR      RIEKEY
234700041213     C                   Z-ADD     $ULK          $ULKS1            3 0
234800041213     C                   MOVEA     $FC           $FC2
234900041213     C                   MOVEL     $KEY1         Z2KE1
235000150910     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
235100140701     C****               MOVEL     $KEY2         Z2KE2
235200041213     C                   WRITE     FMTZ2
235300041213      *
235400041213     C     XF24S1        ENDSR
235500041213      ************************************************************
235600041213      * CONTROLLO CAMPI I/O RIGA LISTA
235700160705      ************************************************************
235800041213     C     RECS1         BEGSR
235900041213      *
236000041213      * reset indicatori DSPATR
236100050103      *   se il campo è rimasto in RI per un errore precedente
236200050103      *   la routine di controllo spegne questo RI poi si dovrà fare
236300050103      *   la write del relativo sfl
236400150922     C                   if        *in(42)<>*off
236500050103     C                   eval      $writec1=*on
236600050103     C                   endif
236700150922     C                   MOVE      *ALL'0'       In4245            4
236800150922     C                   MOVEA     In4245        *IN(42)
236900041213      *
237000041213      * Controllo se Utente Autorizzato alla scelta
237100041213     C     S1OPZ         IFNE      0
237200041213     C                   Z-ADD     1             X
237300041213     C     S1OPZ         LOOKUP    $OP1(X)                                21
237400041213      * mi salvo l'indice dell' opzione
237500041213     C   21              Z-add     X             PosOpz            2 0
237600041213     C     *IN21         IFEQ      *OFF
237700041213     C     $FL1(X)       OREQ      'N'
237800041213     C                   MOVE      *ON           $ESCI
237900050427     C                   SETON                                        429199
238000041213     C                   ENDIF
238100041213     C                   ENDIF
238200160408R2212C*
238300160408 "   C* se opz=15=Mostra legame o opz=4=Annulla selezione
238400160408 "   C* ma bolla   non legata, segnalo errore
238500160408 "   C                   if        (S1OPZ      = 15    or
238600160408 "   C                              S1OPZ      = 4        )and
238700160418 "   C                             H1DocLeg   <> '1'
238800160408 "   C*
238900160408 "   C                   SETON                                        429199
239000160408R2212C                   endif
239100160707R2322C*
239200160707  "  C* Controllo se bolla già legata a fattura
239300160707  "  C                   if        S1OPZ       = 44    and
239400160707  "  C                             H1DocLeg    = '1'
239500160707  "  C                   SETON                                        429199
239600160707R2322C                   endif
239700051116      *
239800150922     C                   if        s1opz = 01
239900150922     C                   add       1             SavOpz01
240000160408R2212C                   endif
240100160408 "    *
240200160408 "   C                   if        s1opz = 04
240300160408 "   C                   add       1             SavDel01
240400160408 "   C                   endif
240500160408 "   C*
240600160408 "   C                   if        s1opz = 01
240700160408 "   C                             or
240800160408R2212C                             s1opz = 04
240900160418     C                   Move      H1DocLeg      SavDocLeg01
241000150925     C                   Move      H1Sys         SavSys01
241100150925     C                   Move      H1NrAsReg     SavNrAs01
241200160412R2212C* salvo il numero del record su cui sono posizionato
241300160414R2212X*****              Z-Add     H1NrR         WC1Rcd
241400150922     C                   endif
241500160414R2212C* salvo il numero del record su cui sono posizionato
241600160414R2212C                   Z-Add     S1NrR         H1NrR
241700160414R2212C                   Z-Add     H1NrR         WC1Rcd
241800150922      *
241900150922     C                   if        savopz01 > 1
242000150922     C                   MOVE      *ON           $ESCI
242100150922     C                   SETON                                        427999
242200150922     C                   endif
242300041213      *
242400041213     C     XRecS1        ENDSR
242500041213      ************************************************************
242600041213      * GESTIONE OPZIONI LISTA
242700041213      ************************************************************
242800041213     C     OPZS1         BEGSR
242900041213      *
243000041213     C                   Z-ADD     RIRI          CURR
243100041213     C                   Z-ADD     COCO          CURC
243200160421R2212C*
243300160624R2312X*ex R2212          Eval      $Tasto      = ENTER
243400041213      *
243500041213     C                   SELECT
243600160412R2212C*
243700160412 "   C* se opz=4 =Cancella legami con bolla
243800160412 "   C     S1OPZ         WHENEQ    4
243900160412 "   C     $TASTO        ANDEQ     F06
244000160412 "   C                   Clear                   S1Opz
244100160412R2212C                   EXSR      DelAna01
244200160125     C*
244300160125     C* se opz=15=Mostra legame
244400160125     C     S1OPZ         WHENEQ    15
244500160418     C     H1DocLeg      andEQ     *On
244600160125     C*
244700160421R2212C*
244800160421 "   C                   Eval      $TastoSav   = $Tasto
244900160418R2212C*
245000160418R2212X****               Clear                   FMTD5
245100160126     C*
245200160125     C                   Eval      LRCSocieta  = XScSoc
245300160125     C                   Eval      LRCSys      = H1Sys
245400160125     C                   Eval      LRCNrAsReg  = H1NrAsReg
245500160125     C* cerco i dati della fattura
245600160412     C     K03LRC01      CHAIN(N)  SDGLRC01L                          21
245700160407R2212X****               if        *In21       = *Off
245800160418R2212C                   DoW       *In21       = *Off      and
245900160418 "   C                             $tasto     <> F12
246000160418 "   C*
246100160418 "   C* mostro solo i veri collegamenti, quelli senza fattura
246200160418 "   C* derivano da dati migrati da altri sistemi e servono solo
246300160418 "   C* per mostrare il soggetto che potrebbe servire in fase di
246400160418 "   C* assegnazione
246500160418 "   C                   Clear                   FMTD5
246600160418 "   C*
246700160418 "   C                   if        LRCNrAsL   <> 0
246800160418R2212C*
246900160125     C* salvo i dati del movimento che ho in linea
247000160125     C                   MoveL     NDREG00F      NDREGSAV
247100160125     C                   MoveL     NDMOV00F      NDMOVSAV
247200160125     C*
247300160125     C                   Eval      $Sys        = LRCSysL
247400160125     C                   Eval      $NrAsReg    = LRCNrAsL
247500160125     C                   EXSR      GetNDREG
247600160125     C                   if        $CodOper   <> '9'
247700160125     C     *Jobrun       Move      REGDtReg      D5DtReg
247800160125     C                   Eval      D5NrReg     = REGNrReg
247900160125     C                   Eval      D5SerReg    = REGSerReg
248000160125     C                   endif
248100160125     C*
248200160125     C                   EXSR      GetNDMOV
248300160125     C                   if        $CodOper   <> '9'
248400160126     C                   Eval      D5Kcc       = MOVKcc
248500160126     C                   Eval      D5Ksc       = MOVKsc
248600160125     C     *Jobrun       Move      MOVDtDoc      D5DtDoc
248700160125     C                   Eval      D5NrDoc     = MOVNrDoc
248800160125     C                   Eval      D5SerDoc    = MOVSerDoc
248900160125     C                   endif
249000160125     C*
249100160125     C* ripristino i dati del movimento che ho in linea
249200160125     C                   MoveL     NDREGSAV      NDREG00F
249300160125     C                   MoveL     NDMOVSAV      NDMOV00F
249400160407R2212X****               endif
249500160421R2212C                   Eval      $TastoSav2  = $Tasto
249600160125     C*
249700160125     C                   WRITE     FMTZ5
249800160421R2212C                   Eval      $Tasto      = $TastoSav2
249900160418R2212X**** $tasto        downe     F12
250000160418R2212C                   if        $tasto     <> F12   or
250100160418 "   C                             $tasto      = ENTER or
250200160418R2212C                             $tasto      = F08
250300160125     C                   EXFMT     FMTD5
250400160418R2212C                   endif
250500160418R2212X****               enddo
250600160125     C                   Eval      $LastV      = 'D5'
250700160418R2212C*
250800160418 "   C                   endif
250900160418 "   C*
251000160418 "   C                   if        $tasto     <> ENTER   or
251100160418 "   C                             LRCNrAsL    = 0
251200160418 "   C     K03LRC01      READE(N)  SDGLRC01L                              21
251300160418 "   C                   endif
251400160418 "   C*
251500160407R2212C                   endDo
251600160421R2212C                   Eval      $Tasto      = $TastoSav
251700160708R2321C*
251800160705 "   C* Richiamo prima nota
251900160705 "   C     S1OPZ         WHENEQ    41
252000160705 "   C                   Eval      $Sys        = H1Sys
252100160705 "   C                   Eval      $NrAsReg    = H1NrAsReg
252200160708R2321C                   EXSR      CallPN
252300160705R2322C*
252400160705 "   C* Modifica codice soggetto su bolla NON collegata a fattura
252500160705 "   C     S1OPZ         WHENEQ    44
252600160705 "   C     H1DocLeg      andNE     *On
252700160705R2322C                   EXSR      ModSogg
252800051115     C*
252900041213      * opzione personalizzata
253000041213     C                   when      $PGO1(PosOpz) <> *blank
253100041213     C                   exsr      Opzs1_Per
253200041213      *
253300041213      * se altre opzioni
253400041213     C                   OTHER
253500041213      *
253600041213     C                   ENDSL
253700050127      *
253800050127     C                   move      *on           $writeC1
253900041213      *
254000041213     C     XOpzS1        ENDSR
254100160408R2212C************************************************************
254200160408  "  c* Gestione record allocato
254300160408  "  C************************************************************
254400160408R2212C     GesAllocato   BEGSR
254500160408     C*
254600160408     C* Gestione RCD allocato
254700160408   B3C     *in22         IFEQ      *ON
254800160408     C                   CLEAR                   XMSGDS
254900160408    >C                   MOVEL     'SDGLRC00F'   MSGFIL
255000160408     C                   MOVE      'S'           MSGLCK
255100160408     C                   Z-ADD     2             MSGMSG
255200160408     C                   MOVEL     'R'           MSGTPR
255300160408     C                   MOVEL     'S'           MSGRSP
255400160408     C                   MOVEL     'S'           MSGEMV
255500160408     C                   Z-ADD     10            MSGRGP
255600160408     C                   MOVEA     STATUS        STS
255700160408     C                   MOVEL     'N'           MSGCNL
255800160408     C                   MOVEL     'N'           MSGVID
255900160408     C                   MOVEL     'N'           MSGOPE
256000160408     C                   MOVEL     'N'           MSGSTP
256100160408     C                   CALL      'XMSGR'
256200160408     C                   PARM                    XMSGDS
256300160408   E3C                   endif
256400160408     C*
256500160408R2212C                   ENDSR
256600160408R2212C/EJECT
256700160708R2321C************************************************************
256800160705 "   c* Richiamo PN
256900160705 "   C************************************************************
257000160708R2321C     CallPN        BEGSR
257100160705     C*
257200160705     C* controllo chiamata ricorsiva p.nota
257300160705     C                   Reset                   X48ChPgmDS
257400160705     C                   Eval      X48Pgm      = 'NDC001R'
257500160705     C                   Eval      KPJBU       = X48ChPgmDS
257600160705     C*
257700160705     C                   CALL      'X48CHPGM'
257800160705     C                   PARM                    kpjba
257900160705     C*
258000160705     C                   MoveL     KPJBU         X48ChPgmDS
258100160705     C*
258200160705     C                   if        X48Errore   = *Off
258300160705     C*
258400160705     C                   RESET                   NDC001DS
258500160705     C                   MOVEL     '02'          OPZ001
258600160705     C* passare nei campi appositi della DS C004DS il valore
258700160705     C* della chiave della VL univoca
258800160705     C                   MOVEL     $Sys          UQ1001
258900160705     C                   MOVEL     $NrAsReg      UQ2001
259000160705     C*
259100160705     C                   MoveL     NDC001DS      KPJBU
259200160705     C                   CALL      'NDC001R'
259300160705     C                   PARM                    KPJBA
259400160705     C*
259500160705     C                   MoveL     KPJBU         NDC001DS
259600160705     C*
259700160705     C* ritorno da PGM gestione
259800160705     C                   EXSR      RTN001
259900160705     C                   endif
260000160705     C*
260100160708R2321C                   ENDSR
260200160708R2321C/EJECT
260300160708R2321C************************************************************
260400160705 "   C* GESTIONE RITORNO DA PGM primanota generica
260500160705 "   C************************************************************
260600160708R2321C     RTN001        BEGSR
260700160705     C*
260800160705     C* modo di ritorno
260900160705     C*
261000160705   B1C                   SELECT
261100160705     C* 1 = F3
261200160705     C     RET001        WHENEQ    '1'
261300160705     C                   MOVE      *ON           $ESCI
261400160705     C                   MOVE      *ON           $FINE
261500160705     C                   MOVE      '1'           RETD25
261600160705     C* 2 = F12
261700160705     C     RET001        WHENEQ    '2'
261800160705     C     OPR001        ANDNE     '1'
261900160705     C                   MOVE      *ON           $ESCI
262000160705     C*
262100160705   E1C                   ENDSL
262200160705     C*
262300160705     C* operazione eseguite dal pgm chiamato
262400160705     C*
262500160705    >C                   MOVE      OPR001        WOPR
262600160705     C                   SELECT
262700160705     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
262800160705     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
262900160705    >C     OPR001        WHENEQ    '1'
263000160705     C                   move      *ON           WInzS1
263100160705     C*
263200160705     C                   ENDSL
263300160705     C*
263400160705     C* funzione non eseguibile per errore :
263500160705     C*
263600160705     C                   SELECT
263700160705     C* 1 = funzione richiamata chiusa in errore
263800160705    >C     ERR001        WHENEQ    '1'
263900160705     C                   MOVE      *ON           $ESCI
264000160705     C                   SETON                                        9599
264100160705     C* 2 = funzione richiamata senza la dovuta autorizzazione
264200160705    >C     ERR001        WHENEQ    '2'
264300160705     C                   MOVE      *ON           $ESCI
264400160705     C                   SETON                                        9499
264500160705     C* 3 = registrazione non modificabile
264600160705     C     ERR001        WHENEQ    '3'
264700160705     C                   MOVE      *ON           $ESCI
264800160705     C                   SETON                                        9399
264900160705     C*
265000160705     C                   ENDSL
265100160705     C*
265200160708R2321C                   ENDSR
265300160708R2321C/EJECT
265400160705R2322C************************************************************
265500160705 "   c* Richiamo PN
265600160705 "   C************************************************************
265700160705R2322C     ModSogg       BEGSR
265800160705     C*
265900160705     C                   RESET                   SDGSD44DS
266000160705     C*
266100160705     C                   Eval      SocietaD44  =  XScSoc
266200160705     C                   Eval      SoggD44     =  S1SoggFor
266300160705     C                   Eval      SysD44      =  H1Sys
266400160705     C                   Eval      NrAsRegD44  =  H1NrAsReg
266500160711     C                   Eval      WdwD44      =  WrkWdwD44
266600160705     C*
266700160705     C                   MoveL     SDGSD44DS     KPJBU
266800160705     C*
266900160711     C                   CALL      'SDGSD44R'
267000160705     C                   PARM                    KPJBA
267100160705     C*
267200160705     C                   MOVEL     KPJBU         SDGSD44DS
267300160705     C*
267400160705     C                   EXSR      RTND44
267500160705     C*
267600160705     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
267700160705     C     *IN99         IFEQ      *ON
267800160705     C                   SETON                                        40
267900160705     C                   ENDIF
268000160705     C*
268100160705     C                   MOVE      *HIVAL        $LASTV
268200160705     C*
268300160705     C* cancello userspace
268400160705     C                   EXSR      DelUsrSpc
268500160705     C*
268600160705R2322C                   ENDSR
268700160705R2322C/EJECT
268800160705R2322C************************************************************
268900160705  "  C* GESTIONE RITORNO DA PGM modifica codice soggetto
269000160705  "  C************************************************************
269100160705R2322C     RTND44        BEGSR
269200160705     C*
269300160711     C                   Eval      WrkWdwD44 = '0'
269400160705     C* modo di ritorno
269500160705     C*
269600160705     C                   SELECT
269700160705     C* questi modi di utilizzo dei valori di ritorno dal
269800160705     C* pgm di manutenzione rcd di anagrafica sono delle
269900160705     C* proposte, normalmente sempre valide, ma modificabili
270000160705     C* per situazioni particolari
270100160705     C* 1 = F3
270200160705     C     RETD44        WHENEQ    '1'
270300160705     C                   MOVE      *ON           $ESCI
270400160705     C                   MOVE      *ON           $FINE
270500160705     C* 2 = F12
270600160705     C     RETD44        WHENEQ    '2'
270700160705     C                   MOVE      *ON           $ESCI
270800160705     C*
270900160705     C                   ENDSL
271000160705     C*
271100160705     C* operazione eseguite dal pgm chiamato
271200160705     C*
271300160705     C                   SELECT
271400160705     C* 1 = eseguito aggiornamento
271500160705     C     OPRD44        WHENEQ    '1'
271600160705     C                   MOVE      *ON           WINZS1
271700160705     C                   ENDSL
271800160705     C*
271900160705     C* funzione non eseguibile per errore :
272000160705     C*
272100160705     C                   SELECT
272200160705     C* 1 = funzione richiamata chiusa in errore
272300160705     C     ERRD44        WHENEQ    '1'
272400160705     C                   MOVE      *ON           $ESCI
272500160705     C                   SETON                                        5299
272600160705     C* 2 = funzione richiamata senza la dovuta autorizzazione
272700160705     C     ERRD44        WHENEQ    '2'
272800160705     C                   MOVE      *ON           $ESCI
272900160705     C                   SETON                                        5499
273000160705     C*
273100160705     C                   ENDSL
273200160705     C*
273300160705R2322C                   ENDSR
273400160708R2322C/EJECT
273500160705R2322 ************************************************************
273600160705  "   * Cancellazione userspace parametri creati da transfer-job *
273700160705  "   ************************************************************
273800160705D2322C     DelUsrSpc     BEGSR
273900160705      *
274000160705     C                   MoveL     Job_Num       Job_Num_A
274100160705     C                   Eval      USName = 'XJ' + Job_Num_A
274200160705      * - parametri
274300160705     C                   Clear                   Cmd
274400160705     C                   Eval      Cmd = 'DLTUSRSPC USRSPC(QGPL/' +
274500160705     C                                   USNAme + ')'
274600160705     C                   Call      'QCMDEXC'                            22
274700160705     C                   Parm                    Cmd             100
274800160705     C                   Parm      100           LenCmd           15 5
274900160705      * - KPJBA
275000160705     C                   Clear                   Cmd
275100160705     C                   Eval      Cmd = 'DLTUSRSPC USRSPC(QGPL/' +
275200160705     C                                   %TrimR(USNAme) + 'KP)'
275300160705     C                   Call      'QCMDEXC'                            22
275400160705     C                   Parm                    Cmd             100
275500160705     C                   Parm      100           LenCmd           15 5
275600160705      *
275700160708R2322C                   ENDSR
275800160705R2322C/EJECT
275900160125     C************************************************************
276000160125     c* Reperisce dati della registrazione
276100160125     C************************************************************
276200160125     C     GetNDREG      BEGSR
276300160125     C*
276400160125     C                   Eval      $LenOut     = %size(NDREG00F)
276500160125     C                   CALLB     'NDDRVREG'
276600160125     C                   PARM                    $Sys
276700160125     C                   PARM                    $NrAsReg
276800160125     C                   PARM      'NDREG     '  $Struttura
276900160125     C                   PARM                    $LenOut
277000160125     C                   PARM      '1'           $CodOper
277100160125     C                   PARM                    NDREG00F
277200160125     C*
277300160125     C                   ENDSR
277400160125     C/EJECT
277500160125     C************************************************************
277600160125     c* Reperisce dati del movimento
277700160125     C************************************************************
277800160125     C     GetNDMOV      BEGSR
277900160125     C*
278000160125     C                   Eval      $LenOut     = %size(NDMOV00F)
278100160125     C                   CALLB     'NDDRVMOV'
278200160125     C                   PARM                    $Sys
278300160125     C                   PARM                    $NrAsReg
278400160125     C* cerco la riga del cliente/fornitore (è sempre la num.1)
278500160125     C* quella dell'IVA è sempre la 3
278600160125     C                   PARM      1             $NrRigaM
278700160125     C                   PARM      'NDMOV     '  $Struttura
278800160125     C                   PARM                    $LenOut
278900160125     C                   PARM      '1'           $CodOper
279000160125     C                   PARM                    NDMOV00F
279100160125     C*
279200160125     C                   ENDSR
279300160125     C/EJECT
279400041213      ************************************************************
279500041213      * opzione personalizzata
279600041213      ************************************************************
279700041213     C     Opzs1_Per     Begsr
279800041213      *
279900150812     C                   movel     SDGSD25DS     $SaveDS
280000041213      *
280100150812     C                   RESET                   SDGSD25DS
280200150910     C                   MOVE      xscsoc        SocietaD25
280300150910     C                   MOVEL     S1OPZ         OPZD25
280400150812      * passare nei campi appositi della DS SDGSD25DS il valore
280500041213      * della chiave della VL univoca
280600150910     C                   movel     xscsoc        SocietaD25
280700150922     C                   z-add     H1Sys         SySD25
280800150922     C                   z-add     H1NrAsReg     NrAsRegD25
280900041213      *
281000150812     C                   MOVEL     SDGSD25DS     KPJBU
281100041213     C                   CALL      $PGO1(PosOpz)                        22
281200041213     C                   PARM                    KPJBA
281300140701     C***                PARM                    NOMPGM
281400041213      *
281500150812     C                   MOVEL     KPJBU         SDGSD25DS
281600041213      * ritorno da PGM
281700041213     C                   exsr      Opzs1_RtnPer
281800041213      * ripristino la DS di ingresso
281900150812     C                   MOVEL     $SaveDS       SDGSD25DS
282000041213      *
282100041213     C     XOpzS1_Per    Endsr
282200041213      ************************************************************
282300041213      * ritorno da opzione personalizzata
282400041213      ************************************************************
282500041213     C     Opzs1_RtnPer  Begsr
282600041213      *----------------------------------------------------------------
282700041213      * modo di ritorno
282800041213     C                   select
282900041213      * 1 = F3
283000150910     C                   when      RetD25 = '1'
283100041213     C                   move      *on           $Esci
283200041213     C                   move      *on           $Fine
283300041213      * 2 = F12
283400041213      * mi fermo sulla riga del sfl in oggetto senza fare nulla,
283500041213      * se non sono stati fatti aggiornamenti
283600041213      * (devo ricaricare il sfl se sono stati fatti aggiornamenti)
283700150910     C                   when      RetD25 = '2' and OprD25 <> '1'
283800041213     C                   move      *on           $Esci
283900041213     C                   endsl
284000041213      *----------------------------------------------------------------
284100041213      * operazione eseguite dal pgm chiamato
284200041213     C                   select
284300041213      * 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
284400150910     C                   when      OprD25 = '1'
284500150910     C                   move      OprD25        WOpr
284600041213      *
284700041213      * se l'opzione con cui è stato chiamato non è la modifica
284800041213      * memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
284900150910     C                   if        OpzD25 <> '02'
285000041213     C                   move      *on           WInzS1
285100041213      * se l'opzione con cui è stato chiamato è la modifica
285200041213      * aggiorno subito la riga di sfl e non richiedo il ripristino sfl
285300041213     C                   else
285400041213     C                   exsr      AggS1
285500041213     C                   endif
285600041213     C                   endsl
285700041213      *----------------------------------------------------------------
285800041213      * funzione non eseguibile per errore :
285900041213     C                   select
286000041213      * 1 = funzione richiamata chiusa in errore
286100041213      *   PRO0007-(F1) La funzione richiamata si è chiusa in errore
286200150910     C                   when      ErrD25 = '1'
286300041213     C                   move      *on           $Esci
286400041213     C                   eval      *in93 = *on
286500041213     C                   eval      *in99 = *on
286600041213      * 2 = funzione richiamata senza la dovuta autorizzazione
286700150910     C                   when      ErrD25 = '2'
286800041213     C                   move      *on           $Esci
286900041213     C                   eval      *in94 = *on
287000041213     C                   eval      *in99 = *on
287100041213     C                   endsl
287200041213      *
287300041213     C     XOpzs1_RtnPer Endsr
287400041213      ************************************************************
287500041213      * AGGIORNAMENTO RIGA LISTA
287600041213      ************************************************************
287700041213     C     AGGS1         BEGSR
287800041213      *
287900041213      * riempio i campi del sfl che sono stati ripassati dal pgm
288000150812      * di manutenzione dopo una opzione 02
288100041213      *
288200041213     C     XAggS1        ENDSR
288300041213      ************************************************************
288400041217*******S2 INIZIO**************************************************
288500041217      ************************************************************
288600041217      * GESTIONE LISTA 2
288700041217      ************************************************************
288800041217     C     GESS2         BEGSR
288900041217      * inizializzazione videata
289000041217     C     $INZS2        IFEQ      *ON
289100041217     C                   EXSR      INZS2
289200041217     C                   MOVE      *OFF          $INZS2
289300041217     C                   ENDIF
289400041217      * inizializzazione videata
289500041217     C     $INZS1        IFEQ      *ON
289600041217     C                   EXSR      INZS1
289700041217     C                   MOVE      *OFF          $INZS1
289800041217     C                   ENDIF
289900041217      * emissione piede videata se proveniente da altra
290000041217      * salvataggio valore formato di provenienza
290100041217     C                   if        $LastV <> 'S2' and
290200041217     C                             $LastV <> 'S1'
290300041217     C                   WRITE     FMTT1
290400041217     C                   EXSR      GESPIE
290500041217     C                   MOVE      $LASTV        $LASTG
290600041217     C                   MOVE      'S2'          $LASTV
290700041217     C                   ENDIF
290800041217      *
290900041217      * S1
291000041217     C                   if        WMaxS1 = 0
291100041217     C                   write     FmtD3
291200041217     C                   endif
291300041223     C                   if        $writec1=*on
291400041217     C                   write     FmtC1
291500041223     C                   eval      $writec1=*off
291600041222     C                   endif
291700041217      *
291800041217      * S2
291900041217     C                   if        WMaxS2 = 0
292000041217     C                   write     FmtD4
292100041217     C                   endif
292200041217      * Inizializzazione indice tasti funzione personalizzati
292300041217     C                   eval      PosFun = 25
292400041217      *
292500041217     C                   EXFMT     FMTC2
292600041217      *
292700041217      * azzero la posizione del cursore
292800041217     C                   Z-ADD     0             H1RIGA
292900041217     C                   Z-ADD     0             H1COLO
293000041217      * Serve per avvertire i pgm di sola finestra di eseguire le operazioni
293100041217      * di refresh dello sfondo
293200041217     C                   MOVE      '1'           WExf
293300041217      *
293400041217     C     C2NRR         IFNE      0
293500041217     C                   Z-ADD     C2NRR         WSflS2
293600160414     C                   Z-ADD     C2NRR         C2RCD
293700041217     C                   ENDIF
293800041222     C**                 IF        SFLNRR > 0
293900041222     C**                 Z-ADD     SFLNRR        C2RCD
294000041222     C**                 ENDIF
294100041217      * inizializzazione indice opzioni personalizzate
294200041217     C                   Eval      PosOpz = 0
294300041217      * CONTROLLO ABILITAZIONE TASTI
294400041217      * INPUT
294500041217      * $FC2 = Abilitazioni videata S1
294600041217      * OUTPUT
294700041217      * *IN97 + *IN99 Errori
294800041217     C                   MOVEA     $FC2          $FC
294900041217     C                   EXSR      CTRKEY
295000041217      *
295100041217     C     *IN99         IFEQ      *OFF
295200041217      *
295300041217     C                   SELECT
295400041217      * F3=Fine
295500041217     C     $TASTO        WHENEQ    F03
295600041217     C                   EXSR      F03S1
295700041217      * F8=Successivo
295800041217     C     $TASTO        WHENEQ    F08
295900041217     C                   EXSR      F08S1
296000041217      * F12=Ritorno
296100041217     C     $TASTO        WHENEQ    F12
296200041217     C                   EXSR      F12S1
296300041217      * F17=Parzializzazioni
296400041217     C     $TASTO        WHENEQ    F17
296500041217     C                   EXSR      F17S1
296600041217      * F18=Modifica valori assunti
296700041217     C     $TASTO        WHENEQ    F18
296800041217     C                   EXSR      F18S1
296900041217      * F23=Altre opzioni
297000041217     C     $TASTO        WHENEQ    F23
297100041217     C                   EXSR      F23S1
297200041217      * F24=Altri tasti
297300041217     C     $TASTO        WHENEQ    F24
297400041217     C                   EXSR      F24S1
297500041217      * ROLL-UP
297600041217     C     $TASTO        WHENEQ    ROLLUP
297700041220     C                   EXSR      ROLS2
297800041217      * Personalizzata
297900041217     C     $PGF2(PosFun) Whenne    *BLANK
298000041217     C                   EXSR      FperS1
298100041217      *
298200041217     C                   OTHER
298300041221      *
298400041221      * CONTROLLI su entrambi i sfl
298500041222      *   il prossimo sfl da gestire sarà quello in errore
298600041222      *   oppure quello sul quale è il cursore
298700041221     C                   eval      WGest = $Gest
298800041221     C                   exsr      CtrS
298900041221     C                   eval      $Gest = WGest
299000041217      *
299100041217     C                   ENDSL
299200041217      *
299300041217     C                   ENDIF
299400041217      *
299500041217     C     XGesS2        ENDSR
299600041221      ************************************************************
299700041221      * Controlli sfl S1 e S2
299800041221      ************************************************************
299900041221     C     CtrS          Begsr
300000041221      *
300100050103     C                   eval      $SflErr = *blank
300200041221      *
300300041221      * VALIDITA' OPZIONI (<> 1)--------------------------------------
300400041222      *
300500041222      * Inizio a fare i controlli dal sfl sul quale è il cursore
300600150812     C                   if        H1NmRc = 'SD25S1'
300700041221      *
300800041221     C                   exsr      CtrS_S1_I
300900041221     C                   if        *in99 = *off
301000041221     C                   exsr      CtrS_S2_I
301100041221     C                   endif
301200041221      *
301300041221     C                   else
301400041221      *
301500041221     C                   exsr      CtrS_S2_I
301600041221     C                   if        *in99 = *off
301700041221     C                   exsr      CtrS_S1_I
301800041221     C                   endif
301900041221      *
302000041221     C                   endif
302100041221      *
302200041221      * RISOLVO OPZIONI------------------------------------------------
302300041222      *
302400041221      *  se opzioni S1 e S2 ok, 'risolvo' opzioni <> 1 sui sfl
302500041221     C                   if        *in99 = *off
302600041221      *
302700041222      *   Inizio a fare i controlli dal sfl sul quale è il cursore
302800150812     C                   if        H1NmRc = 'SD25S1'
302900041222      *
303000041221     C                   exsr      CtrS_S1_II
303100041221     C                   if        *in99 = *off
303200041221     C                   exsr      CtrS_S2_II
303300041221     C                   endif
303400041221      *
303500041221     C                   else
303600041221      *
303700041221     C                   exsr      CtrS_S2_II
303800041221     C                   if        *in99 = *off
303900041221     C                   exsr      CtrS_S1_II
304000041221     C                   endif
304100041221      *
304200041221     C                   endif
304300041221     C                   endif
304400050427      *
304500050427      *
304600041221      * OPZIONI = 1--------------------------------------------
304700051116      *    ----------------------------------------------------
304800160408     C* controllo congruenza record  scelti per opzione '1'
304900160408     C                   if        *in99 = *off
305000160408     C*  se non sono stati scelti movimenti
305100160408R2212C*
305200160412 "   C* presenti sia opz. 1 che opz. 4
305300160408 "   C                   if        (Savopz01 > 0  or
305400160408 "   C                              Savopz02 > 0     ) and
305500160408 "   C                             (SavDel01 > 0  or
305600160408 "   C                              SavDel02 > 0     )
305700160408 "   C                   SETON                                        7599
305800160408 "   C                   move      *on           $WriteC2
305900160408 "   C                   EndIf
306000160418R2212C*
306100160412R2212X****               if        savopz01 = 0
306200160412R2212C*
306300160412 "   C* NON presenti opzioni o solo una scelta
306400160412 "   C                   if        Savopz01 <= 1 and
306500160408 "   C                             Savopz02 = 0  and
306600160408 "   C                             SavDel01 = 0  and
306700160408 "   C                             SavDel02 = 0
306800160408R2212C                   eval      *in99 = *on
306900160408     C                   eval      *in80 = *on
307000160408     C                   move      *on           $WriteC1
307100160408     C                   endif
307200160408     C*
307300160412R2212X****               if        savopz02 = 0
307400160412R2212C*
307500160412 "   C* NON presenti opzioni o solo una scelta
307600160412 "   C                   if        Savopz02 <= 1 and
307700160408 "   C                             Savopz01 = 0  and
307800160408 "   C                             SavDel01 = 0  and
307900160412R2212C                             SavDel02 = 0
308000160408     C                   eval      *in99 = *on
308100160408     C                   eval      *in70 = *on
308200160408     C                   move      *on           $WriteC2
308300160408     C                   endif
308400160408     C* Controllo coerenza della scelta
308500160408     C                   If        *in99 = *off
308600160408     C*
308700160408R2212X****               If        SavDocLeg01 <> SavDocLeg02
308800160412R2212C*
308900160412 "   C* se cerco di legare, la fattura non deve essere già legata
309000160412 "   C*
309100160412 "   C                   If        SavOpz01     = 1            and
309200160412 "   C                             SavOpz02     = 1            and
309300160412R2212C                             SavDocLeg02 = *On
309400160408     C                   SETON                                        7599
309500160408     C                   move      *on           $WriteC2
309600160408     C                   EndIf
309700160408     C                   EndIf
309800160408     C*
309900160408     C
310000160408     C                   endif
310100041222      *
310200041223      * F6-----------------------------------------------------
310300041222     C                   if        $Tasto = F06
310400160412R2212C                             and
310500160412 "   C                             SavOpz01     = 1            and
310600160412R2212C                             SavOpz02     = 1
310700041222      *
310800041222      *
310900150923      *    se non esiste nessun creo o aggiorno il legame
311000041222     C                   if        *in99=*off
311100041222     C                   exsr      AggAna
311200041222      *       segnalo al pgm chiamante l'avvenuta conferma
311300041222     C                   move      *on           WOpr
311400160413R2212C                   move      *on           WOprS1
311500160413R2212C                   move      *on           WOprS2
311600041222      *       ripristino la videata
311700160413     C                   move      *on           $InzS1
311800160412     C                   move      *off          WInzS1
311900160413     C                   move      *on           $InzS2
312000160412     C                   move      *off          WInzS2
312100041222     C                   move      'S1'          WGest
312200041222     C                   endif
312300041222      *
312400041222     C                   endif
312500041222      *
312600041222      * -------------------------------------------------------
312700050103      * Se non si è verificato un errore riferito a solo uno dei 2 sfl
312800041222      * passo a gestire il sfl sul quale è posizionato il cursore
312900050103     C                   if        $SflErr <> *blank
313000050103     C                   eval      WGest = $SflErr
313100050103     C                   else
313200150812     C                   if        H1NmRc = 'SD25S1'
313300041222     C                   eval      WGest = 'S1'
313400041222     C                   else
313500041222     C                   eval      WGest = 'S2'
313600041222     C                   endif
313700041222     C                   endif
313800041221      *
313900041221     C     XCtrS         Endsr
314000041221      ************************************************************
314100041221      * Controllo opzioni sfl S1
314200041221      ************************************************************
314300041221     C     CtrS_S1_I     Begsr
314400041221      *
314500041221     C                   eval      *in99 = *off
314600041221     C                   eval      $Esci = *off
314700041221     C                   eval      S1opz = 0
314800150923     C                   clear                   SavOpz01
314900160408R2212C                   clear                   SavDel01
315000160408     C                   clear                   SavDocLeg01
315100150923     C                   clear                   SavSys01
315200150923     C                   clear                   SavNrAs01
315300041221      *
315400041221      * Leggo il sfl solo se ci sono rcd
315500041221     C                   if        WMaxS1 > 0
315600041221     C                   readc     FmtS1                                  21
315700041221      *
315800041221      * esce se fine sfl o errore che richiede l'uscita
315900041221     C                   dow       *in21 = *off  and  $Esci = *off
316000041221      *
316100041221      *  ctrl opzioni su riga
316200041221     C                   exsr      RecS1
316300041221      *
316400041221      *  la riga è da rileggere se errore o opz valida
316500041221     C                   if        *in99 = *on  or  S1Opz <> 0
316600041221     C                   move      *on           *in32
316700041221     C                   endif
316800041221      *  se errore, passerò a gestire il sfl in errore
316900041221      *  nella pagina della riga appena letta
317000041221     C                   if        *in99 = *on
317100050103     C                   eval      $SflErr = 'S1'
317200041221     C                   z-add     S1Nrr         C1Rcd
317300041221     C                   endif
317400041221      *  attributi sfl
317500041221     C                   exsr      AtrS1
317600041221      *  aggiorna sfl
317700041221     C                   update    FmtS1
317800041221     C                   move      *off          *in32
317900041221      *
318000041221      *  leggo prossimo rcd dal sfl se no errori
318100041221     C                   if        $Esci = *off
318200041221     C                   readc     FmtS1                                  21
318300041221     C                   endif
318400041221     C                   enddo
318500041221      *
318600041221     C                   endif
318700041221      *
318800041221     C     XCtrS_S1_I    Endsr
318900041221      ************************************************************
319000041221      * Controllo opzioni sfl S2
319100041221      ************************************************************
319200041221     C     CtrS_S2_I     Begsr
319300041221      *
319400041221     C                   eval      *in99 = *off
319500041221     C                   eval      $Esci = *off
319600041221     C                   eval      S2opz = 0
319700150922     c                   clear                   SavOpz02
319800160408R2212c                   clear                   SavDel02
319900160408     C                   clear                   SavDocLeg02
320000150924     C                   clear                   SavSys02
320100150924     C                   clear                   SavNrAs02
320200150924     C                   clear                   SavSogg02
320300041221      *
320400041221      * Leggo il sfl solo se ci sono rcd
320500041221     C                   if        WMaxS2 > 0
320600041221     C                   readc     FmtS2                                  21
320700041221      *
320800041221      * esce se fine sfl o errore che richiede l'uscita
320900041221     C                   dow       *in21 = *off  and  $Esci = *off
321000041221      *
321100041221      *  ctrl opzioni su riga
321200041221     C                   exsr      RecS2
321300041221      *
321400041221      *  la riga è da rileggere se errore o opz valida
321500041221     C                   if        *in99 = *on  or  S2Opz <> 0
321600041221     C                   move      *on           *in37
321700041221     C                   endif
321800041221      *  se errore, passerò a gestire il sfl in errore
321900041221      *  nella pagina della riga appena letta
322000041221     C                   if        *in99 = *on
322100050103     C                   eval      $SflErr = 'S2'
322200041221     C                   z-add     S2Nrr         C2Rcd
322300041221     C                   endif
322400041221      *
322500041221      *  attributi sfl
322600041221     C                   exsr      AtrS2
322700041221      *  aggiorna sfl
322800041221     C                   update    FmtS2
322900041221     C                   move      *off          *in37
323000041221      *
323100041221      *  leggo prossimo rcd dal sfl se no errori
323200041221     C                   if        $Esci = *off
323300041221     C                   readc     FmtS2                                  21
323400041221     C                   endif
323500041221     C                   enddo
323600041221      *
323700041221     C                   endif
323800041221      *
323900041221     C     XCtrS_S2_I    Endsr
324000041221      ************************************************************
324100041221      * 'Risolvo' opzioni sfl S1  (eccetto 1=scelta)
324200041221      ************************************************************
324300041221     C     CtrS_S1_II    Begsr
324400041221      *
324500041221     C                   eval      *in99 = *off
324600041221     C                   eval      $Esci = *off
324700160412     C                   eval      S1opz = 0
324800041221      *
324900041221      * Leggo il sfl solo se ci sono rcd
325000041221     C                   if        WMaxS1 > 0
325100041221     C                   readc     FmtS1                                  21
325200041221      *
325300041221      * esce se fine sfl o errore che richiede l'uscita
325400041221     C                   dow       *in21 = *off  and  $Esci = *off
325500041221     C                   z-add     S1Nrr         C1Rcd
325600041221      *  risolvo opzioni su riga (eccetto l'1 di scelta)
325700051116     C                   if        S1Opz <> 0 and S1Opz <> 1
325800041221     C                   exsr      OpzS1
325900041221     C                   endif
326000041221      *  attivo sflnxtchg se rilevato errore o se devo ancora
326100051116      *  analizzare l'opz. 1 e l'opz 30
326200041221     C                   if        *in99 = *on  or  S1Opz = 1
326300160412R2212C                             or  S1Opz = 4
326400041221     C                   move      *on           *in32
326500041221     C                   endif
326600041221      *  passerò a gestire il sfl in errore
326700041221     C                   if        *in99 = *on
326800050103     C                   eval      $SflErr = 'S1'
326900041221     C                   endif
327000041221      *  attributi sfl
327100041221     C                   exsr      AtrS1
327200041221      *  aggiorna sfl
327300140714     C                   if        S1Opz <> 1
327400160408R2212C                             and  S1Opz <> 4
327500041221     C                   z-add     0             S1Opz
327600041221     C                   endif
327700041221     C                   update    FmtS1
327800041221     C                   move      *off          *in32
327900041221      *
328000041221      *  leggo prossimo rcd dal sfl se no errori
328100041221     C                   if        $Esci = *off
328200041221     C                   readc     FmtS1                                  21
328300041221      *   a fine ciclo ripristino il flag richiesta iniz.sfl
328400041221     C                   if        *in21 = *on
328500041221     C                   move      WInzS1        $InzS1
328600041221     C                   move      *off          WInzS1
328700041221      *   se è avvenuta almeno una modifica
328800041221     C                   if        $InzS1 = *on
328900041221      *   calcolo pagina a cui deve posizionarsi
329000041221     C                   if        WSflS1 > 0
329100041221     C                   exsr      ClcPagS1
329200041221     C                   else
329300041221     C                   z-add     SflPagS1      WPagS1
329400041221     C                   endif
329500041221     C                   endif
329600041221     C                   endif
329700041221     C                   endif
329800041221      *
329900041221     C                   enddo
330000041221      *
330100041221     C                   endif
330200041221      *
330300041221     C     XCtrS_S1_II   Endsr
330400041221      ************************************************************
330500041221      * 'Risolvo' opzioni sfl S2  (eccetto 1=scelta)
330600041221      ************************************************************
330700041221     C     CtrS_S2_II    Begsr
330800041221      *
330900041221     C                   eval      *in99 = *off
331000041221     C                   eval      $Esci = *off
331100041221     C                   eval      S2opz = 0
331200041221      *
331300041221      * Leggo il sfl solo se ci sono rcd
331400041221     C                   if        WMaxS2 > 0
331500041221     C                   readc     FmtS2                                  21
331600041221      *
331700041221      * esce se fine sfl o errore che richiede l'uscita
331800041221     C                   dow       *in21 = *off  and  $Esci = *off
331900041221     C                   z-add     S2Nrr         C2Rcd
332000041221      *  risolvo opzioni su riga (eccetto l'1 di scelta)
332100160408R2212 *  ed eccetto il 4= annulla selezione
332200041221     C                   if        S2Opz <> 0  and S2Opz <> 1
332300041221     C                   exsr      OpzS2
332400041221     C                   endif
332500041221      *  attivo sflnxtchg se rilevato errore o se devo ancora
332600041221      *  analizzare l'opz. 1
332700160408R2212 *  oppure     l'opz. 4
332800041221     C                   if        *in99 = *on  or  S2Opz = 1
332900160408R2212C                             or  S2Opz = 4
333000041221     C                   move      *on           *in37
333100041221     C                   endif
333200041221      *  passerò a gestire il sfl in errore
333300041221     C                   if        *in99 = *on
333400050103     C                   eval      $SflErr = 'S2'
333500041221     C                   endif
333600041221      *  attributi sfl
333700041221     C                   exsr      AtrS2
333800041221      *  aggiorna sfl
333900041221     C                   if        S2Opz <> 1
334000160408R2212C                             and S2Opz <> 4
334100041221     C                   z-add     0             S2Opz
334200041221     C                   endif
334300041221     C                   update    FmtS2
334400041221     C                   move      *off          *in37
334500041221      *
334600041221      *  leggo prossimo rcd dal sfl se no errori
334700041221     C                   if        $Esci = *off
334800041221     C                   readc     FmtS2                                  21
334900041221      *   a fine ciclo ripristino il flag richiesta iniz.sfl
335000041221     C                   if        *in21 = *on
335100041221     C                   move      WInzS2        $InzS2
335200041221     C                   move      *off          WInzS2
335300041221      *   se è avvenuta almeno una modifica
335400041221     C                   if        $InzS2 = *on
335500041221      *   calcolo pagina a cui deve posizionarsi
335600041221     C                   if        WSflS2 > 0
335700041221     C                   exsr      ClcPagS2
335800041221     C                   else
335900041221     C                   z-add     SflPagS2      WPagS2
336000041221     C                   endif
336100041221     C                   endif
336200041221     C                   endif
336300041221     C                   endif
336400041221      *
336500041221     C                   enddo
336600041221      *
336700041221     C                   endif
336800041221      *
336900041221     C     XCtrS_S2_II   Endsr
337000141007     C/eject
337100160412R2212 ************************************************************
337200160412 "    * Cancello legami con la bolla
337300160412 "    ************************************************************
337400160412R2212C     DelAna01      BEGSR
337500160412      *
337600160412     C                   Eval      LRCSocieta  = XScSoc
337700160412     C                   Eval      LRCSys      = H1Sys
337800160412     C                   Eval      LRCNrAsReg  = H1NrAsReg
337900160412     C* cerco tutti legami con le varie fatture e li cancello
338000160412     C     K03LRC01      CHAIN     SDGLRC01L                          2122
338100160412     C                   DoW       *In21       = *Off
338200160412     C                   EXSR      GesAllocato
338300160412     C*
338400160412     C                   if        *In21       = *Off
338500160412     C                   Delete    SdGLRC000
338600160412     C                   endif
338700160412     C*
338800160412     C     K03LRC01      READE     SDGLRC01L                              21
338900160412     C                   endDo
339000160412      *
339100160412      *       segnalo al pgm chiamante l'avvenuta conferma
339200160412     C                   move      *on           WOpr
339300160414R2212C                   move      *on           WOprS1
339400160701R2312X*ex R2212          move      *on           WOprS2
339500160412      *       ripristino la videata
339600160412     C                   move      *on           $InzS1
339601170911R266 C                   move      *on           WInzS1
339700170911R266 X**                 move      *off          WInzS1
339800160412     C                   move      *on           $InzS2
339900170912     C                   move      *off          WInzS2
340000160412     C                   move      'S1'          WGest
340100160412      *
340200160418     C                   eval      S1SoggFor   = *Blank
340300160418     C                   eval      S1SoggForD  = *Blank
340400160418      *
340500160418     C                   eval      H1DocLeg    = *Off
340502170912      *
340600160414     C                   EXSR      ATRS1
340700160624R2312C*
340800160627R2312C                   Sub       1             SavDel01
340801170912R266 C*
340802170912  "  C                   IF        D1BolLeg = '1'
340803170912  "  C                   Sub       1             WsflS1
340804170912R266 C                   Endif
340900160412      *
341000160412R2212C                   ENDSR
341100160412R2212C/EJECT
341200160412R2212 ************************************************************
341300160412 "    * Cancello legame con la fattura
341400160412 "    ************************************************************
341500160412R2212C     DelAna02      BEGSR
341600160412     C*
341700160412     C                   Eval      LRCSocieta  = XScSoc
341800160412     C                   Eval      LRCSys      = H2Sys
341900160412     C                   Eval      LRCNrAsL    = H2NrAsReg
342000160412     C* cerco il legame con la bolla e lo cancello
342100160412     C     K03LRC02      CHAIN     SDGLRC02L                          2122
342200160412     C                   EXSR      GesAllocato
342300160412     C                   if        *In21       = *Off
342400160412     C                   Delete    SdGLRC002
342500160412     C                   endif
342600160412      *
342700160412      *       segnalo al pgm chiamante l'avvenuta conferma
342800160412     C                   move      *on           WOpr
342900160701R2312X*ex R2212          move      *on           WOprS1
343000160414R2212C                   move      *on           WOprS2
343100160412      *       ripristino la videata
343200160412     C                   move      *on           $InzS1
343300160412     C                   move      *off          WInzS1
343400160412     C                   move      *on           $InzS2
343401170911R266 C                   move      *on           WInzS2
343500170911R266 X***                move      *off          WInzS2
343600160412     C                   move      'S2'          WGest
343700160412      *
343800160418     C                   eval      H2DocLeg    = *Off
343900160414     C                   EXSR      ATRS2
344000160624R2312C*
344100160627R2312C                   Sub       1             SavDel02
344101170912R266 C*
344102170912  "  C                   IF        D1BolLeg = '1'
344103170912  "  C                   Sub       1             wsfls2
344104170912R266 C                   Endif
344200160412     C*
344300160412R2212C                   ENDSR
344400160412R2212C/EJECT
344500041223      ************************************************************
344600150924      * Selezione
344700041223      ************************************************************
344800041223     C     AggAna        Begsr
344900041223      *
345000150923     C                   Eval      LRCSocieta = Xscsoc
345100160408R2212X****               Eval      LRCSys = SavSys01
345200160408R2212X****               Eval      LRCNrasReg = SavNrAs01
345300160414R2212C                   Eval      LRCSysL    = SavSys02
345400160413R2212C                   Eval      LRCNrasL   = SavNrAs02
345500150923     C*
345600160408R2212X**** K03Lrc01      Chain     SdgLRC01l                          2122
345700160408R2212C     K03Lrc02      Chain     SdgLRC02l                          2122
345800150923     C*
345900150923     C* Gestione RCD allocato
346000150923   B3C     *in22         IFEQ      *ON
346100150923     C                   CLEAR                   XMSGDS
346200150923    >C                   MOVEL     'SDGLRC00F'   MSGFIL
346300150923     C                   MOVE      'S'           MSGLCK
346400150923     C                   Z-ADD     2             MSGMSG
346500150923     C                   MOVEL     'R'           MSGTPR
346600150923     C                   MOVEL     'S'           MSGRSP
346700150923     C                   MOVEL     'S'           MSGEMV
346800150923     C                   Z-ADD     10            MSGRGP
346900150923     C                   MOVEA     STATUS        STS
347000150923     C                   MOVEL     'N'           MSGCNL
347100150923     C                   MOVEL     'N'           MSGVID
347200150923     C                   MOVEL     'N'           MSGOPE
347300150923     C                   MOVEL     'N'           MSGSTP
347400150923     C                   CALL      'XMSGR'
347500150923     C                   PARM                    XMSGDS
347600150923   E3C                   endif
347700150923     C*
347800150923     C                   Exsr      RieAna
347900150923     C*
348000160408     C* Aggiornamento
348100160408     C     *in21         IfEq      *off
348200160408     C                   Update    SdGLRC000                            22
348300160408     C                   Else
348400160408     C                   Write     SDGLRC000                            22
348500160408     C                   EndIf
348600150923     C* se rcd non scrivibile attivo errore
348700150923     C     *IN22         IfEq      *ON
348800150923     C                   SetOn                                        9599
348900150923     C                   EndIf
349000041223      *
349100041223     C     XAggAna       Endsr
349200150923     C/EJECT
349300150923     C**************************************************6*********
349400150923     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
349500150923     C************************************************************
349600150923     C     RIEANA        BegSr
349700150923     C*
349800150923     C* imposto la data e l'utente di immissione
349900150923     C                   If        *in21 = *on
350000150923     C                   Clear                   SDGLRC000
350100150923     C                   Eval      LRCDtImm    = ZDateIso
350200150923     C                   Eval      LRCUteImm   = XScCUt
350300160408     C                   Endif
350400160408R2212X**** deseleziono
350500160418 "   X****               If        LRCNRASL <> 0
350600160418 "   X****               Eval      LRCNRASL = 0
350700160407 "   X****               Eval      LRCSYSL  = 0
350800160418 "   X****               Eval      LRCSogg    = *blanks
350900160408R2212X****               Else
351000160408R2212C*
351100150923     C* seleziono
351200150929     C                   Eval      LRCNRASREG= SavNrAs01
351300150929     C                   Eval      LRCSYS   = SavSys01
351400160122     C                   Eval      LRCSogg    = SavSogg02
351500150923     C                   Eval      LRCNRASL = SavNrAs02
351600150923     C                   Eval      LRCSYSL  = SavSys02
351700160122     C                   Eval      LRCSogg    = SavSogg02
351800160408R2212X****               ENDIF
351900150923     C*
352000150923     C* imposto la data e l'utente di ultima modifica
352100150925     C                   Eval      LRCDtUlMo   = ZDateIso
352200150925     C                   Eval      LRCUteUlMo  = XScCUt
352300150923     C*
352400150923     C                   MoveL     XscSoc        LRCSocieta
352500150923     C*
352600150923     C*
352700150923     C                   EndSr
352800140714     C/EJECT
352900041217      ************************************************************
353000041217      * INIZIALIZZAZIONE LISTA
353100041217      ************************************************************
353200041217     C     INZS2         BEGSR
353300041217      * pulizia SFL
353400041217     C                   SETOFF                                         3536
353500041217     C                   WRITE     FMTC2
353600041217     C                   SETON                                          36
353700041217      *
353800141007     C                   move      *ALL'0'       IN5098           49
353900141007     C                   movea     IN5098        *IN(50)
354000041217      * opzioni
354100041217      * (sono già caricate in S1)
354200041217      *
354300041217      * CARICAMENTO SFL A PAGINE
354400041217     C                   Z-ADD     0             S2NRR
354500041217     C                   Z-ADD     0             WMaxS2
354600041217      *
354700041217     C                   EXSR      SetAnaS2
354800041217      *
354900041217      * Lettura file pilota
355000041217     C                   EXSR      RedAnaS2
355100041217      *
355200041217      * se arrivo dalla parzializzazione devo caricare solo la prima pagina
355300041217     C                   IF        $LastV = 'D1'
355400041217     C                   Z-ADD     SflPagS2      WPagS2
355500041217     C                   ENDIF
355600041217     C                   EXSR      RolS2
355700041217      * imposto il caricamento sfl del numero massimo di righe per pagina
355800041217     C                   Z-ADD     SflPagS2      WPagS2
355900050103      *
356000050103     C                   move      *on           $writeC2
356100041217      *
356200041217     C     XInzS2        ENDSR
356300150925     C************************************************************
356400041217      * CARICAMENTO PAGINA LISTA s2
356500041217      ************************************************************
356600041217     C     ROLS2         BEGSR
356700041217      *
356800041217      *SflNxtChg inizializzato
356900041217      *(ripristino l'attributo di 'riga cambiata')
357000041217     C                   SETOFF                                       37
357100041217     C                   Z-ADD     0             Y
357200041217      *mi posiziono sull'ultima riga caricata
357300041217     C                   Z-ADD     WMaxS2        S2Nrr
357400041217      *
357500041217      * Leggo dal file anagrafico per caricare la lista
357600041217      *
357700041217     C     $EndFile      DOWEQ     *OFF
357800041217     C     Y             ANDLT     WPagS2
357900041217      *
358000041217     C                   exsr      RieS2
358100041217      *
358200041217     C                   ADD       1             S2NRR
358300041217     C                   ADD       1             Y
358400160413R2212C* Salvo il numero relativo di record
358500160413R2212C                   Z-Add     S2NRR         H2Nrr
358600041217      *
358700041217     C                   WRITE     FMTS2
358800041217      *
358900140701      * richiedo al pgm di lettura il rcd successivo
359000150925     C                   MOVE      '1'           RICD26
359100140701      *
359200041217     C                   EXSR      REDANAS2
359300041217      *
359400041217     C                   ENDDO
359500041217      *
359600041217      *memorizzo quante righe ho caricato
359700041220     C                   Z-ADD     S2Nrr         WMaxS2               35
359800041217      *
359900041217      *
360000160413R2212C* Se aggiornamento mi posiziono sulla pagina aggiornata
360100160413  "  C                   If        WOprS2 = *on
360200160413  "  C                   Exsr      PosPagAS2
360300160413  "  C                   Movel     *off          Woprs2
360400160413R2212C                   Else
360500041217      * Posizionamemto al 1° RCD della pagina
360600160414     C                   Exsr      PosPag1S2
360700160413R2212C                   Endif
360800041217      *
360900041217     C     XRolS2        ENDSR
361000041217      ************************************************************
361100041217      * Posizionamemto al 1° RCD della pagina S2
361200041217      ************************************************************
361300041217     C     PosPag1S2     Begsr
361400041217      *
361500041217     C     S2NRR         DIV       SflPagS2      PAGINE            4 0
361600041217     C                   MVR                     RESTO             3 0
361700041217     C     PAGINE        MULT      SflPagS2      C2RCD
361800041217     C     RESTO         IFGT      0
361900041217     C                   ADD       1             C2RCD
362000041217     C                   ELSE
362100041217     C                   SUB       SflPagS2      C2RCD
362200041217     C                   ADD       1             C2RCD
362300041217     C                   ENDIF
362400041217     C     XPosPag1S2    Endsr
362500160413R2212 ************************************************************
362600160413  "   * Posizionamemto al RCD aggiornato della pagina S2
362700160413  "   ************************************************************
362800160413R2212C     PosPagAS2     Begsr
362801170912R266 C* Se opzione di cancellazione / scelta e filtro
362802170912  "  C* correggo il posizionamento.
362803170912  "  C*
362804170911  "  C                   IF        D1FatLeg = '1'   and
362805170911  "  C                             SavDel02 >0      or
362806170911  "  C                             D1FatLeg = '0'   and
362807170911  "  C                             SavOpz02 >0
362808170911  "  C                   Sub       1             WC2Rcd
362809170911  "  C                   IF        Wc2Rcd <= 0
362810170911  "  C                   Z-add     1             Wc2Rcd
362811170911  "  C                   Endif
362812170911R266 C                   Endif
362900160413      *
363000160413     C     WC2RCD        DIV       2             PAGINE            4 0
363100160413     C                   MVR                     RESTO             3 0
363200160413     C     PAGINE        MULT      2             C2RCD
363300160413     C     RESTO         IFGT      0
363400160413     C                   ADD       1             C2RCD
363500160413     C                   ELSE
363600160413     C                   SUB       2             C2RCD
363700160413     C                   ADD       1             C2RCD
363800160413     C                   ENDIF
363900160413R2212C     XPosPagAS2    Endsr
364000041217      ************************************************************
364100041217      * POSIZIONAMENTO RCD ARCHIVIO PILOTA x caricare S2
364200041217      ************************************************************
364300041217     C     SETANAS2      BEGSR
364400150923      *
364500150924     C                   CLEAR                   SDGSD26DS
364600150923     C*
364700150924     C* passo i campi di parzializzazione alla DS del PGM SQL SDGSD26R
364800150923     C* che mi seleziona i record
364900150924     C                   MOVEL     XscSoc        SocietaD26
365000150924     C                   MOVEL     'CG'          CtbD26
365100150924     C                   MOVEL     XcapKcc       KccD26
365200150924     C                   MOVEL     D1Ksc         KscD26
365300160125     C*
365400160125     C                   Eval      TpDtRifD26  = TpDtRifA11
365500150923     C*
365600160125     C                   if        TPDTRIFA11 = 'R'
365700160118     C     *JobRun       Move      D1DtRe2Da     DtRegDaD26
365800160118     C     *JobRun       Move      D1DtRe2A      DtRegAD26
365900160125     C                   else
366000160118     C     *JobRun       Move      D1DtDo2Da     DtDocDaD26
366100160118     C     *JobRun       Move      D1DtDo2A      DtDocAD26
366200160125     C                   endif
366300150923     C*
366400150924     C                   MOVE      1             ORDD26
366500150923     C* richiedo al pgm di lettura il 1° rcd con la conseguente
366600150923     C* inizializzazione delle variabili SQL
366700150924     C                   MOVE      '0'           RICD26
366800150923      *
366900150923     C     XSetAnaS2     ENDSR
367000041217      ************************************************************
367100041217      * LETTURA RCD ARCHIVIO PILOTA x caricare S2
367200041217      ************************************************************
367300041217     C     RedAnaS2      BEGSR
367400150923      *
367500150923     C                   movel     *off          $EndFile
367600150923     C                   movel     *off          $RcdOk
367700150923      *
367800150923     C                   dou       $EndFile = *on or $RcdOk = *on
367900150923      *
368000150923      * LETTURA
368100150923      *
368200150924     C                   CALL      'SDGSD26R'
368300150924     C                   PARM                    SDGSD26DS
368400150923     C                   PARM                    NDMOV00F
368500150923      *
368600150924     C                   MOVE      ENDD26        $EndFile
368700150929      * richiedo al pgm di lettura il rcd successivo
368800150929     C                   MOVE      '1'           RICD26
368900150923      *
369000150923      * SELEZIONE
369100150923     C                   if        $EndFile = *off
369200150923     C                   exsr      RepLegFat
369300150929     C                   exsr      RepReg
369400150929     C                   exsr      RepCausale
369500150923     C                   exsr      RepIva
369600150923     C                   exsr      SelAnaS2
369700150923     C                   endif
369800150923      *
369900150923     C                   enddo
370000150923      *
370100150923     C                   move      $EndFile      *IN38
370200150923      *
370300041217      *
370400041217     C     XRedAnaS2     ENDSR
370500041217      ************************************************************
370600041217      * SELEZIONI SULLA RIGA LETTA s2
370700041217      ************************************************************
370800041217     C     SelAnaS2      Begsr
370900041217      *
371000041217     C                   movel     *on           $RcdOk
371100041217      *
371200150923     C                   Select
371300150923     C*
371400150929     C                   When      $NdReg   = *Off
371500150929     C                   movel     *off          $RcdOk
371600150929     C*
371700150929     C                   When      $Anope   = *Off or
371800150929     C                             ($anope = *On and
371900150929     C                             (Opestp <> 'F' or
372000150929     C                             OpeTpc <> '1' or
372100150929     C                             OpeTpDoci = '8'))
372200150929     C                   movel     *off          $RcdOk
372300150929     C*
372400150923     C                   When      $LegFat  = *Off  and
372500160407R2212X****                         D1DocLeg = XscOn or
372600160407R2212C                             D1FatLeg = '1'   or
372700150923     C                             $LegFat  = *On and
372800160407R2212X****                         D1DocLeg = XscOff
372900160407R2212C                             D1FatLeg = '0'
373000150923     C                   movel     *off          $RcdOk
373100150923     C*
373200160418R2212X****               When      D1Sogg <> *blanks
373300160418 "   X**** Decodifico il conto e reperisco il soggetto
373400160418 "   X****               Move      MOVKcc        $Kcc
373500160418 "   X****               Move      MOVKsc        $Ksc
373600160418 "   X****               exsr      CALLMVC002
373700160418 "   X****               If        ($LegFat  = *Off  and
373800160418 "   X****                          Esito   <> *On   and
373900160418 "   X****                          Sogg002 <> D1Sogg    ) or
374000160418 "   X****                         ($LegFat = *On    and
374100160418 "   X****                          LRCSogg <> D1Sogg    )
374200160418 "   X****               movel     *off          $RcdOk
374300160418R2212X****               Endif
374400150923     C*
374500150923     C                   EndSl
374600140701      *
374700041217     C     XSelAnaS2     Endsr
374800041217      ************************************************************
374900041217      * RIEMPIMENTO RIGA LISTA S2
375000041217      ************************************************************
375100041217     C     RieS2         BEGSR
375200150923      *
375300150923     C                   clear                   FMTS2
375400150923      *
375500150923     C                   eval      H2Sys     = MovSys
375600150925     C                   eval      H2NrAsReg = MovNrAsReg
375700160418     C                   eval      H2DocLeg  = $LegFat
375800150923     C                   eval      S2Ksc     = MovKsc
375900151009     C                   eval      H2Kcc     = MovKcc
376000150924     C* Decodifico il conto e reperisco il soggetto
376100151009     C                   Move      H2Kcc         $Kcc
376200150924     C                   Move      S2Ksc         $Ksc
376300150924     C                   exsr      CALLMVC002
376400150924     C* se nessun errore
376500150924     C                   if        Esito      <> *On
376600151009     C                   MoveL     Descr002      S2KscD
376700151009     C                   MoveL     Sogg002       S2Sogg
376800150924     C                   endif
376900151009     C* Decodifico il soggetto
377000151009     C*
377100151009     C                   If        Sogg002  <> *blanks
377200151009     C                   MoveL     Sogg002       WSogg
377300151009     C                   EXSR      CallDvaSog
377400151009     C*
377500151009     C                   if        DVAErrore  <> '1'
377600151009     C*
377700151009     C                   Eval      %subst(DVASOG:1:DVALENOUT)
377800151009     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
377900151009     C*
378000151009     C                   MoveL     DVSDes        S2SoggD
378100151009     C*
378200151009     C                   endif
378300151009     C                   endif
378400150923      *
378500150925     C     *jobrun       Move      MovDtDoc      S2DtDoc
378600150929     C                   Move      MovNrDoc      S2NrDoc
378700150925     C     *jobrun       Move      MovDtReg      S2DtReg
378800150929     C                   Move      RegNrReg      S2NrReg
378900150923     C*
379000150923      *
379100150923     C                   If        $NdIva = *on
379200150923     C                   eval      S2Importo = $Imponib
379300150923     C                   eval      S2Impiva  = $Iva
379400150923     C                   eval      S2Aliq    = $Aliq
379500150923     C                   eval      S2RifIva  = $RifIva
379600150923     C                   Else
379700150923     C                   eval      S2Importo = MovImporto
379800150923     C                   Endif
379900150923      *
380000150925     C                   eval      S2Causale = MovCausTes
380100150923      *
380200150923      * attributi sfl
380300150923     C                   exsr      AtrS2
380400150923      *
380500041217      *
380600041217     C     XRieS2        Endsr
380700140701     C/EJECT
380800041217      ************************************************************
380900041217      * ATTRIBUTI DELLA RIGA DEL SFL S2
381000041217      ************************************************************
381100041217     C     AtrS2         Begsr
381200150923      *
381300160418     C                   If        H2DocLeg =  *on
381400160418     C                   seton                                        13
381500160418     C                   Else
381600160418     C                   setoff                                       13
381700160418     C                   Endif
381800041217      *
381900041217     C     XAtrS2        Endsr
382000041217      ************************************************************
382100041217      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL s2
382200041217      ************************************************************
382300041217     C     ClcPagS2      Begsr
382400041217      *
382500041217      * Input :
382600041217      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
382700041217      *          cursore
382800041217      * - SFLPAG = numero rcd per pagina sfl
382900041217      * Output :
383000041217      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
383100041217      *
383200041217     C     WSflS2        div       SflPagS2      Pagine            4 0
383300041217     C                   mvr                     Resto             3 0
383400041217     C                   if        Resto > 0
383500041217     C                   add       1             Pagine
383600041217     C                   endif
383700041217     C     Pagine        mult      SflPagS2      WPagS2
383800041217      *
383900041217     C     XClcPagS2     Endsr
384000041217      ************************************************************
384100041217      * CONTROLLO CAMPI I/O RIGA LISTA S2
384200041217      ************************************************************
384300041217     C     RECS2         BEGSR
384400041217      *
384500041217      * reset indicatori DSPATR
384600050103      *   se il campo è rimasto in RI per un errore precedente
384700050103      *   la routine di controllo spegne questo RI poi si dovrà fare
384800050103      *   la write del relativo sfl
384900050427     C                   if        *in(46)<>*off
385000050103     C                   eval      $writec2=*on
385100050103     C                   endif
385200050427     C                   MOVE      *ALL'0'       In4649            4
385300050427     C                   MOVEA     In4649        *IN(46)
385400041217      *
385500041217      * Controllo se Utente Autorizzato alla scelta
385600041217     C     S2OPZ         IFNE      0
385700041217     C                   Z-ADD     1             X
385800041217     C     S2OPZ         LOOKUP    $OP1(X)                                21
385900041217      * mi salvo l'indice dell' opzione
386000041217     C   21              Z-add     X             PosOpz            2 0
386100041217     C     *IN21         IFEQ      *OFF
386200041217     C     $FL1(X)       OREQ      'N'
386300041217     C                   MOVE      *ON           $ESCI
386400050427     C                   SETON                                        469199
386500041217     C                   ENDIF
386600041217     C                   ENDIF
386700160408R2212C*
386800160408 "   C* se opz=15=Mostra legame o opz=4=Annulla selezione
386900160408 "   C* ma fattura non legata, segnalo errore
387000160408 "   C                   if        (S2OPZ      = 15    or
387100160408 "   C                              S2OPZ      = 4        )and
387200160418 "   C                             H2DocLeg   <> *On
387300160408 "   C*
387400160408 "   C                   SETON                                        469199
387500160408 "   C                   endif
387600160408 "   C*
387700160408 "   C* se opz=1 =Scelta
387800160408 "   C* ma fattura legata, segnalo errore
387900160408 "   C                   if        S2OPZ       = 1         and
388000160418 "   C                             H2DocLeg    = *On
388100160408 "   C*
388200160408 "   C                   SETON                                        469199
388300160408R2212C                   endif
388400160707R2322C*
388500160707 "   C* se opz=44=Modifica soggetto
388600160707 "   C* su fattura, segnalo errore
388700160707 "   C                   if        S2OPZ       = 44
388800160707 "   C                   SETON                                        469199
388900160707R2322C                   endif
389000140714      *
389100150929     C                   if        s2opz = 01
389200150929     C                   add       1             SavOpz02
389300160408R2212C                   EndIf
389400160418 "   C*
389500160408 "   C                   if        s2opz = 04
389600160408 "   C                   add       1             SavDel02
389700160408R2212C                   endif
389800150929     C*
389900150929     C                   if        SavOpz02=1
390000160408R2212C                             or
390100160412R2212C                             S2opz = 04
390200150925     C                   Move      H2Sys         SavSys02
390300150925     C                   Move      H2NrAsReg     SavNrAs02
390400151009     C                   Movel     S2Sogg        SavSogg02
390500160418     C                   Movel     H2DOCLEG      SavDocLeg02
390600160414R2212C                   Z-Add     H2Nrr         WC2Rcd
390700150923     C*
390800150923     C                   EndIf
390900160408R2212X****               EndIf
391000160414R2212C*
391100160414 "   C* salvo il numero del record su cui sono posizionato
391200160418 "   C* Salvo il numero relativo di record
391300160418 "   C                   Z-Add     S2NRR         H2Nrr
391400160414R2212C                   Z-Add     H2Nrr         WC2Rcd
391500140714      *
391600150922     c                   if        savopz02 > 1
391700140714     C                   MOVE      *ON           $ESCI
391800140923     C                   SETON                                        466999
391900140714     c                   endif
392000041217      *
392100041217     C     XRecS2        ENDSR
392200041217      ************************************************************
392300041217      * GESTIONE OPZIONI LISTA  S2
392400041217      ************************************************************
392500041217     C     OPZS2         BEGSR
392600041217      *
392700041217     C                   Z-ADD     RIRI          CURR
392800041217     C                   Z-ADD     COCO          CURC
392900160421R2212C*
393000160624R2312X*ex R2212          Eval      $Tasto      = ENTER
393100041217      *
393200041217     C                   SELECT
393300160412R2212C*
393400160412 "   C* se opz=4 =Cancella legame con fattura
393500160412 "   C     S2OPZ         WHENEQ    4
393600160412 "   C     $TASTO        ANDEQ     F06
393700160412 "   C                   Clear                   S2Opz
393800160412R2212C                   EXSR      DelAna02
393900160125     C*
394000160125     C* se opz=15=Mostra legame
394100160125     C     S2OPZ         WHENEQ    15
394200160418     C     H2DocLeg      andEQ     *On
394300160421R2212C*
394400160421R2212C                   Eval      $TastoSav   = $Tasto
394500160126     C*
394600160126     C                   Clear                   FMTD5
394700160418R2212C*
394800160418 "   C* accendo l'indicatore per mostrare tasti funzione
394900160418 "   C* diversi da quelli validi per le bolle doganali
395000160421R2212C*******            Eval      *In12       = *On
395100160125     C*
395200160125     C                   Eval      LRCSocieta  = XScSoc
395300160125     C                   Eval      LRCSysL     = H2Sys
395400160125     C                   Eval      LRCNrAsL    = H2NrAsReg
395500160125     C* cerco i dati della bolla
395600160125     C     K03LRC02      CHAIN(N)  SDGLRC02L                          21
395700160125     C                   if        *In21       = *Off
395800160125     C* salvo i dati del movimento che ho in linea
395900160125     C                   MoveL     NDREG00F      NDREGSAV
396000160125     C                   MoveL     NDMOV00F      NDMOVSAV
396100160125     C*
396200160125     C                   Eval      $Sys        = LRCSys
396300160125     C                   Eval      $NrAsReg    = LRCNrAsReg
396400160125     C                   EXSR      GetNDREG
396500160125     C                   if        $CodOper   <> '9'
396600160125     C     *Jobrun       Move      REGDtReg      D5DtReg
396700160125     C                   Eval      D5NrReg     = REGNrReg
396800160125     C                   Eval      D5SerReg    = REGSerReg
396900160125     C                   endif
397000160125     C*
397100160125     C                   EXSR      GetNDMOV
397200160125     C                   if        $CodOper   <> '9'
397300160126     C                   Eval      D5Kcc       = MOVKcc
397400160126     C                   Eval      D5Ksc       = MOVKsc
397500160125     C     *Jobrun       Move      MOVDtDoc      D5DtDoc
397600160125     C                   Eval      D5NrDoc     = MOVNrDoc
397700160125     C                   Eval      D5SerDoc    = MOVSerDoc
397800160125     C                   endif
397900160125     C*
398000160125     C* ripristino i dati del movimento che ho in linea
398100160125     C                   MoveL     NDREGSAV      NDREG00F
398200160125     C                   MoveL     NDMOVSAV      NDMOV00F
398300160125     C                   endif
398400160125     C*
398500160421R2212C*
398600160421R2212C                   Eval      $TastoSav2  = $Tasto
398700160125     C                   WRITE     FMTZ5
398800160421R2212C                   Eval      $Tasto      = $TastoSav2
398900160421R2212C*
399000160125     C     $tasto        downe     F12
399100160421R2212C     $tasto        andNe     F08
399200160125     C                   EXFMT     FMTD5
399300160125     C                   enddo
399400160125     C                   Eval      $LastV      = 'D5'
399500160421R2212C                   Eval      $Tasto      = $TastoSav
399600041217      *
399700041217      * opzione personalizzata
399800041217     C                   when      $PGO1(PosOpz) <> *blank
399900041217     C                   exsr      Opzs2_Per
400000160708R2321C*
400100160705 "   C* Richiamo prima nota
400200160705 "   C     S2OPZ         WHENEQ    41
400300160705 "   C                   Eval      $Sys        = H2Sys
400400160705 "   C                   Eval      $NrAsReg    = H2NrAsReg
400500160708R2321C                   EXSR      CallPN
400600041217      *
400700041217      * se altre opzioni
400800041217     C                   OTHER
400900041217      *
401000041217     C                   ENDSL
401100050127      *
401200050127     C                   move      *on           $writeC2
401300041217      *
401400041217     C     XOpzS2        ENDSR
401500041217      ************************************************************
401600041217      * opzione personalizzata
401700041217      ************************************************************
401800041217     C     Opzs2_Per     Begsr
401900041217      *
402000150812     C                   movel     SDGSD25DS     $SaveDS
402100041217      *
402200150812     C                   RESET                   SDGSD25DS
402300150910     C                   MOVE      xscsoc        SocietaD25
402400150910     C                   MOVEL     S2OPZ         OPZD25
402500150812      * passare nei campi appositi della DS SDGSD25DS il valore
402600041217      * della chiave della VL univoca
402700150910     C                   movel     xscsoc        SocietaD25
402800041217      *
402900150812     C                   MOVEL     SDGSD25DS     KPJBU
403000041217     C                   CALL      $PGO1(PosOpz)                        22
403100041217     C                   PARM                    KPJBA
403200140701     C***                PARM                    NOMPGM
403300041217      *
403400150812     C                   MOVEL     KPJBU         SDGSD25DS
403500041217      * ritorno da PGM
403600041217     C                   exsr      Opzs2_RtnPer
403700041217      * ripristino la DS di ingresso
403800150812     C                   MOVEL     $SaveDS       SDGSD25DS
403900041217      *
404000041217     C     XOpzS2_Per    Endsr
404100041217      ************************************************************
404200041217      * ritorno da opzione personalizzata  S2
404300041217      ************************************************************
404400041217     C     Opzs2_RtnPer  Begsr
404500041217      *----------------------------------------------------------------
404600041217      * modo di ritorno
404700041217     C                   select
404800041217      * 1 = F3
404900150910     C                   when      RetD25 = '1'
405000041217     C                   move      *on           $Esci
405100041217     C                   move      *on           $Fine
405200041217      * 2 = F12
405300041217      * mi fermo sulla riga del sfl in oggetto senza fare nulla,
405400041217      * se non sono stati fatti aggiornamenti
405500041217      * (devo ricaricare il sfl se sono stati fatti aggiornamenti)
405600150910     C                   when      RetD25 = '2' and OprD25 <> '1'
405700041217     C                   move      *on           $Esci
405800041217     C                   endsl
405900041217      *----------------------------------------------------------------
406000041217      * operazione eseguite dal pgm chiamato
406100041217     C                   select
406200041217      * 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
406300150910     C                   when      OprD25 = '1'
406400150910     C                   move      OprD25        WOpr
406500041217      *
406600041217      * se l'opzione con cui è stato chiamato non è la modifica
406700041217      * memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
406800150910     C                   if        OpzD25 <> '02'
406900041217     C                   move      *on           WInzS2
407000041217      * se l'opzione con cui è stato chiamato è la modifica
407100041217      * aggiorno subito la riga di sfl e non richiedo il ripristino sfl
407200041217     C                   else
407300041217     C                   exsr      AggS2
407400041217     C                   endif
407500041217     C                   endsl
407600041217      *----------------------------------------------------------------
407700041217      * funzione non eseguibile per errore :
407800041217     C                   select
407900041217      * 1 = funzione richiamata chiusa in errore
408000041217      *   PRO0007-(F1) La funzione richiamata si è chiusa in errore
408100150910     C                   when      ErrD25 = '1'
408200041217     C                   move      *on           $Esci
408300041217     C                   eval      *in93 = *on
408400041217     C                   eval      *in99 = *on
408500041217      * 2 = funzione richiamata senza la dovuta autorizzazione
408600150925     C                   when      ErrD25 = '2'
408700041217     C                   move      *on           $Esci
408800041217     C                   eval      *in94 = *on
408900041217     C                   eval      *in99 = *on
409000041217     C                   endsl
409100041217      *
409200041217     C     XOpzs2_RtnPer Endsr
409300041217      ************************************************************
409400041217      * AGGIORNAMENTO RIGA LISTA
409500041217      ************************************************************
409600041217     C     AGGS2         BEGSR
409700041217      *
409800041217      * riempio i campi del sfl che sono stati ripassati dal pgm
409900041217      * di manutenzione dopo una opzione 02
410000041217      *
410100041217     C     XAggS2        ENDSR
410200041217*******S2 FINE****************************************************
410300041217      ************************************************************
410400041213      * GESTIONE VALORI ASSUNTI
410500041213      ************************************************************
410600041213     C     GESD2         BEGSR
410700041213      *
410800041213     C     $INZD2        IFEQ      *ON
410900041213     C                   EXSR      INZD2
411000041213     C                   MOVE      *OFF          $INZD2
411100041213     C                   ENDIF
411200041213      *
411300041213     C     $LASTV        IFNE      'D2'
411400041213     C                   EXSR      GESPIE
411500041213     C     $LASTV        IFNE      *HIVAL
411600041213     C                   MOVE      $LASTV        $LASTG
411700041213     C                   ENDIF
411800041213     C                   MOVE      'D2'          $LASTV
411900041213     C                   ENDIF
412000041213      *
412100041213      * se esistono errori sulla videata
412200041213      * emetto la write del formato a indicatori spenti per vedere
412300041213      * le eventuali decodifiche
412400041213     C     *IN99         IFEQ      *ON
412500041213     C                   MOVEA     *IN           WIN
412600041213     C                   MOVE      *ALL'0'       IN5098           49
412700041213     C                   MOVEA     IN5098        *IN(50)
412800041213     C                   WRITE     FMTD2
412900041213     C                   MOVEA     WIN           *IN
413000041213     C                   ENDIF
413100041213      *
413200041213     C                   EXFMT     FMTD2
413300041213      *
413400041213      * azzero la posizione del cursore
413500041213     C                   Z-ADD     0             H1RIGA
413600041213     C                   Z-ADD     0             H1COLO
413700041213      *
413800041213      * CONTROLLO ABILITAZIONE TASTI
413900041213      * INPUT
414000041213      * $FC3 = Abilitazioni videata D2
414100041213      * OUTPUT
414200041213      * *IN97 + *IN99 Errori
414300041213     C                   MOVEA     $FC3          $FC
414400041213     C                   EXSR      CTRKEY
414500041213      *
414600041213     C     *IN99         IFEQ      *OFF
414700041213      *
414800041213     C                   SELECT
414900041213      * F3=Fine
415000041213     C     $TASTO        WHENEQ    F03
415100041213     C                   EXSR      F03D2
415200041213      * F12=Ritorno
415300041213     C     $TASTO        WHENEQ    F12
415400041213     C                   EXSR      F12D2
415500041213      * F24=Altri tasti
415600041213     C     $TASTO        WHENEQ    F24
415700041213     C                   EXSR      F24D2
415800041213      *
415900041213     C                   OTHER
416000041213     C                   EXSR      CTRD2
416100041213     C     *IN99         IFEQ      *OFF
416200041213     C                   MOVE      $LASTG        $GEST
416300041213     C                   SELECT
416400041213     C     $LASTG        WHENEQ    'S1'
416500041213      * se è stato modificato l'ordinamento di default, ricarico la lista
416600041213     C     $ORD          IFNE      D2ORD
416700041213     C                   MOVE      *ON           $INZS1
416800041217     C                   MOVE      *ON           $INZS2
416900041213     C                   MOVE      *BLANK        W1POS
417000041213     C                   ENDIF
417100041213     C     $LASTG        WHENEQ    'D1'
417200041213      * per ora niente
417300041213     C                   ENDSL
417400041213      * definisco i nuovi valori di default
417500041213     C                   Z-ADD     D2LOP         $LOP
417600041213     C                   Z-ADD     D2ORD         $ORD
417700051115     c                   move      D2val         $val
417800041213    > *  elenco altri dati default specifici dell'anagrafica
417900041213      *
418000041213      *
418100041213     C                   ENDIF
418200041213      *
418300041213     C                   ENDSL
418400041213      *
418500041213     C                   ENDIF
418600050104      *
418700050104     C                   if        $Gest = 'S1' or $Gest = 'S2'
418800050104     C                   eval      $writeC1 = *on
418900050104     C                   eval      $writeC2 = *on
419000050104     C                   endif
419100041213      *
419200041213     C     XGesD2        ENDSR
419300041213      ************************************************************
419400041213      * INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
419500041213      ************************************************************
419600041213     C     INZD2         BEGSR
419700041213      *
419800041213     C                   CLEAR                   FMTD2
419900041213      *
420000051115     c                   eval      d2on=xscon
420100051115     c                   eval      d2off=xscoff
420200051115     C                   EVAL      D2vala = Normale
420300051115     c                   eval      d2val=$val
420400041213      * se sono entrato con chiave univoca
420500041213      * impedisco il salvataggio valori assunti
420600041213     C     $Unique       IFEQ      *ON
420700051115     C                   move      xscoff        D2val
420800041213     C                   ENDIF
420900041213     C                   MOVEL     $LOP          D2LOP
421000041213     C                   MOVE      $ORD          D2ORD
421100041213    > *  elenco altri dati default specifici dell'anagrafica
421200041213      *
421300041213     C     XInzD2        ENDSR
421400041213      ************************************************************
421500041213      * GESTIONE F03 VIDEO D2
421600041213      ************************************************************
421700041213     C     F03D2         BEGSR
421800041213      *
421900041213     C                   MOVE      *ON           $FINE
422000041213      * fine programma
422100041213     C                   EXSR      ENDPGM
422200041213      *
422300041213     C     XF03D2        ENDSR
422400041213      ************************************************************
422500041213      * GESTIONE F12 VIDEO D2
422600041213      ************************************************************
422700041213     C     F12D2         BEGSR
422800041213      *
422900041213     C                   MOVE      $LASTG        $GEST
423000041213      *
423100041213     C     XF12D2        ENDSR
423200041213      ************************************************************
423300041213      * GESTIONE F24 VIDEO D2
423400041213      ************************************************************
423500041213     C     F24D2         BEGSR
423600041213      *
423700041213     C                   MOVEA     $FX3          $FX
423800041213     C                   MOVEA     $FC3          $FC
423900041213     C                   MOVEA     $FR3          $FR
424000041213     C                   Z-ADD     $ULKD2        $ULK
424100041213     C                   Z-ADD     $D2L01        $L01
424200041213     C                   Z-ADD     $D2L02        $L02
424300041213     C                   EXSR      RIEKEY
424400041213     C                   Z-ADD     $ULK          $ULKD2            3 0
424500041213     C                   MOVEA     $FC           $FC3
424600041213     C                   MOVEL     $KEY1         Z3KE1
424700150910     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
424800041213     C                   WRITE     FMTZ3
424900041213      *
425000041213     C     XF24D2        ENDSR
425100041213      ************************************************************
425200041213      * CONTROLLO VIDEATA VALORI ASSUNTI
425300041213      ************************************************************
425400041213     C     CTRD2         BEGSR
425500041213      *
425600041213     C                   SETOFF                                       99
425700041213      *
425800041213      * OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
425900041213     C     D2LOP         IFEQ      0
426000041213     C                   SETON                                          5099
426100041213     C                   ENDIF
426200041213      *
426300041213      * OBBLIGATORIO VALORE PER ORDINAMENTO
426400041213     C     D2ORD         IFEQ      0
426500041213     C                   SETON                                          5199
426600041213     C                   ENDIF
426700051115      * salvataggio dati
426800051115     C     D2val         IFne      xscon
426900051115     C     d2val         andne     xscoff
427000051115     C                   SETON                                          5299
427100051115     C                   ENDIF
427200041213    > *  elenco controlli altri dati default specifici dell'anagrafica
427300041213      *
427400041213     C     XCtrD2        ENDSR
427500041213      ************************************************************
427600041213      * CONTROLLO ABILITAZIONE TASTI
427700041213      * INPUT
427800041213      * $TASTO =Tasto funzione premuto
427900041213      * $FC = Tasti abilitati
428000041213      * OUTPUT
428100041213      * IN99 = ERrore
428200041213      ************************************************************
428300041213     C     CTRKEY        BEGSR
428400041213      *
428500041213     C                   SETOFF                                       99
428600041213      *
428700041213     C                   SELECT
428800041213     C     $TASTO        WHENEQ    F01
428900041213     C                   z-add     01            posfun
429000041213     C     $FC(1)        IFEQ      '0'
429100041213     C                   SETON                                        9997
429200041213     C                   ENDIF
429300041213     C     $TASTO        WHENEQ    F02
429400041213     C                   z-add     02            posfun
429500041213     C     $FC(2)        IFEQ      '0'
429600041213     C                   SETON                                        9997
429700041213     C                   ENDIF
429800041213     C     $TASTO        WHENEQ    F03
429900041213     C                   z-add     03            posfun
430000041213     C     $FC(3)        IFEQ      '0'
430100041213     C                   SETON                                        9997
430200041213     C                   ENDIF
430300041213     C     $TASTO        WHENEQ    F04
430400041213     C                   z-add     04            posfun
430500041213     C     $FC(4)        IFEQ      '0'
430600041213     C                   SETON                                        9997
430700041213     C                   ENDIF
430800041213     C     $TASTO        WHENEQ    F05
430900041213     C                   z-add     05            posfun
431000041213     C     $FC(5)        IFEQ      '0'
431100041213     C                   SETON                                        9997
431200041213     C                   ENDIF
431300041213     C     $TASTO        WHENEQ    F06
431400041213     C                   z-add     06            posfun
431500041213     C     $FC(6)        IFEQ      '0'
431600041213     C                   SETON                                        9997
431700041213     C                   ENDIF
431800041213     C     $TASTO        WHENEQ    F07
431900041213     C                   z-add     07            posfun
432000041213     C     $FC(7)        IFEQ      '0'
432100041213     C                   SETON                                        9997
432200041213     C                   ENDIF
432300041213     C     $TASTO        WHENEQ    F08
432400041213     C                   z-add     08            posfun
432500041213     C     $FC(8)        IFEQ      '0'
432600041213     C                   SETON                                        9997
432700041213     C                   ENDIF
432800041213     C     $TASTO        WHENEQ    F09
432900041213     C                   z-add     09            posfun
433000041213     C     $FC(09)       IFEQ      '0'
433100041213     C                   SETON                                        9997
433200041213     C                   ENDIF
433300041213     C     $TASTO        WHENEQ    F10
433400041213     C                   z-add     10            posfun
433500041213     C     $FC(10)       IFEQ      '0'
433600041213     C                   SETON                                        9997
433700041213     C                   ENDIF
433800041213     C     $TASTO        WHENEQ    F11
433900041213     C                   z-add     11            posfun
434000041213     C     $FC(11)       IFEQ      '0'
434100041213     C                   SETON                                        9997
434200041213     C                   ENDIF
434300041213     C     $TASTO        WHENEQ    F12
434400041213     C                   z-add     12            posfun
434500041213     C     $FC(12)       IFEQ      '0'
434600041213     C                   SETON                                        9997
434700041213     C                   ENDIF
434800041213     C     $TASTO        WHENEQ    F13
434900041213     C                   z-add     13            posfun
435000041213     C     $FC(13)       IFEQ      '0'
435100041213     C                   SETON                                        9997
435200041213     C                   ENDIF
435300041213     C     $TASTO        WHENEQ    F14
435400041213     C                   z-add     14            posfun
435500041213     C     $FC(14)       IFEQ      '0'
435600041213     C                   SETON                                        9997
435700041213     C                   ENDIF
435800041213     C     $TASTO        WHENEQ    F15
435900041213     C                   z-add     15            posfun
436000041213     C     $FC(15)       IFEQ      '0'
436100041213     C                   SETON                                        9997
436200041213     C                   ENDIF
436300041213     C     $TASTO        WHENEQ    F16
436400041213     C                   z-add     16            posfun
436500041213     C     $FC(16)       IFEQ      '0'
436600041213     C                   SETON                                        9997
436700041213     C                   ENDIF
436800041213     C     $TASTO        WHENEQ    F17
436900041213     C                   z-add     17            posfun
437000041213     C     $FC(17)       IFEQ      '0'
437100041213     C                   SETON                                        9997
437200041213     C                   ENDIF
437300041213     C     $TASTO        WHENEQ    F18
437400041213     C                   z-add     18            posfun
437500041213     C     $FC(18)       IFEQ      '0'
437600041213     C                   SETON                                        9997
437700041213     C                   ENDIF
437800041213     C     $TASTO        WHENEQ    F19
437900041213     C                   z-add     19            posfun
438000041213     C     $FC(19)       IFEQ      '0'
438100041213     C                   SETON                                        9997
438200041213     C                   ENDIF
438300041213     C     $TASTO        WHENEQ    F20
438400041213     C                   z-add     20            posfun
438500041213     C     $FC(20)       IFEQ      '0'
438600041213     C                   SETON                                        9997
438700041213     C                   ENDIF
438800041213     C     $TASTO        WHENEQ    F21
438900041213     C                   z-add     21            posfun
439000041213     C     $FC(21)       IFEQ      '0'
439100041213     C                   SETON                                        9997
439200041213     C                   ENDIF
439300041213     C     $TASTO        WHENEQ    F22
439400041213     C                   z-add     22            posfun
439500041213     C     $FC(22)       IFEQ      '0'
439600041213     C                   SETON                                        9997
439700041213     C                   ENDIF
439800041213     C     $TASTO        WHENEQ    F23
439900041213     C                   z-add     23            posfun
440000041213     C     $FC(23)       IFEQ      '0'
440100041213     C                   SETON                                        9997
440200041213     C                   ENDIF
440300041213     C     $TASTO        WHENEQ    F24
440400041213     C                   z-add     24            posfun
440500041213     C     $FC(24)       IFEQ      '0'
440600041213     C                   SETON                                        9997
440700041213     C                   ENDIF
440800041213     C                   ENDSL
440900041213      *
441000041213     C     XCtrKey       ENDSR
441100041213      ************************************************************
441200041213      * AGGIORNAMENTO VALORI ASSUNTI
441300041213      ************************************************************
441400041213     C     AGGMPE        BEGSR
441500041213      *
441600041213      * preparo i parametri di chiamata al driver
441700041213     C                   CLEAR                   XMPEDS
441800041213     C                   EVAL      TpChiamata = '03'
441900041213     C                   EVAL      NChiavi = 5
442000041213     C                   EVAL      VLogica = 2
442100041213      *
442200041213      * preparo le chiavi per la chiamata al driver
442300041213     C                   CLEAR                   XMPEDS1
442400041213     C                   MOVEL     XSCSOC        DSMPESOC
442500041213     C                   MOVEL     DSPGM         DSMPEPGM
442600041213     C                   MOVEL     KNMUS         DSMPEPRF
442700041213      *
442800041213      * Si memorizzano i dati di default a seconda delle modalità con
442900041213      * cui il pgm è usato
443000041213      * MPETIP = 'PX'  pgm richiamato come lista (CMD4)
443100041213      * MPETIP = 'PV'  pgm richiamato da menù
443200041213      * MPETIP = 'PB'  x lancio Batch
443300041213      *
443400041213     C                   SELECT
443500150925     C     OPZD25        WHENEQ    '01'
443600041213     C                   MOVEL     'PX'          DSMPETIP
443700041213     C                   OTHER
443800041213     C                   MOVEL     'PV'          DSMPETIP
443900041213     C                   ENDSL
444000041213      * dati
444100051122     c                   if        $val = xscon
444200160407R2212X****               eval      $Docleg  = D1DocLeg
444300160407R2212C                   eval      $BolLeg  = D1BolLeg
444400160407R2212C                   eval      $FatLeg  = D1FatLeg
444500150910     c                   eval      $Sogg    = D1Sogg
444600150910     c                   eval      $KscDog  = D1KscDog
444700150910     c                   eval      $Dtregda = D1DtRegDa
444800150924     c                   eval      $Dtrega  = D1DtRega
444900150910     c                   eval      $Dtdocda = D1DtDocDa
445000150924     c                   eval      $Dtdoca  = D1DtDocA
445100150923     c                   eval      $KscFatt = D1Ksc
445200150910     c                   eval      $Dtre2da = D1DtRe2Da
445300150924     c                   eval      $Dtre2a  = D1DtRe2a
445400150910     c                   eval      $Dtdo2da = D1DtDo2Da
445500150924     c                   eval      $Dtdo2a  = D1DtDo2A
445600051115     c                   endif
445700041213     C                   MOVEL     DSMPE         DSMPEPAR
445800041213      *
445900041213     C                   CALLB     'XMPE'
446000041213     C                   PARM                    XMPEDS
446100051115     C                   PARM                    XMPEDS1
446200041213      *
446300041213     C     XAggMpe       ENDSR
446400041213      ************************************************************
446500041213      * OPERAZIONI INIZIALI
446600041213      ************************************************************
446700041213     C     *INZSR        BEGSR
446800051115     C     *DMY          Move      Udate         ZDateISO
446900051116     C     *JobRun       Move      ZDateISO      ZDate             6 0
447000041213      *
447100041213      * Reperimento parametri
447200041213     C     *ENTRY        PLIST
447300041213     C                   PARM                    KPJBA
447400041213     C                   PARM                    DSAUT
447500041213      *
447600041213      * testo se sono stati passati i dati di autorizzazione
447700041213     C     PARMS         IFLT      2
447800041213     C                   MOVE      *BLANK        AUTDS
447900041213     C                   ELSE
448000041213     C                   MOVEL     DSAUT         AUTDS
448100041213     C                   ENDIF
448200041213      *
448300041213      * Reperimento dati società
448400041213     C                   MOVEL     'SOC001'      TIPXSC
448500041213     C                   MOVEL     *BLANK        SOCXSC
448600041213     C                   EXSR      REPSOC
448700041213      *
448800041213      * Controlli dati società
448900041213     C     RTNXSC        IFNE      '1'
449000041213     C                   MOVEL     XSOCDS        SOC001
449100041213     C                   ELSE
449200041213     C                   MOVE      *ON           $FINE
449300150910     C                   MOVE      '1'           ErrD25
449400041213     C                   EXSR      ENDPGM
449500041213     C                   ENDIF
449600160122     C*
449700170814R258 X****               EXSR      RepSDBLACK2
449800170814R258 C                   Move      'R'           TpDtRifA11        1
449900041213      *
450000041213      * reperisco le dimensioni delle variabili che conterranno i tasti
450100041213     C                   EVAL      $D1L01 = %Size(Z1Ke1)
450200041213     C                   EVAL      $D1L02 = %Size(Z1Ke2)
450300041213     C                   EVAL      $S1L01 = %Size(Z2Ke1)
450400140701     C***                EVAL      $S1L02 = %Size(Z2Ke2)
450500041213     C                   EVAL      $C1Op1 = %Size(C1Opz1)
450600041213     C                   EVAL      $C1Op2 = %Size(C1Opz2)
450700041213     C                   EVAL      $D2L01 = %Size(Z3Ke1)
450800041213     C                   EVAL      $D2L02 = 0
450900041213    > * aggiungere altre variabili nel caso di videate indipendenti
451000041213    > * ulteriori a quelle std
451100041213      *
451200041213      * Reperimento opzioni
451300041213     C                   EXSR      REPOPZ
451400041213      *
451500041213      * Reperimento tasti di funzione
451600041213     C                   EXSR      REPKEY
451700041213      * Chiamata a routine per il reperimento delle opzioni
451800041213      * personalizzate
451900041213    > * se si vogliono abilitare i tasti personalizzati in ogni videata
452000041213    > * ripeterla passando le schiere dei tasti funzione
452100041213     C                   CAllb     'X46USREXT'
452200041213     C                   PARM                    $OP1
452300041213     C                   PARM                    $TX1
452400041213     C                   PARM                    $FL1
452500041213     C                   PARM                    $PGO1
452600041213     C                   PARM      $dimOp        NumOpz
452700041213     C                   PARM                    $FX2
452800041213     C                   PARM                    $FC2
452900041213     C                   PARM                    $PGF2
453000041213     C                   PARM                    DsPgm
453100041213     C                   PARM                    xsccut
453200041213     C                   PARM                    xscgrp
453300041213      *
453400041213     C                   Z-add     1             OPZLEN
453500041213     C     *blank        Lookup    $TX1(opzlen)                           21
453600041213     C                   if        *in21 = *on
453700041213     C                   sub       1             OPZLEN
453800041213     C                   else
453900041213     C                   Z-add     $DimOp        OPZLEN
454000041213     C                   endif
454100050428      *
454200150911     C* reperisco DATFMT del job
454300150911     C                   CallB     'XDATFMT'
454400150911     C                   Parm                    $DatFmt           4
454500150911     C*
454600150911     C                   callb     'XSTRCMT'
454700150911     C*
454800041213      *
454900041213     C     XInzSr        ENDSR
455000041213      ************************************************************
455100041213      * INIZIALIZZAZIONE VARIABILI
455200041213      ************************************************************
455300041213     C     INZVAR        BEGSR
455400041213      *
455500041213      * Dati da PGM chiamante
455600150812     C                   MOVEL     KPJBU         SDGSD25DS
455700041213      *
455800041213      * Se società diversa da quella impostata nella *inzsr, rieseguo
455900041213      * reperimento dati società
456000150910     C                   IF        SocietaD25 <> xscsoc  and
456100150910     C                             OpzD25 <>*blank
456200041213     C                   MOVEL     'SOC001'      TIPXSC
456300150910     C                   MOVEL     SocietaD25    SOCXSC
456400041213     C                   EXSR      REPSOC
456500041213     C     RTNXSC        IFNE      '1'
456600041213     C                   MOVEL     XSOCDS        SOC001
456700041213     C                   ELSE
456800041213     C                   MOVE      *ON           $FINE
456900150910     C                   MOVE      '1'           ErrD25
457000041213     C                   EXSR      ENDPGM
457100041213     C                   ENDIF
457200041213     C                   ENDIF
457300051115     c*
457400051115     c* controllo che nessun utente ci stia lavorando
457500140701     C**                 CALL      'X21ELABC'
457600140701     C**                 parm      '1'           Modalita          1
457700140701     C**                 parm                    xscsoc
457800140701     C**                 parm                    unita             8
457900140701     C**                 parm                    keygest          15
458000140701     C**                 parm      'RICONTES'    tipX21            8
458100140701     C**                 parm      '0'           commit            1
458200140701     C**                 parm      '0'           erroreX21         1
458300140701     C**                 parm                    kpjba
458400140701     c**
458500140701     c**                 if        errorex21 = *on
458600140701     c**                 eval      errI02 = *on
458700140701     c**                 exsr      endpgm
458800140701     c**                 endif
458900041213      *
459000041213      * Reperimento autorizzazioni
459100140701     C**   AUTDS         IFEQ      *BLANK
459200140701     C**                 EXSR      REPAUT
459300140701     C**                 ELSE
459400140701     C**                 MOVEL     XCADDS        CONTESDS
459500140701     C**                 ENDIF
459600050103      *
459700041213      * Reperimento valori default utente
459800041213     C                   EXSR      REPDFT
459900041213      *
460000041213      * Definizione delle opzioni utilizzabili a pgm
460100041213     C                   EXSR      DEFOPZ
460200041213      *
460300041213      * Variabili per gestione videate
460400041213     C                   MOVE      *OFF          $FINE
460500041213     C                   MOVE      *OFF          $INZD1
460600041213     C                   MOVE      *OFF          $INZS1
460700041217     C                   MOVE      *OFF          $INZS2
460800041213     C                   MOVE      *OFF          $INZD2
460900041213     C                   MOVE      *OFF          $EndFile
461000041213     C                   MOVE      *OFF          $ESCI
461100041213     C                   MOVE      *OFF          $RCDOK
461200041213     C                   MOVE      *HIVAL        $LASTV
461300041213     C                   Z-ADD     0             $ULKD1            3 0
461400041213     C                   Z-ADD     0             $ULKS1            3 0
461500041213     C                   Z-ADD     0             $ULKD2            3 0
461600041213      * azzero la posizione del cursore
461700041213     C                   Z-ADD     0             H1RIGA
461800041213     C                   Z-ADD     0             H1COLO
461900041213      *
462000041213     C                   MOVE      *ON           $INZD1
462100041213     C                   MOVE      *ON           $INZS1
462200041217     C                   MOVE      *ON           $INZS2
462300041213      *
462400160701R2313X***** se pgm richiamato con chiave univoca impostata,
462500160624 "   X***** forzo la emissione della lista e imposto l'ordinamento
462600160624 "   X**** OpzD25        IFNE      *BLANK
462700160624 "   X****               MOVE      *ON           $Unique
462800160624 "   X****               Z-ADD     2             $LOP
462900160624 "   X****               ELSE
463000160624 "   X****               MOVE      *OFF          $Unique
463100160624 "   X****               Z-ADD     1             $LOP
463200160701R2313X****               ENDIF
463300160701R2313C*
463400160624 "   C                   MOVE      *OFF          $Unique
463500160701R2313C                   Z-ADD     1             $LOP
463600041213      *
463700041213      * Variabili appoggio
463800041220     C                   Z-ADD     SflPagS1      WPagS1
463900041220     C                   Z-ADD     SflPagS2      WPagS2
464000041213     C                   MOVEL     *OFF          WOPR
464100160413R2212C                   MOVEL     *OFF          WOPRS1
464200160413R2212C                   MOVEL     *OFF          WOPRS2
464300041213     C                   CLEAR                   W1POS
464400041213      *
464500041213      * definisco qual'è la prima videata
464600041213      * (se la prima videata risulta la lista, devo comunque caricare
464700041213      *  la videata di parzializzazione e gestire la definizione
464800041213      *  dell'ordinamento, perchè, dai default oppure se sono stati
464900041213      *  passati dei dati, può variare il caricamento)
465000041213     C                   SELECT
465100041213     C     $LOP          WHENEQ    1
465200041213     C                   MOVE      'D1'          $GEST
465300041213     C     $LOP          WHENEQ    2
465400041213     C                   MOVE      'S1'          $GEST
465500041213     C                   EXSR      INZD1
465600041213     C                   MOVE      *OFF          $INZD1
465700041213      *  in parzializzazione ci sono campi obbligatori
465800041213      *  quindi se ci sono errori emetto D1
465900041213     C                   exsr      CtrD1
466000041213     C                   if        *in99 = *on
466100041213     C                   move      'D1'          $Gest
466200041213     C                   else
466300041213     C                   EXSR      DEFORD
466400041213     C                   endif
466500041213     C                   ENDSL
466600041213      *
466700050105      * Campi testata
466800041213     C                   CLEAR                   FMTT1
466900041213     C                   MOVEL     KNSIF         NOMSIF
467000041213     C                   MOVEL     XSCDSI        NOMDIT
467100140701     C                   MoveL     KCDAZ         NOMAZN
467200140701     C***                MOVEL     DSPGM         NOMPGM
467300050105      *
467400140701     C* recupero il sottotitolo (se c'è)
467500041213     C                   MOVEL     *BLANK        T1TIT
467600041213     C     SOTTIT        IFNE      *BLANK
467700041213     C                   MOVEL     SOTTIT        IDMSG
467800041213     C                   CALLB     'XRTVM'
467900041213     C                   PARM                    IDMSG            27
468000041213     C                   PARM                    TXTMSG          100
468100041213     C                   PARM                    ERRMSG            1
468200041213     C                   PARM      *ON           CTRMSG            1
468300041213     C                   PARM      30            LENMSG            3 0
468400041213     C     ERRMSG        IFNE      *ON
468500041213     C                   MOVEL     TXTMSG        T1TIT
468600041213     C                   ELSE
468700041213     C                   MOVEL     *ALL'?'       T1TIT
468800041213     C                   ENDIF
468900041213     C                   ENDIF
469000041222      *
469100041223     C                   move      *on           $writeC1          1
469200041223     C                   move      *on           $writeC2          1
469300041213      *
469400041213     C     XInzVar       ENDSR
469500041213      ************************************************************
469600041213      * REPERIMENTO DATI SOCIETA'
469700041213      ************************************************************
469800041213     C     REPSOC        BEGSR
469900041213      *
470000041213     C                   CALLB     'XSOC'
470100041213     C                   PARM                    TIPXSC            6
470200041213     C                   PARM                    SOCXSC            3
470300041213     C                   PARM                    CDSXSC            9 0
470400140701     C                   PARM                    MODXSC            3
470500041213     C                   PARM      *BLANKS       RTNXSC            1
470600041213     C                   PARM                    XSOCDS
470700041213     C                   PARM                    KPJBA
470800041213      *
470900041213     C     XRepSoc       ENDSR
471000170814R258 X************************************************************
471100170814 "   X* Reperimento parametri di setup per Black List
471200170814 "   X************************************************************
471300170814 "   X**** RepSDBLACK2   BEGSR
471400170814 "   X****
471500170814 "   X****               Clear                   SDGPA11DS
471600170814 "   X****
471700170814 "   X****               CALLB     'XPAR'
471800170814 "   X****               Parm                    XScSoc
471900170814 "   X****               Parm      'SDBLACK2'    XPaPar            8
472000170814 "   X****               Parm                    XPaOut         2000
472100170814 "   X****               Parm      *OFF          XPaErr            1
472200170814 "   X****               Parm      ' '           XPaRic            1
472300170814 "   X****
472400170814 "   X****               if        XPaErr      = *On
472500170814 "   X****               Eval      ERRD25      = '1'
472600170814 "   X****               EXSR      ENDPGM
472700170814 "   X****
472800170814 "   X****               else
472900170814 "   X****               MoveL     XPaOut        SDGPA11DS
473000170814 "   X****
473100170814 "   X****               if        AttBLA11    <> *On
473200170814 "   X****               Eval      ERRD25      = '1'
473300170814 "   X****               EXSR      ENDPGM
473400170814 "   X****               endif
473500170814 "   X****
473600170814 "   X****               endif
473700170814 "   X****
473800170814 "   X****               ENDSR
473900170814R258 X**** /EJECT
474000041213      ************************************************************
474100041213      * REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
474200041213      ************************************************************
474300041213     C     REPDFT        BEGSR
474400041213      *
474500041213      * preparo i parametri di chiamata al driver
474600041213     C                   CLEAR                   XMPEDS
474700041213     C                   EVAL      TpChiamata = '01'
474800041213     C                   EVAL      NChiavi = 5
474900041213     C                   EVAL      VLogica = 2
475000041213      *
475100041213      * preparo le chiavi per la chiamata al driver
475200041213     C                   CLEAR                   XMPEDS1
475300041213     C                   MOVEL     XSCSOC        DSMPESOC
475400041213     C                   MOVEL     DSPGM         DSMPEPGM
475500041213     C                   MOVEL     KNMUS         DSMPEPRF
475600041213      * Si reperiscono i dati di default a seconda delle modalità con
475700041213      * cui il pgm è usato
475800041213      * MPETIP = 'PX'  pgm richiamato come lista (CMD4)
475900041213      * MPETIP = 'PV'  pgm richiamato da menù
476000041213      * MPETIP = 'PB'  x lancio Batch
476100041213     C                   SELECT
476200150910     C     OPZD25        WHENEQ    '01'
476300041213     C                   MOVEL     'PX'          DSMPETIP
476400041213     C                   OTHER
476500041213     C                   MOVEL     'PV'          DSMPETIP
476600041213     C                   ENDSL
476700041213      *
476800041213     C                   CALLB     'XMPE'
476900041213     C                   PARM                    XMPEDS
477000041213     C                   PARM                    XMPEDS1
477100041213      *
477200041213     C     Risultato     IFEQ      '0'
477300041213     C                   MOVEL     DSMPEPAR      DSMPE
477400041213      * nel caso sia la prima volta che l'utente apre questa anagrafica
477500041213      * i suoi valori di default vengono decisi a pgm
477600041213     C                   ELSE
477700041213     C                   Z-ADD     1             $LOP
477800041213     C                   Z-ADD     1             $ORD
477900051115     c                   move      xscoff        $val
478000041213    > *  elenco altri dati default specifici dell'anagrafica
478100041213     C                   ENDIF
478200041213      *
478300041213     C     XRepDft       ENDSR
478400041213      ************************************************************
478500041213      * DEFINIZIONE OPZIONI UTILIZZABILI A PGM
478600041213      ************************************************************
478700041213     C     DEFOPZ        BEGSR
478800041213      *
478900041213      * Questa routine deve valorizzare la schiera $FL1
479000041213      * con S o N a seconda della utilizzabilità o meno
479100041213      * dell'opzione correlata
479200041213      *
479300041213      * Inizialmente si considerano tutte le opzioni come valide
479400041213     C     1             DO        $DIMOP        X
479500041213      * Imposta tutti i flg delle opzioni non a *blank, a 'S'
479600041213     C                   If        $TX1(X)  <> *Blank
479700041213     C                   MOVE      'S'           $FL1(X)
479800041213     C                   Else
479900041213     C                   Leave
480000041213     C                   EndIf
480100041213     C                   ENDDO
480200050103      *
480300041213      *
480400041213      * Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
480500041213      * con 1 o 0 a seconda della utilizzabilità o meno
480600041213      * del tasto funzione correlato
480700041213      *
480800041213      * Inizialmente ripristino le abilitazioni come da schiera a tempo
480900041213      * di compilazione
481000041213     C                   RESET                   $FC1
481100041213     C                   RESET                   $FR1
481200041213     C                   RESET                   $FC2
481300041213     C                   RESET                   $FR2
481400041213     C                   RESET                   $FC3
481500041213     C                   RESET                   $FR3
481600041213      *
481700041213      * Dopodichè, a seconda del richiamo e delle autorizzazioni
481800041213      * invalido alcune opzioni
481900041213     C                   SELECT
482000041213      * Se pgm richiamato in ricerca disattivo :
482100150910     C     OPZD25        WHENEQ    '01'
482200041213      * F3=Fine
482300041213     C                   Z-ADD     3             X
482400041213     C                   MOVE      '0'           $FC1(X)
482500041213     C                   MOVE      '0'           $FC2(X)
482600041213     C                   MOVE      '0'           $FC3(X)
482700041213      *
482800041213      * Se pgm richiamato in interrogazione disattivo :
482900150910     C     OPZD25        WHENEQ    '05'
483000041213      * F10=Immissione
483100041213     C                   Z-ADD     10            X
483200041213     C                   MOVE      '0'           $FC1(X)
483300041213     C                   MOVE      '0'           $FC2(X)
483400041221      * 1=Selezione
483500041221     C                   Z-ADD     1             X
483600041221     C     01            LOOKUP    $OP1(X)                                21
483700041221     C     *IN21         IFEQ      *ON
483800041221     C                   MOVE      'N'           $FL1(X)
483900041221     C                   ENDIF
484000041213      * 2=Modifica
484100041213     C                   Z-ADD     1             X
484200041213     C     02            LOOKUP    $OP1(X)                                21
484300041213     C     *IN21         IFEQ      *ON
484400041213     C                   MOVE      'N'           $FL1(X)
484500041213     C                   ENDIF
484600041213      * 3=Copia
484700041213     C                   Z-ADD     1             X
484800041213     C     03            LOOKUP    $OP1(X)                                21
484900041213     C     *IN21         IFEQ      *ON
485000041213     C                   MOVE      'N'           $FL1(X)
485100041213     C                   ENDIF
485200041213      * 4=Annullamento
485300041213     C                   Z-ADD     1             X
485400041213     C     04            LOOKUP    $OP1(X)                                21
485500041213     C     *IN21         IFEQ      *ON
485600041213     C                   MOVE      'N'           $FL1(X)
485700041213     C                   ENDIF
485800041213      *
485900150910     C     OPZD25        WHENEQ    *BLANKS
486000041213      * Se pgm non richiamato, disattivo subito :
486100041213      * F8=Selezione seguente
486200041213     C                   Z-ADD     8             X
486300041213     C                   MOVE      '0'           $FC1(X)
486400041213     C                   MOVE      '0'           $FC2(X)
486500041221      **1=Selezione
486600041221     C***                Z-ADD     1             X
486700041221     C***  01            LOOKUP    $OP1(X)                                21
486800041221     C***  *IN21         IFEQ      *ON
486900041221     C***                MOVE      'N'           $FL1(X)
487000041221     C***                ENDIF
487100041213      *
487200041213      * In tutti gli altri casi, disattivo subito :
487300041213     C                   OTHER
487400041217      **1=Selezione
487500041217     C***                Z-ADD     1             X
487600041217     C***  01            LOOKUP    $OP1(X)                                21
487700041217     C***  *IN21         IFEQ      *ON
487800041217     C***                MOVE      'N'           $FL1(X)
487900041217     C***                ENDIF
488000041213      *
488100041213     C                   ENDSL
488200041213      *
488300150910     C                   if        OpzD25 ='05'
488400050427      *
488500150910      * disattivo 1=Seleziona/deseleziona se richiamato in visualizzazione
488600050103     C                   Z-ADD     1             X
488700050103     C     01            LOOKUP    $OP1(X)                                21
488800050103     C     *IN21         IFEQ      *ON
488900050103     C                   MOVE      'N'           $FL1(X)
489000050103     C                   ENDIF
489100050427      *
489200050104     C                   endif
489300050103      *
489400050103      * poi, disattivo a seconda delle autorizzazioni :
489500041213      *
489600041213     C     XDefOpz       ENDSR
489700041213      ************************************************************
489800041213      * REPERIMENTO OPZIONI
489900041213      ************************************************************
490000041213     C     REPOPZ        BEGSR
490100041213      *
490200041213     C     1             DO        $DIMOP        X
490300041213      *
490400041213     C     $MS1(X)       IFNE      *BLANK
490500041213      *
490600041213     C                   MOVEL     $MS1(X)       IDMSG
490700041213     C                   CALLB     'XRTVM'
490800041213     C                   PARM                    IDMSG            27
490900041213     C                   PARM                    TXTMSG          100
491000041213     C                   PARM                    ERRMSG            1
491100041213      *
491200041213     C     ERRMSG        IFNE      *ON
491300041213     C                   MOVEL     TXTMSG        $TX1(X)
491400041213     C                   ELSE
491500041213     C                   MOVEL     *ALL'?'       $TX1(X)
491600041213     C                   ENDIF
491700041213      *
491800041213     C                   ENDIF
491900041213      *
492000041213     C                   ENDDO
492100041213      *
492200041213     C     XRepOpz       ENDSR
492300041213      ************************************************************
492400041213      * REPERIMENTO TASTI DI FUNZIONE
492500041213      ************************************************************
492600041213     C     REPKEY        BEGSR
492700041213     C                   MOVEA     $FM1          $FM
492800041213     C                   MOVEA     $FC1          $FC
492900041213     C                   EXSR      REPKKK
493000041213     C                   MOVEA     $FX           $FX1
493100041213      *
493200041213     C                   MOVEA     $FM2          $FM
493300041213     C                   MOVEA     $FC2          $FC
493400041213     C                   EXSR      REPKKK
493500041213     C                   MOVEA     $FX           $FX2
493600041213      *
493700041213     C                   MOVEA     $FM3          $FM
493800041213     C                   MOVEA     $FC3          $FC
493900041213     C                   EXSR      REPKKK
494000041213     C                   MOVEA     $FX           $FX3
494100041213     C     XRepKey       ENDSR
494200041213      ************************************************************
494300041213      * REPERIMENTO TASTI DI FUNZIONE
494400041213      ************************************************************
494500041213     C     REPKKK        BEGSR
494600041213      *
494700041213     C                   CLEAR                   $FX
494800041213     C     1             DO        24            X
494900041213      *
495000041213     C     $FM(X)        IFNE      *BLANK
495100041213     C     $Fc(X)        andne     '0'
495200041213      *
495300041213     C                   MOVEL     $FM(X)        IDMSG
495400041213     C                   CALLB     'XRTVM'
495500041213     C                   PARM                    IDMSG            27
495600041213     C                   PARM                    TXTMSG          100
495700041213     C                   PARM                    ERRMSG            1
495800041213      *
495900041213     C     ERRMSG        IFNE      *ON
496000041213     C                   MOVEL     TXTMSG        $FX(X)
496100041213     C                   ELSE
496200041213     C                   MOVEL     *ALL' '       $FX(X)
496300041213     C                   ENDIF
496400041213      *
496500041213     C                   ENDIF
496600041213      *
496700041213     C                   ENDDO
496800041213      *
496900041213     C     XRepKKK       ENDSR
497000041213      ************************************************************
497100041213      * DEFINIZIONE KLIST
497200041213      ************************************************************
497300041213     C     DefKlist      BEGSR
497400150924      *
497500150924     C* NdIva01l
497600150924     C     K02Iva01      Klist
497700150924     C                   Kfld                    IVASys
497800150924     C                   Kfld                    IVANrasReg
497900150929     C* NdREG01l
498000150929     C     K02REG01      Klist
498100150929     C                   Kfld                    REGSys
498200150929     C                   Kfld                    REGNrasReg
498300150918      *
498400150924     C* SdgLrc01l
498500150918     C     K03Lrc01      Klist
498600150918     C                   Kfld                    LrcSocieta
498700150918     C                   Kfld                    LrcSys
498800150918     C                   Kfld                    LrcNrasReg
498900150918      *
499000150924     C* SdgLrc02l
499100150918     C     K03Lrc02      Klist
499200150918     C                   Kfld                    LrcSocieta
499300150918     C                   Kfld                    LrcSysl
499400150918     C                   Kfld                    LrcNrasL
499500041222      *
499600041213     C     XDefKlist     ENDSR
499700041213      ************************************************************
499800041213** $MS1 / $TX1
499900150929PROMSG    *LIBL     OPZ0001
500000160408PROMSG    *LIBL     OPZ0484     ## R2212
500100160125SDGMSG    *LIBL     SDO0054
500200160708PROMSG    *LIBL     OPZ0041     ## R2321
500300160705PROMSG    *LIBL     OPZ0332     ## R2322
500400140714
500500140701
500600140701
500700041213** $OP1 / $FL1
50080004121301S      <Selezione
50090016040804S      <Annulla selezione     ## R2212
50100016012515S      <Documento legato
50110016070841S      <Contabilità           ## R2321
50120016070544S      <Modifica codice soggetto  ## R2322
501300140701
501400140701
501500041213** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
501600041213PROMSG    *LIBL     KEY0001  01
501700041213PROMSG    *LIBL     KEY0002  02
501800041213PROMSG    *LIBL     KEY0003  03
501900041213PROMSG    *LIBL     KEY0004  04
502000041213PROMSG    *LIBL     KEY0005  05
502100041213PROMSG    *LIBL     KEY0006  06
502200041213PROMSG    *LIBL     KEY0007  07
502300041213PROMSG    *LIBL     KEY0008  08
502400041213PROMSG    *LIBL     KEY0009  09
502500041213PROMSG    *LIBL     KEY0010  10
502600041213PROMSG    *LIBL     KEY0011  11
502700041213PROMSG    *LIBL     KEY0012  12
502800041213PROMSG    *LIBL     KEY0013  13
502900041213PROMSG    *LIBL     KEY0014  14
503000041213PROMSG    *LIBL     KEY0015  15
503100041213PROMSG    *LIBL     KEY0016  16
503200041213PROMSG    *LIBL     KEY0017  17
503300041213PROMSG    *LIBL     KEY0018  18
503400041213PROMSG    *LIBL     KEY0019  19
503500041213PROMSG    *LIBL     KEY0020  20
503600041213PROMSG    *LIBL     KEY0021  21
503700041213PROMSG    *LIBL     KEY0022  22
503800041213PROMSG    *LIBL     KEY0023  23
503900041213PROMSG    *LIBL     KEY0024  24
504000041213**  ABILITAZIONE D1 : $FC1 / $FR1
5041000412130N  01
5042000412131N  02
5043000412131N  03
5044000412131N  04
5045000412130N  05
5046000412130N  06
5047000412130N  07
5048000412131N  08
5049000412130N  09
5050000412130N  10
5051000412130N  11
5052000412131N  12
5053000412130N  13
5054000412130N  14
5055000412130N  15
5056000412130N  16
5057000412130N  17
5058000511141N  18
5059000412130N  19
5060000412130N  20
5061000412130N  21
5062000412130N  22
5063000412130N  23   Impostare a '1' se si utilizzano opzioni
5064000412131N  24   Lasciare sempre impostata a '1' il valore della riga 24
506500041213** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
506600041213PROMSG    *LIBL     KEY0001  01
506700041213PROMSG    *LIBL     KEY0002  02
506800041213PROMSG    *LIBL     KEY0003  03
506900041213PROMSG    *LIBL     KEY0004  04
507000041213PROMSG    *LIBL     KEY0005  05
507100041213PROMSG    *LIBL     KEY0006  06
507200041213PROMSG    *LIBL     KEY0007  07
507300041213PROMSG    *LIBL     KEY0008  08
507400041213PROMSG    *LIBL     KEY0009  09
507500041213PROMSG    *LIBL     KEY0010  10
507600041213PROMSG    *LIBL     KEY0011  11
507700041213PROMSG    *LIBL     KEY0012  12
507800041213PROMSG    *LIBL     KEY0013  13
507900041213PROMSG    *LIBL     KEY0014  14
508000041213PROMSG    *LIBL     KEY0015  15
508100041213PROMSG    *LIBL     KEY0016  16
508200041213PROMSG    *LIBL     KEY0017  17
508300041213PROMSG    *LIBL     KEY0018  18
508400041213PROMSG    *LIBL     KEY0019  19
508500041213PROMSG    *LIBL     KEY0020  20
508600041213PROMSG    *LIBL     KEY0021  21
508700041213PROMSG    *LIBL     KEY0022  22
508800041213PROMSG    *LIBL     KEY0023  23
508900041213PROMSG    *LIBL     KEY0024  24
509000041213**  ABILITAZIONE S1 : $FC2 / $FR2
5091000412130N  01
5092000412130N  02
5093000412131N  03
5094000412130N  04
5095000412130N  05
5096000412231N  06
5097000412130N  07
5098000412131N  08
5099000412170N  09
5100000412130N  10
5101000412130N  11
5102000412131N  12
5103000412170N  13
5104000412130N  14
5105001601250N  15
5106000412130N  16
5107000511160N  17
5108001509300N  18
5109000412130N  19
5110000412130N  20
5111000412130N  21
5112000412130N  22
5113000412131N  23   Impostare a '1' se si utilizzano opzioni
5114000412131N  24   Lasciare sempre impostata a '1' il valore della riga 24
511500041213** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
511600041213PROMSG    *LIBL     KEY0001  01
511700041213PROMSG    *LIBL     KEY0002  02
511800041213PROMSG    *LIBL     KEY0003  03
511900041213PROMSG    *LIBL     KEY0004  04
512000041213PROMSG    *LIBL     KEY0005  05
512100041213PROMSG    *LIBL     KEY0006  06
512200041213PROMSG    *LIBL     KEY0007  07
512300041213PROMSG    *LIBL     KEY0008  08
512400041213PROMSG    *LIBL     KEY0009  09
512500041213PROMSG    *LIBL     KEY0010  10
512600041213PROMSG    *LIBL     KEY0011  11
512700041213PROMSG    *LIBL     KEY0012  12
512800041213PROMSG    *LIBL     KEY0013  13
512900041213PROMSG    *LIBL     KEY0014  14
513000041213PROMSG    *LIBL     KEY0015  15
513100041213PROMSG    *LIBL     KEY0016  16
513200041213PROMSG    *LIBL     KEY0017  17
513300041213PROMSG    *LIBL     KEY0018  18
513400041213PROMSG    *LIBL     KEY0019  19
513500041213PROMSG    *LIBL     KEY0020  20
513600041213PROMSG    *LIBL     KEY0021  21
513700041213PROMSG    *LIBL     KEY0022  22
513800041213PROMSG    *LIBL     KEY0023  23
513900041213PROMSG    *LIBL     KEY0024  24
514000041213**  ABILITAZIONE D3 : $FC3 / $FR3
5141000412130N  01
5142000412130N  02
5143000412131N  03
5144000412130N  04
5145000412130N  05
5146000412130N  06
5147000412130N  07
5148000412130N  08
5149000412130N  09
5150000412130N  10
5151000412130N  11
5152000412131N  12
5153000412130N  13
5154000412130N  14
5155000412130N  15
5156000412130N  16
5157000412130N  17
5158000412130N  18
5159000412130N  19
5160000412130N  20
5161000412130N  21
5162000412130N  22
5163000412130N  23   Impostare a '1' se si utilizzano opzioni
5164000412131N  24   Lasciare sempre impostata a '1' il valore della riga 24
