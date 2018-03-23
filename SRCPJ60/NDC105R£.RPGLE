000100161130>>>>>H DFTACTGRP(*NO) ACTGRP('QILE')
000200161130>>>>>H BNDDIR('PJXBND':'PJCBND')
000300970107     H*PARMS CVTOPT(*NONE)
000400950608     H DECEDIT('0,') DATEDIT(*DMY.)
000500990323     F*-------------
000600900731     F*
000700950614     F* STAMPA GIORNALE BOLLATO
000800900731     F*
000900990323     F*-------------
001000990323     FNDGIOPRT  O    E             PRINTER OFLIND(*IN44) USROPN
001001161129>>>>>F                                     INFDS(yNdGioPrtInf)
001100100528C2161X***FNDREG15L  IF   E           K DISK
001200100528C2161FNDREG15L  UF   E           K DISK
001300951102    >FNDDES01L  IF   E           K DISK
001400950614    >FNDMOV01L  UF   E           K DISK
001500950614    >FANOPE01L  IF   E           K DISK
001600990323     D*-------------
001700900731     D KPJBA         E DS
001800950605     D*-------------
001900950130     D* Reperimento nome PGM
002000950130     D STATUS         SDS           333
002100950130     D  DSPGM            *PROC
002101161129>>>>>D  psJobName            244    253                                         Nome lavoro
002102161129>>>>>D  psJobUser            254    263                                         Utente
002103161129>>>>>D  psJobNbr             264    269                                         Numero lavoro
002200950605     D*-------------
002300950130     D* Parametri del PGM
002400950605     D C103DS        E DS                  EXTNAME(NDC103DS)
002500950616     D*-------------
002600950616     D* Decodifica conti
002700950616     D ND002DS       E DS
002800950405     D*-------------
002900990421     D* DS esterna per dati soggetti
003000990421     D DVASOG        E DS
003100990421     D*-------------
003200990421     D* Ds per output variazioni estremi fiscali
003300990421     D ANCPI         E DS                  EXTNAME(ANCPI00F)
003400990421     D*-------------
003500941103     D* Dati per pgm XSOC
003600941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
003700100601     D    SIFArcSos                   1    overlay(XScFGO:12)
003800941103     D*-------------
003900950605     D* DS Interna per dati utente
004000941103     D XSOCDS          DS          1000
004100970623     D*-------------
004200970623     D* Parametri per reperimento esercizio
004300990323    >D x10eseds      E DS
004400950614     D*-------------
004500950614     D* Sottodefinizione campo MELSTRINGA
004600960705     D GIOBDS        E DS                  EXTNAME(ndc105ds)
004700990323     D*-------------
004800950614     D* Definizione campi di lavoro
004900950614     D DARE            S                   like(MOVIMPORTO)
005000950614     D AVERE           S                   like(MOVIMPORTO)
005100950614     D GIODARE         S                   like(MOVIMPORTO)
005200950614     D GIOAVERE        S                   like(MOVIMPORTO)
005300950614     D ESEDARE         S                   like(MOVIMPORTO)
005400950614     D ESEAVERE        S                   like(MOVIMPORTO)
005500950614     D PEGDARE         S                   like(MOVIMPORTO)
005600950614     D PEGAVERE        S                   like(MOVIMPORTO)
005700950614     D PEEDARE         S                   like(MOVIMPORTO)
005800950614     D PEEAVERE        S                   like(MOVIMPORTO)
005900950614     D PRGDARE         S                   like(MOVIMPORTO)
006000950614     D PRGAVERE        S                   like(MOVIMPORTO)
006100950606     D savdtlgio       S                   like(movdtlgio)
006200100528C2161D SAVNRASREG      S                   like(movnrasreg)
006300950614     D NRRIGA          S                   like(URIGA)
006400100528C2161D NRREGI          S                   like(UREGI)
006500970623     D esedtlgio       S                   like(x10eserciz)
006600970623     D esedtreg        S                   like(x10eserciz)
006700970916     D wrkep           S                   like(p1ep)
006800950614     D XDTDAT          S               D
006900950614     D XMLDAT          S               D
007000030908D1435D DtfPrec         S               D
007100950614     D XMLstr          S            256
007200011211     D*-------------
007300011212c1529D* Parametri memorizzazione intestazione
007400011211  "  D xpaout          DS          2000
007500011211  "  D* DS esterna per stampa intestazione bollato
007600011211C1529D ndc099ds      E DS
007700990323     D*-------------
007800950616     D* Definizioni parametri standard chiamate moduli
007900950616     D$Esito           S              1
008000950616     D$gesmsg          S              1
008100950616     D$lenout          S              9B 0
008200950616     D$struttura       S             10
008300950616     D$date            S               D
008400950616     D$kcc             S              6
008500950616     D$ksc             S              8
008600950616     D$SOc             S              3
008700990323     D*-------------
008800990421     D* Definizioni parametri driver soggetti
008900990421     D dvaRic          S              1
009000990421     D dvaSocieta      S              3
009100990421     D dvaUnita        S              8
009200990421     D dvaDtRif        S               D
009300990421     D dvaStrutt       S             10
009400990421     D dvaLenOut       S              9B 0
009500990421     D dvaSnatura      S              1
009600990421     D dvaCodice       S              8
009700990421     D dvaLineav       S              3
009800990421     D dvaFiliale      S              3  0
009900990421     D dvaSogg         S              8
010000990421     D dvaTpInd        S              2
010100990421     D dvaCdInd        S              3
010200990421     D dvaOutput       S           4000
010300990421     D dvaErrore       S              1
010400990421     D*-------------
010500990421     D* Definizioni parametri driver estremi fiscali
010600990421     D dvcRic          S              1
010700990421     D dvcSocieta      S              3
010800990421     D dvcUnita        S              8
010900990421     D dvcStrutt       S             10
011000990421     D dvcDtRif        S               D
011100990421     D dvcLenOut       S              9B 0
011200990421     D dvcSogg         S              8
011300990421     D dvcErrore       S              1
011400990421     D dvcOutput       S           4000
011500990421     D*-------------
011600130618>>>>>D Sys             S                   Like(MOVSys)
011700130618>>>>>D NrAsInt         S                   Like(MOVNrAsReg)
011800130618>>>>>D NrAsGen         S                   Like(MOVNrAsReg)
011900130618>>>>>D LenOut          S              9B 0
012000130618>>>>>D Esito           S              1
012100130618>>>>>D NDCPADS       E DS                  ExtName(NDCPA00F) Inz
012200000118C1309D ZDateISO        S               D
012201161129>>>>>D yNdGioPrtInf    DS                  QUALIFIED
012203161129>>>>>D  spoolNum             160    163I 0                                      6 digit Spool Nbr
012205161130>>>>> /COPY GAITRASRC/SRCPROTOPR,TRBMAF05R
012207161130
012300950405     C*----------------------------------------------------*
012400950405     C*                MAIN LINE PROGRAM
012500950405     C*----------------------------------------------------*
012600990323     C*
012700950606     C* Inizializzazione variabili
012800950606     C                   EXSR      INZVAR
012900950606     C*
013000950614     C* Lettura NDREG e NDMOV
013100950606     C                   EXSR      REDREG
013200950614     C*
013300950614     C* Stampa totali periodo e progressivo
013400950614     C                   EXSR      STAPER
013500950614     C*
013600990421     C* Se stampa definitiva aggiorna ultima data giornale bollato
013700990421     C* Se stampa definitiva scrive progressivi riga bollato e nr. riga
013800950614     C     STD103        IFEQ      *ON
013801161130>>>>>C                   IF        SendToDocFlow() < *ZERO
013802161130     C                   EVAL      err103 = *ON
013803161130     C                   EXSR      EndPgm
013804161130     C                   ENDIF
013900950614     C                   EXSR      AGGDAT
014000950614     C                   EXSR      AGGMEL
014100950614     C                   ENDIF
014200990421     C* Se richiesto stampa lista causali
014300990126     C     SCC103        IFEQ      *ON
014400990126     C                   EXSR      staliscau
014500990126     C                   ENDIF
014600900731     C*
014700950606     C* Fine programma
014800950606     C                   EXSR      ENDPGM
014900950606     C*
015000950606     C/EJECT
015100950606     C************************************************************
015200950606     C* FINE PROGRAMMA
015300950606     C************************************************************
015400950606     C     ENDPGM        BEGSR
015500011211     c*
015600011212c1529C* se era stata chiesta l'intestazione, aggiorno con ultimo numero
015700011212  "  C* di pagina il parametro modulo dove è memorizzata
015800011211  "  c                   if        int103=*on  and
015900011211  "  c* e se stampa definitiva
016000011212  "  c                             std103=*on  and
016100011212  "  c* e se non ci sono stati errori
016200011212  "  c                             err103<> *on
016300011212  "  c                   move      NumeroPag     Pgi099
016400011211  "  C                   CALLB     'XPAR'
016500011211  "  C                   parm      soc103        xscsoc
016600011211  "  c                   parm      'BOLLATI '    xpapar            8
016700011211  "  c                   parm      ndc099ds      xpaout
016800011211  "  C                   parm      *OFF          xpaerr            1
016900011211  "  C                   parm      '1'           xparic            1
017000011211c1529c                   endif
017100950606     C*
017200950606     C                   MOVEL     C103DS        KPJBU
017300950606     C                   SETON                                        LR
017400950606     C                   RETURN
017500950606     C*
017600950606     C                   ENDSR
017700950606     C************************************************************
017800950606     C* INIZIALIZZAZIONE VARIABILI
017900950606     C************************************************************
018000950606     C     INZVAR        BEGSR
018100950606     C*
018200950606     C                   MOVEL     KPJBU         C103DS
018300950606     C*
018400950606     C* Reperimento azienda
018500950606     C                   MOVE      SOC103        SOCXSC
018600950606     C                   MOVEL     'SOC001'      TIPXSC
018700950606     C                   EXSR      REPSOC
018800950606     C     RTNXSC        IFNE      '1'
018900950606     C                   MOVEL     XSOCDS        SOC001
019000950606     C                   ELSE
019100950614     C                   MOVE      *ON           ERR103
019200950606     C                   EXSR      ENDPGM
019300950606     C                   ENDIF
019400981124     C*
019500981124     C* Eseguo ovrprtf da tabella G75
019600981124     C                   EXSR      CallX32
019700950606     C*
019800950606     C* Inizializzazione KLIST lettura NDREG
019900950606     C                   MOVE      SOC103        REGSOCIETA
020000950606     C                   MOVE      CTB103        REGCTB
020100950606     C                   MOVE      'D'           REGTPREGZ
020200950606     C                   MOVE      DTI103        REGDTLGIO
020300950614     C*
020400950615     C* reperimento ultima data giornale bollato
020500950615     C* reperimento ultima riga bollato e progressivi
020600030908D1435c                   if        Std103 = *on
020700950614     C                   EXSR      REPDAT
020800030908D1435c                   endif
020900950614     C                   EXSR      REPMEL
021000991029     C* Se richiesto stampa nr. protocollo ...
021100991029     C                   If        NrP103 = *On
021200991029     C                   SetOn                                        18
021300991029     C                   EndIf
021400011211     C*
021500011211c1529C* se richiesto reperisco intestazione memorizzata sul
021600011211  "  C* parametro modulo 'BOLLATI'
021700011211  "  C                   if        int103=*on
021800011211  "  C                   CALLB     'XPAR'
021900011211  "  C                   parm                    xscsoc
022000011211  "  C                   parm      'BOLLATI'     xpapar            8
022100011211  "  C                   parm                    xpaout
022200011211  "  C                   parm      *OFF          xpaerr            1
022300011212  "  C                   parm                    xparic            1
022400011211  "  c                   movel     xpaout        ndc099ds
022500011212  "  c                   if        xpaerr <> *on
022600011211  "  c* imposto pagina da cui partire
022700011212  "  c                   move      pgi099        NumeroPag         6 0
022800011212  "  c                   else
022900011212  "  c* arriva messaggio di errore da xpar all'operatore
023000011212  "  c                   eval      err103 = *on
023100011212  "  c                   exsr      endpgm
023200011212  "  c                   endif
023300011211c1529c                   endif
023400950614     C* Stampa testata
023500011211c1529x***                WRITE     C105T1
023600011211c1529c                   exsr      StampaTes
023700950614     C* Stampa progressivi
023800950614     C                   Z-ADD     PDARE         P6DARE
023900950614     C                   Z-ADD     PAVERE        P6AVERE
024000950615     C* Incremento progressivi
024100950615     C                   Z-ADD     PDARE         PRGDARE
024200950615     C                   Z-ADD     PAVERE        PRGAVERE
024300950614     C                   Z-ADD     URIGA         NRRIGA
024400100528C2161C                   Z-ADD     UREGI         NRREGI
024500950614     C                   WRITE     C105P6
024600950606     C*
024700950606     C                   ENDSR
024800950606     C/EJECT
024900950405     C************************************************************
025000950606     C* LETTURA NDREG
025100950405     C************************************************************
025200950606     C     REDREG        BEGSR
025300940831     C*
025400950606     C     K4REG15       SETLL     NDREG15L
025500950606     C     K3REG15       READE     NDREG000                               21
025600950606     C*
025700950606   B1C     *IN21         DOWEQ     *OFF
025800950606     C*
025900950606     C* Se data libro giornale > data fine esco
026000950606   B2C     REGdtlgio     IFGT      DTF103
026100950606     C                   LEAVE
026200950606   E2C                   ENDIF
026300950606     C*
026400950606     C* Azzero per giornata i totali dare e avere
026500950606     C                   MOVE      REGDTLGIO     SAVDTLGIO
026600950614     C                   CLEAR                   GIODARE
026700950614     C                   CLEAR                   GIOAVERE
026800950614     C                   CLEAR                   ESEDARE
026900950614     C                   CLEAR                   ESEAVERE
027000950606   B2C     REGDTLGIO     DOWEQ     SAVdtlGIO
027100950606     C     *IN21         ANDEQ     *OFF
027200950606     C*
027300950606     C* Lettura NDMOV01L per numero registrazione
027400950606     C                   EXSR      REDMOV
027500951102     C*
027600951102     C* Stampa descrizioni aggiuntive di testata
027700951102     C                   Eval      DesTipDes = 'T'
027800951102     C                   Eval      DesNrRigaM = 0
027900951102     C                   exsr      stadesagg
028000950606     C*
028100950606     C     K3REG15       READE     NDREG000                               21
028200950606   E2C                   ENDDO
028300950606     C*
028400950614     c* A rottura di giornata stampo totale giorno dare avere
028500950614     C* e totale esercizio dare e avere
028600950614     C                   EXSR      STAGIO
028700950614     C*
028800950606   E1C                   ENDDO
028900950127     C*
029000940831     C                   ENDSR
029100950606     C/EJECT
029200950606     C************************************************************
029300950606     C* LETTURA NDMOV
029400950606     C************************************************************
029500950606     C     REDMOV        BEGSR
029600950606     C*
029700950606     C                   MOVE      REGSYS        MOVSYS
029800950606     C                   MOVE      REGNRASREG    MOVNRASREG
029900950606     C     K2MOV01       SETLL     NDMOV01L
030000950606     C                   DO        *hival
030100080208C2080C     STD103        IFNE      *on
030200080208  "  C     K2MOV01       READE(n)  NDMOV000                               21
030300080208c2080c                   else
030400950606     C     K2MOV01       READE     NDMOV000                               21
030500080208c2080c                   endif
030600950606     C   21              LEAVE
030700950606     C*
030800950614     C     MOVIMPORTO    MULT      MOVAVERE      AVERE
030900950614     C     MOVIMPORTO    MULT      MOVDARE       DARE
031000970916     c* Controllo movimento esercizio precedente
031100970916     C                   clear                   wrkep
031200970916     c* se data libro giornale diversa da data registrazione
031300970916     C     MOVDTLGIO     IFNE      MOVDTREG
031400970916     c* reperisco esercizio contabile data libro giornale
031500970916     c                   reset                   x10eseds
031600970916     C                   move      xscsoc        X10societa
031700970916     C                   move      ctb103        X10ctb
031800970916     C                   move      movdtlgio     x10data
031900970916     C                   call      'X10ESER'
032000970916     C                   parm                    x10eseds
032100970916     c                   z-add     x10eserciz    esedtlgio
032200970916     c* reperisco esercizio contabile data registrazione
032300970916     c                   reset                   x10eseds
032400970916     C                   move      xscsoc        X10societa
032500970916     C                   move      ctb103        X10ctb
032600970916     C                   move      movdtreg      x10data
032700970916     C                   call      'X10ESER'
032800970916     C                   parm                    x10eseds
032900970916     c                   z-add     x10eserciz    esedtreg
033000970916     c* se esercizi diversi stampo indicazione 'esercizio precedente'
033100970916     c                   if        esedtlgio <> esedtreg
033200970916     C                   eval      wrkep = 'EP'
033300970916     c                   endif
033400970916     C                   ENDIF
033500950614     C* Totali periodo del giorno o esercizio e
033600950614     C* Totali giorno o esercizio
033700970916     C     wrkep         ifeq      *blanks
033800950614     C                   ADD       AVERE         GIOAVERE
033900950614     C                   ADD       DARE          GIODARE
034000950614     C                   ADD       AVERE         PEGAVERE
034100950614     C                   ADD       DARE          PEGDARE
034200950614     C                   ELSE
034300950614     C                   ADD       AVERE         ESEAVERE
034400950614     C                   ADD       DARE          ESEDARE
034500950614     C                   ADD       AVERE         PEEAVERE
034600950614     C                   ADD       DARE          PEEDARE
034700950614     C                   ENDIF
034800950614     C* stampa riga registrazione
034900950614     C                   EXSR      STARIG
035000951102     C*
035100951102     C* stampa descrizioni aggiuntive di movimento se presenti
035200951102     C                   Eval      DesTipDes = 'M'
035300951102     C                   Eval      DesNrRigaM = MovNrRigaM
035400951102     C                   EXSR      StaDesAgg
035500950614     C*
035600950614     C* AGGIORNA NDMOv per nr. riga e desbrevcausale
035700950614     C*
035800950614     C     STD103        IFEQ      *on
035900950614     C                   Z-ADD     P1NRLGIO      MOVNRLGIO
036000950614     C     movdesbrev    ifeq      *blanks
036100950614     C                   MOVEL     p1desbrev     MOVdesbrev
036200950614     C                   ENDIF
036300950614     C                   EXCEPT    AGGMOV
036400950614     C                   ENDIF
036500950614     C*
036600950606     C                   ENDDO
036700100528C2161C*
036800100528  "  C* AGGIORNA NDREG per nr. registrazione
036900100528  "  C*
037000100528  "  C     STD103        IFEQ      *on
037100100528  "  C     SifArcSos     ANDEQ     *on
037200100528  "  C                   MOVE      NRREGI        REGENTIT3
037300100528  "  C                   EXCEPT    AGGREG
037400100528C2161C                   ENDIF
037500950606     C*
037600950606     C                   ENDSR
037700951102     C/EJECT
037800951102     C************************************************************
037900951102     C* Stampa descrizioni aggiuntive
038000951102     C************************************************************
038100951102     C     STADesAgg     BEGSR
038200951102     C*
038300951102     C                   Eval      DesSYS     =  MOVSYS
038400951102     C                   eval      DesNRASREG =  MOVNRASREG
038500951102     C     K04Des01      SETLL     NDDes01L
038600951102     C                   DO        *hival
038700951102     C     K04des01      READE     NDdes000                               21
038800951102     C   21              LEAVE
038900951102     C*
039000951102     C                   IF        DesUsoCiv = *on
039100951102     C*
039200951102     C                   SELECT
039300951102     C                   WHEN      DesTipDes = 'T'
039400951102     C                   eval      TDesAgg = DesTesRig
039500951103     C   44              EXSR      OVER
039600100528c2161c                   if        SifArcSos=*on
039700100528  "  C                   write     C105DESTX
039800100528c2161c                   else
039900951102     C                   write     C105DEST
040000100528c2161c                   endif
040100951102     C*
040200951102     C                   WHEN      DesTipDes = 'M'
040300951102     C                   eval      MDesAgg = DesTesRig
040400951103     C   44              EXSR      OVER
040500100528c2161c                   if        SifArcSos=*on
040600100528  "  C                   write     C105DESMX
040700100528c2161c                   else
040800951102     C                   write     C105DESM
040900100528c2161c                   endif
041000951102     C                   ENDSL
041100951102     C*
041200951102     C                   ENDIF
041300951102     C*
041400951102     C                   ENDDO
041500951102     C*
041600951102     C                   ENDSR
041700951102     C/EJECT
041800950614     C************************************************************
041900950614     C* STAMPA TOTALI DEL GIORNO
042000950614     C************************************************************
042100950614     C     STAGIO        BEGSR
042200950614     C* giorno
042300950614     C                   Z-ADD     GIODARE       P2DARE
042400950614     C                   Z-ADD     GIOAVERE      P2AVERE
042500000118C1309C******dmy          MOVE      SAVDTLGIO     P2DTREG
042600000118C1309C     *JobRun       MOVE      SAVDTLGIO     P2DTREG
042700000118C1309C******dmy          MOVE      SAVDTLGIO     P2DTREGA
042800000118C1309C     *JobRun       MOVE      SAVDTLGIO     P2DTREGA
042900950614     C*
043000011211c1529x***44              WRITE     C105T1
043100011211c1529c   44              exsr      StampaTes
043200950614     C   44              SETOFF                                       44
043300950614     C                   WRITE     C105P2
043400950614     C* esercizio precedente
043500950614     C                   Z-ADD     ESEDARE       P3DARE
043600950614     C                   Z-ADD     ESEAVERE      P3AVERE
043700950614     C     P3DARE        IFNE      0
043800950614     C     P3AVERE       ORNE      0
043900011211c1529x***44              WRITE     C105T1
044000011211c1529c   44              exsr      StampaTes
044100950614     C   44              SETOFF                                       44
044200950614     C                   WRITE     C105P3
044300950614     C                   ENDIF
044400950614     C*
044500950614     C                   ENDSR
044600950614     C/EJECT
044700950614     C************************************************************
044800950614     C* STAMPA TOTALI DEL PERIODO
044900950614     C************************************************************
045000950614     C     STAPER        BEGSR
045100950614     C*
045200950614     C                   Z-ADD     PEGDARE       P4DARE
045300950614     C                   Z-ADD     PEGAVERE      P4AVERE
045400950614     C*
045500011211c1529x***44              WRITE     C105T1
045600011211c1529c   44              exsr      StampaTes
045700950614     C   44              SETOFF                                       44
045800950614     C                   WRITE     C105P4
045900950614     C* esercizio precedente
046000950614     C                   Z-ADD     PEEDARE       P5DARE
046100950614     C                   Z-ADD     PEEAVERE      P5AVERE
046200950614     C     P5DARE        IFNE      0
046300950614     C     P5AVERE       ORNE      0
046400011211c1529x***44              WRITE     C105T1
046500011211c1529c   44              exsr      StampaTes
046600950614     C   44              SETOFF                                       44
046700950614     C                   WRITE     C105P5
046800950614     C                   ENDIF
046900950614     C*
047000950614     C* stampo progressivi finali
047100950614     C*
047200950614     C                   Z-ADD     PRGDARE       P6DARE
047300950614     C                   Z-ADD     PRGAVERE      P6AVERE
047400011211c1529x***44              WRITE     C105T1
047500011211c1529c   44              exsr      StampaTes
047600950615     C   44              SETOFF                                       44
047700950614     C                   WRITE     C105P6
047800950614     C*
047900950614     C                   ENDSR
048000950614     C/EJECT
048100950606      ************************************************************
048200950614      * STAMPA RIGA REGISTRAZIONE
048300950606      ************************************************************
048400950614     C     STARIG        BEGSR
048500950606      *
048600950614     C                   CLEAR                   C105P1
048700100528c2161c                   CLEAR                   C105P1X
048800970916     C                   eval      p1ep = wrkep
048900950614     C     STD103        IFEQ      *ON
049000950614     C                   ADD       1             NRRIGA
049100950614     C                   Z-add     nrriga        P1NRLGIO
049200100601c2161C     MOVNRASREG    IFNE      SAVNRASREG
049300100528  "  C                   ADD       1             NRREGI
049400100528  "  C                   Z-add     nrregi        P1RRLGIO
049500100528  "  C                   Z-add     MOVNRASREG    SAVNRASREG
049600100601c2161C                   ENDIF
049700950614     C                   ENDIF
049800970604     C     MOVDTLGIO     IFNE      *LOVAL
049900000118C1309C******dmy          MOVE      movdtlgio     P1dtreg
050000000118C1309C     *JobRun       MOVE      movdtlgio     P1dtreg
050100950615     C                   ENDIF
050200991029     C                   If        NrP103 = *On
050300991029     C                   Z-Add     REGNrReg      P1NrReg
050400991029     C                   EndIf
050500950614     C                   Z-ADD     movnrdoc      P1nrdoc
050600950615     C     MOVDTDOC      IFNE      *loval
050700000118C1309C******dmy          MOVE      movdtdoc      P1dtdoc
050800000118C1309C     *JobRun       MOVE      movdtdoc      P1dtdoc
050900960704     c                   seton                                        17
051000960704     c                   else
051100960704     c                   setoff                                       17
051200950615     C                   END
051300950614     C                   MOVE      MOVCAUSRIG    P1causrig
051400950614     C                   MOVEL     MOVDESBREV    P1DESBREV
051500950614     C     P1DESBREV     IFEQ      *BLANKS
051600950614     C                   MOVE      MOVSOCIETA    OPESOCIETA
051700950614     C                   MOVE      MOVCAUSRIG    OPECAUSALE
051800950614     C     K2OPE01       CHAIN     ANOPE01L                           21
051900950614     C  N21              MOVEL     OPEDESBREV    P1DESBREV
052000950614     C                   ENDIF
052100950614     C                   MOVE      MOVKCC        P1KCC
052200950614     C                   MOVE      MOVKSC        P1KSC
052300950614     C                   Z-add     avere         P1AVERE
052400950614     C                   Z-add     DARE          P1DARE
052500990421     C* Reperisco ragione sociale
052600990421     C                   ExSr      RepRagSoc
052700950614      *
052800950614     C   44              EXSR      OVER
052900960704     C* Incremento progressivi dopo salto pagina
053000960704     C                   ADD       AVERE         PRGAVERE
053100960704     C                   ADD       DARE          PRGDARE
053200960704     c*
053300100528c2161c                   if        SifArcSos=*on
053400100528  "  C                   WRITE     C105P1X
053500100528c2161c                   else
053600950614     C                   WRITE     C105P1
053700100528c2161c                   endif
053800950606      *
053900950606     C                   ENDSR
054000950614     C/EJECT
054100990421      ************************************************************
054200990421      * Reperimento ragione sociale
054300990421      ************************************************************
054400990421     C     RepRagSoc     BEGSR
054500990421      *
054600990421     C                   EXSR      CALLMVC002
054700990421     C     $esito        ifeq      *off
054800990819D0103C                   movel     Descr002      P1Descr
054900990819D0194 * Se conto gestito a soggetti ma non è una banca ...
055000030804D0194 * (per la banca è sufficiente la descrizione del p.d.c.)
055100030804D1416C                   if        snatura002='C' or snatura002='F'
055200020905D0194C     kccsogg002    ifeq      *on
055300020905D1193C***ex D0194  snatura002    andne     '2'
055400990421      * Reperimento dati da variazioni estremi fiscali
055500990421     C                   EXSR      CALLDVACPI
055600990421     C     dvcerrore     ifeq      *off
055700990421     C                   movel     dvcoutput     ancpi
055800990421     C                   movel     cpidessog1    p1descr
055900990421     C                   else
056000990421      * Reperimento dati da anagrafica soggetti
056100990421     C                   EXSR      CALLDVASOG
056200990421     C     dvaerrore     ifeq      *off
056300990421     C                   movel     dvaoutput     dvasog
056400990421     C                   movel     dvsdes        p1descr
056500990421     C                   endif
056600990421     C                   endif
056700990819D0194C                   endif
056800030804D1416C                   endif
056900130618>>>>>C* Reperisco cliente se esiste in porti assegnati
057000130618>>>>>C                   if        tecnico002 = '2'
057100130618>>>>>C                   EXSR      RepEstNomP
057200130618>>>>>C                   EndIf
057300990421     C                   endif
057400990421      *
057500990421     C                   ENDSR
057600990421     C/EJECT
057700950614      ************************************************************
057800950614      * DECODIFICA CONTO
057900950614      ************************************************************
058000950616     C     CallMvc002    BEGSR
058100950616     C*
058200950616     C                   Clear                   ND002DS
058300950616     C                   EVAL      $LenOut = %Size(ND002DS)
058400950616     C                   CallB     'NDMVC002'
058500950616     C                   PARM      MOVSOCIETA    $soc
058600950616     C                   PARM      MOVkcc        $kcc
058700950616     C                   PARM      movksc        $ksc
058800950616     C                   PARM                    $date
058900950616     C                   PARM      *OFF          $GesMsg
059000950616     C                   PARM      *OFF          $Esito
059100950616     C                   PARM      'ND002DS'     $Struttura
059200950616     C                   PARM                    ND002DS
059300950616     C                   PARM                    $LenOut
059400950616     C*
059500950616     C                   ENDSR
059600950405     C/EJECT
059700130618>>>>> ************************************************************
059800130618>>>>> * Reperimento dati società da porti assegnati
059900130618>>>>> ************************************************************
060000130618>>>>>C     RepEstNomP    BEGSR
060100130618>>>>> *
060200130618>>>>> * prepara chiave per reperimento decodifica,se esiste,
060300130618>>>>> * da file porti assegnati
060400130618>>>>> *
060500130618>>>>>C                   Clear                   NDCPADS
060600130618>>>>>C                   Eval      LenOut = %Size(NDCPADS)
060700130618>>>>>C*
060800130618>>>>>C                   CALL      'NDC066R2'
060900130618>>>>>C                   PARM      movsys        Sys
061000130618>>>>>C                   PARM      0             NrAsInt
061100130618>>>>>C                   PARM      movnrasreg    NrAsGen
061200130618>>>>>C                   PARM                    LenOut
061300130618>>>>>C                   PARM                    NDCPADS
061400130618>>>>>C                   PARM      '0'           Esito
061500130618>>>>>C*
061600130618>>>>>C                   If        Esito = '0'
061700130618>>>>>c                   movel     cpadescr      p1descr
061800130618>>>>>c                   endif
061900130618>>>>>C*
062000130618>>>>>C                   ENDSR
062100990421     C************************************************************
062200990421     C* DATI ANAGRAFICI SOGGETTO
062300990421     C************************************************************
062400990421     C     CALLDVASOG    BEGSR
062500990421     C*
062600990421     C                   move      '1'           DVARIC
062700990421     C                   move      movsocieta    DVASOCIETA
062800990421     C                   move      *blank        DVAUNITA
062900990421     C                   movel     'DVASOG'      DVASTRUTT
063000990421     C                   eval      DVALENOUT = %Size(DVASOG)
063100990421     C                   move      sogg002       DVASOGG
063200990421     C*
063300990421     C                   CALLB     'NDDVASOG'    PDVASOG
063400990421     C*
063500990421     C                   ENDSR
063600990421     C/EJECT
063700990421     C************************************************************
063800990421     C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
063900990421     C************************************************************
064000990421     C     CALLDVACPI    BEGSR
064100990421     C*
064200990421     C                   move      '2'           DVCRIC
064300990421     C                   move      movsocieta    DVCSOCIETA
064400990421     C                   move      *blank        DVCUNITA
064500990421     C                   movel     'ANCPI'       DVCSTRUTT
064600020905D1193C     snatura002    Ifeq      '2'
064700020905D1193C                   move      movdtreg      DVCDTRIF
064800020905D1193C                   Else
064900990421     C                   move      movdtdoc      DVCDTRIF
065000020905D1193C                   EndIf
065100990421     C                   eval      DVCLENOUT = %Size(ANCPI)
065200990421     C                   move      sogg002       DVCSOGG
065300990421     C*
065400990421     C                   CALLB     'NDDVACPI'    PDVACPI
065500990421     C*
065600990421     C                   ENDSR
065700990421     C/EJECT
065800941103      ************************************************************
065900941103      * Reperimento dati società
066000941103      ************************************************************
066100941103     C     REPSOC        BEGSR
066200941103      *
066300960926     C                   CALLB     'XSOC'
066400941103     C                   PARM                    TIPXSC            6
066500941103     C                   PARM                    SOCXSC            3
066600941103     C                   PARM                    CDSXSC            9 0
066700941103     C                   PARM                    MODXSC            3
066800941103     C                   PARM      *BLANKS       RTNXSC            1
066900941103     C                   PARM                    XSOCDS
067000941103     C                   PARM                    KPJBA
067100941103      *
067200941103     C                   ENDSR
067300011211     C/EJECT
067400011211      ************************************************************
067500011211C1529 * STAMPA TESTATA
067600011211      ************************************************************
067700011211     C     StampaTes     BEGSR
067800011211     c*
067900011211     C* se richiesto stampo prima intestazione memorizzata sul
068000011211     C* parametro modulo 'BOLLATI'
068100011211     C                   if        int103=*on
068200011211     c                   eval      T0riga1 =rg1099
068300011211     c* anno/pagina
068400030218c1615x***  anno099       ifne      *blanks
068500030218  "  x*                  eval      %subst(T0riga1:122:4) = anno099
068600030218  "  x*                  eval      %subst(T0riga1:126:1) = '/'
068700030218  "  x*                  endif
068800030218  "  x*                  move      NumeroPag     T0riga1
068900030218  "  c                   move      numeropag     numpag            6
069000030218C1615C                   eval      P1AnnoPag= Anno099+ '/' + numpag
069100011211     c                   eval      T0riga2 =rg2099
069200011211     c                   eval      T0riga3 =rg3099
069300011211     c                   eval      T0riga4 =rg4099
069400011211     c                   WRITE     C105T0
069500011211     C                   ADD       1             NumeroPag
069600011211     c                   endif
069700011211     C*
069800100528c2161c                   if        SifArcSos =*on
069900100528  "  c                   WRITE     C105T1X
070000100528c2161c                   else
070100011211     c                   WRITE     C105T1
070200100528c2161c                   endif
070300011211     C*
070400011211C1529C                   ENDSR
070500950606     C/EJECT
070600950606      ************************************************************
070700950614      * OVERFLOW per righe di dettaglio
070800950606      ************************************************************
070900950606     C     OVER          BEGSR
071000950615     C*
071100011211c1529x***                WRITE     C105T1
071200011211c1529c                   exsr      StampaTes
071300950606     C*
071400950614     C* stampo progressivi al salto pagina
071500950614     C*
071600950614     C                   Z-ADD     PRGDARE       P6DARE
071700950614     C                   Z-ADD     PRGAVERE      P6AVERE
071800950614     C                   WRITE     C105P6
071900950614     C*
072000950606     C                   SETOFF                                       44
072100950606     C*
072200950606     C                   ENDSR
072300950614     C/EJECT
072400950614      ************************************************************
072500950614      * REPDAT  - reperisce ultima data giornale bollato
072600950614      * e alloca il record per aggiornarla
072700950614      ************************************************************
072800950614     C     REPDAT        BEGSR
072900950614     C*
073000950614     C                   MOVE      '1'           XDTRIC
073100950614     C                   MOVE      XSCSOC        XDTSOC
073200950614     C                   MOVE      *blanks       XDTUNI
073300950614     C                   MOVE      CTB103        XDTCTB
073400950614     C                   MOVE      *BLANKS       XDTKEY
073500950630     C                   MOVE      'GIOBOL'      XDTTIP
073600950614     C                   MOVE      *OFF          XDTCMT
073700950614     C                   MOVE      *ON           XDTALC
073800950614     C*
073900960926     C                   CALLB     'XDATR'       PDAT
074000950614     C     XDTERR        IFEQ      *ON
074100950614     C                   MOVE      *ON           ERR103
074200950614     C                   EXSR      ENDPGM
074300950614     C                   ENDIF
074400950614     C*
074500950614     C                   ENDSR
074600950614      ************************************************************
074700950614      * REPMEL
074800950614      ************************************************************
074900950614     C     REPMEL        BEGSR
075000950614     C*
075100950614      *
075200950614      * Reperisce ultima riga progressivi bollato
075300950614      *
075400950614     C                   MOVE      '1'           XMLRIC
075500950614     C                   MOVE      XSCSOC        XMLSOC
075600950614     C                   MOVE      *blanks       XMLUNI
075700950614     C                   MOVE      CTB103        XMLCTB
075800950614     C                   MOVE      *BLANKS       XMLKEY
075900950614     C                   MOVE      'GIOBOL'      XMLTIP
076000030908D1435c                   if        std103 = *on
076100950614     C                   MOVE      XDTDAT        XMLDAT
076200030908D1435c                   else
076300030908 "   C* se provvisorio reperisce i progressivi relativi alla data prec.
076400030908 "   c     DtI103        subdur    1:*D          DtfPrec
076500030908 "   C                   MOVE      DtfPrec       XMLDAT
076600030908D1435c                   endif
076700950614     C                   MOVE      *OFF          XMLCMT
076800950614     C                   CLEAR                   XMLSTR
076900950614     C*
077000960926     C                   CALL      'XMELR'       PMEL
077100950614     C     XMLERR        IFEQ      *OFF
077200100528C2161C                   IF        %subst(XMLSTR:55:7) = *BLANKS
077300100528  "  C                   EVAL      XMLSTR = %subst(XMLSTR:1:54)+
077400100528  "  C                                          '0000000'
077500100528C2161C                   ENDIF
077600950614     C                   MOVEL     XMLSTR        GIOBDS
077700970807     c* azzero i progressivi se richiesto dal lancio
077800970807     c                   if        azzera103 = *on
077900960705     c                   clear                   pdare
078000960705     c                   clear                   pavere
078100970711     c                   clear                   uriga
078200100528C2161c                   clear                   uregi
078300960705     c                   endif
078400950614     C                   ELSE
078500950614     C                   CLEAR                   GIOBDS
078600950614     C                   ENDIF
078700950614     C*
078800950614     C                   ENDSR
078900950614     C/EJECT
079000950614      ************************************************************
079100950614      * AGGDAT  - aggiorna data giornale bollato
079200950614      ************************************************************
079300950614     C     AGGDAT        BEGSR
079400950614     C*
079500950614     C                   MOVE      '4'           XDTRIC
079600950614     C                   MOVE      XSCSOC        XDTSOC
079700950614     C                   MOVE      *blanks       XDTUNI
079800950614     C                   MOVE      CTB103        XDTCTB
079900950614     C                   MOVE      *BLANKS       XDTKEY
080000950630     C                   MOVE      'GIOBOL'      XDTTIP
080100950614     C                   MOVE      *OFF          XDTCMT
080200950614     C                   MOVE      *ON           XDTALC
080300950614     C                   MOVE      DTF103        XDTDAT
080400950614     C*
080500960926     C                   CALLB     'XDATR'       PDAT
080600950614     C*
080700950614     C                   ENDSR
080800950614      ************************************************************
080900950614      * AGGMEL
081000950614      ************************************************************
081100950614     C     AGGMEL        BEGSR
081200950614     C*
081300950614      *
081400950614      * Scrive  ultima riga progressivi bollato
081500950614      *
081600950614     C                   MOVE      '4'           XMLRIC
081700950614     C                   MOVE      XSCSOC        XMLSOC
081800950614     C                   MOVE      *blanks       XMLUNI
081900950614     C                   MOVE      CTB103        XMLCTB
082000950614     C                   MOVE      *BLANKS       XMLKEY
082100950614     C                   MOVE      'GIOBOL'      XMLTIP
082200950614     C                   MOVE      DTF103        XMLDAT
082300950614     C                   MOVE      *OFF          XMLCMT
082400950614     C                   MOVE      XSCCUT        UTENTE
082500950614     C                   Z-ADD     PRGDARE       PDARE
082600950614     C                   Z-ADD     PRGAVERE      PAVERE
082700000626D0645 * Memorizzo l'incrementale raggiunto anzichè il campo del printer
082800000626  "   * file perchè quest'ultimo potrebbe valere zero, se nel periodo
082900000626  "   * di cui è richiesta la stampa del giornale bollato non sono
083000000626  "   * state trovate registrazioni
083100000626  "  X***                Z-ADD     P1NRLGIO      URIGA
083200000626D0645C                   Z-ADD     NrRiga        URiga
083300100528C2161C                   Z-ADD     NrRegi        URegi
083400950614     C                   MOVEL     GIOBDS        XMLSTR
083500950614     C*
083600960926     C                   CALL      'XMELR'       PMEL
083700950614     C*
083800950614     C                   ENDSR
083900950614     C*
084000950614     C/EJECT
084100950614     C************************************************************
084200950614     C* RICHIAMA PGM stampa lista causali
084300950614     C************************************************************
084400950614     C     staliscau     BEGSR
084500020124     c*
084600020124c1529c                   exsr      IntestaCau
084700950614     C*
084800950614     C                   WRITE     C106TS
084900991112     C                   SetOff                                       44
085000950614     C*
085100950614     C                   MOVE      SOC103        OPESOCIETA
085200950614     C*
085300950614     C     OPESOCIETA    SETLL     ANOPE01L
085400950614     C                   DO        *HIVAL
085500950614     C     OPESOCIETA    READE     ANOPE01L                               21
085600950614     C   21              LEAVE
085700950614     C                   ADD       1             $CT               1 0
085800950614     C     $ct           ifeq      4
085900950614     C                   CLEAR                   C106P1
086000950614     C                   Z-add     1             $ct
086100950614     C                   ENDIF
086200950614     C                   SELECT
086300950614     C     $ct           WHENEQ    1
086400950614     C                   EVAL      P1cau1 =opecausale
086500950614     C                   EVAL      P1des1 =opedescr
086600950614     C     $ct           WHENEQ    2
086700950614     C                   EVAL      P1cau2 =opecausale
086800950614     C                   EVAL      P1DES2 =opedescr
086900950614     C     $ct           WHENEQ    3
087000950614     C                   EVAL      P1cau3 =opecausale
087100950614     C                   EVAL      P1DES3 =opedescr
087200950614     C*
087300020124c1529c   44              exsr      IntestaCau
087400950614     C   44              WRITE     C106TS
087500950614     C   44              SETOFF                                       44
087600950614     C                   WRITE     C106P1
087700950614     C*
087800950614     C                   ENDSL
087900950614     C*
088000950614   E1C                   ENDDO
088100950615     C* stampa ultima riga se non stampata
088200950615     C     $ct           ifne      3
088300060803D2032c   44              exsr      IntestaCau
088400950615     C   44              WRITE     C106TS
088500950614     C   44              SETOFF                                       44
088600950614     C                   WRITE     C106P1
088700950615     c                   endif
088800950614     C*
088900950614     C                   ENDSR
089000981124     C/EJECT
089100020124c1529c*
089200020124 "   C************************************************************
089300020124 "   C* intestazione causali da bollato
089400020124 "   C************************************************************
089500020124 "   C     IntestaCau    BEGSR
089600020124 "   c*
089700020124 "   C* se richiesto stampo prima intestazione memorizzata sul
089800020124 "   C* parametro modulo 'BOLLATI'
089900020124 "   C                   if        int103=*on
090000020124 "   c                   eval      T0riga1 =rg1099
090100030218c1529c* anno/pagina
090200030218c1615x***  anno099       ifne      *blanks
090300030218  "  x*                  eval      %subst(T0riga1:122:4) = anno099
090400030218  "  x*                  eval      %subst(T0riga1:126:1) = '/'
090500030218  "  x*                  endif
090600030218  "  x*                  move      NumeroPag     T0riga1
090700030218  "  c                   move      numeropag     numpag            6
090800030218C1615C                   eval      P1AnnoPag= Anno099+ '/' + numpag
090900030218c1529c                   eval      T0riga2 =rg2099
091000020124 "   c                   eval      T0riga3 =rg3099
091100020124 "   c                   eval      T0riga4 =rg4099
091200020124 "   c                   WRITE     C105T0
091300020124 "   C                   ADD       1             NumeroPag
091400020124 "   c                   endif
091500020124 "   c*
091600020124c1529c                   endsr
091700981124     C*----------------------------------------------------------
091800981124     C* chiamata ad X32 per eseguire ovrprtf
091900981124     C*----------------------------------------------------------
092000981124     C     CallX32       BEGSR
092100981124     c
092200981124     c                   eval      x32Societa = Soc103
092300991108     c                   eval      x32CdStamp = 'GIO'
092400981124     c                   eval      x32Unita   = Uni103
092500991108     c                   eval      x32KeyGest = *blanks
092600981124     c                   eval      x32Errore  = *off
092700981124     C                   call      'X32OVRPRTF'
092800981124     C                   PARM                    X32societa        3
092900981124     C                   PARM                    X32CdStamp        3
093000981124     C                   PARM                    X32Unita          8
093100981124     C                   PARM                    X32KeyGest       12
093200981124     C                   PARM                    X32Errore         1
093300991105B0001C                   PARM                    KPJBA
093400991105  "  C*** Se non trova tabella G75 errore
093500991104  "  c***                   if        X32Errore = *on
093600991104  "  C***                   MOVE      *ON           ERR103
093700991104  "  c***                   exsr      endpgm
093800991104  "  C***                   ELSE
093900991104B0001c* Apro comunque il file
094000981124     C                   OPEN      NDGIOPRT
094100991104B0001c***                endif
094200981124     c*
094300981124     c                   ENDSR
094400981124     C/EJECT
094500950614     C************************************************************
094600950614     C* Definizioni e inizializzazioni
094700950614     C************************************************************
094800950614     C     *INZSR        BEGSR
094900000118C1309C     *DMY          Move      Udate         ZDateISO
095000000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
095100950614     C*
095200950614     C     *ENTRY        PLIST                                                  riceve
095300950614     C                   PARM                    KPJBA                          parametro
095400950614     C*
095500950614     C* KLIST PER NDREG
095600950614     C     K3REG15       KLIST
095700950614     C                   KFLD                    REGSOCIETA
095800950614     C                   KFLD                    REGCTB
095900950614     C                   KFLD                    REGTPREGZ
096000950614     C     K4REG15       KLIST
096100950614     C                   KFLD                    REGSOCIETA
096200950614     C                   KFLD                    REGCTB
096300950614     C                   KFLD                    REGTPREGZ
096400950614     C                   KFLD                    REGDTLGIO
096500950614     C*
096600950614     C* KLIST PER NDMOV
096700950614     C     K2MOV01       KLIST
096800950614     C                   KFLD                    MOVSYS
096900950614     C                   KFLD                    MOVNRASREG
097000951102     C*
097100951102     C* KLIST PER NDDES
097200951102     C     K04Des01      KLIST
097300951102     C                   KFLD                    DESSYS
097400951102     C                   KFLD                    DESNRASREG
097500951102     C                   KFLD                    DESTipDes
097600951102     C                   KFLD                    DESNRRigaM
097700950614     C*
097800950614     C* KLIST PER ANOPE
097900950614     C     K2OPE01       KLIST
098000950614     C                   KFLD                    OPESOCIETA
098100950614     C                   KFLD                    OPECAUSALE
098200950614      *
098300990421      * Plist x XDAT
098400950614      *
098500950614     C     PDAT          PLIST
098600950614     C                   PARM                    XDTRIC            1
098700950614     C                   PARM                    XDTSOC            3
098800950614     C                   PARM                    XDTUNI            8
098900950614     C                   PARM                    XDTCTB            2
099000950614     C                   PARM                    XDTKEY            8
099100950614     C                   PARM                    XDTALC            1
099200950614     C                   PARM                    XDTCMT            1
099300950614     C                   PARM                    XDTDAT
099400950614     C                   PARM                    XDTTIP            6
099500950614     C                   PARM                    XDTERR            1
099600950614     C                   PARM                    XDTTST            1
099700950614      *
099800990421      * Plist x XMEL
099900950614      *
100000950614     C     PMEL          PLIST
100100950614     C                   PARM                    XMLRIC            1
100200950614     C                   PARM                    XMLSOC            3
100300950614     C                   PARM                    XMLUNI            8
100400950614     C                   PARM                    XMLCTB            2
100500950614     C                   PARM                    XMLKEY            8
100600950614     C                   PARM                    XMLALC            1
100700950614     C                   PARM                    XMLCMT            1
100800950614     C                   PARM                    XMLDAT
100900950614     C                   PARM                    XMLTIP            6
101000950614     C                   PARM                    XMLERR            1
101100950614     C                   PARM                    XMLSTR
101200990421      *
101300990421      * Plist x NDVASOG
101400990421      *
101500990421     C     PDVASOG       PLIST
101600990421     C                   PARM                    DVARIC
101700990421     C                   PARM                    DVASOCIETA
101800990421     C                   PARM                    DVAUNITA
101900990421     C                   PARM                    DVASTRUTT
102000990421     C                   PARM                    DVADTRIF
102100990421     C                   PARM                    DVALENOUT
102200990421     C                   PARM                    *OMIT
102300990421     C                   PARM                    *OMIT
102400990421     C                   PARM                    *OMIT
102500990421     C                   PARM                    *OMIT
102600990421     C                   PARM                    DVASOGG
102700990421     C                   PARM                    DVATPIND
102800990421     C                   PARM                    DVACDIND
102900990421     C                   PARM                    DVAERRORE
103000990421     C                   PARM                    DVAOUTPUT
103100990421      *
103200990421      * Plist x NDDVACPI
103300990421      *
103400990421     C     PDVACPI       PLIST
103500990421     C                   PARM                    DVCRIC
103600990421     C                   PARM                    DVCSOCIETA
103700990421     C                   PARM                    DVCUNITA
103800990421     C                   PARM                    DVCSTRUTT
103900990421     C                   PARM                    DVCDTRIF
104000990421     C                   PARM                    DVCLENOUT
104100990421     C                   PARM                    DVCSOGG
104200990421     C                   PARM                    DVCERRORE
104300990421     C                   PARM                    DVCOUTPUT
104400950614     C*
104500950614     C                   ENDSR
104600950614     C*
104700950614     ONDMOV000  E            AGGMOV
104800950614     O                       MOVNRLGIO
104900950614     O                       MOVDESBREV
105000100528C2161ONDREG000  E            AGGREG
105100100528C2161O                       REGENTIT3
105101161129>>>>>
105102161129>>>>>  // --------------------------------------------------
105103161129>>>>>  // Procedure name: SendToDocFlow
105104161129>>>>>  // Purpose:        Invia lo spool a DocFlow
105105161129>>>>>  // Returns:        Esito.
105106161129>>>>>  // --------------------------------------------------
105107161129>>>>>
105108161129>>>>>  DCL-PROC SendToDocFlow ;
105109161129>>>>>
105110161129>>>>>    DCL-PI *N INT(10);
105111161129>>>>>    END-PI ;
105112161129>>>>>
105113161130>>>>>    DCL-DS local QUALIFIED INZ;
105114161129>>>>>      aplKey CHAR(15) INZ('PJGIORNALE');
105115161129>>>>>      filNam CHAR(10) INZ('NDGIOPRT');
105116161129>>>>>      numFil CHAR(6);
105117170721>>>>>      fileOut CHAR(255) INZ('GIOR');
105118161129>>>>>      subFldr CHAR(50) INZ('GIOR');
105119161129>>>>>      esito CHAR(1);
105125161129>>>>>    END-DS;
105126161129>>>>>
105127161129>>>>>    // Non creo il file per DocFlow quando non è una stampa definitiva.
105128161129>>>>>
105129161129>>>>>    IF std103 = *OFF;
105130161129>>>>>      RETURN *ZERO;
105131161129>>>>>    ENDIF;
105132161129>>>>>
105143161129>>>>>    local.numFil = %EDITC(%DEC(yNdGioPrtInf.spoolNum:6:0):'X');
105148161129>>>>>
105149161129>>>>>    local.fileOut = %TRIMR(local.fileOut) + soc103 + %CHAR(dtf103)
105150161129>>>>>                  + '.spl';
105151161129>>>>>
105152161129>>>>>    CLOSE ndGioPrt;
105153161129>>>>>
105154161129>>>>>    TRBM_SendSpoolFileToIfs( local.aplKey
105155161129>>>>>                           : *BLANK
105156161129>>>>>                           : local.filNam
105157161129>>>>>                           : psJobName : psJobUser : psJobNbr
105158161129>>>>>                           : local.numFil
105159161129>>>>>                           : *BLANK
105160161129>>>>>                           : local.fileOut
105161161129>>>>>                           : local.subFldr
105162161129>>>>>                           : 'S'
105163161129>>>>>                           : 'S'
105164161129>>>>>                           : 'S'
105165161129>>>>>                           : local.esito
105166161129>>>>>                           );
105167161129>>>>>
105168161129>>>>>    IF local.esito = '2';
105169161129>>>>>      DUMP(A);
105179161129>>>>>      RETURN -1;
105180161129>>>>>    ENDIF;
105181161129>>>>>
105182161129>>>>>    RETURN *ZERO;
105183161129>>>>>
105184161129>>>>>  END-PROC ;
105185161129>>>>>
