000100020916      /TITLE OUTQ per invio stampe da sede a PO.
000200020917     ***********************************************************************
000300020917     **
000400020920     ** Questo programma riceve il codice PO e il tipo coda da reperire
000500020920     ** e reperisce dall'organigramma aziendale la coda di stampa remota.
000600020917     **
000700020917     ** PARAMETRI DI INPUT:
000800020920     **
000900020920     ** PrmFine     Tipo fine programma:
001000020920     **             ' ' = vale 'R'.
001100020920     **             'R' = RETURN
001200020920     **             'L' = LR
001300020920     **
001400020917     ** PrmFil      Codice PO (obbligatorio).
001500020920     **
001600020919     ** PrmTpOutQ   Tipo coda:
001700020920     **             ' ' = vale '0'.
001800020919     **             '0' = remota generica.
001900020919     **             '1' = PO amministrazione.
002000020919     **             '2' = Fax in.
002100020919     **             '3' = Fax out.
002200020919     **             '4' = Lettere vettura per stampa in arrivo.
002300050707     **             '5' = PO Commerciale
002400020917     **
002500020917     ** PARAMETRI DI OUTPUT:
002600020920     **
002700020917     ** PrmOutQ     Nome coda di stampa.
002800020917     ** PrmOutQLib  Libreria coda di stampa (sempre *LIBL).
002900020917     ** PrmEsito    '0'=eseguito; '1'=errori.
003000020917     **
003100020917     ***********************************************************************
003200020916
003300020916     H DFTACTGRP(*NO) ACTGRP(*CALLER)
003400020920     H OPTION(*NODEBUGIO) OPTIMIZE(*FULL)
003500020916
003600020916     FAzOrg01L  IF   E           K DISK
003700020916
003800020920     D Return          C                   'R'
003900020920     D LR              C                   'L'
004000020916     D Eseguito        C                   '0'
004100020916     D Errore          C                   '1'
004200020919     D Generica        C                   '0'
004300020919     D POAmministr     C                   '1'
004400020919     D FaxIn           C                   '2'
004500020919     D FaxOut          C                   '3'
004600050707     D Commerciale     C                   '5'
004700040921     D*LetVettura      C                   '4'
004800020919
004900020920     D PrmFine         S              1
005000020919     D PrmFil          S              3S 0
005100020919     D PrmTpOutQ       S              1
005200020919     D PrmOutQ         S             10
005300020919     D PrmOutQLib      S             10
005400020919     D PrmEsito        S              1
005500020916
005600050708     D OG141Ptr        S               *
005700050708     D                                     INZ(%ADDR(OrgDE1))
005800050708     D OG141         E DS
005900050708     D                                     BASED(OG141Ptr)
006000020919
006100020919     D OG142Ptr        S               *
006200020919     D                                     INZ(%ADDR(OrgDE2))
006300020919     D OG142         E DS
006400020919     D                                     BASED(OG142Ptr)
006500020919
006600020919     D OG144Ptr        S               *
006700020919     D                                     INZ(%ADDR(OrgDE4))
006800020919     D OG144         E DS
006900020919     D                                     BASED(OG144Ptr)
007000020919
007100020919     D Trul26DS      E DS
007200020919     D                                     INZ
007300020919     D  Lin                           3  0
007400020919     D                                     DIM(250)
007500020919     D                                     OVERLAY(O26Lin)
007600020919     D  Tfe                           3  0
007700020919     D                                     DIM(250)
007800020919     D                                     OVERLAY(O26Tfe)
007900020918
008000020919     D PO0             S                                                        Codici PO elaborati
008100020916     D                                     LIKE(OrgFil)
008200020916     D                                     DIM(999)
008300020919     D PO1             S                                                        Codici PO elaborati
008400020919     D                                     LIKE(OrgFil)
008500020919     D                                     DIM(999)
008600020919     D PO2             S                                                        Codici PO elaborati
008700020919     D                                     LIKE(OrgFil)
008800020919     D                                     DIM(999)
008900020919     D PO3             S                                                        Codici PO elaborati
009000020919     D                                     LIKE(OrgFil)
009100020919     D                                     DIM(999)
009200020919     D PO4             S                                                        Codici PO elaborati
009300020919     D                                     LIKE(OrgFil)
009400020919     D                                     DIM(999)
009500050708     D PO5             S                                                        Codici PO elaborati
009600050708     D                                     LIKE(OrgFil)
009700050708     D                                     DIM(999)
009800020920
009900020919     D OQ0             S             10                                         OUTQ generiche
010000020916     D                                     DIM(999)
010100020919     D OQ1             S             10                                         OUTQ amministrativ.
010200020916     D                                     DIM(999)
010300020919     D OQ2             S             10                                         OUTQ fax in
010400020919     D                                     DIM(999)
010500020919     D OQ3             S             10                                         OUTQ fax out
010600020919     D                                     DIM(999)
010700020919     D OQ4             S             10                                         OUTQ lettere vettura
010800020919     D                                     DIM(999)
010900050708     D OQ5             S             10                                         OUTQ commerciale
011000050708     D                                     DIM(999)
011100020920
011200020920     D L               S              5I 0
011300020920     D P               S              5I 0
011400020916
011500020916     C     *ENTRY        PLIST
011600020917     ** Parametri di input.
011700020920     C                   PARM                    PrmFine
011800020916     C                   PARM                    PrmFil
011900020919     C                   PARM                    PrmTpOutQ
012000020917     ** Parametri di output.
012100020916     C                   PARM                    PrmOutQ
012200020917     C                   PARM                    PrmOutQLib
012300020916     C                   PARM                    PrmEsito
012400020916
012500020916     C                   EXSR      ChkParm
012600020916
012700020919     C                   EXSR      SetOutQ
012800020916
012900020916     C                   EXSR      Uscita
013000020916
013100020916     ***********************************************************************
013200020916     **
013300020919     ** Imposto coda di stampa.
013400020916     **
013500020916     ***********************************************************************
013600020919     C     SetOutQ       BEGSR
013700020920
013800020920     ** Reperisco le schiere LIN e TFE.
013900020920     C                   EXSR      RtvLinTfe
014000020919
014100020916     C                   SELECT
014200020920     C                   WHEN      PrmTpOutQ = Generica
014300020920     C                   EXSR      RtvOutQ0
014400020919     C                   WHEN      PrmTpOutQ = POAmministr
014500020919     C                   EXSR      RtvOutQ1
014600020919     C                   WHEN      PrmTpOutQ = FaxIn
014700020919     C                   EXSR      RtvOutQ2
014800020919     C                   WHEN      PrmTpOutQ = FaxOut
014900020919     C                   EXSR      RtvOutQ3
015000040921     C**                 WHEN      PrmTpOutQ = LetVettura
015100040921     C**                 EXSR      RtvOutQ4
015200050707     C                   WHEN      PrmTpOutQ = Commerciale
015300050707     C                   EXSR      RtvOutQ5
015400020916     C                   ENDSL
015500020917
015600020920     C                   IF        PrmOutQ <> *BLANK
015700020918     C                   EVAL      PrmOutQLib = '*LIBL'
015800020920     C                   ENDIF
015900020916
016000020916     C                   ENDSR
016100020919
016200020919     ***********************************************************************
016300020919     **
016400020919     ** Reperisco coda di stampa generica.
016500020919     **
016600020919     ***********************************************************************
016700020919     C     RtvOutQ0      BEGSR
016800020919
016900020920     C                   IF        PO0(P) > 0
017000020919
017100020920     C                   EVAL      PrmOutQ = OQ0(P)
017200020919
017300020919     C                   ELSE
017400020919
017500020920     C                   IF        OrgFil <> PrmFil
017600020919     C                   EVAL      OrgFil = PrmFil
017700020919     C                   EXSR      RtvAzOrg
017800020920     C                   ENDIF
017900020919
018000020919     C                   IF        %FOUND(AzOrg01L)
018100020920     C                   EVAL      OQ0(P) = §OGSSP
018200020920     C                   EVAL      PO0(P) = PrmFil
018300020920     C                   EVAL      PrmOutQ = OQ0(P)
018400020919     C                   ENDIF
018500020919
018600020919     C                   ENDIF
018700020919
018800020919     C                   ENDSR
018900020919
019000020919     ***********************************************************************
019100020919     **
019200020919     ** Reperisco coda di stampa PO amministrazione.
019300020919     **
019400020919     ***********************************************************************
019500020919     C     RtvOutQ1      BEGSR
019600020919
019700020920     C                   IF        PO1(P) > 0
019800020919
019900020920     C                   EVAL      PrmOutQ = OQ1(P)
020000020919
020100020919     C                   ELSE
020200020919
020300020920     ** Reperisco il PO dell'amministrazione.
020400020920     C                   IF        OrgFil <> PrmFil
020500020920     C                   EVAL      OrgFil = PrmFil
020600020920     C                   EXSR      RtvAzOrg
020700020920     C                   ENDIF
020800020920
020900020919     C                   IF        OrgFC0 <> OrgFil
021000020919     C                             AND
021100020919     C                             OrgFC0 <> 0
021200020920     C                             AND
021300020920     C                             %FOUND(AzOrg01L)
021400020919     C                   EVAL      OrgFil = OrgFC0
021500020919     C                   EXSR      RtvAzOrg
021600020920     C                   ENDIF
021700020919
021800020919     C                   IF        %FOUND(AzOrg01L)
021900020920     C                   EVAL      OQ1(P) = §OGSSP
022000020920     C                   EVAL      PO1(P) = PrmFil
022100020920     C                   EVAL      PrmOutQ = OQ1(P)
022200020919     C                   ENDIF
022300020919
022400020919     C                   ENDIF
022500020919
022600020919     C                   ENDSR
022700020919
022800020919     ***********************************************************************
022900020919     **
023000020919     ** Reperisco coda di stampa fax in.
023100020919     **
023200020919     ***********************************************************************
023300020919     C     RtvOutQ2      BEGSR
023400020919
023500020920     C                   IF        PO2(P) > 0
023600020919
023700020920     C                   EVAL      PrmOutQ = OQ2(P)
023800020919
023900020919     C                   ELSE
024000020919
024100020919     C                   EVAL      L = 1
024200020919     C     PrmFil        LOOKUP    Lin(L)                                 01
024300020919
024400020919     C                   IF        %EQUAL
024500020919
024600020920     C                   IF        OrgFil <> Tfe(L)
024700020919     C                   EVAL      OrgFil = Tfe(L)
024800020919     C                   EXSR      RtvAzOrg
024900020920     C                   ENDIF
025000020919
025100020919     C                   IF        %FOUND(AzOrg01L)
025200020920     C                   EVAL      OQ2(P) = §OGFxI
025300020920     C                   EVAL      PO2(P) = PrmFil
025400020920     C                   EVAL      PrmOutQ = OQ2(P)
025500020919     C                   ENDIF
025600020919
025700020919     C                   ENDIF
025800020919
025900020919     C                   ENDIF
026000020919
026100020919     C                   ENDSR
026200020919
026300020919     ***********************************************************************
026400020919     **
026500020919     ** Reperisco coda di stampa fax out.
026600020919     **
026700020919     ***********************************************************************
026800020919     C     RtvOutQ3      BEGSR
026900020919
027000020920     C                   IF        PO3(P) > 0
027100020919
027200020920     C                   EVAL      PrmOutQ = OQ3(P)
027300020919
027400020919     C                   ELSE
027500020919
027600020919     C                   EVAL      L = 1
027700020919     C     PrmFil        LOOKUP    Lin(L)                                 01
027800020919
027900020919     C                   IF        %EQUAL
028000020919
028100020920     C                   IF        OrgFil <> Tfe(L)
028200020919     C                   EVAL      OrgFil = Tfe(L)
028300020919     C                   EXSR      RtvAzOrg
028400020920     C                   ENDIF
028500020919
028600020919     C                   IF        %FOUND(AzOrg01L)
028700020920     C                   EVAL      OQ3(P) = §OGFxO
028800020920     C                   EVAL      PO3(P) = PrmFil
028900020920     C                   EVAL      PrmOutQ = OQ3(P)
029000020919     C                   ENDIF
029100020919
029200020919     C                   ENDIF
029300020919
029400020919     C                   ENDIF
029500020919
029600020919     C                   ENDSR
029700020919
029800020919     ***********************************************************************
029900020919     **
030000020919     ** Reperisco coda di stampa lettere vettura.
030100020919     **
030200020919     ***********************************************************************
030300040921     C***  RtvOutQ4      BEGSR
030400040921      ***
030500040921     C***                IF        PO4(P) > 0
030600040921      ***
030700040921     C***                EVAL      PrmOutQ = OQ4(P)
030800040921      ***
030900040921     C***                ELSE
031000040921      ***
031100040921     C***                EVAL      L = 1
031200040921     C***  PrmFil        LOOKUP    Lin(L)                                 01
031300040921      ***
031400040921     C***                IF        %EQUAL
031500040921      ***
031600040921     C***                IF        OrgFil <> Tfe(L)
031700040921     C***                EVAL      OrgFil = Tfe(L)
031800040921     C***                EXSR      RtvAzOrg
031900040921     C***                ENDIF
032000040921      ***
032100040921     C***                IF        %FOUND(AzOrg01L)
032200040921     C***                EVAL      OQ4(P) = §OGOFQ
032300040921     C***                EVAL      PO4(P) = PrmFil
032400040921     C***                EVAL      PrmOutQ = OQ4(P)
032500040921     C***                ENDIF
032600040921      ***
032700040921     C***                ENDIF
032800040921      ***
032900040921     C***                ENDIF
033000040921      ***
033100040921     C***                ENDSR
033200040921      ***
033300050708
033400050708     ***********************************************************************
033500050708     **
033600050708     ** Reperisco coda di stampa PO commerciale
033700050708     **
033800050708     ***********************************************************************
033900050708     C     RtvOutQ5      BEGSR
034000050708
034100050708     C                   IF        PO5(P) > 0
034200050708
034300050708     C                   EVAL      PrmOutQ = OQ5(P)
034400050708
034500050708     C                   ELSE
034600050708
034700050708     C                   IF        OrgFil <> PrmFil
034800050708     C                   EVAL      OrgFil = PrmFil
034900050708     C                   EXSR      RtvAzOrg
035000050708     C                   ENDIF
035100050708
035200050708     C                   IF        %FOUND(AzOrg01L)
035300050708     C                   EVAL      OQ5(P) = §OGSSC
035400050708     C                   EVAL      PO5(P) = PrmFil
035500050708     C                   EVAL      PrmOutQ = OQ5(P)
035600050708     C                   ENDIF
035700050708
035800050708     C                   ENDIF
035900050708
036000050708     C                   ENDSR
036100050708
036200020916     ***********************************************************************
036300020916     **
036400020916     ** Reperisco organigramma aziendale.
036500020916     **
036600020916     ***********************************************************************
036700020916     C     RtvAzOrg      BEGSR
036800020916
036900020920     C     k01Org01      CHAIN     AzOrg01L
037000020920
037100020920     C                   IF        NOT %FOUND
037200020920     C                   EVAL      PrmEsito = Errore
037300020920     C                   EXSR      Uscita
037400020920     C                   ENDIF
037500020916
037600020916     C                   ENDSR
037700020916
037800020916     ***********************************************************************
037900020916     **
038000020916     ** Controllo parametri ricevuti.
038100020916     **
038200020916     ***********************************************************************
038300020916     C     ChkParm       BEGSR
038400020916
038500020916     C                   CLEAR                   PrmOutQ
038600020917     C                   CLEAR                   PrmOutQLib
038700020916     C                   CLEAR                   PrmEsito
038800020916
038900020919     ** Il codice PO è obbligatorio.
039000020918     C                   IF        PrmFil > 0
039100020920     C                   EVAL      P = PrmFil
039200020918     C                   ELSE
039300020917     C                   EVAL      PrmEsito = Errore
039400020916     C                   EXSR      Uscita
039500020916     C                   ENDIF
039600020919
039700020919     C                   SELECT
039800020919     ** Valore assunto per il tipo coda.
039900020919     C                   WHEN      PrmTpOutQ = *BLANK
040000020919     C                   EVAL      PrmTpOutQ = Generica
040100020919     ** Valori ammessi per il tipo coda.
040200020919     C                   WHEN      PrmTpOutQ <> Generica
040300020919     C                             AND
040400020919     C                             PrmTpOutQ <> POAmministr
040500020919     C                             AND
040600020919     C                             PrmTpOutQ <> FaxIn
040700020919     C                             AND
040800020919     C                             PrmTpOutQ <> FaxOut
040900050708     C                             AND
041000050708     C                             PrmTpOutQ <> Commerciale
041100040921     C***                          AND
041200040921     C***                          PrmTpOutQ <> LetVettura
041300020919     C                   EVAL      PrmEsito = Errore
041400020919     C                   EXSR      Uscita
041500020919     C                   ENDSL
041600020920
041700020920     C                   SELECT
041800020920     ** Valore assunto per il tipo fine.
041900020920     C                   WHEN      PrmFine = *BLANK
042000020920     C                   EVAL      PrmFine = Return
042100020920     ** Valori ammessi per il tipo fine.
042200020920     C                   WHEN      PrmFine <> Return
042300020920     C                             AND
042400020920     C                             PrmFine <> LR
042500020920     C                   EVAL      PrmEsito = Errore
042600020920     C                   EXSR      Uscita
042700020920     C                   ENDSL
042800020916
042900020916     C                   ENDSR
043000020920
043100020920     ***********************************************************************
043200020920     **
043300020920     ** Reperisco schiere LIN e TFE.
043400020920     **
043500020920     ***********************************************************************
043600020920     C     RtvLinTfe     BEGSR
043700020920
043800020920     C                   IF        (PrmTpOutQ = FaxIn
043900020920     C                              OR
044000040921     C                              PrmTpOutQ = FaxOut)
044100040921     C***                           OR
044200040921     C***                           PrmTpOutQ = LetVettura)
044300020920     C                             AND
044400020920     C                             Lin(1) = 0
044500020920
044600020920     C                   EVAL      I26TLa = 'L'
044700020920     C                   EVAL      I26TpT = 'P'
044800020920     C                   CALL      'TRUL26R'
044900020920     C                   PARM                    Trul26DS
045000020920
045100020920     C                   ENDIF
045200020920
045300020920     C                   ENDSR
045400020916
045500020916     ***********************************************************************
045600020916     **
045700020916     ** Operazioni iniziali.
045800020916     **
045900020916     ***********************************************************************
046000020916     C     *INZSR        BEGSR
046100020916
046200020916     C     k01Org01      KLIST
046300020916     C                   KFLD                    OrgFil
046400020916
046500020916     C                   ENDSR
046600020916
046700020916     ***********************************************************************
046800020916     **
046900020916     ** Operazioni finali.
047000020916     **
047100020916     ***********************************************************************
047200020916     C     Uscita        BEGSR
047300020916
047400020917     C                   IF        PrmEsito = *BLANK
047500020916     C                   EVAL      PrmEsito = Eseguito
047600020916     C                   ENDIF
047700020916
047800020920     C                   EVAL      *INLR = (PrmFine = LR)
047900020916     C                   RETURN
048000020916
048100020916     C                   ENDSR
