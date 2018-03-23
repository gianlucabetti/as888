000100000111     H DECEDIT('0,') DATEDIT(*DMY.)
000200981201     F*-------------------------------------------------------------------------------------------*
000300141111     F*  Il pgm cerca per ogni membro in carico se ha un riferimento all'ACTGRP
000400981201     F*-------------------------------------------------------------------------------------------*
000500981203     FINPUTBND  IF   F  112        DISK    USROPN
000600140117     FWFTIDTMP  O    E             DISK
000700981201     D*-------------------------------------------------------------------------------------------*
000800160302     D* spezza la specifica
000900140117     DTotSpecDS        DS           112
001000140117     D SRCSEQ                  1      6  0                                      N° specifica
001100140117     D SRCDAT                  7     12  0                                      Data modifica spec.
001200140117     D SRCNUM                 13     17                                         N° contrassegno
001300140117     D SRCTIP                 18     18                                         Tipo specifica
001400141111     D SRCPAR                 19     19                                         '*' per commento
001500141111     D SRCStrRPG              19    112                                         Stringa
001600141111     D SRCStrCL               13    112                                         Stringa
001700140626     Dpsds            sds
001800140626     D pgmname           *proc
001900140626     D pgmsts            *status
002000140117     D*---------                                  d                         ----------------------*
002100990604     D* flag di fine sorgente
002200990604     D $Esci           S              1A
002300140116     D*---------                                  d                         ----------------------*
002400140116     D* parametri
002500140117     d pIn_Lib         s             10a
002600140117     d pIn_File        s             10a
002700140625     d pIn_Mbr         s             10a
002800141112     d pIn_TipoMbr     s             10a
002900140117     d pOut_Esito      s              2  0
003000140625     D*---------                                                            ----------------------*
003100141112     D wString         s            112
003200141113     D wPos            s              3s 0
003300151201     D wPosSlash       s              3s 0
003400151201     D wIter           s               n
003500160404     D* ho trovato il DFTACTGRP(*NO)
003600160404     D wDFTACTGRPNO    s               n   inz(*off)
003700160404     D* ho trovato il DFTACTGRP(*YES)
003800160404     D wDFTACTGRPYES   s               n   inz(*off)
003900160404     D* ho trovato l' ACTGRP
004000160404     D wACTGRP         s             50    inz(*blank)
004100141112     D*---------                                                            ----------------------*
004200140116     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004300140116     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004400140114     D*---------                                                            ----------------------*
004500000110     I* Tracciato record file sorgente in lettura
004600981126     IINPUTBND  NS  01
004700140117     I                                  1  112  TotSpec                         Specifica
004800000110     I*---------                                                            ----------------------*
004900981201     C*-------------------------------------------------------------------------------------------*
005000981201     C*  FLUSSO PRINCIPALE                                                                        *
005100981201     C*-------------------------------------------------------------------------------------------*
005200981201     C*
005300981201     C     *ENTRY        PLIST
005400140116     C                   PARM                    pIn_Lib
005500140116     C                   PARM                    pIn_File
005600140625     C                   PARM                    pIn_Mbr
005700141111     C                   PARM                    pIn_TipoMbr
005800140116     C                   PARM                    pOut_Esito
005900981201     C*
006000140116     C* controlli sul membro che non fa il CLLE
006100140116     C                   EXSR      CtrlMbr
006200140114     C*
006300981126     C                   OPEN      INPUTBND
006400141113     C*
006500141113     C                   Clear                   WFTID000
006600160404     C                   RESET                   wDFTACTGRPNO
006700160404     C                   RESET                   wDFTACTGRPYES
006800160404     C                   RESET                   wACTGRP
006900990604     C*
007000981126     C                   READ      INPUTBND                               21
007100990603     C                   MOVE      *In21         $Esci
007200990604     C*
007300990604     C                   DoW       $Esci <> *On
007400140116     C*
007500140116     C* maiuscolizzo la specifica
007600140117     C                   EVAL      TotSpecDS = %xlate(minu:maiu:TotSpec)
007700000111     C*
007800141113     C* salvo la parte di codice in una variabile d'appoggio unica per RPG e CL
007900141113     C                   SELECT
008000141113     C                   WHEN      (pIn_TipoMbr = 'RPGLE' or
008100141113     C                              pIn_TipoMbr = 'SQLRPGLE')
008200141113     C                   EVAL      wString = SrcStrRPG
008300141113     C                   WHEN      pIn_TipoMbr = 'CLLE'
008400141113     C                   EVAL      wString = SrcStrCL
008500141113     C                   ENDSL
008600141113     C*
008700141111     C* 1) appena trovo un tipo specifica non asteriscato, diverso da blank e H,
008800141111     C*    ed è un RPG, esco
008900141111     C                   If        (pIn_TipoMbr = 'RPGLE' or
009000141111     C                              pIn_TipoMbr = 'SQLRPGLE') and
009100141111     C                             SrcPar <> '*' and
009200141113     C                             (SrcTip <> ' ' and SrcTip <> 'H')
009300141111     C                   LEAVE
009400141113     C                   ENDIF
009500141113     C*
009600141113     C* 1bis) appena trovo un tipo specifica non commento
009700141113     C*    ed è un CL, esco
009800141113     C                   If        pIn_TipoMbr = 'CLLE' and
009900141113     C                             %scan('/*' : wString) = 0 and
010000141113     C                             %scan('*/' : wString) = 0
010100141113     C                   LEAVE
010200141113     C                   ENDIF
010300151201
010400151201     C*
010500151201     C                   EVAL      wIter = *off
010600160302     C                   EVAL      wPosSlash = 0
010700151201     C                   If        (pIn_TipoMbr = 'RPGLE' or
010800151201     C                              pIn_TipoMbr = 'SQLRPGLE')
010900151201     C* 2) itero se la riga è asteriscata ed è un RPG in formato a colonna
011000151201     C                   If        SrcPar = '*'
011100151202     C*    la riga la devo considerare anche se asteriscata nel caso ci sia PARMS nelle colonne
011200151202     C*    dalla 8 alla 12 o PRM dalla 8 alla 10 (compilatore PRO o BRT)
011300151202     C                             and %subst(wString:2:5) <> 'PARMS'
011400151202     C                             and %subst(wString:2:3) <> 'PRM'
011500151202     C                   EVAL      wIter = *on
011600151201     C                   ELSE
011700151201     C* 2bis) itero se la riga è commentata ed è un RPG in formato free
011800151201     C*       per scoprire se è solo commento, controllo se contiene '//' e prima non c'è nulla
011900151201     C                   EVAL      wPosSlash = %scan('//': wString)
012000151201     C                   IF        wPosSlash > 1 AND
012100151201     C                             %subst(wString:1:wPosSlash-1) = *blank    OR
012200151201     C                             wPosSlash = 1
012300151201     C                   EVAL      wIter = *on
012400151201     C                   ENDIF
012500151201     C                   ENDIF
012600151201     C                   ENDIF
012700151201
012800141111     C*
012900151201     C* 3) scansiono se nella riga c'è un riferimento a ACTGRP
013000160404     C* siccome ci sono 3 modi attualmente per compilare (l'opzione PC, quella BC e il 14) e
013100160404     C* quindi 3 ipotetici modi di dichiarare nel sorgente i 2 parametri DFTACTGRP e ACTGRP,
013200160404     C* per evitare che in TIDRESULT appaia la somma di tutti queste specifiche,
013300160404     C* prendo per buona l'ultima,
013400160404     C* che negli RPGLE sono le H-spec cioè le più probabili
013500160404     C* mentre nei CLLE mi spetto ci sia un solo modo di compilazione
013600151201     C                   IF        wIter = *off
013700141112
013800141112     C                   EVAL      wPos = 0
013900141112     C* ciclo finché non trovo più la stinga ACTGRP (c'è sia DFTACTGRP che ACTGRP)
014000141112     C                   DOU       wPos = 0
014100141112     C                   EVAL      wPos = %scan('ACTGRP': wString:wPos+1)
014200141112     C* se l'ho trovata
014300141112     C                   IF        wPos > 0
014400160302     C* ed è precedente agli slashes // del free format, o questi non ci sono (test che va bene
014500160302     C*  anche per i CLLE)
014600151202     C                             and (wPos < wPosSlash OR wPosSlash = 0)
014700141112     C* controllo se è DFTACTGRP(*NO)
014800141113     C                   IF        wPos > 3 and
014900141113     C                             %subst(wString : wPos - 3:
015000160404     C                              14) = 'DFTACTGRP(*NO)'
015100160404     C                   EVAL      wDFTACTGRPNO = *on
015200160404     C                   EVAL      wDFTACTGRPYES = *off
015300160404     C***                EVAL      TIDResult = %trim(TIDResult) + ' ' +
015400160404     C***                           'DFTACTGRP(*NO)'
015500141111     C                   ENDIF
015600141112     C* controllo se è DFTACTGRP(*YES)
015700141113     C                   IF        wPos > 3 and
015800141113     C                             %subst(wString : wPos - 3:
015900141112     C                              15) = 'DFTACTGRP(*YES)'
016000160404     C                   EVAL      wDFTACTGRPYES = *on
016100160404     C                   EVAL      wDFTACTGRPNO = *off
016200160404     C                   EVAL      wACTGRP = *blank
016300160404     C***                EVAL      TIDResult = %trim(TIDResult) + ' ' +
016400160404     C***                           'DFTACTGRP(*YES)'
016500141112     C                   ENDIF
016600141112     C* controllo se è ACTGRP(
016700141113     C                   IF        wPos > 1 and
016800141113     C                             %subst(wString : wPos - 1 : 8)
016900141112     C                              = ' ACTGRP('
017000160404     C                   EVAL      wACTGRP =
017100141114     C                             %subst(wString : wPos :
017200141114     C                             %scan(' ' : wString : wPos +7) - wPos)
017300160404     C                   EVAL      wDFTACTGRPYES= *off
017400160404     C                   EVAL      wDFTACTGRPNO = *on
017500160404     C***                EVAL      TIDResult = %trim(TIDResult) + ' ' +
017600160404     C***                          %subst(wString : wPos :
017700160404     C***                          %scan(' ' : wString : wPos +7) - wPos)
017800141111     C                   ENDIF
017900141112     C                   ENDIF
018000141112     C                   ENDDO
018100151201
018200151201     C                   ENDIF
018300000110     C*
018400990604     C                   READ      INPUTBND                               21
018500990604     C                   MOVE      *In21         $Esci
018600981203     C                   EndDo
018700990604     C*
018800160112     C* Se alla fine del sorgente NON ho trovato niente che mi dica in che ACTGRP deve girare,
018900160112     C* per default BRT allora è in DFTACTGRP(*YES)
019000160404     C                   If        wDFTACTGRPNO = *off and
019100160404     C                             wDFTACTGRPYES= *off and
019200160404     C                             wACTGRP = *blank
019300160404     C***                If        TIDResult = *blank
019400160404     C***                EVAL      TIDResult =
019500160404     C***                           'DFTACTGRP(*YES)'
019600160404     C                   EVAL      wDFTACTGRPYES= *on
019700160112     C                   EndIf
019800160112     C*
019900160112     C* Se alla fine del sorgente ho trovato qualcosa, scrivo il risultato sul file
020000160404     C                   If        wDFTACTGRPNO = *on  or
020100160404     C                             wDFTACTGRPYES= *on  or
020200160404     C                             wACTGRP <> *blank
020300160404     C***                If        TIDResult <> *blank
020400000110     C*
020500141113     C                   MoveL     pIn_Mbr       TIDNome
020600140116     C                   MoveL     pIn_File      TIDFile
020700140116     C                   MoveL     pIn_Lib       TIDLib
020800160401     C                   MoveL     pIn_TipoMbr   TIDAttr
020900160404     C                   SELECT
021000160404     C                   WHEN      wDFTACTGRPYES = *on
021100160404     C                   EVAL      TIDResult = 'DFTACTGRP(*YES)'
021200160404     C                   WHEN      wDFTACTGRPNO= *on or
021300160404     C                             wACTGRP <> *blank
021400160404     C                   EVAL      TIDResult = 'DFTACTGRP(*NO)'
021500160404     C                   EVAL      TIDResult =
021600160404     C                                         %trim(TIDResult) + ' ' +
021700160404     C                                         %trim(wACTGRP)
021800160404     C                   ENDSL
021900000110     C*
022000140116     C                   Write     WFTID000
022100140120     C                   FEOD      WFTIDTMP
022200000110     C*
022300000110     C                   EndIf
022400000110     C*
022500981126     C                   CLOSE     INPUTBND
022600981126     C*
022700980325     C                   SETON                                            RT
022800981126     C*
022900140116     C*------------------------------------------------------------------------*
023000140116     C* Controlli sul membro che non fa il CLLE chiamante
023100140116     C*------------------------------------------------------------------------*
023200140116     C     CtrlMbr       BEGSR
023300140116     C*
023400140116     C* controllo che il membro in elaborazione non sia una copia di salvataggio
023500141113     C                   IF        %subst(pIn_Mbr :
023600141113     C                               %checkr(' ' : pIn_Mbr)) = '£'
023700140116     C                   RETURN
023800140116     C                   ENDIF
023900140116     C*
024000140116     C                   ENDSR
