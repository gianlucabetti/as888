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
003500141112     D*---------                                                            ----------------------*
003600140116     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003700140116     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003800140114     D*---------                                                            ----------------------*
003900000110     I* Tracciato record file sorgente in lettura
004000981126     IINPUTBND  NS  01
004100140117     I                                  1  112  TotSpec                         Specifica
004200000110     I*---------                                                            ----------------------*
004300981201     C*-------------------------------------------------------------------------------------------*
004400981201     C*  FLUSSO PRINCIPALE                                                                        *
004500981201     C*-------------------------------------------------------------------------------------------*
004600981201     C*
004700981201     C     *ENTRY        PLIST
004800140116     C                   PARM                    pIn_Lib
004900140116     C                   PARM                    pIn_File
005000140625     C                   PARM                    pIn_Mbr
005100141111     C                   PARM                    pIn_TipoMbr
005200140116     C                   PARM                    pOut_Esito
005300981201     C*
005400140116     C* controlli sul membro che non fa il CLLE
005500140116     C                   EXSR      CtrlMbr
005600140114     C*
005700981126     C                   OPEN      INPUTBND
005800141113     C*
005900141113     C                   Clear                   WFTID000
006000990604     C*
006100981126     C                   READ      INPUTBND                               21
006200990603     C                   MOVE      *In21         $Esci
006300990604     C*
006400990604     C                   DoW       $Esci <> *On
006500140116     C*
006600140116     C* maiuscolizzo la specifica
006700140117     C                   EVAL      TotSpecDS = %xlate(minu:maiu:TotSpec)
006800000111     C*
006900141113     C* salvo la parte di codice in una variabile d'appoggio unica per RPG e CL
007000141113     C                   SELECT
007100141113     C                   WHEN      (pIn_TipoMbr = 'RPGLE' or
007200141113     C                              pIn_TipoMbr = 'SQLRPGLE')
007300141113     C                   EVAL      wString = SrcStrRPG
007400141113     C                   WHEN      pIn_TipoMbr = 'CLLE'
007500141113     C                   EVAL      wString = SrcStrCL
007600141113     C                   ENDSL
007700141113     C*
007800141111     C* 1) appena trovo un tipo specifica non asteriscato, diverso da blank e H,
007900141111     C*    ed è un RPG, esco
008000141111     C                   If        (pIn_TipoMbr = 'RPGLE' or
008100141111     C                              pIn_TipoMbr = 'SQLRPGLE') and
008200141111     C                             SrcPar <> '*' and
008300141113     C                             (SrcTip <> ' ' and SrcTip <> 'H')
008400141111     C                   LEAVE
008500141113     C                   ENDIF
008600141113     C*
008700141113     C* 1bis) appena trovo un tipo specifica non commento
008800141113     C*    ed è un CL, esco
008900141113     C                   If        pIn_TipoMbr = 'CLLE' and
009000141113     C                             %scan('/*' : wString) = 0 and
009100141113     C                             %scan('*/' : wString) = 0
009200141113     C                   LEAVE
009300141113     C                   ENDIF
009400151201
009500151201     C*
009600151201     C                   EVAL      wIter = *off
009700160302     C                   EVAL      wPosSlash = 0
009800151201     C                   If        (pIn_TipoMbr = 'RPGLE' or
009900151201     C                              pIn_TipoMbr = 'SQLRPGLE')
010000151201     C* 2) itero se la riga è asteriscata ed è un RPG in formato a colonna
010100151201     C                   If        SrcPar = '*'
010200151202     C*    la riga la devo considerare anche se asteriscata nel caso ci sia PARMS nelle colonne
010300151202     C*    dalla 8 alla 12 o PRM dalla 8 alla 10 (compilatore PRO o BRT)
010400151202     C                             and %subst(wString:2:5) <> 'PARMS'
010500151202     C                             and %subst(wString:2:3) <> 'PRM'
010600151202     C                   EVAL      wIter = *on
010700151201     C                   ELSE
010800151201     C* 2bis) itero se la riga è commentata ed è un RPG in formato free
010900151201     C*       per scoprire se è solo commento, controllo se contiene '//' e prima non c'è nulla
011000151201     C                   EVAL      wPosSlash = %scan('//': wString)
011100151201     C                   IF        wPosSlash > 1 AND
011200151201     C                             %subst(wString:1:wPosSlash-1) = *blank    OR
011300151201     C                             wPosSlash = 1
011400151201     C                   EVAL      wIter = *on
011500151201     C                   ENDIF
011600151201     C                   ENDIF
011700151201     C                   ENDIF
011800151201
011900141111     C*
012000151201     C* 3) scansiono se nella riga c'è un riferimento a ACTGRP
012100151201     C                   IF        wIter = *off
012200141112
012300141112     C                   EVAL      wPos = 0
012400141112     C* ciclo finché non trovo più la stinga ACTGRP (c'è sia DFTACTGRP che ACTGRP)
012500141112     C                   DOU       wPos = 0
012600141112     C                   EVAL      wPos = %scan('ACTGRP': wString:wPos+1)
012700141112     C* se l'ho trovata
012800141112     C                   IF        wPos > 0
012900160302     C* ed è precedente agli slashes // del free format, o questi non ci sono (test che va bene
013000160302     C*  anche per i CLLE)
013100151202     C                             and (wPos < wPosSlash OR wPosSlash = 0)
013200141112     C* controllo se è DFTACTGRP(*NO)
013300141113     C                   IF        wPos > 3 and
013400141113     C                             %subst(wString : wPos - 3:
013500141112     C                              14) = 'DFTACTGRP(*NO)'
013600141111     C                   EVAL      TIDResult = %trim(TIDResult) + ' ' +
013700141111     C                              'DFTACTGRP(*NO)'
013800141111     C                   ENDIF
013900141112     C* controllo se è DFTACTGRP(*YES)
014000141113     C                   IF        wPos > 3 and
014100141113     C                             %subst(wString : wPos - 3:
014200141112     C                              15) = 'DFTACTGRP(*YES)'
014300141112     C                   EVAL      TIDResult = %trim(TIDResult) + ' ' +
014400141112     C                              'DFTACTGRP(*YES)'
014500141112     C                   ENDIF
014600141112     C* controllo se è ACTGRP(
014700141113     C                   IF        wPos > 1 and
014800141113     C                             %subst(wString : wPos - 1 : 8)
014900141112     C                              = ' ACTGRP('
015000141111     C                   EVAL      TIDResult = %trim(TIDResult) + ' ' +
015100141114     C                             %subst(wString : wPos :
015200141114     C                             %scan(' ' : wString : wPos +7) - wPos)
015300141111     C                   ENDIF
015400141112     C                   ENDIF
015500141112     C                   ENDDO
015600151201
015700151201     C                   ENDIF
015800000110     C*
015900990604     C                   READ      INPUTBND                               21
016000990604     C                   MOVE      *In21         $Esci
016100981203     C                   EndDo
016200990604     C*
016300160112     C* Se alla fine del sorgente NON ho trovato niente che mi dica in che ACTGRP deve girare,
016400160112     C* per default BRT allora è in DFTACTGRP(*YES)
016500160112     C                   If        TIDResult = *blank
016600160112     C                   EVAL      TIDResult =
016700160112     C                              'DFTACTGRP(*YES)'
016800160112     C                   EndIf
016900160112     C*
017000160112     C* Se alla fine del sorgente ho trovato qualcosa, scrivo il risultato sul file
017100160112     C                   If        TIDResult <> *blank
017200000110     C*
017300141113     C                   MoveL     pIn_Mbr       TIDNome
017400140116     C                   MoveL     pIn_File      TIDFile
017500140116     C                   MoveL     pIn_Lib       TIDLib
017600160401     C                   MoveL     pIn_TipoMbr   TIDAttr
017700000110     C*
017800140116     C                   Write     WFTID000
017900140120     C                   FEOD      WFTIDTMP
018000000110     C*
018100000110     C                   EndIf
018200000110     C*
018300981126     C                   CLOSE     INPUTBND
018400981126     C*
018500980325     C                   SETON                                            RT
018600981126     C*
018700140116     C*------------------------------------------------------------------------*
018800140116     C* Controlli sul membro che non fa il CLLE chiamante
018900140116     C*------------------------------------------------------------------------*
019000140116     C     CtrlMbr       BEGSR
019100140116     C*
019200140116     C* controllo che il membro in elaborazione non sia una copia di salvataggio
019300141113     C                   IF        %subst(pIn_Mbr :
019400141113     C                               %checkr(' ' : pIn_Mbr)) = '£'
019500140116     C                   RETURN
019600140116     C                   ENDIF
019700140116     C*
019800140116     C                   ENDSR
