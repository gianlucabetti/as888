000100000111     H DECEDIT('0,') DATEDIT(*DMY.)
000200981201     F*-------------------------------------------------------------------------------------------*
000300140625     F*  Il pgm cerca per ogni membro in carico qual è la prima posizione libera.
000400981201     F*-------------------------------------------------------------------------------------------*
000500981203     FINPUTBND  IF   F  112        DISK    USROPN
000600140117     FWFTIDTMP  O    E             DISK
000700981201     D*-------------------------------------------------------------------------------------------*
000800140116     D* posizioni trovate
000900140116     D $PosOkDS        DS            20
001000140116     D $PosOk                         1A   dim(20)
001100140117     D*---------                                  d                         ----------------------*
001200140117     D* spezza la specifica
001300140117     DTotSpecDS        DS           112
001400140117     D SRCSEQ                  1      6  0                                      N° specifica
001500140117     D SRCDAT                  7     12  0                                      Data modifica spec.
001600140117     D SRCNUM                 13     17                                         N° contrassegno
001700140117     D SRCTIP                 18     18                                         Tipo specifica
001800140117     D SRCPAR                 19     23                                         '*' per commento
001900140117     D SRCFat1                24     37                                         Codice operativo
002000140117     D SRCCodOp               38     47
002100140117     D SRCFat2                48     61
002200140117     D SRCFat2Ext             48    112
002300140117     D SRCRisu                62     75
002400140117     D SRCSpeO                42     55
002500140626     Dpsds            sds
002600140626     D pgmname           *proc
002700140626     D pgmsts            *status
002800140117     D*---------                                  d                         ----------------------*
002900990604     D* flag di fine sorgente
003000990604     D $Esci           S              1A
003100140116     D*---------                                  d                         ----------------------*
003200140116     D* parametri
003300140117     d pIn_Lib         s             10a
003400140117     d pIn_File        s             10a
003500140625     d pIn_Mbr         s             10a
003600140116     d pIn_SelTxt      s             10a
003700140116     d pIn_MLNAME      s             10a
003800140116     d pIn_MLMTXT      s             50a
003900140117     d pOut_Esito      s              2  0
004000140625     D*---------                                                            ----------------------*
004100140625     d wPosVINDTA      s              3  0
004200140625     d wPosLen         s              3  0
004300140625     d wPosEnd         s              3  0
004400140625     d wPos            s              4  0
004500140625     d wLen            s              4  0
004600140625     d wwPos           s              4  0
004700140625     d wwLen           s              4  0
004800140116     D*------------------
004900140116     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005000140116     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005100140114     D*---------                                                            ----------------------*
005200000110     I* Tracciato record file sorgente in lettura
005300981126     IINPUTBND  NS  01
005400140117     I                                  1  112  TotSpec                         Specifica
005500000110     I*---------                                                            ----------------------*
005600981201     C*-------------------------------------------------------------------------------------------*
005700981201     C*  FLUSSO PRINCIPALE                                                                        *
005800981201     C*-------------------------------------------------------------------------------------------*
005900981201     C*
006000981201     C     *ENTRY        PLIST
006100140116     C                   PARM                    pIn_Lib
006200140116     C                   PARM                    pIn_File
006300140625     C                   PARM                    pIn_Mbr
006400140116     C                   PARM                    pIn_SelTxt
006500140116     C                   PARM                    pIn_MLNAME
006600140116     C                   PARM                    pIn_MLMTXT
006700140116     C                   PARM                    pOut_Esito
006800981201     C*
006900140116     C* controlli sul membro che non fa il CLLE
007000140116     C                   EXSR      CtrlMbr
007100140114     C*
007200981126     C                   OPEN      INPUTBND
007300000111     C                   Move      *Off          $SpecH            1
007400140120     C                   CLEAR                   $PosOkDS
007500140626     C                   EVAL      wwPos = 0
007600140626     C                   EVAL      wwLen = 0
007700990604     C*
007800981126     C                   READ      INPUTBND                               21
007900990603     C                   MOVE      *In21         $Esci
008000990604     C*
008100990604     C                   DoW       $Esci <> *On
008200140116     C*
008300140116     C* maiuscolizzo la specifica
008400140117     C                   EVAL      TotSpecDS = %xlate(minu:maiu:TotSpec)
008500000111     C*
008600000111     C* 2) specifiche 'C' (non asteriscate !!!)
008700000110     C                   If        (SrcTip = 'C' or SrcTip = 'c')
008800000110     C                             and %SubSt(SrcPar:1:1) <> '*'
008900140625     C                   EVAL      wPos = 0
009000140625     C                   EVAL      wLen = 0
009100000111     C*
009200140626     C* - cerco la stringa '%SUBST(VINDTA:' in cui ci sarà una posizione e una lunghezza
009300140626     C                   EVAL      wPosVINDTA =
009400140626     C                                        %scan('%SUBST(VINDTA:':SRCFat2Ext)
009500140625     C                   IF        wPosVINDTA <> 0
009600140625     C* - se c'è la stringa 'VINDTA:' cerco la posizione e la lunghezza
009700140625     C                   EVAL      wPosLen = %scan(':' : SRCFat2Ext :
009800140626     C                                        wPosVINDTA + 14)
009900140626     C* se non ho trovato la posizione del secondo : significa che la specifica va a capo e non
010000140626     C* posso fare diagnostica
010100140626     C                   IF        wPosLen > 0
010200140625     C                   EVAL      wPosEnd = %scan(')' : SRCFat2Ext :
010300140625     C                                        wPosLen + 1)
010400140626     C* se non ho trovato la posizione della ) significa che la specifica va a capo e non
010500140626     C* posso fare diagnostica
010600140626     C                   IF        wPosEnd > 0
010700140626     C                   MONITOR
010800140625     C                   EVAL      wPos = %dec(%subst(SRCFat2Ext:
010900140626     C                                     wPosVINDTA + 14:
011000140626     C                                     wPosLen - (wPosVINDTA + 14))
011100140626     C                                     : 4 : 0)
011200140626     C* controllo se il %dec non ha cercato di processare un alfanumerico
011300140626     C                   ON-ERROR  *all
011400140626     C                   IF        pgmsts = 105
011500140626     C                   EVAL      wPos = 0
011600140626     C                   ENDIF
011700140626     C                   ENDMON
011800140626     C                   MONITOR
011900140625     C                   EVAL      wLen = %dec(%subst(SRCFat2Ext:
012000140626     C                                     wPosLen + 1:
012100140626     C                                     wPosEnd - (wPosLen + 1))
012200140626     C                                     : 4 : 0)
012300140626     C* controllo se il %dec non ha cercato di processare un alfanumerico
012400140626     C                   ON-ERROR  *all
012500140626     C                   IF        pgmsts = 105
012600140626     C                   EVAL      wLen = 0
012700140626     C                   ENDIF
012800140626     C                   ENDMON
012900140626     C                   ENDIF
013000140626     C*
013100140626     C                   ENDIF
013200140626     C*
013300140120     C                   ENDIF
013400140625     C*
013500140625     C* - se la posizione trovata è > dell'ultima salvata,
013600140625     C*   oppure è = all'ultima salvata ma la lunghezza è >,
013700140625     C*   salvo questa e la lunghezza
013800140625     C                   IF        wPos > wwPos
013900140625     C                             or (wPos = wwPos and wLen > wwLen)
014000140625     C                   EVAL      wwPos = wPos
014100140625     C                   EVAL      wwLen = wLen
014200140625     C                   ENDIF
014300140312     C*
014400000110     C                   EndIf
014500000110     C*
014600990604     C                   READ      INPUTBND                               21
014700990604     C                   MOVE      *In21         $Esci
014800981203     C                   EndDo
014900990604     C*
015000140116     C* Se alla fine del sorgente ho trovato qualcosa, scrivo il risultato sul file
015100140625     C                   If        wwPos > 0
015200000110     C*
015300140116     C                   Clear                   WFTID000
015400000110     C*
015500140117     C                   MoveL     pIn_MLNAME    TIDNome
015600140117     C                   MoveL     pIn_MLMTXT    TIDDescr
015700140116     C                   MoveL     pIn_File      TIDFile
015800140116     C                   MoveL     pIn_Lib       TIDLib
015900140625     C                   EVAL      TIDResult = %editc(wwPos:'X') +
016000140625     C                                         %editc(wwLen:'X')
016100000110     C*
016200140116     C                   Write     WFTID000
016300140120     C                   FEOD      WFTIDTMP
016400000110     C*
016500000110     C                   EndIf
016600000110     C*
016700981126     C                   CLOSE     INPUTBND
016800981126     C*
016900980325     C                   SETON                                            RT
017000981126     C*
017100140116     C*------------------------------------------------------------------------*
017200140116     C* Controlli sul membro che non fa il CLLE chiamante
017300140116     C*------------------------------------------------------------------------*
017400140116     C     CtrlMbr       BEGSR
017500140116     C*
017600140116     C* controllo che il membro in elaborazione non sia una copia di salvataggio
017700140116     C                   IF        %subst(pIn_MLNAME :
017800140116     C                               %checkr(' ' : pIn_MLNAME)) = '£'
017900140116     C                   RETURN
018000140116     C                   ENDIF
018100140116     C*
018200140117     C* controllo che la descrizione del membro in elaborazione contenga la stringa inviata
018300140117     C* (siccome la stringa di selezione è in maiuscolo, maiuscolizzo anche la descrizione)
018400140116     C                   IF        pIn_SelTxt <> *blank
018500140117     C                   IF        %scan(%trim(pIn_SelTxt) :
018600140117     C                              %xlate(minu:maiu:pIn_MLMTXT)) = 0
018700140116     C                   RETURN
018800140116     C                   ENDIF
018900140116     C                   ENDIF
019000140116     C*
019100140116     C                   ENDSR
