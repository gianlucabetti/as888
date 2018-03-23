000100000111     H DECEDIT('0,') DATEDIT(*DMY.)
000200981201     F*-------------------------------------------------------------------------------------------*
000300140114     F*  Il pgm cerca per ogni membro in carico se in operazione EVAL sono presenti i seguenti
000400140114     F*  campi che hanno una gestione alfanumerica:
000500140114     F*  VABRSD - VABIND - VABLOD - VABPRD - VABCAD
000600140114     F*  associati alla stringa 'VINDTA:' che significa che si stanno valorizzando da file.
000700981201     F*-------------------------------------------------------------------------------------------*
000800981203     FINPUTBND  IF   F  112        DISK    USROPN
000900140117     FWFTIDTMP  O    E             DISK
001000981201     D*-------------------------------------------------------------------------------------------*
001100140116     D* posizioni trovate
001200140116     D $PosOkDS        DS            20
001300140116     D $PosOk                         1A   dim(20)
001400140117     D*---------                                  d                         ----------------------*
001500140117     D* spezza la specifica
001600140117     DTotSpecDS        DS           112
001700140117     D SRCSEQ                  1      6  0                                      N° specifica
001800140117     D SRCDAT                  7     12  0                                      Data modifica spec.
001900140117     D SRCNUM                 13     17                                         N° contrassegno
002000140117     D SRCTIP                 18     18                                         Tipo specifica
002100140117     D SRCPAR                 19     23                                         '*' per commento
002200140117     D SRCFat1                24     37                                         Codice operativo
002300140117     D SRCCodOp               38     47
002400140117     D SRCFat2                48     61
002500140117     D SRCFat2Ext             48    112
002600140117     D SRCRisu                62     75
002700140117     D SRCSpeO                42     55
002800140117     D*---------                                  d                         ----------------------*
002900990604     D* flag di fine sorgente
003000990604     D $Esci           S              1A
003100140116     D*---------                                  d                         ----------------------*
003200140116     D* parametri
003300140117     d pIn_Lib         s             10a
003400140117     d pIn_File        s             10a
003500140116     d pIn_PosRSD      s              4s 0
003600140116     d pIn_PosIND      s              4s 0
003700140116     d pIn_PosLOD      s              4s 0
003800140116     d pIn_PosPRD      s              4s 0
003900140116     d pIn_PosCAD      s              4s 0
004000140312     d pIn_PoTNOTE     s              4s 0
004100140312     d pIn_PoTNOTI     s              4s 0
004200150407     d pIn_VAORSR      s              4s 0
004300150407     d pIn_VAOTER      s              4s 0
004400150407     d pIn_VAORFA      s              4s 0
004500140116     d pIn_SelTxt      s             10a
004600140116     d pIn_MLNAME      s             10a
004700140116     d pIn_MLMTXT      s             50a
004800140117     d pOut_Esito      s              2  0
004900140116     D*------------------
005000140116     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005100140116     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005200140114     D*---------                                                            ----------------------*
005300000110     I* Tracciato record file sorgente in lettura
005400981126     IINPUTBND  NS  01
005500140117     I                                  1  112  TotSpec                         Specifica
005600000110     I*---------                                                            ----------------------*
005700981201     C*-------------------------------------------------------------------------------------------*
005800981201     C*  FLUSSO PRINCIPALE                                                                        *
005900981201     C*-------------------------------------------------------------------------------------------*
006000981201     C*
006100981201     C     *ENTRY        PLIST
006200140116     C                   PARM                    pIn_Lib
006300140116     C                   PARM                    pIn_File
006400140116     C                   PARM                    pIn_PosRSD
006500140116     C                   PARM                    pIn_PosIND
006600140116     C                   PARM                    pIn_PosLOD
006700140116     C                   PARM                    pIn_PosPRD
006800140116     C                   PARM                    pIn_PosCAD
006900140312     C                   PARM                    pIn_PoTNOTE
007000140312     C                   PARM                    pIn_PoTNOTI
007100150407     C                   PARM                    pIn_VAORSR
007200150407     C                   PARM                    pIn_VAOTER
007300150407     C                   PARM                    pIn_VAORFA
007400140116     C                   PARM                    pIn_SelTxt
007500140116     C                   PARM                    pIn_MLNAME
007600140116     C                   PARM                    pIn_MLMTXT
007700140116     C                   PARM                    pOut_Esito
007800981201     C*
007900140116     C* controlli sul membro che non fa il CLLE
008000140116     C                   EXSR      CtrlMbr
008100140114     C*
008200981126     C                   OPEN      INPUTBND
008300000111     C                   Move      *Off          $SpecH            1
008400140120     C                   CLEAR                   $PosOkDS
008500990604     C*
008600981126     C                   READ      INPUTBND                               21
008700990603     C                   MOVE      *In21         $Esci
008800990604     C*
008900990604     C                   DoW       $Esci <> *On
009000140116     C*
009100140116     C* maiuscolizzo la specifica
009200140117     C                   EVAL      TotSpecDS = %xlate(minu:maiu:TotSpec)
009300000111     C*
009400000111     C* 2) specifiche 'C' (non asteriscate !!!)
009500000110     C                   If        (SrcTip = 'C' or SrcTip = 'c')
009600000110     C                             and %SubSt(SrcPar:1:1) <> '*'
009700000111     C*
009800140116     C* - cerco VABRSD
009900140120     C                   IF        pIn_PosRSD >0                   and  (
010000140117     C                             %scan('VABRSD':SRCFat2Ext) <> 0 and
010100140120     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
010200140120     C                             %scan('VINDTA:'+ %trim(%char(pIn_PosRSD))
010300140117     C                              + ':' :
010400140117     C                              SRCFat2Ext) <> 0                     OR
010500140120     C                             %scan('VABRSD':SRCFat2Ext) <> 0 and
010600140120     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
010700140117     C                             %scan('VINDTA:'+ %editc(pIn_PosRSD:'X')
010800140117     C                              + ':' :
010900140117     C                              SRCFat2Ext) <> 0                    )
011000140116     C                   EVAL      $PosOk(1) = 'X'
011100140120     C                   ENDIF
011200140116     C*
011300140116     C* - cerco VABIND
011400140120     C                   IF        pIn_PosIND >0                   and  (
011500140117     C                             %scan('VABIND':SRCFat2Ext) <> 0 and
011600140120     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
011700140120     C                             %scan('VINDTA:'+ %trim(%char(pIn_PosIND))
011800140120     C                              + ':' :
011900140120     C                              SRCFat2Ext) <> 0                     OR
012000140120     C                             %scan('VABIND':SRCFat2Ext) <> 0 and
012100140120     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
012200140120     C                             %scan('VINDTA:'+ %editc(pIn_PosIND:'X')
012300140120     C                              + ':' :
012400140117     C                              SRCFat2Ext) <> 0                    )
012500140116     C                   EVAL      $PosOk(2) = 'X'
012600140120     C                   ENDIF
012700140116     C*
012800140116     C* - cerco VABLOD
012900140120     C                   IF        pIn_PosLOD >0                   and  (
013000140117     C                             %scan('VABLOD':SRCFat2Ext) <> 0 and
013100140120     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
013200140120     C                             %scan('VINDTA:'+ %trim(%char(pIn_PosLOD))
013300140120     C                              + ':' :
013400140120     C                              SRCFat2Ext) <> 0                     OR
013500140120     C                             %scan('VABLOD':SRCFat2Ext) <> 0 and
013600140120     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
013700140120     C                             %scan('VINDTA:'+ %editc(pIn_PosLOD:'X')
013800140120     C                              + ':' :
013900140117     C                              SRCFat2Ext) <> 0                    )
014000140116     C                   EVAL      $PosOk(3) = 'X'
014100140120     C                   ENDIF
014200140116     C*
014300140116     C* - cerco VABPRD
014400140120     C                   IF        pIn_PosPRD >0
014500140120     C                   ENDIF
014600140116     C*
014700140116     C* - cerco VABCAD
014800140120     C                   IF        pIn_PosCAD >0
014900140120     C                   ENDIF
015000140312     C*
015100140312     C* - cerco VATNOT_E
015200140312     C                   IF        pIn_PoTNOTE>0                   and  (
015300140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
015400140312     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
015500140312     C                             %scan('VINDTA:'+ %trim(%char(pIn_PoTNOTE))
015600140312     C                              + ':' :
015700140312     C                              SRCFat2Ext) <> 0                     OR
015800140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
015900140312     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
016000140312     C                             %scan('VINDTA:'+ %editc(pIn_PoTNOTE:'X')
016100140312     C                              + ':' :
016200140312     C                              SRCFat2Ext) <> 0                    )
016300140312     C                   EVAL      $PosOk(4) = 'X'
016400140312     C                   ENDIF
016500140312     C*
016600140312     C* - cerco VATNOT_I
016700140312     C                   IF        pIn_PoTNOTI>0                   and  (
016800140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
016900140312     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
017000140312     C                             %scan('VINDTA:'+ %trim(%char(pIn_PoTNOTI))
017100140312     C                              + ':' :
017200140312     C                              SRCFat2Ext) <> 0                     OR
017300140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
017400140312     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
017500140312     C                             %scan('VINDTA:'+ %editc(pIn_PoTNOTI:'X')
017600140312     C                              + ':' :
017700140312     C                              SRCFat2Ext) <> 0                    )
017800140312     C                   EVAL      $PosOk(5) = 'X'
017900140312     C                   ENDIF
018000150407     C*
018100150407     C* - cerco VAORSR
018200150407     C                   IF        pIn_VAORSR >0                   and  (
018300150407     C                             %scan('VAORSR':SRCFat2Ext) <> 0 and
018400150407     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
018500150407     C                             %scan('VINDTA:'+ %trim(%char(pIn_VAORSR))
018600150407     C                              + ':' :
018700150407     C                              SRCFat2Ext) <> 0                     OR
018800150407     C                             %scan('VAORSR':SRCFat2Ext) <> 0 and
018900150407     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
019000150407     C                             %scan('VINDTA:'+ %editc(pIn_VAORSR:'X')
019100150407     C                              + ':' :
019200150407     C                              SRCFat2Ext) <> 0                    )
019300150407     C                   EVAL      $PosOk(6) = 'X'
019400150407     C                   ENDIF
019500150407     C*
019600150407     C* - cerco VAOTER
019700150407     C                   IF        pIn_VAOTER >0                   and  (
019800150407     C                             %scan('VAOTER':SRCFat2Ext) <> 0 and
019900150407     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
020000150407     C                             %scan('VINDTA:'+ %trim(%char(pIn_VAOTER))
020100150407     C                              + ':' :
020200150407     C                              SRCFat2Ext) <> 0                     OR
020300150407     C                             %scan('VAOTER':SRCFat2Ext) <> 0 and
020400150407     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
020500150407     C                             %scan('VINDTA:'+ %editc(pIn_VAOTER:'X')
020600150407     C                              + ':' :
020700150407     C                              SRCFat2Ext) <> 0                    )
020800150407     C                   EVAL      $PosOk(7) = 'X'
020900150407     C                   ENDIF
021000150407     C*
021100150407     C* - cerco VAORFA
021200150407     C                   IF        pIn_VAORFA >0                   and  (
021300150407     C                             %scan('VAORFA':SRCFat2Ext) <> 0 and
021400150407     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
021500150407     C                             %scan('VINDTA:'+ %trim(%char(pIn_VAORFA))
021600150407     C                              + ':' :
021700150407     C                              SRCFat2Ext) <> 0                     OR
021800150407     C                             %scan('VAORFA':SRCFat2Ext) <> 0 and
021900150407     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
022000150407     C                             %scan('VINDTA:'+ %editc(pIn_VAORFA:'X')
022100150407     C                              + ':' :
022200150407     C                              SRCFat2Ext) <> 0                    )
022300150408     C                   EVAL      $PosOk(8) = 'X'
022400150407     C                   ENDIF
022500140312     C*
022600000110     C                   EndIf
022700000110     C*
022800990604     C                   READ      INPUTBND                               21
022900990604     C                   MOVE      *In21         $Esci
023000981203     C                   EndDo
023100990604     C*
023200140116     C* Se alla fine del sorgente ho trovato qualcosa, scrivo il risultato sul file
023300140116     C                   If        $PosOkDS <> *blank
023400000110     C*
023500140116     C                   Clear                   WFTID000
023600000110     C*
023700140117     C                   MoveL     pIn_MLNAME    TIDNome
023800140117     C                   MoveL     pIn_MLMTXT    TIDDescr
023900140116     C                   MoveL     pIn_File      TIDFile
024000140116     C                   MoveL     pIn_Lib       TIDLib
024100140116     C                   MoveL     $PosOkDS      TIDResult
024200000110     C*
024300140116     C                   Write     WFTID000
024400140120     C                   FEOD      WFTIDTMP
024500000110     C*
024600000110     C                   EndIf
024700000110     C*
024800981126     C                   CLOSE     INPUTBND
024900981126     C*
025000980325     C                   SETON                                            RT
025100981126     C*
025200140116     C*------------------------------------------------------------------------*
025300140116     C* Controlli sul membro che non fa il CLLE chiamante
025400140116     C*------------------------------------------------------------------------*
025500140116     C     CtrlMbr       BEGSR
025600140116     C*
025700140116     C* controllo che il membro in elaborazione non sia una copia di salvataggio
025800140116     C                   IF        %subst(pIn_MLNAME :
025900140116     C                               %checkr(' ' : pIn_MLNAME)) = '£'
026000140116     C                   RETURN
026100140116     C                   ENDIF
026200140116     C*
026300140117     C* controllo che la descrizione del membro in elaborazione contenga la stringa inviata
026400140117     C* (siccome la stringa di selezione è in maiuscolo, maiuscolizzo anche la descrizione)
026500140116     C                   IF        pIn_SelTxt <> *blank
026600140117     C                   IF        %scan(%trim(pIn_SelTxt) :
026700140117     C                              %xlate(minu:maiu:pIn_MLMTXT)) = 0
026800140116     C                   RETURN
026900140116     C                   ENDIF
027000140116     C                   ENDIF
027100140116     C*
027200140116     C                   ENDSR
