000100000111     H DECEDIT('0,') DATEDIT(*DMY.)
000200161020      *-------------------------------------------------------------------------------------------*
000300161020      *  Questo pgm scansiona un sorgente RPGLE o SQLRPGLE e cerca quello che gli è stato chiesto
000400161020      *  non per singola riga ma per ISTRUZIONE, cioè tenendo conto delle possibilità che dà l'RPG
000500161020      *  di andare a capo, sia in formato fisso che free.
000600161020      *-------------------------------------------------------------------------------------------*
000700981203     FINPUTBND  IF   F  112        DISK    USROPN
000800140117     FWFTIDTMP  O    E             DISK
000900981201     D*-------------------------------------------------------------------------------------------*
001000140116     D* posizioni trovate
001100140116     D $PosOkDS        DS            20
001200140116     D $PosOk                         1A   dim(20)
001300140117     D*---------                                  d                         ----------------------*
001400161020     D* spezza la specifica a formato fisso
001500140117     DTotSpecDS        DS           112
001600140117     D SRCSEQ                  1      6  0                                      N° specifica
001700140117     D SRCDAT                  7     12  0                                      Data modifica spec.
001800161020     D SRCNUM                 13     17
001900140117     D SRCTIP                 18     18                                         Tipo specifica
002000140117     D SRCPAR                 19     23                                         '*' per commento
002100140117     D SRCFat1                24     37                                         Codice operativo
002200140117     D SRCCodOp               38     47
002300140117     D SRCFat2                48     61
002400140117     D SRCFat2Ext             48    112
002500140117     D SRCRisu                62     75
002600140117     D SRCSpeO                42     55
002700161020     D SRCFree                13    112
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
005200161020
005300161021         dcl-c cFree CONST('**FREE');
005400161021     D* cFree           c                   CONST('**FREE')
005500161020
005600140114     D*---------                                                            ----------------------*
005700000110     I* Tracciato record file sorgente in lettura
005800981126     IINPUTBND  NS  01
005900140117     I                                  1  112  TotSpec                         Specifica
006000000110     I*---------                                                            ----------------------*
006100981201     C*-------------------------------------------------------------------------------------------*
006200981201     C*  FLUSSO PRINCIPALE                                                                        *
006300981201     C*-------------------------------------------------------------------------------------------*
006400981201     C*
006500161020     C* differenza tra formato fisso e free
006600161020     C* - un sorgente è completamente free format se a riga 1 colonna 1 c'è scritto **FREE
006700161020     C*   in questo caso gli statement possono iniziare da colonna 1
006800161020     C* - altrimenti gli statement iniziano a colonna 7
006900161020     C* - sia nel free format non del tutto free (senza **FREE) sia nel formato fisso, a colonna 6
007000161020     C*   c'è qualcosa che ci aiuta a capire di che specifica si tratta
007100161020     C* - le spec-C avranno il carattere C a colonna 6 se si tratta di formato fisso, altrimenti
007200161020     C*   colonna 6 sarà blank
007300161020     C* - le altre specifiche un carattere specifico a colonna 6 se si tratta di formato fisso,
007400161020     C*   altrimenti colonna 6 sarà blank e ci sarà un cmd ad identificare di cosa si tratta:
007500161020     C*   . CTL-OPT for control specs (H)
007600161020     C*   . DCL-F for file specs (F)
007700161020     C*   . DCL-S, DCL-DS, DCL-SUBF, DCL-C, DCL-PR, DCL-PI, DCL-PARM for data specs (D)
007800161020     C*   . DCL-PROC for procedure specs (P)
007900161020     C*
008000981201     C     *ENTRY        PLIST
008100140116     C                   PARM                    pIn_Lib
008200140116     C                   PARM                    pIn_File
008300140116     C                   PARM                    pIn_PosRSD
008400140116     C                   PARM                    pIn_PosIND
008500140116     C                   PARM                    pIn_PosLOD
008600140116     C                   PARM                    pIn_PosPRD
008700140116     C                   PARM                    pIn_PosCAD
008800140312     C                   PARM                    pIn_PoTNOTE
008900140312     C                   PARM                    pIn_PoTNOTI
009000150407     C                   PARM                    pIn_VAORSR
009100150407     C                   PARM                    pIn_VAOTER
009200150407     C                   PARM                    pIn_VAORFA
009300140116     C                   PARM                    pIn_SelTxt
009400140116     C                   PARM                    pIn_MLNAME
009500140116     C                   PARM                    pIn_MLMTXT
009600140116     C                   PARM                    pOut_Esito
009700981201     C*
009800140116     C* controlli sul membro che non fa il CLLE
009900140116     C                   EXSR      CtrlMbr
010000140114     C*
010100981126     C                   OPEN      INPUTBND
010200000111     C                   Move      *Off          $SpecH            1
010300140120     C                   CLEAR                   $PosOkDS
010400990604     C*
010500981126     C                   READ      INPUTBND                               21
010600990603     C                   MOVE      *In21         $Esci
010700161020       wTotalFree = *off;
010800990604     C*
010900990604     C                   DoW       $Esci <> *On
011000140116     C*
011100140116     C* maiuscolizzo la specifica
011200140117     C                   EVAL      TotSpecDS = %xlate(minu:maiu:TotSpec)
011300161020
011400161020       // se è la prima riga
011500161020       if SrcSeq = 1;
011600161020         // controllo se da colonna 1 c'è scritto **FREE
011700161020         if %subst(SrcFree : 1 : 6) = cFree;
011800161020           wTotalFree = *on;
011900161020         endif;
012000161020       endif;
012100161020
012200161020       // se non siamo in caso di total free
012300161020       if wTotalFree = *off;
012400161020
012500161021       // per capire che tipo specifica è guardo a colonna 6
012600161020       select;
012700161020        when SrcTip = 'H';
012800161020        when SrcTip = 'F';
012900161020        when SrcTip = 'D';
013000161020        when SrcTip = 'P';
013100161020        when SrcTip = 'C';
013200161020        when SrcTip = *blank;
013300161020         // siamo in free format, ma devo capire sempre che specifica è
013400161020       endsl;
013500161020
013600000111     C*
013700000111     C* 2) specifiche 'C' (non asteriscate !!!)
013800000110     C                   If        (SrcTip = 'C' or SrcTip = 'c')
013900000110     C                             and %SubSt(SrcPar:1:1) <> '*'
014000000111     C*
014100140116     C* - cerco VABRSD
014200140120     C                   IF        pIn_PosRSD >0                   and  (
014300140117     C                             %scan('VABRSD':SRCFat2Ext) <> 0 and
014400140120     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
014500140120     C                             %scan('VINDTA:'+ %trim(%char(pIn_PosRSD))
014600140117     C                              + ':' :
014700140117     C                              SRCFat2Ext) <> 0                     OR
014800140120     C                             %scan('VABRSD':SRCFat2Ext) <> 0 and
014900140120     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
015000140117     C                             %scan('VINDTA:'+ %editc(pIn_PosRSD:'X')
015100140117     C                              + ':' :
015200140117     C                              SRCFat2Ext) <> 0                    )
015300140116     C                   EVAL      $PosOk(1) = 'X'
015400140120     C                   ENDIF
015500140116     C*
015600140116     C* - cerco VABIND
015700140120     C                   IF        pIn_PosIND >0                   and  (
015800140117     C                             %scan('VABIND':SRCFat2Ext) <> 0 and
015900140120     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
016000140120     C                             %scan('VINDTA:'+ %trim(%char(pIn_PosIND))
016100140120     C                              + ':' :
016200140120     C                              SRCFat2Ext) <> 0                     OR
016300140120     C                             %scan('VABIND':SRCFat2Ext) <> 0 and
016400140120     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
016500140120     C                             %scan('VINDTA:'+ %editc(pIn_PosIND:'X')
016600140120     C                              + ':' :
016700140117     C                              SRCFat2Ext) <> 0                    )
016800140116     C                   EVAL      $PosOk(2) = 'X'
016900140120     C                   ENDIF
017000140116     C*
017100140116     C* - cerco VABLOD
017200140120     C                   IF        pIn_PosLOD >0                   and  (
017300140117     C                             %scan('VABLOD':SRCFat2Ext) <> 0 and
017400140120     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
017500140120     C                             %scan('VINDTA:'+ %trim(%char(pIn_PosLOD))
017600140120     C                              + ':' :
017700140120     C                              SRCFat2Ext) <> 0                     OR
017800140120     C                             %scan('VABLOD':SRCFat2Ext) <> 0 and
017900140120     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
018000140120     C                             %scan('VINDTA:'+ %editc(pIn_PosLOD:'X')
018100140120     C                              + ':' :
018200140117     C                              SRCFat2Ext) <> 0                    )
018300140116     C                   EVAL      $PosOk(3) = 'X'
018400140120     C                   ENDIF
018500140116     C*
018600140116     C* - cerco VABPRD
018700140120     C                   IF        pIn_PosPRD >0
018800140120     C                   ENDIF
018900140116     C*
019000140116     C* - cerco VABCAD
019100140120     C                   IF        pIn_PosCAD >0
019200140120     C                   ENDIF
019300140312     C*
019400140312     C* - cerco VATNOT_E
019500140312     C                   IF        pIn_PoTNOTE>0                   and  (
019600140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
019700140312     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
019800140312     C                             %scan('VINDTA:'+ %trim(%char(pIn_PoTNOTE))
019900140312     C                              + ':' :
020000140312     C                              SRCFat2Ext) <> 0                     OR
020100140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
020200140312     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
020300140312     C                             %scan('VINDTA:'+ %editc(pIn_PoTNOTE:'X')
020400140312     C                              + ':' :
020500140312     C                              SRCFat2Ext) <> 0                    )
020600140312     C                   EVAL      $PosOk(4) = 'X'
020700140312     C                   ENDIF
020800140312     C*
020900140312     C* - cerco VATNOT_I
021000140312     C                   IF        pIn_PoTNOTI>0                   and  (
021100140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
021200140312     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
021300140312     C                             %scan('VINDTA:'+ %trim(%char(pIn_PoTNOTI))
021400140312     C                              + ':' :
021500140312     C                              SRCFat2Ext) <> 0                     OR
021600140312     C                             %scan('VATNOT':SRCFat2Ext) <> 0 and
021700140312     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
021800140312     C                             %scan('VINDTA:'+ %editc(pIn_PoTNOTI:'X')
021900140312     C                              + ':' :
022000140312     C                              SRCFat2Ext) <> 0                    )
022100140312     C                   EVAL      $PosOk(5) = 'X'
022200140312     C                   ENDIF
022300150407     C*
022400150407     C* - cerco VAORSR
022500150407     C                   IF        pIn_VAORSR >0                   and  (
022600150407     C                             %scan('VAORSR':SRCFat2Ext) <> 0 and
022700150407     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
022800150407     C                             %scan('VINDTA:'+ %trim(%char(pIn_VAORSR))
022900150407     C                              + ':' :
023000150407     C                              SRCFat2Ext) <> 0                     OR
023100150407     C                             %scan('VAORSR':SRCFat2Ext) <> 0 and
023200150407     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
023300150407     C                             %scan('VINDTA:'+ %editc(pIn_VAORSR:'X')
023400150407     C                              + ':' :
023500150407     C                              SRCFat2Ext) <> 0                    )
023600150407     C                   EVAL      $PosOk(6) = 'X'
023700150407     C                   ENDIF
023800150407     C*
023900150407     C* - cerco VAOTER
024000150407     C                   IF        pIn_VAOTER >0                   and  (
024100150407     C                             %scan('VAOTER':SRCFat2Ext) <> 0 and
024200150407     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
024300150407     C                             %scan('VINDTA:'+ %trim(%char(pIn_VAOTER))
024400150407     C                              + ':' :
024500150407     C                              SRCFat2Ext) <> 0                     OR
024600150407     C                             %scan('VAOTER':SRCFat2Ext) <> 0 and
024700150407     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
024800150407     C                             %scan('VINDTA:'+ %editc(pIn_VAOTER:'X')
024900150407     C                              + ':' :
025000150407     C                              SRCFat2Ext) <> 0                    )
025100150407     C                   EVAL      $PosOk(7) = 'X'
025200150407     C                   ENDIF
025300150407     C*
025400150407     C* - cerco VAORFA
025500150407     C                   IF        pIn_VAORFA >0                   and  (
025600150407     C                             %scan('VAORFA':SRCFat2Ext) <> 0 and
025700150407     C* - assieme alla stringa 'VINDTA:yyy:' con la posizione passata senza 0
025800150407     C                             %scan('VINDTA:'+ %trim(%char(pIn_VAORFA))
025900150407     C                              + ':' :
026000150407     C                              SRCFat2Ext) <> 0                     OR
026100150407     C                             %scan('VAORFA':SRCFat2Ext) <> 0 and
026200150407     C* - OPPURE alla stringa 'VINDTA:yyy:' con la posizione passata con 0
026300150407     C                             %scan('VINDTA:'+ %editc(pIn_VAORFA:'X')
026400150407     C                              + ':' :
026500150407     C                              SRCFat2Ext) <> 0                    )
026600150408     C                   EVAL      $PosOk(8) = 'X'
026700150407     C                   ENDIF
026800140312     C*
026900000110     C                   EndIf
027000000110     C*
027100990604     C                   READ      INPUTBND                               21
027200990604     C                   MOVE      *In21         $Esci
027300981203     C                   EndDo
027400990604     C*
027500140116     C* Se alla fine del sorgente ho trovato qualcosa, scrivo il risultato sul file
027600140116     C                   If        $PosOkDS <> *blank
027700000110     C*
027800140116     C                   Clear                   WFTID000
027900000110     C*
028000140117     C                   MoveL     pIn_MLNAME    TIDNome
028100140117     C                   MoveL     pIn_MLMTXT    TIDDescr
028200140116     C                   MoveL     pIn_File      TIDFile
028300140116     C                   MoveL     pIn_Lib       TIDLib
028400140116     C                   MoveL     $PosOkDS      TIDResult
028500000110     C*
028600140116     C                   Write     WFTID000
028700140120     C                   FEOD      WFTIDTMP
028800000110     C*
028900000110     C                   EndIf
029000000110     C*
029100981126     C                   CLOSE     INPUTBND
029200981126     C*
029300980325     C                   SETON                                            RT
029400981126     C*
029500140116     C*------------------------------------------------------------------------*
029600140116     C* Controlli sul membro che non fa il CLLE chiamante
029700140116     C*------------------------------------------------------------------------*
029800140116     C     CtrlMbr       BEGSR
029900140116     C*
030000140116     C* controllo che il membro in elaborazione non sia una copia di salvataggio
030100140116     C                   IF        %subst(pIn_MLNAME :
030200140116     C                               %checkr(' ' : pIn_MLNAME)) = '£'
030300140116     C                   RETURN
030400140116     C                   ENDIF
030500140116     C*
030600140117     C* controllo che la descrizione del membro in elaborazione contenga la stringa inviata
030700140117     C* (siccome la stringa di selezione è in maiuscolo, maiuscolizzo anche la descrizione)
030800140116     C                   IF        pIn_SelTxt <> *blank
030900140117     C                   IF        %scan(%trim(pIn_SelTxt) :
031000140117     C                              %xlate(minu:maiu:pIn_MLMTXT)) = 0
031100140116     C                   RETURN
031200140116     C                   ENDIF
031300140116     C                   ENDIF
031400140116     C*
031500140116     C                   ENDSR
031600161021
031700161024       //----------------------------------------------------------------------*
031800161021        begsr fixedFormat_spec_C_Statement;
031900161024       //----------------------------------------------------------------------*
032000161021
032100161021          // è stato definito che questa è una specifica C a formato fisso
032200161024          // si vuole creare un'unica istruzione su cui fare la ricerca.
032300161024          // esistono
032400161024          // es.:
032500161021     C* N01              EVAL      a = %subst(
032600161021     C*                                b : 1
032700161021     C*                                : 3)
032800161024          // in pratica se la riga successiva non commentata non ha un codice operativo,
032900161024          // la si potrebbe unire in una unica istruzione;
033000161024          // es.:
033100161021      * N01              EVAL      a = %subst(    b : 1    : 3)
033200161021
033300161021
033400161021     C  N01              EVAL      a = 'prova +
033500161021     C                                  prova +
033600161021     C                                  prova'
033700161021
033800161021     C  N01              EVAL      a = 'prova prova prova'
033900161024
034000161024
034100161024          // questa NO
034200161024     C  N01              EVAL      a = 123    +
034300161024     C                                 b      +
034400161024     C                                 c
034500161021
034600161021
034700161021
034800161021
034900161021
035000161021
035100161021
035200161021
035300161021        endsr;
