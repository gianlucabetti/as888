000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*----------------------------------------------------*
000400000000     H*
000500000000     H*         ------------------------------------------------
000600150107     H*                Comunicazione Dichiarazione d'intento
000700000000     H*                         STAMPA SU MODULO
000800000000     H*         ------------------------------------------------
000900000000     F*----------------------------------------------------*
001000000000     F* File contenente i dati da trasmettere all'agenzia
001100150107     FSDGDIW00F IF   E             DISK    UsrOpn
001200000000     F*----------------
001300150112     FSDGSJ64P  O    E             PRINTER OFLIND(*IN39)
001400000000     D*----------------------------------------------------
001500000000     D* Parametri ricevuti
001600150112     D SDGSJ64DS     E DS
001700000000     D*-------------
001800000000     D* Dati di ambiente ottenuti da SDGSOCDR
001900000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
002000000000     D*-------------
002100000000     D* DS Interna per dati di output di SDGSOCDR
002200000000     D XSOCDS          S           1000
002300000000     D*----------------
002400000000     D* Reperimento nome PGM
002500000000     D STATUS         SDS           333
002600000000     D  DSPGM            *PROC
002700000000     D*------------
002800000000     D* Passaggio Parametri
002900000000     D KPJBA         E DS
003000000000     D*-------------
003100000000     D DATIREC         DS
003200000000     D  TREC                   1      1
003300000000     D*-------------
003400000000     D DtGMA1          DS
003500000000     D  GMAGiorno1             1      2
003600000000     D  T1                     3      3
003700000000     D  GMAMese1               4      5
003800000000     D  T2                     6      6
003900000000     D  GMAAnno1               7     10
004000000000     D*-------------
004100000000     D DtGMA           DS
004200000000     D  GMAGiorno              1      2  0
004300000000     D  GMAMese                3      4  0
004400000000     D  GMAAnno                5      8  0
004500000000     D*-------------
004600150107     D SDGSJ61DSA    E DS
004700150107     D*-------------
004800150107     D SDGSJ61DSB    E DS
004900150107     D*-------------
005000150107     D SDGSJ61DSZ    E DS
005100150113     D*-------------
005200150113     D $IdProt         S             17A
005300150113     D $IdProtd        S              6A
005400150120     D WNrInter1       S                   Like(NrInte1J64)
005500150120     D WNrInter2       S                   Like(NrInte1J64)
005600150113     D*----------------
005700150113     D* Valori assunti
005800150113     D XMPEDS        E DS                  INZ
005900150113     D XMPEDS1       E DS                  INZ
006000150113     D DSInterm        DS                  INZ
006100150113     D $FisInt                             Like(J61BCFINT  )
006200150113     D $ImpDta                       10A
006300150113     D $FirInt                       30A
006400150113     D $FISSOG                       16A
006500150113     D $FISDIC                       11A
006600150113     D $CARSOG                        2A
006700150113     D $COGSOG                       24A
006800150113     D $NOMSOG                       20A
006900150113     D $SEXRSOG                       1A
007000150113     D $DTNSOG                       10A
007100150113     D $COMSOG                       40A
007200150113     D $PRVSOG                        2A
007300150113     D $TELEFON                      12A
007400150113     D $FIRDIC                       30A
007500150113     D*-------------
007600150113     D DSInterm2       DS                  INZ
007700150113     D $IdProd                       16A
007800150113     D $EMAIL                        69A
007900150212R168 D*-------------
008000150212 "   D Importo16A      S             16A
008100150212 "   D Max$Z           C                   CONST(16)
008200150212 "   D $Z              S              3S 0
008300150212R168 D Sk_Importo      S              1    DIM(Max$Z)
008400150420R180 D WPosInt         S              2  0
008500150420R180 D WPosDec         S              2  0
008600000000     C*----------------------------------------------------*
008700000000     C*                MAIN LINE PROGRAM
008800000000     C*----------------------------------------------------*
008900000000     C*
009000000000     C* inizializzazione variabili
009100000000     C                   EXSR      INZVAR
009200000000     C*
009300150112     C                   open      SDGDIW00F
009400000000     C*
009500000000     C                   Seton                                        39
009600000000     C*
009700000000     C* -----------------------------------------------------
009800000000     C* Prima lettura
009900150112     C                   read      SDGDIW00F                              90
010000000000     C     *IN90         DOWEQ     *OFF
010100000000     C*
010200150107     C                   MoveL     DIWREC        DATIREC
010300000000     C*
010400000000     C                   SELECT
010500000000     C* ---------------------------
010600000000     C* Tipo Rec.  A
010700000000     C                   WHEN      TREC    = 'A'
010800000000     C                   Exsr      ElaRecA
010900000000     C* ---------------------------
011000000000     C* Tipo Rec.  B
011100000000     C                   WHEN      TREC    = 'B'
011200150112     C*
011300150112     C                   MoveL     DIWREC        SDGSJ61DSB
011400150120     C                   movel     NrInte1J64    WNrInter1
011500150120     C                   callb     'XALLINEA'
011600150120     C                   parm                    WNrInter1
011700150120     C                   parm      16            LEN2              5 0
011800150120     C                   parm      '0'           Input             1
011900150120     C                   movel     NrInte2J64    WNrInter2
012000150120     C                   callb     'XALLINEA'
012100150120     C                   parm                    WNrInter2
012200150120     C                   parm      16            LEN2              5 0
012300150120     C                   parm      '0'           Input             1
012400150120     C                   if        J61BNumDic >= WNrInter1 and
012500150120     C                             J61BNumDic <= WNrInter2
012600000000     C                   Exsr      ElaRecB
012700150112     C                   endif
012800000000     C*
012900000000     C* ---------------------------
013000000000     C*
013100000000     C                   EndSl
013200000000     C*
013300150112     C                   read      SDGDIW00F                              90
013400000000     C                   EndDo
013500000000     C*
013600150112     C                   close     SDGDIW00F
013700000000     C*
013800000000     C* fine programma
013900000000     C                   Exsr      EndPgm
014000000000     C*
014100000000     C************************************************************
014200000000     C* Tipo REC A
014300000000     C************************************************************
014400000000     C     ElaRecA       BEGSR
014500000000     C*
014600150107     C                   MoveL     DIWREC        SDGSJ61DSA
014700000000     C*
014800000000     C*
014900000000     C                   ENDSR
015000000000     C************************************************************
015100000000     C* Tipo REC B
015200000000     C************************************************************
015300000000     C     ElaRecB       BEGSR
015400000000     C*
015500150417R180 X****               move      J61BNumDic    BNumDic
015600150417R180 C                   movel     J61BNumDic    BNumDic
015700150120     C* allineamento a dx con blank
015800150417R180 C                   eval      Len2      = %size(BNumDic)
015900150120     C                   callb     'XALLINEA'
016000150120     C                   parm                    BNumDic
016100150417R180 X****               parm      16            LEN2              5 0
016200150417R180 C                   parm                    LEN2              5 0
016300150120     C                   parm      '2'           Input             1
016400150109     C*
016500150109     C* Dati del dichiarante
016600150109     C                   if        J61BCogno <> *blank
016700150109     C                   eval      BDenomi     = J61BCogno
016800150109     C                   else
016900150109     C                   eval      BDenomi     = J61BDenomi
017000150109     C                   endif
017100150109     C                   If        J61BDTNASC    <> *zeros
017200150109     C                   Move      J61BDTNASC    DTGMA
017300150109     C                   Move      GMAAnno       BDTNAA
017400150109     C                   Move      GMAMese       BDTNMM
017500150109     C                   Move      GMAGiorno     BDTNGG
017600150109     C                   EndIf
017700150109     C                   eval      BLUONAS     = J61BLUONAS
017800150109     C                   eval      BPRVNAS     = J61BPRVNAS
017900000000     C*
018000150109     C* DATI RELATIVI AL RAPPRESENTANTE FIRMATARIO DELLA DICHIARAZIONE
018100150109     C                   if        J61BCdFis1 = *zeros
018200150109     C                   eval      J61BCdFis1 = *blank
018300150109     C                   endif
018400150109     C                   if        J61BCdFis2 = *zeros
018500150109     C                   eval      J61BCdFis2 = *blank
018600150109     C                   endif
018700150109     C                   If        J61BDTNAS1    <> *zeros
018800150107     C                   Move      J61BDTNAS1    DTGMA
018900000000     C                   Move      GMAAnno       BDTNAA1
019000000000     C                   Move      GMAMese       BDTNMM1
019100000000     C                   Move      GMAGiorno     BDTNGG1
019200000000     C                   EndIf
019300150109     C                   eval      BLUONAS1    = J61BLUONA1
019400150109     C                   eval      BPRVNAS1    = J61BPRVNA1
019500150109     C*
019600150109     C* RECAPITI
019700150109     C                   eval      BEMail      = J61BEmail
019800150109     C*
019900150109     C* INTEGRATIVA
020000150109     C                   if        J61BCint    = 1
020100150109     C                   eval      BCint       = 'X'
020200150109     C                   endif
020300150113     C*
020400150113     C* PROTOCOLLO DI INVIO
020500150113     C                   if        J61BProt <> 0
020600150113     C                   move      J61BProt      $IdProt
020700150417R180 X****               eval      BProt01 = %subst($IdProt:01:1)
020800150417 "   X****               eval      BProt02 = %subst($IdProt:02:1)
020900150417 "   X****               eval      BProt03 = %subst($IdProt:03:1)
021000150417 "   X****               eval      BProt04 = %subst($IdProt:04:1)
021100150417 "   X****               eval      BProt05 = %subst($IdProt:05:1)
021200150417 "   X****               eval      BProt06 = %subst($IdProt:06:1)
021300150417 "   X****               eval      BProt07 = %subst($IdProt:07:1)
021400150417 "   X****               eval      BProt08 = %subst($IdProt:08:1)
021500150417 "   X****               eval      BProt09 = %subst($IdProt:09:1)
021600150417 "   X****               eval      BProt10 = %subst($IdProt:10:1)
021700150417 "   X****               eval      BProt11 = %subst($IdProt:11:1)
021800150417 "   X****               eval      BProt12 = %subst($IdProt:12:1)
021900150417 "   X****               eval      BProt13 = %subst($IdProt:13:1)
022000150417 "   X****               eval      BProt14 = %subst($IdProt:14:1)
022100150417 "   X****               eval      BProt15 = %subst($IdProt:15:1)
022200150417 "   X****               eval      BProt16 = %subst($IdProt:16:1)
022300150417R168 X****               eval      BProt17 = %subst($IdProt:17:1)
022400150417R180 C                   z-add     *Zeros        $c                3 0
022500150417 "    /Free
022600150417 "      For $c = 2 by 2 to 34;
022700150417 "        %Subst(BProt: $c-1: 2) = %Subst($IdProt: %Int($c/2): 1) + ' ';
022800150417 "      EndFor;
022900150417R180  /End-Free
023000150113     C                   move      J61BProtD     $IdProtD
023100150417R180 X****               eval      BProtD01 = %subst($IdProtD:01:1)
023200150417 "   X****               eval      BProtD02 = %subst($IdProtD:02:1)
023300150417 "   X****               eval      BProtD03 = %subst($IdProtD:03:1)
023400150417 "   X****               eval      BProtD04 = %subst($IdProtD:04:1)
023500150417 "   X****               eval      BProtD05 = %subst($IdProtD:05:1)
023600150417 "   X****               eval      BProtD06 = %subst($IdProtD:06:1)
023700150417R180 C                   z-add     *Zeros        $c                3 0
023800150417 "    /Free
023900150417 "      For $c = 2 by 2 to 12;
024000150417 "        %Subst(BProtD: $c-1: 2) = %Subst($IdProtD: %Int($c/2): 1) + ' ';
024100150417 "      EndFor;
024200150417R180  /End-Free
024300150113     C                   endif
024400150109     C*
024500150109     C* DICHIARAZIONE
024600150109     C                   if        J61BAcq     = 1
024700150109     C                   eval      BAcq        = 'X'
024800150113     C                   else
024900150113     C                   eval      BAcq        = *blank
025000150109     C                   endif
025100150109     C                   if        J61BImp     = 1
025200150109     C                   eval      BImp        = 'X'
025300150113     C                   else
025400150113     C                   eval      BImp        = *blank
025500150109     C                   endif
025600150109     C                   eval      BDMerce     = J61BDMerce
025700150212R168 C*
025800150212 "   C                   if        J61BImpOpe  = *zeros
025900150212 "   C                   clear                   J61BImpOpe
026000150212 "   C                   else
026100150212 "   C                   eval      Importo16A = J61BImpOpe
026200150212 "   C                   exsr      ABlank
026300150420R168 C                   eval      J61BImpOpe = Importo16A
026400150420R180 C                   clear                   IntImpOpe
026500150420 "   C                   clear                   DecImpOpe
026600150420 "   C                   eval      WPosInt = %Scan(',':J61BImpOpe) - 1
026700150420 "   C                   eval      WPosDec = %Scan(',':J61BImpOpe) + 1
026800150420 "   C                   eval      IntImpOpe = %trim(%subst(J61BImpOpe:1
026900150420 "   C                                           :WPosInt))
027000150420 "   C* allineamento a dx con blank
027100150420 "   C                   eval      Len2      = %size(IntImpOpe)
027200150420 "   C                   callb     'XALLINEA'
027300150420 "   C                   parm                    IntImpOpe
027400150420 "   C                   parm                    LEN2              5 0
027500150420 "   C                   parm      '2'           Input             1
027600150420 "   C                   eval      DecImpOpe = %trim(%subst(J61BImpOpe:WPosDec
027700150420R180 C                                           :2))
027800150420R168 C                   endif
027900150212 "   C*
028000150212 "   C                   if        J61BImpCor  = *zeros
028100150212 "   C                   clear                   J61BImpCor
028200150212 "   C                   else
028300150212 "   C                   eval      Importo16A = J61BImpCor
028400150212 "   C                   exsr      ABlank
028500150420R168 C                   eval      J61BImpCor = Importo16A
028600150420R180 C                   clear                   IntImpCor
028700150420 "   C                   clear                   DecImpCor
028800150420 "   C                   eval      WPosInt = %Scan(',':J61BImpCor) - 1
028900150420 "   C                   eval      WPosDec = %Scan(',':J61BImpCor) + 1
029000150420 "   C                   eval      IntImpCor = %trim(%subst(J61BImpCor:1
029100150420 "   C                                           :WPosInt))
029200150420 "   C* allineamento a dx con blank
029300150420 "   C                   eval      Len2      = %size(IntImpCor)
029400150420 "   C                   callb     'XALLINEA'
029500150420 "   C                   parm                    IntImpCor
029600150420 "   C                   parm                    LEN2              5 0
029700150420 "   C                   parm      '2'           Input             1
029800150420 "   C                   eval      DecImpCor = %trim(%subst(J61BImpCor:WPosDec
029900150420R180 C                                           :2))
030000150212R168 C                   endif
030100000000     C*
030200150109     C                   If        J61BDTINP     <> *zeros
030300150107     C                   Move      J61BDTINP     DTGMA
030400000000     C                   Move      GMAAnno       BDTINPAA
030500000000     C                   Move      GMAMese       BDTINPMM
030600000000     C                   Move      GMAGiorno     BDTINPGG
030700000000     C                   EndIf
030800150109     C                   If        J61BDTFIP     <> *zeros
030900150107     C                   Move      J61BDTFIP     DTGMA
031000150107     C                   Move      GMAAnno       BDTFIPAA
031100150107     C                   Move      GMAMese       BDTFIPMM
031200150107     C                   Move      GMAGiorno     BDTFIPGG
031300150107     C                   EndIf
031400150109     C*
031500150109     C* DESTINATARIO DELLA DICHIARAZIONE
031600150109     C                   if        J61BDogana  = 1
031700150109     C                   eval      BDogana     = 'X'
031800150109     C                   endif
031900150109     C                   if        J61BCdFisD = *zeros
032000150109     C                   eval      J61BCdFisD = *blank
032100150109     C                   endif
032200150109     C                   if        J61BPIvaD  = *zeros
032300150109     C                   eval      J61BPIvaD  = *blank
032400150109     C                   endif
032500150109     C                   if        J61BCognoD <> *blank
032600150109     C                   eval      BDenomiD    = J61BCognoD
032700150109     C                   else
032800150109     C                   eval      BDenomiD    = J61BDenomD
032900150109     C                   endif
033000150109     C*
033100150109     C* FIRMA
033200150109     C                   if        J61BFirma   = 1
033300150113     C                   eval      BFirma      = $FirDic
033400150109     C                   endif
033500150109     C*
033600150112     C                   write     SJ64T1
033700150109     C*
033800150112     C                   if        TipoStpJ64  = '2'
033900150109     C* QUADRO A
034000150109     C                   if        J61BTpPlf   = 1
034100150109     C                   eval      BFisso      = 'X'
034200150109     C                   else
034300150109     C                   eval      BMobile     = 'X'
034400150109     C                   endif
034500150109     C                   if        J61BIvaPre  = 1
034600150109     C                   eval      BIvaPre     = 'X'
034700150109     C                   endif
034800150109     C                   if        J61BEsport  = 1
034900150109     C                   eval      BEsport     = 'X'
035000150109     C                   endif
035100150109     C                   if        J61BCeIntr  = 1
035200150109     C                   eval      BCeIntr     = 'X'
035300150109     C                   endif
035400150109     C                   if        J61BCeRsm   = 1
035500150109     C                   eval      BCeRsm      = 'X'
035600150109     C                   endif
035700150109     C                   if        J61BOpeAss  = 1
035800150109     C                   eval      BOpeAss     = 'X'
035900150109     C                   endif
036000150109     C                   if        J61BOpeStr  = 1
036100150109     C                   eval      BOpeStr     = 'X'
036200150109     C                   endif
036300150109     C*
036400150109     C* IMPEGNO ALLA PRESENTAZIONE TELEMATICA
036500150109     C                   If        J61BDTIMPE    <> *zeros
036600150107     C                   Move      J61BDTIMPE    DTGMA
036700150107     C                   Move      GMAAnno       BDTIMPAA
036800150107     C                   Move      GMAMese       BDTIMPMM
036900150107     C                   Move      GMAGiorno     BDTIMPGG
037000150107     C                   EndIf
037100150109     C*
037200150109     C                   if        J61BFirInt  = 1
037300150113     C                   eval      BFirInt     = $FirInt
037400150109     C                   endif
037500150107     C*
037600150112     C                   write     SJ64PLAF
037700150112     C                   endif
037800000000     C*
037900150116     C                   clear                   SJ64T1
038000150116     C                   clear                   SJ64PLAF
038100150116     C*
038200000000     C                   ENDSR
038300150212R168 C******************************************************
038400150212 "   C* Toglie gli zeri non significativi
038500150212R168 C******************************************************
038600150212     C     ABlank        BEGSR
038700150212     C*
038800150212     C                   MoveA     Importo16A    SK_Importo
038900150212     C*
039000150212     C                   do        Max$Z         $Z
039100150212     C*
039200150212     C                   if        Sk_Importo($Z) = ' '   or
039300150212     C                             Sk_Importo($Z) = '0'
039400150212     C                   Move      ' '           Sk_Importo($Z)
039500150212     C                   else
039600150212     C                   leave
039700150212     C                   endif
039800150212     C*
039900150212     C                   enddo
040000150212     C*
040100150212     C                   MoveA     Sk_Importo    Importo16A
040200150212R168 C*
040300150212 "   C                   ENDSR
040400150212R168 C/EJECT
040500150113     C************************************************************
040600150113     C* Reperimento dati Intermediario
040700150113     C************************************************************
040800150113     C     RepDatInter   BEGSR
040900150113     C*
041000150113     C* preparo i parametri di chiamata al driver
041100150113     C                   Clear                   XMPEDS
041200150113     C                   EVAL      TpChiamata = '01'
041300150113     C                   EVAL      NChiavi = 5
041400150113     C                   EVAL      VLogica = 2
041500150113     C*
041600150113     C* preparo le chiavi per la chiamata al driver
041700150113     C                   Clear                   XMPEDS1
041800150113     C                   MoveL     XSCSOC        DSMPESOC
041900150113     C                   MoveL     'SDGSJ60R R'  DSMPEPGM
042000150113     C                   Move      'R'           DSMPEPGM
042100150113     C                   MoveL     *blanks       DSMPEPRF
042200150113     C* Si reperiscono i dati di default a seconda delle modalità con
042300150113     C* cui il pgm è usato
042400150113     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
042500150113     C* MPETIP = 'PV'  pgm richiamato da menù
042600150113     C* MPETIP = 'PB'  x lancio Batch
042700150113     C                   MoveL     'PB'          DSMPETIP
042800150113     C*
042900150113     C                   CALLB     'XMPE'
043000150113     C                   PARM                    XMPEDS
043100150113     C                   PARM                    XMPEDS1
043200150113     C*
043300150113     C     Risultato     IfEq      '0'
043400150113     C                   MoveL     DSMPEPAR      DSInterm
043500150113     C                   EndIf
043600150113     C*
043700150113     C                   EndSR
043800150113     C************************************************************
043900150113     C* Reperimento dati Intermediario
044000150113     C************************************************************
044100150113     C     RepDatInter2  BEGSR
044200150113     C*
044300150113     C* preparo i parametri di chiamata al driver
044400150113     C                   Clear                   XMPEDS
044500150113     C                   EVAL      TpChiamata = '01'
044600150113     C                   EVAL      NChiavi = 5
044700150113     C                   EVAL      VLogica = 2
044800150113     C*
044900150113     C* preparo le chiavi per la chiamata al driver
045000150113     C                   Clear                   XMPEDS1
045100150113     C                   MoveL     XSCSOC        DSMPESOC
045200150113     C                   MoveL     'SDGSJ60R S'  DSMPEPGM
045300150113     C                   Move      'S'           DSMPEPGM
045400150113     C                   MoveL     *blanks       DSMPEPRF
045500150113     C* Si reperiscono i dati di default a seconda delle modalità con
045600150113     C* cui il pgm è usato
045700150113     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
045800150113     C* MPETIP = 'PV'  pgm richiamato da menù
045900150113     C* MPETIP = 'PB'  x lancio Batch
046000150113     C                   MoveL     'PB'          DSMPETIP
046100150113     C*
046200150113     C                   CALLB     'XMPE'
046300150113     C                   PARM                    XMPEDS
046400150113     C                   PARM                    XMPEDS1
046500150113     C*
046600150113     C     Risultato     IfEq      '0'
046700150113     C                   MoveL     DSMPEPAR      DSInterm2
046800150113     C                   EndIf
046900150113     C*
047000150113     C                   EndSR
047100000000     C************************************************************
047200000000     C* Inizializzazioni variabili
047300000000     C************************************************************
047400000000     C     InzVar        BegSr
047500000000     C*
047600150112     C                   MoveL     KPJBU         SDGSJ64DS
047700000000     C*
047800150112     C                   Eval      ERRJ64      = *Off
047900000000     C*
048000000000     C                   MoveL     'SOC001'      TIPXSC
048100150112     C                   MoveL     SOCIETAJ64    SOCXSC
048200000000     C*
048300000000     C                   Exsr      RepSoc
048400000000     C                   If        RTNXSC     <> '1'
048500000000     C                   MoveL     XSOCDS        SOC001
048600000000     C                   ElSe
048700000000     C                   Exsr      EndPgm
048800000000     C                   EndIf
048900150113     C*
049000150113     C* Dati Rappresentante
049100150113     C                   Exsr      RepDatInter
049200150113     C                   Exsr      RepDatInter2
049300000000     C*
049400150107     C                   EndSr
049500000000     C************************************************************
049600000000     C* OPERAZIONI INIZIALI
049700000000     C************************************************************
049800000000     C     *INZSR        BegSr
049900000000     C*
050000000000     C* Reperimento parametri
050100000000     C*
050200150107     C     *ENTRY        PLIST
050300150107     C                   PARM                    KPJBA
050400000000     C*
050500000000     C                   EndSr
050600000000     C************************************************************
050700000000     C* DEFINIZIONE KLIST
050800000000     C************************************************************
050900000000     C     DefKlist      BegSr
051000000000     C*
051100000000     C* KLIST
051200000000     C*
051300000000     C                   EndSr
051400000000     C************************************************************
051500000000     C* Reperimento dati società
051600000000     C************************************************************
051700000000     C     RepSoc        BegSr
051800000000     C*
051900000000     C                   CALL      'SDGSOCDR'
052000000000     C                   PARM                    TIPXSC            6
052100000000     C                   PARM                    SOCXSC            3
052200000000     C                   PARM                    CDSXSC            9 0
052300000000     C                   PARM                    MODXSC            3
052400000000     C                   PARM      *Blanks       RTNXSC            1
052500000000     C                   PARM                    XSOCDS
052600000000     C                   PARM                    KPJBA
052700000000     C*
052800000000     C                   EndSr
052900000000     C************************************************************
053000000000     C* FINE PROGRAMMA
053100000000     C************************************************************
053200000000     C     EndPgm        BegSr
053300000000     C*
053400150112     C                   MoveL     SDGSJ64DS     KPJBU
053500000000     C                   SETON                                        LR
053600000000     C*
053700000000     C                   RETURN
053800000000     C*
053900000000     C                   EndSr
054000000000     C************************************************************
