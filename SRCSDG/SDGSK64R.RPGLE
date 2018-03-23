000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*----------------------------------------------------*
000400000000     H*
000500000000     H*         ------------------------------------------------
000600160205     H*                       Certificazione Unica
000700000000     H*                         STAMPA SU MODULO
000800000000     H*         ------------------------------------------------
000900000000     F*----------------------------------------------------*
001000000000     F* File contenente i dati da trasmettere all'agenzia
001100150204     FSDGCUW00F IF   E             DISK    UsrOpn
001200150224R169 F* Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
001300150224R169 FANSIF01L  IF   E           K DISK
001400000000     F*----------------
001500150204     FSDGSK64P  O    E             PRINTER OFLIND(*IN39)
001600000000     D*----------------------------------------------------
001700000000     D* Parametri ricevuti
001800150204     D SDGSK64DS     E DS
001900000000     D*-------------
002000000000     D* Dati di ambiente ottenuti da SDGSOCDR
002100000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
002200000000     D*-------------
002300000000     D* DS Interna per dati di output di SDGSOCDR
002400000000     D XSOCDS          S           1000
002500000000     D*----------------
002600000000     D* Reperimento nome PGM
002700000000     D STATUS         SDS           333
002800000000     D  DSPGM            *PROC
002900000000     D*------------
003000000000     D* Passaggio Parametri
003100000000     D KPJBA         E DS
003200150224R169 D*----------------
003300150224  "  D* Richiamo a SDGTABDR
003400150224  "  D SDGTABDS      E DS                  inz
003500150224  "  D*----------------
003600150224  "  D DSInterm        DS                  INZ
003700150224  "  D $IdProd                       16A
003800150224  "  D $FisInt                       16A
003900150224  "  D $ImpTrs                        1A
004000150224  "  D $FirInt                       30A
004100150224  "  D $ImpDta                       10A
004200150224  "  D $FisSog                       16A
004300150224  "  D $CarSog                        2A
004400150224  "  D $CogSog                       24A
004500150224  "  D $NomSog                       20A
004600150224  "  D $FisDic                       11A
004700150224  "  D $DtFirDic                     10A
004800150224R169 D $FirDic                       30A
004900000000     D*-------------
005000000000     D DATIREC         DS
005100000000     D  TREC                   1      1
005200000000     D*-------------
005300000000     D DtGMA1          DS
005400150209     D  T0                     1      2
005500150209     D  GMAGiorno1             2      3
005600150209     D  T1                     4      6
005700150209     D  GMAMese1               7      8
005800150209     D  T2                     9     10
005900150209     D  GMAAnno1              11     14
006000000000     D*-------------
006100000000     D DtGMA           DS
006200150205     D  GMAGiorno              1      2
006300150205     D  GMAMese                3      4
006400150205     D  GMAAnno                5      8
006500150323R170 D*-------------
006600150323R170 D SDGSK61DSB    E DS
006700150107     D*-------------
006800150204     D SDGSK61DSD    E DS
006900150107     D*-------------
007000150204     D SDGSK61DSH    E DS
007100150224R169 D*----------------
007200150224  "  D* tabelle delle nature giuridiche
007300150224  "  D ANGG16DS      E DS                  INZ
007400150224  "  D*----------------
007500150224  "  D* Ds flag ansif
007600150224  "  D                 DS
007700150224  "  D  SocSogg               31     38
007800150224  "  D  WrkFlgOpe              1     50
007900150224R169 D*----------------
008000150205     D* tabella per i dati NON posizionali
008100150205     D Max$I           C                   CONST(100)
008200150205     D Skiera          S             24    DIM(Max$I)
008300150205     D $I              S              3S 0
008400150304     D $C              S              3S 0
008500150205     D*-------------
008600150205     D DsCampo         DS
008700150205     D  Nome                          8
008800150205     D  Valore                       16
008900150205     D*----------------
009000150224R169 D  Stringa        S            100
009100150224  "  D  SavNome        S                   like(Nome)
009200150224  "  D*----------------
009300150224  "  D  Sifirma        S              1
009400150224R169 D WrKNatGiu       S              2A
009500150113     D*----------------
009600150113     D* Valori assunti
009700150113     D XMPEDS        E DS                  INZ
009800150113     D XMPEDS1       E DS                  INZ
009900150113     D*-------------
010000000000     C*----------------------------------------------------*
010100000000     C*                MAIN LINE PROGRAM
010200000000     C*----------------------------------------------------*
010300000000     C*
010400000000     C* inizializzazione variabili
010500000000     C                   EXSR      INZVAR
010600150224     C
010700150224R169 C* reperimento dati generali
010800150224R169 C                   EXSR      RepDatiGen
010900150224R169 C*
011000150205     C                   OPEN      SDGCUW00F
011100000000     C*
011200000000     C                   Seton                                        39
011300000000     C*
011400000000     C* -----------------------------------------------------
011500000000     C* Prima lettura
011600150204     C                   read      SDGCUW00F                              90
011700150205     C*
011800000000     C     *IN90         DOWEQ     *OFF
011900000000     C*
012000150205     C                   MoveL     CUWREC        DATIREC
012100000000     C*
012200000000     C                   SELECT
012300150318R170 C* ---------------------------
012400150318 "   C* Tipo Rec.  B
012500150318 "   C                   WHEN      TREC       = 'B' and
012600150318 "   C                             TipoStpK64 = '2'
012700150318 "   C                   Exsr      ElaRecB
012800150318 "   C                   Write     SK64TRB
012900150318R170 C                   leave
013000000000     C* ---------------------------
013100150205     C* Tipo Rec.  D
013200150205     C                   WHEN      TREC    = 'D'
013300150205     C                   Exsr      ElaRecD
013400000000     C* ---------------------------
013500150205     C* Tipo Rec.  H
013600150205     C                   WHEN      TREC    = 'H'
013700150112     C*
013800150205     C                   Exsr      ElaRecH
013900150318R170 C                   if        TipoStpK64 = '1'
014000150205     C                   Write     SK64TRD
014100150205     C                   Write     SK64TRH
014200150318R170 C                   endif
014300000000     C*
014400000000     C* ---------------------------
014500000000     C*
014600000000     C                   EndSl
014700000000     C*
014800150205     C                   read      SDGCUW00F                              90
014900000000     C                   EndDo
015000000000     C*
015100150205     C                   close     SDGCUW00F
015200000000     C*
015300000000     C* fine programma
015400000000     C                   Exsr      EndPgm
015500000000     C*
015600150323R170 C************************************************************
015700150304  "  C* Tipo REC B
015800150304  "  C************************************************************
015900150323R170 C     ElaRecB       BEGSR
016000150304     C*
016100150304     C                   MoveL     CUWREC        SDGSK61DSB
016200150304     C*
016300150304     C                   Clear                   SK64TRB
016400150304     C*
016500150304     C* Codice fiscale del fornitore
016600150323     C                   Z-add     *Zeros        $c
016700150323      /Free
016800150323        For $c = 2 by 2 to 32;
016900150323          %Subst(BCfi: $c-1: 2) = %Subst(K61BCfi: %Int($c/2): 1) + ' ';
017000150323        EndFor;
017100150323      /End-Free
017200150304     C*
017300150304     C* Tipo di comunicazione
017400150304     C                   If        K61BCANN = *on
017500150304     C                   Eval      Bcann = 'X'
017600150304     C                   Endif
017700150304     C*
017800150304     C                   If        K61BCSos = *on
017900150304     C                   Eval      Bcsos = 'X'
018000150304     C                   Endif
018100170216R250 C* Eventi eccezionali
018200170216R250 C                   Eval      BEventi = K61BEve
018300150304     C*
018400150304     C* Dati relativi al sostituto
018500150304     C                   Eval      BcfiSos = K61BCFI
018600150304     C                   Eval      Bcogno  = K61BCogno
018700150304     C                   Eval      BNome   = K61BNome
018800150323     C                   If        Bcogno = *blanks
018900150323     C                   Eval      Bcogno  = K61BDenomi
019000150323     C                   Endif
019100150304     C                   Eval      BTel    = K61BTel
019200150304     C                   Eval      BEmail  = K61BEmail
019300150304     C*
019400150304     C* Dati relativi al rappresentante firmatario
019500150304     C                   Eval      BCdfis1 = K61BCdfis1
019600150304     C                   Eval      BCarr1  = K61BCarr1
019700150304     C*
019800150323     C                   If        K61Bcdfis2 <> *all'0' and
019900150323     C                             K61Bcdfis2 <> *blanks
020000150305     C                   Do        11            $C
020100150313     C                   Eval      BCdFis2 = %trimr(BCdFis2) + ' ' +
020200150304     C                                    %subst(K61BCdfis2: $C: 1)
020300150304     C                   EndDO
020400150323     C                   Endif
020500150304     C*
020600150304     C                   Eval      BCogno1 = K61BCogno1
020700150304     C                   Eval      BNome1  = K61BNome1
020800150304     C*
020900150304     C* Firma della comunicazione
021000160201R170 X***                Eval      BNrDip  = K61BNrDip
021100150304     C                   Eval      BNrAut  = K61BNrAut
021200150304     C                   IF        K61BQuaCT = *on
021300150304     C                   Eval      BQuaCT  = 'X'
021400150304     C                   Endif
021500150304     C                   If        K61Bfirma = *on
021600150304     C                   Movel     $Firdic       BFirma
021700150304     C                   Endif
021800150304     C*
021900150304     C* Impegno alla trasmissione telematica
022000150304     C                   Eval      BCFINT = K61BCfint
022100160301R216 C                   Movel     K61BImpe      BFirIntX
022200150304     C*
022300150305     C                   If        K61BDTImpe <> *all'0' and
022400150305     C                             K61BDTImpe <> *blanks
022500150304     C                   Move      K61BDTImpe    DtGMA
022600150313     C                   eval      BDtImpe  = ' ' + GMAGiorno + ' ' +
022700150313     C                                        GMAMEse + ' ' + GMAAnno
022800150305     C                   Endif
022900150304     C*
023000150304     C                   If        K61BFirInt = *on
023100160301R216 X***                Movel     'X'           BFirIntX
023200150304     C                   Movel     $FirInt       BFirInt
023300150304     C                   Endif
023400150323R170 C*
023500150323R170 C                   ENDSR
023600000000     C************************************************************
023700150205     C* Tipo REC D
023800000000     C************************************************************
023900150205     C     ElaRecD       BEGSR
024000000000     C*
024100150205     C                   MoveL     CUWREC        SDGSK61DSD
024200150205     C*
024300150205     C                   MoveA     K61DDATI      Skiera
024400150205     C*
024500150205     C                   Clear                   SavNome
024600150205     C                   Clear                   SK64TRD
024700150205     C                   Clear                   Stringa
024800150205     C*
024900150227R172 C* Reperisco l'anno dal nome del file
025000150227  "  C                   eval      DAnno  = '20' + %subst(FileK64:5:2)
025100150227R172 c*
025200150205     C                   do        max$i         $I
025300150205     C                   Movel     SKiera($I)    DsCampo
025400150205     C*
025500150209     C                   If        Nome = *blanks
025600150209     C                   leave
025700150209     C                   Endif
025800150205     C*
025900150205     C                   Select
026000150205     C* Dati Anagrafici relativi al datore di lavoro
026100150205     C                   When      Nome = 'DA001001'
026200150205     C*
026300150205     C                   if        Nome <> SavNome
026400150205     C                   Eval      Stringa = Valore
026500150205     C                   Else
026600150205     C*
026700150206     C                   If        %Checkr(' ':stringa) < %size(DA001001)
026800160202     C                   Eval      stringa=%Trimr(Stringa) +
026900150206     C                             %triml(%subst(Valore:2))
027000150205     C                   Endif
027100150205     C*
027200150205     C                   Endif
027300150205     C*
027400150205     C                   Movel     Stringa       DA001001
027500150205     C*
027600150205     C                   When      Nome = 'DA001002'
027700150205     C*
027800150205     C                   if        Nome <> SavNome
027900150205     C                   Eval      Stringa = Valore
028000150205     C                   Else
028100150205     C*
028200150206     C                   If        %Checkr(' ':stringa) < %size(DA001002)
028300160202     C                   Eval      stringa=%Trimr(Stringa) +
028400150206     C                             %triml(%subst(Valore:2))
028500150205     C                   Endif
028600150205     C*
028700150205     C                   Endif
028800150205     C*
028900150205     C                   Movel     Stringa       DA001002
029000150302R172 X*ex R169 Se persona giuridica valorizzo anche gli altri campi anagrafici
029100150302 "   X*ex R169 che altrimenti non lo sarebbero.
029200150302 "   X*ex R169           if        TPNG16      = '2'  or
029300150302 "   X*ex R169                     TPNG16      = '3'
029400150302 "   X*ex R169           Movel(p)  XSCCIT        DA001004
029500150302 "   X*ex R169           Movel(p)  XSCPRV        DA001005
029600150302 "   X*ex R169           Movel(p)  XSCCAP        DA001006
029700150302 "   X*ex R169           Movel(p)  XSCVIA        DA001007
029800150302R172 X*ex R169           Endif
029900150205     C*
030000150205     C                   When      Nome = 'DA001003'
030100150205     C*
030200150205     C                   if        Nome <> SavNome
030300150205     C                   Eval      Stringa = Valore
030400150205     C                   Else
030500150205     C*
030600150206     C                   If        %Checkr(' ':stringa) < %size(DA001003)
030700160202     C                   Eval      stringa=%Trimr(Stringa) +
030800150206     C                             %triml(%subst(Valore:2))
030900150206     C                   Endif
031000150205     C*
031100150205     C                   Endif
031200150205     C*
031300150205     C                   Movel     Stringa       DA001003
031400150205     C*
031500150205     C                   When      Nome = 'DA001004'
031600150205     C*
031700150205     C                   if        Nome <> SavNome
031800150205     C                   Eval      Stringa = Valore
031900150205     C                   Else
032000150205     C*
032100150206     C                   If        %Checkr(' ':stringa) < %size(DA001004)
032200160202     C                   Eval      stringa=%Trimr(Stringa) +
032300150206     C                             %triml(%subst(Valore:2))
032400150206     C                   Endif
032500150205     C*
032600150205     C                   Endif
032700150205     C*
032800150205     C                   Movel     Stringa       DA001004
032900150205     C*
033000150205     C                   When      Nome = 'DA001005'
033100150205     C*
033200150205     C                   Movel(p)  Valore        DA001005
033300150205     C*
033400150205     C                   When      Nome = 'DA001006'
033500150205     C*
033600150205     C                   Movel(p)  Valore        DA001006
033700150205     C*
033800150205     C                   When      Nome = 'DA001007'
033900150205     C*
034000150205     C                   if        Nome <> SavNome
034100150205     C                   Eval      Stringa = Valore
034200150205     C                   Else
034300150205     C*
034400150206     C                   If        %Checkr(' ':stringa) < %size(DA001007)
034500160202     C                   Eval      stringa=%Trimr(Stringa) +
034600150206     C                             %triml(%subst(Valore:2))
034700150206     C                   Endif
034800150205     C*
034900150205     C                   Endif
035000150205     C*
035100150205     C                   Movel     Stringa       DA001007
035200150205     C*
035300150205     C                   When      Nome = 'DA001008'
035400150205     C*
035500150205     C                   if        Nome <> SavNome
035600150205     C                   Eval      Stringa = Valore
035700150205     C                   Else
035800150205     C*
035900150206     C                   If        %Checkr(' ':stringa) < %size(DA001008)
036000160202     C                   Eval      stringa=%Trimr(Stringa) +
036100150206     C                             %triml(%subst(Valore:2))
036200150206     C                   Endif
036300150205     C*
036400150205     C                   Endif
036500150205     C*
036600150205     C                   Movel     Stringa       DA001008
036700150205     C*
036800150205     C                   When      Nome = 'DA001009'
036900150205     C*
037000150205     C                   if        Nome <> SavNome
037100150205     C                   Eval      Stringa = Valore
037200150205     C                   Else
037300150205     C*
037400150206     C                   If        %Checkr(' ':stringa) < %size(DA001009)
037500160202     C                   Eval      stringa=%Trimr(Stringa) +
037600150206     C                             %triml(%subst(Valore:2))
037700150206     C                   Endif
037800150205     C*
037900150205     C                   Endif
038000150205     C*
038100150205     C                   Movel     Stringa       DA001009
038200150205     C*
038300150205     C                   When      Nome = 'DA001010'
038400150205     C*
038500150205     C                   Movel(P)  Valore        DA001010
038600150205     C*
038700150205     C                   When      Nome = 'DA001011'
038800150205     C*
038900150205     C                   Movel(P)  Valore        DA001011
039000150205     C*
039100150205     C                   When      Nome = 'DA002001'
039200150205     C*
039300150205     C                   if        Nome <> SavNome
039400150205     C                   Eval      Stringa = Valore
039500150205     C                   Else
039600150205     C*
039700150206     C                   If        %Checkr(' ':stringa) < %size(DA002001)
039800160202     C                   Eval      stringa=%Trimr(Stringa) +
039900150206     C                             %triml(%subst(Valore:2))
040000150206     C                   Endif
040100150205     C*
040200150205     C                   Endif
040300150205     C*
040400150205     C                   Movel     Stringa       DA002001
040500160201R170 C*
040600160201 "   C* DATI RELATIVI AL PERCETTORE
040700160201R170 C* Cognome
040800150205     C                   When      Nome = 'DA002002'
040900150205     C*
041000150205     C                   if        Nome <> SavNome
041100150205     C                   Eval      Stringa = Valore
041200150205     C                   Else
041300150205     C*
041400150206     C                   If        %Checkr(' ':stringa) < %size(DA002002)
041500160202     C                   Eval      stringa=%Trimr(Stringa) +
041600150206     C                             %triml(%subst(Valore:2))
041700150206     C                   Endif
041800150205     C*
041900150205     C                   Endif
042000150205     C*
042100150205     C                   Movel     Stringa       DA002002
042200150205     C*
042300160201R170 C* Nome
042400150205     C                   When      Nome = 'DA002003'
042500150205     C*
042600150205     C                   if        Nome <> SavNome
042700150205     C                   Eval      Stringa = Valore
042800150205     C                   Else
042900150205     C*
043000150206     C                   If        %Checkr(' ':stringa) < %size(DA002003)
043100160202     C                   Eval      stringa=%Trimr(Stringa) +
043200150206     C                             %triml(%subst(Valore:2))
043300150206     C                   Endif
043400150205     C*
043500150205     C                   Endif
043600150205     C*
043700150205     C                   Movel     Stringa       DA002003
043800150205     C*
043900160201R170 C* Sesso
044000150205     C                   When      Nome = 'DA002004'
044100150205     C*
044200150205     C                   Movel(P)  Valore        DA002004
044300150205     C*
044400160201R170 C* Data di nascita
044500150205     C                   When      Nome = 'DA002005'
044600150205     C*
044700150205     C                   Move(P)   Valore        DtGMA
044800150323R170 X****               Movel     GMAGiorno     GMAGiorno1
044900150323R170 X****               Movel     GMAMese       GMAMese1
045000150323R170 X****               Movel     GMAAnno       GMAAnno1
045100150323R170 X****               Movel(P)  DtGMA1        Da002005
045200150313     C                   eval      Da002005 = ' ' + GMAGiorno + ' ' +
045300150313     C                                        GMAMEse + ' ' + GMAAnno
045400150205     C*
045500160201R170 C* Comune di nascita
045600150205     C                   When      Nome = 'DA002006'
045700150205     C*
045800150205     C                   if        Nome <> SavNome
045900150205     C                   Eval      Stringa = Valore
046000150205     C                   Else
046100150205     C*
046200150206     C                   If        %Checkr(' ':stringa) < %size(DA002006)
046300160202     C                   Eval      stringa=%Trimr(Stringa) +
046400150206     C                             %triml(%subst(Valore:2))
046500150206     C                   Endif
046600150205     C*
046700150205     C                   Endif
046800150205     C*
046900150205     C                   Movel     Stringa       DA002006
047000150205     C*
047100160201R170 C* Provincia di nascita
047200150205     C                   When      Nome = 'DA002007'
047300150205     C*
047400150205     C                   Movel(P)  Valore        DA002007
047500150205     C*
047600150205     C                   When      Nome = 'DA002008'
047700150205     C*
047800160201R170 C* Categorie particolari
047900150402R170 X****               Movel(P)  Valore        DA002008
048000150402R170 C                   eval      Da002008 = %trim(Valore)
048100150205     C*
048200150205     C                   When      Nome = 'DA002009'
048300150205     C*
048400160201R170 C* Eventi eccezionali
048500150402R170 X****               Move(P)   Valore        DA002009
048600150402R170 C                   eval      Da002009 = %trim(Valore)
048700150205     C*
048800150205     C                   When      Nome = 'DA002010'
048900150205     C*
049000160201R170 C* Casi di esclusione dalla precompilata
049100150402R170 X****               Move(P)   Valore        DA002010
049200150402R170 C                   eval      Da002010 = %trim(Valore)
049300150402     C*
049400160202R170 X*** Domicilio fiscale al 01/01/2014
049500160202R170 C* Domicilio fiscale anno precedente
049600150205     C                   When      Nome = 'DA002020'
049700150205     C*
049800150205     C                   if        Nome <> SavNome
049900150205     C                   Eval      Stringa = Valore
050000150205     C                   Else
050100150205     C*
050200150206     C                   If        %Checkr(' ':stringa) < %size(DA002020)
050300160202     C                   Eval      stringa=%Trimr(Stringa) +
050400150206     C                             %triml(%subst(Valore:2))
050500150206     C                   Endif
050600150205     C*
050700150205     C                   Endif
050800150205     C*
050900150205     C                   Movel     Stringa       DA002020
051000150205     C*
051100150205     C                   When      Nome = 'DA002021'
051200150205     C*
051300150205     C                   Movel(P)  Valore        DA002021
051400150205     C*
051500150205     C                   When      Nome = 'DA002022'
051600150205     C*
051700150205     C                   Movel(P)  Valore        DA002022
051800180123R279 C* Fusione comuni
051900180123 "   C*
052000180123 "   C                   When      Nome = 'DA002023'
052100180123 "   C*
052200180123R279 C                   Movel(P)  Valore        DA002023
052300160202R170 X*** Domicilio fiscale al 01/01/2015
052400160202R170 C* Domicilio fiscale anno corrente
052500180123R279 X***                When      Nome = 'DA002023'
052600180123R279 C                   When      Nome = 'DA002024'
052700150205     C*
052800150205     C                   if        Nome <> SavNome
052900150205     C                   Eval      Stringa = Valore
053000150205     C                   Else
053100150205     C*
053200180123R279 X***                If        %Checkr(' ':stringa) < %size(DA002023)
053300180123R279 C                   If        %Checkr(' ':stringa) < %size(DA002024)
053400160202     C                   Eval      stringa=%Trimr(Stringa) +
053500150206     C                             %triml(%subst(Valore:2))
053600150206     C                   Endif
053700150205     C*
053800150205     C                   Endif
053900150205     C*
054000180123R279 X***                Movel     Stringa       DA002023
054100180123R279 C                   Movel     Stringa       DA002024
054200150205     C*
054300180123R279 X***                When      Nome = 'DA002024'
054400180123R279 C                   When      Nome = 'DA002025'
054500150205     C*
054600180123R279 X***                Movel(P)  Valore        DA002024
054700180123R279 C                   Movel(P)  Valore        DA002025
054800150205     C*
054900180123R279 X***                When      Nome = 'DA002025'
055000180123R279 C                   When      Nome = 'DA002026'
055100150205     C*
055200180123R279 X***                Movel(P)  Valore        DA002025
055300180123R279 C                   Movel(P)  Valore        DA002026
055400180123R279 X***ExR250 Fusione comuni
055500180123 "   X***ExR250          When      Nome = 'DA002026'
055600180123 "   X***ExR250
055700180123R279 X***ExR250          Movel(P)  Valore        DA002026
055800180123R279 C* Fusione comuni
055900180123 "   C                   When      Nome = 'DA002027'
056000180123 "   C*
056100180123R279 C                   Movel(P)  Valore        DA002027
056200150205     C*
056300150205     C* Dati relativi al rappresentante (CF)
056400150205     C*
056500150205     C                   When      Nome = 'DA002030'
056600150205     C*
056700150205     C                   Movel(P)  Valore        DA002030
056800150205     C*
056900150205     C* Riservato ai percipienti esteri
057000150205     C*
057100150205     C                   When      Nome = 'DA002040'
057200150205     C*
057300150205     C                   Movel(P)  Valore        DA002040
057400150205     C*
057500150205     C                   When      Nome = 'DA002041'
057600150205     C*
057700150205     C                   if        Nome <> SavNome
057800150205     C                   Eval      Stringa = Valore
057900150205     C                   Else
058000150205     C*
058100150206     C                   If        %Checkr(' ':stringa) < %size(DA002041)
058200160202     C                   Eval      stringa=%Trimr(Stringa) +
058300150206     C                             %triml(%subst(Valore:2))
058400150206     C                   Endif
058500150205     C*
058600150205     C                   Endif
058700150205     C*
058800150205     C                   Movel     Stringa       DA002041
058900150205     C*
059000150205     C                   When      Nome = 'DA002042'
059100150205     C*
059200150205     C                   if        Nome <> SavNome
059300150205     C                   Eval      Stringa = Valore
059400150205     C                   Else
059500150205     C*
059600150206     C                   If        %Checkr(' ':stringa) < %size(DA002042)
059700160202     C                   Eval      stringa=%Trimr(Stringa) +
059800150206     C                             %triml(%subst(Valore:2))
059900150206     C                   Endif
060000150205     C*
060100150205     C                   Endif
060200150205     C*
060300150205     C                   Movel     Stringa       DA002042
060400150205     C*
060500150205     C                   When      Nome = 'DA002043'
060600150205     C*
060700160201R170 X****               Move(P)   Valore        DA002043
060800160201R170 C                   if        %trim(valore) <> *blank
060900160201 "   C                   eval      DA002043 = 'X'
061000160201 "   C                   endif
061100160201 "   C*
061200160201 "   C                   When      Nome = 'DA002044'
061300160201 "   C*
061400160201R170 C                   Move(P)   Valore        DA002044
061500150205     C* Firma unica
061600150205     C                   When      Nome = 'DA003001'
061700150205     C*
061800150205     C                   Move(P)   Valore        DtGMA
061900150323R170 X****               Movel     GMAGiorno     GMAGiorno1
062000150323R170 X****               Movel     GMAMese       GMAMese1
062100150323R170 X****               Movel     GMAAnno       GMAAnno1
062200150323R170 X****               Movel(P)  DtGMA1        Da003001
062300150313     C                   eval      Da003001 = ' ' + GMAGiorno + ' ' +
062400150313     C                                        GMAMEse + ' ' + GMAAnno
062500150205     C*
062600150205     C                   When      Nome = 'DA003002'
062700150205     C*
062800150224R169 C                   Move(P)   Valore        SiFirma
062900150224 "   C* Reperisco la firma dai dati generali
063000150224 "   C                   If        SiFirma = *on
063100150224 "   C****               Move(P)   Valore        DA003002
063200150224 "   C                   Movel     $Firdic       DA003002
063300150224R169 C                   Endif
063400150205     C*
063500150205     C                   Endsl
063600150205     C*
063700150205     C                   Movel     Nome          SavNome
063800150205     C*
063900150205     C                   Enddo
064000150205     C*
064100000000     C                   ENDSR
064200000000     C************************************************************
064300150205     C* Tipo REC H
064400000000     C************************************************************
064500150205     C     ElaRecH       BEGSR
064600150205     C*
064700150205     C                   MoveL     CUWREC        SDGSK61DSH
064800150205     C*
064900150205     C                   MoveA     K61HDATI      Skiera
065000150205     C*
065100150205     C                   Clear                   SavNome
065200150205     C                   Clear                   SK64TRH
065300160202R170 C                   Clear                   Stringa
065400150205     C*
065500150205     C* Codice Fiscale del percipiente
065600150205     C                   Movel(p)  K61HCFP       CFISCP
065700150205     C* Progressivo del modulo
065800150205     C                   Move(P)   K61HPRGMOD    PRGMOD
065900150205     C*
066000150205     C                   do        max$i         $I
066100150205     C                   Movel     SKiera($I)    DsCampo
066200150209     C*
066300150209     C                   If        Nome = *blanks
066400150209     C                   leave
066500150209     C                   Endif
066600150205     C*
066700150205     C                   Select
066800150205     C* Causale
066900150205     C                   When      Nome = 'AU001001'
067000150205     C                   Movel(p)  Valore        AU001001
067100150205     C* Anno
067200150205     C                   When      Nome = 'AU001002'
067300150205     C                   Move(p)   Valore        AU001002
067400150205     C* Anticipazione
067500150205     C                   When      Nome = 'AU001003'
067600150318R170 X****               Move(p)   Valore        AU001003
067700160201R170 C                   if        %trim(valore) <> *blank
067800150318 "   C                   eval      Au001003 = 'X'
067900150318R170 C                   endif
068000150205     C* Ammontare lordo corrisposto
068100150205     C                   When      Nome = 'AU001004'
068200150223R169 C****               Move(p)   Valore        AU001003
068300150223R169 C                   Move(p)   Valore        AU001004
068400150205     C* Somme non soggette a ritenuta
068500150205     C                   When      Nome = 'AU001005'
068600150205     C                   Move(p)   Valore        AU001005
068700150205     C* Codice
068800150205     C                   When      Nome = 'AU001006'
068900150205     C                   Move(p)   Valore        AU001006
069000150205     C* Altre somme non soggette a ritenuta
069100150205     C                   When      Nome = 'AU001007'
069200150205     C                   Move(p)   Valore        AU001007
069300150205     C* Imponibile
069400150205     C                   When      Nome = 'AU001008'
069500150205     C                   Move(p)   Valore        AU001008
069600150205     C* Ritenute a titolo di acconto
069700150205     C                   When      Nome = 'AU001009'
069800150205     C                   Move(p)   Valore        AU001009
069900150205     C* Ritenute a titolo di imposta
070000150205     C                   When      Nome = 'AU001010'
070100150205     C                   Move(p)   Valore        AU001010
070200150205     C* Ritenute sospese
070300150205     C                   When      Nome = 'AU001011'
070400150205     C                   Move(p)   Valore        AU001011
070500150205     C* Addizionale regionale a titolo di acconto
070600150205     C                   When      Nome = 'AU001012'
070700150205     C                   Move(p)   Valore        AU001012
070800150205     C* Addizionale regionale a titolo di imposta
070900150205     C                   When      Nome = 'AU001013'
071000150205     C                   Move(p)   Valore        AU001013
071100150205     C* Addizionale regionale sospesa
071200150205     C                   When      Nome = 'AU001014'
071300150205     C                   Move(p)   Valore        AU001014
071400150205     C* Addizionale comunale a titolo di acconto
071500150205     C                   When      Nome = 'AU001015'
071600150205     C                   Move(p)   Valore        AU001015
071700150205     C* Addizionale comunale a titolo di imposta
071800150205     C                   When      Nome = 'AU001016'
071900150205     C                   Move(p)   Valore        AU001016
072000150205     C* Addizionale comunale sospesa
072100150205     C                   When      Nome = 'AU001017'
072200150205     C                   Move(p)   Valore        AU001017
072300150205     C* Imponibile anni precedenti
072400150205     C                   When      Nome = 'AU001018'
072500150205     C                   Move(p)   Valore        AU001018
072600150205     C* Ritenute operate anni precedenti
072700150205     C                   When      Nome = 'AU001019'
072800150205     C                   Move(p)   Valore        AU001019
072900160201R170 X***** Contributi previdenziali a carico del soggetto erogante
073000160201R170 C* Spese rimborsate
073100150205     C                   When      Nome = 'AU001020'
073200150205     C                   Move(p)   Valore        AU001020
073300160201R170 X****** Contributi previdenziali a carico del percipiente
073400160201R170 C* Ritenute rimborsate
073500150205     C                   When      Nome = 'AU001021'
073600150205     C                   Move(p)   Valore        AU001021
073700160201R170 X*** Spese rimborsate
073800160201R170 X***                When      Nome = 'AU001022'
073900160201R170 X***                Move(p)   Valore        AU001022
074000160201R170 X*** Ritenute rimborsate
074100160201R170 X***                When      Nome = 'AU001023'
074200160201R170 X***                Move(p)   Valore        AU001023
074300160201     C*
074400160201R170 C* DATI PREVIDENZIALI
074500160201 "   C* Codice fiscale ente previdenziale
074600160201 "   C                   When      Nome = 'AU001029'
074700160201 "   C                   Move(p)   Valore        AU001029
074800160201 "   C* Denominazione ente previdenziale
074900160201 "   C                   When      Nome = 'AU001030'
075000160202 "   C                   if        Nome <> SavNome
075100160202 "   C                   Eval      Stringa = Valore
075200160202 "   C                   Else
075300160202 "   C                   If        %Checkr(' ':stringa) < %size(AU001030)
075400160202 "   C                   Eval      stringa=%Trimr(Stringa) +
075500160202 "   C                             %triml(%subst(Valore:2))
075600160202 "   C                   Endif
075700160202 "   C                   Endif
075800170216R170 C                   Movel     Stringa       AU001030
075900170216R250 X****Ex R170 Ente previdenziale
076000170216R250 X****Ex R170        When      Nome = 'AU001031'
076100170216R250 X****Ex R170        Move(p)   Valore        AU001031
076200170216R170 C* Codice azienda
076300160201 "   C                   When      Nome = 'AU001032'
076400160201 "   C                   Move(p)   Valore        AU001032
076500160201 "   C* Categoria
076600160201 "   C                   When      Nome = 'AU001033'
076700160201 "   C                   Move(p)   Valore        AU001033
076800160201 "   C* Contributi previdenziali a carico del soggetto erogante
076900160201 "   C                   When      Nome = 'AU001034'
077000160201 "   C                   Move(p)   Valore        AU001034
077100160201 "   C* Contributi previdenziali a carico del percipiente
077200160201 "   C                   When      Nome = 'AU001035'
077300160201 "   C                   Move(p)   Valore        AU001035
077400160201 "   C* Altri contributi
077500160201 "   C                   When      Nome = 'AU001036'
077600160201 "   C                   if        %trim(valore) <> *blank
077700160201 "   C                   eval      AU001036 = 'X'
077800160201 "   C                   endif
077900160201 "   C* Importo altri contributi
078000160201 "   C                   When      Nome = 'AU001037'
078100160201 "   C                   Move(p)   Valore        AU001037
078200160201 "   C* Contributi dovuti
078300160201 "   C                   When      Nome = 'AU001038'
078400160201 "   C                   Move(p)   Valore        AU001038
078500160201 "   C* Contributi versati
078600160201 "   C                   When      Nome = 'AU001039'
078700160201 "   C                   Move(p)   Valore        AU001039
078800160201 "   C*
078900160201 "   C* SOMME LIQUIDATE A SEGUITO DI PIGNORAMENTO
079000160201 "   C* Codice fiscale debitore principale
079100160201 "   C                   When      Nome = 'AU001101'
079200160201 "   C                   Move(p)   Valore        AU001101
079300160201 "   C* Somme erogate
079400160201 "   C                   When      Nome = 'AU001102'
079500160201 "   C                   Move(p)   Valore        AU001102
079600160201 "   C* Ritenute operate
079700160201 "   C                   When      Nome = 'AU001103'
079800170216R170 C                   Move(p)   Valore        AU001103
079900170216R250 X**** Ex R170 Ritenute non operate
080000170216R250 C* Somme erogate non tassate
080100170216R170 C                   When      Nome = 'AU001104'
080200170216R250 X**** Ex R170       if        %trim(valore) <> *blank
080300170216 "   X**** Ex R170       eval      Au001104 = 'X'
080400170216R250 X**** Ex R170       endif
080500170216R250 C                   Move(p)   Valore        AU001104
080600170216R170 C* Riservata al soggetto erogatore delle somme
080700160201 "   C                   When      Nome = 'AU001105'
080800160201 "   C                   Move(p)   Valore        AU001105
080900160201 "   C                   When      Nome = 'AU001106'
081000160201 "   C                   Move(p)   Valore        AU001106
081100160201 "   C                   When      Nome = 'AU001107'
081200160201 "   C                   Move(p)   Valore        AU001107
081300170216R170 C                   When      Nome = 'AU001108'
081400170216R250 X**** Ex R170       if        %trim(valore) <> *blank
081500170216 "   X**** Ex R170       eval      Au001108 = 'X'
081600170216R250 X**** Ex R170       endif
081700170216R250 C                   Move(p)   Valore        AU001108
081800170216R170 C*
081900160201 "   C* SOMME CORRISPOSTE A TITOLO D'INDENNITÀ
082000160201 "   C* - INDENNITÀ DI ESPROPRIO E RISARCIMENTO DEL DANNO
082100160201 "   C*   - Somme corrisposte
082200160201 "   C                   When      Nome = 'AU001131'
082300160201 "   C                   Move(p)   Valore        AU001131
082400160201 "   C*   - Ritenute operate
082500160201 "   C                   When      Nome = 'AU001132'
082600160201 "   C                   Move(p)   Valore        AU001132
082700160201 "   C* - ALTRE INDENNITÀ E INTERESSI
082800160201 "   C*   - Somme corrisposte
082900160201 "   C                   When      Nome = 'AU001133'
083000160201 "   C                   Move(p)   Valore        AU001133
083100160201 "   C*   - Ritenute operate
083200160201 "   C                   When      Nome = 'AU001134'
083300160201 "   C                   Move(p)   Valore        AU001134
083400160201 "   C* - INDENNITÀ DI ESPROPRIO E RISARCIMENTO DEL DANNO
083500160201 "   C*   - Somme corrisposte
083600160201 "   C                   When      Nome = 'AU001135'
083700160201 "   C                   Move(p)   Valore        AU001135
083800160201 "   C*   - Ritenute operate
083900160201 "   C                   When      Nome = 'AU001136'
084000160201 "   C                   Move(p)   Valore        AU001136
084100160201 "   C* - ALTRE INDENNITÀ E INTERESSI
084200160201 "   C*   - Somme corrisposte
084300160201 "   C                   When      Nome = 'AU001137'
084400160201 "   C                   Move(p)   Valore        AU001137
084500160201 "   C*   - Ritenute operate
084600160201 "   C                   When      Nome = 'AU001138'
084700160201R170 C                   Move(p)   Valore        AU001138
084800150205     C*
084900150205     C                   ENDSL
085000160202R170 C*
085100160202R170 C                   Movel     Nome          SavNome
085200150205     C*
085300150205     C                   ENDDO
085400150116     C*
085500000000     C                   ENDSR
085600000000     C************************************************************
085700000000     C* Inizializzazioni variabili
085800000000     C************************************************************
085900000000     C     InzVar        BegSr
086000000000     C*
086100150204     C                   MoveL     KPJBU         SDGSK64DS
086200000000     C*
086300150205     C                   Eval      ERRK64      = *Off
086400000000     C*
086500000000     C                   MoveL     'SOC001'      TIPXSC
086600150205     C                   MoveL     SOCIETAK64    SOCXSC
086700000000     C*
086800000000     C                   Exsr      RepSoc
086900000000     C                   If        RTNXSC     <> '1'
087000000000     C                   MoveL     XSOCDS        SOC001
087100000000     C                   ElSe
087200000000     C                   Exsr      EndPgm
087300000000     C                   EndIf
087400150224R169 C*
087500150224  "  C                   Move      XScSoc        SIFSocieta
087600150224  "  C     K01SIF01      Chain     ANSIF01L                           21
087700150224  "  C  N21              Move      SIFFlgOpe     WrkFlgOpe
087800150224R169 C   21              Move      *Blanks       SocSogg
087900000000     C*
088000150107     C                   EndSr
088100150224R169 C/EJECT
088200150224 "   C************************************************************
088300150224 "   C* Reperimento dati generali
088400150224 "   C************************************************************
088500150224R169 C     RepDatiGen    BEGSR
088600150224     C*
088700150224     C* preparo i parametri di chiamata al driver
088800150224     C                   Clear                   XMPEDS
088900150224     C                   EVAL      TpChiamata = '01'
089000150224     C                   EVAL      NChiavi = 5
089100150224     C                   EVAL      VLogica = 2
089200150224     C*
089300150224     C* preparo le chiavi per la chiamata al driver
089400150224     C                   Clear                   XMPEDS1
089500150224     C                   MoveL     XSCSOC        DSMPESOC
089600150224     C                   MoveL     'SDGSK60R R'  DSMPEPGM
089700150224     C                   MoveL     *Blanks       DSMPEPRF
089800150224     C* Si reperiscono i dati di default a seconda delle modalità con
089900150224     C* cui il pgm è usato
090000150224     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
090100150224     C* MPETIP = 'PV'  pgm richiamato da menù
090200150224     C* MPETIP = 'PB'  x lancio Batch
090300150224     C                   MoveL     'PB'          DSMPETIP
090400150224     C*
090500150224     C                   CALLB     'XMPE'
090600150224     C                   PARM                    XMPEDS
090700150224     C                   PARM                    XMPEDS1
090800150224     C*
090900150224     C     Risultato     IfEq      '0'
091000150224     C                   MoveL     DSMPEPAR      DSInterm
091100150224     C                   endif
091200150224     C*
091300150224     C                   Exsr      RepDatiPer
091400150224     C*
091500150224R169 C                   ENDSR
091600150224 "   C/EJECT
091700150224 "   C************************************************************
091800150224 "   C* Reperimento dati persona
091900150224 "   C************************************************************
092000150224R169 C     RepDatiPer    BEGSR
092100150224     C* reperisco la natura giuridica del contribuente
092200150224     C                   MoveL     XScNCR        WrkNatGiu
092300150224     C                   EXSR      RepNatGiu
092400150224     C*
092500150224R169 C                   ENDSR
092600150224 "   C/EJECT
092700150224 "   C******************************************************
092800150224 "   C* Reperimento della natura giuridica
092900150224 "   C******************************************************
093000150224R169 C     RepNatGiu     BEGSR
093100150224     C*
093200150224     C                   Reset                   SDGTABDS
093300150224     C*
093400150224     C                   MOVE      '1'           XTBRIC
093500150224     C                   MOVE      SocietaK64    XTBAZI
093600150224     C                   MOVE      *ZERO         XTBKEY
093700150224     C                   MOVE      'G16'         XTBCOD
093800150224     C                   MOVE      WrkNatGiu     XTBKEY
093900150224     C                   CALL      'SDGTABDR'
094000150224     C                   PARM                    SDGTABDS
094100150224     C                   if        XTBERR      = *Off
094200150224     C                   MoveL     XTBUNI        ANGG16DS
094300150224     C                   endif
094400150224     C*
094500150224     C                   ENDSR
094600150224R169 C/EJECT
094700150224     C************************************************************
094800150224     C* OPERAZIONI INIZIALI
094900000000     C************************************************************
095000000000     C     *INZSR        BegSr
095100000000     C*
095200000000     C* Reperimento parametri
095300000000     C*
095400150107     C     *ENTRY        PLIST
095500150107     C                   PARM                    KPJBA
095600000000     C*
095700000000     C                   EndSr
095800000000     C************************************************************
095900000000     C* DEFINIZIONE KLIST
096000000000     C************************************************************
096100000000     C     DefKlist      BegSr
096200000000     C*
096300000000     C* KLIST
096400150224R169 C*
096500150224 "   C* ANSIF01L
096600150224 "   C     K01SIF01      KLIST
096700150224R169 C                   KFLD                    SIFSocieta
096800000000     C*
096900000000     C                   EndSr
097000000000     C************************************************************
097100000000     C* Reperimento dati società
097200000000     C************************************************************
097300000000     C     RepSoc        BegSr
097400000000     C*
097500000000     C                   CALL      'SDGSOCDR'
097600000000     C                   PARM                    TIPXSC            6
097700000000     C                   PARM                    SOCXSC            3
097800000000     C                   PARM                    CDSXSC            9 0
097900000000     C                   PARM                    MODXSC            3
098000000000     C                   PARM      *Blanks       RTNXSC            1
098100000000     C                   PARM                    XSOCDS
098200000000     C                   PARM                    KPJBA
098300000000     C*
098400000000     C                   EndSr
098500000000     C************************************************************
098600000000     C* FINE PROGRAMMA
098700000000     C************************************************************
098800000000     C     EndPgm        BegSr
098900000000     C*
099000150204     C                   MoveL     SDGSK64DS     KPJBU
099100000000     C                   SETON                                        LR
099200000000     C*
099300000000     C                   RETURN
099400000000     C*
099500000000     C                   EndSr
099600000000     C************************************************************
