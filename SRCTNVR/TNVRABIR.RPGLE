000100970722     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970722     H*PARMS CVTOPT(*DATETIME)
000300970722     H DECEDIT('0,')
000400970722     ***********************************************************************
000500000417      * Legge file coordinate bancarie di Proj e lo confronta con i codici
000600000417      *            ABI-CAB indicati sui file di Orion Cnind e Cnclp
000700970722      *
000800970722     ***********************************************************************
000900000417     FCNIND00F  IF   E           K DISK
001000970722     F*-------
001100000417     FCNCLP00F  IF   E           K DISK
001200970722     F*-------
001300000417     FANCBA01L  IF   E           K DISK
001400000417     F*-------
001500000417     FQsysprt   O    F  132        printer oflind(*inoa)
001600000417     ***********************************************************************
001700000417     D* DS
001800000417     ***********************************************************************
001900000417     D KPJBA         E DS
002000970722     C*****************************************************************
002100970722     C* FLUSSO PRINCIPALE
002200970722     C*****************************************************************
002300000417      * imposto la societa dalla Kpjba
002400000417     C                   move      knsif         wcomo4            4
002500000417     C                   movel     wcomo4        ksoc
002600000417      * imposto capoconto cba a *blanks + flag cli/for = 'C'
002700000417     C                   Movel     *blanks       Kkcc1
002800000418     C                   Movel     *zeros        Kksc1
002900000417     C                   Movel     *blanks       Kentita
003000000417     C                   Movel     'C'           Kclifor
003100000417      * imposto dati x lettura Cnind
003200000417     C                   Z-add     1             kkut
003300000417     C                   Z-add     151           kkcc
003400000417      * Loop da cnind
003500000417     C     Kind          Chain     Cnind00f                           30
003600000417     C                   Dow       not *in30
003700000417     C                   move      indkSc        kksc1
003800000417      * controllo se c'è tipo appoggio a blanks e se si confronto Cnind
003900000417      * con esso
004000000418     C                   if        indabi<>0 or indcab<>0
004100000417     C                   movel     *blanks       ktpappo
004200000417     C     Kcba          Chain     Ancba01L                           31
004300000417      * se non trovato cerco con tipo appoggio 'CA'
004400000417     C                   if        *in31
004500000417     C                   movel     'CA'          ktpappo
004600000417     C     Kcba          Chain     Ancba01L                           31
004700000417     C                   end
004800000417      * se non trovato alcun appoggio segnalo errore
004900000417     C                   if        *in31
005000000417     C   oa              except    teserr
005100000417     C                   except    erro01
005200000417      * ... altrimenti verifico se sono uguali
005300000417     C                   else
005400000417     C                   move      indabi        wabi
005500000417     C                   move      indcab        wcab
005600000417     C                   if        wabi<>cbaabi or wcab<>cbacab
005700000417     C   oa              except    teserr
005800000417     C                   except    erro02
005900000417     C                   end
006000000417     C                   end
006100000418     C                   end
006200000417      * leggo cnind successivo
006300000417     C     Kind          reade     cnind00f                               30
006400000417     C                   end
006500000417      * Loop da cnclp
006600000418     C     Kclp          Chain     Cnclp00f                           30
006700000417     C                   Dow       not *in30
006800000417     C                   move      clpkSc        kksc1
006900000417      * controllo se c'è tipo appoggio a 'CA' e se si confronto Cnind
007000000417      * con esso
007100000418     C                   if        clpabi<>0 or clpcab<>0
007200000417     C                   movel     'CA'          ktpappo
007300000417     C     Kcba          Chain     Ancba01L                           31
007400000417      * se non trovato cerco con tipo appoggio *blanks
007500000417     C                   if        *in31
007600000417     C                   movel     *blanks       ktpappo
007700000417     C     Kcba          Chain     Ancba01L                           31
007800000417     C                   end
007900000417      * se non trovato alcun appoggio segnalo errore
008000000417     C                   if        *in31
008100000417     C   oa              except    teserr
008200000417     C                   except    erro03
008300000417      * ... altrimenti verifico se sono uguali
008400000417     C                   else
008500000417     C                   move      clpabi        wabi
008600000417     C                   move      clpcab        wcab
008700000417     C                   if        wabi<>cbaabi or wcab<>cbacab
008800000417     C   oa              except    teserr
008900000417     C                   except    erro04
009000000417     C                   end
009100000417     C                   end
009200000418     C                   end
009300000417      * leggo cnind successivo
009400000417     C     Kclp          reade     cnclp00f                               30
009500000417     C                   end
009600970722      *
009700970722     C                   SETON                                        LR
009800970722     C*------------------------------------------------------------*
009900970722     C* Operazioni iniziali
010000970722     C*------------------------------------------------------------*
010100970722     C     *INZSR        BEGSR
010200970722     C*
010300000417     C*  Reperisco come parametro di entrata la società da controllare
010400970722     C     *ENTRY        PLIST
010500000418     C                   PARM                    KPJBA
010600000417     C* Definizione klist
010700000417     C     kcba          Klist
010800000417     C                   Kfld                    Ksoc
010900000417     C                   Kfld                    Kclifor
011000000417     C                   Kfld                    Kkcc1
011100000417     C                   Kfld                    Kksc1
011200000417     C                   Kfld                    Kentita
011300000417     C                   Kfld                    Ktpappo
011400000417      *
011500000417     C     Kind          Klist
011600000417     C                   Kfld                    Kkut
011700000417     C                   Kfld                    Kkcc
011800000417      *
011900000417     C     Kclp          Klist
012000000417     C                   Kfld                    Kkut
012100000417     C                   Kfld                    Kkcc
012200000417      * Definizione campi
012300000417     C     *like         define    cbasocieta    Ksoc
012400000417     C     *like         define    cbaclifor     Kclifor
012500000417     C     *like         define    cbakcc        Kkcc1
012600000417     C     *like         define    cbaksc        Kksc1
012700000417     C     *like         define    cbaentita     Kentita
012800000417     C     *like         define    cbatpappo     ktpappo
012900000417     C     *like         define    cbasocieta    wsoc
013000000417     C     *like         define    cbaabi        wabi
013100000417     C     *like         define    cbacab        wcab
013200000417     C     *like         define    indkut        Kkut
013300000417     C     *like         define    indkcc        Kkcc
013400000417      *
013500970722     C                   ENDSR
013600000417     o*--------------------------------------------------------------*
013700000417     o* Stampa
013800000417     o*-------------------------------------------------------------*
013900000417     OQsysPrt   E            Teserr           01
014000000417     o                                           66 'Elenco clienti con coor'
014100000417     o                                              'dinate bancarie errate'
014200000417     O          E            Teserr         1 03
014300000417     o                                           10 'Cd.cliente'
014400000417     o                                           20 'tp.appoggio'
014500000417     o                                           32 'coord. Proj'
014600000417     o                                           42 'Orion'
014700000417     o                                           56 'errore'
014800000417     O          E            erro01         1
014900000417     O                       kksc1                9
015000000417     o                                           76 'non esiste appogio su Proj'
015100000417     o                                          100 'corrispondente x Cnind'
015200000417     O          E            erro02         1
015300000417     O                       kksc1                9
015400000417     O                       cbatpappo           15
015500000417     O                       cbaabi              26
015600000417     O                       cbacab              32
015700000417     O                       indabi              40
015800000417     O                       indcab              46
015900000417     o                                           76 'tipo appoggio Proj diverso'
016000000417     o                                          101 'da quello orion x Cnind'
016100000417     O          E            erro03         1
016200000417     O                       kksc1                9
016300000417     o                                           76 'non esiste appogio su Proj'
016400000417     o                                          100 'corrispondente x Cnclp'
016500000417     O          E            erro04         1
016600000417     O                       kksc1                9
016700000417     O                       cbatpappo           15
016800000417     O                       cbaabi              26
016900000417     O                       cbacab              32
017000000417     O                       clpabi              40
017100000417     O                       clpcab              46
017200000417     o                                           76 'tipo appoggio Proj diverso'
017300000417     o                                          101 'da quello orion x Cnclp'
