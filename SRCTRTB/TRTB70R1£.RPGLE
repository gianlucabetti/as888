000100110608     /*END
000200891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000300891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000400931209     F*  KC        FINE                                              *
000500940228     F*  KE        RIPRISTINO                                        *
000600940228     F*  KF        CONFERMA                                          *
000700931209     F*  KL        RITORNO                                           *
000800931209     F*  KQ        ANNULLAMENTO                                      *
000900891030     F*--------------------------------------------------------------*
001000110412     fTNTBE01L  if   e           k disk
001100110412      *
001200110412     FTABEL00F  UF A E           K DISK
001300110412     F*----------
001400940930     FTRTB70D   CF   E             WORKSTN
001500110413       //-------------------------------------------------------------
001600110413       //?COSTANTI                                                    ?
001700110413       //-------------------------------------------------------------
001800110413     d c_ColorGrn      c                   const(x'A0')
001900110413     d c_ColorBlu      c                   const(x'BA')
002000110413     d c_ColorRed      c                   const(x'A8')
002100940418     D*--------------------------------------------------------------*
002200940418     D*   SCHIERE                                                    *
002300940701     D*--------------------------------------------------------------*
002400141119     D ERR             S             70    DIM(12) CTDATA PERRCD(1)
002500940928     D COST            S             16    DIM(3) CTDATA PERRCD(1)
002600940928     D*--------------------------------------------------------------*
002700940929     D CODP            S              2    DIM(30)
002800041116     D C2A             S              3    DIM(250)
002900041116     D D2A             S             25    DIM(250)
003000041116     D T2A             S              1    DIM(250)
003100041116     D T2Z             S              1    DIM(250)
003200940930     D C3A             S              2    DIM(50)
003300940930     D D3A             S             25    DIM(50)
003400940928     D*--------------------------------------------------------------*
003500940928     D VTI             S              1    DIM(5)
003600940930     D VNT             S              1  0 DIM(5)
003700940930     D VEV             S              3    DIM(5)
003800940930     D VDE             S             20    DIM(5)
003900970116     D VTE             S              1    DIM(5)
004000970116     D VOL             S              1    DIM(5)
004100940928     D*--------------------------------------------------------------*
004200940929     D IN1             S              2  0 DIM(5) CTDATA PERRCD(5)
004300940930     D IN2             S              2  0 DIM(5) CTDATA PERRCD(5)
004400940930     D IN3             S              2  0 DIM(5) CTDATA PERRCD(5)
004500970116     D IN4             S              2  0 DIM(5) CTDATA PERRCD(5)
004600940928     D*--------------------------------------------------------------*
004700940928     D*  DS                                                          *
004800940928     D*--------------------------------------------------------------*
004900940930     D DSTB70        E DS                  EXTNAME(DS7PQRS)
005000940930     D DS7R          E DS
005100960111     D  TIP                   22     26
005200940930     D                                     DIM(5)
005300960111     D  NTE                   27     31  0
005400940930     D                                     DIM(5)
005500960111     D  EVE                   32     46
005600940930     D                                     DIM(5)
005700970116     D  TPT                   47     51
005800970116     D                                     DIM(5)
005900970116     D  NVO                   52     56
006000970116     D                                     DIM(5)
006100940927     D DS2A          E DS
006200940927     D DS2Z          E DS
006300900608      *
006400891030     D KPJBA         E DS
006500911118      *
006600911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
006700111104
006800111104     d tibs02ds      e ds                  inz
006900940228      *
007000940228     D                SDS
007100940927     D  V1CPGM                 1     10
007200110412
007300110412       //--------------------------------------------------------------
007400110412       //?Definizione variabili globali.                               ?
007500110412       //--------------------------------------------------------------
007600110412
007700110412       // -?Flags booleani?
007800110412     d $FoundPXA       s               n   inz
007900110412
008000110412       //--------------------------------------------------------------
008100110412       //?Definizione prototipi procedure.                             ?
008200110412       //--------------------------------------------------------------
008300110412
008400110412       // -?Gestione tabella "PXA"?
008500110412     d tntb71r         pr                  extpgm('TNTB71R')
008600110412     d   kpjba                             likeds(KPJBA)
008700110412
008800110412       //--------------------------------------------------------------
008900110412       //?Definizione key-list.                                        ?
009000110412       //--------------------------------------------------------------
009100110412
009200110412       // -?File TNTBE01L?
009300110412     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
009400110412     d                                     prefix(k_)   inz
009500110412
009600891030     I*--------------------------------------------------------------*
009700891030     I*                        I N P U T                             *
009800891030     I*--------------------------------------------------------------*
009900940930     ITB70D04
010000940930     I              V4C6T1                      VTI(1)
010100940930     I              V4C6T2                      VTI(2)
010200940930     I              V4C6T3                      VTI(3)
010300940930     I              V4C6T4                      VTI(4)
010400940930     I              V4C6T5                      VTI(5)
010500940930     I              V4C6N1                      VNT(1)
010600940930     I              V4C6N2                      VNT(2)
010700940930     I              V4C6N3                      VNT(3)
010800940930     I              V4C6N4                      VNT(4)
010900940930     I              V4C6N5                      VNT(5)
011000940930     I              V4C6E1                      VEV(1)
011100940930     I              V4C6E2                      VEV(2)
011200940930     I              V4C6E3                      VEV(3)
011300940930     I              V4C6E4                      VEV(4)
011400940930     I              V4C6E5                      VEV(5)
011500940930     I              V4D6E1                      VDE(1)
011600940930     I              V4D6E2                      VDE(2)
011700940930     I              V4D6E3                      VDE(3)
011800940930     I              V4D6E4                      VDE(4)
011900940930     I              V4D6E5                      VDE(5)
012000970116     I              V4C7T1                      VTE(1)
012100970116     I              V4C7T2                      VTE(2)
012200970116     I              V4C7T3                      VTE(3)
012300970116     I              V4C7T4                      VTE(4)
012400970116     I              V4C7T5                      VTE(5)
012500970116     I              V4C7V1                      VOL(1)
012600970116     I              V4C7V2                      VOL(2)
012700970116     I              V4C7V3                      VOL(3)
012800970116     I              V4C7V4                      VOL(4)
012900970116     I              V4C7V5                      VOL(5)
013000931209     C*--------------------------------------------------------------*
013100931209     C*    C I C L O    P R I N C I P A L E                          *
013200931209     C*--------------------------------------------------------------*
013300940928     C     *ENTRY        PLIST
013400940928     C                   PARM                    KPJBA
013500940930     C                   MOVEL     KPJBU         DSTB70
013600940930     C                   MOVEL     ' '           DS7ERR
013700940930     C                   MOVEL     ' '           DS7F03
013800941107     C                   MOVEL     *BLANKS       DS7MSG
013900960111     C                   MOVEL     'N'           WFINE
014000940928     C*  Prima volta
014100940928     C     WPRIMA        IFNE      'N'
014200940929     C                   MOVEL     'N'           WPRIMA            1
014300940928     C                   EXSR      DEFCAM
014400940928     C                   END
014500940928     C*  Carico dati da tabella
014600940928     C                   EXSR      INZ01
014700940928     C*  Loop gestione videate
014800940927     C     WFINE         DOWNE     'S'
014900940930     C     WTPVID        CASEQ     '1'           GESD01
015000970116     C     WTPVID        CASEQ     '2'           GESD02
015100050503     C     WTPVID        CASEQ     '8'           GESD08
015200140409     C     WTPVID        CASEQ     '9'           GESD09
015300970116     C     WTPVID        CASEQ     '3'           GESD03
015400931209     C                   END
015500931209     C                   END
015600940929     C*
015700931209     C* Fine programma
015800931209     C     FINE          TAG
015900940930     C                   MOVEL     DSTB70        KPJBU
016000960111     C* Libero tabel
016100960111     C     *LOVAL        SETLL     TABEL00F
016200940928     C* Se il programma è richiamato da altre funzioni chiudo in LR
016300940928     C* altrimenti chiudo in RT
016400940929     C     DS7RIC        IFEQ      'S'
016500931209     C                   SETON                                        LR
016600940928     C                   ELSE
016700940928     C                   SETON                                        RT
016800940928     C                   END
016900940928     C*---------------------------------------------------*
017000940928     C*  GESD01: Gestione prima videata                   *
017100940928     C*---------------------------------------------------*
017200940928     C     GESD01        BEGSR
017300940928     C*---------------------------------------------------*
017400931209     C*  Emissione videata
017500940930     C                   WRITE     TB70T01
017600940930     C                   EXFMT     TB70D03
017700940928     C                   SETOFF                                       404128
017800940929     C                   SETOFF                                       424344
017900940929     C                   SETOFF                                       454647
018000940930     C                   SETOFF                                       484950
018100940418     C                   MOVEL     *BLANKS       $MSG
018200110412     C*----------------------------------------------------*
018300931209     C*  Fine elaborazione
018400931209     C     *INKC         IFEQ      '1'
018500940929     C                   MOVEL     'S'           WFINE
018600940929     C                   MOVEL     '1'           DS7F03
018700940928     C                   GOTO      FINVD1
018800931209     C                   END
018900940928     C*----------------------------------------------------*
019000931209     C*  Ritorno prima videata
019100940928     C     *INKL         IFEQ      '1'
019200940929     C                   MOVEL     'S'           WFINE
019300960111     C                   MOVEL     '2'           DS7F03
019400940928     C                   GOTO      FINVD1
019500931209     C                   END
019600960112     C*----------------------------------------------------*
019700960112     C     DS7GES        IFEQ      ' '
019800960112     C     DS7OPZ        ANDEQ     '1'
019900960112     C*----------------------------------------------------*
020000110413      /free
020100110413         // -?F8 = Tab. "PXA"?
020200110413         if  *inKH;
020300110413           kpjbu = V3Ccod;
020400110413           tntb71r ( kpjba );
020500110413           exsr  sr_Text_V2DPXA;
020600110413           leavesr;
020700110413         endif;
020800110413      /end-free
020900931209     C*  Annullamento
021000931209     C     *INKQ         IFEQ      '1'
021100931209     C                   EXSR      ANNUL
021200940929     C                   MOVEL     'S'           WFINE
021300940928     C                   GOTO      FINVD1
021400931209     C                   END
021500940725     C*  controlli
021600940928     C                   EXSR      CTR01
021700940928     C   28              GOTO      FINVD1
021800931209     C*  Rirpristino
021900940928     C     *INKE         IFEQ      '1'
022000931209     C                   EXSR      RIPRIS
022100940929     C                   MOVEL     'S'           WFINE
022200940928     C                   GOTO      FINVD1
022300940928     C                   END
022400940928     C*----------------------------------------------------*
022500940928     C                   END
022600940928     C*----------------------------------------------------*
022700940929     C                   MOVEL     '2'           WTPVID
022800940928     C*
022900940928     C     FINVD1        ENDSR
023000110412      /free
023100110412       //---------------------------------------------------
023200110413       //?Verifica esistenza tab. "PXA" per la particolarità?
023300110413       //?  consegna in esame.                              ?
023400110412       //---------------------------------------------------
023500110412       BEGSR  sr_Text_V2DPXA;
023600110412
023700110413         DApxa = c_ColorGrn;
023800110412         clear  V2Dpxa;
023900110412         reset  $FoundPXA;
024000110412
024100110413         k_TBEcod = 'PXA';
024200110412         k_TBEke1 = V3Ccod;
024300110502         k_TBEke2 = '001 ';
024400110502         setll  %kds(k05tntbe01 : 3)  TNTBE000;
024500110502         reade  %kds(k05tntbe01 : 2)  TNTBE000;
024600110502         DoW  Not %eof(TNTBE01L)  and  TBEatb <> *blank;
024700110502           reade  %kds(k05tntbe01 : 2)  TNTBE000;
024800110502         EndDo;
024900110502         $FoundPXA = (Not %eof(TNTBE01L)   and   TBEatb = *blank);
025000110502         if  Not $FoundPXA;
025100110412           k_TBEke2 = 'PDA';
025200110412           chain  %kds(k05tntbe01 : 3)  TNTBE000;
025300110412           $FoundPXA = (%found(TNTBE01L)   and   TBEatb = *blank);
025400110502         endif;
025500110412
025600110608         select;
025700110608           // -?Trovato - in visualizzazione?
025800110608           when  $FoundPXA  and  *in90;
025900110608             evalr V2Dpxa = 'Presente "Promemoria per l''AUT" ';
026000110608             DApxa = c_ColorRed;
026100110608           // -?Trovato - in manutenzione?
026200110608           when  $FoundPXA  and  Not *in90;
026300110608             evalr V2Dpxa = 'F8=Gestione "Promemoria per l''AUT"';
026400110608             DApxa = c_ColorRed;
026500110608           // -?NON trovato - in manutenzione?
026600110608           when  Not $FoundPXA  and  Not *in90;
026700110608             evalr V2Dpxa = 'F8=Inserim. "Promemoria per l''AUT"';
026800110608             DApxa = c_ColorBlu;
026900110608         endsl;
027000110412
027100110412       ENDSR;
027200110412      /end-free
027300970116     C*----------------------------------------------------*
027400970116     C*  GESD02: Gestione seconda videata                  *
027500970116     C*----------------------------------------------------*
027600970116     C     GESD02        BEGSR
027700970116     C*
027800970116     C*  Emissione videata
027900970116     C                   WRITE     TB70T01
028000970116     C                   EXFMT     TB70D04
028100970116     C                   SETOFF                                       404142
028200970116     C                   SETOFF                                       434445
028300970116     C                   SETOFF                                       464748
028400970116     C                   SETOFF                                       495051
028500970116     C                   SETOFF                                       525354
028600970116     C                   SETOFF                                       555657
028700970116     C                   SETOFF                                       585960
028800111104     C                   SETOFF                                       6128
028900970116     C                   MOVEL     *BLANKS       $MSG
029000970116     C*  Fine elaborazione
029100970116     C     *INKC         IFEQ      '1'
029200970116     C                   MOVEL     'S'           WFINE
029300970116     C                   MOVEL     '1'           DS7F03
029400970116     C                   GOTO      FINVD2
029500970116     C                   END
029600970116     C*  Ritorno prima videata
029700970116     C     *INKL         IFEQ      '1'
029800970116     C                   MOVEL     '1'           WTPVID            1
029900970116     C                   GOTO      FINVD2
030000970116     C                   END
030100970116     C*----------------------------------------------------*
030200970116     C     DS7GES        IFEQ      ' '
030300970116     C     DS7OPZ        ANDEQ     '1'
030400970116     C*----------------------------------------------------*
030500970116     C*  Annullamento
030600970116     C     *INKQ         IFEQ      '1'
030700970116     C                   EXSR      ANNUL
030800970116     C                   MOVEL     'S'           WFINE
030900970116     C                   GOTO      FINVD2
031000970116     C                   END
031100970116     C*  controlli
031200970116     C                   EXSR      CTR02
031300970116     C   28              GOTO      FINVD2
031400970116     C*  Ripristino
031500970116     C     *INKE         IFEQ      '1'
031600970116     C                   EXSR      RIPRIS
031700970116     C                   MOVEL     'S'           WFINE
031800970116     C                   GOTO      FINVD2
031900970116     C                   END
032000970116     C*----------------------------------------------------*
032100970116     C                   END
032200970116     C*----------------------------------------------------*
032300050503     C                   MOVEL     '8'           WTPVID
032400970116     C*----------------------------------------------------*
032500970116     C     FINVD2        ENDSR
032600940928     C*----------------------------------------------------*
032700970116     C*  GESD03: Gestione terza videata                    *
032800940928     C*----------------------------------------------------*
032900970116     C     GESD03        BEGSR
033000940928     C*
033100940928     C*  Emissione videata
033200940930     C                   WRITE     TB70T01
033300970116     C                   EXFMT     TB70D07
033400111104     C                   SETOFF                                       6728
033500111104     C                   MOVEL     *BLANKS       $MSG
033600940928     C*  Fine elaborazione
033700940928     C     *INKC         IFEQ      '1'
033800940929     C                   MOVEL     'S'           WFINE
033900940929     C                   MOVEL     '1'           DS7F03
034000970116     C                   GOTO      FINVD3
034100940928     C                   END
034200940928     C*  Ritorno prima videata
034300940928     C     *INKL         IFEQ      '1'
034400140409     C                   MOVEL     '9'           WTPVID            1
034500970116     C                   GOTO      FINVD3
034600940928     C                   END
034700940928     C*----------------------------------------------------*
034800940929     C     DS7GES        IFEQ      ' '
034900940929     C     DS7OPZ        ANDEQ     '1'
035000940928     C*----------------------------------------------------*
035100940928     C*  Annullamento
035200940928     C     *INKQ         IFEQ      '1'
035300940928     C                   EXSR      ANNUL
035400940929     C                   MOVEL     'S'           WFINE
035500970116     C                   GOTO      FINVD3
035600940928     C                   END
035700111104     C*  controlli
035800111104     C                   EXSR      CTR03
035900111104     C   28              GOTO      FINVD3
036000940928     C*  Ripristino
036100940928     C     *INKE         IFEQ      '1'
036200940928     C                   EXSR      RIPRIS
036300940929     C                   MOVEL     'S'           WFINE
036400970116     C                   GOTO      FINVD3
036500940928     C                   END
036600940928     C*  Conferma
036700940928     C     *INKF         IFEQ      '1'
036800940928     C                   EXSR      AGGIOR
036900940929     C                   MOVEL     'S'           WFINE
037000970116     C                   GOTO      FINVD3
037100940928     C                   END
037200940928     C*----------------------------------------------------*
037300940928     C                   ELSE
037400940928     C                   MOVEL     'S'           WFINE
037500940928     C*----------------------------------------------------*
037600940928     C                   END
037700940928     C*----------------------------------------------------*
037800970116     C     FINVD3        ENDSR
037900050503     C*----------------------------------------------------*
038000050503     C*  GESD08: Gestione record 08 video                  *
038100050503     C*----------------------------------------------------*
038200050503     C     GESD08        BEGSR
038300050503     C*
038400050503     C*  Emissione videata
038500050503     C                   WRITE     TB70T01
038600050503     C                   EXFMT     TB70D08
038700100204     C                   SETOFF                                       2829
038800141119     C                   SETOFF                                       17
038900050503     C                   MOVEL     *BLANKS       $MSG
039000050503     C*  Fine elaborazione
039100050503     C     *INKC         IFEQ      '1'
039200050503     C                   MOVEL     'S'           WFINE
039300050503     C                   MOVEL     '1'           DS7F03
039400050503     C                   GOTO      FINVD8
039500050503     C                   END
039600050503     C*  Ritorno videata precedente
039700050503     C     *INKL         IFEQ      '1'
039800050503     C                   MOVEL     '2'           WTPVID            1
039900050503     C                   GOTO      FINVD8
040000050503     C                   END
040100050503     C*----------------------------------------------------*
040200050503     C     DS7GES        IFEQ      ' '
040300050503     C     DS7OPZ        ANDEQ     '1'
040400050503     C*----------------------------------------------------*
040500050503     C*  Annullamento
040600050503     C     *INKQ         IFEQ      '1'
040700050503     C                   EXSR      ANNUL
040800050503     C                   MOVEL     'S'           WFINE
040900050503     C                   GOTO      FINVD8
041000050503     C                   END
041100050503     C*  controlli
041200141119     C**************     EXSR      CTR02
041300141119     C                   EXSR      CTR08
041400050506     c                   if        *in28 or *in29
041500050506     C                   GOTO      FINVD8
041600050506     c                   end
041700050503     C*  Ripristino
041800050503     C     *INKE         IFEQ      '1'
041900050503     C                   EXSR      RIPRIS
042000050503     C                   MOVEL     'S'           WFINE
042100050503     C                   GOTO      FINVD8
042200050503     C                   END
042300050503     C*----------------------------------------------------*
042400050503     C                   END
042500050503     C*----------------------------------------------------*
042600140409     C                   MOVEL     '9'           WTPVID
042700050503     C*----------------------------------------------------*
042800050503     C     FINVD8        ENDSR
042900140409     C*----------------------------------------------------*
043000140409     C*  GESD09: Gestione record 09 video                  *
043100140409     C*----------------------------------------------------*
043200140409     C     GESD09        BEGSR
043300140409     C*
043400140409     C*  Emissione videata
043500141119     C                   WRITE     TB70T01
043600140409     C                   EXFMT     TB70D09
043700140409     C                   SETOFF                                       2829
043800141119     C                   SETOFF                                       17
043900140409     C                   MOVEL     *BLANKS       $MSG
044000140409     C*  Fine elaborazione
044100140409     C     *INKC         IFEQ      '1'
044200140409     C                   MOVEL     'S'           WFINE
044300140409     C                   MOVEL     '1'           DS7F03
044400140409     C                   GOTO      FINVD9
044500140409     C                   END
044600140409     C*  Ritorno videata precedente
044700140409     C     *INKL         IFEQ      '1'
044800140409     C                   MOVEL     '8'           WTPVID            1
044900140409     C                   GOTO      FINVD9
045000140409     C                   END
045100140409     C*----------------------------------------------------*
045200140409     C     DS7GES        IFEQ      ' '
045300140409     C     DS7OPZ        ANDEQ     '1'
045400140409     C*----------------------------------------------------*
045500140409     C*  Annullamento
045600140409     C     *INKQ         IFEQ      '1'
045700140409     C                   EXSR      ANNUL
045800140409     C                   MOVEL     'S'           WFINE
045900140409     C                   GOTO      FINVD9
046000140409     C                   END
046100140409     C*  controlli
046200141119     C                   EXSR      CTR09
046300140409     c                   if        *in28 or *in29
046400140409     C                   GOTO      FINVD9
046500140409     c                   end
046600140409     C*  Ripristino
046700140409     C     *INKE         IFEQ      '1'
046800140409     C                   EXSR      RIPRIS
046900140409     C                   MOVEL     'S'           WFINE
047000140409     C                   GOTO      FINVD9
047100140409     C                   END
047200140409     C*----------------------------------------------------*
047300140409     C                   END
047400140409     C*----------------------------------------------------*
047500140409     C                   MOVEL     '3'           WTPVID
047600140409     C*----------------------------------------------------*
047700140409     C     FINVD9        ENDSR
047800940928     C*----------------------------------------------------*
047900940928     C*  INZ01   Gestione inizializzazione prima videata   *
048000940928     C*----------------------------------------------------*
048100940927     C     INZ01         BEGSR
048200940928     C*----------------------------------------------------*
048300940928     C                   SETOFF                                       010203
048400940930     C                   SETOFF                                       040506
048500940928     C                   SETOFF                                       284041
048600940929     C                   SETOFF                                       424344
048700940929     C                   SETOFF                                       454647
048800940930     C                   SETOFF                                       484950
048900940930     C                   SETOFF                                       515253
049000940930     C                   SETOFF                                       545556
049100970116     C                   SETOFF                                       575859
049200970116     C                   SETOFF                                       606107
049300940928     C                   SETOFF                                       929390
049400940928     C                   MOVEL     *BLANKS       $MSG
049500940928     C                   MOVEL     'N'           WFINE             1
049600940928     C                   MOVEL     '1'           WTPVID
049700960111     C     DS7KSC        IFNE      0
049800960111     C                   MOVEA     DS7CDP        CODP
049900960111     C                   END
050000940928     C*
050100940930     C                   MOVEL     '7R'          KCOD
050200940928     C                   MOVEL     *BLANKS       KKEY
050300940929     C                   MOVEL     DS7COD        KKEY
050400940929     C                   MOVEL     DS7COD        V3CCOD
050500940930     C                   CLEAR                   DS7R
050600940928     C     KTAB2         CHAIN     TABEL00F                           31
050700940930     C  N31              MOVEL     TBLUNI        DS7R
050800940928     C*----------------------------------------------------*
050900940928     C     *IN31         IFEQ      '0'
051000940929     C                   MOVEL     COST(2)       V1DFUN
051100940928     C     TBLFLG        IFNE      ' '
051200940928     C                   SETON                                        92
051300940928     C                   ELSE
051400940928     C                   SETON                                        93
051500940928     C                   END
051600940928     C                   ELSE
051700940929     C                   MOVEL     COST(1)       V1DFUN
051800940930     C                   CLEAR                   DS7R
051900940928     C                   END
052000940928     C*
052100940929     C     DS7GES        IFEQ      'V'
052200940929     C     DS7OPZ        OREQ      '5'
052300940929     C                   MOVEL     COST(3)       V1DFUN
052400940928     C                   SETON                                        90
052500940928     C                   END
052600940928     C*
052700970116     C                   MOVE      §7RABC        V7CABC
052800970116     C                   MOVE      §7RELI        V7CELI
052900970116     C                   MOVE      §7RINS        V7CINS
053000940928     C                   MOVEA     TIP           VTI
053100940930     C                   MOVEA     EVE           VEV
053200940930     C                   MOVEA     NTE           VNT
053300970116     C                   MOVEA     TPT           VTE
053400970116     C                   MOVEA     NVO           VOL
053500940930     C                   MOVEA     *BLANKS       VDE
053600050105     C                   MOVE      §7R6pu        V4C6pu
053700940930     C                   MOVE      §7R5EV        V4C5EV
053800940930     C                   MOVE      §7R5TM        V4C5TM
053900050105     C                   MOVE      §7R5pu        V4C5pu
054000940930     C                   MOVEL     *BLANKS       V4D5EV
054100940930     C                   MOVEL     §7R1BF        V3C1BF
054200050503     C                   MOVEL     §7Rldv        V3Cldv
054300940930     C                   MOVEL     §7R1CB        V3C1CB
054400940930     C                   MOVEL     *BLANKS       V3D1CB
054500940930     C                   MOVEL     §7R2FI        V3C2FI
054600940930     C                   MOVEL     §7R2OR        V3C2OR
054700940930     C                   MOVEL     §7R3TM        V3C3TM
054800050105     C                   MOVEL     §7R3pu        V3C3pu
054900940930     C                   MOVEL     §7R3EV        V3C3EV
055000940930     C                   MOVEL     *BLANKS       V3D3EV
055100940930     C                   MOVEL     §7R4TM        V3C4TM
055200050105     C                   MOVEL     §7R4pu        V3C4pu
055300940930     C                   MOVEL     §7R4NT        V3C4NT
055400940930     C                   MOVEL     §7R4EV        V3C4EV
055500940930     C                   MOVEL     *BLANKS       V3D4EV
055600050503     C                   MOVE      §7R8cp        v8c8cp
055700050503     C                   MOVE      §7R8di        v8c8di
055800050503     C                   move      §7R8gg        v8c8gg
055900081127     C                   MOVE      §7R9cd        v8c9cd
056000100505     C                   MOVE      §7Ratt        v8c10ca
056100100505     C                   MOVE      §7Rrc         v8c11rc
056200120426     C                   MOVE      §7Rvpcd       v8cvpcd
056300120426     C                   MOVE      §7Rldvfir     v8cldvfir
056400111104     c                   eval      v7caut = §7Raut
056500970116     C                   MOVEL     ' '           V7CFTR
056600130610     C                   MOVE      §7RGGFD       v8ggfd
056700140409     C                   MOVE      §7RFD         v9c15fd
056800140409     C                   MOVE      §7RAG         v9c16ng
056900141118     C                   MOVE      §7RPINCODE    v9pincode
057000940929     C     TBLFTT        IFEQ      ' '
057100940929     C     *IN31         ANDEQ     '0'
057200970116     C                   MOVEL     'N'           V7CFTR
057300940929     C                   END
057400110412      /free
057500110412
057600110412         // -?Verifica esistenza tab. "PXA" x abilitazione F8?
057700110412         exsr  sr_Text_V2DPXA;
057800110412
057900110412      /end-free
058000940928     C*  Decodifico eventi
058100940928     C                   Z-ADD     1             Y
058200940930     C     V3C3EV        LOOKUP    C2A(Y)                                 32
058300940930     C   32              MOVEL     D2A(Y)        V3D3EV
058400940928     C                   Z-ADD     1             Y
058500940930     C     V3C4EV        LOOKUP    C2A(Y)                                 32
058600940930     C   32              MOVEL     D2A(Y)        V3D4EV
058700940930     C     V4C5EV        LOOKUP    C2A(Y)                                 32
058800940930     C   32              MOVEL     D2A(Y)        V4D5EV
058900940928     C*  Decodifico codici disposizioni riapertura
059000070618     C                   Z-ADD     1             Y                 3 0
059100940930     C     V3C1CB        LOOKUP    C3A(Y)                                 32
059200940930     C   32              MOVEL     D3A(Y)        V3D1CB
059300940930     C*  Decodifico codici eventi
059400940928     C                   DO        5             X
059500940930     C     VEV(X)        IFNE      *BLANKS
059600070618     C                   Z-ADD     1             Y
059700940930     C     VEV(X)        LOOKUP    C2A(Y)                                 32
059800940930     C   32              MOVEL     D2A(Y)        VDE(X)
059900940928     C                   END
060000940928     C                   END
060100940928     C*  Accendo indicatori x posizionamento/REVERSE IMMAGE
060200940928     C*  su gruppo parametri da modificare
060300940930     C                   MOVEA     TIP           TIPOL             5
060400940930     C                   MOVEA     EVE           EVEN             15
060500940930     C                   XFOOT     NTE           NRTEN             3 0
060600940930     C     TIPOL         IFNE      *BLANKS
060700940930     C     EVEN          ORNE      *BLANKS
060800940930     C     NRTEN         ORNE      *ZEROS
060900940930     C                   SETON                                        06
061000940930     C                   END
061100970116     C                   MOVEA     TPT           TIPTRA            5
061200970116     C                   MOVEA     NVO           VOLTE             5
061300970116     C     TIPTRA        IFNE      *BLANKS
061400970116     C     VOLTE         ORNE      *BLANKS
061500970116     C                   SETON                                        07
061600970116     C                   END
061700941003     C     §7R5TM        IFNE      *BLANKS
061800940930     C     §7R5EV        ORNE      *BLANKS
061900940928     C                   SETON                                        05
062000940928     C                   END
062100940930     C     §7R4TM        IFNE      *BLANKS
062200940930     C     §7R4EV        ORNE      *BLANKS
062300940930     C     §7R4NT        ORNE      *ZEROS
062400940928     C                   SETON                                        04
062500940928     C                   END
062600940930     C     §7R3TM        IFNE      *BLANKS
062700940930     C     §7R3EV        ORNE      *BLANKS
062800940928     C                   SETON                                        03
062900940928     C                   END
063000940930     C     §7R2OR        IFNE      *BLANKS
063100940930     C     §7R2FI        ORNE      *BLANKS
063200940928     C                   SETON                                        02
063300940928     C                   END
063400960111     C     §7R1BF        IFNE      *BLANKS
063500940928     C                   SETON                                        01
063600940928     C                   END
063700111104      * decodifico chi autorizza
063800111104     c                   clear                   tibs02ds
063900111104     c                   clear                   V7Daut
064000111104     c                   eval      T02mod = 'C'
064100111104     c                   eval      T02cod = 'RAC'
064200111104     c                   movel     V7Caut        t02ke1
064300111104     c                   call      'TIBS02R'
064400111104     c                   parm                    kpjba
064500111104     c                   parm                    tibs02ds
064600111104     c                   if        t02err = *blanks
064700111104     c                   movel     t02uni        V7Daut
064800111104     c                   endif
064900940928     C*
065000940929     C     FINCAR        ENDSR
065100940928     C*----------------------------------------------------*
065200940928     C*  CTR01 : Gestione inizial. prima   videata         *
065300940928     C*----------------------------------------------------*
065400940928     C     CTR01         BEGSR
065500940928     C*
065600940930     C*  Controllo dati obbligo restituzione documento firmato
065700960111     C     V3C1BF        IFEQ      *BLANKS
065800960111     C     V3C1CB        IFNE      *BLANKS
065900940930     C                   SETON                                        2840
066000960111     C                   MOVEL     ERR(7)        $MSG
066100940930     C                   GOTO      FINCT1
066200940930     C                   END
066300940930     C                   END
066400940930     C*
066500940930     C                   MOVEL     *BLANKS       V3D1CB
066600960111     C     V3C1CB        IFNE      *BLANKS
066700940930     C                   Z-ADD     1             X
066800940930     C     V3C1CB        LOOKUP    C3A(X)                                 32
066900940930     C     *IN32         IFEQ      '1'
067000940930     C                   MOVEL     D3A(X)        V3D1CB
067100940930     C                   ELSE
067200940930     C                   SETON                                        2842
067300960111     C                   MOVEL     ERR(8)        $MSG
067400940930     C                   END
067500940930     C                   END
067600940930     C*
067700940930     C*  Controllo dati obbligo fimatario/ora consegna
067800940930     C     V3C2OR        IFEQ      *BLANKS
067900940930     C     V3C2FI        IFNE      *BLANKS
068000940930     C                   SETON                                        2843
068100960111     C                   MOVEL     ERR(7)        $MSG
068200940929     C                   GOTO      FINCT1
068300940929     C                   END
068400940929     C                   END
068500940929     C*
068600940930     C     V3C2FI        IFEQ      *BLANKS
068700940930     C     V3C2OR        IFNE      *BLANKS
068800940930     C                   SETON                                        2844
068900960111     C                   MOVEL     ERR(7)        $MSG
069000940929     C                   GOTO      FINCT1
069100940929     C                   END
069200940929     C                   END
069300940929     C*
069400940930     C*  Controllo lasciato avviso diverso da standard
069500940930     C     V3C3TM        IFEQ      *BLANKS
069600940930     C     V3C3EV        IFNE      *BLANKS
069700940930     C                   SETON                                        2846
069800960111     C                   MOVEL     ERR(10)       $MSG
069900940929     C                   GOTO      FINCT1
070000940929     C                   END
070100940930     C                   ELSE
070200940930     C     V3C3TM        LOOKUP    T2A                                    32
070300940930     C     *IN32         IFEQ      '0'
070400940930     C                   SETON                                        2846
070500940930     C                   MOVEL     ERR(6)        $MSG
070600940930     C                   END
070700940929     C                   END
070800940930     C*
070900940930     C                   MOVEL     *BLANKS       V3D3EV
071000940930     C     V3C3EV        IFEQ      *BLANKS
071100940930     C     V3C3TM        IFNE      *BLANKS
071200940930     C                   SETON                                        2847
071300940930     C                   MOVEL     ERR(5)        $MSG
071400940930     C                   GOTO      FINCT1
071500940930     C                   END
071600940930     C                   ELSE
071700940930     C                   Z-ADD     1             X
071800940930     C     V3C3EV        LOOKUP    C2A(X)                                 32
071900940930     C     *IN32         IFEQ      '0'
072000940930     C                   SETON                                        2847
072100940930     C                   MOVEL     ERR(1)        $MSG
072200940930     C                   ELSE
072300940930     C                   MOVEL     D2A(X)        V3D3EV
072400940930     C                   END
072500940930     C                   END
072600940928     C*
072700940930     C*  Controllo lasciato avviso dopo n.tentativi consegne
072800940930     C     V3C4TM        IFEQ      *BLANKS
072900940930     C     V3C4NT        IFNE      *ZEROS
073000940930     C     V3C4EV        ORNE      *BLANKS
073100940930     C                   SETON                                        2848
073200960111     C                   MOVEL     ERR(10)       $MSG
073300940930     C                   GOTO      FINCT1
073400940930     C                   END
073500940930     C                   ELSE
073600940930     C     V3C4TM        LOOKUP    T2A                                    32
073700940930     C     *IN32         IFEQ      '0'
073800940930     C                   SETON                                        2848
073900940930     C                   MOVEL     ERR(6)        $MSG
074000941003     C                   GOTO      FINCT1
074100940930     C                   END
074200940930     C                   END
074300940930     C*
074400940930     C     V3C4NT        IFEQ      *ZEROS
074500940930     C     V3C4TM        IFNE      *BLANKS
074600940930     C     V3C4EV        ORNE      *BLANKS
074700940930     C                   SETON                                        2849
074800940930     C                   MOVEL     ERR(3)        $MSG
074900940930     C                   GOTO      FINCT1
075000940930     C                   END
075100940930     C                   END
075200940930     C*
075300940930     C                   MOVEL     *BLANKS       V3D4EV
075400940930     C     V3C4EV        IFEQ      *BLANKS
075500940930     C     V3C4TM        IFNE      *BLANKS
075600940930     C     V3C4NT        ORNE      *ZEROS
075700940930     C                   SETON                                        2850
075800940930     C                   MOVEL     ERR(5)        $MSG
075900940930     C                   GOTO      FINCT1
076000940930     C                   END
076100940930     C                   ELSE
076200940930     C                   Z-ADD     1             X
076300940930     C     V3C4EV        LOOKUP    C2A(X)                                 32
076400940930     C     *IN32         IFEQ      '0'
076500940930     C                   SETON                                        2850
076600940930     C                   MOVEL     ERR(1)        $MSG
076700940930     C                   ELSE
076800940930     C                   MOVEL     D2A(X)        V3D3EV
076900940930     C                   END
077000940930     C                   END
077100940928     C*
077200940928     C     FINCT1        ENDSR
077300940928     C*----------------------------------------------------*
077400940928     C*  CTR02 : Controlli seconda videata                 *
077500940928     C*----------------------------------------------------*
077600940928     C     CTR02         BEGSR
077700940930     C*  Controllo tipo mancata consegna
077800940930     C     V4C5TM        IFEQ      *BLANKS
077900940930     C     V4C5EV        IFNE      *BLANKS
078000940930     C                   SETON                                        2840
078100960111     C                   MOVEL     ERR(10)       $MSG
078200940930     C                   GOTO      FINCT2
078300940930     C                   END
078400940930     C                   ELSE
078500940930     C     V4C5TM        LOOKUP    T2A                                    32
078600940930     C     *IN32         IFEQ      '0'
078700940930     C                   SETON                                        2840
078800940930     C                   MOVEL     ERR(6)        $MSG
078900941003     C                   GOTO      FINCT2
079000940930     C                   END
079100940930     C                   END
079200940930     C*
079300940930     C                   MOVEL     *BLANKS       V4D5EV
079400940930     C     V4C5EV        IFEQ      *BLANKS
079500940930     C     V4C5TM        IFNE      *BLANKS
079600940930     C                   SETON                                        2841
079700940930     C                   MOVEL     ERR(5)        $MSG
079800940930     C                   GOTO      FINCT2
079900940930     C                   END
080000940930     C                   ELSE
080100940930     C                   Z-ADD     1             X
080200940930     C     V4C5EV        LOOKUP    C2A(X)                                 32
080300940930     C     *IN32         IFEQ      '0'
080400940930     C                   SETON                                        2841
080500940930     C                   MOVEL     ERR(1)        $MSG
080600940930     C                   ELSE
080700940930     C                   MOVEL     D2A(X)        V4D5EV
080800940930     C                   END
080900940930     C                   END
081000940930     C*
081100940930     C*  Se ho un codice con tipologia 'T' non ne posso imm.altri
081200940930     C                   Z-ADD     1             X
081300940930     C                   Z-ADD     0             WX                2 0
081400940930     C     'T'           LOOKUP    VTI(X)                                 32
081500940930     C   32              Z-ADD     X             WX
081600940930     C*  Controllo Tipologie/n.tentativi/codici eventi
081700940928     C                   DO        5             X
081800940930     C     VTI(X)        IFNE      *BLANKS
081900940928     C     VTI(X)        ANDNE     'T'
082000940928     C     VTI(X)        LOOKUP    T2Z                                    32
082100940928     C     *IN32         IFEQ      '0'
082200940930     C                   SETON                                        28
082300940928     C                   MOVEL     IN1(X)        C                 2 0
082400940928     C                   MOVEL     '1'           *IN(C)
082500940928     C                   MOVEL     ERR(2)        $MSG
082600940928     C                   GOTO      FINCT2
082700940928     C                   END
082800940930     C     VTI(X)        IFNE      *BLANKS
082900940930     C     WX            ANDNE     X
083000940930     C     WX            ANDNE     0
083100940930     C                   SETON                                        28
083200940930     C                   MOVEL     IN1(X)        C                 2 0
083300940930     C                   MOVEL     '1'           *IN(C)
083400960111     C                   MOVEL     ERR(9)        $MSG
083500940930     C                   GOTO      FINCT2
083600940930     C                   END
083700940930     C*  Controllo se tipologia doppia
083800940930     C                   Z-ADD     1             Y
083900940930     C     VTI(X)        LOOKUP    VTI(Y)                                 97
084000940930     C     X             IFNE      Y
084100940930     C                   SETON                                        28
084200940930     C                   MOVEL     IN1(X)        C                 2 0
084300940930     C                   MOVEL     '1'           *IN(C)
084400960111     C                   MOVEL     ERR(9)        $MSG
084500940930     C                   GOTO      FINCT2
084600940930     C                   END
084700940929     C                   END
084800940929     C*
084900940930     C                   MOVEL     *BLANKS       VDE(X)
085000940930     C     VEV(X)        IFNE      *BLANKS
085100940930     C                   Z-ADD     1             Y
085200940930     C     VEV(X)        LOOKUP    C2A(Y)                                 32
085300940929     C     *IN32         IFEQ      '0'
085400940929     C                   SETON                                        28
085500940930     C                   MOVEL     IN3(X)        C                 2 0
085600940929     C                   MOVEL     '1'           *IN(C)
085700940930     C                   MOVEL     ERR(1)        $MSG
085800940929     C                   GOTO      FINCT2
085900940929     C                   ELSE
086000941003     C                   MOVEL     D2A(Y)        VDE(X)
086100940929     C                   END
086200940929     C                   END
086300940928     C*
086400940929     C     VTI(X)        IFEQ      *BLANKS
086500940930     C     VNT(X)        IFNE      *ZEROS
086600940930     C     VEV(X)        ORNE      *BLANKS
086700940929     C                   SETON                                        28
086800940929     C                   MOVEL     IN1(X)        C                 2 0
086900940929     C                   MOVEL     '1'           *IN(C)
087000940929     C                   MOVEL     ERR(4)        $MSG
087100940929     C                   GOTO      FINCT2
087200940929     C                   END
087300940930     C                   END
087400940930     C*
087500940930     C     VNT(X)        IFEQ      *ZEROS
087600940930     C     VEV(X)        IFNE      *BLANKS
087700940930     C     VTI(X)        ORNE      *BLANKS
087800940930     C                   SETON                                        28
087900940930     C                   MOVEL     IN2(X)        C                 2 0
088000940930     C                   MOVEL     '1'           *IN(C)
088100940930     C                   MOVEL     ERR(3)        $MSG
088200940930     C                   GOTO      FINCT2
088300940930     C                   END
088400940930     C                   END
088500940929     C*
088600940930     C     VEV(X)        IFEQ      *BLANKS
088700940930     C     VNT(X)        IFNE      *ZEROS
088800940930     C     VTI(X)        ORNE      *BLANKS
088900940929     C                   SETON                                        28
089000940930     C                   MOVEL     IN3(X)        C                 2 0
089100940929     C                   MOVEL     '1'           *IN(C)
089200940929     C                   MOVEL     ERR(5)        $MSG
089300940929     C                   GOTO      FINCT2
089400940929     C                   END
089500940930     C                   END
089600940929     C*
089700940928     C                   END
089800970116     C*
089900970116     C*  Se ho un codice con tipologia 'T' non ne posso imm.altri
090000970116     C                   Z-ADD     1             X
090100970116     C                   Z-ADD     0             WX                2 0
090200970116     C     'T'           LOOKUP    VTE(X)                                 32
090300970116     C   32              Z-ADD     X             WX
090400970116     C*  Controllo Tipologie/n.tentativi/codici eventi
090500970116     C                   DO        5             X
090600970116     C     VTE(X)        IFNE      *BLANKS
090700970116     C     VTE(X)        ANDNE     'T'
090800970116     C     VTE(X)        LOOKUP    T2A                                    32
090900970116     C     *IN32         IFEQ      '0'
091000970116     C                   SETON                                        28
091100970116     C                   MOVEL     IN4(X)        C                 2 0
091200970116     C                   MOVEL     '1'           *IN(C)
091300970116     C                   MOVEL     ERR(6)        $MSG
091400970116     C                   GOTO      FINCT2
091500970116     C                   END
091600970116     C     VTE(X)        IFNE      *BLANKS
091700970116     C     WX            ANDNE     X
091800970116     C     WX            ANDNE     0
091900970116     C                   SETON                                        28
092000970116     C                   MOVEL     IN4(X)        C                 2 0
092100970116     C                   MOVEL     '1'           *IN(C)
092200970116     C                   MOVEL     ERR(6)        $MSG
092300970116     C                   GOTO      FINCT2
092400970116     C                   END
092500970116     C*  Controllo se tipologia doppia
092600970116     C                   Z-ADD     1             Y
092700970116     C     VTE(X)        LOOKUP    VTE(Y)                                 97
092800970116     C     X             IFNE      Y
092900970116     C                   SETON                                        28
093000970116     C                   MOVEL     IN4(X)        C                 2 0
093100970116     C                   MOVEL     '1'           *IN(C)
093200970116     C                   MOVEL     ERR(6)        $MSG
093300970116     C                   GOTO      FINCT2
093400970116     C                   END
093500970116     C                   END
093600970116     C*
093700970116     C     VTE(X)        IFEQ      *BLANKS
093800970116     C     VOL(X)        ANDNE     *BLANKS
093900970116     C                   SETON                                        28
094000970116     C                   MOVEL     IN4(X)        C                 2 0
094100970116     C                   MOVEL     '1'           *IN(C)
094200970116     C                   MOVEL     ERR(10)       $MSG
094300970116     C                   GOTO      FINCT2
094400970116     C                   END
094500970116     C*
094600970116     C                   END
094700940929     C*
094800940928     C     FINCT2        ENDSR
094900111104
095000111104      *----------------------------------------------------*
095100111104      *  CTR03 : Controlli terza videata                   *
095200111104      *----------------------------------------------------*
095300111104     c     ctr03         BEGSR
095400111104
095500111104      * Chi autorizza
095600111104     c                   IF        V7Caut = *blanks
095700111104     c                   eval      *in67 = *on
095800111104     c                   eval      *in28 = *on
095900130220     c                   eval      $msg = err(11)
096000111104     c                   leavesr
096100111104     c                   ENDIF
096200111104     c                   IF        V7Caut = '?'
096300111104     c                   clear                   tibs02ds
096400111104     c                   eval      T02mod = 'R'
096500111104     c                   eval      T02cod = 'RAC'
096600111104     c                   call      'TIBS02R'
096700111104     c                   parm                    kpjba
096800111104     c                   parm                    tibs02ds
096900111104     c                   IF        T02err = *blanks
097000111104     c                   eval      V7Caut = T02ke1
097100111104     c                   eval      V7Daut = T02uni
097200111104     c                   ENDIF
097300111104     c                   ENDIF
097400111104     c                   IF        V7Caut <> *blanks
097500111104     c                   clear                   tibs02ds
097600111104     c                   eval      T02mod = 'C'
097700111104     c                   eval      T02cod = 'RAC'
097800111104     c                   eval      T02ke1 = V7Caut
097900111104     c                   call      'TIBS02R'
098000111104     c                   parm                    kpjba
098100111104     c                   parm                    tibs02ds
098200111104     c                   IF        T02err <> *blanks
098300111104     c                   eval      *in67 = *on
098400111104     c                   eval      *in28 = *on
098500130220     c                   eval      $msg = err(11)
098600111104     c                   leavesr
098700111104     c                   ENDIF
098800111104     c                   eval      V7Daut = T02uni
098900111104     c                   ENDIF
099000111104
099100111104     c                   ENDSR
099200141119     C*----------------------------------------------------*
099300141119     C*  CTR08 : Controlli ottava  videata                 *
099400141119     C*----------------------------------------------------*
099500141119     C     CTR08         BEGSR
099600141119     C*
099700141119     C* Controllo sull'utilizzo del PINCODE che non può essere utilizzato
099800141119     C*  se richiesta la firma sulla LDV
099900141119     C     v8CLDVFIR     ifNE      *BLANKS
100000141119     C     v9PINCODE     andNE     *BLANKS
100100141119      *
100200141119     C                   SETON                                          2817
100300141119     C                   MOVEL     ERR(12)       $MSG
100400141119     C                   GOTO      finct8
100500141119      *
100600141119     C                   END
100700141119     C*
100800141119     c     finct8        ENDSR
100900141119     C*----------------------------------------------------*
101000141119     C*  CTR09 : Controlli nona    videata                 *
101100141119     C*----------------------------------------------------*
101200141119     C     CTR09         BEGSR
101300141119     C*
101400141119     C* Controllo sull'utilizzo del PINCODE che non può essere utilizzato
101500141119     C*  se richiesta la firma sulla LDV
101600141119     C     v8CLDVFIR     ifNE      *BLANKS
101700141119     C     v9PINCODE     andNE     *BLANKS
101800141119      *
101900141119     C                   SETON                                          2817
102000141119     C                   MOVEL     ERR(12)       $MSG
102100141119     C                   GOTO      finct9
102200141119      *
102300141119     C                   END
102400141119
102500141119     c     finct9        ENDSR
102600940928     C*----------------------------------------------------*
102700940928     C*  AGGIOR: Aggiornamento paramteri                   *
102800940928     C*----------------------------------------------------*
102900931209     C     AGGIOR        BEGSR
103000940701     C*------------------------------------------------------*
103100940725     C*  Procedo all'aggiornamento della tabella
103200940929     C                   MOVEL     *BLANKS       KKEY
103300940930     C                   MOVE      '7R'          KCOD
103400940929     C                   MOVEL     V3CCOD        KKEY
103500940929     C     KTAB2         CHAIN     TABEL00F                           31
103600970116     C                   MOVE      V7CABC        §7RABC
103700970116     C                   MOVE      V7CELI        §7RELI
103800970116     C                   MOVE      V7CINS        §7RINS
103900940929     C                   MOVEA     VTI           TIP
104000940930     C                   MOVEA     VNT           NTE
104100940930     C                   MOVEA     VEV           EVE
104200970116     C                   MOVEA     VTE           TPT
104300970116     C                   MOVEA     VOL           NVO
104400050105     C                   MOVE      V4C6pu        §7R6pu
104500940930     C                   MOVE      V4C5EV        §7R5EV
104600940930     C                   MOVE      V4C5TM        §7R5TM
104700050105     C                   MOVE      V4C5pu        §7R5pu
104800940930     C                   MOVEL     V3C1BF        §7R1BF
104900050503     C                   movel     V3Cldv        §7Rldv
105000940930     C                   MOVEL     V3C1CB        §7R1CB
105100940930     C                   MOVEL     V3C2OR        §7R2OR
105200940930     C                   MOVEL     V3C2FI        §7R2FI
105300940930     C                   MOVEL     V3C3TM        §7R3TM
105400050105     C                   MOVEL     V3C3pu        §7R3pu
105500940930     C                   MOVEL     V3C3EV        §7R3EV
105600940930     C                   MOVEL     V3C4TM        §7R4TM
105700050105     C                   MOVEL     V3C4pu        §7R4pu
105800940930     C                   MOVEL     V3C4NT        §7R4NT
105900940930     C                   MOVEL     V3C4EV        §7R4EV
106000050503     c                   movel     v8c8cp        §7r8cp
106100050503     c                   movel     v8c8di        §7r8di
106200050503     c                   move      v8c8gg        §7r8gg
106300081127     c                   movel     v8c9cd        §7r9cd
106400100505     c                   movel     v8c10ca       §7ratt
106500100505     c                   movel     v8c11rc       §7rrc
106600120426     c                   movel     v8cvpcd       §7rvpcd
106700120426     c                   movel     v8cldvfir     §7rldvfir
106800130610     c                   move      v8ggfd        §7RGGFD
106900140409     c                   move      v9c15fd       §7RFD
107000140409     c                   move      v9c16ng       §7RAG
107100141118     C                   eval      §7RPINCODE = v9pincode
107200111104     c                   eval      §7Raut = V7Caut
107300940929     C                   MOVEL     *BLANKS       TBLUNI
107400940930     C                   MOVEL     DS7R          TBLUNI
107500970116     C     V7CFTR        IFEQ      ' '
107600940929     C                   MOVEL     '1'           TBLFTT
107700960111     C                   MOVEL     ' '           DS7TRA
107800940929     C                   ELSE
107900940929     C                   MOVEL     ' '           TBLFTT
108000960111     C                   MOVEL     'N'           DS7TRA
108100940929     C                   END
108200951130     C                   MOVEL     *BLANKS       TBLFTR
108300951130     C                   MOVEL     *ZEROS        TBLDTR
108400940929     C     *IN31         IFEQ      '1'
108500940418     C                   Z-ADD     1             TBLKUT
108600940930     C                   MOVEL     '7R'          TBLCOD
108700940725     C                   MOVEL     *BLANKS       TBLKEY
108800940929     C                   MOVEL     V3CCOD        TBLKEY
108900931209     C                   WRITE     TABEL
109000940725     C                   ELSE
109100940725     C                   UPDATE    TABEL
109200940418     C                   END
109300940725     C*----------------------------------------------------*
109400931209     C                   ENDSR
109500931209     C*--------------------------------------------------------------*
109600931209     C*  ANNUL: Annullamento                                         *
109700931209     C*--------------------------------------------------------------*
109800931209     C     ANNUL         BEGSR
109900931209     C*----------------------------------------------------*
110000940929     C                   MOVEL     *BLANKS       KKEY
110100940930     C                   MOVE      '7R'          KCOD
110200940929     C                   MOVEL     V3CCOD        KKEY
110300940929     C     KTAB2         CHAIN     TABEL00F                           80
110400940725     C     *IN80         IFEQ      '0'
110500960111     C                   MOVEL     'A'           DS7RIP
110600940418     C                   MOVEL     '*'           TBLFLG
110700940418     C                   EXCEPT    ANNTBL
110800940418     C                   END
110900931209     C*----------------------------------------------------*
111000931209     C                   ENDSR
111100931209     C*--------------------------------------------------------------*
111200931209     C*  RIPRI: Ripristino                                           *
111300931209     C*--------------------------------------------------------------*
111400931209     C     RIPRIS        BEGSR
111500931209     C*----------------------------------------------------*
111600940929     C                   MOVEL     *BLANKS       KKEY
111700940930     C                   MOVE      '7R'          KCOD
111800940929     C                   MOVEL     V3CCOD        KKEY
111900940929     C     KTAB2         CHAIN     TABEL00F                           80
112000940725     C     *IN80         IFEQ      '0'
112100960111     C                   MOVEL     'R'           DS7RIP
112200940418     C                   MOVEL     ' '           TBLFLG
112300940418     C                   EXCEPT    ANNTBL
112400931209     C                   END
112500931209     C*----------------------------------------------------*
112600931209     C                   ENDSR
112700931209     C*--------------------------------------------------------------*
112800940928     C*  DEFCAM: Operazioni iniziali                                 *
112900931209     C*--------------------------------------------------------------*
113000940928     C     DEFCAM        BEGSR
113100931209     C*
113200931209     C*  Richiamo XPARUT
113300931209     C                   Z-ADD     1             CODUT
113400931209     C                   CALL      'XPARUT'
113500931209     C                   PARM                    UTEDSE
113600931209     C     CODUT         CABEQ     -1            FINE
113700940927     C                   MOVEL     RAGUT         V1CRSU
113800931209     C*----------------------------------------------------*
113900931209     C* Definizione chiavi                                 *
114000940927     C     KTAB1         KLIST
114100940927     C                   KFLD                    KKUT
114200940927     C                   KFLD                    KCOD
114300940927     C*
114400940927     C     KTAB2         KLIST
114500940927     C                   KFLD                    KKUT
114600940927     C                   KFLD                    KCOD
114700940927     C                   KFLD                    KKEY
114800931209     C*----------------------------------------------------*
114900931209     C* Definizione variabili                              *
115000940927     C     *LIKE         DEFINE    TBLKUT        KKUT
115100940927     C     *LIKE         DEFINE    TBLCOD        KCOD
115200940927     C     *LIKE         DEFINE    TBLKEY        KKEY
115300931209     C*----------------------------------------------------*
115400940929     C*  Caricamento tabelle descrizioni solo se non devo
115500940929     C*  eseguire i soli controlli
115600940929     C     DS7GES        IFNE      'C'
115700940929     C*----------------------------------------------------*
115800940930     C*  Caricamento tabella 3a
115900940927     C                   Z-ADD     1             KKUT
116000940930     C                   MOVE      '3A'          KCOD
116100090126     C                   Z-ADD     0             X                 3 0
116200940927     C     KTAB1         CHAIN     TABEL00F                           31
116300940927     C     *IN31         DOWEQ     '0'
116400940725     C     X             ANDLT     50
116500940725     C                   ADD       1             X
116600940930     C                   MOVEL     TBLKEY        C3A(X)
116700940930     C                   MOVEL     TBLUNI        D3A(X)
116800940929     C     KTAB1         READE     TABEL00F                               31
116900940725     C                   END
117000940927     C*----------------------------------------------------*
117100940927     C*  Caricamento tabella 2A E 2Z
117200090126     C                   Z-ADD     0             X
117300940927     C                   MOVEL     '2A'          KCOD
117400940927     C     KTAB1         CHAIN     TABEL                              31
117500940927     C     *IN31         DOWEQ     '0'
117600041116     C     X             ANDLT     250
117700940927     C                   MOVEL     TBLUNI        DS2A
117800940930     C     TBLFLG        IFEQ      ' '
117900940927     C                   ADD       1             X
118000940927     C                   MOVEL     TBLKEY        C2A(X)
118100940930     C                   MOVEL     §2ADES        D2A(X)
118200940930     C                   MOVEL     §2AFTC        T2A(X)
118300940927     C                   END
118400940927     C     KTAB1         READE     TABEL                                  31
118500940927     C                   END
118600940927     C*
118700940927     C                   MOVEL     '2Z'          KCOD
118800940929     C     KTAB1         CHAIN     TABEL                              31
118900940927     C     *IN31         DOWEQ     '0'
119000940927     C                   MOVEL     TBLUNI        DS2A
119100940927     C     TBLFLG        IFEQ      ' '
119200940927     C                   Z-ADD     1             X
119300940927     C                   MOVEL     TBLKEY        COD2Z             3
119400940927     C     COD2Z         LOOKUP    C2A(X)                                 32
119500940930     C     *IN32         IFEQ      '1'
119600940927     C                   MOVEL     TBLUNI        DS2Z
119700950104     C                   MOVEL     §2ZTIC        T2Z(X)
119800940927     C                   END
119900940927     C                   END
120000940929     C     KTAB1         READE     TABEL                                  31
120100940927     C                   END
120200940928     C*----------------------------------------------------*
120300940928     C                   END
120400940928     C*----------------------------------------------------*
120500931209     C                   ENDSR
120600931209     O*-----------------------------------------------------*
120700931209     OTABEL     E            ANNTBL
120800931209     O                       TBLFLG
120900970116**
121000940930Codice evento inesistente                                             01
121100940928Tipologia giacenza inesistente                                        02
121200940930Immettere numero tentativi consegna                                   03
121300940928Immettere tipologia giacenza                                          04
121400940930Immettere codice evento                                               05
121500940930Tipo mancata consegna inesistente                                     06
121600960111Il valore immesso per il campo non è valido                           07
121700960111Codice bolla inesistente                                              08
121800960111Tipologia già prevista                                                09
121900960111Immettere tipo mancata consegna                                       10
122000111104Codice "Chi Autorizza" errato                                         11
122100141119Se Forzatura Firma su LDV=S, non è possibile usare il PINCODE         12
122200940419**
122300110412   IMMISSIONE
122400110412   VARIAZIONE
122500110412 INTERROGAZIONE
122600940928**
1227009409304243444546
122800940928**
1229009409304748495051
123000940930**
1231009409305253545556
123200970116**
1233009701165758596061
