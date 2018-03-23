000100990119     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990119     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* ANG900R  *-AN/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*       - MANUTENZIONE ANAGRAFICO MATERIE PRIME -              *
000600000000     F*       -----------------------------------------              *
000700000000     F*                                                   AZN.: G900 *
000800990119     F*         FILE : tmamp01L - ANTAB00F - ANG900D - tmfop01L      *
000900000000     F*                                                              *
001000000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100000000     F*                RICDATE   - CONTROLLA E INVERTE DATE          *
001200990317     F*                tii104R   - RICERCA ARTICOLI MATERIE PRIME    *
001300990317     F*                tii110R   - RICERCA CLASSE MERCEOLOGICA       *
001400990317     F*                TII112R   - RICERCA SOTTOCLASSE MERCEOLOG.    *
001500990317     F*                TII109R   - RICERCA UNITA' DI MISURA          *
001600990317     F*                TII118R   - RICERCA CATEGORIA FISCALE         *
001700990317     F*                TII116R   - RICERCA LISTINI MATERIE PRIME     *
001800000000     F*                ANU164R   - RICERCA FORNITORI USO INTERNO     *
001900990317     F*                TII113R   - RICERCA IMBALLO                   *
002000990317     F*                tii108R   - RICERCA MAGAZZINI                 *
002100000000     F*                                                              *
002200000000     F*--------------------------------------------------------------*
002300990119     Ftmamp01L  UF A E           K DISK
002400000000     F*---------
002500000000     FANTAB00F  IF   E           K DISK
002600871024     F*---------
002700990119     Ftmfop01L  IF   E           K DISK
002800000000     F*---------
002900990317     Ftii009D   CF   E             WORKSTN
003000000000     F*--------------------------------------------------------------*
003100000000     D ERR             S             30    DIM(1) CTDATA PERRCD(1)              ERRORE
003200000000     D KPJBA         E DS
003300000000     D*-----PASSAGGIO PARAMETRI
003400000000     D  MSGERR               453    502
003500990119     D soc001        E DS                  EXTNAME(xsoc001ds)
003600990119     D xsocds          DS          1000
003700990119     d dataiso         s               d   datfmt(*iso)
003800990119     d dataeur         s               d   datfmt(*eur)
003900990409     D a064ds        E DS                  EXTNAME(ana064ds)
004000990119     d
004100871024     C*--- RICERCA PARAMETRI VARI -------------------------*
004200871024     C******************************************************
004300000000     C     *ENTRY        PLIST                                                  *
004400000000     C                   PARM                    KPJBA                          *
004500990119     C*---------- RICERCA DITTA :
004600990119     C                   MOVEL     'SOC001'      TIPXSC
004700990119     C                   MOVEL     *blank        SOCXSC
004800990119     C                   EXSR      REPSOC
004900990119     C     RTNXSC        IFNE      '1'
005000990119     C                   MOVEL     XSOCDS        SOC001
005100990119     C                   MOVEL     xscrgs        RSUT             20
005200990119     C                   MOVEL     xscrgs        desdit
005300990119     c                   end
005400990119     c                   time                    como14           14 0
005500990119     c                   move      como14        woggi             8 0
005600990119     c                   move      woggi         dataeur
005700990119     c                   move      dataeur       dataiso
005800990119     c                   move      dataiso       woggig            8 0
005900000000     C*                                                    *
006000000000     C     KEYART        KLIST                                                  *
006100000000     C                   KFLD                    CODART                         *
006200000000     C*---------- KEY PER RICERCA TABELLE :                *
006300000000     C     KEYTAB        KLIST                                                  *
006400000000     C                   KFLD                    TABGRU                         *
006500000000     C                   KFLD                    TABCOD                         *
006600000000     C*----------------------------------------------------*
006700000000     C*     EMISSIONE PROMPT                               *
006800000000     C*----------------------------------------------------*
006900000000     C*                                                    *
007000871013     C                   MOVEL     *BLANKS       INTERR           12
007100861211     C                   MOVEL     '?'           INTERR
007200871013     C                   MOVEL     '00000000'    ZERO             12
007300871013     C                   MOVE      '00000000'    ZERO
007400871013     C                   MOVEL     '99999999'    NOVE             12
007500871013     C                   MOVE      '99999999'    NOVE
007600000000     C                   WRITE     ANG9001                                      *
007700000000     C     INIZIO        TAG                                                    *
007800000000     C                   SETOFF                                       12        *
007900000000     C   82                                                                     * VER.GIA' ESI.
008000000000     COR 92                                                                     * VER.NUO. ERR.
008100000000     COR 91              SETON                                            90    * ART. ERRATO
008200000000     C                   EXFMT     ANG9002                                      *
008300000000     C                   MOVEA     *ZEROS        *IN(1)                         *
008400990408     C   Kc              GOTO      FINE                                         *
008500000000     C*---------- CONTROLLO DATI INPUT :                   *
008600861211     C     CODART        CABEQ     NOVE          INIZIO                   91    *
008700861211     C     CODART        CABEQ     ZERO          INIZIO                   91    *
008800000000     C*--- SCELTO INSERIMENTO ARTICOLO NUOVO - CALCOLO     *
008900000000     C*--- AUTOMATICO CODICE ARTICOLO - :                  *
009000000000     C     CODART        IFEQ      *BLANKS                                      *
009100861211     C                   GOTO      INIZIO                                       *
009200000000     C                   ELSE                                                   *
009300000000     C*---------- RICERCA ALFABETICA :                     *
009400861211     C     CODART        IFEQ      INTERR                                       *
009500000000     C                   MOVE      *BLANKS       CODART                         *
009600990317     C                   CALL      'TII104R'                                    *
009700000000     C                   PARM                    CODART                         *
009800000000     C                   PARM                    DESART                         *
009900000000     C                   GOTO      INIZIO                                       *
010000000000     C                   END                                                    *
010100000000     C                   END                                                    *
010200000000     C*----------                                          *
010300000000     C     SOPRA         TAG                                                    *
010400000000     C                   EXSR      RUTCTL                                       *
010500000000     C   82              MOVEL     ERR(1)        DESART                         *
010600990408     C   82              GOTO      INIZIO                                       *
010700000000     C*----------                                          *
010800000000     C     SOPRA1        TAG                                                    *
010900990119     C   02              Z-ADD     woggi         VIDDIG                         *
011000000000     C   10              SETON                                        11        *
011100000000     C*---------- ERRORI A VIDEO :                         *
011200860924     C   82                                                                     *
011300000000     COR 83                                                                     *
011400000000     COR 84                                                                     *
011500000000     COR 85              SETON                                        90        *
011600000000     C   86                                                                     *
011700000000     COR 87                                                                     *
011800000000     COR 88                                                                     *
011900000000     COR 89              SETON                                        90        *
012000000000     C   91                                                                     *
012100000000     COR 92                                                                     *
012200000000     COR 93                                                                     *
012300000000     COR 94                                                                     *
012400000000     COR 95              SETON                                        90        *
012500000000     C   96                                                                     *
012600000000     COR 97                                                                     *
012700000000     COR 98                                                                     *
012800000000     COR 99              SETON                                        90        *
012900000000     C*----------                                          *
013000000000     C                   EXFMT     ANG9003                                      *
013100000000     C   02              MOVE      AMPMAT        CODART                         *
013200000000     C                   MOVEA     *ZEROS        *IN(80)                        *
013300990119     C   Kl              MOVE      *BLANKS       DESART                         *
013400990119     C   Kl              GOTO      INIZIO                                       *
013500000000     C                   GOTO      SOPRA                                        *
013600000000     C*     USCITA PROGRAMMA                               *
013700000000     C     FINE          TAG                                                    *
013800000000     C                   SETON                                            LR    *
013900000000     C***------------------------------------------------***
014000000000     C**--------- CONTROLLO DATI IMMESSI ----------------***
014100000000     C***------------------------------------------------***
014200000000     CSR   RUTCTL        BEGSR                                                  *
014300990119     C  N11KEYART        CHAIN     tmamp01L                           10        *
014400000000     C  N11
014500000000     CAN 10              EXSR      RUVIAR                                       *INS.ART.NUOVO
014600000000     C  N11
014700000000     CAN 10              GOTO      SOPRA1                                       *
014800000000     C  N11
014900000000     CANN10
015000000000     CANN12              EXSR      RUVIVE                                       *IN.VE.NU./AGG.
015100000000     C  N11
015200000000     CANN10
015300000000     CANN12              SETON                                        12        *
015400000000     C*--------- CONTROLLO CAMPI ------------------------***
015500000000     C*                                                    *
015600000000     C*--------- GIRA DATA FINE GESTIONE                   *
015700000000     C     VIDDFG        IFNE      0                                            *
015800990119     c     *eur          test(d)                 viddfg                 36
015900990119     c                   if        *in36
016000990119     c     *dmy          test(d)                 viddfg                 36
016100990119     c   36              goto      sopra1
016200990119     c     *dmy          move      viddfg        dataeur
016300990119     c                   move      dataeur       dataiso
016400990119     c                   move      dataiso       ampdfg
016500990119     c                   else
016600990119     c                   move      viddfg        dataeur
016700990119     c                   move      dataeur       dataiso
016800990119     c                   move      dataiso       ampdfg
016900990119     c                   endif
017000990119     c                   z-add     0             ampdfg
017100000000     C                   END                                                    *
017200000000     C*---------                                           *CLASSE MERC.
017300000000     C     VIDCLM        IFEQ      '?  '                                        *
017400000000     C                   MOVE      *BLANKS       VIDCLM                         *
017500000000     C                   MOVE      *BLANKS       DESCLM           25            *
017600990317     C                   CALL      'TII110R'                                    *
017700000000     C                   PARM                    VIDCLM                         *
017800000000     C                   PARM                    DESCLM                         *
017900000000     C     VIDCLM        COMP      '999'                                  91    *
018000890626     C  N91              DO                                                     *
018100890626     C                   MOVE      '155'         TABGRU                         *
018200890626     C                   MOVE      *ZEROS        TABCOD                         *
018300890626     C                   MOVE      VIDCLM        TABCOD                         *
018400890626     C     KEYTAB        CHAIN     ANTAB00F                           70        *
018500890626     C     *IN(70)       CABEQ     '1'           SOPRA1                   91    *
018600890626     C                   END                                                    *
018700890626     C                   GOTO      SOPRA1                                       *
018800890626     C                   ELSE                                                   *
018900890626     C                   MOVE      '155'         TABGRU                         *
019000890626     C                   MOVE      *ZEROS        TABCOD                         *
019100890626     C                   MOVE      VIDCLM        TABCOD                         *
019200890626     C     KEYTAB        CHAIN     ANTAB00F                           70        *
019201110905     C     *IN(70)       CABEQ     '1'           SOPRA1                   91    *
019300890626     C                   END                                                    *
019400000000     C*---------                                           *S/CLASSE MERC.
019500860314     C     VIDSCM        IFEQ      '?    '                                      *
019600000000     C                   MOVE      *BLANKS       VIDSCM                         *
019700000000     C                   MOVE      *BLANKS       DESSCM           25            *
019800990317     C                   CALL      'TII111R'                                    *
019900000000     C                   PARM                    VIDSCM                         *
020000000000     C                   PARM                    DESSCM                         *
020100860314     C     VIDSCM        COMP      '99999'                                92    *
020200890626     C                   GOTO      SOPRA1
020300110826     C  N92              DO                                                     *
020400110826     C                   MOVE      '156'         TABGRU                         *
020500110826     C                   MOVE      *ZEROS        TABCOD                         *
020600110826     C                   MOVE      VIDSCM        TABCOD                         *
020700110826     C     KEYTAB        CHAIN     ANTAB00F                           70        *
020800110826     C     *IN(70)       CABEQ     '1'           SOPRA1                   92    *
020900110826     C                   END                                                    *
021000110826     C                   GOTO      SOPRA1                                       *
021100110826     C                   ELSE                                                   *
021200110826     C                   MOVE      '156'         TABGRU                         *
021300110826     C                   MOVE      *ZEROS        TABCOD                         *
021400110826     C                   MOVE      VIDSCM        TABCOD                         *
021500110826     C     KEYTAB        CHAIN     ANTAB00F                           70        *
021501110905     C     *IN(70)       CABEQ     '1'           SOPRA1                   92    *
021600890626     C                   END
021700000000     C*---------                                           *UN.MIS.ANAGRAF.
021800000000     C     VIDUMA        IFEQ      '?  '                                        *
021900000000     C                   MOVE      *BLANKS       VIDUMA                         *
022000000000     C                   MOVE      *BLANKS       DESUMA           25            *
022100990317     C                   CALL      'TII109R'                                    *
022200000000     C                   PARM                    VIDUMA                         *
022300000000     C                   PARM                    DESUMA                         *
022400000000     C     VIDUMA        COMP      '999'                                  93    *
022500000000     C                   GOTO      SOPRA1                                       *
022600000000     C                   ELSE                                                   *
022700000000     C                   MOVE      '154'         TABGRU                         *
022800000000     C                   MOVE      *ZEROS        TABCOD                         *
022900000000     C                   MOVE      VIDUMA        TABCOD                         *
023000000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
023100110905     C     *IN(70)       CABEQ     '0'           SOPRA1                   93    *
023200000000     C                   END                                                    *
023300000000     C*---------                                           *UN.MIS.STOCCAG.
023400000000     C     VIDUMS        IFEQ      '?  '                                        *
023500000000     C                   MOVE      *BLANKS       VIDUMS                         *
023600000000     C                   MOVE      *BLANKS       DESUMS           25            *
023700990317     C                   CALL      'TII109R'                                    *
023800000000     C                   PARM                    VIDUMS                         *
023900000000     C                   PARM                    DESUMS                         *
024000000000     C     VIDUMS        COMP      '999'                                  94    *
024100000000     C                   GOTO      SOPRA1                                       *
024200000000     C                   ELSE                                                   *
024300000000     C                   MOVE      '154'         TABGRU                         *
024400000000     C                   MOVE      *ZEROS        TABCOD                         *
024500000000     C                   MOVE      VIDUMS        TABCOD                         *
024600000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
024700000000     C     *IN(70)       CABEQ     '0'           SOPRA1                   94    *
024800000000     C                   END                                                    *
024900000000     C*--------- FATTORE CONVERSIONE DA UMA A UMS          *
025000000000     C     VIDFCS        CABLE     0             SOPRA1                 8383    *
025100000000     C*---------                                           *UN.MIS.ACQUISTO
025200000000     C     VIDUMQ        IFEQ      '?  '                                        *
025300000000     C                   MOVE      *BLANKS       VIDUMQ                         *
025400000000     C                   MOVE      *BLANKS       DESUMQ           25            *
025500990317     C                   CALL      'TII109R'                                    *
025600000000     C                   PARM                    VIDUMQ                         *
025700000000     C                   PARM                    DESUMQ                         *
025800000000     C     VIDUMQ        COMP      '999'                                  95    *
025900000000     C                   GOTO      SOPRA1                                       *
026000000000     C                   ELSE                                                   *
026100000000     C                   MOVE      '154'         TABGRU                         *
026200000000     C                   MOVE      *ZEROS        TABCOD                         *
026300000000     C                   MOVE      VIDUMQ        TABCOD                         *
026400000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
026500000000     C     *IN(70)       CABEQ     '0'           SOPRA1                   95    *
026600000000     C                   END                                                    *
026700000000     C*--------- FATTORE CONVERSIONE DA UMA A UMQ          *
026800000000     C     VIDFCQ        CABLE     0             SOPRA1                 8484    *
026900000000     C*---------                                           *UN.MIS.VENDITA
027000000000     C     VIDUMV        IFEQ      '?  '                                        *
027100000000     C                   MOVE      *BLANKS       VIDUMV                         *
027200000000     C                   MOVE      *BLANKS       DESUMV           25            *
027300990317     C                   CALL      'TII109R'                                    *
027400000000     C                   PARM                    VIDUMV                         *
027500000000     C                   PARM                    DESUMV                         *
027600000000     C     VIDUMV        COMP      '999'                                  96    *
027700000000     C                   GOTO      SOPRA1                                       *
027800000000     C                   ELSE                                                   *
027900000000     C                   MOVE      '154'         TABGRU                         *
028000000000     C                   MOVE      *ZEROS        TABCOD                         *
028100000000     C                   MOVE      VIDUMV        TABCOD                         *
028200000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
028300000000     C     *IN(70)       CABEQ     '0'           SOPRA1                   96    *
028400000000     C                   END                                                    *
028500000000     C*--------- FATTORE CONVERSIONE DA UMA A UMQ          *
028600000000     C     VIDFCV        CABLE     0             SOPRA1                 8585    *
028700000000     C*---------                                           *CATEG.FISCALE
028800000000     C     VIDCTF        IFEQ      '? '                                         *
028900000000     C                   MOVE      *BLANKS       VIDCTF                         *
029000000000     C                   MOVE      *BLANKS       DESCTF           25            *
029100990317     C                   CALL      'TII118R'                                    *
029200000000     C                   PARM                    VIDCTF                         *
029300000000     C                   PARM                    DESCTF                         *
029400000000     C     VIDCTF        COMP      '99'                                   97    *
029500000000     C                   GOTO      SOPRA1                                       *
029600000000     C                   ELSE                                                   *
029700000000     C                   MOVE      '163'         TABGRU                         *
029800000000     C                   MOVE      *ZEROS        TABCOD                         *
029900000000     C                   MOVE      VIDCTF        TABCOD                         *
030000000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
030100000000     C     *IN(70)       CABEQ     '0'           SOPRA1                   97    *
030200000000     C                   END                                                    *
030300000000     C*---------                                           *COD.LISTINO
030400000000     C     VIDLIS        IFEQ      '?  '                                        *
030500000000     C                   MOVE      *BLANKS       VIDLIS                         *
030600000000     C                   MOVE      *BLANKS       DESLIS           75            *
030700990317     C                   CALL      'TII116R'                                    *
030800000000     C                   PARM                    VIDLIS                         *
030900000000     C                   PARM                    DESLIS                         *
031000000000     C     VIDLIS        COMP      '999'                                  98    *
031100000000     C                   GOTO      SOPRA1                                       *
031200000000     C                   ELSE                                                   *
031300000000     C                   MOVE      '161'         TABGRU                         *
031400000000     C                   MOVE      *ZEROS        TABCOD                         *
031500000000     C                   MOVE      VIDLIS        TABCOD                         *
031600000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
031700000000     C     *IN(70)       CABEQ     '0'           SOPRA1                   98    *
031800000000     C                   END                                                    *
031900000000     C*---------                                           *IMBALLO
032000000000     C     VIDIMB        IFEQ      '?      '                                    *
032100000000     C                   MOVE      *BLANKS       VIDIMB                         *
032200000000     C                   MOVE      *BLANKS       DESIMB           75            *
032300990317     C                   CALL      'TII113R'                                    *
032400000000     C                   PARM                    VIDIMB                         *
032500000000     C                   PARM                    DESIMB                         *
032600000000     C     VIDIMB        COMP      '9999999'                              89    *
032700000000     C                   GOTO      SOPRA1                                       *
032800000000     C                   ELSE                                                   *
032900000000     C                   MOVE      '158'         TABGRU                         *
033000000000     C                   MOVE      *ZEROS        TABCOD                         *
033100000000     C                   MOVE      VIDIMB        TABCOD                         *
033200000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
033300000000     C     *IN(70)       CABEQ     '0'           SOPRA1                   89    *
033400000000     C                   END                                                    *
033500860131     C*---------                                           *MAGAZZINO
033600860131     C     VIDMAG        IFNE      '9999999'                                    *
033700000000     C     VIDMAG        IFEQ      '?      '                                    *
033800000000     C                   MOVE      *BLANKS       VIDMAG                         *
033900000000     C                   MOVE      *BLANKS       DESMAG           75            *
034000990317     C                   CALL      'TII108R'                                    *
034100000000     C                   PARM                    VIDMAG                         *
034200000000     C                   PARM                    DESMAG                         *
034300000000     C                   ELSE                                                   *
034400000000     C                   MOVE      '153'         TABGRU                         *
034500000000     C                   MOVE      *ZEROS        TABCOD                         *
034600000000     C                   MOVE      VIDMAG        TABCOD                         *
034700000000     C     KEYTAB        SETLL     ANTAB00F                               70    *
034800000000     C     *IN(70)       CABEQ     '0'           SOPRA1                   87    *
034900000000     C                   END                                                    *
035000860131     C                   END                                                    *
035100990408     c                   setoff                                       6667
035200990409     c
035300990409     C*----------RICERCA aliquota o esenzione vendita
035400990409     c                   if        videsv = '?  '
035500990409     c                   clear                   a064ds
035600990409     C                   MOVEL     '01'          OPZ064
035700990409     C                   MOVEL     '2'           tpr064
035800990409     C                   MOVEL     *blank        vidaiv
035900990409     C                   MOVEL     *blank        videsv
036000990409     C                   MOVEL     A064DS        KPJBU
036100990409     C                   CALL      'ANA064R'
036200990409     C                   PARM                    KPJBA
036300990409     C                   MOVEL     KPJBU         A064DS
036400990409     C     ali064        IFNE      *zeros
036500990409     C                   MOVEL     ali064        vidaiv
036600990409     C                   GOTO      sopra1                                       *
036700990409     C                   ENDIF
036800990409     C     rfi064        IFNE      *blank
036900990409     C                   MOVEL     rfi064        videsv
037000990409     C                   GOTO      sopra1                                       *
037100990409     C                   ENDIF
037200990409     C                   ENDIF
037300990408     c                   if        vidaiv > 0 and videsv <> *blank or
037400990408     c                             vidaiv = 0 and videsv = *blank
037500990408     c                   seton                                        66
037600990408     c   66              goto      sopra1
037700990408     c                   end
037800990413     C*----------RICERCA aliquota o esenzione acquisto
037900990413     c                   if        videsa = '?  '
038000990413     c                   clear                   a064ds
038100990413     C                   MOVEL     '01'          OPZ064
038200990413     C                   MOVEL     '1'           tpr064
038300990413     C                   MOVEL     *blank        vidaia
038400990413     C                   MOVEL     *blank        videsa
038500990413     C                   MOVEL     A064DS        KPJBU
038600990413     C                   CALL      'ANA064R'
038700990413     C                   PARM                    KPJBA
038800990413     C                   MOVEL     KPJBU         A064DS
038900990413     C     ali064        IFNE      *zeros
039000990413     C                   MOVEL     ali064        vidaia
039100990413     C                   GOTO      sopra1                                       *
039200990413     C                   ENDIF
039300990413     C     rfi064        IFNE      *blank
039400990413     C                   MOVEL     rfi064        videsa
039500990413     C                   GOTO      sopra1                                       *
039600990413     C                   ENDIF
039700990413     C                   ENDIF
039800990413     c                   if        vidaia > 0 and videsa <> *blank or
039900990413     c                             vidaia = 0 and videsa = *blank
040000990413     c                   seton                                        67
040100990413     c   67              goto      sopra1
040200990413     c                   end
040300990408     c   kf              do
040400990408     C                   EXSR      RUTFIL                                       *
040500990408     c                   goto      inizio
040600990408     c                   enddo
040700000000     C                   SETOFF                                       02        *
040800000000     CSR                 ENDSR                                                  *
040900000000     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
041000000000     C**--- INSERIMENTO ARTICOLO NUOVO ------------------***
041100000000     C***------------------------------------------------***
041200000000     CSR   RUVIAR        BEGSR                                                  *
041300000000     C                   MOVE      CODART        AMPMAT                         *
041400990119     C                   MOVE      woggi         VIDDIG                         *
041500000000     C                   MOVE      *ZEROS        VIDDFG                         *
041600000000     C                   MOVE      *BLANKS       VIDDMT                         *
041700000000     C                   MOVE      *BLANKS       VIDCLM                         *
041800000000     C                   MOVEL     '?'           VIDCLM                         *
041900000000     C                   MOVE      *ZEROS        VIDLRO                         *
042000000000     C                   MOVE      *BLANKS       VIDSCM                         *
042100000000     C                   MOVEL     '?'           VIDSCM                         *
042200000000     C                   MOVE      *ZEROS        VIDLVM                         *
042300000000     C                   MOVE      *BLANKS       VIDUMA                         *
042400871118     C                   MOVEL     'NR'          VIDUMA                         *
042500000000     C                   MOVE      *ZEROS        VIDLEC                         *
042600000000     C                   MOVE      *BLANKS       VIDUMS                         *
042700871118     C                   MOVEL     'NR'          VIDUMS                         *
042800871118     C                   Z-ADD     1             VIDFCS                         *
042900000000     C                   MOVE      *BLANKS       VIDUMQ                         *
043000871118     C                   MOVEL     'NR'          VIDUMQ                         *
043100871118     C                   Z-ADD     1             VIDFCQ                         *
043200000000     C                   MOVE      *BLANKS       VIDUMV                         *
043300871118     C                   MOVEL     'NR'          VIDUMV                         *
043400871118     C                   Z-ADD     1             VIDFCV                         *
043500000000     C                   MOVE      *BLANKS       VIDCTF                         *
043600871207     C                   MOVEL     '1'           VIDCTF                         *
043700000000     C                   MOVE      *ZEROS        VIDGAP                         *
043800000000     C                   MOVE      *BLANKS       VIDLIS                         *
043900871118     C                   MOVEL     '1  '         VIDLIS                         *
044000000000     C                   MOVE      *BLANKS       VIDAMT                         *
044100000000     C                   MOVE      *ZEROS        VIDPDC                         *
044200000000     C                   MOVE      *ZEROS        VIDTRP                         *
044300000000     C                   MOVE      *ZEROS        VIDCCA                         *
044400000000     C                   MOVE      *ZEROS        VIDPNT                         *
044500000000     C                   MOVE      *ZEROS        VIDTAR                         *
044600000000     C                   MOVE      *ZEROS        VIDAIV                         *
044700841017     C                   MOVE      *BLANKS       VIDIMB                         *
044800871118     C                   MOVEL     '1      '     VIDIMB                         *
044900000000     C                   MOVE      *BLANKS       VIDMAG                         *
045000871118     C                   MOVEL     '1      '     VIDMAG                         *
045100000000     C                   MOVE      *BLANKS       VIDUBI                         *
045200990119     C                   MOVE      *BLANKS       VIDesa                         *
045300990119     C                   MOVE      *BLANKS       VIDesv                         *
045400000000     C                   MOVE      *ZEROS        VIDAIA                         *
045500000000     CSR                 ENDSR                                                  *
045600000000     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
045700000000     C**--- AGGIORNAMENTO ARTICOLO ESISTENTE ------------***
045800000000     C**--- INSERIMENTO VERSIONE NUOVA ------------------***
045900000000     C***------------------------------------------------***
046000000000     CSR   RUVIVE        BEGSR                                                  *
046100000000     C*--------- GIRA DATA INIZIO GESTIONE                 *
046200990119     c                   if        ampdig > 0
046300990119     c                   move      ampdig        dataiso
046400990119     c                   move      dataiso       dataeur
046500990119     c                   move      dataeur       viddig
046600990119     c                   end
046700000000     C*--------- GIRA DATA FINE GESTIONE                   *
046800990119     c                   if        ampdfg > 0
046900990119     c                   move      ampdfg        dataiso
047000990119     c                   move      dataiso       dataeur
047100990119     c                   move      dataeur       viddfg
047200990119     c                   end
047300990119     C*--------- GIRA DATA ultima uscita                   *
047400990119     c                   if        amptar > 0
047500990119     c                   move      amptar        dataiso
047600990119     c                   move      dataiso       dataeur
047700990119     c                   move      dataeur       vidtar
047800990119     c                   end
047900000000     C*---------                                           *
048000000000     C                   MOVE      AMPDMT        VIDDMT                         *
048100000000     C                   MOVE      AMPCLM        VIDCLM                         *
048200000000     C                   Z-ADD     AMPLRO        VIDLRO                         *
048300000000     C                   MOVE      AMPSCM        VIDSCM                         *
048400000000     C                   Z-ADD     AMPLVM        VIDLVM                         *
048500000000     C                   MOVE      AMPUMA        VIDUMA                         *
048600000000     C                   Z-ADD     AMPLEC        VIDLEC                         *
048700000000     C                   MOVE      AMPUMS        VIDUMS                         *
048800000000     C                   Z-ADD     AMPFCS        VIDFCS                         *
048900000000     C                   MOVE      AMPUMQ        VIDUMQ                         *
049000000000     C                   Z-ADD     AMPFCQ        VIDFCQ                         *
049100000000     C                   MOVE      AMPUMV        VIDUMV                         *
049200000000     C                   Z-ADD     AMPFCV        VIDFCV                         *
049300000000     C                   MOVE      AMPCTF        VIDCTF                         *
049400000000     C                   Z-ADD     AMPGAP        VIDGAP                         *
049500000000     C                   MOVE      AMPLIS        VIDLIS                         *
049600000000     C                   MOVE      AMPAMT        VIDAMT                         *
049700000000     C                   Z-ADD     AMPPDC        VIDPDC                         *
049800000000     C                   Z-ADD     AMPTRP        VIDTRP                         *
049900000000     C                   Z-ADD     AMPCCA        VIDCCA                         *
050000000000     C                   Z-ADD     AMPPNT        VIDPNT                         *
050100000000     C                   Z-ADD     AMPAIV        VIDAIV                         *
050200990119     C                   move      AMPesV        VIDesV                         *
050300000000     C                   MOVE      AMPIMB        VIDIMB                         *
050400000000     C                   MOVE      AMPMAG        VIDMAG                         *
050500000000     C                   MOVE      AMPUBI        VIDUBI                         *
050600000000     C                   Z-ADD     AMPAIA        VIDAIA                         *
050700990119     C                   move      AMPesa        VIDesa                         *
050800000000     CSR                 ENDSR                                                  *
050900000000     C***------------------------------------------------***
051000000000     C**--- IMPOSTAZIONE CAMPI X SCRITTURA FILE ---------***
051100000000     C***------------------------------------------------***
051200000000     CSR   RUTFIL        BEGSR                                                  *
051300990119     C                   move      xsccut        AMPUTE                         *
051400000000     C*--------- GIRA DATA INIZIO GESTIONE                 *
051500990119     C                   move      woggig        ampdig                         *
051600000000     C*--------- GIRA DATA FINE GESTIONE                   *
051700990119     C     VIDDFG        IFNE      0                                            *
051800990119     c     *eur          test(d)                 viddfg                 36
051900990119     c                   if        *in36
052000990119     c     *dmy          test(d)                 viddfg                 36
052100990119     c   36              goto      sopra1
052200990119     c     *dmy          move      viddfg        dataeur
052300990119     c                   move      dataeur       dataiso
052400990119     c                   move      dataiso       ampdfg
052500990119     c                   else
052600990119     c                   move      viddfg        dataeur
052700990119     c                   move      dataeur       dataiso
052800990119     c                   move      dataiso       ampdfg
052900990119     c                   endif
053000990119     c                   else
053100990119     c                   z-add     0             ampdfg
053200990119     C                   END                                                    *
053300990119     C*--------- GIRA DATA ultima uscita                   *
053400990119     C     VIDtar        IFNE      0                                            *
053500990119     c     *eur          test(d)                 vidtar                 36
053600990119     c                   if        *in36
053700990119     c     *dmy          test(d)                 vidtar                 36
053800990119     c   36              goto      sopra1
053900990119     c     *dmy          move      vidtar        dataeur
054000990119     c                   move      dataeur       dataiso
054100990119     c                   move      dataiso       amptar
054200990119     c                   else
054300990119     c                   move      vidtar        dataeur
054400990119     c                   move      dataeur       dataiso
054500990119     c                   move      dataiso       amptar
054600990119     c                   endif
054700990119     c                   else
054800990119     c                   z-add     0             vidtar
054900990119     C                   END                                                    *
055000000000     C                   MOVEL     VIDDMT        AMPDMT                         *
055100000000     C                   MOVEL     VIDCLM        AMPCLM                         *
055200000000     C                   Z-ADD     VIDLRO        AMPLRO                         *
055300000000     C                   MOVEL     VIDSCM        AMPSCM                         *
055400000000     C                   Z-ADD     VIDLVM        AMPLVM                         *
055500000000     C                   MOVEL     VIDUMA        AMPUMA                         *
055600000000     C                   Z-ADD     VIDLEC        AMPLEC                         *
055700000000     C                   MOVEL     VIDUMS        AMPUMS                         *
055800000000     C                   Z-ADD     VIDFCS        AMPFCS                         *
055900000000     C                   MOVEL     VIDUMQ        AMPUMQ                         *
056000000000     C                   Z-ADD     VIDFCQ        AMPFCQ                         *
056100000000     C                   MOVEL     VIDUMV        AMPUMV                         *
056200000000     C                   Z-ADD     VIDFCV        AMPFCV                         *
056300000000     C                   MOVEL     VIDCTF        AMPCTF                         *
056400000000     C                   Z-ADD     VIDGAP        AMPGAP                         *
056500000000     C                   MOVEL     VIDLIS        AMPLIS                         *
056600000000     C                   MOVEL     VIDAMT        AMPAMT                         *
056700000000     C                   Z-ADD     VIDPDC        AMPPDC                         *
056800000000     C                   Z-ADD     VIDTRP        AMPTRP                         *
056900000000     C                   Z-ADD     VIDCCA        AMPCCA                         *
057000000000     C                   Z-ADD     VIDPNT        AMPPNT                         *
057100000000     C                   Z-ADD     VIDAIV        AMPAIV                         *
057200990119     C                   move      VIDesV        AMPesV                         *
057300000000     C                   MOVEL     VIDIMB        AMPIMB                         *
057400000000     C                   MOVEL     VIDMAG        AMPMAG                         *
057500000000     C                   MOVEL     VIDUBI        AMPUBI                         *
057600000000     C                   Z-ADD     VIDAIA        AMPAIA                         *
057700990119     C                   move      VIDesA        AMPesA                         *
057800000000     C  N10
057900000000     CAN 02                                                                     *
058000990119     COR 10              WRITE     tmamp000                             82      *
058100000000     C  N10
058200990119     CANN02              UPDATE    tmamp000                                     *
058300000000     C                   SETOFF                                       1011      *
058400000000     CSR                 ENDSR                                                  *
058500990119     C*----------------------------------------------------*
058600990119     C* Reperimento dati società
058700990119     C*----------------------------------------------------*
058800990119     C     REPSOC        BEGSR
058900990119     C*
059000990119     C                   CALLB     'XSOC'
059100990119     C                   PARM                    TIPXSC            6
059200990119     C                   PARM                    SOCXSC            3
059300990119     C                   PARM                    CDSXSC            9 0
059400990119     C                   PARM                    MODXSC            3
059500990119     C                   PARM      *blanks       RTNXSC            1
059600990119     C                   PARM                    XSOCDS
059700990119     C                   PARM                    KPJBA
059800990119     C*
059900990119     C                   ENDSR
060000000000** ERR SCHIERA ERRORI
060100000000*- Versione già  Esistente  -*
