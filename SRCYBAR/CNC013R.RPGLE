000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F* CNC013R * -----------------------------------------*
000300000000     F*          MANUTENZIONE TABELLE GENERICHE            *
000400000000     F*          ------------------------------   AZ. C013 *
000500000000     F*          FILE : TABEL00F-CNC013D                   *
000600000000     F*          FMT. : M002001 M0020CF1 M0020SC1          *
000700000000     F*                 M002002 M0020CF2 M0020SC2          *
000800930820     F*   DAGR = Modifiche apportate da Daniele Greggio Pro*
000900930820     F*       Milano in data 03/03/92 x passaggio nome pgm *
001000930820     F*       pgm al video con DS.                         *
001100000000     F*----------------------------------------------------*
001200000000     FTABEL00F  UF A E           K DISK
001300000000     FCNC013D   CF   E             WORKSTN
001400000000     F                                     SFILE(M0020SF1:RN1)
001500000000     F                                     SFILE(M0020SF2:RN2)
001600000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001700000000     D KPJBA         E DS
001800000000     D  EDP                    1      3
001900971209     D  SIF                   98    100
002000881025     D DSTAB           DS
002100881025     D  S2DES                  1     60
002200881025     D  S2DES1                61     89
002300971210     D DS            E DS
002400971209     D SING            DS
002500971209     D  V1DES                  1     30
002600971209     D  V1CGRC                69     69
002700930820DAGR D                SDS
002800930820DAGR D  NOMPG                  1      8
002900930820DAGR D                 DS
003000930820DAGR D  NOMPGM                 1     11
003100930820DAGR D  AST1                   1      1    INZ('*')
003200930820DAGR D  NOMPG1                 3     10
003300930820DAGR D  AST2                  11     11    INZ('*')
003400000000     C/EJECT
003500000000     C*----------------------------------------------------*
003600000000      * INDICATORI
003700000000      *
003800000000      * 14 COSTANTE SECONDA COLONNA                                  C1
003900000000      * 21
004000000000      * 61 ROLLUP                                                    S2
004100000000      * 63 ERASEINP                                                  C1
004200000000      *
004300000000      * 91
004400000000      * 92
004500000000      * 93
004600000000     C*----------------------------------------------------*
004700000000      * CHIAVI
004800000000     C*---------                                           *
004900000000     C     KTAB          KLIST                                                  *
005000000000     C                   KFLD                    CODUT
005100000000     C                   KFLD                    TBLCOD                         *
005200000000     C                   KFLD                    TBLKEY                         *
005300000000     C     KTAB1         KLIST                                                  *
005400000000     C                   KFLD                    CODUT
005500000000     C                   KFLD                    TBLCOD                         *
005600000000     C*----------------------------------------------------*
005700000000     C     *ENTRY        PLIST                                                  *
005800000000     C                   PARM                    KPJBA                          *
005900890429     C                   Z-ADD     1             CODUT
006000000000     C                   CALL      'XPARUT'
006100000000     C                   PARM                    PARUT
006200000000     C                   Z-ADD     CODUT         TBLKUT
006300000000     C                   MOVEL     RAGUT         RSUT             20
006400930820DAGR C                   MOVEL     NOMPG         NOMPG1
006500980302     C                   MOVEL     KNMUS         §§PRF             3
006600000000     C*-------- CLEAR SUBFILE INDICE TABELLA --------------*
006700881025?    C     INIZIO        TAG                                                    *
006800000000     C                   SETON                                        70        *
006900000000     C                   WRITE     M0020SC1                                     *
007000000000     C                   SETOFF                                       1470      *
007100000000     C                   SETON                                        71
007200000000     C*--------                                            *
007300920518     C                   Z-ADD     0             RN1               4 0          *
007400920518     C                   Z-ADD     1             CURSOR            4 0          *
007500000000     C*
007600971210  AA C                   MOVEL     *BLANK        DS
007700880606     C                   MOVE      *BLANKS       TBLCOD
007800880606     C     KTAB1         CHAIN     TABEL                              99        *
007900000000      *
008000000000      * FILE NUOVO -- GENERA RECORD INDICE
008100000000      *
008200880606     C   99              DO
008300000000     C                   MOVE      *ZERO         TBLKEY
008400880606     C                   MOVE      '  '          TBLKEY
008500000000     C                   MOVEL     'INDICE T'    SV1DES           14
008600000000     C                   MOVE      'ABELLE'      SV1DES
008700000000     C                   MOVEL     SV1DES        TBLUNI
008800900413     C                   MOVE      S2DFTT        TBLFTT
008900900413     C                   MOVE      S2DFLT        TBLFLT
009000900413     C                   MOVE      ' '           TBLFTR                          DA TRASM.
009100900413     C                   MOVE      *ZERO         TBLDTR                          N.TRASM.
009200000000     C                   WRITE     TABEL
009300880606     C                   END
009400000000     C*
009500000000     C     KTAB1         SETLL     TABEL                              99        *
009600881025     C*
009700881025     C*-------- CARICA SUBFILE INDICE TABELLA -------------*
009800881025     C*
009900980302     C                   MOVEL     *BLANKS       WFORZA
010000980302     C                   MOVEL     *BLANKS       WCDTAB
010100971210     C                   MOVEL     'GAITRA20'    WSIFGR           10
010200971210     C                   MOVE      '1 '          WSIFGR
010300971210     C*
010400971210     C     LAB100        TAG                                                    *
010500000000     C     KTAB1         READE     TABEL                                  99
010600000000     C*
010700000000     C  N99              DO
010800000000     C*
010900000000     C                   ADD       1             RN1                            *
011000000000     C                   MOVE      TBLKEY        S1COD                          *
011100971210     C                   MOVEL     TBLUNI        DS                             *
011200971210     C                   MOVEL     §DES1         S1DES
011300060412     c                   clear                   s1fglin
011400060412     c                   if        §fglin <> *blanks
011500060412     c                   eval      s1fglin = 'L'
011600060412     c                   endif
011700971210     C                   MOVE      TBLUNI        S1DES1
011800971210     C                   MOVE      §PGM          PROG
011900971210     C                   MOVE      §TIP          V1CGRU
012000971210     C* SE INDICE GENERALE MEMORIZZO SIF MANUTENZIONE
012100971210     C     TBLKEY        IFEQ      '000000  '
012200980126     C                   MOVEL     §SIFGB        WSIFGR           10
012300980126     C                   MOVEL     §SIFGB        WSIFBA           10
012400980126     C                   MOVEL     §SIFGB        V1CSMG
012500980126     C                   MOVEL     §SIFGB        V1CSMB
012600980126     C                   END
012700000000     C                   WRITE     M0020SF1                                     *
012800000000     C                   GOTO      LAB100                                       *
012900000000     C                   END                                                    *
013000881028     C*
013100971210  AA C                   MOVEL     *BLANK        DS
013200881028     C*
013300000000     C*
013400881025?    C     T1TAG         TAG                                                    *
013500881025     C*
013600000000     C     RN1           COMP      14                                 14  14    *
013700000000     C*-------- EMISSIONE TESTATA INDICE ------------------*
013800000000     C                   WRITE     M002001                                      *
013900000000     C*-------- EMISSIONE SUBFILE INDICE ------------------*
014000881025?    C     C1TAG         TAG                                                    *
014100881025     C*
014200881025     C     RN1           IFEQ      0
014300000000     C                   SETOFF                                       71
014400000000     C                   END
014500881025     C*
014600930923     C     SAVPOS        IFNE      0
014700930923     C                   Z-ADD     SAVPOS        CURSOR
014800930923     C                   Z-ADD     0             SAVPOS
014900930923     C                   END
015000930923     C*
015100000000     C                   EXFMT     M0020SC1                                     *
015200000000     C                   SETON                                        71        *
015300000000     C                   MOVEA     *ZEROS        *IN(90)                        *
015400950915     C                   SETOFF                                       632840
015500930820     C   KC              GOTO      FINE                                         *
015600000000     C*-------- RICHIESTA STAMPA TABELLE :                 *
015700930820     C   KH              CALL      'CNC006R'                                    *
015800000000     C                   PARM                    KPJBA                          *
015900000000     C   KH              GOTO      C1TAG                                        *
016000000000     C/SPACE 3
016100000000     C*-------- CONTROLLO CREAZIONE NUOVA TABELLA ---------*
016200930820     C   KF              DO                                                     *
016300880606     C     V1COD         IFEQ      *BLANK
016400000000     C                   SETON                                        9092
016500000000     C                   GOTO      C1TAG
016600000000     C                   END
016700000000     C     V1DES         IFEQ      *BLANKS
016800000000     C                   SETON                                        9093
016900000000     C                   GOTO      C1TAG
017000000000     C                   END
017100971210     C* CONTROLLO MODIFICABILITA'
017200050511     C* NON LO FACCIO IN PROVA!!!
017300050511     C                   MOVE      KNSIF         W004A             4
017400050511     C     W004A         IFNE      'PFM'
017500050511     C     W004A         ANDNE     'PFP'
017600050511     C     W004A         ANDNE     'PSM'
017700050511     C     W004A         ANDNE     'PSP'
017800971210     C     V1CGRC        IFEQ      'B'
017900971210     C     KNSIF         ANDNE     WSIFBA
018000971210     C     V1CGRC        OREQ      'G'
018100971210     C     KNSIF         ANDNE     WSIFGR
018200971209     C                   SETON                                        9094
018300971209     C                   GOTO      C1TAG
018400971209     C                   END
018500050511     C                   END
018600971210     C*
018700880606     C                   MOVE      *BLANKS       TBLCOD                         *
018800000000     C                   MOVE      *ZEROS        TBLKEY
018900000000     C                   MOVE      V1COD         TBLKEY                         *
019000000000     C     KTAB          CHAIN     TABEL00F                           21        *
019100000000     C  N21              SETON                                        9091      *
019200000000     C  N21              GOTO      C1TAG
019300880606     C                   MOVE      *BLANKS       TBLCOD                         *
019400000000     C                   MOVE      *ZEROS        TBLKEY                         *
019500000000     C                   MOVE      V1COD         TBLKEY                         *
019600881028     C                   MOVEL     *BLANK        TBLUNI
019700971209     C                   MOVEL     SING          TBLUNI                         *
019800900413     C                   MOVE      S2DFTT        TBLFTT
019900900413     C                   MOVE      S2DFLT        TBLFLT
020000900413     C                   MOVE      ' '           TBLFTR                          DA TRASM.
020100900413     C                   MOVE      *ZERO         TBLDTR                          N.TRASM.
020200000000     C                   WRITE     TABEL                                        *
020300000000     C                   MOVE      *BLANKS       V1COD                          *
020400000000     C                   MOVE      *BLANKS       V1DES                          *
020500000000     C                   SETON                                        63
020600000000      *
020700000000     C                   GOTO      INIZIO                                       *
020800000000     C                   END                                                    *
020900950915     C*  Controllo se richiesto il posizionamento
021000950915     C     V1COD         IFNE      *BLANKS
021100950915     C     V1COD         ANDNE     SAVCOD
021200950915     C                   MOVEL     V1COD         TBLCOD
021300951205     C     KTAB1         CHAIN(N)  TABEL00F                           40
021400950915     C     *IN40         IFEQ      '1'
021500950915     C                   GOTO      C1TAG                                        *
021600950915     C                   END
021700950915     C*  Eseguo lettura subfile fino a posizionamento
021800950915     C                   Z-ADD     1             RN1
021900950915     C     RN1           CHAIN     M0020SF1                           30
022000950915     C     *IN30         DOWEQ     '0'
022100950915     C     S1COD         ANDNE     TBLCOD
022200950915     C                   ADD       1             RN1
022300950915     C     RN1           CHAIN     M0020SF1                           30
022400950915     C                   END
022500950915     C*
022600950915     C     S1COD         IFEQ      TBLCOD
022700950915     C                   Z-ADD     RN1           CURSOR
022800950915     C                   END
022900950915     C                   MOVEL     S1COD         SAVCOD            2
023000950915     C                   GOTO      C1TAG                                        *
023100950915     C                   END
023200000000     C/EJECT
023300000000     C     RN1           CABEQ     0             C1TAG
023400000000      *
023500000000     C*-------- SCELTA TABELLA DA AGGIORNARE --------------*
023600000000     C*
023700980302     C     RILEG         TAG
023800000000     C                   READC     M0020SF1                               21    *
023900000000     C*
024000971209     C                   SETOFF                                       95
024100000000     C     *IN21         CABEQ     '1'           C1TAG                7171
024200971209     C     S1SEL         IFEQ      *BLANKS
024300971209     C                   SETOFF                                       95
024400971209     C                   SETON                                        22
024500971209 AA  C                   UPDATE    M0020SF1
024600971209     C                   SETOFF                                       22
024700980302     C                   GOTO      RILEG
024800971209     C                   END
024900060412
025000060412      * se tabella gestita in lingua DEVO usare per forza il programma quindi errore se
025100060412      * richiesta manutenzione con '1'
025200060412     c                   if        s1fglin <> *blanks and s1sel = '1'
025300060412     c                   z-add     rn1           savpos
025400060412     c                   eval      *in22 = *on
025500060412     c                   eval      *in90 = *on
025600060412     c                   eval      *in95 = *on
025700060412     c                   eval      *in97 = *on
025800060412     c                   update    m0020sf1
025900060412     c                   eval      *in22 = *off
026000060412     c                   goto      c1tag
026100060412     c                   endif
026200060412     c                   if        s1fglin <> *blanks and s1sel = 'P' and
026300060412     c                             prog = *blanks
026400060412     c                   z-add     rn1           savpos
026500060412     c                   eval      *in22 = *on
026600060412     c                   eval      *in90 = *on
026700060412     c                   eval      *in95 = *on
026800060412     c                   eval      *in98 = *on
026900060412     c                   update    m0020sf1
027000060412     c                   eval      *in22 = *off
027100060412     c                   goto      c1tag
027200060412     c                   endif
027300060412
027400971209     C* NON POSSO MANUTENZIONARE TABELLA BARTOLINI O DI GRUPPO
027500971209     C* SE NON SONO IN GAITRA201
027600050511     c* In prova MAI Errore, in filiale solo se non gaitra201 e filtra201
027700050511     c                   move      knsif         w004a
027800050511     c                   if        w004A<>'201 '  and  W004A<>'PFM ' AND
027900050511     C                             W004A<>'PFP' AND W004A<>'PSP ' AND
028000050511     C                             W004A<>'PSM '
028100971210     C     V1CGRU        IFEQ      'B'
028200971210     C     V1CSMB        ANDNE     *BLANKS
028300971210     C     V1CSMB        ANDNE     KNSIF
028400971210     C     V1CGRU        OREQ      'B'
028500971210     C     V1CSMB        ANDEQ     *BLANKS
028600971210     C     KNSIF         ANDNE     WSIFBA
028700971210     C     V1CGRU        OREQ      'G'
028800971210     C     V1CSMG        ANDNE     *BLANKS
028900971210     C     V1CSMG        ANDNE     KNSIF
029000971210     C     V1CGRU        OREQ      'G'
029100971210     C     V1CSMG        ANDEQ     *BLANKS
029200971210     C     KNSIF         ANDNE     WSIFGR
029300980302     C*  Se tipo profilo EDP permetto di forzare l'aggiornamento
029400980302     C*  delle tabelle
029500980302     C     §§PRF         IFEQ      'EDP'
029600980302     C     WCDTAB        IFEQ      S1COD
029700980302     C     WFORZA        IFNE      'S'
029800971210     C                   Z-ADD     RN1           SAVPOS            4 0
029900971209     C                   SETON                                        909596
030000971209     C                   SETON                                        22
030100971209 AA  C                   UPDATE    M0020SF1
030200971209     C                   SETOFF                                       22
030300980303     C                   MOVEL     'S'           WFORZA
030400971209     C                   GOTO      C1TAG
030500971209     C                   END
030600980302     C* Se modificato codice imposto nuovo codice e imposto
030700980302     C* WFORZA a 'S'
030800980302     C                   ELSE
030900980302     C                   MOVEL     S1COD         WCDTAB            2
031000980302     C                   MOVEL     'S'           WFORZA            1
031100980302     C                   Z-ADD     RN1           SAVPOS            4 0
031200980302     C                   SETON                                        909596
031300980302     C                   SETON                                        22
031400980302 AA  C                   UPDATE    M0020SF1
031500980302     C                   SETOFF                                       22
031600980302     C                   GOTO      C1TAG
031700980302     C                   END
031800980302     C* Se utenten no EDP blocco comunque
031900980302     C                   ELSE
032000980302     C                   Z-ADD     RN1           SAVPOS            4 0
032100980302     C                   SETON                                        909596
032200980302     C                   SETON                                        22
032300980302 AA  C                   UPDATE    M0020SF1
032400980302     C                   SETOFF                                       22
032500980302     C                   GOTO      C1TAG
032600980302     C                   END
032700980302     C                   END
032800050511     C                   END
032900000000     C*
033000971209     C                   Z-ADD     RN1           SAVPOS            4 0
033100881025 AA  C     S1SEL         IFEQ      'P'
033200091120     c                   clear                   kpjbu
033300891201     C*§ SE RICHIAMATO IL PGM DESCRIZ. GENERICO PASSO IN KPJBU IL
033400891201     C*§ CODICE TABELLA
033500891201     C     PROG          IFEQ      'TRTB16R '
033600920526     C     PROG          OREQ      'TRTB36R '
033700060412     C**!!!PROG          OREQ      'TRTB25R '
033800971124     C     PROG          OREQ      'TRTB01R '
033900891201     C                   MOVEL     S1COD         KPJBU
034000891201     C                   END
034100891201     C*§
034200881025 AA  C                   CALL      PROG                                 21
034300881025 AA  C                   PARM                    KPJBA                          *
034400060427      * se gestione tabella in lingua ed errore nel pgm di gestione non devo
034500060427      * emettere il subfile generico
034600060427     c                   if        *in21 and s1fglin <> *blanks
034700060427     c                   setoff                                       21
034800060427     c                   endif
034900891201     C*§
035000891201     C     PROG          IFEQ      'TRTB16R '
035100920526     C     PROG          OREQ      'TRTB36R '
035200060412     C**!!!PROG          OREQ      'TRTB25R '
035300971124     C     PROG          OREQ      'TRTB01R '
035400891201     C                   MOVEL     '  '          KPJBU
035500891201     C                   END
035600891201     C*§
035700881025 AA  C  N21              MOVE      *BLANK        S1SEL
035800881025 AA  C  N21              UPDATE    M0020SF1
035900881025 AA  C  N21              GOTO      C1TAG
036000881025 AA  C                   END
036100881025     C*
036200000000     C                   MOVE      *BLANK        S1SEL
036300000000     C                   UPDATE    M0020SF1
036400000000     C*-------- EMISSIONE REC.TESTATA TABELLA -------------*
036500000000     C                   MOVE      S1COD         V2COD                          *
036600000000     C                   MOVEL     S1DES         V2DES                          *
036700000000     C                   WRITE     M002002                                      *
036800881025     C*
036900881025?    C     CARS2         TAG                                                    *
037000000000     C*-------- PULIZIA RK CONTROLLO SUB. ELEM. TABELLA ---*
037100000000     C                   SETON                                        70        *
037200000000     C                   WRITE     M0020SC2                                     *
037300000000     C                   SETOFF                                       70        *
037400000000     C                   SETON                                        71        *
037500000000      *
037600000000     C*-------- CARICAMENTO SUBFILE ELEMENTI TABELLA ------*
037700920518     C                   Z-ADD     0             RN2               4 0          *
037800000000     C                   MOVE      S1COD         TBLCOD                         *
037900000000     C     KTAB1         SETLL     TABEL00F                           21        *
038000000000     C*
038100000000     C*----------------------------------------------------*
038200000000      /EJECT
038300000000     C*-------- LETTURA FILE E CARIC.SUBFILE ELEM.TAB. ----*
038400000000     C     KTAB1         READE     TABEL00F                               21    *
038500000000     C     *IN21         DOWNE     '1'                                          *
038600881025     C                   MOVEL     TBLUNI        DSTAB                          *
038700000000     C                   MOVE      TBLKEY        S2KEY                          *
038800000000     C                   MOVE      TBLKEY        H2KEY                          *
038900900517     C     TBLFLG        IFEQ      '*'                                          *CANC.LOGI
039000900517     C                   MOVE      'A'           S2ANN
039100900517     C                   ELSE
039200900517     C                   MOVE      *BLANK        S2ANN
039300900517     C                   END                                                    *
039400900413     C                   MOVE      TBLFTT        S2DFTT                         *
039500900413     C                   MOVE      TBLFLT        S2DFLT                         *
039600900413     C                   MOVE      TBLFTR        S2DFTR                         *
039700900413     C                   MOVE      TBLDTR        S2DDTR                         *
039800000000     C                   ADD       1             RN2
039900000000     C                   WRITE     M0020SF2                                     *
040000000000     C*
040100000000     C     KTAB1         READE     TABEL                                  21    *
040200000000     C  N21              END                                                    *
040300000000     C/SPACE 3
040400000000      * RIEMPIO LA PAGINA
040500000000     C                   EXSR      ROLLUP
040600930820RD   C     RN2           IFNE      0
040700930820RD   C                   Z-ADD     1             CURS2
040800930820RD   C                   END
040900000000     C*-------- EMISSIONE SUBFILE ELEMENTI TABELLA --------*
041000000000      *
041100881025?    C     C2TAG         TAG                                                    *
041200000000     C     RN2           IFEQ      0
041300000000     C                   SETOFF                                           71
041400000000     C                   END
041500000000      *
041600000000     C                   EXFMT     M0020SC2                                     *
041700000000     C*--------                                            *
041800930820     C   KC              GOTO      INIZIO                                       *
041900930820     C   KQ              GOTO      CARS2                                        *
042000000000      *
042100000000     C   61              EXSR      ROLLUP
042200000000     C   61              GOTO      C2TAG
042300000000     C*----------------------------------------------------*
042400000000      /EJECT
042500000000     C*-------- LEGGO RECORD VARIATI ----------------------*
042600000000     C*
042700000000     C                   READC     M0020SF2                               74    *
042800000000     C   74              SETOFF                                           71
042900000000     C   74              GOTO      C2TAG
043000000000      *
043100000000     C     *IN74         DOWNE     '1'
043200000000     C*-------- AGGIORNAMENTO :                            *
043300000000     C     H2KEY         IFNE      *ZERO
043400000000     C                   MOVE      H2KEY         TBLKEY                         *
043500000000     C                   MOVE      H2KEY         S2KEY
043600000000      *
043700000000      *
043800000000     C     KTAB          CHAIN     TABEL00F                           21        *
043900881025     C                   MOVEL     DSTAB         TBLUNI                         *
044000900413     C                   MOVE      S2DFTT        TBLFTT
044100900413     C                   MOVE      S2DFLT        TBLFLT
044200900413     C     *IN(21)       IFEQ      '0'
044300900413     C     S2ANN         IFEQ      'A'                                          *CANC.LOGI
044400900413     C                   MOVE      '*'           TBLFLG
044500900517     C                   ELSE
044600900517     C                   MOVE      ' '           TBLFLG
044700900413     C                   END                                                    *UPDATE
044800900413     C                   MOVE      ' '           TBLFTR                          DA TRASM.
044900900413     C                   MOVE      *ZERO         TBLDTR                          N.TRASM.
045000000000     C                   UPDATE    TABEL
045100000000     C                   END                                                    *
045200000000     C*
045300000000     C                   ELSE
045400000000     C*-------- SCRIVO    NUOVO RECORD --------------------*
045500000000     C     S2KEY         IFNE      *ZERO
045600000000     C                   MOVE      S2KEY         TBLKEY                         *
045700000000     C     KTAB          CHAIN     TABEL00F                           21        *
045800000000     C   21              DO
045900000000     C                   MOVE      S2KEY         TBLKEY
046000881025     C                   MOVEL     DSTAB         TBLUNI                         *
046100900413     C                   MOVE      S2DFTT        TBLFTT
046200900413     C                   MOVE      S2DFLT        TBLFLT
046300900413     C                   MOVE      ' '           TBLFTR                          DA TRASM.
046400900413     C                   MOVE      *ZERO         TBLDTR                          N.TRASM.
046500000000     C                   WRITE     TABEL
046600000000     C                   MOVE      S2KEY         H2KEY
046700000000     C                   END
046800000000      *
046900000000     C                   END
047000000000     C                   END
047100000000     C*
047200000000     C                   UPDATE    M0020SF2
047300000000     C*
047400000000     C                   READC     M0020SF2                               74    *
047500000000     C  N74              END
047600000000     C                   Z-ADD     RN2           CURS2
047700000000      *
047800000000     C                   GOTO      C2TAG
047900000000     C*-------- FINE LAVORO      --------------------------*
048000881025?    C     FINE          TAG                                                    *
048100000000     C                   SETON                                        LR        *
048200000000     C*----------------------------------------------------*
048300000000      /EJECT
048400000000     C*-------- -------------------------------------------*
048500000000     C     ROLLUP        BEGSR                                                  *
048600920518     C     RN2           DIV       7             SV1RN2            4 0
048700920518     C                   MVR                     SV2RN2            4 0
048800000000     C     SV2RN2        DO        6                                            *
048900000000     C                   MOVE      *ZEROS        S2KEY                          *
049000000000     C                   MOVE      *ZEROS        H2KEY                          *
049100881025     C                   MOVE      *BLANKS       DSTAB                          *
049200000000     C                   MOVE      *BLANKS       S2ANN                          *
049300900413     C                   MOVE      *BLANKS       S2DFTT                         *
049400900413     C                   MOVE      *ZEROS        S2DFLT                         *
049500900413     C                   MOVE      *BLANKS       S2DFTR                         *
049600900413     C                   MOVE      *ZEROS        S2DDTR                         *
049700000000     C                   ADD       1             RN2
049800000000     C                   WRITE     M0020SF2                                     *
049900000000     C                   END                                                    *
050000000000     C     RN2           SUB       7             CURS2
050100000000     C     CURS2         IFLT      1
050200000000     C                   Z-ADD     1             CURS2
050300000000     C                   END
050400000000     C                   ENDSR
