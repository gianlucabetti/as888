000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200911216      * TRTB33R *-----------------------------------------------------*
000300911216      *                   - GESTIONE TABELLA 7L -                     *
000400911216      *---------------------------------------------------------------*
000500911216     FTRTB33D   CF   E             WORKSTN
000600891030     FTABEL00F  UF A E           K DISK
000700911216      *
000800911216     D DS7L          E DS
000900061027     D DS7Lcampo     E DS
001000131127     D DTCV          E DS
001100900608      *
001200891030     D KPJBA         E DS
001300911118      *
001400911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
001500131127      *
001600131127     d Tibs02ds      e ds
001700911216      *****************************************************************
001800911216      *  RIEPILOGO INDICATORI
001900911216      *****************************************************************
002000911216      * 01    - INSERIMENTO
002100911216      * 02    - VARIAZIONE
002200920116      * 03    - ANNULLATO
002300911216      * 79    - COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP
002400911216      * 80/98 - DI COMODO,RIUTILIZZABILI
002500911216      * 99    - ERRORE GENERICO
002600911216      *****************************************************************
002700131127      *------------------------------------------------------------------------*
002800131127      * pgm richiamati
002900131127      *------------------------------------------------------------------------*
003000131127      /copy gaitrasrc/srcprotopr,tibs02r
003100131127
003200891030     C     *ENTRY        PLIST
003300891030     C                   PARM                    KPJBA
003400891030     C                   Z-ADD     1             CODUT
003500891030     C                   CALL      'XPARUT'
003600891030     C                   PARM                    UTEDSE
003700941013     C                   MOVEL     RAGUT         VIDDSA
003800911216      *---------------------------------------------------------------*
003900911216      * CHIAVE X POSIZIONAMENTO TABELLE
004000911216      *
004100911216     C     KTAB          KLIST
004200911216     C                   KFLD                    §KUT
004300911216     C                   KFLD                    §COD
004400911216     C                   KFLD                    §KEY
004500911216      *
004600911216      *---------------------------------------------------------------*
004700891030     C     *LIKE         DEFINE    TBLKUT        §KUT
004800891030     C     *LIKE         DEFINE    TBLCOD        §COD
004900891030     C     *LIKE         DEFINE    TBLKEY        §KEY
005000060310     C     *LIKE         DEFINE    §7lmtv        s_§7lmtv
005100941013     C*
005200891102     C     STAR          TAG
005300891102     C                   SETOFF                                       79
005400891102     C                   MOVEL     KNMUS         PROF              3
005500941013     C*
005600891102     C     PROF          IFNE      'EDP'
005700891102     C                   SETON                                        79
005800891102     C                   END
005900911216      *§
006000911115     C                   MOVEL     *BLANKS       §DES             20
006100891107     C                   MOVEL     *BLANKS       VCOD
006200891107     C                   MOVEL     '?'           VCOD
006300941013     C*
006400891030     C     INIZIO        TAG
006500941013     C                   EXFMT     TB33D01
006600941013     C* F3 - FINE
006700911115     C   KC              GOTO      FINE
006800941013     C*
006900891107     C     VCOD          IFEQ      *BLANKS
007000911216     C                   SETON                                        2999
007100891030     C                   GOTO      INIZIO
007200891030     C                   END
007300941013     C*
007400911216      **--- RICERCA IN TABEL00F
007500941013     C     '?'           SCAN      VCOD                                   90
007600941013     C     *IN90         IFEQ      *ON
007700891030     C                   MOVEL     CODUT         §KUT
007800911216     C                   MOVEL     '7L'          §COD
007900891107     C                   MOVE      *ZEROS        VCOD
008000000510     C                   CLEAR                   §KEY
008100891030     C                   CALL      'X§TABER'
008200891030     C                   PARM                    §KUT
008300891030     C                   PARM                    §COD
008400891030     C                   PARM                    §KEY
008500911115     C                   PARM                    §DES
008600891107     C                   MOVEL     §KEY          VCOD
008700891030     C                   GOTO      INIZIO
008800911216      *§
008900891102     C                   ELSE
009000891102     C                   MOVEL     CODUT         §KUT
009100911216     C                   MOVEL     '7L'          §COD
009200891107     C                   MOVEL     VCOD          §KEY
009300891030     C                   END
009400911216      *§
009500920116     C                   SETOFF                                       010203
009600941013     C* DS DELLA TABELLA
009700941013     C                   CLEAR                   DS7L
009800941013     C*
009900900604     C     KTAB          CHAIN     TABEL                              80
010000941013     C*
010100891030     C     *IN80         IFEQ      '1'
010200891030     C                   SETON                                        01
010300891030     C                   ELSE
010400911216     C                   MOVEL     TBLUNI        DS7L
010500891030     C                   SETON                                        02
010600891030     C                   END
010700060310     c                   eval      s_§7lmtv = §7lmtv
010800061027     c* Imposto i sottocampi della consegna
010900061027     c                   movel     §7lvdco       ds7lcampo
011000061027     c                   movel     §7lvdopo      vidvdco
011100061027     c                   movel     §7lines       vidcoie
011200061027     c                   movel     §7lfras       vidcofa
011300061027     c                   movel     §7lcodif      vidcocod
011400061027     c*
011500061027     c* Imposto i sottocampi della consegna parziale
011600061027     c                   movel     §7lvdcop      ds7lcampo
011700061027     c                   movel     §7lvdopo      vidvdcop
011800061027     c                   movel     §7lines       vidcpie
011900061027     c                   movel     §7lfras       vidcpfa
012000061027     c                   movel     §7lcodif      vidcpcod
012100920116     C*
012200920116     C   02TBLFLG        IFEQ      '*'
012300941013     C                   SETON                                        03
012400920116     C                   END
012500041028     c* Se il campo 7LFFI  non e' S e non ' ':
012600041028     c* se "P" imposto " " -  se "E" imposto "S"
012700041028     c                   if        §7lffi='P'
012800041028     c                   eval      §7lffi=' '
012900041028     c                   endif
013000041028     c                   if        §7lffi='E'
013100041028     c                   eval      §7lffi='S'
013200041028     c                   endif
013300131127      /free
013400131127       clear VIDdpda ;
013500131127       Vidpda = §7Lpda ;
013600131127       If  §7Lpda <> ' ' ;
013700131127          clear TIBS02DS;
013800131127          T02mod = 'C';
013900131127          T02cod = 'TCV';
014000131127          T02ke1 = §7Lpda;
014100131127          TNTBE_RicercaControllo (kpjba : tibs02ds);
014200131127          IF  T02err = *blanks;
014300131127            DTCV = t02uni ;
014400131127            VIDdpda = §TCVdes;
014500131127          ENDIF;
014600131127        ENDIF;
014700131127      /end-free
014800911216      *§
014900891030     C     SU            TAG
015000891030     C                   SETOFF                                       99
015100941013     C*
015200941013     C                   EXFMT     TB33D02
015300941013     C* F3  - FINE LAVORO
015400911115     C   KC              GOTO      FINE
015500941013     C* F12 - RITRONO
015600911213     C   KL              GOTO      INIZIO
015700941013      * F16 -  ANNULLAMENTO
015800911115     C   KQ              DO
015900911115     C     KTAB          CHAIN     TABEL                              98
016000911115     C  N98              MOVEL     '*'           TBLFLG
016100911115     C  N98              UPDATE    TABEL
016200891030     C                   GOTO      INIZIO
016300891030     C                   END
016400941013      * F5 - RIPRISTINO
016500920116     C   KE              DO
016600920116     C     KTAB          CHAIN     TABEL                              98
016700920116     C  N98              MOVEL     ' '           TBLFLG
016800920116     C  N98              UPDATE    TABEL
016900920116     C                   GOTO      INIZIO
017000920116     C                   END
017100911216      *§
017200891102     C                   EXSR      CTRFMT
017300061027     C   99              GOTO      SU
017400061027     c*********
017500061027     c* 3 videata
017600061027     c*********
017700061027     C     for03         TAG
017800061027     C                   SETOFF                                       99
017900061027     C*
018000061027     C                   EXFMT     TB33D03
018100061027     C* F3  - FINE LAVORO
018200061027     C   KC              GOTO      FINE
018300061027     c
018400061027     C* F12 - RItorno a seconda videata
018500061027     C   KL              GOTO      su
018600941013     C*
018700061027     c* controllo dati videata
018800061027     c                   exsr      CTRd03
018900061027     c*
019000061027     c   99
019100061027     corn06              goto      for03
019200061027     c*
019300061027     c* f06 - Aggiornamento
019400060310     c                   exsr      ges_wdw
019500891030     C                   EXSR      SCRIVI
019600941013     C*
019700891102     C                   GOTO      STAR
019800911216      *§
019900891030     C     FINE          TAG
020000891030     C                   SETON                                        LR
020100911216      **------------------------
020200911216      ** CONTROLLI 2°VIDEATA
020300911216      **------------------------
020400891030     C     CTRFMT        BEGSR
020500920116     C                   SETOFF                                       5099
020600911118      *
020700911216     C     §7LDES        IFEQ      *BLANKS
020800911216     C                   SETON                                        9950
020900911118     C   99              GOTO      END1
021000911118     C                   END
021100911218      *
021200891030     C     END1          ENDSR
021300061027      **------------------------
021400061027      ** CONTROLLI 3°VIDEATA
021500061027      **------------------------
021600061027     C     CTRd03        BEGSR
021700061027     c* Impostare tipo bolla o codificati, solo se impostato I/E
021800061027     c                   if        vidcoie=' '
021900061027     c                   if        vidcocod<>' ' or vidcofa<>' '
022000061027     c                   seton                                        5199
022100061027     c                   goto      end3
022200061027     c                   endif
022300061027     c                   else
022400061027     c
022500061027     c* Se impostato I/E, impostare anche il tipo bolla o codifICATI
022600061027     C                   IF        VIDCOCOD=' ' and VIDCOFA=' '
022700061027     c                   seton                                        5399
022800061027     c                   goto      end3
022900061027     c                   endif
023000061027     c                   endif
023100061027     c*
023200061027     c* Impostare tipo bolla o codificati, solo se impostato I/E
023300061027     c                   if        vidcpie=' '
023400061027     c                   if        vidcpcod<>' ' or vidcpfa<>' '
023500061027     c                   seton                                        5299
023600061027     c                   goto      end3
023700061027     c                   endif
023800061027     c                   else
023900061027     c
024000061027     c* Se impostato I/E, impostare anche il tipo bolla o codifICATI
024100061027     C                   IF        VIDCpCOD=' ' and VIDCpFA=' '
024200061027     c                   seton                                        5499
024300061027     c                   goto      end3
024400061027     c                   endif
024500061027     c                   endif
024600131126     c
024700131129     c* Se viene impostata il tipo della variaizone da inviare a PDA
024800131127     c* verificare se esiste nella tabella TCV
024900131127      /free
025000131129       // controllo tipo variazione da inviare a PDA ed AUT
025100131127       clear ViDdpda  ;
025200131127       If  Vidpda <> ' ' ;
025300131127          clear TIBS02DS;
025400131127          T02mod = 'C';
025500131127          T02cod = 'TCV';
025600131127          T02ke1 = Vidpda ;
025700131127          TNTBE_RicercaControllo (kpjba : tibs02ds);
025800131127          IF  T02err = *blanks;
025900131127            DTCV = t02uni ;
026000131127            VIDdpda = §TCVdes;
026100131127          Else ;
026200131127            *in55 = *on ;
026300131127            *in99 = *on ;
026400131127            leavesr ;
026500131127          ENDIF;
026600131127        ENDIF;
026700131127      /end-free
026800131127     c
026900061027     c
027000061027     C     END3          ENDSR
027100911216      **----------------------------
027200911216      ** AGGIORNAMENTO / INSERIMENTO
027300911216      **----------------------------
027400891030     C     SCRIVI        BEGSR
027500911216      *§
027600060310
027700891107     C     KTAB          CHAIN     TABEL                              80
027800891030     C                   MOVEL     §KUT          TBLKUT
027900891030     C                   MOVEL     §COD          TBLCOD
028000891030     C                   MOVEL     §KEY          TBLKEY
028100061027     c* consegna totale
028200061027     c                   movel     vidvdco       §7lvdopo
028300061027     c                   movel     vidcoie       §7lines
028400061027     c                   movel     vidcofa       §7lfras
028500061027     c                   movel     vidcocod      §7lcodif
028600061027     c                   movel     ds7lcampo     §7lvdco
028700061027     c* consegna parziale
028800061027     c                   movel     vidvdcop      §7lvdopo
028900061027     c                   movel     vidcpie       §7lines
029000061027     c                   movel     vidcpfa       §7lfras
029100061027     c                   movel     vidcpcod      §7lcodif
029200061027     c                   movel     ds7lcampo     §7lvdcop
029300131127     c                   movel     VIDpda        §7Lpda
029400911216     C                   MOVEL     DS7L          TBLUNI
029500911216      *§
029600900420     C                   MOVE      '1'           TBLFTT
029700900420     C                   MOVE      *ZERO         TBLFLT
029800900420     C                   MOVE      *BLANK        TBLFTR
029900900420     C                   MOVE      *ZERO         TBLDTR
030000911216      *§
030100891102     C   80              WRITE     TABEL                                98
030200891102     C  N80              UPDATE    TABEL                                98
030300911216      *§
030400891030     C                   ENDSR
030500060310      **----------------------------
030600060310      ** Emissione window informativa
030700060310      **----------------------------
030800060310     C     ges_wdw       BEGSR
030900060310     c                   if        s_§7lmtv = ' ' and s_§7lmtv <> §7lmtv
031000060310     c                   eval      wsdcod = '(§' + vcod + ')'
031100060310     c                   do        *hival
031200060310     c                   exfmt     tb33wdw
031300060310     c   kl              leave
031400060310     c                   enddo
031500060310     c                   endif
031600060310     c                   endsr
