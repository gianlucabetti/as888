000100920529     H DECEDIT('0,') DATEDIT(*YMD.)
000200990910     F* TRMK61R *-----------------------------------------------------*
000300050830     F*       TRASCODIFICA DEL FILE FORNITOCI DALLA CONSODATA 08/2005 *
000400050830     F* copia TRASCODIFICA DEL FILE FORNITOCI DALLA SEAT  07/2003     *
000500030901     F*       il codice seat è dichairato a 9 ma utilizzato a 8       *
000600010125     F*       SE FOSSE 9 non ci stà nella struttura !!!!!!!!!!!!!     *
000700030903     f*                                                               *
000800030903     F* se è una elaborazione di prova ELAB è uguale a SI quindi      *
000900030903     F* non vengono aggiornati gli archivi
001000920508     F*---------------------------------------------------------------*
001100990909     FTNCPO01L  UF A E           K DISK
001200030903     FTNCPO01S  UF A E           K DISK    rename(Tncpo000:Tncpos)
001300050830     FCNSEA00F  IF   F  455        DISK
001400981201     FCNACO16L  IF   E           K DISK
001500990909     FTABEL00F  IF   E           K DISK
001600990910     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001700981201     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOE)
001800010125     FSYSPRT    O    F  132        PRINTER OFlind(*INOG)
001900920508     F*---------------------------------------------------------------*
002000030903     D CTI             S              4    DIM(200)                             CAT SEAT INES.NTI
002100981201     D ERR             S             75    DIM(2) CTDATA PERRCD(1)
002200920526     F*---------------------------------------------------------------*
002300000000     D KPJBA         E DS
002400900831     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500010122     D DS1M          E DS
002600981201     D WLBDA8          DS                  INZ
002700981201     D  G08DAT                 1      8  0
002800981201     D  G08INV                 9     16  0
002900981201     D  G08ERR                17     17
003000981201     D  G08TGI                18     22  0
003100950302     D                 DS
003200950302     D  WSRAG                  1     48
003300981201     D  WSVIA                 49     83
003400981201     D  WSCIT                 84    118
003500981201     D  WSCAP                119    127
003600010122     D  WSPRV                128    129
003700981201     D  WSTEL                130    145
003800981201     D  WCAT                 146    150  0
003900981201     D  DSSEA                  1    150
004000950302     D*
004100950302     D                 DS
004200950302     D  CPORAG                 1     48
004300981201     D  CPOVIA                49     83
004400981201     D  CPOCIT                84    118
004500981201     D  CPOCAP               119    127
004600981201     D  CPOPRV               128    129
004700981201     D  CPOTEL               130    145
004800981201     D  CPOSCT               146    150  0
004900981201     D  DSCPO                  1    150
005000990910     D DSDATE          DS
005100990910     D  DATACQ                 1      8  0
005200030903     D  PROVA                  9     10
005300920520      *
005400980730     D* DS PER TISI95R - CONTROLLO DI CAP
005500980730     D DSSI95        E DS                  EXTNAME(TISI95DS)
005600980826     D DSLV55        E DS                  EXTNAME(FNLV55DS)
005700030903      * DS PER TRMK25R - CONTROLLO ULTIMO STATO
005800030903     D DSCPS         E DS                  EXTNAME(TNCPS00F)
005900030903     D DSMK25        E DS                  EXTNAME(TRMK25DS)
005901050902      *
005902050902     d totale          s              9  0
006000030903      *
006100981201     I*
006200030903     ICNSEA00F  AA
006300030902     I                                  1    2 0SEACRG                          Cod.Regione ISTAT
006400030902     I                                  4    6 0SEACPR                          Cod.Provincia ISTAT
006500030902     I                                  8   10 0SEACCO                          Cod.Comune ISTAT
006600050830     I****** non è + numerico          12   16 0SEACST                          Cod.Strada SEAT
006700050830     I                                 12   16  SEACST                          Cod.Strada SEAT
006800030902     I                                 18   20  SEAPRV                          Sigla Provincia Ital
006900030902     I                                 22   56  SEACIT                          Località
007000030902     I                                 58  107  SEAVIA                          Indirizzo
007100030902     I                                109  123  SEACIV                          Civico
007200030902     I                                125  134  SEACAP                          Cap
007300030902     I                                136  235  SEANOM                          Ragione Sociale
007400030902     I                                237  246  SEADGI                          Descriz.Forma Giurid
007500030902     I                                248  254 0SEACAT                          Cod. Categoria P.G.
007600030902     I                                256  259  SEAPTE                          Prefisso Telefonico
007700030902     I                                261  269  SEATEL                          Nr.Telefono
007800030902     I                                271  273 0SEALTE                          Numero linee
007900030902     I                                275  278  SEAMAC                          Cod.Macrocateg. SEAT
008000030902     I                                280  339  SEADCA                          Descrizione categori
008100030902     I                                341  400  SEAPES                          Parole ESPLICATIVE
008200030902     I                                402  402  SEASEX                          Sesso
008300030902     I                                404  413  SEADTI                          Descrizione Titolo
008400030902     I                                431  431  SEANIE                          Area Nielsen
008500030902     I                                433  433  SEACLC                          Classe Dimensionale
008600030902     I                                435  443 0SEACOD                          Cod.Operatore SEAT
008700030902     I                                445  453 0SEAPRG                          Num.progressivo
008800030902     I                                455  455  SEAAST                          Asterisco
008900950302     C*---------------------------------------------------------------*
009000981201     C**
009100981201     C** 1) LETTURA NUOVI POTENZIALI DA CNSEA
009200981201     C**
009300030903     C                   DO        *HIVAL
009400030903      *
009500030903     C                   READ      CNSEA00F                               10
009600030903      * fine lettura
009700030903    1C                   if        *in10
009800030903     c                   leave
009900030903     c                   endif
010000030903      *
010100030903     C                   Z-ADD     SEACOD        KCPOCD
010200030903      *
010300030903      * Controllo se gia' presente in ..................... STORICO POTENZIALI
010400930309      *
010500030903     C     KCPOCD        CHAIN     TNCPO01S
010600030903      *---------------------------------------------------------------------
010700030903      * se esiste conteggio e stampo e leggo un nuovo potenziale da caricare
010800030903      *---------------------------------------------------------------------
010900030903     c                   if        %found(tncpo01s)
011000030903      * conteggio
011100030903     c                   add       1             contsto           9 0
011200030903      * stampo i codici nellol storico
011300050902     c   og              except    tespro
011400050902     c   og              setoff                                       og
011500030903      *
011600030903     c                   except    storico
011700030903      *
011800030903     c                   iter
011900030903      *
012000030903     c                   endif
012100030903      *---------------------------------------------------------------------
012200030903      * imposto la DS con i campi di CNSEA e verifico categoria seat
012300030903      *---------------------------------------------------------------------
012400030903     C                   EXSR      IMPO
012500030903      *
012600030903      * Controllo se gia' presente in ..................... ARCHIVIO POTENZIALI
012700030903      *
012800990909     C     KCPOCD        CHAIN     TNCPO01L                           30
012900990910     C*
013000030903     C**
013100030903     C* CLIENTE POTENZIALE GIA' ESISTENTE
013200030903     C**
013300950302    2C     *IN30         IFEQ      *OFF
013400030903      *---------------------------------------------------------------------
013500030903      * se esiste stampo che esiste già, aggiorno dati anagrafici e la filiale
013600030903      * di trasmissione (se risulta diversa da quella calcolata dal  cappario
013700030903      * e se il potenziale ha stato "0")  e conteggio i record aggionati
013800030903      *---------------------------------------------------------------------
013900030903      * stampo
014000030903     C   og              EXCEPT    TESPRO
014100030903     C   og              SETOFF                                       og
014200030903     C                   EXCEPT    PRTCOD
014300030903     C* aggiorno il file
014400990909     C                   EXSR      WRIOL1
014500990909     C**
014600010125   X2C                   ELSE
014700010125     C*
014800010125     C* CLIENTE POTENZIALE NUOVO
014900030903      *---------------------------------------------------------------------
015000030903      * se non esiste verifico se esiste già il cliente in CNACO , verifico
015100030903      * il cap se esistente ed in base a quello imposto la filiale di
015200030903      * trasmissione scrivo il file e conteggio
015300030903      *---------------------------------------------------------------------
015400030903     C**codice presente in ACO e non in CPO..........!!!!!
015500030903     C                   Z-ADD     SEACOD        KACOCD
015600030903     C     KACOCD        CHAIN     CNACO16L                           31
015700030903     C  N31              EXCEPT    STA8
015800030903     c*
015900010125     C                   EXSR      WRINE1
015901050902      * Tolto STA7 che non guarda + nessuno e lascio solo le
016000050902     C***                EXCEPT    STA7
016100030903     c* se elaborazione di prova non scrivo
016200030903     c                   if        PROVA = 'NO'
016300981201     C                   WRITE     TNCPO000
016400030903     c                   endif
016500010123     c                   add       1             CONTad            9 0
016600980825    2C                   ENDIF
016700950302     C*
016800950303    1C                   ENDDO
016900980730     C** CHIUSURA PGM
017000980730     C                   CLEAR                   DSSI95
017100980826     C                   MOVEL     'C'           D55TLA
017200980826     C                   CALL      'FNLV55R'
017300980826     C                   PARM                    DSLV55
017400980826     C                   CLEAR                   DSLV55
017500980826     C                   MOVEL     'C'           I95TLA
017600980826     C                   CALL      'TISI95R'
017700980826     C                   PARM                    DSSI95
017800980826     C*
017801050902      * totale record
017802050902     c                   eval      totale = contsto + contad + contup
017900980826     C                   EXCEPT    STAFIN
018000981201     C**
018100980827     C**
018200980827     C                   SETON                                        LR
018300950302     C*------------------------------------------------------------------------*
018400950302     C*  IMPO   - IMPOSTO CAMPI DSSEA PER CONFRONTO SE RECORD VARIATO
018500950302     C*------------------------------------------------------------------------*
018600950302     C     IMPO          BEGSR
018700980825     C* NUMERO DI TELEFONO
018800010124     C* UNIRE prefisso + / + telefono
018900010124     c                   eval      wstel = %trim(seapte) +
019000010124     c                             '/' + %trim(seatel)
019100010122     C* MACRO-CATEGORIA SEAT
019200010122     C                   MOVEL     '1M'          KCOD
019300010122     C                   MOVEL(P)  SEAMAC        KKEY
019400981201     C     KTAB          CHAIN(N)  TABEL                              91
019500980826     C     *IN91         IFEQ      *OFF
019600010122     C  N91              MOVEL     TBLUNI        DS1M
019700980826     C                   ELSE
019800010122     C                   CLEAR                   DS1M
019900980826     C* SE NON L'HO ANCORA STAMPATA STAMPO
020000010122     C     SEAMAC        LOOKUP    CTI                                    90
020100980826     C     *IN90         IFEQ      *OFF
020200980826     C                   EXCEPT    CATEG
020300980826     C                   ADD       1             DD                3 0
020400010122     C                   MOVEL     SEAMAC        CTI(DD)
020500980826     C                   ENDIF
020600980826     C                   ENDIF
020700980825     C**
020800010122     C                   MOVEL     §1MMCB        WCAT
020900010123     C** UNIRE COGNOME NOME (50+50) se persona fisica
021000950302     C                   MOVEL     SEANOM        WSRAG
021100010123     c                   if        seasex <> 'S'
021200010123     c                   eval      wsrag = %trim(%subst(seanom:1:50)) +
021300010123     c                             ' ' + %trim(%subst(seanom:51:50))
021400010123     c                   endif
021500010122     C** UNIRE INDIRIZZO A NUMERO CIVICO CON UNO SPAZIO
021600010123     c                   eval      wsvia = %trim(seavia) +
021700010123     c                             ' ' + %trim(seaciv)
021800950302     C                   MOVEL     SEACIT        WSCIT
021900981201     C                   MOVEL     SEACAP        WSCAP
022000010122     C                   MOVEL     SEAPRV        WSPRV
022100030903     C     SEAPRV        IFEQ      'RSM'                                        Rep.S.Marino
022101050901     C     SEAPRV        OREQ      'SM '                                        Rep.S.Marino
022200010122     C                   MOVEL     'RN'          WSPRV
022300010122     C                   ENDIF
022400050901     C**** SEAPRV        IFEQ      'PU '                                        Pesaro Urbino
022500050901     C****               MOVEL     'PS'          WSPRV
022600050901     C****               ENDIF
022700050901     C**** SEAPRV        IFEQ      'FC '                                        Forlì Cesena
022800050901     C****               MOVEL     'FO'          WSPRV
022900050901     C****               ENDIF
023000950302     C*
023100950302     C                   ENDSR
023200941111     C*------------------------------------------------------------------------*
023300980730     C*  CHKCAP - CONTROLLO CAP
023400941111     C*------------------------------------------------------------------------*
023500950302     C     CHKCAP        BEGSR
023600950302     C*
023700950302     C                   SETOFF                                       20
023800980730     C**
023900980730     C                   CLEAR                   DSSI95
024000980730     C                   MOVEL     '3'           I95TCN
024100980730     C                   MOVEL     SEACAP        I95CAP
024200980730     C                   MOVEL     SEAPRV        I95PRV
024300010122     C     SEAPRV        IFEQ      'RSM'                                        REP.S.MAR.
024301050902     C     SEAPRV        OREQ      'SM '                                        Rep.S.Marino
024400010122     C                   MOVEL     'RN'          I95PRV
024500010122     C                   ENDIF
024600050902     C***  SEAPRV        IFEQ      'PU '                                        Pesaro Urbino
024700050902     C***                MOVEL     'PS'          I95PRV
024800050902     C***                ENDIF
024900050902     C***  SEAPRV        IFEQ      'FC '                                        Forlì Cesena
025000050902     C***                MOVEL     'FO'          I95PRV
025100050902     C***                ENDIF
025200980730     C                   MOVEL     SEACIT        I95LOC
025300990910     C                   MOVEL     DATACQ        I95DAT
025400980730     C                   CALL      'TISI95R'
025500980730     C                   PARM                    DSSI95
025600980730     C* ERRORE
025700980730     C     O95ERR        IFNE      *BLANKS
025800980825     C                   SETON                                        20
025801050902     c                   add       1             contcaperr        9 0
025901050902     C**   se cap errato cerco di recuperarlo lo stesso
025902050902     C                   CLEAR                   DSSI95
025903050902     C                   MOVEL     '7'           I95TCN
025904050902     C                   MOVEL     SEACAP        I95CAP
025905050902     C                   MOVEL     SEAPRV        I95PRV
025906050902     C     SEAPRV        IFEQ      'RSM'                                        REP.S.MAR.
025907050902     C     SEAPRV        OREQ      'SM '                                        Rep.S.Marino
025908050902     C                   MOVEL     'RN'          I95PRV
025909050902     C                   ENDIF
025916050902     C                   MOVEL     SEACIT        I95LOC
025917050902     C                   MOVEL     DATACQ        I95DAT
025918050902     C                   CALL      'TISI95R'
025919050902     C                   PARM                    DSSI95
025920050902     C* ERRORE
025921050902     C     O95ERR        IFEQ      *BLANKS
025922050902     C                   SETOff                                       20
025923050902     c                   add       1             contcapimpo       9 0
025924050902     C                   ENDIF
026000950302      *
026001050902     C                   ENDIF
026002050902      *
026100950302     C                   ENDSR
026200990909     C*---------------------------------------------------------------
026300030903     c* Aggiorno TNCPO
026400030903     C*---------------------------------------------------------------
026500990909     C     WRIOL1        BEGSR
026600990909     C**
026700990909     C                   MOVEL     DSSEA         DSCPO
026800990909     C**
026900990909     C                   EXSR      CHKCAP
027000990909     C* CAP INESISTENTE IN CAPPARIO
027100030903    1C     *IN20         IFEQ      *ON
027200990909     C   OF              EXCEPT    TESTA
027300990909     C                   SETOFF                                       OF
027400990909     C                   MOVEL     ERR(2)        MSGERR           75
027500990909     C                   EXCEPT    CAPNEW
027600030903      *
027700030903   x1c                   ELSE
027800030903      * verifico se lna diversa dalla filiale di trasmissione
027900030903    2c                   if        o95lna <> cpoflt
028000030903      * verifico se il potenziale ha stato  0
028100030903     C                   clear                   DsMK25
028200030903     C                   movel     CpoCpo        i25Cpo
028300030903     C                   MOVEL     *blanks       I25DIT
028400030903     C                   MOVEL     DsMK25        KPJBU
028500030903      *
028600030903     C                   clear                   DSCPS
028700030903     C                   CALL      'TRMK25R'
028800030903     C                   PARM                    KPJBA
028900030903     C                   PARM                    DSCPS
029000030903     C                   MOVEL     KPJBU         DSMK25
029100030903      *
029200030903    3C                   IF        O25Err = ' '
029300030903      * se è senza stato stampo e aggiorno la filiale di trasmissione
029400030903    4c                   if        cpsfst = *blank
029500030903     C   OF              EXCEPT    TESTA
029600030903     C                   SETOFF                                       OF
029700030903     C                   EXCEPT    FLTNEW
029800030903      *
029900030903     c                   movel     o95lna        cpoflt
030000030903    4c                   endif
030100030903      *
030200030903    3c                   endif
030300030903      *
030400030903    2c                   endif
030500030903    1C                   ENDIF
030600990910     C**
030700040806     c                   z-add     *date         CPODTR
030800990909     C***
030900030903     c* se elaborazione di prova non aggiorno
031000030903     c                   if        PROVA = 'NO'
031100990909     C                   UPDATE    TNCPO000
031200030903     c                   else
031300030903     c                   unlock    tncpo01l
031400030903     c                   endif
031500010123     c                   add       1             CONTup            9 0
031600990909     C                   ENDSR
031700990909      *-------------------------------------------------------------------------
031800030903     c* Scrivo  TNCPO
031900030903     C*---------------------------------------------------------------
032000990909     C     WRINE1        BEGSR
032100990909     C                   CLEAR                   TNCPO000
032200990909     C                   Z-ADD     SEACOD        CPOCPO
032300030903     C***********        EXSR      IMPO
032400990910     C                   Z-ADD     DATACQ        CPODAQ
032500990909     C                   MOVEL     DSSEA         DSCPO
032600990909     C**
032700990909     C                   EXSR      CHKCAP
032800990909     C* CAP INESISTENTE IN CAPPARIO
032900990909     C     *IN20         IFEQ      *ON
033000990909     C   OF              EXCEPT    TESTA
033100990909     C                   SETOFF                                       OF
033200990909     C                   MOVEL     ERR(2)        MSGERR
033300990909     C                   EXCEPT    CAPNEW
033400990909     C                   ELSE
033500990909     C                   MOVEL     O95LNA        CPOFLT
033600990909     C                   ENDIF
033700040806
033800040806     c                   z-add     *date         CPODTR
033900990909     C                   ENDSR
034000950302     C*------------------------------------------------------------------------*
034100950302     C     *INZSR        BEGSR
034200950302     C*
034300950302     C     *ENTRY        PLIST
034400950302     C                   PARM                    KPJBA
034500990910     C                   MOVEL     KPJBU         DSDATE
034600980826     C**
034700981201     C                   TIME                    UTIME             6 0
034800950302     C                   Z-ADD     1             CODUT
034900950302     C                   CALL      'X§PARUT'
035000950302     C                   PARM                    UT§DSE
035100950302     C                   MOVEL     RAGUT         RSUT             20
035200950302      *
035300950302     C     KTAB          KLIST
035400950302     C                   KFLD                    CODUT
035500980825     C                   KFLD                    KCOD              2
035600980825     C                   KFLD                    KKEY              8
035700950302     C*
035800950302     C     *LIKE         DEFINE    CPOCPO        KCPOCD
035900990909     C     *LIKE         DEFINE    ACOLIB        KACOCD
036000980826     C**
036100980826     C** TESTATA IN STAMPA
036200980826     C                   EXCEPT    TESTA
036300010125     C                   EXCEPT    TESPRO
036400950302     C                   ENDSR
036500981201     C*---------------------------------------------------------------
036600930311     O*****************************************************************
036700950307     OPRTF198   E            TESTA            02
036800950303     O                       RSUT                20
036900950303     O                                           62 'ELENCO ANOMALIE CLIENTI'
037000950303     O                                         +  1 'POTENZIALI DA SEAT'
037100950303     O                                           90 'Data:'
037200950303     O                       UDATE             +  1 '  /  /  '
037300990910     O                                         +  5 'TRMK61R'
037400930311     O                                          125 'Pagina:'
037500930311     O                       PAGE          Z    132
037600950307     O          E            TESTA            03
037700950302     O                       KNSIF               10
037800950302     O                       KNMUS               21
037900950303     O                       UTIME               99 '0 :  :  '
038000950307     O          E            TESTA            05
038100980826     O                                           11 'CLIENTE'
038400950307     O          E            TESTA          1 06
038700950306     O*------------------------------------
038800950306     O          E            CAPNEW         1
038900010122     O                       SEACOD        Z      9
039000030903     O                       SEAPRV            +  2
039100030903     O                       CPOCAP            +  2
039200980826     O                       MSGERR             110
039300980826     O                       O95MSG             198
039400980826     O          E            CATEG          1
039500010122     O                       SEAMAC            + 31
039600010122     O                                         +  2 'MACRO CAT SEAT INESISTEN'
039700980826     O                                         +  0 'TE'
039800030903     O                       SEADCA            +  5
040200030903     O          E            FLTNEW         1
040300030903     O                       SEACOD        Z      9
040400030903     O                       O95LNA            +  8
040500030903     o                                         +  1 'nuova'
040600030903     O                       CPOFLT            +  8
040700030903     o                                         +  1 'vecchia'
040800030903     O                                         +  8 'Modif.filiale di trasmis.'
040900930311     O*------------------------------------
041000010125     O          E            STAFIN      1
041100050902     O                                              'Presenti nello storico   '
041200050902     O                       contsto       Z   +  2
041201050902     O                                         +  2 'Non aggiornati'
041202050902     O          E            STAFIN      1
041203050902     O                                              'Potenziali già esistenti '
041204050902     O                       contup        Z   +  2
041205050902     O                                         +  2 'Aggiornati'
041300010125     O          E            STAFIN      1
041400050902     O                                              'Potenziali aggiunti      '
041500050902     O                       contad        Z   +  2
041802050902     O                                         +  2 'Con cap errato'
041803050902     O                       contcaperr    Z   +  2
041805050902     O                                         +  2 'da noi impostato'
041806050902     O                       contcapimpo   Z   +  2
041807050902     O          E            STAFIN      2
041808050902     O                                              'T O T A L E _____________'
041809050902     O                       totale        Z   +  2
041900010125     O          E            STAFIN      1
042000930311     O                                           76 '*** FINE STAMPA ***'
042100990909     OQSYSPRT   E            STA5        1
042200990909     O                                              'SEAT:PRESENTE IN CNACO '
042300990909     O                                         +  0 'SENZA TNCPO            '
042400990909     O                       SEACOD        Z   +  1
042500010123     O                                          132 '5'
042600990909     OQSYSPRT   E            STA6        1
042700990909     O                                              'SEAT:PRESENTE IN CNACO '
042800990909     O                                         +  0 'MA NON TROVATA ANAGR   '
042900990909     O                       SEACOD        Z   +  1
043000010123     O                                          132 '6'
043100990909     OQSYSPRT   E            STA7        1
043200990909     O                                              'SEAT:NUOVO E NON PRESENT'
043300990909     O                                         +  0 'E  IN CNACO            '
043400990909     O                       SEACOD        Z   +  1
043500010123     O                                          132 '7'
043600010125     OQSYSPRT   E            STA8        1
043700010125     O                                              'SEAT:NO TNCPO e PRESENT'
043800010125     O                                         +  0 'E  IN CNACO            '
043900010125     O                       SEACOD        Z   +  1
044000010125     O                                          132 '8'
044100990909     OQSYSPRT   E            STA11       1
044200990909     O                                              'SEAT:PIU''DI UN ACO PER '
044300990909     O                                         +  0 'LO STESSO TNCPO        '
044400990909     O                       CPOCPO        Z   +  1
044500010123     O                                          132 'B'
044600010125     OSYSPRT    E            TESPRO           02
044700010125     O                       RSUT                20
044800010125     O                                           62 'ELENCO POTENZIALI ACQUI'
044900010125     O                                         +  0 'SITI .............'
045000010125     O                                           90 'Data:'
045100010125     O                       UDATE             +  1 '  /  /  '
045200010125     O                                         +  5 'TRMK61R'
045300010125     O                                          125 'Pagina:'
045400050902     O                       PAGE1         Z    132
045500010125     O          E            TESPRO           03
045600010125     O                       KNSIF               10
045700010125     O                       KNMUS               21
045800010125     O                       UTIME               99 '0 :  :  '
045900010125     O          E            TESPRO           05
046000010125     O                                           11 'CLIENTE'
046100010125     O                                         +  5 'CLIENTE'
046200010125     O          E            TESPRO         1 06
046300010125     O                                           11 'VECCHIO'
046400010125     O                                         +  5 'NUOVO'
046500990910     O***----------------------------------
046600030903     O          E            PRTCOD         1
046700010125     O                       SEACOD        Z     11
046800010125     O              N30      CPOCPO        Z   +  5
046801050902     O          E            STORICO        1
046802050902     O                       SEACOD        z     11
046803050902     O                                         + 20 'Presente nello storico'
046900930311     O*----------------------------------------------------------------
047000980826** ERR
047100980825C A P, DI CLIENTE GIA' ESISTENTE, NON TROVATO
047200980826C A P, DI CLIENTE NUOVO         , NON TROVATO
